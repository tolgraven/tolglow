"use strict";
// Browser rendering boundary only. Application UI is the studio's re-frame page.
const readline = require("node:readline");
const { performance } = require("node:perf_hooks");

const WIDTH = 960, HEIGHT = 160;

function localDisplayUrl(value) {
  const url = new URL(value);
  if (!["http:", "https:"].includes(url.protocol) || !["127.0.0.1", "localhost", "[::1]"].includes(url.hostname) ||
      url.username || url.password || url.pathname !== "/push-display")
    throw new Error("Display URL must be an HTTP(S) loopback /push-display page");
  return url.href;
}

function deadline(promise, milliseconds, label) {
  let timer;
  return Promise.race([promise, new Promise((_, reject) => {
    timer = setTimeout(() => reject(new Error(label + " deadline exceeded")), milliseconds);
  })]).finally(() => clearTimeout(timer));
}

async function createWorker(options, launch) {
  const url = localDisplayUrl(options.url), timeout = options.captureTimeoutMs || 2000;
  const startupTimeout = options.startupTimeoutMs || 10000;
  const sceneStartup = options.sceneStartupTimeoutMs ?? 45000, sceneCapture = options.sceneCaptureTimeoutMs ?? 10000;
  if (typeof options.executablePath !== "string" || !options.executablePath.length)
    throw new Error("Configure the Chromium executable explicitly; no browser is downloaded");
  for (const value of [timeout, startupTimeout, sceneStartup, sceneCapture])
    if (!Number.isInteger(value) || value < 10 || value > 60000) throw new Error("Display deadlines must be integers in 10..60000 milliseconds");
  let httpCredentials;
  if (options.authTokenEnv !== undefined) {
    if (typeof options.authTokenEnv !== "string" || !/^[A-Za-z_][A-Za-z0-9_]*$/.test(options.authTokenEnv))
      throw new Error("Invalid Studio operator token environment variable name");
    const password = process.env[options.authTokenEnv];
    if (!password) throw new Error("Studio operator token environment variable is unset");
    httpCredentials = { username: "studio", password, origin: new URL(url).origin };
  }
  let browser, context, page, busy = false, closed = false, previousScene = false;
  async function close() {
    if (closed) return;
    closed = true;
    if (browser) await deadline(browser.close(), 1000, "Browser shutdown").catch(() => {});
  }
  try {
    browser = await launch({ executablePath: options.executablePath, headless: true, timeout: startupTimeout,
      args: ["--disable-dev-shm-usage", ...(options.noSandbox ? ["--no-sandbox"] : [])] });
    context = await browser.newContext({ viewport: { width: WIDTH, height: HEIGHT }, deviceScaleFactor: 1,
      reducedMotion: "reduce", colorScheme: "dark", ...(httpCredentials && { httpCredentials }) });
    page = await context.newPage();
    page.setDefaultTimeout(timeout);
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: startupTimeout });
    await page.waitForFunction(() => globalThis.tolglowPushDisplay?.ready === true &&
      typeof globalThis.tolglowPushDisplay.update === "function", null, { timeout: startupTimeout });
    await deadline(page.evaluate(() => document.fonts.ready), startupTimeout, "Display fonts");
  } catch (error) { await close(); throw error; }

  return {
    async capture(view) {
      if (closed) throw new Error("Display browser is closed");
      if (busy) throw new Error("Only one capture may be in flight");
      if (!view || typeof view !== "object" || Array.isArray(view)) throw new Error("Display view must be an object");
      if (Buffer.byteLength(JSON.stringify(view)) > 65536) throw new Error("Display view exceeds 64 KiB");
      busy = true;
      const started = performance.now();
      const scene = !!view.scene && ["simulator", "rig"].includes(view["display-mode"]);
      // Entering the venue compiles the same shaders as Studio on this browser.
      // Once warm, every frame returns to the warm scene deadline.
      const budget = scene ? (previousScene ? sceneCapture : Math.max(sceneStartup, sceneCapture)) : timeout;
      try {
        const png = await deadline((async () => {
          await page.evaluate(value => globalThis.tolglowPushDisplay.update(value), view);
          await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
          await page.evaluate(() => globalThis.tolglowPushDisplay.frameReady?.());
          return page.screenshot({ type: "png", timeout: budget, animations: "disabled", clip: { x: 0, y: 0, width: WIDTH, height: HEIGHT } });
        })(), budget, "Display capture");
        previousScene = scene;
        if (png.length > 2 * 1024 * 1024) throw new Error("Display PNG exceeds 2 MiB");
        return { width: WIDTH, height: HEIGHT, png: png.toString("base64"), "capture-ms": performance.now() - started };
      } catch (error) { await close(); throw error; }
      finally { busy = false; }
    },
    close
  };
}

async function launchOwned(chromium, config) {
  // BrowserServer exposes a process owner and kill operation, unlike a bare
  // Browser connection. A stuck graceful close must not leave Chrome orphaned.
  const server = await chromium.launchServer(config);
  let browser;
  try { browser = await chromium.connect(server.wsEndpoint()); }
  catch (error) { await server.kill(); throw error; }
  return {
    newContext: options => browser.newContext(options),
    async close() {
      try { await deadline(server.close(), 500, "Graceful Chromium shutdown"); }
      catch (_) {
        try { await deadline(server.kill(), 400, "Forced Chromium shutdown"); }
        catch (_) { const process = server.process(); if (process && !process.killed) process.kill("SIGKILL"); }
      }
    }
  };
}

module.exports = { WIDTH, HEIGHT, createWorker, launchOwned, localDisplayUrl, deadline };

if (require.main === module) {
  (async () => {
    const options = JSON.parse(process.argv[2] || "{}");
    // A packaged worker lives in a temporary directory. Resolve installed
    // modules from the launch directory as well as a source checkout; never
    // install/download them or derive their location from the display URL.
    const playwright = require(require.resolve(options.playwrightModule || "playwright-core",
      { paths: [process.cwd(), __dirname] }));
    const worker = await createWorker(options, config => launchOwned(playwright.chromium, config));
    const write = value => process.stdout.write(JSON.stringify(value) + "\n");
    write({ status: "ready", width: WIDTH, height: HEIGHT });
    const lines = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
    lines.on("line", async line => {
      let request;
      try {
        if (Buffer.byteLength(line) > 70000) throw new Error("Display request exceeds its input bound");
        request = JSON.parse(line);
        if (!Number.isSafeInteger(request.id)) throw new Error("Display request needs an integer id");
        const result = await worker.capture(request.view);
        write({ id: request.id, status: "captured", ...result });
      } catch (error) {
        write({ id: request?.id, status: "failed", error: error.message });
      }
    });
    const stop = () => worker.close().finally(() => process.exit(0));
    lines.on("close", stop); process.on("SIGTERM", stop); process.on("SIGINT", stop);
  })().catch(error => {
    process.stdout.write(JSON.stringify({ status: "failed", error: error.message }) + "\n");
    process.exit(1);
  });
}
