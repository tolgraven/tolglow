// Run against a disposable Studio with its production ClojureScript build.
const assert = require("node:assert/strict");
const {chromium} = require("playwright-core");

(async () => {
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || "/usr/bin/chromium",
    args: ["--no-sandbox", "--use-gl=angle", "--use-angle=swiftshader", "--enable-unsafe-swiftshader"],
  });
  try {
    const context = await browser.newContext({
      baseURL: process.env.STUDIO_URL || "http://127.0.0.1:8500",
      viewport: {width: 1024, height: 1366}, hasTouch: true,
    });
    const page = await context.newPage();
    const statusGets = [], inventory = [], errors = [], interests = [];
    const sockets = [];
    page.on("pageerror", e => errors.push(e.message));
    page.on("request", r => {
      if (r.method() !== "GET") return;
      const path = new URL(r.url()).pathname;
      if (["/api/sync", "/api/audio"].includes(path)) statusGets.push(path);
      if (["/api/sync/ports", "/api/audio/inputs"].includes(path)) inventory.push(path);
    });
    // Deliberately interrupt the genuine state socket to exercise reconnect.
    await page.addInitScript(() => {
      const Original = window.WebSocket;
      window.WebSocket = class extends Original {
        constructor(...args) {
          super(...args);
          if (new URL(this.url).pathname === "/api/state/stream") window.stateTestSocket = this;
        }
      };
    });
    page.on("websocket", socket => {
      if (new URL(socket.url()).pathname !== "/api/state/stream") return;
      sockets.push(socket);
      socket.on("framesent", ({payload}) => {
        const data = JSON.parse(payload);
        if (data.includes("~:connections")) interests.push(data[data.indexOf("~:enabled?") + 1]);
      });
    });
    const clock = id => page.waitForFunction(source => document.querySelector("#clock-source")?.value === source, id);
    const selectSource = async source => {
      const response = await context.request.post("/api/sync/source", {data: {"source-id": source}});
      assert.ok(response.ok(), await response.text());
    };
    await page.goto("/connections", {waitUntil: "domcontentloaded"});
    await page.getByText("Capture stopped", {exact: true}).first().waitFor();
    await page.waitForFunction(() => !document.querySelector("#clock-source")?.disabled);
    await selectSource("ableton-link");
    await clock("ableton-link");
    await page.locator("#link-port").fill("17666");
    await selectSource("midi-clock");
    await clock("midi-clock");
    assert.equal(await page.locator("#link-port").inputValue(), "17666");
    await page.evaluate(() => window.stateTestSocket.close(4000, "testing reconnect"));
    await page.waitForFunction(() => window.stateTestSocket.readyState === WebSocket.OPEN);
    await selectSource("beat-link");
    await clock("beat-link");
    assert.ok(sockets.length >= 2);
    await page.waitForTimeout(1700); // Covers the removed 100ms and 750ms timers.
    assert.deepEqual(inventory.sort(), ["/api/audio/inputs", "/api/sync/ports"]);
    // Headless Chromium keeps every page visible. Drive its DOM visibility
    // signal explicitly and wait for the actual server's unscoped baseline.
    await page.evaluate(() => {
      Object.defineProperty(document, "hidden", {configurable: true, value: true});
      document.dispatchEvent(new Event("visibilitychange"));
    });
    await page.getByText("Awaiting capture status", {exact: true}).waitFor();
    await selectSource("ableton-link");
    await page.evaluate(() => {
      delete document.hidden;
      document.dispatchEvent(new Event("visibilitychange"));
    });
    await clock("ableton-link");
    await page.locator('nav a[data-view="library"]').tap();
    await page.waitForURL("**/library");
    await selectSource("manual");
    await page.locator('nav a[data-view="connections"]').tap();
    await clock("manual");
    await page.getByText("Capture stopped", {exact: true}).first().waitFor();
    assert.deepEqual(interests, [true, true, false, true, false, true]);
    assert.deepEqual(statusGets, []);
    assert.deepEqual(errors, []);
    console.log("Verified touch navigation, live changes, waveform status, draft preservation and socket reconnection without status GETs.");
  } finally {
    await browser.close();
  }
})().catch(e => { console.error(e); process.exitCode = 1; });
