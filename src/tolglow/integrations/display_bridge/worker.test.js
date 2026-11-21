"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const { spawnSync } = require("node:child_process");
const { createWorker, launchOwned, localDisplayUrl } = require("./worker.js");

function harness(overrides = {}) {
  const calls = [], browser = { close: async () => calls.push("close") };
  const page = {
    setDefaultTimeout: n => calls.push(["timeout", n]),
    goto: async (...args) => calls.push(["goto", ...args]),
    waitForFunction: async () => calls.push("ready"),
    evaluate: async (_fn, arg) => { calls.push(["evaluate", arg]); },
    screenshot: async options => { calls.push(["screenshot", options]); return Buffer.from("test-png"); },
    ...overrides
  };
  browser.newContext = async options => { calls.push(["context", options]); return { newPage: async () => page }; };
  return { calls, browser, launch: async options => { calls.push(["launch", options]); return browser; } };
}
const options = { url: "http://127.0.0.1:8500/push-display", executablePath: "/explicit/chromium" };

test("capture uses an isolated exact-size page and the supplied re-frame update API", async () => {
  const h = harness(), worker = await createWorker(options, h.launch);
  try {
    const view = { title: "Tolglow", parameters: [{ name: "Level", value: 0.7 }] };
    const result = await worker.capture(view);
    assert.equal(result.width, 960); assert.equal(result.height, 160);
    assert.equal(Buffer.from(result.png, "base64").toString(), "test-png");
    assert.deepEqual(h.calls.find(x => x[0] === "context")[1].viewport, { width: 960, height: 160 });
    assert.ok(h.calls.some(x => x[0] === "evaluate" && x[1] === view));
    assert.deepEqual(h.calls.find(x => x[0] === "screenshot")[1].clip, { x: 0, y: 0, width: 960, height: 160 });
    assert.ok(result["capture-ms"] >= 0);
  } finally { await worker.close(); await worker.close(); }
  assert.equal(h.calls.filter(x => x === "close").length, 1);
});

test("capture has one in-flight request, a deadline and deterministic browser cleanup", async () => {
  const h = harness({ screenshot: () => new Promise(() => {}) });
  const worker = await createWorker({ ...options, captureTimeoutMs: 20 }, h.launch);
  const first = worker.capture({ title: "A" });
  await assert.rejects(worker.capture({ title: "B" }), /one capture/);
  await assert.rejects(first, /deadline/);
  await assert.rejects(worker.capture({ title: "C" }), /closed/);
  assert.equal(h.calls.filter(x => x === "close").length, 1);
});

test("startup failure closes the browser and rejects hidden URL or executable defaults", async () => {
  const h = harness({ goto: async () => { throw new Error("server unavailable"); } });
  await assert.rejects(createWorker(options, h.launch), /unavailable/);
  assert.equal(h.calls.filter(x => x === "close").length, 1);
  await assert.rejects(createWorker({ ...options, executablePath: undefined }, h.launch), /executable explicitly/);
  for (const url of ["https://example.com/push-display", "http://127.0.0.1/", "http://user:secret@localhost/push-display"])
    assert.throws(() => localDisplayUrl(url), /loopback/);
});

test("bounded view messages reject before executing application code", async () => {
  const h = harness(), worker = await createWorker(options, h.launch);
  try {
    const before = h.calls.length;
    await assert.rejects(worker.capture({ title: "x".repeat(65537) }), /64 KiB/);
    await assert.rejects(worker.capture([]), /object/);
    assert.equal(h.calls.length, before);
  } finally { await worker.close(); }
});

test("owned Chromium server is killed when graceful shutdown fails", async () => {
  const calls = [], server = { wsEndpoint: () => "owned-browser",
    close: async () => { calls.push("close"); throw new Error("browser stuck"); },
    kill: async () => calls.push("kill") };
  const chromium = { launchServer: async () => server,
    connect: async endpoint => { assert.equal(endpoint, "owned-browser"); return { newContext: async () => ({}) }; } };
  const browser = await launchOwned(chromium, {});
  await browser.close();
  assert.deepEqual(calls, ["close", "kill"]);
  await assert.rejects(launchOwned({ ...chromium, connect: async () => { throw new Error("connect failed"); } }, {}), /connect failed/);
  assert.deepEqual(calls, ["close", "kill", "kill"]);
});

test("an extracted worker resolves the installed module from the launch directory or explicit override", () => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "tolglow-display-resolve-"));
  try {
    const script = path.join(root, "extracted", "worker.cjs"), cwd = path.join(root, "application");
    const modulePath = path.join(cwd, "node_modules", "playwright-core");
    fs.mkdirSync(path.dirname(script), { recursive: true });
    fs.mkdirSync(modulePath, { recursive: true });
    fs.copyFileSync(path.join(__dirname, "worker.js"), script);
    // Distinct errors prove which real Node module was loaded, before any
    // browser startup. An extracted worker cannot resolve this via __dirname.
    fs.writeFileSync(path.join(modulePath, "index.js"), "throw new Error('installed launch-directory module');");
    const override = path.join(root, "explicit-playwright.cjs");
    fs.writeFileSync(override, "throw new Error('explicit module override');");
    for (const [config, expected] of [[{}, "installed launch-directory module"],
      [{ playwrightModule: override }, "explicit module override"]]) {
      const result = spawnSync(process.execPath, [script, JSON.stringify(config)],
        { cwd, encoding: "utf8", timeout: 10000 });
      assert.equal(result.error, undefined);
      assert.equal(result.status, 1);
      assert.deepEqual(JSON.parse(result.stdout), { status: "failed", error: expected });
    }
  } finally { fs.rmSync(root, { recursive: true, force: true }); }
});
