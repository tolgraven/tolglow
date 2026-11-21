"use strict";
// Actual built re-frame page + Chromium, no substitute page and no hardware sink.
const fs = require("node:fs/promises");
const path = require("node:path");
const { performance } = require("node:perf_hooks");
const { chromium } = require("playwright-core");
const { createWorker } = require("../../resources/devices/push-display/worker.cjs");

const sampleView = {
  title: "Tolglow", page: "main", origin: [0, 0], status: "ready", master: 0.82,
  "blackout?": false, bpm: 128, selected: "ember",
  cue: { id: "ember", name: "Ember drift", color: [0.85, 0.19, 0.1], "active?": true },
  effect: { id: "ember-effect", name: "Ember", categories: ["color", "intensity"],
    envelope: { attack: { beats: 0.5 }, decay: { beats: 1 }, sustain: 0.7, release: { beats: 2 } } },
  swatch: [0.85, 0.19, 0.1], labels: ["Ember", "Prism", "Sweep", "Pulse", "Haze", "Echo", "Chase", "Dark"],
  parameters: [
    { id: "level", name: "Level", value: 0.72, schema: { type: "number", bounds: [0, 1], default: 0.5, "control/axis": "pressure" } },
    { id: "color", name: "Color", value: [0.85, 0.19, 0.1], schema: { type: "color", default: [1, 0, 0] } },
    { id: "mode", name: "Mode", value: "bounce", schema: { type: "enum", values: ["forward", "bounce", "random"], default: "forward" } },
    { id: "enabled", name: "Enabled", value: true, schema: { type: "boolean", default: true } },
    { id: "count", name: "Steps", value: 4, schema: { type: "integer", bounds: [1, 8], default: 4 } },
    { id: "position", name: "Position", value: [0.3, 0.8], schema: { type: "vector", size: 2, bounds: [0, 1], default: [0.5, 0.5] } },
    { id: "range", name: "Range", value: [0.2, 0.7], schema: { type: "range", bounds: [0, 1], default: [0, 1] } },
    { id: "focus", name: "Focus", value: 0.65, schema: { type: "number", bounds: [0, 1], default: 0.5, "control/axis": "slide" } }
  ]
};

async function run({ url, executablePath, noSandbox = false, frames = 60, outputDirectory }) {
  let browser;
  const worker = await createWorker({ url, executablePath, noSandbox, startupTimeoutMs: 15000, captureTimeoutMs: 3000 },
    async config => { browser = await chromium.launch(config); return browser; });
  try {
    const cdp = await browser.newBrowserCDPSession();
    const cpuSnapshot = async () => (await cdp.send("SystemInfo.getProcessInfo")).processInfo;
    await worker.capture(sampleView);
    const before = await cpuSnapshot(), started = performance.now(), samples = [];
    let last;
    for (let frame = 0; frame < frames; frame++) {
      const view = { ...sampleView, parameters: sampleView.parameters.map((parameter, i) =>
        i === 0 ? { ...parameter, value: frame / Math.max(1, frames - 1) } : parameter) };
      const at = performance.now(); last = await worker.capture(view); samples.push(performance.now() - at);
    }
    const wallMs = performance.now() - started, after = await cpuSnapshot();
    const prior = new Map(before.map(p => [p.id, p.cpuTime]));
    const cpuMs = after.reduce((sum, p) => sum + Math.max(0, p.cpuTime - (prior.get(p.id) || 0)) * 1000, 0);
    const sorted = [...samples].sort((a, b) => a - b);
    const result = { frames, width: 960, height: 160, browser: browser.version(), wallMs,
      captureMedianMs: sorted[Math.floor(sorted.length / 2)], captureP95Ms: sorted[Math.floor((sorted.length - 1) * 0.95)],
      effectiveFps: frames * 1000 / wallMs, browserProcessesCpuMs: cpuMs, browserCpuMsPerFrame: cpuMs / frames,
      cpuMethod: "Chromium CDP SystemInfo.getProcessInfo cumulative CPU deltas; all currently reported browser processes",
      environment: { platform: process.platform, architecture: process.arch, node: process.version, headless: true },
      hardwareOutput: false, source: "actual production re-frame /push-display route with explicit synthetic view data" };
    await fs.mkdir(outputDirectory, { recursive: true });
    await fs.writeFile(path.join(outputDirectory, "push-display.png"), Buffer.from(last.png, "base64"));
    await fs.writeFile(path.join(outputDirectory, "benchmark.json"), JSON.stringify(result, null, 2) + "\n");
    return result;
  } finally { await worker.close(); }
}

module.exports = { run, sampleView };
if (require.main === module) {
  const [url, executablePath, outputDirectory, flag] = process.argv.slice(2);
  if (!url || !executablePath || !outputDirectory) {
    process.stderr.write("Usage: node dev/push-display/benchmark.cjs URL CHROMIUM_EXECUTABLE OUTPUT_DIRECTORY [--no-sandbox]\n");
    process.exitCode = 2;
  } else run({ url, executablePath, outputDirectory, noSandbox: flag === "--no-sandbox" })
    .then(result => process.stdout.write(JSON.stringify(result, null, 2) + "\n"))
    .catch(error => { process.stderr.write(error.stack + "\n"); process.exitCode = 1; });
}
