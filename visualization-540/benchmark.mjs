import assert from 'node:assert/strict';
import {readFileSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';

const require = createRequire(import.meta.url);
const {chromium} = require(process.env.PLAYWRIGHT_MODULE ?? 'playwright-core');
const bundle = process.argv[2];
assert.ok(bundle, 'Pass the built test-only main.js');
const root = 'http://history-binding.test';
const scripts = new Map([
  ['/main.js', readFileSync(bundle)],
  ...[
    'tolglow-visualization-ring-540/ring-history.mjs',
    'tolglow-visualization-precision-540/trace-storage.mjs',
    'tolglow-visualization-precision-540/fixtures.mjs',
  ].map(path => ['/' + path, readFileSync(new URL('../' + path, import.meta.url))]),
]);
const methods = ['current', 'fullSnapshot', 'fullReadView'];
const rounds = 3;
const size = 128;
const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH ?? '/usr/bin/chromium',
  args: ['--no-sandbox', '--disable-dev-shm-usage'],
});

async function open() {
  const page = await browser.newPage();
  await page.route(root + '/**', async route => {
    const path = new URL(route.request().url()).pathname;
    if (path === '/') {
      await route.fulfill({contentType: 'text/html', body: '<!doctype html><script src="/main.js"></script>'});
    } else if (scripts.has(path)) {
      await route.fulfill({contentType: 'text/javascript', body: scripts.get(path)});
    } else {
      await route.abort();
    }
  });
  await page.goto(root);
  await page.evaluate(async () => {
    const {RingHistory} = await import('/tolglow-visualization-ring-540/ring-history.mjs');
    const {traces, zooms} = await import('/tolglow-visualization-precision-540/fixtures.mjs');
    const chart = globalThis.historyBindingBench;
    if (!chart) throw new Error('Compiled chart benchmark failed to initialize');
    function frame(tick) {
      return traces.map(({samples}, j) => ({
        timeMs: 1e6 + tick * 25,
        beat: 6000 + tick / 20,
        source: 'stable-clock-a',
        samplePeriodMs: 100,
        value: samples[(tick + j * 17) % samples.length],
      }));
    }
    function advance(mode, banks, frame) {
      let changed = false;
      for (let i = 0; i < banks.length; i++) {
        if (mode === 'current') {
          const previous = banks[i];
          banks[i] = chart.append(previous, frame[i]);
          changed = previous !== banks[i] || changed;
        } else {
          const action = banks[i].observe(frame[i]);
          changed = action === 'append' || action === 'reset' || action === 'replace' || changed;
        }
      }
      return changed;
    }
    function makeBanks(mode, n = traces.length) {
      return Array.from({length: n}, () => mode === 'current' ? null : new RingHistory('full'));
    }
    function draw(mode, bank, window) {
      if (mode === 'current') return chart.draw(bank, window);
      return chart.drawRing(bank, mode === 'fullSnapshot', window);
    }
    const same = (left, right, label) => {
      if (left !== right) throw new Error('Unexpected chart output at ' + label);
    };
    globalThis.runHistoryBinding = (mode, command, heapSize) => {
      if (command === 'verify') {
        const current = makeBanks('current');
        const rings = makeBanks('fullReadView');
        let paths = 0;
        let captions = 0;
        for (let i = 0; i < 1140; i++) {
          const incoming = frame(i);
          same(advance('current', current, incoming.map(chart.prepare)), advance('fullReadView', rings, incoming), 'admission');
          if (i % 37 === 0) {
            for (const window of zooms) {
              for (let j = 0; j < rings.length; j++) {
                const original = draw('current', current[j], window);
                same(draw('fullReadView', rings[j], window), original, 'ring view path');
                same(draw('fullSnapshot', rings[j], window), original, 'ring snapshot path');
                paths += 2;
              }
            }
          }
        }
        for (const incoming of [
          frame(1136),
          frame(1136).map(value => ({...value, value: 0.5001})),
          frame(1140).map(value => ({...value, value: null})),
          frame(1144).map(value => ({...value, value: 1.2})),
          frame(1144).map(value => ({...value, source: 'stable-clock-b'})),
          frame(2).map(value => ({...value, source: 'stable-clock-b'})),
        ]) {
          same(advance('current', current, incoming.map(chart.prepare)), advance('fullReadView', rings, incoming), 'special admission');
          for (let j = 0; j < rings.length; j++) {
            const original = chart.show(current[j], 4);
            same(chart.showRing(rings[j], false, 4), original, 'ring view output and reading');
            same(chart.showRing(rings[j], true, 4), original, 'ring snapshot output and reading');
            captions += 2;
          }
        }
        return {paths, captions};
      }
      if (command === 'heap') {
        const banks = makeBanks(mode, heapSize);
        for (let tick = 0; tick < 256; tick++) {
          for (let i = 0; i < banks.length; i++) {
            const sample = {
              timeMs: tick * 100, beat: tick / 5, source: 'heap',
              value: traces[i % traces.length].samples[tick] * (1 - i * 0.00001),
            };
            if (mode === 'current') banks[i] = chart.append(banks[i], chart.prepare(sample));
            else banks[i].observe(sample);
          }
        }
        const retained = banks.reduce((n, bank) => n + (mode === 'current' ? chart.size(bank) : bank.length), 0);
        globalThis.benchmarkRetainedHistory = banks;
        return retained;
      }
      const incoming = Array.from({length: 1280}, (_, i) => {
        const arrival = frame(i);
        return mode === 'current' ? arrival.map(chart.prepare) : arrival;
      });
      function run(track = false) {
        const banks = makeBanks(mode);
        const rows = {admission: [], admissionAndPath: [], fullHistoryAndPath: []};
        let checksum = 0;
        for (const [i, samples] of incoming.entries()) {
          const start = performance.now();
          const changed = advance(mode, banks, samples);
          const ingested = performance.now();
          if (changed) {
            const window = (i & 4) ? 16 : 4;
            for (const bank of banks) {
              const d = draw(mode, bank, window);
              checksum += d.length + d.charCodeAt(d.length >>> 1);
            }
          }
          const end = performance.now();
          if (track && changed) {
            rows.admission.push((ingested - start) * 1000);
            rows.admissionAndPath.push((end - start) * 1000);
            if (i >= 1020) rows.fullHistoryAndPath.push((end - start) * 1000);
          }
        }
        if (!(checksum > 0)) throw new Error('Failed to consume paths');
        return Object.fromEntries(Object.entries(rows).map(([key, values]) => {
          values.sort((a, b) => a - b);
          return [key, {
            samples: values.length,
            p50Us: values[Math.ceil(values.length * 0.50) - 1],
            p95Us: values[Math.ceil(values.length * 0.95) - 1],
          }];
        }));
      }
      run();
      return run(true);
    };
  });
  return page;
}

const receipt = {chromium: browser.version(), rounds, historiesForHeap: size, results: {}};
try {
  const verificationPage = await open();
  receipt.verification = await verificationPage.evaluate(() => runHistoryBinding('current', 'verify'));
  await verificationPage.close();
  for (const method of methods) receipt.results[method] = {timings: []};
  for (let round = 0; round < rounds; round++) {
    for (let j = 0; j < methods.length; j++) {
      const method = methods[(j + round) % methods.length];
      const page = await open();
      const timing = await page.evaluate(mode => runHistoryBinding(mode, 'timing'), method);
      receipt.results[method].timings.push(timing);
      await page.close();
    }
  }
  for (const method of methods) {
    const page = await open();
    const cdp = await page.context().newCDPSession(page);
    async function collect() {
      await cdp.send('HeapProfiler.collectGarbage');
      await cdp.send('HeapProfiler.collectGarbage');
      return cdp.send('Runtime.getHeapUsage');
    }
    const before = await collect();
    const retained = await page.evaluate(({method, size}) => runHistoryBinding(method, 'heap', size), {method, size});
    assert.equal(retained, size * 256);
    const after = await collect();
    const {timings} = receipt.results[method];
    receipt.results[method].heap = {
      retained,
      liveJsHeapDelta: after.usedSize - before.usedSize,
      backingStorageDelta: after.backingStorageSize - before.backingStorageSize,
    };
    receipt.results[method].medianP95 = Object.fromEntries(Object.keys(timings[0]).map(metric => [
      metric, timings.map(t => t[metric].p95Us).sort((a,b) => a-b)[1],
    ]));
    await page.close();
  }
} finally {
  await browser.close();
}
writeFileSync(new URL('binding-report.json', import.meta.url), JSON.stringify(receipt, null, 2) + '\n');
console.log('Verified exact production-chart paths and captions in Chromium. Wrote binding-report.json.');
