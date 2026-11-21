const fs = require('node:fs');
const path = require('node:path');
const { execFileSync } = require('node:child_process');
const { chromium } = require('playwright-core');

async function main() {
  const root = path.resolve(__dirname, '../..');
  const corpus = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
  const source = fs.readFileSync(path.join(__dirname, 'mover-shader.mjs'), 'utf8');
  const browser = await chromium.launch({
    executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
    args: process.env.MOVER_USE_HARDWARE === '1' ? [] :
      ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  });
  try {
    const page = await browser.newPage();
    const driverMessages = new Set();
    page.on('console', message => {
      if (['error', 'warning'].includes(message.type())) driverMessages.add(message.text());
    });
    await page.route('http://localhost/moving-heads', route => route.fulfill({
      headers: { 'content-type': 'text/plain', 'Cross-Origin-Opener-Policy': 'same-origin',
        'Cross-Origin-Embedder-Policy': 'require-corp' }, body: ''
    }));
    await page.goto('http://localhost/moving-heads');
    const result = await page.evaluate(async ({ corpus, source }) => {
      const moduleUrl = code => URL.createObjectURL(new Blob([code], { type: 'application/javascript' }));
      const harness = moduleUrl(source);
      const solver = moduleUrl(corpus.renderer.module);
      try {
        const [{ measure }, { createMovingHeadSolver }] = await Promise.all([import(harness), import(solver)]);
        return await measure(corpus, corpus.renderer.vertex, createMovingHeadSolver);
      } finally {
        URL.revokeObjectURL(solver);
        URL.revokeObjectURL(harness);
      }
    }, { corpus, source });
    result.chromium = browser.version();
    result.cpu = require('node:os').cpus()[0]?.model;
    result.driverMessages = [...driverMessages].filter(message => !message.includes('loseContext: context lost'));
    result.afterglow = execFileSync('git', ['-C', process.env.AFTERGLOW_ROOT || path.join(root, '../afterglow'), 'rev-parse', 'HEAD'], { encoding: 'utf8' }).trim();
    console.log(JSON.stringify(result, null, 2));
    if (!result.passed || (process.env.MOVER_REQUIRE_EXACT === '1' && result.byteMismatches)) process.exitCode = 1;
  } finally { await browser.close(); }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
