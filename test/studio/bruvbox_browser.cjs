// Explicit real-browser/WASM gate; start the capture-only studio first.
// STUDIO_URL=http://127.0.0.1:8500 node test/studio/bruvbox_browser.cjs
const { chromium } = require('playwright-core');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');

(async () => {
  const root = path.resolve(__dirname, '../../resources/studio/vendor/neovim');
  const manifest = JSON.parse(fs.readFileSync(path.join(root, 'bruvbox-profile.json'), 'utf8'));
  for (const file of manifest.files) {
    assert.equal(crypto.createHash('sha256').update(fs.readFileSync(path.join(root, file.path))).digest('hex'), file.sha256);
  }
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  });
  try {
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(process.env.STUDIO_URL || 'http://127.0.0.1:8500', { waitUntil: 'domcontentloaded' });
    await page.waitForFunction(() => typeof globalThis.tolglowLoadEngine === 'function');
    const result = await page.evaluate(async () => {
      const { NeovimEngine } = await globalThis.tolglowLoadEngine('neovim');
      const host = document.createElement('div');
      host.style.cssText = 'position:fixed;inset:20px;width:800px;height:420px;z-index:1000';
      document.body.append(host);
      const failures = [];
      const editor = new NeovimEngine(host, { onError: error => failures.push(error) });
      try {
        await editor.start('(def message "Bruvbox ☃")\n;; native syntax');
        const native = await editor.nvim.request('nvim_exec_lua', [
          "local r={name=vim.g.colors_name}; for _,name in ipairs({'Normal','Comment','String','Function','LineNr'}) do r[name]=vim.api.nvim_get_hl(0,{name=name,link=false}) end; r.terminal={}; for i=0,15 do table.insert(r.terminal,vim.g['terminal_color_'..i]) end; return r", [],
        ]);
        await editor.setText('(def answer 42)', 5);
        const selection = await editor.selection();
        const palette = getComputedStyle(document.documentElement);
        return { native, selection, source: await editor.text(), failures,
          css: { background: palette.getPropertyValue('--bruvbox-bg').trim(), foreground: palette.getPropertyValue('--bruvbox-fg').trim() } };
      } finally { editor.stop(); host.remove(); }
    });
    assert.equal(result.native.name, 'bruvbox');
    assert.equal(result.native.Normal.fg, 0xf0e3ba);
    assert.equal(result.native.Normal.bg, 0x262525);
    assert.equal(result.native.Comment.fg, 0x908682);
    assert.equal(result.native.String.fg, 0xb4b88d);
    assert.equal(result.native.Function.fg, 0xdba184);
    assert.equal(result.native.LineNr.bg, 0x191818);
    assert.deepEqual(result.native.terminal, ['#3a393d', '#d18479', '#b4b88d', '#dabd7a', '#7692ab', '#bd979d', '#91b8a4', '#c6bc9b', '#46494c', '#cca687', '#8b8c63', '#d0af80', '#80a0b3', '#af8589', '#9ab499', '#f0e3ba']);
    assert.equal(result.source, '(def answer 42)');
    assert.equal(result.selection.cursor, 5);
    assert.deepEqual(result.css, { background: '#262525', foreground: '#f0e3ba' });
    assert.deepEqual(result.failures, []);
    assert.deepEqual(errors, []);
    console.log(JSON.stringify({ status: 'passed', checks: ['pinned theme source hashes', 'native Bruvbox name and highlight RGB values', 'native 16-color terminal palette', 'actual buffer and cursor preserved', 'shared CSS palette', 'clean browser/editor teardown'], native: result.native, css: result.css }, null, 2));
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
