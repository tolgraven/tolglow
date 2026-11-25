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
        const source = await editor.text();
        await editor.nvim.request('nvim_command', ['new']);
        await editor.setText('scratch');
        const scratch = await editor.text();
        const palette = getComputedStyle(document.documentElement);
        return { native, selection, source, scratch, failures,
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
    assert.equal(result.scratch, 'scratch');
    assert.deepEqual(result.css, { background: '#262525', foreground: '#f0e3ba' });
    assert.deepEqual(result.failures, []);

    const original = '(def label "Cue ☃")\n';
    const pinned = await page.evaluate(async source => {
      const { NeovimEngine } = await globalThis.tolglowLoadEngine('neovim');
      const host = document.createElement('div');
      host.style.cssText = 'position:fixed;inset:20px;width:800px;height:420px;z-index:1000';
      document.body.append(host);
      const changes = [], failures = [];
      let resolveChange;
      const editor = new NeovimEngine(host, {
        pinBuffer: true, fileName: '/home/user/cue.clj',
        onChange: (text, draft) => { changes.push({ text, draft }); resolveChange?.(); },
        onError: error => failures.push(error),
      });
      try {
        await editor.start(source);
        const before = await editor.draft();
        const nvim = editor.nvim;
        const note = [{ lnum: 0, col: 0, message: 'Draft diagnostic', severity: 1 }];
        const findings = buf => nvim.request('nvim_exec_lua', [
          "return vim.tbl_map(function(item) return item.message end, vim.diagnostic.get(...))", [buf],
        ]);
        const accepted = await editor.setDraftDiagnostics(before, note);
        const initialFindings = await findings(before.bufferId);
        await nvim.request('nvim_command', ['new']);
        const scratchBuffer = await nvim.request('nvim_get_current_buf', []);
        await nvim.request('nvim_buf_set_lines', [scratchBuffer, 0, -1, true, ['scratch']]);
        const whileScratch = await editor.draft();
        const unfocusedSelectionRejected = await editor.selection().then(() => false, () => true);
        const changed = new Promise(resolve => { resolveChange = resolve; });
        await editor.setText('(def label "Edited")');
        await Promise.race([changed, new Promise((_, reject) => setTimeout(() => reject(new Error('Pinned change was not delivered')), 4000))]);
        const after = await editor.draft();
        const clearedOnEdit = await findings(before.bufferId);
        const scratchText = await nvim.request('nvim_buf_get_lines', [scratchBuffer, 0, -1, true]);
        const stale = await editor.setDraftDiagnostics(before, note);
        const wrongBuffer = await editor.setDraftDiagnostics({ ...after, bufferId: scratchBuffer }, note);
        const wrongSession = await editor.setDraftDiagnostics({ ...after, sessionId: 'closed-editor' }, note);
        const wrongDigest = await editor.setDraftDiagnostics({ ...after, sourceSha256: '0'.repeat(64) }, note);

        // Edit after the real native read but before the annotation RPC.
        const digest = crypto.subtle.digest;
        crypto.subtle.digest = async (...args) => {
          const value = await digest.apply(crypto.subtle, args);
          await nvim.request('nvim_buf_set_lines', [before.bufferId, 0, -1, true, ['(def label "Raced")']]);
          return value;
        };
        let raced;
        try { raced = await editor.setDraftDiagnostics(after, note); }
        finally { crypto.subtle.digest = digest; }
        const final = await editor.draft();
        const acceptedFinal = await editor.setDraftDiagnostics(final, note);
        const cleared = await editor.setDraftDiagnostics(final, []);
        const finalFindings = await findings(before.bufferId);
        await editor.setText(source);
        const restored = await editor.draft();
        const staleAfterRestoringText = await editor.setDraftDiagnostics(before, note);
        const scratchFindings = await findings(scratchBuffer);
        await nvim.request('nvim_buf_delete', [before.bufferId, { force: true }]);
        const closedReadRejected = await editor.text().then(() => false, () => true);
        return { before, whileScratch, after, accepted, refused: [stale, wrongBuffer, wrongSession, wrongDigest, raced],
          initialFindings, clearedOnEdit, scratchText, scratchFindings, unfocusedSelectionRejected,
          acceptedFinal, cleared, finalFindings, restored, staleAfterRestoringText, closedReadRejected, changes, failures };
      } finally { editor.stop(); host.remove(); }
    }, original);
    assert.equal(pinned.before.source, original);
    assert.equal(pinned.before.sourceSha256, crypto.createHash('sha256').update(original).digest('hex'));
    assert.deepEqual(pinned.whileScratch, pinned.before);
    assert.equal(pinned.after.bufferId, pinned.before.bufferId);
    assert.ok(pinned.after.changedtick > pinned.before.changedtick);
    assert.equal(pinned.after.source, '(def label "Edited")');
    assert.deepEqual(pinned.changes[0], { text: pinned.after.source, draft: pinned.after });
    assert.deepEqual(pinned.scratchText, ['scratch']);
    assert.equal(pinned.unfocusedSelectionRejected, true);
    assert.equal(pinned.accepted, true);
    assert.deepEqual(pinned.initialFindings, ['Draft diagnostic']);
    assert.deepEqual(pinned.clearedOnEdit, []);
    assert.deepEqual(pinned.scratchFindings, []);
    assert.deepEqual(pinned.refused, [false, false, false, false, false]);
    assert.equal(pinned.acceptedFinal, true);
    assert.equal(pinned.cleared, true);
    assert.deepEqual(pinned.finalFindings, []);
    assert.equal(pinned.restored.sourceSha256, pinned.before.sourceSha256);
    assert.ok(pinned.restored.changedtick > pinned.before.changedtick);
    assert.equal(pinned.staleAfterRestoringText, false);
    assert.equal(pinned.closedReadRejected, true);
    assert.deepEqual(pinned.failures, []);
    assert.deepEqual(errors, []);
    console.log(JSON.stringify({ status: 'passed', checks: ['pinned theme source hashes', 'native Bruvbox name and highlight RGB values', 'native 16-color terminal palette', 'actual buffer and cursor preserved', 'shared CSS palette', 'pinned cue across native scratch windows', 'UTF-8 draft digest and changedtick', 'stale and racing diagnostics rejected', 'owned diagnostics cleared on edit', 'clean browser/editor teardown'], native: result.native, css: result.css }, null, 2));
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
