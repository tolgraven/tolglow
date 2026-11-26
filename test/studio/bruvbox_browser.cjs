// Explicit real-browser/WASM gate; start the capture-only studio first and build it
// with npm run studio:build. STUDIO_CHROMIUM or CHROMIUM_PATH names the browser.
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
    executablePath: process.env.STUDIO_CHROMIUM || process.env.CHROMIUM_PATH || '/usr/bin/chromium',
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

    const closed = 'The cue buffer is closed.';
    const closedNotice = 'The cue buffer was closed in Neovim. Reopen it there or switch editors to continue.';
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
        const unfocusedSelection = await editor.selection().then(() => null, error => error.message);
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
        const failuresWhileOpen = failures.slice();
        await nvim.request('nvim_buf_delete', [before.bufferId, { force: true }]);
        const rejection = promise => promise.then(() => null, error => error.message);
        const wiped = [await rejection(editor.text()), await rejection(editor.draft()), await rejection(editor.setText('late'))];
        const wipedFinding = await editor.setDraftDiagnostics(restored, note);
        for (let wait = 0; wait < 100 && !failures.length; wait++) await new Promise(resolve => setTimeout(resolve, 20));
        return { before, whileScratch, after, accepted, refused: [stale, wrongBuffer, wrongSession, wrongDigest, raced],
          initialFindings, clearedOnEdit, scratchText, scratchFindings, unfocusedSelection,
          acceptedFinal, cleared, finalFindings, restored, staleAfterRestoringText, failuresWhileOpen, wiped, wipedFinding, changes, failures };
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
    assert.equal(pinned.unfocusedSelection, 'Focus the cue buffer before reading a selection.');
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
    assert.deepEqual(pinned.failuresWhileOpen, []);
    assert.deepEqual(pinned.wiped, [closed, closed, closed]);
    assert.equal(pinned.wipedFinding, false);
    assert.deepEqual(pinned.failures, [closedNotice]);

    // Reload, unload and reopen keep one observer on the same numbered buffer.
    const lifecycle = await page.evaluate(async source => {
      const { NeovimEngine } = await globalThis.tolglowLoadEngine('neovim');
      const host = document.createElement('div');
      host.style.cssText = 'position:fixed;inset:20px;width:800px;height:420px;z-index:1000';
      document.body.append(host);
      const changes = [], failures = [];
      const editor = new NeovimEngine(host, {
        pinBuffer: true, fileName: '/home/user/cue.clj',
        onChange: (_text, draft) => changes.push(draft), onError: error => failures.push(error),
      });
      const until = async (ready, what) => {
        for (let wait = 0; !ready(); wait++) {
          if (wait > 250) throw new Error('Timed out waiting for ' + what);
          await new Promise(resolve => setTimeout(resolve, 20));
        }
      };
      const settle = () => new Promise(resolve => setTimeout(resolve, 450));
      const rejection = promise => promise.then(() => null, error => error.message);
      try {
        await editor.start(source);
        const nvim = editor.nvim, first = await editor.draft(), buf = first.bufferId;
        let notified = 0;
        nvim.listen('tolglow_changed', () => { notified++; });
        const command = text => nvim.request('nvim_command', [text]);
        const write = lines => nvim.request('nvim_buf_set_lines', [buf, 0, -1, true, lines]);
        const findings = () => nvim.request('nvim_exec_lua', ["return #vim.diagnostic.get(...)", [buf]]);
        const delivered = text => until(() => changes.at(-1)?.source === text, 'the draft ' + text).then(() => changes.at(-1));
        const note = [{ lnum: 0, col: 0, message: 'Draft diagnostic', severity: 1 }];

        // Another file in the cue's window hides the cue buffer and leaves it the draft.
        await command('edit /home/user/other.clj');
        const other = await nvim.request('nvim_get_current_buf', []);
        await nvim.request('nvim_buf_set_lines', [other, 0, -1, true, ['(def other 1)']]);
        await settle();
        const hidden = { draft: await editor.draft(), changes: changes.length, text: await editor.text(),
          applied: await editor.setDraftDiagnostics(first, note), findings: await findings() };
        await command('buffer ' + buf);

        // :edit! reverts to the written file. The observer stays and the findings go.
        await command('write');
        await write(['(def unsaved 2)']);
        const unsaved = await delivered('(def unsaved 2)');
        const unsavedApplied = await editor.setDraftDiagnostics(unsaved, note);
        await command('edit!');
        await until(() => changes.at(-1)?.changedtick > unsaved.changedtick, 'the reloaded draft');
        const reloaded = changes.at(-1), reloadedFindings = await findings();
        const lateForUnsaved = await editor.setDraftDiagnostics(unsaved, note);
        await write(['(def reloaded 3)']);
        const afterReload = await delivered('(def reloaded 3)');

        // :bdelete unloads the buffer. Reopening loads the written file under the same number.
        await command('write');
        await write(['(def discarded 4)']);
        const discarded = await delivered('(def discarded 4)');
        await command('bdelete!');
        await until(() => failures.length > 0, 'the closed buffer notice');
        const unloaded = [await rejection(editor.text()), await rejection(editor.draft()), await rejection(editor.setText('late'))];
        const unloadedFinding = await editor.setDraftDiagnostics(discarded, note);
        const failuresWhileUnloaded = failures.slice();
        await command('buffer ' + buf);
        await until(() => changes.at(-1)?.changedtick > discarded.changedtick, 'the reopened draft');
        const reopened = changes.at(-1);
        const lateForDiscarded = await editor.setDraftDiagnostics(discarded, note);
        await write(['(def reopened 5)']);
        const afterReopen = await delivered('(def reopened 5)');
        const reopenedApplied = await editor.setDraftDiagnostics(afterReopen, note);

        // Reloading again must not stack observers: one edit, one notification.
        await command('write'); await command('edit!'); await command('edit!');
        await until(() => changes.at(-1)?.changedtick > afterReopen.changedtick, 'the twice reloaded draft');
        await settle();
        const before = notified;
        await write(['(def counted 6)']);
        await delivered('(def counted 6)');
        await settle();
        const notifiedPerEdit = notified - before;

        // A reply still in flight when the editor stops is refused, not thrown.
        const current = await editor.draft();
        const inFlight = editor.setDraftDiagnostics(current, note).then(value => ({ value }), error => ({ error: error.message }));
        editor.stop();
        return { first, hidden, unsavedApplied, reloaded, reloadedFindings, lateForUnsaved, afterReload, unloaded, unloadedFinding,
          failuresWhileUnloaded, reopened, lateForDiscarded, afterReopen, reopenedApplied, notifiedPerEdit,
          stopped: [await inFlight, await editor.setDraftDiagnostics(current, note)], failures };
      } finally { editor.stop(); host.remove(); }
    }, '(def saved 1)');
    assert.deepEqual(lifecycle.hidden, { draft: lifecycle.first, changes: 0, text: '(def saved 1)', applied: true, findings: 1 });
    assert.equal(lifecycle.unsavedApplied, true);
    assert.equal(lifecycle.reloaded.source, '(def saved 1)');
    assert.equal(lifecycle.reloaded.bufferId, lifecycle.first.bufferId);
    assert.equal(lifecycle.reloadedFindings, 0);
    assert.equal(lifecycle.lateForUnsaved, false);
    assert.ok(lifecycle.afterReload.changedtick > lifecycle.reloaded.changedtick);
    assert.deepEqual(lifecycle.unloaded, [closed, closed, closed]);
    assert.equal(lifecycle.unloadedFinding, false);
    assert.deepEqual(lifecycle.failuresWhileUnloaded, [closedNotice]);
    assert.equal(lifecycle.reopened.source, '(def reloaded 3)');
    assert.equal(lifecycle.reopened.bufferId, lifecycle.first.bufferId);
    assert.equal(lifecycle.reopened.sessionId, lifecycle.first.sessionId);
    assert.equal(lifecycle.lateForDiscarded, false);
    assert.ok(lifecycle.afterReopen.changedtick > lifecycle.reopened.changedtick);
    assert.equal(lifecycle.reopenedApplied, true);
    assert.equal(lifecycle.notifiedPerEdit, 1);
    assert.deepEqual(lifecycle.stopped, [{ value: false }, false]);
    assert.deepEqual(lifecycle.failures, [closedNotice]);

    // A finding is visible while typing: its message on its line and its own underline colour.
    const display = await page.evaluate(async source => {
      const { NeovimEngine } = await globalThis.tolglowLoadEngine('neovim');
      const host = document.createElement('div');
      host.style.cssText = 'position:fixed;inset:20px;width:800px;height:420px;z-index:1000';
      document.body.append(host);
      const failures = [];
      const editor = new NeovimEngine(host, { pinBuffer: true, fileName: '/home/user/cue.clj', onError: error => failures.push(error) });
      const until = async (ready, what) => {
        for (let wait = 0; !ready(); wait++) {
          if (wait > 150) throw new Error('Timed out waiting for ' + what);
          await new Promise(resolve => setTimeout(resolve, 20));
        }
      };
      const rowText = row => editor.cells[row].map(([text]) => text).join('');
      try {
        await editor.start(source);
        await editor.send('i');
        await until(() => editor.currentMode.startsWith('insert'), 'insert mode');
        const draft = await editor.draft();
        const line = draft.source.split('\n')[1], token = line.indexOf('oops');
        const col = new TextEncoder().encode(line.slice(0, token)).length;
        const applied = await editor.setDraftDiagnostics(draft, [{ lnum: 1, col, end_col: col + 4, message: 'Unable to resolve symbol: oops', severity: 1 }]);
        await until(() => editor.cells.some((_, row) => rowText(row).includes('Unable to resolve symbol: oops')), 'the finding message on screen');
        await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)));
        const row = editor.cells.findIndex((_, r) => rowText(r).trimStart().startsWith('2 '));
        const numberWidth = rowText(row).indexOf('(');
        const screenCol = numberWidth + Array.from(line.slice(0, token)).length;
        const cell = editor.cells[row][screenCol], hl = editor.highlights[cell[1]] || {};
        const ratio = editor.ratio, x = Math.floor((screenCol + 0.5) * editor.cellWidth * ratio), y = Math.floor((row * editor.cellHeight + 15.5) * ratio);
        const [r, g, b] = editor.canvas.getContext('2d').getImageData(x, y, 1, 1).data;
        return { applied, mode: editor.currentMode, messageRow: editor.cells.findIndex((_, r) => rowText(r).includes('Unable to resolve symbol: oops')), row,
          cell: cell[0], underlined: Boolean(hl.underline || hl.undercurl), special: hl.special, pixel: (r << 16) | (g << 8) | b, failures };
      } finally { editor.stop(); host.remove(); }
    }, '(def a 1)\n(def label "Cue ☃" oops)\n(def c 3)');
    assert.equal(display.applied, true);
    assert.ok(display.mode.startsWith('insert'));
    assert.equal(display.messageRow, display.row);
    assert.equal(display.cell, 'o');
    assert.equal(display.underlined, true);
    assert.equal(typeof display.special, 'number');
    assert.equal(display.pixel, display.special);
    assert.deepEqual(display.failures, []);

    // A remount can repeat the buffer number, changedtick and digest; only the session tells them apart.
    // Many edit and finding cycles leave a bounded number of marks, findings and observers.
    const remount = await page.evaluate(async source => {
      const { NeovimEngine } = await globalThis.tolglowLoadEngine('neovim');
      const mount = async () => {
        const host = document.createElement('div');
        host.style.cssText = 'position:fixed;inset:20px;width:800px;height:420px;z-index:1000';
        document.body.append(host);
        const editor = new NeovimEngine(host, { pinBuffer: true, fileName: '/home/user/cue.clj' });
        await editor.start(source);
        return { editor, host };
      };
      const note = [{ lnum: 0, col: 0, message: 'Draft diagnostic', severity: 1 }];
      const first = await mount();
      const firstDraft = await first.editor.draft();
      first.editor.stop(); first.host.remove();
      const second = await mount();
      try {
        const editor = second.editor, nvim = editor.nvim, secondDraft = await editor.draft(), buf = secondDraft.bufferId;
        const lateFromFirstMount = await editor.setDraftDiagnostics(firstDraft, note);
        const stoppedFirstMount = await first.editor.setDraftDiagnostics(firstDraft, note);
        const count = () => nvim.request('nvim_exec_lua', [
          "local buf=...; local marks=0; local spaces=0; for _, id in pairs(vim.api.nvim_get_namespaces()) do spaces=spaces+1; marks=marks+#vim.api.nvim_buf_get_extmarks(buf,id,0,-1,{}) end; return {marks=marks, namespaces=spaces, findings=#vim.diagnostic.get(buf), autocmds=#vim.api.nvim_get_autocmds({buffer=buf})}", [buf],
        ]);
        const sizes = [];
        let applied = 0;
        for (let cycle = 0; cycle < 60; cycle++) {
          await editor.setText(`(def cycle ${cycle})\n(def label "Cue ☃")`);
          if (await editor.setDraftDiagnostics(await editor.draft(), note)) applied++;
          if (cycle === 5 || cycle === 59) sizes.push(await count());
        }
        return { firstDraft, secondDraft, lateFromFirstMount, stoppedFirstMount, applied, sizes };
      } finally { second.editor.stop(); second.host.remove(); }
    }, original);
    assert.equal(remount.secondDraft.bufferId, remount.firstDraft.bufferId);
    assert.equal(remount.secondDraft.changedtick, remount.firstDraft.changedtick);
    assert.equal(remount.secondDraft.sourceSha256, remount.firstDraft.sourceSha256);
    assert.notEqual(remount.secondDraft.sessionId, remount.firstDraft.sessionId);
    assert.equal(remount.lateFromFirstMount, false);
    assert.equal(remount.stoppedFirstMount, false);
    assert.equal(remount.applied, 60);
    assert.deepEqual(remount.sizes[1], remount.sizes[0]);
    assert.equal(remount.sizes[1].findings, 1);
    assert.ok(remount.sizes[1].marks <= 4, `extmarks ${remount.sizes[1].marks}`);
    assert.deepEqual(errors, []);
    console.log(JSON.stringify({ status: 'passed', checks: ['pinned theme source hashes', 'native Bruvbox name and highlight RGB values', 'native 16-color terminal palette', 'actual buffer and cursor preserved', 'shared CSS palette', 'pinned cue across native scratch windows', 'UTF-8 draft digest and changedtick', 'stale and racing diagnostics rejected', 'owned diagnostics cleared on edit', 'one observer through reload, unload and reopen', 'late replies to a closed or stopped editor refused', 'finding message and underline shown while typing', 'remount refused by session', 'bounded marks, findings and observers', 'clean browser/editor teardown'], native: result.native, css: result.css }, null, 2));
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
