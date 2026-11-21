// Actual Neovim WASM interop engine. Application state and publishing live in re-frame.
const namedKeys = { Backspace: 'BS', Enter: 'CR', Escape: 'Esc', Tab: 'Tab', ArrowUp: 'Up', ArrowDown: 'Down', ArrowLeft: 'Left', ArrowRight: 'Right', Delete: 'Del', Home: 'Home', End: 'End', PageUp: 'PageUp', PageDown: 'PageDown' };
const color = value => `#${Math.max(0, value).toString(16).padStart(6, '0')}`;
let codecPromise;
function loadCodec() {
  if (!codecPromise) codecPromise = new Promise((resolve, reject) => {
    const script = document.createElement('script'); script.src = '/studio/vendor/neovim/msgpackr.js'; script.onload = resolve; script.onerror = () => reject(new Error('Could not load Neovim MessagePack transport.')); document.head.append(script);
  });
  return codecPromise;
}
export class NeovimEngine {
  constructor(host, options = {}) {
    this.fileName = options.fileName || '/home/user/definition.edn'; this.fileType = typeof options.fileType === 'string' && /^[a-zA-Z0-9_.-]{1,40}$/.test(options.fileType) ? options.fileType : 'clojure'; this.host = host; this.onChange = options.onChange || (() => {}); this.onStatus = options.onStatus || (() => {}); this.onError = options.onError || (() => {}); this.stopped = false;
  }
  async start(source) {
    if (!window.crossOriginIsolated || typeof SharedArrayBuffer !== 'function') throw new Error('Neovim requires localhost or HTTPS and COOP/COEP isolation. The accessible editor remains available.');
    this.onStatus('Loading actual Neovim WebAssembly runtime (local assets)…');
    await loadCodec(); const { start } = await import('./vendor/neovim/transport.js');
    if (this.stopped) return;
    this.createScreen();
    this.nvim = start('/studio/vendor/neovim/studio-worker.js', events => this.redraw(events), text => { if (!this.ready) this.onStatus(text); }, code => this.onError(`Neovim exited (${code}).`), error => this.onError(error));
    await this.nvim.ready;
    const info = await this.nvim.request('nvim_get_api_info', []); this.nvim.registerTypes(info[1].types);
    await this.nvim.request('nvim_exec_lua', [`vim.opt.swapfile=false; vim.opt.backup=false; vim.opt.writebackup=false; vim.opt.number=true; vim.opt.mouse='a'; vim.opt.termguicolors=true; vim.opt.background='dark'; vim.opt.laststatus=2; vim.opt.signcolumn='no'; vim.opt.showmode=true; vim.opt.tabstop=2; vim.opt.shiftwidth=2; vim.opt.expandtab=true; vim.opt.wrap=true; vim.g.bruvbox_number_column='bg0_h'; vim.g.bruvbox_sign_column='bg0_h2'; vim.g.bruvbox_color_column='bg0_h2'; vim.g.bruvbox_fold_column='bg0_h2'; vim.g.bruvbox_vert_split='bg3'; vim.g.bruvbox_cursorline_bg='bg0_m'; vim.g.bruvbox_italic=1; vim.g.bruvbox_invert_selection=1; vim.g.bruvbox_underline_cursorline=0; vim.opt.runtimepath:prepend('/home/user/.config/nvim/wasm/bruvbox'); vim.cmd('syntax enable'); vim.cmd('colorscheme bruvbox'); vim.api.nvim_create_autocmd({'TextChanged','TextChangedI'}, {callback=function() vim.rpcnotify(1,'tolglow_changed') end})`, []]);
    const theme = await this.nvim.request('nvim_exec_lua', ["local normal=vim.api.nvim_get_hl(0,{name='Normal',link=false}); return {name=vim.g.colors_name,fg=normal.fg,bg=normal.bg}", []]);
    if (theme.name !== 'bruvbox' || theme.fg !== 0xf0e3ba || theme.bg !== 0x262525) {
      this.stop();
      throw new Error('The native Bruvbox colorscheme did not load correctly.');
    }
    await this.nvim.request('nvim_buf_set_name', [0, this.fileName]);
    await this.nvim.request('nvim_set_option_value', ['filetype', this.fileType, { buf: 0 }]);
    await this.nvim.request('nvim_buf_set_lines', [0, 0, -1, true, source.split('\n')]);
    await this.nvim.request('nvim_ui_attach', [this.cols, this.rows, { rgb: true, ext_linegrid: true }]);
    this.nvim.listen('tolglow_changed', () => {
      clearTimeout(this.changeTimer);
      this.changeTimer = setTimeout(() => this.text().then(text => this.onChange(text)).catch(error => this.onError(error.message)), 200);
    });
    if (this.stopped) return;
    this.ready = true; this.resize(); this.input.focus();
    this.onStatus('Neovim is running in WebAssembly with Bruvbox. Esc returns to normal mode; i inserts. Publish reads the actual buffer. :w writes only the temporary browser filesystem.');
  }
  async text() {
    if (!this.ready) throw new Error('Wait for Neovim to finish loading.');
    return (await this.nvim.request('nvim_buf_get_lines', [0, 0, -1, true])).join('\n');
  }
  async selection() {
    if (!this.ready) throw new Error('Wait for Neovim to finish loading.');
    const snapshot = await this.nvim.request('nvim_exec_lua', ["return {lines=vim.api.nvim_buf_get_lines(0,0,-1,true),cursor=vim.api.nvim_win_get_cursor(0),mode=vim.api.nvim_get_mode().mode,anchor=vim.fn.getpos('v')}", []]);
    const { lines, cursor, mode } = snapshot;
    const byteOffset = (line, byte) => { let chars = 0, bytes = 0; for (const character of line) { const length = new TextEncoder().encode(character).length; if (bytes + length > byte) break; bytes += length; chars += character.length; } return chars; };
    const offset = (row, column) => lines.slice(0, row - 1).reduce((n, line) => n + line.length + 1, 0) + byteOffset(lines[row - 1] || '', column);
    const caret = offset(cursor[0], cursor[1]);
    if (!['v', 'V'].includes(mode)) return { source: lines.join('\n'), cursor: caret, selected: '' };
    const anchor = snapshot.anchor;
    let start = offset(anchor[1], anchor[2] - 1), end = caret;
    if (start > end) [start, end] = [end, start];
    const source = lines.join('\n');
    if (mode === 'V') { start = source.lastIndexOf('\n', start - 1) + 1; const newline = source.indexOf('\n', end); end = newline < 0 ? source.length : newline; }
    else end += String.fromCodePoint(source.codePointAt(end) || 0).length;
    return { source, cursor: caret, selected: source.slice(start, end), start, end };
  }
  async setText(text, cursor) {
    if (!this.ready) return;
    await this.nvim.request('nvim_buf_set_lines', [0, 0, -1, true, text.split('\n')]);
    if (Number.isInteger(cursor)) { const prefix = text.slice(0, Math.max(0, cursor)).split('\n'); await this.nvim.request('nvim_win_set_cursor', [0, [prefix.length, new TextEncoder().encode(prefix.at(-1)).length]]); }
    else await this.nvim.request('nvim_command', ['normal! gg']);
  }
  stop() { this.stopped = true; this.ready = false; clearTimeout(this.changeTimer); this.resizeObserver?.disconnect(); this.nvim?.stop(); this.nvim = null; }
  createScreen() {
    const host = this.host; host.replaceChildren(); host.hidden = false;
    this.canvas = document.createElement('canvas'); this.canvas.className = 'nvim-screen'; this.canvas.setAttribute('aria-label', 'Neovim code editor. Choose accessible text editor above for screen readers.');
    this.input = document.createElement('textarea'); this.input.className = 'nvim-input'; this.input.setAttribute('aria-label', 'Neovim keyboard input'); this.input.setAttribute('autocapitalize', 'off'); this.input.setAttribute('autocomplete', 'off'); this.input.spellcheck = false;
    host.append(this.canvas, this.input); this.cols = 80; this.rows = 24; this.cells = []; this.highlights = {}; this.cursor = [0, 0]; this.foreground = 0xf0e3ba; this.background = 0x262525; this.currentMode = 'normal';
    this.canvas.addEventListener('pointerdown', event => {
      this.input.focus(); if (!this.ready) return; event.preventDefault(); const rect = this.canvas.getBoundingClientRect(); const row = Math.floor((event.clientY - rect.top) / this.cellHeight), col = Math.floor((event.clientX - rect.left) / this.cellWidth);
      this.nvim.request('nvim_input_mouse', ['left', 'press', '', 0, Math.min(this.rows - 1, row), Math.min(this.cols - 1, col)]).catch(error => this.onError(error.message));
      this.nvim.request('nvim_input_mouse', ['left', 'release', '', 0, Math.min(this.rows - 1, row), Math.min(this.cols - 1, col)]).catch(() => {});
    });
    this.input.addEventListener('keydown', event => {
      if (event.isComposing || event.metaKey || (event.ctrlKey && event.shiftKey && event.key.toLowerCase() === 'v')) return;
      const named = namedKeys[event.key], prefix = `${event.ctrlKey ? 'C-' : ''}${event.altKey ? 'A-' : ''}${named && event.shiftKey ? 'S-' : ''}`;
      const key = named ? `<${prefix}${named}>` : event.key.length === 1 ? prefix ? `<${prefix}${event.key}>` : event.key === '<' ? '<lt>' : event.key : null;
      if (key) { event.preventDefault(); this.send(key); }
    });
    this.input.addEventListener('input', event => { if (event.isComposing) return; if (this.input.value) this.paste(this.input.value); this.input.value = ''; });
    this.input.addEventListener('paste', event => { event.preventDefault(); this.paste(event.clipboardData.getData('text/plain')); });
    this.resizeObserver = new ResizeObserver(() => this.resize()); this.resizeObserver.observe(this.canvas); this.resize();
  }
  async send(keys) { if (this.ready) try { await this.nvim.request('nvim_input', [keys]); } catch (error) { this.onError(error.message); } }
  async paste(text) {
    if (!this.ready) return;
    try { if (text.length > 8000) throw new Error('Paste at most 8,000 characters at a time in Neovim; use the accessible editor for larger definitions.'); await this.nvim.request('nvim_paste', [text, false, -1]); } catch (error) { this.onError(error.message); }
  }
  resize() {
    if (!this.canvas) return;
    const bounds = this.canvas.getBoundingClientRect(); if (!bounds.width) return;
    const ratio = window.devicePixelRatio || 1; this.canvas.width = Math.round(bounds.width * ratio); this.canvas.height = Math.round(bounds.height * ratio); this.ratio = ratio;
    const context = this.canvas.getContext('2d'); context.font = '12px monospace'; this.cellWidth = context.measureText('M').width; this.cellHeight = 17;
    this.cols = Math.max(20, Math.min(240, Math.floor(bounds.width / this.cellWidth))); this.rows = Math.max(8, Math.min(80, Math.floor(bounds.height / this.cellHeight)));
    if (this.ready) this.nvim.request('nvim_ui_try_resize', [this.cols, this.rows]).catch(error => this.onError(error.message));
    this.draw();
  }
  redraw(events) {
    for (const [name, ...entries] of events) for (const entry of entries) {
      const [grid, a, b, c, d, e] = entry;
      switch (name) {
        case 'grid_resize': if (grid === 1) { this.cols = a; this.rows = b; this.cells = Array.from({ length: b }, () => Array.from({ length: a }, () => [' ', 0])); } break;
        case 'grid_clear': if (grid === 1) this.cells.forEach(row => row.fill([' ', 0])); break;
        case 'grid_line': if (grid === 1 && this.cells[a]) { let column = b, highlight = 0; for (const [text, attr, count = 1] of c) { if (attr !== undefined) highlight = attr; for (let n = 0; n < count; n++) { if (column < this.cols) this.cells[a][column] = [text, highlight]; column++; } } } break;
        case 'grid_scroll': if (grid === 1) { const old = this.cells.map(row => row.slice()); for (let row = a; row < b; row++) for (let col = c; col < d; col++) this.cells[row][col] = row + e >= a && row + e < b ? old[row + e][col] : [' ', 0]; } break;
        case 'grid_cursor_goto': if (grid === 1) this.cursor = [a, b]; break;
        case 'hl_attr_define': this.highlights[grid] = a; break;
        case 'default_colors_set': if (grid >= 0) this.foreground = grid; if (a >= 0) this.background = a; break;
        case 'mode_change': this.currentMode = grid; break;
      }
    }
    if (!this.drawPending) { this.drawPending = true; requestAnimationFrame(() => { this.drawPending = false; this.draw(); }); }
  }
  draw() {
    if (!this.canvas || !this.cellWidth) return;
    const ctx = this.canvas.getContext('2d'); ctx.setTransform(this.ratio, 0, 0, this.ratio, 0, 0); ctx.fillStyle = color(this.background); ctx.fillRect(0, 0, this.canvas.width / this.ratio, this.canvas.height / this.ratio); ctx.textBaseline = 'top';
    for (let row = 0; row < this.cells.length; row++) for (let col = 0; col < this.cells[row].length; col++) {
      const [text, attr] = this.cells[row][col], hl = this.highlights[attr] || {}; let fg = hl.foreground ?? this.foreground, bg = hl.background ?? this.background;
      if (hl.reverse) [fg, bg] = [bg, fg];
      const cursor = row === this.cursor[0] && col === this.cursor[1]; if (cursor && !this.currentMode.startsWith('insert')) [fg, bg] = [bg, fg];
      ctx.fillStyle = color(bg); ctx.fillRect(col * this.cellWidth, row * this.cellHeight, this.cellWidth + .3, this.cellHeight);
      ctx.fillStyle = color(fg); ctx.font = `${hl.italic ? 'italic ' : ''}${hl.bold ? 'bold ' : ''}12px monospace`; ctx.fillText(text, col * this.cellWidth, row * this.cellHeight + 2);
      if (hl.underline || hl.undercurl) ctx.fillRect(col * this.cellWidth, row * this.cellHeight + 15, this.cellWidth, 1);
      if (cursor && this.currentMode.startsWith('insert')) { ctx.fillStyle = color(fg); ctx.fillRect(col * this.cellWidth, row * this.cellHeight, 2, this.cellHeight); }
    }
  }
}
