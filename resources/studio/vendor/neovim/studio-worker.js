// Keep the pinned upstream worker/WASM intact. Install the exact Bruvbox sources
// into its ephemeral filesystem before Neovim starts; no source bytes cross RPC.
// Source hashes and upstream MIT/X11 provenance are in bruvbox-profile.json.
const upstreamLog = console.log.bind(console);
console.log = (message, ...details) => {
  if (message !== 'POP BYTE' && message !== 'FS.write-----') upstreamLog(message, ...details);
};
let themeFiles;
const upstreamImportScripts = self.importScripts.bind(self);
self.importScripts = (...paths) => {
  upstreamImportScripts(...paths);
  if (paths.includes('./nvim.js')) {
    const create = self.createNvim;
    self.createNvim = options => create({
      ...options,
      preRun: [...(options.preRun || []), module => {
        for (const file of themeFiles) {
          const path = '/home/user/.config/nvim/wasm/' + file.path;
          module.FS.mkdirTree(path.slice(0, path.lastIndexOf('/')));
          module.FS.writeFile(path, file.text);
        }
      }],
    });
    self.importScripts = upstreamImportScripts;
  }
};
upstreamImportScripts('./nvim-worker.js');
const upstreamMessage = self.onmessage;
self.onmessage = async event => {
  if (event.data.type === 'init') {
    try {
      const manifestResponse = await fetch('./bruvbox-profile.json');
      if (!manifestResponse.ok) throw new Error('Could not load the bundled Bruvbox manifest');
      const manifest = await manifestResponse.json();
      if (manifest.version !== 1 || manifest.theme !== 'bruvbox' || !Array.isArray(manifest.files) || manifest.files.length !== 3)
        throw new Error('Unsupported Bruvbox profile');
      themeFiles = await Promise.all(manifest.files.map(async file => {
        if (typeof file.path !== 'string' || !file.path.startsWith('bruvbox/') ||
            file.path.includes('\\') || file.path.includes('\0') ||
            file.path.split('/').some(part => !part || part === '.' || part === '..') ||
            !/^[0-9a-f]{64}$/.test(file.sha256)) throw new Error('Invalid Bruvbox profile file');
        const response = await fetch('./' + file.path);
        if (!response.ok) throw new Error('Could not load a bundled Bruvbox source');
        const bytes = await response.arrayBuffer();
        if (bytes.byteLength > 131072) throw new Error('Bundled Bruvbox source exceeds its size limit');
        const digest = [...new Uint8Array(await crypto.subtle.digest('SHA-256', bytes))].map(b => b.toString(16).padStart(2, '0')).join('');
        if (digest !== file.sha256) throw new Error('Bundled Bruvbox source checksum mismatch');
        return { path: file.path, text: new TextDecoder('utf-8', { fatal: true }).decode(bytes) };
      }));
      self.postMessage({ type: 'status', text: 'Loading the bundled Bruvbox colorscheme…' });
      return await upstreamMessage(event);
    } catch (error) {
      self.postMessage({ type: 'profile-error', text: error.message });
      return;
    }
  }
  return upstreamMessage(event);
};
