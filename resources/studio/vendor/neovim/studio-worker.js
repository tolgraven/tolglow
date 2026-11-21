// Keep the pinned upstream worker intact, suppressing its per-byte debug logging.
// No host filesystem or arbitrary browser capabilities are exposed to Neovim.
const upstreamLog = console.log.bind(console);
console.log = (message, ...details) => {
  if (message !== 'POP BYTE' && message !== 'FS.write-----') upstreamLog(message, ...details);
};
importScripts('./nvim-worker.js');
