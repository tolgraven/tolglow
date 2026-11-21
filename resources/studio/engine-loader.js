// Interop loading only. The ClojureScript application owns all views and state.
// Keep native module loading outside Closure's classic-script compilation.
(() => {
  const modules = new Map();
  globalThis.tolglowLoadEngine = name => {
    const path = { preview: '/studio/preview.js', neovim: '/studio/neovim-engine.js' }[name];
    if (!path) return Promise.reject(new Error('Unknown studio engine'));
    if (!modules.has(name)) modules.set(name, import(path));
    return modules.get(name);
  };
})();
