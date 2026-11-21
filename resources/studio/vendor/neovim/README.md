# Neovim WebAssembly provenance

These local assets are reused from `tolgraven/tolgraven`, checkout
`9daaea09577d9c7b80519e5c309583faf789c855`, under
`resources/public/vendor/neovim/`. Its `doc/neovim.md` records:

* Official Neovim nightly 2026-10-13, commit
  `a054b698c0b7c83e934a38df5c0021ae2cb444cc`.
* Release archive `nvim-wasm-emscripten.zip`, asset ID `636003451`, archive SHA-256
  `cb37edbf2474788d1d221e65a28da30f639ebc5b3b7f40a6f5207b79b4db225c`.
* `nvim.js`, `nvim.wasm`, `nvim.data` and `LICENSE.txt` are copied unchanged from
  that checkout. No release archive was downloaded again for this integration.
* `nvim-worker.js` is copied unchanged from that checkout, whose documented
  adaptation bounds the input diagnostic sample to fifty bytes.
* `msgpackr.js` is the vendored no-eval UMD/CommonJS build of msgpackr 2.1.0;
  `MSGPACKR-LICENSE` retains its MIT license. It is loaded as a local classic
  script to expose `globalThis.msgpackr`.
* `transport.js` adapts Tolgraven's Worker/SharedArrayBuffer/MessagePack adapter
  from CommonJS exports to an ES module, consuming that global codec.
* `studio-worker.js` wraps the copied worker to suppress per-byte debug logs.
  The studio uses the bundled Neovim runtime and built-in Clojure syntax, without
  copying Tolgraven-specific profile/plugins or its repository workspace.

The WebAssembly process uses an ephemeral browser filesystem. It has no access
to the host checkout. Saving with `:w` does not publish a lighting definition;
the studio explicitly reads the current buffer and sends it to the validation
API. Shared memory requires HTTPS or localhost and COOP `same-origin` / COEP
`require-corp`. All assets are served locally; no third-party font/CDN requests
are needed.
