# Neovim WebAssembly provenance

The runtime assets are the official Neovim WebAssembly nightly published on
2026-11-21, commit `f2f87f5fc55fbb4c0307bd775d625515b2de8dea`.
Stable Neovim 0.12.6 does not publish a WebAssembly artifact, so this existing
nightly distribution channel is kept explicit.

* Release archive `nvim-wasm-emscripten.zip`, asset ID `774513857`, archive SHA-256
  `0a0a6ad258df89b2f59012d3c0a0d3be0f0c043684a4d441a824a464eaa08d8c`.
* `nvim.js`, `nvim.wasm` and `nvim.data` were copied unchanged from that verified
  official archive. Individual hashes and release provenance are in
  [runtime-version.json](runtime-version.json). The preserved Apache-2.0 license
  originated from the initial Tolgraven integration.
* The transport integration originates from `tolgraven/tolgraven`, checkout
  `9daaea09577d9c7b80519e5c309583faf789c855`, under
  `resources/public/vendor/neovim/`. Its `doc/neovim.md` records the initial build.
* `nvim-worker.js` is copied unchanged from that checkout, whose documented
  adaptation bounds the input diagnostic sample to fifty bytes.
* `msgpackr.js` is the vendored no-eval UMD/CommonJS build of msgpackr 2.1.4;
  `MSGPACKR-LICENSE` retains its MIT license. The npm archive integrity was verified before extracting the unchanged
  `dist/index-no-eval.min.js` bundle. It is loaded as a local classic
  script to expose `globalThis.msgpackr`.
* `transport.js` adapts Tolgraven's Worker/SharedArrayBuffer/MessagePack adapter
  from CommonJS exports to an ES module, consuming that global codec.
* `studio-worker.js` wraps the copied worker to suppress per-byte debug logs and
  install the exact Bruvbox theme subset before startup. Its manifest verifies
  source SHA256 values. See [Bruvbox provenance and MIT/X11 notice](bruvbox/NOTICE.md).
  The studio retains the bundled runtime and built-in language syntax; it does
  not copy Tolgraven's repository workspace or unrelated profile plugins.
* `neovim-engine.js` enables the native `colorscheme bruvbox` and verifies both
  `g:colors_name` and the Normal foreground/background RGB values. Editor text,
  selection and cursor behavior continue to use Neovim's actual buffer/API.

The WebAssembly process uses an ephemeral browser filesystem. It has no access
to the host checkout. Saving with `:w` does not publish a lighting definition;
the studio explicitly reads the current buffer and sends it to the validation
API. Shared memory requires HTTPS or localhost and COOP `same-origin` / COEP
`require-corp`. All assets are served locally; no third-party font/CDN requests
are needed.
