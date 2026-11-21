# Bruvbox source and license

Bruvbox is authored by **tolgraven**, derived from **morhetz's Gruvbox**. The
upstream README declares **MIT/X11**; its license section and the source authors'
headers are retained verbatim in this directory. The upstream repository at the
pinned revision does not contain a separate LICENSE file.

Canonical repository: <https://github.com/tolgraven/bruvbox>.

The native `colors/bruvbox.vim`, `autoload/bruvbox.vim` and `README.md` here are
copied byte-for-byte from the Bruvbox subset of
`tolgraven/tolgraven/resources/public/vendor/neovim/profile.json`, checkout
`9daaea09577d9c7b80519e5c309583faf789c855`. That profile explicitly identifies
Bruvbox base commit `bfb60742ba37a7ec967dece0007f55f18fe15e00` with local
modifications. We preserve those authored modifications; we do not mislabel them
as an unmodified upstream checkout. `../bruvbox-profile.json` records each exact
SHA256, and the browser verifies those hashes before installing the files into
Neovim's temporary filesystem.

The HTML/REPL token palette follows Tolgraven's
`src/frontend/website/modules/highlight/theme.cljs`, which cites
`tolgraven/bruvbox` commit `dd115f1133ce40f83711584fad51d83796de6898`,
[`colors/bruvbox.vim`](https://github.com/tolgraven/bruvbox/blob/dd115f1133ce40f83711584fad51d83796de6898/colors/bruvbox.vim).
The dark Normal colors are `#f0e3ba` on `#262525`. xterm uses the native theme's
actual `terminal_color_0` through `terminal_color_15` mapping, including its
different bright-color choices.

No vendored Neovim WASM/runtime binary is modified. Bruvbox is loaded with its
native `colorscheme bruvbox` command; applying a few similarly named CSS colors
is not used as a substitute for loading the colorscheme.
