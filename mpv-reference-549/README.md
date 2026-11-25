# Tolglow #549 mpv reconnect replay

On Studio’s Connections page, the Track reference card starts **Aligned**. Stopping the local mpv process changes it to **Unavailable**. Starting mpv again and clicking **Reconnect mpv** restores alignment through a new reference session. **Patterns suspended** stays visible throughout. [6.3-second replay](reconnect.webm)

The replay also checks that a show restart invalidates the old epoch, reconnect with that epoch returns `stale-show-session`, and a new reconnect keeps the operator’s pattern override. The still shows that final state. The loopback host uses a generated silent WAV and a no-op lighting sink. [Still](connections.png) · [Replay result](replay.log)

## Revisions and validation

- Tolglow candidate `88a5321ca76d0ff45c8a06ea1e279727b454c4e5`, based on master `7efcd6f028158bfbe004a63b6b2cefa0dc72b434`.
- Afterglow `16efb8b525e01d02a27897a389960da755247d13`, as pinned by the base.
- `bin/test parity` passed 1,051 tests and 10,139 assertions. It covers recovery and verifies that output reaches safe idle while mpv cleanup is blocked.
- The replay reused the verified Studio release from `099a187f`. Both Tolglow revisions have identical Studio and shared source trees, assets, and build inputs. Both Afterglow pins have identical ClojureScript and shared sources. `main.js` SHA-256 is `97f64dbd4d58377f7a826529e741adf10b0d6b11ddb050fa878ffc27b5b80b26`.
- `reconnect.webm` is 1152 × 900 VP9 at 15 fps with no audio track. Its SHA-256 is `400da7a8de3576e461f1764cd81f7617a2651c833b624f3d7cc6d209ea4f77e7`. The still’s SHA-256 is `9e70a30d6b68098a91ef4963d02e1daaebb639b12126529a64bf2e72b192d1fb`.

## Reproduce on Linux

Use JDK 21, Leiningen with the project dependency cache populated, Node.js, Python 3, OpenSSL, Chromium, and mpv with Unix socket support. Check out the revisions above. From the Tolglow worktree, install the locked Node dependencies, build the Studio release, and install Playwright’s recording helper.

```sh
export AFTERGLOW_ROOT=/path/to/afterglow
npm ci
npm run studio:build
node node_modules/playwright-core/cli.js install ffmpeg
bash /path/to/replay-artifacts/reproduce.sh "$PWD" /path/to/replay-output
```

The script uses a temporary loopback port, a generated operator credential, and an isolated mpv socket. It stops the test host and mpv when it exits. Use `LEIN_BIN`, `STUDIO_CHROMIUM`, `MPV_BIN`, `MPV_LIB_DIR`, or `PLAYWRIGHT_BROWSERS_PATH` to select existing local installations. [Launcher](reproduce.sh) · [Host fixture](host.clj) · [Chromium replay](replay.cjs)
