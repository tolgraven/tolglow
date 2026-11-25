#!/usr/bin/env bash
set -euo pipefail

recipe="$(cd "$(dirname "$0")" && pwd)"
worktree="$(cd "${1:?Pass a Tolglow worktree with the Studio release built}" && pwd)"
output="${2:-$recipe}"
lein_bin="${LEIN_BIN:-lein}"
mkdir -p "$output"
output="$(cd "$output" && pwd)"
scratch="$(mktemp -d)"
export TOLGLOW_REFERENCE_QA="$scratch" TOLGLOW_REFERENCE_EVIDENCE="$output"
export TOLGLOW_REFERENCE_TEST_AUTH
TOLGLOW_REFERENCE_TEST_AUTH="$(openssl rand -hex 24)"
export NODE_PATH="$worktree/node_modules"
host_pid=

cleanup() {
  if [[ -n "$host_pid" ]]; then
    touch "$scratch/stop"
    for attempt in {1..40}; do
      kill -0 "$host_pid" 2>/dev/null || break
      sleep 0.1
    done
    kill "$host_pid" 2>/dev/null || true
    wait "$host_pid" 2>/dev/null || true
  fi
  rm -rf -- "$scratch"
}
trap cleanup EXIT

python3 - "$scratch/track.wav" <<'PY'
import sys
import wave
with wave.open(sys.argv[1], "wb") as track:
    track.setparams((1, 2, 16000, 0, "NONE", "not compressed"))
    track.writeframes(bytes(45 * 16000 * 2))
PY

cd "$worktree"
test -s resources/studio/js/main.js
"$lein_bin" -o run -m clojure.main "$recipe/host.clj" > "$scratch/studio.log" 2>&1 &
host_pid=$!
for attempt in {1..180}; do
  test -s "$scratch/port" && break
  kill -0 "$host_pid" 2>/dev/null || { echo "Studio failed to start" >&2; exit 1; }
  sleep 1
done
test -s "$scratch/port"
node "$recipe/replay.cjs" | tee "$output/replay.log"
