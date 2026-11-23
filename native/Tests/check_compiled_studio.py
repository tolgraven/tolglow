"""Exercise the packaged compiled Clojure host, its interpreter and real Studio assets."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
import urllib.request


def wait_for_origin(process, ready):
    deadline = time.monotonic() + 120
    while not ready.exists():
        if process.poll() is not None:
            raise RuntimeError(f"Show host exited with status {process.returncode}")
        if time.monotonic() >= deadline:
            raise TimeoutError("Show host did not publish its port")
        time.sleep(0.05)
    return f"http://127.0.0.1:{int(ready.read_text())}"


def evaluate(origin, request_id, source):
    request = urllib.request.Request(
        origin + "/api/repl/eval",
        data=json.dumps({"request-id": request_id, "source": source}).encode(),
        headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(request, timeout=20) as response:
        events = [json.loads(line[6:]) for line in response if line.startswith(b"data: ")]
    assert events and events[-1]["status"] == "completed", events
    return events[-1]["result"]


def check_restored_session(origin, folder):
    assert evaluate(origin, "read-rig", "(require 'rig) (rig/hz)") == "40"
    with urllib.request.urlopen(origin + "/api/export", timeout=10) as response:
        saved = response.read()
        content_type = response.headers.get_content_type()
    restore = urllib.request.Request(origin + "/api/import", data=saved,
                                     headers={"Content-Type": content_type})
    with urllib.request.urlopen(restore, timeout=20) as response:
        result = json.load(response)
        assert result.get("ack", result)["status"] == "accepted", result
    (folder / "swing.clj").write_text("(ns swing) (defn range-at [v] [(- v 6) (+ v 6)])")
    assert evaluate(origin, "restored-rig", "(require 'swing) (swing/range-at 40)") == "[34 46]"


def check(app):
    executable = app.resolve() / "Contents/Helpers/TolglowNativeHost"
    assert os.access(executable, os.X_OK)
    with tempfile.TemporaryDirectory(prefix="Tolglow native ") as temporary:
        root = Path(temporary)
        (root / "rig.cljc").write_text("(ns rig) (defn hz [] (* 20 2))")
        (root / "show.clj").write_text(
            "(ns show (:require [rig :as rig]))"
            "(def config {:hz (rig/hz) :init {:components [:show] :modules [:web-server]}})")
        ready = root / "port"
        for _ in range(2):
            with subprocess.Popen([executable, root, ready], cwd=root, stdin=subprocess.PIPE,
                                  env={**os.environ, "PATH": "/usr/bin:/bin", "JAVA_HOME": "/nonexistent"}) as process:
                try:
                    origin = wait_for_origin(process, ready)
                    for resource, expected in [("/", b"<html"), ("/studio/js/main.js", b"function")]:
                        with urllib.request.urlopen(origin + resource, timeout=20) as result:
                            assert result.status == 200 and expected in result.read(), resource
                    with urllib.request.urlopen(origin + "/api/state", timeout=20) as result:
                        state = json.load(result)
                        assert isinstance(state, dict) and state
                    check_restored_session(origin, root)
                except BaseException:
                    process.kill()
                    raise
                finally:
                    process.stdin.close()
                    try:
                        process.wait(timeout=20)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        raise
                assert process.returncode == 0
            ready.unlink()


if __name__ == "__main__":
    check(Path(sys.argv[1]))
