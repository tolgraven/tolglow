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
