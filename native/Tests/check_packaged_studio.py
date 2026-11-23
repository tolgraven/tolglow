"""Boot the embedded GraalVM, execute a user's Clojure and load release assets."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile
import time
import urllib.request


def exercise(java, jar, environment):
    with tempfile.TemporaryDirectory(prefix="tolglow show: ") as path:
        folder = Path(path)
        # Require a second user namespace through the actual folder classloader.
        (folder / "fixture_setup.clj").write_text(
            '(ns fixture-setup) (defn enabled [_ _] (spit "started" "loaded") {})')
        (folder / "show.clj").write_text(
            "(ns show) (def config {:init {:components [:show] :modules [:enabled :web-server] "
            ":search-in-namespaces [(quote fixture-setup)]}})")
        ready = folder / "port"
        args = [str(java), "-Djava.awt.headless=true", "-cp", str(jar), "clojure.main",
                "-m", "tolglow.desktop", str(folder), str(ready)]
        with subprocess.Popen(args, stdin=subprocess.PIPE, cwd=folder, env=environment) as process:
            try:
                deadline = time.monotonic() + 140
                while not ready.exists():
                    if process.poll() is not None or time.monotonic() > deadline:
                        raise RuntimeError("Packaged Studio failed to start")
                    time.sleep(.1)
                assert (folder / "started").read_text() == "loaded"
                origin = "http://127.0.0.1:" + ready.read_text()
                for route, expected in [("/", b"app"), ("/studio/js/main.js", b"function")]:
                    with urllib.request.urlopen(origin + route, timeout=10) as response:
                        assert response.status == 200 and expected in response.read()
                def require_new_namespace(name):
                    filename = name.replace("-", "_") + ".clj"
                    (folder / filename).write_text(
                        f'(ns {name}) (defn mark [] (spit "{name}" "from REPL"))')
                    body = json.dumps({"source": f"(require '{name}) ({name}/mark)",
                                       "request-id": name}).encode()
                    request = urllib.request.Request(origin + "/api/repl/eval", data=body,
                                                     headers={"Content-Type": "application/json"})
                    with urllib.request.urlopen(request, timeout=15) as response:
                        events = [json.loads(line[6:]) for line in response if line.startswith(b"data: ")]
                    assert events[-1]["status"] == "completed", events
                    assert (folder / name).read_text() == "from REPL"

                require_new_namespace("late-fixture")
                with urllib.request.urlopen(origin + "/api/export", timeout=10) as response:
                    backup = response.read()
                    transit = response.headers.get_content_type()
                restore = urllib.request.Request(origin + "/api/import", data=backup,
                                                 headers={"Content-Type": transit})
                with urllib.request.urlopen(restore, timeout=20) as response:
                    result = json.load(response)
                    assert result.get("ack", result)["status"] == "accepted", result
                require_new_namespace("after-restore")
            finally:
                process.stdin.close()
                try:
                    process.wait(timeout=30)
                except subprocess.TimeoutExpired:
                    process.kill()
                    raise
            assert process.returncode == 0, process.returncode


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("app", type=Path)
    args = parser.parse_args()
    resources = args.app.resolve() / "Contents/Resources"
    env = {**os.environ, "PATH": "/usr/bin:/bin", "JAVA_HOME": "/nonexistent"}
    exercise(resources / "runtime/bin/java", resources / "studio.jar", env)
