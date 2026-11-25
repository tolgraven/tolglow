#!/usr/bin/env python3
import argparse
from functools import partial
import gzip
import hashlib
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from io import BytesIO
import json
from pathlib import Path
from threading import Thread

from PIL import Image
from playwright.sync_api import sync_playwright


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("www", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--pairs", type=int, default=4)
    parser.add_argument("--retained-only", action="store_true")
    parser.add_argument("--chromium", default="/usr/bin/chromium")
    args = parser.parse_args()
    payloads = {}

    class Server(SimpleHTTPRequestHandler):
        def do_GET(self):
            if self.path.startswith("/plan/"):
                data = payloads.get(self.path)
                if data is None:
                    self.send_error(404)
                    return
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Content-Encoding", "gzip")
                self.send_header("Content-Length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)
                return
            super().do_GET()

        def log_message(self, *_):
            pass

    server = ThreadingHTTPServer(("127.0.0.1", 0), partial(Server, directory=str(args.www)))
    thread = Thread(target=server.serve_forever, daemon=True)
    thread.start()
    origin = f"http://127.0.0.1:{server.server_port}"

    flags = [
        "--use-gl=angle",
        "--use-angle=swiftshader",
        "--enable-unsafe-swiftshader",
        "--disable-gpu-shader-disk-cache",
    ]
    result = {"identity": json.loads((args.www / "identity.json").read_text()), "flags": flags, "samples": []}
    def checkpoint():
        args.output.joinpath("raw.json").write_text(json.dumps(result, indent=2) + "\n")

    try:
        with sync_playwright() as playwright:
            def measure(mode, shape="original", reload=False, settle=False):
                browser = playwright.chromium.launch(executable_path=args.chromium, args=flags)
                result["chromium"] = browser.version
                try:
                    page = browser.new_page(viewport={"width": 800, "height": 600}, reduced_motion="reduce")
                    errors = []
                    page.on("pageerror", lambda error: errors.append(str(error)))
                    page.goto(f"{origin}/?mode={mode}&shape={shape}", wait_until="domcontentloaded")
                    page.wait_for_function(
                        "() => window.roomStartup?.engine.ready || window.roomStartup?.engine.error"
                        " || window.roomStartup?.engine.fallback", timeout=45_000
                    )
                    page.evaluate("window.roomStartup.pause()")
                    if mode != "release":
                        page.wait_for_function("window.roomStartup.diagnostic.length > 0", timeout=10_000)
                    metrics = page.evaluate("""() => {
                      const {thread, failed, fixtureCount, activeLights, quality, shadowCount,
                        deviceRenderer, softwareRenderer, programCount, pixels, displayPixels, fogAmount}
                        = window.roomStartup.engine.getTelemetry();
                      return {startup: window.roomStartup.summary, thread, failed, fixtureCount, activeLights, quality,
                        shadowCount, deviceRenderer, softwareRenderer, programCount, pixels, displayPixels,
                        fogAmount, visibleStatus: window.roomStartup.status.textContent,
                        probe: window.roomStartup.diagnostic[0]};
                    }""")
                    if errors or metrics["failed"] or metrics["thread"] != "worker" or metrics["activeLights"] == 0:
                        raise RuntimeError((errors, metrics["visibleStatus"]))
                    if not metrics["softwareRenderer"] or "SwiftShader" not in metrics["deviceRenderer"]:
                        raise RuntimeError("Wrong renderer: " + str(metrics["deviceRenderer"]))
                    if metrics["startup"].get("show") != "room-startup":
                        raise RuntimeError("The initial frame was for the wrong show")
                    screenshot = page.locator("#room").screenshot(animations="disabled")
                    with Image.open(BytesIO(screenshot)) as image:
                        pixels = image.convert("RGB")
                        if any(low == high for low, high in pixels.getextrema()):
                            raise RuntimeError("The first Room image has no visible content")
                        metrics["imageSha256"] = hashlib.sha256(pixels.tobytes()).hexdigest()
                    if reload:
                        if settle:
                            metrics["warmupProgramCount"] = [page.evaluate("window.roomStartup.drawCurrentShow()")
                                for _ in range(2)]
                            page.wait_for_function("window.roomStartup.diagnostic.length >= 3", timeout=10_000)
                            metrics["settledProbe"] = page.evaluate("window.roomStartup.diagnostic.at(-1)")
                        old = page.evaluate("window.roomStartup.diagnostic.length")
                        metrics["reload"] = page.evaluate("window.roomStartup.reloadWithinContext()")
                        if (metrics["reload"]["show"], metrics["reload"]["epoch"]) != ("room-startup-restored", 2):
                            raise RuntimeError("Wrong show frame on retained-context reload")
                        page.wait_for_function("old => window.roomStartup.diagnostic.length > old", arg=old, timeout=10_000)
                        metrics["reload"]["probe"] = page.evaluate("window.roomStartup.diagnostic.at(-1)")
                    metrics.update({"mode": mode, "shape": shape})
                    return metrics
                finally:
                    browser.close()

            args.output.mkdir(parents=True, exist_ok=True)
            if args.retained_only:
                result["retained"] = measure("baseline", reload=True, settle=True)
                row = result["retained"]
                before = row["settledProbe"]["report"]["stages"]
                after = row["reload"]["probe"]["report"]["stages"]
                if len(before) != len(after):
                    raise RuntimeError("The settled Room compiled a new program for an identical rig")
                args.output.joinpath("retained.json").write_text(json.dumps(result, indent=2) + "\n")
                return

            capture = measure("capture")
            plan = capture["probe"].pop("plan")
            raw = lambda value: json.dumps(value, ensure_ascii=False, separators=(",", ":")).encode()
            uncompressed = raw(plan)
            payloads["/plan/restore"] = gzip.compress(uncompressed, mtime=0)
            payloads["/plan/stale"] = gzip.compress(raw(plan | {"build": "stale-build"}), mtime=0)
            result["plan"] = {
                "bytes": len(uncompressed), "gzipBytes": len(payloads["/plan/restore"]),
                "variants": sum(len(bucket) for _, bucket in plan["entries"])
            }
            result["capture"] = capture
            checkpoint()
            for pair in range(args.pairs):
                modes = ["baseline", "restore"] if pair % 2 == 0 else ["restore", "baseline"]
                for mode in modes:
                    metrics = measure(mode)
                    metrics["pair"] = pair
                    result["samples"].append(metrics)
                    checkpoint()
                    print(f'pair {pair} {mode}: {metrics["startup"]["observedMs"]:.1f} ms', flush=True)
            result["controls"] = []
            for mode, shape, reload in [
                ("release", "original", False), ("stale", "original", False),
                ("baseline", "changed", False), ("restore", "changed", False),
                ("baseline", "original", True)
            ]:
                result["controls"].append(measure(mode, shape, reload))
                checkpoint()
            def stages(sample):
                return sample["probe"]["report"]["stages"]

            if not all(all(stage["hit"] for stage in stages(sample))
                       for sample in result["samples"] if sample["mode"] == "restore"):
                raise RuntimeError("Original show did not produce all source hits")
            if any(stage["hit"] for stage in stages(result["controls"][1])):
                raise RuntimeError("Stale build unexpectedly reused GLSL")
            changed_base, changed_restore = result["controls"][2:4]
            hits = sum(stage["hit"] for stage in stages(changed_restore))
            if not 0 < hits < len(stages(changed_restore)) or changed_base["imageSha256"] != changed_restore["imageSha256"]:
                raise RuntimeError("Changed rig did not show safe fallback and reuse")
            if len({x["imageSha256"] for x in result["samples"] + result["controls"]
                    if x["shape"] == "original"}) != 1:
                raise RuntimeError("Source-plan rendering changed the comparison image")
            result["qualified"] = True
            checkpoint()
    finally:
        server.shutdown()
        thread.join()
        server.server_close()


if __name__ == "__main__":
    main()
