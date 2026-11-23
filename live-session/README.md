# Connected Push 3 browser screen replay

The normal show host accepted MIDI control changes from the captured-button fixture on a Push 3 session. It rendered actual re-frame views in offscreen Chromium via the local Ring page, converted every visible pixel of each recorded screen to the production USB transfer format, decoded the payload and compared every pixel against the source at 5/6/5 precision. The final receipt includes the recorded handle release and the MIDI error list. Nine fixtures were configured, and the real show's eight moving-head paths and fogger cue ran. The USB bus and MIDI transport used a recording boundary, so this does not photograph or time physical hardware.

From a Tolglow checkout with the Studio production browser and preview assets built, Node Playwright installed, Chromium available at /usr/bin/chromium, and the paired Afterglow source in AFTERGLOW_ROOT, use `lein run -m clojure.main /path/to/replay.clj`. The recorder writes native-size screenshots under /tmp/push3-browser-wire-flow. The fit/panorama and selected-fixture bounds come from the actual Studio WebGL scene.

The accompanying GIF steps through the four unmodified 960 × 160 screenshots. It holds each captured state for inspection. The earlier simulator.gif at the root of the evidence branch records 24 successive venue states.
