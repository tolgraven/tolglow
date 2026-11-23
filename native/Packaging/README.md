# Standalone Apple Studio

The Apple Silicon Mac application opens the same re-frame Studio and Ring show host used by the browser. The app bundle includes the compiled frontend, Java dependencies, a GraalVM Community JVM and both application source checkouts. Building uses Leiningen, Node and Xcode. Running a packaged show needs macOS 14 or newer on Apple Silicon.

## Open a show

Unzip the **Tolglow-Studio-arm64** artifact from the **Packaged Apple Studio** workflow. It contains a development build with an ad hoc signature. Launch **Tolglow Studio** and choose **Open Show Folder**. The selected folder is the process working directory and its Clojure namespace root. Files referenced by the configuration can use relative paths. Only open folders whose code and configured hardware modules you intend to run.

The runner reads the first available file in this order.

1. `tolglow.edn` contains the normal Tolglow configuration. Existing `:init` stages, registered modules and `:search-in-namespaces` work here.
2. `show.clj` defines the var `show/config`. Use ordinary Clojure to compose the show and require other namespaces in the folder.

For example, place this in `show.clj` to start a preview with no physical outputs.

```clojure
(ns show)

(def config
  {:hz 40
   :init {:components [:show]
          :modules [:web-server]}})
```

Include `:web-server` in the startup order. The desktop window owns its own loopback listener on an OS-assigned port. Stop Show or quit the window to close the show and run configured shutdown handlers. The command-line equivalent for a packaged app is `open -a "Tolglow Studio" --args --open-show "/path/to/show"`.

## Edit the application

Choose **Set Up Editable Application Sources** before starting the show. Studio installs the Tolglow and Afterglow revisions used to build this release under `~/Library/Application Support/Tolglow/Sources/`. The installation works offline. **Reveal Application Sources** opens the revision pair in Finder. Each pair has its own workspaces directory. Existing branch edits survive reruns and app upgrades. The installed repositories have their upstream GitHub remotes and a shallow history. Fetch more history if a development task needs older commits.

Studio passes the pair to the existing `:show :workspace-options` registry. The application agent's fresh branches, diffs, checks and staged changes use that manager. The packaged window continues serving the release it opened while an application workspace is edited. A config may supply its own `:workspace-options`. Provision Git for edits and the normal tools for development checks such as Leiningen and Docker. The runtime includes all the tools needed for opening a show. User effects and show-specific Clojure stay in the selected show folder.

The show's `:agents` module and the **Agents** tab use the existing provider settings. A local model server runs in its own process. Configure its supported HTTP endpoint through the same provider registry as a network model. The Apple shell does not execute inference on a render thread.

## Live presentation

On browsers with video Picture in Picture and canvas capture, use **PiP**, then **Open PiP** in the Room panel. The second tap enters the system window after the first real video frame exists. This captures the presented lighting canvas, including worker-rendered frames. Closing PiP, cancelling preparation, navigating away, or unmounting the panel releases all capture tracks. An active PiP surface keeps its own render and data subscriptions while it is visible. The operating system controls background execution.

For receiver playback, choose **Share video** in Room. Select the show host’s network interface reachable from the receiver and choose **Start sharing**. The browser encodes the live Room canvas as H.264. Once the first HLS segment is ready, use **AirPlay** in Safari or the packaged WebKit app. The resolution link opens the same read-only playlist in a compatible player. **Stop sharing** releases the encoder and invalidates the receiver URL. A closed tab or failed connection also expires the receiver after 45–90 seconds without uploads. Keep Studio open during playback.

The sender probes encoder support up to 3840 × 2160 at 30 frames per second, subject to the existing canvas resolution. Run the following test with Chromium and FFmpeg to qualify the local encoder at UHD. It requests two colors from a WebGL canvas, receives its HLS segments, checks the coded 3840 × 2160 raster with FFprobe and decodes the colors independently with FFmpeg.

```sh
REQUIRE_UHD=1 node --test test/js/preview/airplay-browser.test.mjs
```

Realtime encoding and the receiver’s buffers add latency. The HLS URL grants access to this preview to anyone who has it on the selected network. Studio operator controls stay on their own listener. System screen mirroring and fullscreen are also available through Control Center. Physical AirPlay output and negotiated display modes require an actual receiver.

iPhone and iPad use the existing **Studio** tab to open an enrolled host. The local native calibration, Watch relay and sensor controls remain available. They require their ordinary permissions. The Mac host bundle cannot run as an iOS subprocess.

Foreground browser work follows the app's lifecycle. iOS and iPadOS may suspend the WebKit process after a window moves to the background. Picture in Picture and an external display do not guarantee continuous JavaScript execution. A host using a phone or tablet for opt-in computation must recover from a missing client locally. Low Power Mode, thermal pressure and display refresh adjustments can reduce throughput before suspension. Physical devices are needed to measure these cases.

## Build and verify

The workflow builds release assets on Linux and opens them on an Apple Silicon runner with the Xcode SDK. For a Mac development build, install XcodeGen, the version of Leiningen specified in the project, Node and the paired Afterglow checkout. Set `JAVA_HOME` to the desired Apple Silicon GraalVM Community JDK, then run the following from the Tolglow repository.

```sh
npm ci
npm run studio:build
lein uberjar
native/Packaging/archive-sources build/sources "$PWD" "$PWD/../afterglow"
native/Packaging/bundle-macos target/uberjar/*-standalone.jar "$PWD/build/apps" build/sources
python3 native/Tests/check_packaged_studio.py "build/apps/Tolglow Studio.app"
```

The Mac release executes JVM bytecode using GraalVM's runtime compiler. Native Image can ahead-of-time compile a fixed set of classes. Ordinary Clojure `eval`, requiring user namespaces that have never been compiled, and recompiling arbitrary application code generate new JVM classes. Keeping that capability on iOS requires an interpreter or an external compilation design compatible with iOS. GluonFX provides separate iOS device and simulator static-library targets. A generic macOS ARM64 Native Image targets a different platform ABI.

- [GraalVM Native Image compatibility](https://www.graalvm.org/latest/reference-manual/native-image/metadata/Compatibility/)
- [GluonFX iOS device and simulator toolchains](https://docs.gluonhq.com/#_platforms)
- [Apple Picture in Picture](https://developer.apple.com/documentation/avkit/adopting-picture-in-picture-in-a-standard-player)
- [Apple app background states](https://developer.apple.com/documentation/uikit/managing-your-app-s-life-cycle)
- [Apple thermal state](https://developer.apple.com/documentation/foundation/processinfo/thermalstate-swift.property)
- [Apple AirPlay screen mirroring](https://support.apple.com/en-us/102661)
