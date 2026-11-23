# Standalone Apple Studio

The Apple Silicon Mac application opens the same re-frame Studio and Ring show host used by the browser. The app bundle includes the compiled frontend, Java dependencies, a GraalVM Community JVM and both application source checkouts. Building uses Leiningen, Node and Xcode. Running a packaged show needs macOS 14 or newer on Apple Silicon.

The **Compiled macOS Studio** artifact adds a native Apple Silicon executable to that bundle. The launch window offers **Compiled Clojure** and **Full JVM Clojure**. The compiled engine runs the local show and Room UI with editable interpreted show code. Full JVM Clojure supports arbitrary JVM libraries and runtime compilation. Both engines use the selected show folder, the application workspace manager and the bundled audience bridge. The app remembers the selected engine. [Compiled runtime, folder format and local REPL](../EmbeddedHost/README.md)

## Open a show

For the full JVM window, unzip **Tolglow-Studio-arm64** from **Packaged Apple Studio**. For the window with both engines, unzip **Tolglow-compiled-Studio-arm64** from **Compiled macOS Studio**. The packaged development app has an ad hoc signature. You can verify its bundled code locally with the command below. If macOS prompts about the developer when you open the downloaded build, follow Apple’s per-app approval steps for a build and workflow revision you trust. [Open an app on your Mac](https://support.apple.com/guide/mac-help/open-a-mac-app-from-an-unknown-developer-mh40616/mac)

```sh
codesign --verify --deep --strict "Tolglow Studio.app"
```

Launch **Tolglow Studio** and choose **Open Show Folder**. The selected folder is the process working directory and its Clojure namespace root. Files referenced by the configuration can use relative paths. Only open folders whose code and configured hardware modules you intend to run.

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

For AirPlay, select **Screen Mirroring** in Control Center and fullscreen the Room panel. On a Mac with an extended AirPlay display, move the Studio window to the external screen before entering fullscreen. Output detail follows the source canvas, GPU budget, negotiated display mode and receiver. A 4K receiver can still negotiate a lower resolution. A captured MediaStream has no receiver-fetchable media URL. Receiver-side video playback independent of screen mirroring requires a separate encoded feed.

iPhone and iPad use **Studio** to open an enrolled host. The embedded-host build also offers **On this device** for a local show in the selected folder. That host runs in a Native Image isolate inside the application and stops when the app enters the background. Calibration, Watch relay and sensors use their ordinary permissions.

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

The bundled GraalVM JVM executes JVM bytecode, including user namespaces that compile new classes. The compiled Mac host and iOS framework use a fixed Native Image and a separate SCI context for editable show code. GluonFX produces different Mach-O products for the iOS simulator and the iOS device ABI. A physical device requires an appropriately signed app.

- [GraalVM Native Image compatibility](https://www.graalvm.org/latest/reference-manual/native-image/metadata/Compatibility/)
- [GluonFX iOS device and simulator toolchains](https://docs.gluonhq.com/#_platforms)
- [Apple Picture in Picture](https://developer.apple.com/documentation/avkit/adopting-picture-in-picture-in-a-standard-player)
- [Apple app background states](https://developer.apple.com/documentation/uikit/managing-your-app-s-life-cycle)
- [Apple thermal state](https://developer.apple.com/documentation/foundation/processinfo/thermalstate-swift.property)
- [Apple AirPlay screen mirroring](https://support.apple.com/en-us/102661)
