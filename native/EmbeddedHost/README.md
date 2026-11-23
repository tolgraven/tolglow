# On-device Studio

The embedded host links Tolglow, Afterglow and the compiled re-frame application into an iOS framework using GluonFX and GraalVM Native Image. The full iPhone and iPad app opens an ephemeral Ring listener on its own loopback address and displays that show in its Studio tab. Each open owns an isolate and one native thread from startup through shutdown. Leaving the local show or backgrounding the app stops that instance. The ordinary remote Studio tab remains available.

## Show folders

Open **Studio → On this device → Choose show folder**. A folder can contain `tolglow.edn` or a `show.clj` defining `show/config`. EDN wins when both files exist. The Small Clojure Interpreter evaluates source in a fresh context for each show. It loads ordinary sibling namespaces from `.clj` and `.cljc` files under the selected folder. Configs can use Tolglow's existing `:init` order and `:search-in-namespaces` to add module setup functions. Interpreted code can call `tolglow.show/submit!` and `tolglow.show/variables`. Desktop JVM mode also supports adding JVM libraries and compiling arbitrary Clojure namespaces. [SCI](https://github.com/babashka/sci)

For example, place the following files in the same folder.

```clojure
;; rig.clj
(ns rig)
(defn cue-page [_ options]
  {:value (mapv #(assoc % :page (:page options)) (:cues options))})
```

```clojure
;; show.clj
(ns show)
(def config
  {:init {:components [:show]
          :modules [:web-server :cue-page]
          :search-in-namespaces '[rig]}
   :cue-page {:page :warmup :cues [{:id :wash} {:id :breathe}]}})
```

This example makes the custom module's value available to later modules at `[:modules :cue-page]`. Open **Open included show** for the demo without an external folder.

## Build and qualification

Use the paired Afterglow revision from the tests workflow. The **Embedded Apple Studio Native Image** workflow runs the production re-frame build, tests the selected folder interpreter, and packages the matching Leiningen `embedded-host` profile. It compiles the same C entry points for the iOS simulator and the iOS device ABI on their supported Gluon toolchains. Simulator and hardware Mach-O binaries have separate artifacts. [GluonFX platforms](https://docs.gluonhq.com/#_platforms) · [GraalVM compatibility](https://www.graalvm.org/latest/reference-manual/native-image/metadata/Compatibility/)

For a local build, install Xcode, XcodeGen, Maven, Node and Leiningen. In Tolglow, run the following commands with the paired Afterglow checkout next to it.

```sh
npm ci
npm run studio:build
lein with-profile +embedded-host test :only tolglow.native.show-source-test
lein with-profile +embedded-host uberjar
native/EmbeddedHost/build-framework ios-sim "$(find target -name '*-standalone.jar' -print -quit)" build/native-host
native/Tests/check-embedded-studio
```

GluonFX 1.0.29 uses Intel macOS for the x86_64 iPad simulator and Apple Silicon macOS for an arm64 device build. Use `ios` for the latter, then run `native/EmbeddedHost/generate-project`. Select your signing team in the generated Xcode project for hardware installation. The simulator test requires the actual local control page to load on two consecutive launches. App Clips and watchOS run their own clients. Backgrounding an iOS app relinquishes its active host. Processes that must keep running throughout a performance belong on the venue host.
