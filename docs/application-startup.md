# Starting a Tolglow show

Run `lein run` from the project root. It reads the ordered
configuration in [resources/tolglow.edn](../resources/tolglow.edn). The supplied
configuration starts a 40 Hz preview show and serves Studio on loopback port
8500. Physical output and device acquisition use explicit settings.

```sh
lein run --headless true
lein run --config ./club.edn
lein run --config my.venue/friday --port 8600
```

`--config` accepts one EDN map or a qualified Clojure Var containing a map.
Set `TOLGLOW_CONFIG` to the same path or Var to select the venue whenever
`lein run` or zero-argument `app/start!` launches. An
explicit `--config` takes precedence. The installed project can therefore use its own fixtures, registry, functions
and setup modules from code. CLI overrides apply only to flags supplied. The
same entry point accepts `--output ola`, the OLA and Art-Net address options,
and `--trust-event-callbacks`.

## Config structure

The configuration follows the original `tolglow.config/data` convention.
`:init :components` lists required setup functions in order. `:init :modules`
and `:init :post` run next. Every module reads its top-level settings by ID.
Both `:osc false` and `:osc {:enabled false}` skip setup. Missing `:enabled`
means run. Required components always run. Before opening resources, Tolglow
checks the entire plan for duplicate IDs and missing enabled setup functions.

```clojure
(require '[tolglow.application :as app]
         '[tolglow.modules :as modules]
         '[tolglow.show :as show])

(def venue
  (-> (app/default-config)
      (assoc :hz 40 :web-server false
             :clock-sync {:enabled true :selected-source :midi-clock
                          :midi {:enabled? true :input-index 2}}
             :sensor-osc {:enabled true :address "127.0.0.1" :port-in 16012})
      (update-in [:init :post] conj :opening)))

(defn opening [context options]
  (let [runtime (get-in context [:modules :show])]
    {:value @(show/submit! runtime {:op :master/set :value 0.65})}))

(def running (app/start! venue (modules/register app/registry :opening opening)))
(:loaded-components (:application running))
(show/snapshot running)
(app/stop! running)
```

A module receives the context and its settings without `:enabled`. Its return
map can contain `:value` for later modules and `:stop!`, a zero-argument
cleanup. `app/stop!` runs cleanups once in reverse order. A startup failure
unwinds completed modules before propagating the error. For functions in your
own code, add `:init {:search-in-namespaces [my.venue.setup] ...}` to resolve
public functions whose names match module IDs; explicitly registered functions
take precedence.

## Built-in modules

| ID | Startup action |
| --- | --- |
| `:show` | Creates the runtime and starts its render and output schedules. Top-level `:hz` sets the frame rate. `:show` accepts runtime options including `:model`, `:workers`, `:ola` and `:artnet`. |
| `:fixtures` | Publishes fixture type `:definitions` and expands named `:patches` groups or installs a full fixture vector using the rig validator. See [fixture patch groups](fixture-patch-groups.md). |
| `:cue-pages` | Installs `:definitions` in order. Each page has an ID and a map from grid coordinates to registered cue IDs. |
| `:show-state` | Loads `:load-file` as a full Transit backup, runs `:actions` (command maps or zero-argument Clojure functions) in order and saves to `:save-file` during shutdown if configured. |
| `:sensors` | Installs `:sources` keyed by device ID. Each entry has the source's portable `:config` and `:pairing`. |
| `:osc` | Connects a generic OSC controller on `:port-in`. Its `:mappings` maps paths to commands, including cue launches and parameter changes. Set `:address` and `:port-out` for an outbound peer. Call the module value’s `:send!` with a path and OSC arguments. |
| `:sensor-osc` | Opens the paired sensor OSC receiver on `:address` and `:port-in`. Its actual bound port is in the module value. |
| `:clock-sync` | Configures the show’s synchronization manager and opens the enabled sources. An original `{:type :midi :device "…"}` selects a unique JVM MIDI input by name. |
| `:audio` | Starts PCM capture using the show audio manager and capture config. |
| `:waveforms` | Publishes `:definitions` to the waveform registry before subsequent modules run. |
| `:expressive` | Applies the same validated device/parameter mappings as Studio. |
| `:agents` | Configures the owned agent manager. An empty options map makes the manager available without initiating a conversation. |
| `:web-server` | Attaches Ring/Jetty to the existing show. Host and actual bound port are available in the module value. |

Modules enable startup actions. The REPL can configure the same runtime and
operate it after launch. The application keeps process handles outside show
backups. Full show files preserve the saved model and its connection settings.

## Original startup behavior

The effects branch at `01146ec` runs `tolglow.core/activate-show`, initializes
logging, then passes the three `:init` lists to `tolglow.setup/init-by-ks`. Its
built-in setup functions include fixture patches, cue pages, controllers and
clock sync. Some depend on the original mutable Afterglow show. Calling that
entry point explicitly continues to run those functions. The functional host
accepts a prepared model in `:show {:model ...}`. The configured fixture module accepts group lists, channel records and pixel heads. Original page builders that draw directly on the mutable grid still need functional equivalents.

A smoke run used the original `tolglow.core/-main` with `:set-ns` required
and the OSC and terminal modules disabled. It confirmed the original order
and exposed a boolean flag bug: `:logging false` still ran. Both loaders now
honor boolean false as well as a settings map.
