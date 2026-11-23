(ns model-lab.show-export
  (:require [afterglow.time :as time]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [tolglow.agents.forms :as forms]
            [tolglow.data :as data]
            [tolglow.demo :as demo]
            [tolglow.effects :as fx]
            [tolglow.registry :as registry]
            [tolglow.show :as show])
  (:import [java.nio.file Files StandardCopyOption]
           [java.security MessageDigest]
           [java.util HexFormat]))

(def effects [:ember :merlot :orchid :indigo :ice :sage :amber :moon])
(def palette [[0.85 0.12 0.24] [0.24 0.64 0.52] [0.05 0.22 0.64] [0.75 0.55 0.12]])
(def durations [{:beats 1/4} {:beats 1/2} {:beats 2} {:bars 1} {:bars 3} {:phrases 1}])
(defn named [prefix & parts] (keyword "tour" (str/join "-" (cons prefix (map #(if (keyword? %) (name %) %) parts)))))
(defn effect-form [id options] [:registry/publish :effects id options])
(defn entry [family prompt form & [setup]] {:family family :prompt prompt :form form :setup (or setup [])})
(defn latest [cue] [:instance/of cue])
(defn dur [d] (let [[unit amount] (first d)] (str amount " " (name unit))))

(defn authoring []
  (concat
   (for [[i color] (map-indexed vector palette) level [0.15 0.55 0.9]
         :let [id (named "wash" i (int (* 100 level)))]]
     (entry :author/color-wash (str "Publish " id " as an editable wash. Use linear RGB " color " and default level " level ".")
            (effect-form id {:template :wash :color color :level level})))
   (for [[i duration] (map-indexed vector durations) shape [:sine :triangle :saw :pulse]
         :let [id (named "breath" shape i) color (nth palette (mod i 4))]]
     (entry :author/breathing-wash (str "Make " id " breathe in " color " with a " (name shape) " cycle every " (dur duration) ".")
            (effect-form id {:template :breathing-wash :color color :wave {:shape shape :period duration}})))
   (for [[i center] (map-indexed vector [[0 1 1] [-2 1 3] [3 2 -1] [1 0 2]]) width [0.2 0.9 1.5] zoom [0.12 0.55]
         :let [id (named "sweep" i (int (* 100 width)) (int (* 100 zoom)))]]
     (entry :author/aim-sweep (str "Publish an aim sweep named " id " around " center ", width " width " metres and zoom " zoom ".")
            (effect-form id {:template :aim-sweep :center center :width width :zoom zoom})))
   (for [[i color] (map-indexed vector palette) level [0.18 0.42 0.78] focus [0.17 0.65]
         :let [id (named "beam" i (int (* level 100)) (int (* focus 100)))]]
     (entry :author/typed-outputs (str "Create " id " with constant color " color ", focus " focus ", and a normalized :level dimmer parameter starting at " level ".")
            (effect-form id {:params {:level level} :outputs {:color color :dimmer {:op :param :key :level} :focus focus}})))
   (for [id effects [i duration] (map-indexed vector (take 4 durations)) :let [k (named "accent" id i)]]
     (entry :author/cue-definition (str "Register cue " k " for existing effect " id " with duration " (dur duration) " and priority 20.")
            [:cue/put k id {:duration duration :priority 20}]))))

(defn launches []
  (concat
   (for [id effects opts [{:mode :persistent} {:mode :one-shot :duration {:beats 1/3}}
                          {:mode :timed :duration {:bars 2}} {:mode :one-shot :duration {:phrases 1}}]]
     (entry :launch/musical (str "Launch cue " id " in " (name (:mode opts)) " mode"
                                 (when-let [d (:duration opts)] (str " for " (dur d))) ".") [:cue/launch id opts]))
   (for [id effects targets [[:head-1] [:head-3 :head-4] [:head-2 :head-6 :head-8]] level [0.28 0.81]]
     (entry :launch/fixture-selection (str "Launch cue " id " on " targets " only. Override :level to " level ".")
            [:cue/launch id {:targets targets :params {:level level}}]))
   (for [id (conj effects :spot :breathe) priority [1 15 75]]
     (entry :launch/effect-direct (str "Start effect " id " directly at priority " priority " without launching a cue.")
            [:effect/launch id {:priority priority}]))))

(defn performance []
  (concat
   (for [id effects v [0 0.09 0.35 0.77 1]]
     (entry :perform/parameter (str "Set :level on the newest running " id " cue to " v ".")
            [:param/set (latest id) :level v] [[:cue/launch id]]))
   (for [id effects [op v] [[:instance/opacity-set 0] [:instance/opacity-set 0.37] [:instance/gain-set 1.25] [:instance/gain-set 1.8]]]
     (entry :perform/alpha-gain (str "Set " (if (= op :instance/opacity-set) "opacity" "gain") " to " v " on the newest running " id " cue.")
            [op (latest id) v] [[:cue/launch id]]))
   (for [id effects [op verb] [[:instance/pause "Pause"] [:instance/resume "Resume"] [:instance/end "Release"]
                                [:instance/remove "Immediately remove"] [:instance/opacity-toggle "Toggle opacity on"]]]
     (entry :perform/lifecycle (str verb " the latest running instance of " id ".") [op (latest id)]
            (cond-> [[:cue/launch id]] (= op :instance/resume) (conj [:instance/pause (latest id)]))))
   (for [id effects [a r] [[{:beats 1/8} {:beats 1/2}] [{:beats 1} {:bars 1}] [{:bars 2} {:phrases 1}]]]
     (entry :perform/envelope (str "Set the latest " id " instance's attack to " (dur a) " and release to " (dur r) ".")
            [:instance/envelope-set (latest id) {:attack a :release r}] [[:cue/launch id]]))
   (for [id effects :let [v 0.61]]
     (entry :perform/preset (str "Save the active " id " cue's edited parameter values as its defaults for future launches. Its :level is currently " v ".")
            [:cue/save-params id (latest id)] [[:cue/launch id] [:param/set (latest id) :level v]]))
   (for [[a b] (partition 2 effects) [action description] [[{:op :instance/opacity-set :value 0.3} "set opacity to 0.3"]
                                                             [{:op :instance/gain-set :value 1.4} "raise gain to 1.4"]
                                                             [{:op :instance/end} "start release"]]]
     (assoc (entry :perform/group (str "For active instances of cues " a " and " b ", " description " together.")
                   [:group/apply {:cue-ids [a b]} action] [[:cue/launch a] [:cue/launch b]]) :expected-members 2))
   (for [bpm [78 96 110 124 132 142 160] playing? [true false]]
     (entry :show/transport (str "Set tempo to " bpm " BPM and " (if playing? "start" "pause") " the transport.")
            [:transport/set {:bpm bpm :playing? playing?}]))
   (for [v [0 0.08 0.24 0.42 0.7 0.92 1]]
     (entry :show/master (str "Set the global master to " v " in normalized units.") [:master/set v]))
   (for [x (range 4) y (range 3) :let [id (nth effects (mod (+ x y) 8)) page (named "page" x y)]]
     (entry :author/page (str "Create page " page " with cue " id " at column " x ", row " y " (zero-based).")
            [:page/put page {:cells {[x y] id}}]))))

(defmacro repl-program [bindings expression]
  ;; Bindings follow the same lexical template in the compiled check and example text.
  `(let [values# (zipmap '~bindings [~@bindings])]
     {:program (walk/postwalk-replace values# '~expression)
      :run! (fn [~'runtime] ~expression)}))

(defn repl-examples []
  (concat
   (for [scale [0.15 0.33 0.5 0.75 0.95]]
     (merge {:family :repl/read-show :prompt (str "At the REPL, scale the current show master by " scale " in one queued form.")}
            (repl-program [scale] (forms/submit! runtime [:master/set (fn [model] (* scale (:master model)))]))))
   (for [id effects amount [0.22 0.58 0.8]]
     (merge {:family :repl/local-bindings :prompt (str "Set the current " id " cue's level via a local binding named :amount, whose value is " amount ".")
             :setup [[:cue/launch id]]}
            (repl-program [id amount] (forms/submit! runtime [:param/set [:instance/of id] :level [:local :amount]] {:amount amount}))))
   (for [[i color] (map-indexed vector palette) [j period] (map-indexed vector durations)
         :let [id (named "lfo" i j)]]
     (merge {:family :repl/composed-wave :prompt (str "Use the Clojure effect helpers to publish " id " with fixed RGB " color
                                                     " and triangle dimmer cycling every " (dur period) " between 0.1 and 0.9.")}
            (repl-program [id color period]
                          (forms/submit! runtime [:registry/publish :effects id
                                                   {:outputs {:color color :dimmer (fx/triangle {:period period :low 0.1 :high 0.9})}}]))))
   (for [cue effects :let [id (named "follow" cue)]]
     (merge {:family :repl/registry-default :prompt (str "Register a wash named " id " whose initial level is the current " cue
                                                         " effect's registry default for :level. Supply a resolver function.")}
            (repl-program [id cue]
                          (forms/submit! runtime [:registry/publish :effects id
                                                   {:template :wash :level (fn [model] (get-in model [:registry :effects cue :params :level :default]))}]))))))

(defn questions [model]
  (let [registry (:registry model)]
    (concat
     (for [[id cue] (sort-by key (:cues registry))
           :let [effect (registry/cue-effect registry cue)]]
       {:family :query/cue-effect :prompt (str "Which effect does cue " id " launch in the demo show?")
        :context (select-keys cue [:id :effect-id :effect-version :priority])
        :answer (:id effect) :schema-path "src/tolglow/registry.clj"})
     (for [[id effect] (sort-by key (:effects registry)) [k schema] (sort-by key (:params effect))
           [field wording] [[:default "default"] [:bounds "allowed bounds"]] :when (contains? schema field)]
       {:family :query/parameter-schema :prompt (str "Return the " wording " of " id " parameter " k " from this schema.")
        :context (select-keys schema [:type :default :bounds :unit])
        :answer (get schema field) :schema-path "src/tolglow/demo.clj"})
     (for [[[x y] id] (sort-by key (get-in model [:pages :main :cells]))]
       {:family :query/page :prompt (str "Which demo cue occupies :main column " x ", row " y " (zero-based)?")
        :context (get-in model [:pages :main :cells]) :answer id :schema-path "src/tolglow/demo.clj"})
     (for [beats-per-bar [3 4 7/2] bars-per-phrase [4 8] length [{:bars 2} {:bars 4} {:phrases 1/2} {:phrases 2}]
           :when (or (contains? length :phrases) (= bars-per-phrase 4))
           :let [meter {:beats-per-bar beats-per-bar :bars-per-phrase bars-per-phrase}]]
       {:family :query/musical-time :prompt (str "In this meter, how many quarter-note beats does " (dur length) " span?")
        :context meter :answer (time/duration->beats length meter)
        :schema-repo "tolgraven/afterglow" :schema-path "src/afterglow/time.clj"})
     (for [id effects v [0.32 0.76] :let [schema (get-in registry [:effects id :params])]]
       {:family :query/parameter-defaults :prompt (str "For " id ", resolve the parameter map when the launch supplies " {:level v} ".")
        :context schema :answer (registry/resolve-params schema {:level v}) :schema-path "src/tolglow/registry.clj"}))))

(def split-by-family
  {:author/aim-sweep :test :launch/effect-direct :valid :perform/envelope :test
   :author/page :valid :repl/registry-default :test :query/page :test :query/parameter-schema :valid :query/musical-time :test})

(defn sha [text]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))))

(defn- consume! [runtime receipt step time-ms]
  (show/tick! runtime time-ms)
  (let [result (deref receipt 2000 {:status :timeout})]
    (when-not (= :accepted (:status result))
      (throw (ex-info "Corpus scenario failed the live show owner" {:step step :receipt result})))
    result))

(defn verify! [{:keys [form program run! setup expected-members]}]
  (when (or form program)
    (let [runtime (show/create-runtime {:workspace-options nil :workers 1})]
      (try
        (doseq [[i step] (map-indexed vector setup)]
          (consume! runtime (forms/submit! runtime (data/read-edn (pr-str step))) step (* i 25)))
        (let [receipt (if program (run! runtime) (forms/submit! runtime (data/read-edn (pr-str form))))
              ack (consume! runtime receipt (or form program) (* (count setup) 25))]
          (when (and expected-members (not= expected-members (count (:instance-ids ack))))
            (throw (ex-info "Group selected an unexpected number of instances" {:step form :receipt ack}))))
        (finally (show/stop! runtime))))))

(defn record [{:keys [family prompt form program answer setup schema-repo schema-path context]}]
  (let [family-name (subs (str family) 1) response (pr-str (or form program answer))
        query (if context (str prompt "\n\n" (pr-str context)) prompt)
        system (cond form "Return one compact Tolglow show form as EDN."
                     program "Return one Clojure REPL form. The runtime is named runtime, forms aliases tolglow.agents.forms, and fx aliases tolglow.effects."
                     :else "Answer using the data in the Tolglow demo registry. Return EDN.")]
    {:id (subs (sha (str family-name "\n" query "\n" response)) 0 24)
     :family (str "show/" family-name) :mode (cond form "show/edn" program "show/clojure" :else "show/qa")
     :source (cond-> {:repo (or schema-repo "tolgraven/tolglow") :path (or schema-path "src/tolglow/agents/forms.clj")
                      :scope "owner-local" :fixture "tolglow.demo/model"}
               (seq setup) (assoc :setup-edn (pr-str setup)))
     :messages [{:role "system" :content system} {:role "user" :content query} {:role "assistant" :content response}]}))

(defn -main [directory]
  (let [dir (io/file directory) parent (or (.getParentFile dir) (io/file "."))
        examples (vec (concat (authoring) (launches) (performance) (repl-examples) (questions (demo/model))))]
    (when (.exists dir) (throw (ex-info "Choose a new output directory" {:directory directory})))
    (doseq [example examples] (verify! example))
    (let [splits (group-by #(get split-by-family (:family %) :train) examples)
          records (mapv record examples)]
      (when-not (= (count records) (count (set (map :id records)))) (throw (ex-info "Duplicate corpus IDs" {})))
      (.mkdirs parent)
      (let [stage (.toFile (Files/createTempDirectory (.toPath parent) "show-corpus-" (make-array java.nio.file.attribute.FileAttribute 0)))]
        (try
          (doseq [[split examples] splits]
            (with-open [out (io/writer (io/file stage (str (name split) ".jsonl")))]
              (doseq [example examples] (.write out (str (json/write-str (record example)) "\n")))))
          (let [manifest {:examples (count records) :counts (update-vals splits count) :scope "owner-local"
                          :source-sha256 (into {} (for [f ["src/tolglow/agents/forms.clj" "dev/model-lab/show/export.clj" "src/tolglow/demo.clj"]]
                                                    [f (sha (slurp f))]))}]
            (spit (io/file stage "manifest.json") (str (json/write-str manifest) "\n"))
            (Files/move (.toPath stage) (.toPath dir) (into-array java.nio.file.CopyOption [StandardCopyOption/ATOMIC_MOVE]))
            (println (json/write-str manifest)))
          (finally (doseq [file (reverse (file-seq stage))] (io/delete-file file true))))))))

(try
  (-main (or (first *command-line-args*) "target/model-lab-show"))
  (finally (shutdown-agents)))
