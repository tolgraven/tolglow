(ns tolglow.agents.forms
  "Positional REPL forms for existing cue, effect and show commands."
  (:require [tolglow.effects :as fx]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(defn- fail! [message value] (throw (ex-info message {:value value})))

(defn- latest-instance [model cue]
  (let [matches (filter #(and (= cue (:cue-id %)) (= :running (:phase %))) (vals (:instances model)))]
    (or (:id (last (sort-by :order matches))) (fail! "Cue has no running instance" cue))))

(defn- expand [model locals value depth]
  (when (> depth 32) (fail! "Form nesting or local references exceed 32 levels" depth))
  (let [child #(expand model locals % (inc depth))]
    (cond
      (or (fn? value) (and (var? value) (fn? @value))) (child (value model))
      (and (vector? value) (#{:local :instance/of} (first value)))
      (let [[op id] value]
        (when-not (= 2 (count value)) (fail! "A reference requires one identifier" value))
        (case op
          :instance/of (latest-instance model id)
          :local (if (contains? locals id) (child (get locals id)) (fail! "Unknown local reference" id))))
      (map? value) (update-vals value child)
      (vector? value) (mapv child value)
      :else value)))

(def ^:private signatures
  {:registry/publish #{3} :cue/put #{2 3} :cue/launch #{1 2} :effect/launch #{1 2}
   :param/set #{3} :instance/opacity-set #{2} :instance/gain-set #{2} :instance/envelope-set #{2}
   :instance/pause #{1} :instance/resume #{1} :instance/end #{1} :instance/remove #{1}
   :instance/opacity-toggle #{1} :cue/save-params #{2} :cue/clear-saved-params #{1}
   :group/apply #{2} :master/set #{1} :transport/set #{1} :page/put #{2}})

(defn- effect-request [model kind id options]
  (when-not (= :effects kind) (fail! "Use :effects for effect publication and :cue/put for cues" kind))
  (when-not (map? options) (fail! "Effect options must be a map" options))
  (let [constructor (case (:template options :effect)
                      :effect fx/effect :wash fx/wash :breathing-wash fx/breathing-wash :aim-sweep fx/aim-sweep
                      (fail! "Unknown effect template" (:template options)))
        previous (get-in model [:registry :effects id])
        definition (constructor id (dissoc options :template))]
    [:definition {:kind :effects :expected-version (:version previous) :definition definition}]))

(defn- cue-command [model id effect-id options]
  (when-not (map? options) (fail! "Cue options must be a map" options))
  (let [cue (assoc options :id id :effect-id effect-id)
        effect (registry/cue-effect (:registry model) cue)]
    (when-not effect (fail! "Unknown effect or version" [effect-id (:effect-version cue)]))
    (let [errors (into (registry/validate-definition :cues cue) (registry/validate-cue-params effect cue))]
      (when (seq errors) (throw (ex-info "Invalid cue" {:errors errors}))))
    {:cue cue :expected-version (get-in model [:registry :cues id :version])}))

(defn- options-map [value]
  (cond (nil? value) {} (map? value) value :else (fail! "Expected a map of options" value)))

(defn- fields [model op a b c]
  (case op
    :cue/put (cue-command model a b (options-map c))
    :cue/launch {:cue-id a :launch (options-map b)}
    :effect/launch {:effect-id a :launch (options-map b)}
    :param/set {:instance-id a :param b :value c}
    (:instance/opacity-set :instance/gain-set) {:instance-id a :value b}
    :instance/envelope-set {:instance-id a :envelope b}
    (:instance/pause :instance/resume :instance/end :instance/remove :instance/opacity-toggle) {:instance-id a}
    :cue/save-params {:cue-id a :instance-id b}
    :cue/clear-saved-params {:cue-id a}
    :group/apply {:selector a :action b}
    :master/set {:value a}
    :transport/set {:transport a}
    :page/put {:page (assoc (options-map b) :id a)}))

(defn request
  "Expand one compact form into [message-type payload] for show/submit!. Values
  may use [:instance/of cue-id] or [:local name]. Actual Clojure functions in
  trusted REPL values receive the captured show model and return data."
  ([model form] (request model form {}))
  ([model form locals]
   (when-not (and (vector? form) (map? locals)) (fail! "Expected a form vector and local bindings map" form))
   (let [op (first form)]
     (when-not (contains? (get signatures op #{}) (dec (count form))) (fail! "Unknown command or wrong number of arguments" form))
     (let [[a b c] (mapv #(expand model locals % 0) (rest form))
           [type payload] (if (= :registry/publish op) (effect-request model a b c)
                              [:command (assoc (fields model op a b c) :op op)])]
       [type (assoc payload :expected-epoch (:epoch model))]))))

(defn submit!
  "Queue an ordinary show request. Returns the owner's receipt promise."
  ([runtime form] (submit! runtime form {}))
  ([runtime form locals]
   (let [[type payload] (request (:model @(:state runtime)) form locals)]
     (show/submit! runtime type payload))))
