(ns tolglow.effects.authoring
  "Author scoped, portable light effects for single-use performance requests."
  (:require [tolglow.agents :as agents]
            [tolglow.compiler :as compiler]
            [tolglow.data :as data]
            [tolglow.registry :as registry]))

(def light-attributes (into registry/emitter-attributes [:color :dimmer]))

(def performance-attributes (into light-attributes [:pan :tilt :aim :direction :strobe :shutter :focus :zoom]))

(defn prepare
  "Validate a declarative candidate in a caller-owned fixture scope. A provider
  may describe parameters and outputs, but the caller supplies identity, scope
  and lifetime. The emitted definition can also be used from the REPL."
  [request candidate]
  (let [outputs (registry/output-entries (:outputs candidate))
        selected (set (:fixture-ids request))
        rows (into [] (keep (fn [{:keys [attribute targets] :as row}]
                              (when-not (performance-attributes attribute)
                                (throw (ex-info "Choose a lighting or motion output" {:attribute attribute})))
                              (if (nil? targets) row
                                (when-let [ids (seq (filter selected targets))]
                                  (assoc row :targets (vec ids)))))) outputs)
        definition (assoc (select-keys candidate [:name :params :envelope :ui/layout])
                          :id (:effect-id request) :version 1 :auto? false
                          :targets (vec (:fixture-ids request)) :outputs rows
                          :performance-owner (:owner-id request))]
    (when (or (empty? selected) (empty? rows))
      (throw (ex-info "The effect needs outputs on the selected fixtures" {})))
    (when-let [errors (seq (registry/validate-definition :effects definition))]
      (throw (ex-info "Invalid performance effect" {:errors errors})))
    (compiler/compile-effect definition)
    definition))

(defn- open-shutters [fixtures]
  (let [settings (for [{:keys [id channels]} fixtures
                       attr [:shutter :strobe]
                       :let [{:keys [inverted-from functions]} (get channels attr)
                             open (some #(when (#{:open :shutter-open} (:type %)) %) functions)]
                       :when open
                       :let [byte (quot (+ (:start open) (:end open)) 2)
                             drive (/ (double (if (and inverted-from (>= byte inverted-from))
                                                (+ inverted-from (- 255 byte)) byte)) 255.0)]]
                   [[attr drive] id])]
    (mapv (fn [[[attr value] rows]]
            {:attribute attr :value value :targets (mapv second rows)})
          (sort-by key (group-by first settings)))))

(defn procedural
  "Generate an emission look with adjustable level and a beat-relative pulse.
  Per-channel rows work on dimmer-only lamps and RGB/RGBW strips alike."
  [{:keys [domain serial amount]} fixtures]
  (let [level {:op :param :key :level}
        period (nth [1.0 0.5 2.0 0.25] (mod serial 4))
        pulse {:op :add :args [0.52 {:op :multiply :args [0.48 {:op :sin :args
                       [{:op :multiply :args [(/ (* 2 Math/PI) period) {:op :beat}]}]}]}]}
        wave (if (= domain :activity) {:op :multiply :args [level pulse]} level)
        fields (sort (filter (disj light-attributes :color :uv) (mapcat (comp keys :channels) fixtures)))
        accent (nth [:red :blue :green :white] (mod serial 4))
        ;; Apply the rhythmic drive once, either to a master dimmer or
        ;; directly to the emitters on fixtures without a dimmer.
        rows (into [] (for [attr (distinct fields)
                            [master? members] (sort-by key (group-by #(contains? (:channels %) :dimmer)
                                                                     (filter #(contains? (:channels %) attr) fixtures)))
                            :let [base (if (or (= attr :dimmer) (not master?)) wave 1.0)
                                  tint (if (or (= attr :dimmer) (= attr accent)) 1.0
                                         (if (= :brightness domain) 0.85 0.60))]]
                        {:attribute attr :targets (mapv :id members)
                         :value (if (== tint 1.0) base {:op :multiply :args [tint base]})}))]
    {:name (if (= domain :activity) "Beat pulse" "Warm fill")
     :ui/layout (if (= domain :activity) :pulse :wash)
     :params {:level {:type :number :bounds [0 1] :default (max 0.1 (min 1.0 amount))}}
     :outputs (into rows (open-shutters fixtures))}))

(def ^:private author-tool
  {:name "propose_effect" :description "Propose one declarative lighting effect for the requested fixture scope."
   :parameters {:type "object" :properties {:definition {:type "object"}}
                :required ["definition"] :additionalProperties false}})

(defn request!
  "Use the configured agent worker to propose an effect. Calls complete! once
  with {:request request :definition ...} or {:request request :error ...}.
  No show command is available to the provider. The host admits the result
  against the current show, scope and request token on its owner queue."
  [manager request complete!]
  (let [candidate (atom nil)
        complete (fn [result] (complete! (assoc result :request request)))]
    (try
      (agents/start-tool-job! manager
        {:messages [{:role :system :content "Compose a portable Tolglow lighting effect. Call propose_effect with its definition, then finish."}
                    {:role :user :content (str "Propose a short one-shot effect for "
                                               (pr-str (select-keys request [:domain :direction :amount :channels :duration]))
                                               ". Use an outputs map, e.g. {:color [0.8 0.1 0.1], :dimmer 0.4}, "
                                               "or declarative beat expressions. Omit targets to use the assigned fixtures.")}]
         :tools [author-tool]
         :dispatch! (fn [_ arguments _]
                      (let [definition (prepare request (data/normalize-json (:definition arguments)))]
                        (when-not (compare-and-set! candidate nil definition)
                          (throw (ex-info "A proposal was already accepted for this request" {})))
                        {:ok? true}))
         :done! (fn [{:keys [status error]}]
                  (complete (if (and (= :completed status) @candidate)
                              {:definition @candidate} {:error (or error :no-valid-proposal)})))})
      (catch Exception e
        (complete {:error (or (:agent/error (ex-data e)) :author-unavailable)})
        nil))))
