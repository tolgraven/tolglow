(ns tolglow.knob-candidates
  "Generated knob candidates (#667): owner commands to generate, reroll, edit,
  keep and discard one draft linked-knob mapping, the projection every surface
  reads, and the private Room model an audition renders.

  A candidate lives under :knob-candidates in the show model. It is never
  saved, it never writes a destination, and it is not a knob: the running
  show and the saved knobs stay as they were until the operator keeps it,
  which creates an ordinary #666 knob through `tolglow.knobs` with the
  candidate's links and recipe. The pure generator is
  `tolglow.knob-candidates.generate`. See docs/generated-knobs.md."
  (:require [afterglow.control.model :as control]
            [tolglow.knob-candidates.generate :as generate]
            [tolglow.knobs :as knobs]
            [tolglow.personalities :as personalities]
            [tolglow.registry :as registry]))

(def operations
  "Owner commands that edit the candidate in the show model."
  #{:knob-candidate/generate :knob-candidate/reroll :knob-candidate/link :knob-candidate/unlink
    :knob-candidate/keep :knob-candidate/discard})

(def audition-operations
  "Owner commands that stage the candidate in the Room without changing the show model."
  #{:knob-candidate/audition})

(def draft-id
  "The Room draft ID a candidate audition uses. Only one Room draft is staged at
  a time, so an audition waits for any other draft to be applied or discarded."
  "knob-candidate")

(def ^:private seen-limit 32)

(defn- fail!
  ([message] (fail! message {}))
  ([message details] (fail! :invalid-knob-candidate message details))
  ([code message details]
   (throw (ex-info message {:control/error code :details details}))))

(defn- state [model] (:knob-candidates model))

(defn candidate
  "The current ready candidate, or nil."
  [model]
  (let [c (:candidate (state model))]
    (when (= :ready (:status c)) c)))

;; ---------------------------------------------------------------------------
;; Inputs from surfaces

(defn- keywordish [x] (if (string? x) (keyword x) x))

(defn- options-of
  "Generation options from a command. Form and JSON surfaces send text, so
  character and cue IDs are accepted as strings. A size is the two link-count
  bounds in either order, as the agent tool's schema allows; it is kept as
  [min max]. :include-knob-driven? offers destinations existing knobs drive."
  [{:keys [character cue-ids size include-knob-driven?]}]
  (when (and (some? cue-ids) (not (and (sequential? cue-ids) (every? #(or (keyword? %) (string? %)) cue-ids))))
    (fail! "Cue IDs are a list of cue identifiers"))
  (when (and (some? size) (not (and (sequential? size) (= 2 (count size)))))
    (fail! :invalid-size (str "Size is [min max] links within " generate/min-links "–" generate/max-links)
           {:size size}))
  (when-not (or (nil? include-knob-driven?) (boolean? include-knob-driven?))
    (fail! "include-knob-driven? is true or false"))
  (cond-> {}
    character (assoc :character (keywordish character))
    (seq cue-ids) (assoc :cue-ids (vec cue-ids))
    size (assoc :size (if (every? integer? size) (vec (sort size)) (vec size)))
    include-knob-driven? (assoc :include-knob-driven? true)))

(defn- knob-driven-options
  "Eligibility options for destinations existing knobs drive, from the current
  knobs: every knob-driven destination is avoided unless
  :include-knob-driven? shares them. A fresh generation and every reroll use
  this, also a reroll after a recipe repeat."
  [model options]
  (let [driven (generate/knob-driven model)]
    (cond
      (empty? driven) nil
      (:include-knob-driven? options) {:shared driven}
      :else {:avoid driven})))

(defn- recipe-avoid
  "Eligibility options that replay a recipe's own candidate: it avoids exactly
  the destinations it recorded, whatever knobs exist now (including the knob it
  was kept as), so it repeats. Only the replay uses this."
  [model recipe]
  (when-let [avoid (seq (:avoid recipe))]
    (let [driven (generate/knob-driven model)]
      {:avoid (into {} (map (fn [d] [d (get driven d [])])) avoid)})))

(defn- recipe-options
  "Generation options a recipe carries. A size is normalised and the sharing
  flag checked as a command's are."
  [recipe]
  (when-not (or (nil? (:include-knob-driven? recipe)) (boolean? (:include-knob-driven? recipe)))
    (fail! "A recipe's include-knob-driven? is true or false"))
  (cond-> (select-keys recipe [:character :size :cue-ids :include-knob-driven?])
    (and (sequential? (:size recipe)) (every? integer? (:size recipe))) (update :size #(vec (sort %)))))

(defn- default-seed
  "A seed from the command ID when none is given. Replaying the command ID
  replays its receipt, so the choice is stable for that command."
  [command]
  (mod (hash (str (:command-id command))) 100000))

(defn- personality-for
  "The personality a generation follows: the recipe's own, from the show's
  library, so a repeat gives the same links whatever feel is active now;
  otherwise the active track feel; otherwise none."
  [model recipe]
  (if-let [{:keys [id version]} (:personality recipe)]
    (let [found (get (personalities/library model) id)]
      (when-not (and found (= version (:version found)))
        (fail! "The recipe's track-feel personality is not in this show's library at that version"
               {:personality (:personality recipe)}))
      found)
    (when-not (contains? recipe :generator)
      (:personality (personalities/active model)))))

(defn- recipe-of [model {:keys [recipe from-knob]}]
  (cond
    recipe recipe
    from-knob (or (get-in model [:knobs :knobs (keywordish from-knob) :recipe])
                  (get-in model [:knobs :knobs from-knob :recipe])
                  (fail! "That knob has no generation recipe" {:knob-id from-knob}))
    :else nil))

(defn- text-link
  "A link edit from a form or JSON surface, with its textual enums as keywords."
  [link]
  (when-not (map? link) (fail! "A link must be a map"))
  (cond-> link
    (string? (:direction link)) (update :direction keyword)
    (string? (get-in link [:destination :field])) (update-in [:destination :field] keyword)
    (string? (get-in link [:destination :input])) (update-in [:destination :input] keyword)
    (string? (get-in link [:quantize :ratios])) (update-in [:quantize :ratios] keyword)
    (= "integer" (:quantize link)) (assoc :quantize :integer)))

(defn- check-links!
  "Validate links against the running show with the knob API's own rules by
  creating a throwaway knob. Nothing is kept."
  [model links]
  (knobs/apply-operation (dissoc model :knobs) {:op :knob/create :knob-id "candidate-check" :name "Candidate"
                                                :links links}))

;; ---------------------------------------------------------------------------
;; Recipes

(defn- replay-edits
  "Apply link edits, live or recorded. An unlink names an existing link. A link
  replaces the link with its ID or adds one, but never leaves two links on one
  destination: a replace or an add onto a destination another link drives is
  refused (replace that link by its ID instead). So an edit recorded against
  other link IDs is refused rather than silently dropped or doubled."
  [candidate edits]
  (reduce (fn [c {:keys [op link link-id]}]
            (case op
              :link (update c :links
                            (fn [links]
                              (let [i (first (keep-indexed #(when (= (:id link) (:id %2)) %1) links))
                                    same (first (keep-indexed #(when (and (not= i %1)
                                                                          (= (knobs/destination-key %2) (knobs/destination-key link)))
                                                                 %2)
                                                              links))]
                                (cond
                                  same (fail! :duplicate-destination
                                              (str "Link " (:id link) " drives the destination of link " (:id same)
                                                   "; replace that link by its ID")
                                              {:link-id (:id link) :existing (:id same)})
                                  i (assoc links i link)
                                  :else (conj links link)))))
              :unlink (do (when-not (some #(= link-id (:id %)) (:links c))
                            (fail! :unknown-link "Unknown link" {:link-id link-id}))
                          (update c :links #(vec (remove (fn [l] (= link-id (:id l))) %))))))
          candidate edits))

(defn- check-edits!
  "A recipe's edits come from a saved knob, so check their shape before replay."
  [edits]
  (when-not (and (or (nil? edits) (vector? edits))
                 (every? #(or (and (= :link (:op %)) (map? (:link %)))
                              (and (= :unlink (:op %)) (some? (:link-id %))))
                         edits))
    (fail! "Recipe edits are :link steps with a link map and :unlink steps with a link ID")))

(defn- from-recipe
  "Regenerate a candidate exactly from a saved recipe against the current show.
  Replayed edits pass the same checks as a live edit."
  [model recipe personality]
  (when-not (and (map? recipe) (integer? (:seed recipe)) (registry/data? recipe))
    (fail! "A recipe needs an integer :seed"))
  (when-not (or (nil? (:avoid recipe)) (and (vector? (:avoid recipe)) (every? map? (:avoid recipe))))
    (fail! "A recipe's :avoid is a vector of knob destinations"))
  (when-not (generate/generator-versions (:generator recipe generate/generator-version))
    (fail! :unsupported-generator "The recipe was made by a generator version this build does not know"
           {:generator (:generator recipe)}))
  (check-edits! (:edits recipe))
  (let [options (recipe-options recipe)
        eligibility (generate/eligibility model (merge options (recipe-avoid model recipe)))
        generated (generate/generate eligibility personality
                                     (assoc options :seed (:seed recipe)
                                            :generator (:generator recipe generate/generator-version)))]
    (if-not (and (= :ready (:status generated)) (seq (:edits recipe)))
      generated
      (let [edited (replay-edits generated (:edits recipe))]
        (when-not (<= generate/min-links (count (:links edited)) generate/max-links)
          (fail! (str "The recipe's edits leave " (count (:links edited)) " links; a candidate keeps "
                      generate/min-links " to " generate/max-links)))
        (check-links! model (:links edited))
        (-> edited (assoc :edited? true) (assoc-in [:recipe :edits] (:edits recipe)))))))

;; ---------------------------------------------------------------------------
;; Owner operations

(defn- session-result
  "Receipt fields for a generation. The receipt keeps its own :status, so the
  candidate's readiness is :candidate-status."
  [candidate]
  (assoc (if (= :ready (:status candidate))
           (select-keys candidate [:seed :character :name])
           (select-keys candidate [:message :eligible :needed]))
         :candidate-status (:status candidate)))

(defn- install [model session candidate events]
  (let [current (state model)
        id (inc (:next-id current 0))
        candidate (assoc candidate :id (str "candidate-" id))
        seen (cond-> (vec (take-last (dec seen-limit) (:seen session)))
               (= :ready (:status candidate)) (conj (generate/candidate-signature candidate)))]
    {:model (assoc model :knob-candidates {:candidate candidate :next-id id
                                           :session (assoc session :seed (:seed candidate (:seed session)) :seen seen)})
     :events events
     :result (assoc (session-result candidate) :candidate-id (:id candidate))}))

(defn- generated-name [command candidate]
  (if (and (string? (:name command)) (<= 1 (count (:name command)) 128))
    (:name command)
    (:name candidate)))

(defn apply-operation
  "Pure [model command] operation for afterglow.control.model/apply-domain-command."
  [model {:keys [op] :as command}]
  (case op
    :knob-candidate/generate
    (let [recipe (recipe-of model command)
          _ (when (and (contains? command :seed) (not (integer? (:seed command))))
              (fail! :invalid-seed "A seed is an integer" {:seed (:seed command)}))
          personality (personality-for model recipe)
          options (if recipe (recipe-options recipe) (options-of command))
          ;; The session's exclusion comes from the current knobs, so rerolls after a
          ;; recipe repeat leave out knob-driven destinations like a fresh generation.
          driven (knob-driven-options model options)
          seed (cond recipe (:seed recipe) (contains? command :seed) (:seed command) :else (default-seed command))
          candidate (if recipe
                      (from-recipe model recipe personality)
                      (generate/generate (generate/eligibility model (merge options driven)) personality
                                         (assoc options :seed seed)))
          candidate (cond-> candidate (= :ready (:status candidate)) (assoc :name (generated-name command candidate)))]
      (install model (cond-> {:options options :seed seed :seen []}
                       driven (assoc :knob-driven driven)
                       personality (assoc :personality (select-keys personality [:id :version])))
               candidate
               [{:type :knob-candidate/generated :status (:status candidate) :seed seed}]))

    :knob-candidate/reroll
    (let [{:keys [session]} (state model)
          _ (when-not session (fail! "Generate a candidate first"))
          {:keys [options seed seen personality knob-driven]} session
          feel (when personality (personality-for model {:personality personality}))
          next-candidate (generate/reroll (generate/eligibility model (merge options knob-driven)) feel options seed
                                          (set seen))
          next-candidate (cond-> next-candidate
                           (= :ready (:status next-candidate))
                           (assoc :name (generated-name command next-candidate)))]
      (install model (assoc session :seed (or (:seed next-candidate) (inc seed))) next-candidate
               [{:type :knob-candidate/rerolled :status (:status next-candidate) :seed (:seed next-candidate)}]))

    (:knob-candidate/link :knob-candidate/unlink)
    (let [current (or (candidate model) (fail! "There is no candidate to edit"))
          edit (if (= op :knob-candidate/link)
                 {:op :link :link (text-link (:link command))}
                 {:op :unlink :link-id (:link-id command)})
          edited (replay-edits current [edit])
          _ (when (< (count (:links edited)) generate/min-links)
              (fail! (str "A candidate keeps at least " generate/min-links " links; discard it instead")))
          _ (when (> (count (:links edited)) generate/max-links)
              (fail! (str "A knob drives at most " generate/max-links " links")))
          ;; The stored link is the knob API's normalized form.
          normalized (-> (check-links! model (:links edited)) :model :knobs :knobs (get "candidate-check") :links)
          edits (conj (vec (get-in current [:recipe :edits])) (if (= :link (:op edit))
                                                                 (assoc edit :link (some #(when (= (:id (:link edit)) (:id %)) %) normalized))
                                                                 edit))]
      {:model (assoc-in model [:knob-candidates :candidate]
                        (-> edited (assoc :links normalized :edited? true) (assoc-in [:recipe :edits] edits)
                            (update :explain (fn [explain] (vec (filter #(some #{(:link-id %)} (map :id normalized)) explain))))))
       :events [{:type :knob-candidate/edited :candidate-id (:id current)}]
       :result {:candidate-id (:id current) :links (count normalized)}})

    :knob-candidate/keep
    (let [current (or (candidate model) (fail! "There is no candidate to keep"))
          knob-id (or (:knob-id command)
                      (let [existing (set (keys (get-in model [:knobs :knobs])))]
                        (first (remove existing (map #(str "generated-" %) (iterate inc 1))))))
          created (knobs/apply-operation model {:op :knob/create :knob-id knob-id
                                                :name (generated-name command current)
                                                :value (double (:value command 0.0))
                                                :links (:links current) :recipe (:recipe current)})]
      {:model (update (:model created) :knob-candidates dissoc :candidate :session)
       :events (conj (vec (:events created)) {:type :knob-candidate/kept :candidate-id (:id current) :knob-id knob-id})
       :result {:knob-id knob-id :candidate-id (:id current) :recipe (:recipe current)}})

    :knob-candidate/discard
    (let [current (:candidate (state model))]
      (when-not current (fail! "There is no candidate to discard"))
      {:model (update model :knob-candidates dissoc :candidate :session)
       :events [{:type :knob-candidate/discarded :candidate-id (:id current)}]
       :result {:candidate-id (:id current)}})))

(defn restart
  "A show restart ends every launch a candidate was generated from, so the
  candidate and its reroll session go with them, like the Room draft, surface
  sessions and composition runs. The ID counter stays, so a later candidate
  never reuses an earlier candidate's ID."
  [model]
  (if-let [next-id (get-in model [:knob-candidates :next-id])]
    (assoc model :knob-candidates {:next-id next-id})
    (dissoc model :knob-candidates)))

;; ---------------------------------------------------------------------------
;; Room audition

(defn running-destinations?
  "True when a running launch carries at least one of the candidate's link
  destinations, so an audition has something to show."
  [model candidate]
  (let [running (filter #(= :running (:phase %)) (vals (:instances model)))]
    (boolean (some (fn [{{:keys [cue-id instance-id]} :destination}]
                     (some #(if instance-id (= instance-id (:id %)) (= cue-id (:cue-id %))) running))
                   (:links candidate)))))

(defn audition-model
  "The private Room model with the candidate's links resolved at knob value and
  written in, through the knob API's resolution and the shared command path.
  Only the private copy changes; nothing here reaches the live show. A sweep
  writes into the same running private model again (see the show's audition
  retune), so the private copy's command receipts are cleared first: a write
  reusing an earlier write's command ID with a new value would be refused as a
  command-ID conflict and never land. The private revision is kept, as staging
  keeps it at the live show's."
  [private candidate value]
  (let [knob {:id draft-id :name "Candidate" :value (double value) :links (:links candidate) :order 0}
        staged (-> private
                   (assoc-in [:control :history] {})
                   (assoc-in [:control :history-order] [])
                   (assoc :knobs {:schema-version knobs/schema-version :next-order 1 :knobs {draft-id knob}}))
        {resolved :model result :result} (knobs/apply-operation staged {:op :knob/set :knob-id draft-id :value (double value)})]
    (-> (reduce (fn [model [index {:keys [op] :as write}]]
                  (case op
                    :variable/set (assoc-in model [:variables (:variable write)] (:value write))
                    :param/set (let [command (-> (select-keys write [:instance-id :param :value])
                                                 (assoc :op :param/set :show-id (:show-id model) :epoch (:epoch model)
                                                        :command-id (str draft-id ":" index)
                                                        :source {:adapter :knob-candidate :session-id draft-id}))
                                     {next-model :model ack :ack} (control/apply-command model command)]
                                 (if (= :accepted (:status ack)) next-model model))))
                resolved (map-indexed vector (:writes result)))
        (dissoc :knobs)
        (cond-> (contains? private :revision) (assoc :revision (:revision private))))))

(defn preview
  "Descriptor a candidate audition carries on its Room draft."
  [candidate value]
  {:kind :knob-candidate :candidate-id (:id candidate) :value (double value) :name (:name candidate)})

;; ---------------------------------------------------------------------------
;; Projection shared by every surface

(def ^:private character-choices (mapv #(select-keys % [:id :name]) generate/characters))

(defn projection
  "The candidate with every link resolved at the audition value (or the knob's
  middle), its score, explanation and recipe; or the explanation of why too few
  parameters were eligible. audition is the Room draft's :preview and :status
  when a candidate audition is staged."
  [model audition]
  (let [{:keys [candidate session]} (state model)
        value (if (= (:candidate-id audition) (:id candidate)) (:value audition 0.5) 0.5)]
    (cond-> {:characters character-choices}
      candidate
      (assoc :status (:status candidate)
             :candidate
             (if (= :ready (:status candidate))
               (let [knob {:id draft-id :value value}]
                 (-> (select-keys candidate [:id :name :seed :character :character-name :score :explain :recipe
                                             :edited? :repeated?])
                     (assoc :links (mapv #(assoc (knobs/link-view model knob % {}) :role (get-in candidate [:roles (:id %)]))
                                         (:links candidate))
                            :value value)))
               (select-keys candidate [:id :status :message :eligible :needed :eligible-labels :excluded :selection])))
      session (assoc :session (select-keys session [:seed :options]))
      (and audition (= (:candidate-id audition) (:id candidate)))
      (assoc :audition (select-keys audition [:value :status :draft-revision :frame-revision])))))
