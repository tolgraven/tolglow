(ns tolglow.rig.roles
  "Fixture roles (#858): what job a fixture does in a rig, in words no venue
  owns. A role has a zone, a function and an optional ordinal, written as one
  keyword such as :back.beam or :floor.wash.2. The vocabulary is closed and
  versioned so a role written on one rig means the same on another; venue
  group tags stay beside it.

  A rig declares roles under [:rig :roles] as {:version 1 :assignment {role
  [fixture-or-pixel-layout-id …]}}. A declared role resolves as a group
  through the ordinary target narrowing (tolglow.spatial.targets), so a cue
  whose :selector is (selector :front.wash) launches on any rig that declares
  that role, with no fixture ID in the cue. An ordinal role also answers to
  its base role, so :floor.wash takes :floor.wash.1 and :floor.wash.2 together
  unless the rig declares :floor.wash itself.

  propose suggests roles from fixture kind, capabilities and normalised
  position. A proposal is only a proposal: roles are a statement about the
  venue, and the operator corrects them before declaring them.

    (require '[tolglow.rig.roles :as roles] '[tolglow.venues.gig-2023 :as gig])
    (def proposal (roles/propose (gig/rig)))
    (:assignment proposal)                    ; {:front.wash [:wash-1 …] …}
    (def rig (roles/assign (gig/rig) (:assignment proposal)))
    (roles/members rig :front.wash)
    ;; On a running show, through the same rig command Studio uses:
    ;; @(show/submit! runtime :rig (roles/assign (:rig (show/model runtime)) assignment))

  Everything here is pure."
  (:require [clojure.string :as str]))

(def vocabulary
  "The closed role vocabulary. A new version may add words; it never changes
  what an existing word means."
  {:id :tolglow/fixture-roles
   :version 1
   :zones [:front :back :side :floor :overhead :audience :set]
   :functions [:wash :beam :blinder :strobe :pixel :key :atmosphere]
   :max-ordinal 99})

(def ^:private zones (set (:zones vocabulary)))

(def ^:private functions (set (:functions vocabulary)))

(defn role
  "The role keyword for zone, function and an optional ordinal."
  ([zone function] (role zone function nil))
  ([zone function ordinal]
   (when-not (and (zones zone) (functions function)
                  (or (nil? ordinal) (and (pos-int? ordinal) (<= ordinal (:max-ordinal vocabulary)))))
     (throw (ex-info "Not a role in the vocabulary" {:zone zone :function function :ordinal ordinal
                                                      :vocabulary (:version vocabulary)})))
   (keyword (str (name zone) "." (name function) (when ordinal (str "." ordinal))))))

(defn parse
  "{:zone :function :ordinal} for a role keyword or string, or nil when it is
  not a role in the vocabulary."
  [role-key]
  (when (or (string? role-key) (and (keyword? role-key) (nil? (namespace role-key))))
    (let [parts (str/split (name role-key) #"\." -1)
          [zone function ordinal] parts]
      (when (<= 2 (count parts) 3)
        (let [zone (keyword zone)
              function (keyword function)
              ordinal (when (and ordinal (re-matches #"[1-9][0-9]*" ordinal)) (parse-long ordinal))]
          (when (and (zones zone) (functions function)
                     (or (= 2 (count parts)) (and ordinal (<= ordinal (:max-ordinal vocabulary)))))
            (cond-> {:zone zone :function function} ordinal (assoc :ordinal ordinal))))))))

(defn role?
  [x]
  (some? (parse x)))

(defn base-role
  "The role without its ordinal."
  [role-key]
  (when-let [{:keys [zone function]} (parse role-key)] (role zone function)))

(defn selector
  "The portable selector for a role. Roles resolve as groups, so the selector
  is accepted wherever a cue or launch takes {:group-id …}."
  [role-key]
  (when-not (role? role-key) (throw (ex-info "Not a role in the vocabulary" {:role role-key})))
  {:group-id (keyword (name role-key))})

;; Geometry -----------------------------------------------------------------

(defn bounds
  "The bounding box of every fixture and pixel head position, in metres."
  [fixtures]
  (let [points (keep :position fixtures)]
    (when (seq points)
      {:min (apply mapv min points) :max (apply mapv max points)})))

(defn npos
  "position mapped into the box so each axis runs from -1 to 1. An axis
  without extent maps to 0."
  [{:keys [min max]} position]
  (mapv (fn [lo hi p]
          (let [extent (- (double hi) (double lo))]
            (if (< extent 1e-9) 0.0 (- (/ (* 2.0 (- (double p) (double lo))) extent) 1.0))))
        min max position))

(defn- centroid [points]
  (let [n (count points)]
    (mapv #(/ (double %) n) (apply mapv + points))))

(defn units
  "The rig's fixtures as units, in patch order: an ordinary fixture is one
  unit; every head of one pixel layout (or multi-head fixture) is one unit
  placed at the heads' centroid. Each unit has :id, :heads (fixture IDs),
  :position, :channels (the union of its heads' channel names), :groups,
  :beam-angle and :pixel?."
  [fixtures]
  (let [unit-id #(or (:pixel-layout-id %) (:fixture-id %) (:id %))
        order (distinct (map unit-id fixtures))
        by-unit (group-by unit-id fixtures)]
    (mapv (fn [id]
            (let [heads (get by-unit id)
                  points (keep :position heads)]
              {:id id
               :heads (mapv :id heads)
               :position (when (seq points) (centroid points))
               :channels (into #{} (mapcat (comp keys :channels)) heads)
               :groups (into #{} (mapcat :groups) heads)
               :beam-angle (some :beam-angle heads)
               :pixel? (boolean (some #(or (:pixel-layout-id %) (:fixture-id %)) heads))}))
          order)))

(defn kind
  "The unit's kind: :pixel, :atmosphere, :mover (pan and tilt) or :static."
  [{:keys [channels pixel? groups]}]
  (cond pixel? :pixel
        (or (contains? channels :fog) (contains? groups :atmosphere)) :atmosphere
        (and (contains? channels :pan) (contains? channels :tilt)) :mover
        :else :static))

;; Proposal -----------------------------------------------------------------

(def proposal-rules
  "The thresholds propose uses, so a proposal can be explained and repeated.
  Heights are metres above the rig's floor plane (Y = 0). A side head sits
  at the rig's lateral edge below a fraction of its highest head, so a high
  truss that spans the width stays overhead, front or back. The rest are
  normalised rig-box coordinates."
  {:floor-below-m 1.0
   :side-from 0.9
   :side-below-top 0.75
   :front-from (/ 1.0 3.0)
   :back-to (/ -1.0 3.0)
   :beam-up-to-deg 30})

(defn- propose-function [unit]
  (let [{:keys [beam-up-to-deg]} proposal-rules]
    (case (kind unit)
      :pixel [:pixel "pixel heads"]
      :atmosphere [:atmosphere "fog output"]
      :mover (cond (contains? (:groups unit) :blinder) [:blinder "patched as a blinder"]
                   (<= (or (:beam-angle unit) 0) beam-up-to-deg)
                   [:beam (str "pan and tilt, beam " (or (:beam-angle unit) "unknown") " degrees")]
                   :else [:wash (str "pan and tilt, beam " (:beam-angle unit) " degrees")])
      :static (cond (contains? (:groups unit) :blinder) [:blinder "patched as a blinder"]
                    (contains? (:groups unit) :strobe) [:strobe "patched as a strobe"]
                    :else [:wash "static head"]))))

(defn- propose-zone [[x y z] position top]
  (let [{:keys [floor-below-m side-from side-below-top front-from back-to]} proposal-rules
        height (double (nth position 1))]
    (cond (< height floor-below-m) [:floor (format "%.2f m above the floor" height)]
          (and (>= (Math/abs (double x)) side-from) (< height (* side-below-top (double top))))
          [:side "at the side of the rig"]
          (>= z front-from) [:front "downstage in the rig"]
          (<= z back-to) [:back "upstage in the rig"]
          (>= y 0.0) [:overhead "high, mid-depth"]
          :else [:set "low, mid-depth"])))

(defn propose
  "Suggest a role for every unit of the rig from its kind, capabilities and
  normalised position (proposal-rules). Returns {:vocabulary :source
  :proposed :assignment {role [unit-id …]} :units [{:id :role :kind :npos
  :reason} …]}. Pixel layouts are named by layout ID in the assignment.
  Ordinals are never proposed; :key, :audience and :set roles are left to the
  operator except :set for a low mid-depth head."
  [rig]
  (let [fixtures (:fixtures rig)
        box (bounds fixtures)
        rows (vec (for [unit (units fixtures) :when (:position unit)]
                    (let [n (npos box (:position unit))
                          [function why-function] (propose-function unit)
                          [zone why-zone] (propose-zone n (:position unit) (get-in box [:max 1]))]
                      {:id (:id unit) :role (role zone function) :kind (kind unit)
                       :npos (mapv #(/ (Math/round (* 100.0 (double %))) 100.0) n)
                       :reason (str why-function "; " why-zone)})))]
    {:vocabulary (:version vocabulary)
     :source :proposed
     :assignment (reduce (fn [m {:keys [id role]}] (update m role (fnil conj []) id)) (sorted-map) rows)
     :units rows}))

;; Declared roles --------------------------------------------------------------

(defn- head-index
  "unit-or-fixture ID -> head IDs in patch order."
  [fixtures]
  (reduce (fn [m f]
            (let [m (update m (:id f) (fnil conj []) (:id f))]
              (reduce #(update %1 %2 (fnil conj []) (:id f)) m
                      (distinct (keep identity [(:pixel-layout-id f) (:fixture-id f)])))))
          {} fixtures))

(defn- patch-groups [fixtures]
  (into #{} (mapcat :groups) fixtures))

(defn- identifier? [v] (or (keyword? v) (and (string? v) (not (str/blank? v)))))

(defn errors
  "Structural errors in rig's declared roles, relative to the rig root. An
  assignment names roles from the vocabulary, each with a vector of fixture,
  head or pixel layout IDs; a fixture holds at most one role; a role (or the
  base role it answers to) may not reuse a patch group's name. IDs the rig no
  longer has are not errors (see report), so deleting a fixture never blocks
  a rig edit."
  [rig]
  (if-not (contains? rig :roles)
    []
    (let [{:keys [version assignment] :as roles} (:roles rig)
          err (fn [path code message] {:path (into [:roles] path) :code code :message message})]
      (cond
        (not (map? roles)) [(err [] :invalid-roles "Roles must be a map with :version and :assignment")]
        (not= version (:version vocabulary))
        [(err [:version] :unsupported-role-vocabulary (str "Role vocabulary version must be " (:version vocabulary)))]
        (not (map? assignment)) [(err [:assignment] :invalid-roles "Role assignment must be a map of role to IDs")]
        :else
        (let [index (head-index (:fixtures rig))
              groups (patch-groups (:fixtures rig))
              claimed (for [[r ids] assignment :when (vector? ids) id ids head (get index id [id])] [head r])]
          (vec
           (concat
            (for [[r ids] assignment
                  problem (cond
                            (not (role? r)) [[[:assignment r] :unknown-role "Not a role in the vocabulary"]]
                            (not (and (vector? ids) (every? identifier? ids)))
                            [[[:assignment r] :invalid-role-members "Role members must be a vector of IDs"]]
                            (or (groups r) (groups (base-role r)))
                            [[[:assignment r] :role-group-collision "A patch group already uses this role's name"]])]
              (apply err problem))
            (for [[head rs] (group-by first claimed)
                  :let [rs (distinct (map second rs))] :when (> (count rs) 1)]
              (err [:assignment] :duplicate-role-member
                   (str "Fixture " (pr-str head) " holds more than one role: " (str/join ", " (map name rs))))))))))))

(defn resolve-ids
  "rig with each role-member ID given the type of the fixture, head or pixel
  layout ID it names, and role names as keywords. Studio's rig editor shows
  IDs as text, so an edit it sends back names keyword fixtures by string; an
  ID matched by text alone resolves only when exactly one rig ID has that
  text."
  [rig]
  (if-not (map? (get-in rig [:roles :assignment]))
    rig
    (let [ids (vec (keys (head-index (:fixtures rig))))
          text (fn [x] (if (keyword? x) (subs (str x) 1) (str x)))
          by-text (group-by text ids)
          resolve-id (fn [id] (if (some #(= id %) ids) id
                               (let [matches (get by-text (text id))] (if (= 1 (count matches)) (first matches) id))))]
      (update-in rig [:roles :assignment]
                 (fn [assignment]
                   (into (empty assignment)
                         (map (fn [[r members]]
                                [(if (and (string? r) (role? r)) (keyword r) r)
                                 (if (vector? members) (mapv resolve-id members) members)]))
                         assignment))))))

(defn validate
  "Return rig when its roles are valid; otherwise throw ex-info with :errors."
  [rig]
  (let [problems (errors rig)]
    (when (seq problems) (throw (ex-info "Invalid fixture roles" {:rig/error :invalid-roles :errors problems})))
    rig))

(defn assign
  "rig with assignment declared as its roles, validated."
  [rig assignment]
  (validate (assoc rig :roles {:version (:version vocabulary) :assignment assignment})))

(defn groups
  "Role -> head IDs in patch order for rig's declared roles, including the
  base role of ordinal roles the rig does not declare itself. IDs the rig no
  longer has are left out, and a role left without heads does not resolve."
  [rig]
  (let [assignment (get-in rig [:roles :assignment])
        fixtures (:fixtures rig)
        index (head-index fixtures)
        order (into {} (map-indexed (fn [i f] [(:id f) i])) fixtures)
        heads (fn [ids] (vec (sort-by order (distinct (mapcat #(get index % []) ids)))))
        declared (into {} (for [[r ids] assignment
                                :let [hs (when (and (role? r) (vector? ids)) (heads ids))]
                                :when (seq hs)]
                            [(keyword (name r)) hs]))
        bases (reduce-kv (fn [m r hs]
                           (let [b (base-role r)]
                             (if (or (= b r) (contains? declared b)) m (update m b (fnil into []) hs))))
                         {} declared)]
    (merge (update-vals bases (fn [hs] (vec (sort-by order (distinct hs))))) declared)))

(defn members
  "Head IDs of role on rig, in patch order; nil when the rig does not declare it."
  [rig role-key]
  (get (groups rig) (keyword (name role-key))))

(defn report
  "What a declared assignment leaves out: IDs the rig no longer has, and
  units without a role."
  [rig]
  (let [assignment (get-in rig [:roles :assignment])
        index (head-index (:fixtures rig))
        named (set (mapcat val assignment))
        assigned (set (mapcat #(get index % []) named))]
    {:unknown-ids (vec (sort-by str (remove index named)))
     :unassigned (vec (for [u (units (:fixtures rig)) :when (not-any? assigned (:heads u))] (:id u)))}))

(defn role-map
  "The roles a translation or retargeting reads for rig: the declared ones
  when the rig has them, otherwise a proposal. :source says which."
  [rig]
  (if (contains? rig :roles)
    {:source :declared :vocabulary (get-in rig [:roles :version]) :groups (groups rig)}
    (let [proposal (propose rig)]
      {:source :proposed :vocabulary (:vocabulary proposal)
       :groups (groups (assoc rig :roles {:version (:version vocabulary) :assignment (:assignment proposal)}))})))
