(ns tolglow.template-groups-test
  (:require [clojure.test :refer :all]
            [tolglow.application :as app]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.template-groups :as template-groups]
            [tolglow.transit :as wire]))

(defmacro with-runtime [[name] & body]
  `(let [~name (show/create-runtime)]
     (try ~@body (finally (app/stop! ~name)))))

(defn- apply-at! [runtime type payload time]
  (let [reply (show/submit! runtime type payload)]
    (show/tick! runtime time)
    (deref reply 1000 ::missing)))

(defn- saved [runtime id]
  (some #(when (= id (:id %)) %) (:template-groups (show/snapshot runtime))))

(defn- members-of
  "Running member instances of a placement, by member index."
  [runtime layer-id]
  (into (sorted-map)
        (keep (fn [instance]
                (let [{:keys [id member]} (get-in instance [:source :native-layer])]
                  (when (= layer-id id) [member instance]))))
        (:instances (show/snapshot runtime))))

(defn- rejection [ack] (get-in ack [:details :errors 0 :path]))

(def look
  {:id :verse-look :name "Verse look"
   :roles [{:id :wash :name "Washes" :selector {:fixture-ids [:head-1 :head-2]}}
           {:id :beams :name "Beams"}]
   :members [{:id :fill :cue-id :ember :role :wash}
             {:id :pulse :cue-id :ice :role :beams}
             {:id :spot :cue-id :spot}]})

(deftest a-template-group-saves-with-an-owner-version
  (with-runtime [r]
    (testing "a new group starts at version 1"
      (let [ack (apply-at! r :template-group look 0)]
        (is (= [:accepted :verse-look 1] ((juxt :status :id :version) ack))))
      (is (= {:id :verse-look :name "Verse look" :version 1 :placements [] :needs-binding [:beams]
              :roles (:roles look) :members (:members look)}
             (saved r :verse-look))))
    (testing "a rename keeps the version; a member change raises it"
      (is (= 1 (:version (apply-at! r :template-group (assoc look :name "Verse") 1))))
      (is (= 2 (:version (apply-at! r :template-group (update look :members pop) 2))))
      (is (= 3 (:version (apply-at! r :template-group look 3)))))
    (testing "a snapshot read back saves without its view keys or a client version"
      (is (= 3 (:version (apply-at! r :template-group (assoc (saved r :verse-look) :version 99) 4))))
      (is (not (contains? (get-in @(:state r) [:model :extensions template-groups/state-key :verse-look]) :placements))))
    (testing "invalid definitions are refused in the template's own terms"
      (doseq [[definition path] [[(assoc look :roles [{:id :wash} {:id :wash}]) [:roles]]
                                 [(assoc-in look [:roles 0 :selector] {:nonsense 1}) [:roles 0 :selector]]
                                 [(assoc-in look [:members 0 :role] :nobody) [:members 0 :role]]
                                 [(assoc-in look [:members 0 :launch] {:selector :all}) [:members 0 :launch]]
                                 [(update-in look [:members 1] dissoc :id) [:members 1 :id]]
                                 [(assoc-in look [:members 2 :id] :fill) [:members]]
                                 [(assoc-in look [:members 2 :cue-id] :no-such-cue) [:members 2]]
                                 [(assoc look :members []) [:members]]
                                 [(assoc look :cards [{:slot-id :ghost :size :large}]) [:cards 0 :slot-id]]]]
        (let [ack (apply-at! r :template-group definition 5)]
          (is (= :rejected (:status ack)) (pr-str definition))
          (is (= path (rejection ack)) (pr-str definition))))
      (let [ack (apply-at! r :template-group (assoc look :cards [{:slot-id :ghost :size :large}]) 5)]
        (is (re-find #"template group" (get-in ack [:details :errors 0 :message])))
        (is (not (re-find #"macro" (get-in ack [:details :errors 0 :message])))))
      (is (= 3 (:version (saved r :verse-look)))))))

(deftest placements-bind-roles-to-fixtures-and-run-side-by-side
  (with-runtime [r]
    (apply-at! r :template-group look 0)
    (testing "a role without a default must be bound, and nothing starts"
      (let [ack (apply-at! r :template-group-launch {:id :verse-look} 1)]
        (is (= [:rejected [:bindings :beams]] [(:status ack) (rejection ack)])))
      (is (empty? (:instances (show/snapshot r)))))
    (testing "an unknown role or fixture is refused before anything starts"
      (doseq [[bindings path] [[{:beams {:fixture-ids [:head-3]} :ghost :all} [:bindings :ghost]]
                               [{:beams {:fixture-ids [:head-99]}} [:bindings :beams]]
                               [{:beams {:group-id :nowhere}} [:bindings :beams]]
                               [{:beams "head-3"} [:bindings :beams]]]]
        (let [ack (apply-at! r :template-group-launch {:id :verse-look :bindings bindings} 2)]
          (is (= [:rejected path] [(:status ack) (rejection ack)]) (pr-str bindings))))
      (is (empty? (:instances (show/snapshot r)))))
    (testing "the main placement launches every member on its role's fixtures"
      (let [ack (apply-at! r :template-group-launch {:id :verse-look :bindings {:beams {:fixture-ids [:head-3]}}} 10)
            {:keys [layer-id member-slots bindings stale?] :as placement} (first (:placements (saved r :verse-look)))
            members (members-of r layer-id)]
        (is (= [:accepted :main "template-group/verse-look/main"] ((juxt :status :placement :run-id) ack)))
        (is (= :main (:placement placement)))
        (is (= [:fill :pulse :spot] member-slots))
        (is (= {:wash {:fixture-ids [:head-1 :head-2]} :beams {:fixture-ids [:head-3]}} bindings))
        (is (false? stale?))
        (is (= [:ember :ice :spot] (mapv :cue-id (vals members))))
        (is (= [[:head-1 :head-2] [:head-3]] (mapv :targets (take 2 (vals members)))))
        (is (not (contains? (get members 2) :targets)) "a member without a role keeps its cue's own targets")))
    (testing "a second placement runs beside it on other fixtures"
      (is (= :accepted (:status (apply-at! r :template-group-launch
                                           {:id :verse-look :placement :left
                                            :bindings {:wash {:fixture-ids [:head-5]} :beams {:fixture-ids [:head-6]}}} 20))))
      (let [[left main] (:placements (saved r :verse-look))]
        (is (= [:left :main] [(:placement left) (:placement main)]))
        (is (= [[:head-5] [:head-6]] (mapv :targets (take 2 (vals (members-of r (:layer-id left)))))))
        (is (= 3 (count (members-of r (:layer-id main)))))))
    (testing "launching a placement again replaces only that placement"
      (let [main-layer (:layer-id (second (:placements (saved r :verse-look))))]
        (apply-at! r :template-group-launch {:id :verse-look :placement :left :bindings {:beams {:fixture-ids [:head-7]}}} 30)
        (show/tick! r 5000)
        (let [[left main] (:placements (saved r :verse-look))]
          (is (= main-layer (:layer-id main)))
          (is (= [[:head-1 :head-2] [:head-7]] (mapv :targets (take 2 (vals (members-of r (:layer-id left)))))))
          (is (= 6 (count (:instances (show/snapshot r))))))))
    (testing "an edited template marks running placements stale until they launch again"
      (apply-at! r :template-group (update look :members pop) 5100)
      (is (every? :stale? (:placements (saved r :verse-look))))
      (is (= 6 (count (:instances (show/snapshot r)))) "running placements keep their captured members")
      (apply-at! r :template-group-launch {:id :verse-look :placement :left :bindings {:beams {:fixture-ids [:head-7]}}} 5200)
      (show/tick! r 10000)
      (is (= [false true] (mapv :stale? (:placements (saved r :verse-look)))))
      (is (= 2 (count (members-of r (:layer-id (first (:placements (saved r :verse-look)))))))))
    (testing "stopping one placement releases only its instances"
      (let [main-layer (:layer-id (second (:placements (saved r :verse-look))))]
        (is (= :accepted (:status (apply-at! r :template-group-stop {:id :verse-look :placement :left} 10100))))
        (show/tick! r 20000)
        (is (= [:main] (mapv :placement (:placements (saved r :verse-look)))))
        (is (= (set (map :id (:instances (show/snapshot r)))) (set (map :id (vals (members-of r main-layer))))))))
    (testing "removal waits for the placements to stop"
      (is (= :rejected (:status (apply-at! r :template-group-remove {:id :verse-look} 20100))))
      (is (= :accepted (:status (apply-at! r :template-group-stop {:id :verse-look} 20200))))
      (show/tick! r 30000)
      (is (empty? (:instances (show/snapshot r))))
      (is (= :accepted (:status (apply-at! r :template-group-remove {:id :verse-look} 30100))))
      (is (nil? (saved r :verse-look))))))

(deftest the-placement-run-prefix-is-reserved
  (testing "a saved macro cannot take a running placement's run"
    (with-runtime [r]
      (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember}]) 0)
      (is (= :accepted (:status (apply-at! r :template-group-launch {:id :verse-look} 1))))
      (doseq [id ["template-group/verse-look/main" :template-group/verse-look "template-group/anything"]]
        (let [ack (apply-at! r :composition {:id id :mode :macro :slots [{:id :x :cue-id :moon}]} 2)]
          (is (= [:rejected [:id]] [(:status ack) (rejection ack)]) (pr-str id))))
      (is (= :rejected (:status (apply-at! r :composition-run {:id "template-group/verse-look/main"} 3))))
      (is (= [:main] (mapv :placement (:placements (saved r :verse-look)))) "the placement keeps its run")))
  (testing "a macro saved first cannot hold a placement's ID either"
    (with-runtime [r]
      (let [ack (apply-at! r :composition {:id "template-group/verse-look/main" :mode :macro :slots [{:id :x :cue-id :ice}]} 0)]
        (is (= :rejected (:status ack))))
      (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember}]) 1)
      (is (= :accepted (:status (apply-at! r :template-group-launch {:id :verse-look} 2))))
      (is (= [:ember] (mapv :cue-id (:instances (show/snapshot r))))))))

(deftest run-ids-never-collide-between-templates
  (with-runtime [r]
    (testing "a template ID or placement name with / is refused"
      (is (= [:rejected [:id]] ((juxt :status rejection) (apply-at! r :template-group {:id :a/b :members [{:id :m :cue-id :ember}]} 0))))
      (is (= [:rejected [:id]] ((juxt :status rejection) (apply-at! r :template-group {:id "a/b" :members [{:id :m :cue-id :ember}]} 0))))
      (apply-at! r :template-group {:id "a" :members [{:id :n :cue-id :ice}]} 1)
      (is (= [:rejected [:placement]] ((juxt :status rejection) (apply-at! r :template-group-launch {:id "a" :placement "b/c"} 2))))
      (is (= [:rejected [:placement]] ((juxt :status rejection) (apply-at! r :template-group-launch {:id "a" :placement :b/c} 2))))
      (is (empty? (:instances (show/snapshot r)))))
    (testing "a launch onto a run another template holds is refused"
      ;; Only a hand-edited show could make this happen; the guard stays.
      (apply-at! r :template-group {:id "b" :members [{:id :m :cue-id :ember}]} 3)
      (apply-at! r :template-group-launch {:id "b"} 4)
      (swap! (:state r) update-in [:model :composition-runs]
             (fn [runs] (assoc runs "template-group/a/main" (get runs "template-group/b/main"))))
      (is (= [:rejected [:placement]] ((juxt :status rejection) (apply-at! r :template-group-launch {:id "a"} 5)))))))

(deftest a-save-can-require-a-free-id-or-the-version-it-read
  (with-runtime [r]
    (apply-at! r :template-group look 0)
    (testing "create-only refuses a taken ID and leaves the template alone"
      (let [ack (apply-at! r :template-group {:id :verse-look :expected-version nil :members [{:id :a :cue-id :moon}]} 1)]
        (is (= [:rejected [:id]] [(:status ack) (rejection ack)])))
      (is (= [1 (:roles look)] ((juxt :version :roles) (saved r :verse-look)))))
    (testing "a stale version is refused, the current one replaces"
      (is (= [:rejected [:expected-version]]
             ((juxt :status rejection) (apply-at! r :template-group (assoc look :expected-version 7 :name "X") 2))))
      (is (= [:accepted 1] ((juxt :status :version) (apply-at! r :template-group (assoc look :expected-version 1 :name "X") 3))))
      (is (= :rejected (:status (apply-at! r :template-group (assoc look :expected-version "1") 4)))))
    (testing "create-only accepts a free ID, and opacity is kept and validated"
      (is (= [:accepted 1] ((juxt :status :version)
                            (apply-at! r :template-group {:id :opening :expected-version nil :opacity 0.5
                                                          :members [{:id :a :cue-id :moon}]} 5))))
      (is (= 0.5 (:opacity (saved r :opening))))
      (is (= [:rejected [:opacity]] ((juxt :status rejection) (apply-at! r :template-group {:id :o2 :opacity 2 :members [{:id :a :cue-id :moon}]} 6))))
      (apply-at! r :template-group-launch {:id :opening} 7)
      (is (= 0.5 (get-in (first (:instances (show/snapshot r))) [:source :native-layer :opacity]))
          "the placement's layer uses the template's opacity")
      (is (= 2 (:version (apply-at! r :template-group {:id :opening :opacity 0.75 :members [{:id :a :cue-id :moon}]} 8)))
          "an opacity change is a launch change"))))

(deftest bindings-must-select-fixtures-and-only-used-roles-resolve
  (with-runtime [r]
    (apply-at! r :template-group (assoc-in look [:roles 1 :selector] {:fixture-ids [:head-3]}) 0)
    (testing "an empty group or fixture list is refused"
      (doseq [[bindings path] [[{:wash {:fixture-ids []}} [:bindings :wash]]
                               [{:beams {:fixture-ids []}} [:bindings :beams]]]]
        (let [ack (apply-at! r :template-group-launch {:id :verse-look :placement :empty :bindings bindings} 1)]
          (is (= [:rejected path] [(:status ack) (rejection ack)]) (pr-str bindings))
          (is (re-find #"selects no fixtures" (get-in ack [:details :errors 0 :message])))))
      (is (empty? (:instances (show/snapshot r)))))
    (testing "an unused role's stale default does not block a launch"
      (apply-at! r :template-group (-> look (assoc-in [:roles 1 :selector] {:group-id :gone})
                                       (assoc :members [{:id :fill :cue-id :ember :role :wash}])) 2)
      (is (= :accepted (:status (apply-at! r :template-group-launch {:id :verse-look} 3))))
      (is (= {:wash {:fixture-ids [:head-1 :head-2]}} (:bindings (first (:placements (saved r :verse-look)))))))))

(deftest stop-refuses-what-is-not-there
  (with-runtime [r]
    (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember}]) 0)
    (is (= [:rejected [:id]] ((juxt :status rejection) (apply-at! r :template-group-stop {:id :verse-lok} 1))))
    (is (= [:rejected [:placement]] ((juxt :status rejection) (apply-at! r :template-group-stop {:id :verse-look :placement :main} 1))))
    (is (= :accepted (:status (apply-at! r :template-group-stop {:id :verse-look} 1))) "stopping every placement of a saved template is idempotent")
    (apply-at! r :template-group-launch {:id :verse-look} 2)
    (is (= :accepted (:status (apply-at! r :template-group-stop {:id :verse-look :placement "main"} 3))))))

(deftest errors-use-template-words-from-the-shared-validators
  (with-runtime [r]
    (doseq [[definition path pattern] [[{:id :t1 :members [{:id :a :kind :empty}]} [:members 0] #"template group member needs a cue"]
                                       [{:id :t2 :members [{:id :a :cue-id :ember :targets :all}]} [:members 0 :targets] #"earlier members"]
                                       [{:id :t3 :members [{:id :a :cue-id :nope}]} [:members 0] #"A member needs a registered cue"]
                                       [{:id :t4 :members [{:id :a :cue-id :ember :params {:level 9}}]} [:members 0 :params :level] #"."]
                                       [{:id :t5 :members [{:id :a :cue-id :ember}] :cards [{:slot-id :b :size :small}]}
                                        [:cards 0 :slot-id] #"A card names a member of this template group"]
                                       [{:id :t6 :members [{:id :a :cue-id :ember}] :cards [{:slot-id :a :size :small} {:slot-id :a :size :large}]}
                                        [:cards] #"A member has one card"]]]
      (let [ack (apply-at! r :template-group definition 0)
            messages (cons (:message ack) (map :message (get-in ack [:details :errors])))]
        (is (= [:rejected path] [(:status ack) (rejection ack)]) (pr-str definition))
        (is (re-find pattern (get-in ack [:details :errors 0 :message])) (pr-str definition))
        (is (not-any? #(re-find #"(?i)\b(macro|slot)s?\b" (str %)) messages) (pr-str messages))))
    (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember}]) 1)
    (let [ack (apply-at! r :template-group-cards {:id :verse-look :resize {:slot-id :ghost :size :small}} 2)]
      (is (= "A card names a member of this template group" (get-in ack [:details :errors 0 :message]))))))

(deftest orphan-placements-are-listed-and-stoppable
  (with-runtime [r]
    (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember} {:id :b :cue-id :ice}]) 0)
    (apply-at! r :template-group-launch {:id :verse-look} 1)
    (let [exported (:body ((studio/handler r) {:request-method :get :uri "/api/export"}))
          orphaned (wire/encode (update-in (wire/decode exported) [:model :extensions] dissoc template-groups/state-key))]
      (is (= :accepted (:status (apply-at! r :import {:source orphaned} 2))))
      (let [{:keys [orphan? members placements]} (saved r :verse-look)]
        (is (true? orphan?))
        (is (= [:a :b] (mapv :id members)))
        (is (= [[:main false]] (mapv (juxt :placement :stale?) placements)))
        (is (= 2 (count (members-of r (:layer-id (first placements)))))))
      (is (= :accepted (:status (apply-at! r :template-group-stop {:id :verse-look} 3))))
      (show/tick! r 10000)
      (is (nil? (saved r :verse-look)))
      (is (empty? (:instances (show/snapshot r)))))))

(deftest a-load-checks-placement-template-data
  (with-runtime [r]
    (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember}]) 0)
    (apply-at! r :template-group-launch {:id :verse-look} 1)
    (let [document (wire/decode (:body ((studio/handler r) {:request-method :get :uri "/api/export"})))
          run-path [:model :composition-runs "template-group/verse-look/main" :template]]
      (doseq [[change path] [[#(assoc-in % (conj run-path :version) 0) [:composition-runs "template-group/verse-look/main" :template :version]]
                             [#(assoc-in % (conj run-path :placement) :left) [:composition-runs "template-group/verse-look/main"]]
                             [#(assoc-in % (conj run-path :bindings) {:wash "x"}) [:composition-runs "template-group/verse-look/main" :template :bindings]]
                             [#(update-in % run-path assoc :extra 1) [:composition-runs "template-group/verse-look/main" :template]]]]
        (let [ack (apply-at! r :import {:source (wire/encode (change document))} 2)]
          (is (= [:rejected path] [(:status ack) (rejection ack)])))))))

(deftest a-volume-that-is-empty-now-is-allowed-and-reported
  (with-runtime [r]
    (apply-at! r :template-group {:id :v :roles [{:id :zone :selector {:volume-id :zone}} {:id :wash :selector {:fixture-ids [:head-1]}}]
                                  :members [{:id :a :cue-id :ember :role :zone} {:id :b :cue-id :ice :role :wash}]} 0)
    ;; A configured volume with no bodies in it selects no fixtures right now.
    (let [model (-> (:model @(:state r))
                    (assoc-in [:settings :spatial :volumes :zone] {:shape :ball :center [0 0 0] :radius 0.1})
                    (assoc-in [:variables :spatial :members] {}))
          launched (template-groups/launch model {:id :v} 10)
          run (get-in launched [:composition-runs "template-group/v/main"])]
      (is (= [:zone] (get-in run [:template :empty-roles])))
      (is (= [:zone] (:empty-roles (first (:placements (first (template-groups/projection launched)))))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"selects no fixtures"
                            (template-groups/launch model {:id :v :bindings {:wash {:fixture-ids []}}} 10))))))

(deftest template-cards-share-the-macro-card-path-and-survive-export
  (with-runtime [r]
    (apply-at! r :template-group look 0)
    (apply-at! r :template-group-launch {:id :verse-look :bindings {:beams {:fixture-ids [:head-3]}}} 1)
    (testing "the REPL sizes and orders cards without changing the version"
      (is (= :accepted (:status (apply-at! r :template-group-cards {:id :verse-look :cards [{:slot-id :pulse :size :large}]} 2))))
      (is (= :accepted (:status (apply-at! r :template-group-cards {:id :verse-look :resize {:slot-id "spot" :size "medium"}} 3))))
      (is (= :accepted (:status (apply-at! r :template-group-cards {:id :verse-look :move {:slot-id :spot :offset -2}} 4))))
      (is (= [{:slot-id :spot :size :medium} {:slot-id :pulse :size :large} {:slot-id :fill :size :small}]
             (:cards (saved r :verse-look))))
      (is (= 1 (:version (saved r :verse-look))))
      (is (false? (:stale? (first (:placements (saved r :verse-look)))))))
    (testing "Studio sends the same command over typed transit"
      (let [{:keys [epoch runtime-id]} (show/snapshot r)
            handler (studio/handler r)
            payload {:id :verse-look :resize {:slot-id :fill :size :large} :expected-epoch epoch :runtime-id runtime-id}
            response (future (handler {:request-method :post :uri "/api/template-group/cards"
                                       :headers {"host" "localhost" "accept" "application/transit+json"
                                                 "content-type" "application/transit+json"}
                                       :body (java.io.StringReader. (wire/encode payload))}))]
        (loop [n 0] (when (and (not (realized? response)) (< n 300)) (show/tick! r (+ 5 n)) (Thread/sleep 5) (recur (inc n))))
        (is (= 200 (:status (deref response 1000 ::missing))))
        (is (= {:slot-id :fill :size :large} (last (:cards (saved r :verse-look)))))))
    (testing "pins use the macro pin forms and refuse a parameter the member's effect lacks"
      (is (= :accepted (:status (apply-at! r :template-group-cards {:id :verse-look :pin {:slot-id :fill :parameter :level}} 5))))
      (is (= [:level] (:pins (some #(when (= :fill (:slot-id %)) %) (:cards (saved r :verse-look))))))
      (let [ack (apply-at! r :template-group-cards {:id :verse-look :pin {:slot-id :fill :parameter :nonsense}} 6)]
        (is (= [:rejected [:pin :parameter]] [(:status ack) (rejection ack)])))
      (is (= :accepted (:status (apply-at! r :template-group-cards {:id :verse-look :pins {:slot-id :fill :pins nil}} 7))))
      (is (nil? (:pins (some #(when (= :fill (:slot-id %)) %) (:cards (saved r :verse-look)))))))
    (testing "invalid card edits are refused with paths and change nothing"
      (let [before (:cards (saved r :verse-look))]
        (doseq [[change path] [[{:resize {:slot-id :fill :size :huge}} [:resize :size]]
                               [{:resize {:slot-id :ghost :size :small}} [:resize :slot-id]]
                               [{:cards [{:slot-id :fill :size :small} {:slot-id :fill :size :large}]} [:cards]]
                               [{:move {:slot-id :fill :offset 0.5}} [:move :offset]]]]
          (let [ack (apply-at! r :template-group-cards (assoc change :id :verse-look) 400)]
            (is (= [:rejected path] [(:status ack) (rejection ack)]) (pr-str change))))
        (is (= :rejected (:status (apply-at! r :template-group-cards {:id :nobody :cards nil} 400))))
        (is (= before (:cards (saved r :verse-look))))))
    (testing "a save that omits :cards keeps them for unchanged members; :cards nil clears them"
      (apply-at! r :template-group (assoc look :name "Kept") 500)
      (is (= [:spot :pulse :fill] (mapv :slot-id (:cards (saved r :verse-look)))))
      (apply-at! r :template-group (assoc-in look [:members 2 :cue-id] :moon) 501)
      (is (= [:pulse :fill] (mapv :slot-id (:cards (saved r :verse-look)))) "a member whose cue changed loses its card")
      (apply-at! r :template-group (assoc look :cards nil) 502)
      (is (not (contains? (saved r :verse-look) :cards))))
    (testing "definitions and running placements survive export and import"
      (apply-at! r :template-group-cards {:id :verse-look :cards [{:slot-id :pulse :size :large}]} 600)
      (let [before (saved r :verse-look)
            exported (:body ((studio/handler r) {:request-method :get :uri "/api/export"}))]
        (is (= :accepted (:status (apply-at! r :import {:source exported} 601))))
        (let [after (saved r :verse-look)
              placement (first (:placements after))]
          (is (= (dissoc before :placements) (dissoc after :placements)))
          (is (= [:main {:wash {:fixture-ids [:head-1 :head-2]} :beams {:fixture-ids [:head-3]}}]
                 ((juxt :placement :bindings) placement)))
          (is (= 3 (count (members-of r (:layer-id placement)))) "member instances follow the restored owner")
          (is (= :accepted (:status (apply-at! r :template-group-stop {:id :verse-look} 700))))
          (show/tick! r 10000)
          (is (empty? (:instances (show/snapshot r)))))))))

(deftest a-load-refuses-a-template-that-names-a-missing-cue
  (with-runtime [r]
    (apply-at! r :template-group look 0)
    (let [exported (:body ((studio/handler r) {:request-method :get :uri "/api/export"}))
          broken (wire/encode (assoc-in (wire/decode exported)
                                        [:model :extensions template-groups/state-key :verse-look :members 1 :cue-id]
                                        :no-such-cue))]
      (let [ack (apply-at! r :import {:source broken} 1)]
        (is (= :rejected (:status ack)))
        (is (= [:extensions template-groups/state-key :verse-look :members 1] (rejection ack)))))))
