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
      (is (= {:id :verse-look :name "Verse look" :version 1 :placements []
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

(deftest a-placement-never-shares-a-run-with-a-saved-macro
  (with-runtime [r]
    (apply-at! r :template-group (assoc look :roles [] :members [{:id :a :cue-id :ember}]) 0)
    (apply-at! r :composition {:id "template-group/verse-look/main" :mode :macro :slots [{:id :x :cue-id :ice}]} 1)
    (let [ack (apply-at! r :template-group-launch {:id :verse-look} 2)]
      (is (= [:rejected [:placement]] [(:status ack) (rejection ack)])))
    (is (empty? (:instances (show/snapshot r))))))

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
