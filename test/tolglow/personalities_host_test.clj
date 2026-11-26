(ns tolglow.personalities-host-test
  "The same personality commands through the show owner, the REPL entry point
  and Studio's Ring routes, and a volume cue whose shape follows the selection."
  (:require [afterglow.control.model :as control]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.personalities :as personalities]
            [tolglow.personalities.icon :as icon]
            [tolglow.show :as show]
            [tolglow.spatial.cues :as cues]
            [tolglow.studio :as studio]
            [tolglow.studio.display :as display]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def ^:private offsets [-0.9 -0.45 0.0 0.45 0.9])

(def fixtures
  "A 5 by 5 wall of single-channel lamps, 0.45 m apart, centred on [0 1 0]."
  (vec (for [[row y] (map-indexed vector offsets) [column x] (map-indexed vector offsets)
             :let [index (+ column (* 5 row))]]
         {:id (keyword (str "lamp-" index)) :position [x (+ 1.0 y) 0.0]
          :universe 1 :address (inc index) :channels {:red 1}})))

(def plain-volume
  {:op :spawn :id :wall :page-id :main :cell [0 0]
   :volume {:shape :rounded-box :position [0.0 1.0 0.0] :size [2.0 2.0 2.0] :rounding 0.0 :selection #{:position}
            :falloff {:inner-m 0.0 :outer-m 0.0 :curve :linear}}
   :paint {:red 1.0}})

(def feel-volume (assoc plain-volume :personality {:seed 7}))

(def lit-count
  "How many of the 25 lamps each sculpt form covers at this size."
  {:square 25 :rectangle 15 :bar 5 :squircle 21 :pill 11 :circle 13})

(defn- runtime [batch?]
  (let [captured (atom nil)
        capture! (fn [universes _frame-id]
                   (reset! captured (mapv #(bit-and 255 %) (take 25 (get universes 1)))))]
    (assoc (show/create-runtime {:model (assoc (control/new-show {}) :rig {:fixtures fixtures})
                                 :batch? batch? :workspace-options nil :sink {:send! capture!}
                                 :audition {:max-live-share ##Inf}})
           ::captured captured)))

(defn- tick! [rt now]
  (show/tick! rt now)
  (is (nil? (get-in @(:state rt) [:status :error])) (pr-str (:status @(:state rt)))))

(defn- submit! [rt now type payload]
  (let [receipt (show/submit! rt type payload)]
    (tick! rt now)
    (deref receipt 2000 {:status :timeout})))

(defn- accept! [rt now type payload]
  (let [ack (submit! rt now type payload)]
    (is (= :accepted (:status ack)) (pr-str ack))
    ack))

(defn- output [rt] @(::captured rt))

(defn- chosen-form [rt]
  (some :personality-form (vals (get-in @(:state rt) [:model :variables :spatial :signals]))))

(defn- request! [rt now method path content-type accept body]
  ;; Run the actual Ring request while the explicit host owner drains its queue.
  (let [result (future ((studio/handler rt)
                        (cond-> {:request-method method :uri path
                                 :headers (cond-> {"host" "localhost" "accept" accept}
                                            content-type (assoc "content-type" content-type))}
                          body (assoc :body (ByteArrayInputStream. (.getBytes ^String body "UTF-8"))))))
        deadline (+ (System/nanoTime) 5000000000)]
    (try
      (loop []
        (cond
          (realized? result) @result
          (> (System/nanoTime) deadline) (throw (ex-info "Ring request timed out" {:path path}))
          :else (do (tick! rt now) (Thread/sleep 5) (recur))))
      (finally (future-cancel result)))))

(defn- post-transit! [rt now path payload]
  (let [response (request! rt now :post path "application/transit+json" "application/transit+json" (transit/encode payload))]
    (assoc response :data (transit/decode (:body response)))))

(defn- post-json! [rt now path payload]
  (let [response (request! rt now :post path "application/json" "application/json" (json/write-str payload))]
    (assoc response :data (json/read-str (:body response) :key-fn keyword))))

(defn- studio-state [rt now]
  (transit/decode (:body (request! rt now :get "/api/state" nil "application/transit+json" nil))))

(deftest repl-and-studio-share-one-validated-command-and-projection
  (let [rt (runtime true)]
    (try
      (testing "a fresh show lists the built-ins and has no active feel"
        (let [view (:personalities (show/snapshot rt))]
          (is (= (mapv :id personalities/builtins) (mapv :id (:library view))))
          (is (nil? (:active view)))))
      (testing "the REPL selects through the owner queue"
        (let [before (:revision (show/snapshot rt))
              ack (accept! rt 0 :personality {:op :select :id :squircle})]
          (is (= {:mix [{:id :squircle :version 1 :weight 1.0}] :seed 0} (:active ack)))
          (is (= (inc before) (:revision ack) (:revision (show/snapshot rt))))
          (is (= :squircle (get-in (show/snapshot rt) [:personalities :active :id])))))
      (testing "Studio reads the committed selection, icon included, from its own route"
        (let [state (studio-state rt 10)]
          (is (= (:personalities (show/snapshot rt)) (:personalities state)))
          (is (re-find #"^<svg " (get-in state [:personalities :active :icon])))))
      (testing "Studio's typed route submits the same operation"
        (let [{:keys [status data]} (post-transit! rt 20 "/api/personality"
                                                   {:op :blend :mix [{:id :squircle :weight 3} {:id :spirals :weight 1}] :seed 4
                                                    :runtime-id (:id rt) :expected-epoch 0})
              active (get-in (show/snapshot rt) [:personalities :active])]
          (is (= 200 status) (pr-str data))
          (is (= :accepted (:status data)))
          (is (= {:mix [{:id :squircle :version 1 :weight 0.75} {:id :spirals :version 1 :weight 0.25}] :seed 4}
                 (:active data)))
          (is (= [personalities/blend-id 4] ((juxt :id :seed) active)))
          (is (= active (get-in (studio-state rt 30) [:personalities :active])))))
      (testing "a plain JSON client can use the same route with textual IDs"
        (let [{:keys [status data]} (post-json! rt 40 "/api/personality" {:op "select" :id "still-water"})]
          (is (= 200 status) (pr-str data))
          (is (= "accepted" (:status data)))
          (is (= :still-water (get-in (show/snapshot rt) [:personalities :active :id])))
          (is (= 4 (get-in (show/snapshot rt) [:personalities :active :seed])) "the seed carries over")))
      (testing "both paths report the same rejection and leave the selection alone"
        (let [revision (:revision (show/snapshot rt))
              from-repl (submit! rt 50 :personality {:op :select :id :no-such-feel})
              from-studio (post-transit! rt 60 "/api/personality" {:op :select :id :no-such-feel})]
          (is (= :rejected (:status from-repl)))
          (is (= 422 (:status from-studio)))
          (is (= "Unknown personality" (:message from-repl) (get-in from-studio [:data :message])))
          (is (= {:id :no-such-feel} (:details from-repl) (get-in from-studio [:data :details])))
          (is (= revision (:revision (show/snapshot rt))))
          (is (= :still-water (get-in (show/snapshot rt) [:personalities :active :id])))))
      (testing "a previous show session cannot change the feel"
        (is (= :rejected (:status @(show/submit! rt :personality {:op :clear :expected-epoch 99})))))
      (testing "an authored personality is validated on the way in and can be selected from either side"
        (let [mine {:id :mine :version 1 :schema 1 :name "Mine" :palette [[0.2 0.4 1.0]]
                    :concurrency {:fixture-fraction 1.0 :max-groups 1}
                    :geometry {:forms {:zigzag 1.0}}
                    :motion {:speed 0.5 :smoothness 0.5 :jitter 0.5 :symmetry 0.5}}
              refused (post-transit! rt 70 "/api/personality" {:op :put :personality (assoc-in mine [:motion :speed] 3)})]
          (is (= 422 (:status refused)))
          (is (= [{:path [:motion :speed] :message "Motion character must be within [0,1]"}]
                 (get-in refused [:data :details :errors])))
          (is (= {:id :mine :version 1} (select-keys (accept! rt 80 :personality {:op :put :personality mine}) [:id :version])))
          (is (= :mine (:id (last (get-in (studio-state rt 90) [:personalities :library])))))
          (accept! rt 100 :personality {:op :select :id :mine})
          (is (= "Mine" (get-in (show/snapshot rt) [:personalities :active :name])))))
      (testing "an agent inspects the same data without the pictures"
        (let [seen (:personalities (show/dispatch-tool! rt "inspect_show" {} {}))]
          (is (= :mine (get-in seen [:active :id])))
          (is (not (re-find #"<svg" (pr-str seen))))
          (is (= (personalities/without-icons (:personalities (show/snapshot rt))) seen))))
      (finally (show/stop! rt)))))

(deftest the-push-display-page-receives-the-active-icon
  (let [rt (runtime true)
        project (:project! (with-redefs [display/browser-display identity] (show/push-browser-display rt {})))
        panel {:display-mode :pages :title "Tolglow"}]
    (try
      (is (not (contains? (project panel) :personality)) "no feel, no change to the display view")
      (accept! rt 0 :personality {:op :select :id :spirals})
      (let [{:keys [id name icon]} (:personality (project panel))]
        (is (= [:spirals "Spiralling in general"] [id name]))
        (is (= (icon/data-uri (get-in (show/snapshot rt) [:personalities :active :icon])) icon)
            "the device page rasterizes the same SVG Studio shows"))
      (accept! rt 10 :personality {:op :clear})
      (is (not (contains? (project panel) :personality)))
      (finally (show/stop! rt)))))

(deftest a-saved-show-reopens-with-its-feel
  (let [rt (runtime true) reopened (runtime true)]
    (try
      (accept! rt 0 :personality {:op :put :personality (assoc (first personalities/builtins) :id "house" :name "House feel")})
      (accept! rt 10 :personality {:op :blend :mix [{:id "house" :weight 1} {:id :random-pink :weight 1}] :seed 12})
      (let [saved (show/export-show-source rt)
            before (:personalities (show/snapshot rt))]
        (is (= :accepted (:status (submit! reopened 0 :import {:source saved}))))
        (tick! reopened 25)
        (is (= before (:personalities (show/snapshot reopened))))
        (is (= (personalities/state (:model @(:state rt))) (personalities/state (:model @(:state reopened)))))
        (testing "a restart keeps the selection, as it keeps settings"
          (accept! reopened 50 :command {:op :show/restart})
          (is (= (:active before) (get-in (show/snapshot reopened) [:personalities :active])))))
      (finally (show/stop! reopened) (show/stop! rt)))))

(defn- wall
  "Launch a volume cue, select a personality and return [form bytes]."
  [batch? volume operations]
  (let [rt (runtime batch?)]
    (try
      (doseq [[index operation] (map-indexed vector operations)]
        (accept! rt (* 10 index) :personality operation))
      (accept! rt 100 :spatial-cue volume)
      (tick! rt 125)
      [(chosen-form rt) (output rt)]
      (finally (show/stop! rt)))))

(deftest a-volume-cue-shapes-itself-from-the-active-personality
  (doseq [batch? [true false]]
    (testing (str "batch " batch?)
      (let [[_ authored] (wall batch? plain-volume [])
            lit (fn [bytes] (count (filter #(= 255 %) bytes)))]
        (is (= (vec (repeat 25 255)) authored) "the authored box covers the whole wall")
        (testing "no active personality: the declared input changes nothing"
          (is (= [nil authored] (wall batch? feel-volume []))))
        (testing "a cue that did not ask is never reshaped"
          (is (= [nil authored] (wall batch? plain-volume [{:op :select :id :still-water}]))))
        (testing "a personality with no sculpt form leaves the authored shape"
          (is (= [nil authored]
                 (wall batch? feel-volume
                       [{:op :put :personality {:id :lines :version 1 :schema 1 :name "Lines only" :palette [[1.0 1.0 1.0]]
                                                :concurrency {:fixture-fraction 1.0 :max-groups 1}
                                                :geometry {:forms {:zigzag 1.0 :spiral 1.0}}
                                                :motion {:speed 0.5 :smoothness 0.5 :jitter 0.0 :symmetry 0.5}}}
                        {:op :select :id :lines}]))))
        (let [[squares-form squares-bytes :as squares] (wall batch? feel-volume [{:op :select :id :squares}])
              [corner-form corner-bytes] (wall batch? feel-volume [{:op :select :id :squircle}])]
          (testing "the same seed and personality give identical bytes"
            (is (= squares (wall batch? feel-volume [{:op :select :id :squares}])))
            (is (= squares (wall batch? feel-volume [{:op :select :id :squircle} {:op :select :id :squares}]))
                "only the committed selection matters, not how the show got there"))
          (testing "each personality draws from its own forms"
            (is (#{:square :rectangle :bar} squares-form))
            (is (#{:squircle :pill :circle} corner-form))
            (is (= (lit-count squares-form) (lit squares-bytes)))
            (is (= (lit-count corner-form) (lit corner-bytes)))
            (is (every? #{0 255} (concat squares-bytes corner-bytes))))
          (testing "changing the personality changes the chosen shape and the output"
            (is (not= squares-form corner-form))
            (is (not= squares-bytes corner-bytes)))
          (testing "a personality with one sculpt form always gets it"
            (is (= [:circle 13] (update (wall batch? feel-volume [{:op :select :id :spirals}]) 1 lit))))
          (testing "the seeds re-roll the draw without changing the personality"
            (let [forms (set (for [seed (range 24)]
                               (first (wall batch? feel-volume [{:op :select :id :squares :seed seed}]))))
                  declared (set (for [seed (range 24)]
                                  (first (wall batch? (assoc feel-volume :personality {:seed seed})
                                               [{:op :select :id :squares}]))))]
              (is (= #{:square :rectangle :bar} forms))
              (is (= #{:square :rectangle :bar} declared)))))))))

(deftest the-shape-follows-live-selection-and-survives-a-save
  (let [rt (runtime true) reopened (runtime true)]
    (try
      (accept! rt 0 :spatial-cue feel-volume)
      (is (= (vec (repeat 25 255)) (output rt)))
      (accept! rt 25 :personality {:op :select :id :spirals})
      (tick! rt 50)
      (is (= :circle (chosen-form rt)))
      (is (= 13 (count (filter pos? (output rt)))) "the running cue became a sphere")
      (let [key (cues/key-for (first (vals (get-in @(:state rt) [:model :instances]))))
            signal (get-in (show/snapshot rt) [:spatial :signals key])]
        (is (= {:shape :sphere :radius 1.0} (select-keys (:volume signal) [:shape :radius])))
        (is (= :circle (:personality-form signal)) "the REPL and Studio can read which form was drawn"))
      (let [sphere (output rt)
            saved (show/export-show-source rt)]
        (is (= :accepted (:status (submit! reopened 0 :import {:source saved}))))
        (tick! reopened 25)
        (tick! reopened 50)
        (is (= sphere (output reopened)) "the reopened show renders the same bytes")
        (is (= :circle (chosen-form reopened))))
      (accept! rt 75 :personality {:op :clear})
      (tick! rt 100)
      (is (= (vec (repeat 25 255)) (output rt)) "clearing the feel returns the authored box")
      (is (nil? (chosen-form rt)))
      (finally (show/stop! reopened) (show/stop! rt)))))

(deftest a-personality-changes-the-shape-and-keeps-the-live-size
  (doseq [batch? [true false]]
    (testing (str "batch " batch?)
      (let [rt (runtime batch?)
            sphere (assoc feel-volume :id :ball
                          :volume {:shape :sphere :position [0.0 1.0 0.0] :radius 1.0 :selection #{:position}
                                   :falloff {:inner-m 0.0 :outer-m 0.0 :curve :linear}})
            lit #(count (filter pos? (output rt)))]
        (try
          (accept! rt 0 :spatial-cue sphere)
          (is (= 13 (lit)))
          (let [id (key (first (get-in @(:state rt) [:model :instances])))]
            (accept! rt 10 :command {:op :param/set :instance-id id :param :radius :value 0.3})
            (tick! rt 20)
            (is (= 1 (lit)) "the operator shrank the sphere to the centre lamp")
            (accept! rt 30 :personality {:op :select :id :spirals})
            (tick! rt 40)
            (is (= :circle (chosen-form rt)))
            (is (= 1 (lit)) "the drawn sphere keeps the operator's radius")
            (accept! rt 50 :personality {:op :select :id :squares :seed 0})
            (tick! rt 60)
            (let [signal (some #(when (:personality-form %) %) (vals (get-in (show/snapshot rt) [:spatial :signals])))
                  side (reduce max (get-in signal [:volume :size]))]
              (is (= :rounded-box (get-in signal [:volume :shape])))
              (is (< (Math/abs (- 0.6 side)) 1.0e-9) "a box drawn from the sphere spans its live diameter")))
          (finally (show/stop! rt)))))))

(deftest a-room-audition-follows-the-live-feel-and-never-changes-it
  ;; A draft cannot stage a feel, and applying replays it on the live model, so
  ;; the audition reads the live selection like it reads the live master.
  (let [rt (runtime true)
        frame! (fn [now] (tick! rt now) (is (show/await-audition rt 5000)) (show/snapshot rt))
        lit (fn [values] (count (filter #(pos? (get-in % [:emitted-color 0] 0)) (vals values))))
        unused {:id :unused :version 1 :outputs {:dimmer {:op :const :value 0.0}}}]
    (try
      (accept! rt 0 :spatial-cue feel-volume)
      (is (= :accepted (:status (submit! rt 25 :command
                                         {:op :scene-draft/stage :draft-id "room" :operations [{:op :registry/publish :kind :effects
                                                                         :definition unused}]}))))
      (let [before (frame! 50)]
        (is (= :auditioning (get-in before [:scene-draft :status])))
        (is (= 25 (lit (get-in before [:output :values])) (lit (get-in before [:draft-output :values])))))
      (accept! rt 75 :personality {:op :select :id :spirals})
      (let [saved (personalities/state (:model @(:state rt)))
            after (last (mapv frame! [100 125 150]))]
        (is (= :auditioning (get-in after [:scene-draft :status])) "a feel change does not make the draft stale")
        (is (= 13 (lit (get-in after [:output :values]))))
        (is (= (get-in after [:output :values]) (get-in after [:draft-output :values]))
            "the Room shows the feel the live show would apply the draft under")
        (is (identical? saved (personalities/state (:model @(:state rt)))) "the audition leaves the live feel alone")
        (is (= 13 (count (filter pos? (output rt))))))
      (finally (show/stop! rt)))))

(deftest only-a-static-morphable-volume-can-declare-the-input
  (let [rt (runtime true)]
    (try
      (doseq [bad [(assoc-in feel-volume [:volume :shape] :cylinder)
                   (assoc feel-volume :volume {:shape :ball :position [0.0 1.0 0.0] :radius 0.5 :velocity [0.0 0.0 0.0]
                                               :selection #{:position}})
                   (assoc feel-volume :personality {:seed 1.5})
                   (assoc feel-volume :personality {:family :waveform})]]
        (is (= :rejected (:status (submit! rt 0 :spatial-cue bad))) (pr-str bad))
        (is (empty? (get-in @(:state rt) [:model :instances]))))
      (accept! rt 10 :spatial-cue feel-volume)
      (is (= {:version 1 :family :sculpt :seed 7}
             (get-in @(:state rt) [:model :registry :effects :wall :spatial-volume :personality])))
      (finally (show/stop! rt)))))
