(ns tolglow.studio.patchbay-test
  (:require [cljs.test :refer-macros [deftest is]]
            [cljs.reader :as reader]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as reagent]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.patchbay :as patchbay]))

(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))
(def route (merge patchbay/route-defaults {:id "kick-wash" :source-id "live/ableton-live/ch/1/note/36"
                                         :cue-id "wash" :param "level" :envelope? true :attack "1/16" :release "1/2"}))
(def form (assoc patchbay/defaults :routes [route] :visible? true))
(def base {:show {:runtime-id "process-A" :epoch 3 :cues [{:id "wash" :effect-id "wash"}]
                 :effects [{:id "wash" :params {:level {:type "number" :bounds [0 1]} :tint {:type "color" :bounds [0 1]}
                                                :mode {:type "enum" :values ["one" "two"]}}}]}
           :patchbay form})

(deftest editor-builds-musical-data-and-preserves-dynamic-port-string-keys
  (let [spec (patchbay/build-spec form) roundtrip (reader/read-string (pr-str spec))]
    (is (= spec roundtrip))
    (is (= #{"live/ableton-live/ch/1/note/36"} (set (keys (:sources spec)))))
    (is (= :midi (get-in spec [:sources "live/ableton-live/ch/1/note/36" :kind])))
    (is (= :wash (get-in spec [:destinations "wash/level" :cue-id])))
    (is (= {:beats 0.0625} (get-in spec [:routes 0 :envelope :attack])))
    (is (= {:beats 0.5} (get-in spec [:routes 0 :envelope :release])))
    (is (= :max (get-in spec [:destinations "wash/level" :combine])))
    (is (= 1 (:revision spec)))))

(deftest form-roundtrip-preserves-mixed-musical-units-and-power-curves
  (let [spec (-> (patchbay/build-spec form)
                 (assoc-in [:routes 0 :envelope] {:attack {:bars 1} :decay {:beats 1} :sustain 0.5 :release {:phrases 0.25} :curve :sine})
                 (assoc-in [:routes 0 :curve] {:type :power :exponent 3}))
        restored (patchbay/form-from-spec spec) built (patchbay/build-spec restored)]
    (is (= (get-in spec [:routes 0 :envelope]) (get-in built [:routes 0 :envelope])))
    (is (= {:type :power :exponent 3} (get-in built [:routes 0 :curve])))
    (is (= 2 (:revision built)))))

(deftest editor-refuses-invalid-ranges-durations-and-ambiguous-mixing
  (doseq [bad [(assoc form :routes []) (assoc-in form [:routes 0 :input-high] "0")
               (assoc-in form [:routes 0 :steps] "1") (assoc-in form [:routes 0 :attack] "-1")
               (assoc-in form [:routes 0 :sustain] "2") (assoc-in form [:routes 0 :min-confidence] "NaN")
               (assoc form :routes [route (assoc route :id "other" :combine "sum")])]]
    (is (thrown? js/Error (patchbay/build-spec bad)))))

(deftest one-observed-source-can-use-different-per-route-input-ranges
  (let [other (assoc route :id "second" :input-low "20" :input-high "100")
        built (patchbay/build-spec (assoc form :routes [route other]))]
    (is (= [0 127] (get-in built [:sources "live/ableton-live/ch/1/note/36" :range])))
    (is (= [20 100] (get-in built [:routes 1 :source-range])))))

(deftest only-scalar-bounded-parameters-are-offered
  (is (= [{:id "level" :label "level" :range [0 1]}] (patchbay/parameter-options (:show base) "wash")))
  (is (empty? (patchbay/parameter-options (:show base) "missing"))))

(deftest loading-is-read-only-and-a-save-does-not-enable-routing
  (let [load (transition base [:patchbay/load])
        save (transition base [:patchbay/save])]
    (is (= "/api/patch" (get-in load [:studio/http :path])))
    (is (nil? (get-in load [:studio/http :body])))
    (is (= "/api/patch/save" (get-in save [:studio/http :path])))
    (is (= "process-A" (get-in save [:studio/http :body :runtime-id])))
    (is (= 3 (get-in save [:studio/http :body :expected-epoch])))
    (is (string? (get-in save [:studio/http :body :source])))
    (is (not (contains? (get-in save [:studio/http :body]) :enabled?)))
    (is (nil? (:dispatch save)))))

(deftest stale-responses-cannot-overwrite-replaced-runtime-or-newer-request
  (let [load (transition base [:patchbay/load]) request-token (patchbay/token (:db load))
        replaced (assoc-in (:db load) [:show :runtime-id] "process-B")
        stale (transition replaced [:patchbay/received request-token :load 0 {:spec nil}])
        reload (transition (:db load) [:patchbay/load])
        old (transition (:db reload) [:patchbay/received request-token :load 0 {:spec nil}])]
    (is (empty? stale)) (is (empty? old))
    (is (= "process-B" (get-in replaced [:show :runtime-id])))))

(deftest editing-during-save-retains-newer-draft-and-advances-saved-revision
  (let [save (transition base [:patchbay/save]) request-token (patchbay/token (:db save))
        edited (transition (:db save) [:patchbay/edit [:routes 0 :high] "0.8"])
        accepted (transition (:db edited) [:patchbay/received request-token :save 0 {:status "accepted"}])]
    (is (= "0.8" (get-in accepted [:db :patchbay :routes 0 :high])))
    (is (true? (get-in accepted [:db :patchbay :dirty?])))
    (is (= 1 (get-in accepted [:db :patchbay :revision])))
    (is (nil? (get-in accepted [:db :patchbay :pending])))
    (is (nil? (:dispatch accepted)))))

(deftest refresh-retains-local-edits-and-explicit-enable-is-separate
  (let [edited (:db (transition base [:patchbay/edit [:routes 0 :release] "2"]))
        load (transition edited [:patchbay/load])
        accepted (transition (:db load) [:patchbay/received (patchbay/token (:db load)) :load 1 {:spec nil :sources []}])
        enabled (transition base [:patchbay/enabled true])]
    (is (= "2" (get-in accepted [:db :patchbay :routes 0 :release])))
    (is (= "/api/patch/enabled" (get-in enabled [:studio/http :path])))
    (is (true? (get-in enabled [:studio/http :body :enabled?])))))

(deftest runtime-change-invalidates-drafts-without-starting-capture
  (let [changed (transition base [:patchbay/runtime-change])]
    (is (empty? (get-in changed [:db :patchbay :routes])))
    (is (= [:patchbay/load] (:dispatch changed)))
    (is (nil? (:studio/http changed)))))

(deftest initial-load-waits-for-show-identity-and-unmount-cancels-pending-poll
  (let [waiting (transition (dissoc base :show) [:patchbay/load])
        closed (transition (:db (transition base [:patchbay/open])) [:patchbay/invalidate])
        stale-poll (transition (:db closed) [:patchbay/load])]
    (is (nil? (:studio/http waiting)))
    (is (= [:patchbay/load] (get-in waiting [:dispatch-later 0 :dispatch])))
    (is (nil? (:studio/http stale-poll)))
    (is (nil? (:dispatch-later stale-poll)))))

(deftest available-source-projection-and-rendered-panel-use-actual-observed-ports
  (let [ports (patchbay/source-ports {:ports [{:id "audio/rms" :kind "audio" :value 0.4}]})]
    (is (= "audio/rms" (:source-id (first ports))))
    (is (= "audio/rms" (:source-id (first (patchbay/source-ports {:sources [{:source-id "audio/rms"}]}))))))
  (reset! rf-db/app-db (assoc-in base [:show :patch]
                               {:enabled? false :sources [{:source-id "live/ableton-live/ch/1/note/36" :kind "midi" :value 110}]}))
  (let [html (renderToStaticMarkup (reagent/as-element [patchbay/panel base]))]
    (is (.includes html "Signal patchbay"))
    (is (.includes html "live/ableton-live/ch/1/note/36"))
    (is (.includes html "Save routing"))
    (is (.includes html "Live audio sends &amp; returns"))))
