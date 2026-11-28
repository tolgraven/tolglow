(ns tolglow.studio.agent-goals-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.agent-goals :as goals]))

(def active
  "As Studio presents the projection: keywords become text, map keys stay."
  [{:id "drop" :scope "section" :order 1 :by "studio" :revision 3 :text "Hold back until the drop."
    :from {:bar 16} :until {:phrase 2 :bar 1}
    :energy [{:at {:bar 16} :level 0.2} {:at {:bar 24} :level 0.9}]}
   {:id "g-2" :scope "set" :order 2 :by "operator" :revision 1 :text "Ember reds drifting to violet."
    :palette [{:at {:phrase 0} :colors [[0.9 0.1 0.02]]} {:at {:phrase 12} :colors [[0.35 0.02 0.3]]}]}])

(defn- db [& [ui]]
  {:show {:epoch 4 :runtime-id "rt" :agent-goals {:active active :limit 8 :text-limit 600}}
   :session "browser-1" :connection {:connected? true} :ui {:agent-goals ui}})

(defn- run [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(defn- next-db [db event] (:db (run db event)))

(defn- html [db] (renderToStaticMarkup (r/as-element [goals/view db])))

(deftest positions-ranges-and-hints-read-as-the-scheduler-writes-them
  (is (= "bar 16 → phrase 2 + bar 1" (goals/range-text (first active))))
  (is (= "until beat 2.5" (goals/range-text {:until {:beat 2.5}})))
  (is (nil? (goals/range-text {})))
  (is (= "linear-gradient(90deg,rgb(230 26 5),rgb(89 5 77))" (goals/palette-gradient (:palette (second active)))))
  (is (= "0,18.6 100,3.2" (goals/energy-points (:energy (first active)))))
  (is (= 2 (goals/text-length "𝄞𝄞")) "counts Unicode characters, as the owner does")
  (is (= 1 (goals/text-length "é")) "after NFC, as the owner stores it"))

(deftest a-draft-becomes-the-shared-command
  (testing "a new goal is a put with only the fields given, its text in NFC"
    (is (= {:op :put :goal {:scope "section" :text "Café" :from {:bar 16}}}
           (goals/draft->command (assoc (goals/new-draft) :text "Café" :from "16") 600))))
  (testing "an edit sends the shown revision and leaves untouched positions alone"
    (let [draft (goals/draft-from-goal (first active))]
      (is (= {:id "drop" :scope "section" :from-unit "bar" :from "16" :until "phrase 2 + bar 1"}
             (select-keys draft [:id :scope :from-unit :from :until])))
      (is (= {:op :edit :id "drop" :expected-revision 3 :changes {:scope "section" :text "Hold back."}}
             (goals/draft->command (assoc draft :text "Hold back.") 600)))
      (is (= {:op :edit :id "drop" :expected-revision 3 :changes {:scope "section" :text (:text (first active)) :until nil}}
             (goals/draft->command (assoc draft :until "" :until-touched? true) 600))
          "clearing a position removes it")))
  (testing "client checks follow the owner's text rules before anything is sent"
    (is (= {:error "Write what the agent should aim for."} (goals/draft->command (goals/new-draft) 600)))
    (is (= {:error "Write what the agent should aim for."} (goals/draft->command (assoc (goals/new-draft) :text "​​") 600)))
    (is (= {:error "Remove the control, bidirectional, invisible or tag character (U+202E)."}
           (goals/draft->command (assoc (goals/new-draft) :text "safe ‮etouq") 600)))
    (is (str/includes? (:error (goals/draft->command (assoc (goals/new-draft) :text (apply str (repeat 601 "x"))) 600))
                       "at most 600 characters; this one has 601"))
    (is (= {:error "From is a nonnegative number of bars"}
           (goals/draft->command (assoc (goals/new-draft) :text "Lift" :from "-1") 600)))))

(deftest move-actions-stop-at-the-ends-and-name-the-shown-revision
  (is (= [nil {:op :reorder :id "drop" :order 2 :expected-revision 3} {:op :remove :id "drop" :expected-revision 3}]
         (mapv last (goals/goal-actions (first active) 2))))
  (is (= [{:op :reorder :id "g-2" :order 1 :expected-revision 1} nil] (mapv last (take 2 (goals/goal-actions (second active) 2))))))

(deftest the-panel-lists-goals-in-order-and-sends-the-owner-command
  (let [markup (html (db))]
    (is (str/includes? markup "2 of 8 · read by the agent as data"))
    (is (= ["Section" "Set"] (map second (re-seq #"data-scope=\"[a-z]+\" aria-label=\"Goal \d, ([A-Za-z]+)\"" markup)))
        "cards follow order")
    (is (= ["section" "set"] (map second (re-seq #"data-slot=\"wide\" data-scope=\"([a-z]+)\"" markup)))
        "every goal is a wide card on the slot grid")
    (is (str/includes? markup "aria-label=\"Move up Goal 2, Set\""))
    (is (re-find #"<button[^>]*disabled=\"\"[^>]*aria-label=\"Move up Goal 1, Section\"" markup) "the top goal cannot move up")
    (is (str/includes? markup "aria-label=\"Palette, 2 keyframes\""))
    (is (str/includes? markup "aria-label=\"Energy curve, 2 points\"")))
  (testing "a move posts the shared command; the listener, not the client, names the source"
    (let [[[effect request]] (:fx (run (db) [::goals/send {:op :reorder :id "g-2" :order 1 :expected-revision 1}]))]
      (is (= :studio/http effect))
      (is (= {:path "/api/agent-goal" :body {:op :reorder :id "g-2" :order 1 :expected-revision 1
                                             :source {:session-id "browser-1"} :expected-epoch 4 :runtime-id "rt"}}
             (select-keys request [:path :body])))))
  (testing "saving a draft posts it; a bad draft shows the reason instead"
    (let [[[_ request]] (:fx (run (db {:draft (assoc (goals/new-draft) :scope "night" :text "Warm")}) [::goals/save]))]
      (is (= {:op :put :goal {:scope "night" :text "Warm"}} (dissoc (:body request) :source :expected-epoch :runtime-id))))
    (let [result (run (db {:draft (goals/new-draft)}) [::goals/save])]
      (is (nil? (:fx result)))
      (is (= "Write what the agent should aim for." (get-in result [:db :ui :agent-goals :error])))))
  (testing "the owner's refusal is shown as an alert"
    (is (re-find #"<p role=\"alert\"[^>]*>At most 8 goals are active"
                 (html (db {:error "At most 8 goals are active. Remove one before adding another"})))))
  (testing "a full list cannot add another; an empty one invites the first"
    (let [full (assoc-in (db) [:show :agent-goals :active] (vec (for [i (range 8)] (assoc (second active) :id (str "g" i) :order (inc i)))))]
      (is (re-find #"disabled=\"\" title=\"Remove a goal before adding another\"" (html full))))
    (is (str/includes? (html (assoc-in (db) [:show :agent-goals :active] [])) "No goals yet"))))

(deftest a-long-goal-has-a-full-text-view-that-is-not-hover-only
  (let [long-goal (assoc (second active) :text (str (apply str (repeat 40 "Warm reds, "))))
        base (assoc-in (db) [:show :agent-goals :active] [(first active) long-goal])
        closed (html base)
        opened (html (next-db base [::goals/toggle-full "g-2"]))]
    (is (not (str/includes? closed "title=")) "no hover-only text")
    (is (re-find #"aria-expanded=\"false\" aria-controls=\"agent-goal-full-g-2\" aria-label=\"Read all of Goal 2, Set\"" closed))
    (is (not (str/includes? closed "Read all of Goal 1")) "a short goal needs no full view")
    (is (re-find #"id=\"agent-goal-full-g-2\" role=\"region\" aria-label=\"Full text of goal 2\"" opened))
    (is (str/includes? opened "aria-expanded=\"true\""))))

(deftest the-editor-closes-on-the-acknowledged-revision
  (let [saving (next-db (db {:draft (assoc (goals/draft-from-goal (second active)) :text "Deeper violet")}) [::goals/save])
        token (get-in saving [:ui :agent-goals :pending])]
    (testing "a queued reply keeps the draft and says so"
      (let [queued (next-db saving [::goals/sent token {:status "pending" :message "Command remains queued"}])]
        (is (some? (goals/editing queued)))
        (is (re-find #"role=\"status\"[^>]*>Command remains queued" (html queued)))))
    (testing "an accepted save keeps the editor until the projection shows that revision"
      (let [acked (next-db saving [::goals/sent token {:status "accepted" :goal-id "g-2" :goal-revision 2}])]
        (is (= "Deeper violet" (:text (goals/editing acked))) "the old projection is still revision 1")
        (is (nil? (goals/editing (assoc-in acked [:show :agent-goals :active 1 :revision] 2))))))))

(deftest a-late-reply-never-closes-a-newer-draft-or-goes-unseen
  (let [moving (next-db (db) [::goals/send {:op :reorder :id "g-2" :order 1 :expected-revision 1}])
        token (get-in moving [:ui :agent-goals :pending])
        typed (-> moving (next-db [::goals/cancel]) (next-db [::goals/start-add]) (next-db [::goals/field :text "Half written"]))]
    (is (string? token))
    (is (= token (get-in typed [:ui :agent-goals :pending])) "opening an editor keeps the request in flight")
    (is (re-find #"disabled=\"\"[^>]*>Add goal</button>" (html typed)) "and its buttons stay disabled")
    (testing "the move's acceptance leaves the new draft open"
      (is (= "Half written" (:text (goals/editing (next-db typed [::goals/sent token {:status "accepted" :goal-id "g-2"}])))))
      (is (nil? (get-in (next-db typed [::goals/sent token {:status "accepted"}]) [:ui :agent-goals :awaiting]))))
    (testing "the move's late refusal is still shown"
      (is (= "That goal changed since it was shown. Look again and retry"
             (get-in (next-db typed [::goals/failed token "That goal changed since it was shown. Look again and retry"])
                     [:ui :agent-goals :error]))))
    (testing "a reply to an older request changes nothing"
      (is (= typed (next-db typed [::goals/failed "older" "refused"]))))))
