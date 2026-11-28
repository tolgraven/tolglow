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

(defn- effects [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest positions-ranges-and-hints-read-as-the-scheduler-writes-them
  (is (= "bar 16 → phrase 2 + bar 1" (goals/range-text (first active))))
  (is (= "until beat 2.5" (goals/range-text {:until {:beat 2.5}})))
  (is (nil? (goals/range-text {})))
  (is (= "linear-gradient(90deg,rgb(230 26 5),rgb(89 5 77))" (goals/palette-gradient (:palette (second active)))))
  (is (= "0,18.6 100,3.2" (goals/energy-points (:energy (first active)))))
  (is (= 2 (goals/text-length "𝄞𝄞")) "counts Unicode characters, as the owner does"))

(deftest a-draft-becomes-the-shared-command
  (testing "a new goal is a put with only the fields given"
    (is (= {:op :put :goal {:scope "section" :text "Lift" :from {:bar 16}}}
           (goals/draft->command (assoc (goals/new-draft) :text "Lift" :from "16") 600))))
  (testing "an edit sends the shown revision and leaves untouched positions alone"
    (let [draft (goals/draft-from-goal (first active))]
      (is (= {:id "drop" :scope "section" :from-unit "bar" :from "16" :until "phrase 2 + bar 1"}
             (select-keys draft [:id :scope :from-unit :from :until])))
      (is (= {:op :edit :id "drop" :expected-revision 3 :changes {:scope "section" :text "Hold back."}}
             (goals/draft->command (assoc draft :text "Hold back.") 600)))
      (is (= {:op :edit :id "drop" :expected-revision 3 :changes {:scope "section" :text (:text (first active)) :until nil}}
             (goals/draft->command (assoc draft :until "" :until-touched? true) 600))
          "clearing a position removes it")))
  (testing "client checks name the problem before anything is sent"
    (is (= {:error "Write what the agent should aim for."} (goals/draft->command (goals/new-draft) 600)))
    (is (str/includes? (:error (goals/draft->command (assoc (goals/new-draft) :text (apply str (repeat 601 "x"))) 600))
                       "at most 600 characters; this one has 601"))
    (is (= {:error "From is a nonnegative number of bars"}
           (goals/draft->command (assoc (goals/new-draft) :text "Lift" :from "-1") 600)))))

(deftest order-actions-stop-at-the-ends
  (is (= [nil {:op :reorder :id "drop" :order 2} {:op :remove :id "drop" :expected-revision 3}]
         (mapv last (goals/goal-actions (first active) 2))))
  (is (= [{:op :reorder :id "g-2" :order 1} nil] (mapv last (take 2 (goals/goal-actions (second active) 2))))))

(deftest the-panel-lists-goals-in-order-and-sends-the-owner-command
  (let [html (renderToStaticMarkup (r/as-element [goals/panel (db)]))]
    (is (str/includes? html "2 of 8 · read by the agent as data"))
    (is (= ["Section" "Set"] (map second (re-seq #"data-scope=\"[a-z]+\" aria-label=\"Goal \d, ([A-Za-z]+)\"" html)))
        "cards follow order")
    (is (= ["section" "set"] (map second (re-seq #"data-slot=\"wide\" data-scope=\"([a-z]+)\"" html)))
        "every goal is a wide card on the slot grid")
    (is (str/includes? html "aria-label=\"Move up Goal 2, Set\""))
    (is (re-find #"<button[^>]*disabled=\"\"[^>]*aria-label=\"Move up Goal 1, Section\"" html) "the top goal cannot rise")
    (is (str/includes? html "aria-label=\"Palette, 2 keyframes\""))
    (is (str/includes? html "aria-label=\"Energy curve, 2 points\"")))
  (testing "a reorder button posts the shared command with this session as author"
    (let [[[effect request]] (:fx (effects (db) [::goals/send {:op :reorder :id "g-2" :order 1}]))]
      (is (= :studio/http effect))
      (is (= {:path "/api/agent-goal" :body {:op :reorder :id "g-2" :order 1 :source {:adapter "web" :session-id "browser-1"}
                                             :expected-epoch 4 :runtime-id "rt"}}
             (select-keys request [:path :body]))
          "the session guard names the show epoch and runtime the panel showed")))
  (testing "saving a draft posts it; a bad draft shows the reason instead"
    (let [[[_ request]] (:fx (effects (db {:draft (assoc (goals/new-draft) :scope "night" :text "Warm")}) [::goals/save]))]
      (is (= {:op :put :goal {:scope "night" :text "Warm"}} (dissoc (:body request) :source :expected-epoch :runtime-id))))
    (let [result (effects (db {:draft (goals/new-draft)}) [::goals/save])]
      (is (nil? (:fx result)))
      (is (= "Write what the agent should aim for." (get-in result [:db :ui :agent-goals :error])))))
  (testing "the owner's refusal is shown as an alert"
    (let [html (renderToStaticMarkup (r/as-element [goals/panel (db {:error "At most 8 goals are active. Remove one before adding another"})]))]
      (is (re-find #"<p role=\"alert\"[^>]*>At most 8 goals are active" html))))
  (testing "a full list cannot add another; an empty one invites the first"
    (let [full (assoc-in (db) [:show :agent-goals :active] (vec (for [i (range 8)] (assoc (second active) :id (str "g" i) :order (inc i)))))]
      (is (re-find #"disabled=\"\" title=\"Remove a goal before adding another\"" (renderToStaticMarkup (r/as-element [goals/panel full])))))
    (is (str/includes? (renderToStaticMarkup (r/as-element [goals/panel (assoc-in (db) [:show :agent-goals :active] [])]))
                       "No goals yet"))))
