(ns tolglow.studio.auto-plan-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.auto-plan :as plan]))

;; The saved config as Studio's projection presents it: keywords as text.
(def saved
  {:interval {:beats 1} :count 1
   :randomize {:params "none" :palette [{:color [1 0 0] :weight 6} {:color [0.85 0.05 0.02] :weight 3}]}
   :categories {:exclude ["strobe"]}
   :max-lit 6
   :sections [{:id "red" :weight 6 :length {:bars 2} :transition "release"
               :config {:scope {:cue-ids ["wash-color" "sweep"]} :count 2}}
              {:id "quiet" :weight 2 :length {:bars 2} :max-run 1 :transition "cut"
               :config {:scope {:cue-ids ["spot"]} :intent "darkness" :max-lit 1 :interval {:bars 1}
                        :randomize {:palette [{:color [0.4 0.18 0.48] :weight 3}]}}}]})

(defn- run-event [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest a-saved-plan-round-trips-through-the-editor-unchanged
  (let [draft (plan/plan->draft saved)
        edit (plan/draft->plan draft)]
    (is (= "strobe" (:exclude draft)))
    (is (= ["red" "quiet"] (mapv :id (:sections draft))))
    (is (= {:count 2} (get-in draft [:sections 0 :extra])) "settings the editor does not show ride along")
    (is (true? (get-in draft [:sections 1 :darkness?])))
    (is (= {:exclude [:strobe]} (:categories edit)))
    (is (= 6 (:max-lit edit)))
    (is (= [{:color [1 0 0] :weight 6} {:color [0.85 0.05 0.02] :weight 3}] (:palette edit))
        "an untouched colour is sent back exactly, not rounded through #rrggbb")
    (is (= {:id :red :weight 6 :length {:bars 2} :transition :release
            :config {:count 2 :scope {:cue-ids [:wash-color :sweep]}}}
           (first (:sections edit))))
    (is (= {:id :quiet :weight 2 :length {:bars 2} :transition :cut :max-run 1
            :config {:interval {:bars 1} :scope {:cue-ids [:spot]} :max-lit 1 :intent :darkness
                     :randomize {:palette [{:color [0.4 0.18 0.48] :weight 3}]}}}
           (second (:sections edit))))))

(deftest the-editor-names-every-part-so-an-emptied-part-clears
  (let [edit (plan/draft->plan {:include "" :exclude "" :max-lit "" :palette [] :sections []})]
    (is (= {:categories nil :max-lit nil :palette nil :sections []} edit)))
  (is (= [:movement :strobe] (plan/words "movement, strobe  movement")))
  (is (= "#ff0000" (plan/color->hex [1 0 0])))
  (is (= [0 0.502 1] (plan/hex->color "#0080ff"))))

(deftest the-editor-refuses-what-it-can-check
  (let [row (assoc plan/blank-section :id "a")]
    (is (thrown-with-msg? js/Error #"Name every section" (plan/draft->plan {:sections [(assoc row :id " ")]})))
    (is (thrown-with-msg? js/Error #"its own name" (plan/draft->plan {:sections [row row]})))
    (is (thrown-with-msg? js/Error #"positive length" (plan/draft->plan {:sections [(assoc row :length-value "0")]})))
    (is (thrown-with-msg? js/Error #"lit limit is a whole number" (plan/draft->plan {:max-lit "2.5" :sections []})))
    (is (thrown-with-msg? js/Error #"own palette but no colours" (plan/draft->plan {:sections [(assoc row :own-palette? true)]})))))

(deftest save-sends-the-plan-edit-with-the-show-identity-and-the-line
  (let [db {:show {:epoch 4 :runtime-id "rt" :auto {:enabled? true :config saved}} :session "s1"
            :ui {:auto-plan {:applies "next-phrase" :draft (assoc (plan/plan->draft saved) :max-lit "4")}}
            :connection {:connected? true}}
        {:keys [studio/http]} (run-event db [::plan/save])]
    (is (= "/api/auto/plan" (:path http)))
    (is (= {:runtime-id "rt" :expected-epoch 4 :at "next-phrase" :source {:adapter "web" :session-id "s1"}}
           (dissoc (:body http) :plan)))
    (is (= 4 (get-in http [:body :plan :max-lit])))
    (testing "with auto off the line does not apply"
      (is (not (contains? (:body (:studio/http (run-event (assoc-in db [:show :auto :enabled?] false) [::plan/save]))) :at))))
    (testing "an editor refusal stays in the panel and sends nothing"
      (let [effects (run-event (assoc-in db [:ui :auto-plan :draft :max-lit] "x") [::plan/save])]
        (is (nil? (:studio/http effects)))
        (is (re-find #"lit limit" (get-in effects [:db :ui :auto-plan :error])))))))

(deftest the-panel-shows-the-saved-plan
  (let [html (renderToStaticMarkup (r/as-element [plan/panel {:show {:auto {:config saved :enabled? false} :cues [{:id "spot"}]}
                                                              :connection {:connected? true}}]))]
    (is (str/includes? html "PLAN THE WANDERING"))
    (is (str/includes? html "2 sections · 2 colours"))
    (is (str/includes? html "data-section-id=\"quiet\""))
    (is (str/includes? html "Start auto plays it"))
    (is (= 3 (count (re-seq #"type=\"color\"" html))) "two plan colours and the quiet section's own")))
