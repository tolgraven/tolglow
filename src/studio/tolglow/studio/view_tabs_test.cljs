(ns tolglow.studio.view-tabs-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.events :as events]
            [tolglow.studio.events-test :refer [base transition full-frame]]
            [tolglow.studio.show-window :as show-window]
            [tolglow.studio.view-tabs :as tabs]))

(defn- views [state] (mapv :view (:tabs state)))

(deftest tabs-open-close-reopen-and-cycle-in-order
  (let [one (tabs/initial :perform)
        two (tabs/open one :rig)
        three (tabs/open (tabs/select two 1) nil)]
    (is (= [:perform] (views one)))
    (is (= [:perform :rig] (views two)))
    (is (= 2 (:active two)) "a new tab is selected")
    (is (= [:perform :perform :rig] (views three)) "a new tab opens after the active one, on its view")
    (is (= 3 (:active three)))
    (testing "closing the active tab selects its right neighbour, or the left one when it was last"
      (is (= 2 (:active (tabs/close three 3))))
      (is (= 3 (:active (tabs/close (tabs/select three 2) 2)))))
    (testing "the last tab cannot be closed"
      (is (= one (tabs/close one 1))))
    (testing "closed views reopen most recent first"
      (let [closed (-> three (tabs/close 2) (tabs/close 3))
            reopened (tabs/reopen closed)]
        (is (= [:perform] (views closed)))
        (is (= [:rig :perform] (:closed closed)))
        (is (= [:perform :perform] (views reopened)))
        (is (= [:perform :perform :rig] (views (tabs/reopen reopened))))
        (is (= [] (:closed (tabs/reopen (tabs/reopen reopened)))))))
    (testing "cycling wraps"
      (is (= 1 (:active (tabs/step two 1))))
      (is (= 1 (:active (tabs/step (tabs/select two 1) 2))))
      (is (= 2 (:active (tabs/step (tabs/select two 1) -1)))))
    (testing "the tab count is bounded"
      (let [full (reduce (fn [s _] (tabs/open s :rig)) one (range 30))]
        (is (= tabs/max-tabs (count (:tabs full))))
        (is (identical? full (tabs/open full :debug)))))))

(deftest navigation-changes-the-active-tab-and-deep-links-select-an-existing-tab
  (let [state (-> (tabs/initial :perform) (tabs/open :rig) (tabs/select 1))]
    (is (= [:compose :rig] (views (tabs/follow state :compose false))))
    (is (= 2 (:active (tabs/follow state :rig true))) "a deep link selects the tab already on that view")
    (is (= [:perform :rig] (views (tabs/follow state :rig true))))
    (is (= [:rig :rig] (views (tabs/follow state :rig false))) "later navigation changes the active tab")
    (is (identical? state (tabs/follow state :push-display false)) "standalone surfaces are not tabs")))

(deftest stored-tabs-are-repaired-on-load
  (let [text (tabs/encode (-> (tabs/initial :perform) (tabs/open :rig) (tabs/close 1)))]
    (is (= {:tabs [{:id 2 :view :rig}] :active 2 :next-id 3 :closed [:perform]}
           (tabs/normalize (tabs/decode text) :perform))))
  (is (nil? (tabs/decode "{")))
  (is (nil? (tabs/decode "{\"version\":2,\"tabs\":[]}")))
  (is (= (tabs/initial :rig) (tabs/normalize nil :rig)))
  (is (= {:tabs [{:id 4 :view :perform} {:id 9 :view :debug}] :active 4 :next-id 10 :closed [:rig]}
         (tabs/normalize {:tabs [{:id 4 :view "perform"} {:id 4 :view "rig"} {:id 0 :view "rig"}
                                 {:id 9 :view "debug"} {:id 5 :view "push-display"} {:id 6 :view "gone"}]
                          :active 77 :next-id 2 :closed ["rig" "nowhere"]}
                         :perform))
      "duplicate, invalid and unknown entries are dropped and the next ID stays ahead"))

(defn- with-tabs [db state] (assoc db :view-tabs state))

(deftest routes-and-tab-events-keep-the-strip-and-the-router-together
  (let [db (with-tabs (assoc base :route-name :perform) (-> (tabs/initial :perform) (tabs/open :rig) (tabs/select 1)))
        to-compose (transition db [:studio/route-matched :compose])
        stored (some (fn [[k v]] (when (= k :view-tabs/store) v)) (:fx to-compose))]
    (is (= [:compose :rig] (views (get-in to-compose [:db :view-tabs]))))
    (is (= [:compose :rig] (views stored)) "every change is saved for this device")
    (testing "choosing a tab navigates to its view"
      (let [selected (transition (:db to-compose) [:view-tabs/select 2])]
        (is (= 2 (get-in selected [:db :view-tabs :active])))
        (is (some #{[:studio/navigate [:push :rig]]} (:fx selected)))))
    (testing "a new tab on the same view needs no navigation and opens nothing else"
      (let [opened (transition db [:view-tabs/open nil])]
        (is (= [:perform :perform :rig] (views (get-in opened [:db :view-tabs]))))
        (is (= #{:view-tabs/store} (set (map first (:fx opened)))))
        (is (= (dissoc db :view-tabs) (dissoc (:db opened) :view-tabs)) "only the tab state changes")))
    (testing "host menu actions"
      (is (= 2 (get-in (transition db [:view-tabs/host "next"]) [:db :view-tabs :active])))
      (is (= [:rig] (views (get-in (transition db [:view-tabs/host "close"]) [:db :view-tabs]))))
      (is (= 3 (count (get-in (transition db [:view-tabs/host "new"]) [:db :view-tabs :tabs]))))
      (is (nil? (transition db [:view-tabs/host "unknown"]))))
    (testing "restored tabs choose the opening view over the show's saved preference"
      (let [home (assoc (with-tabs base (assoc (tabs/initial :rig) :restored? true)) :route-name :home)]
        (is (= [:replace :rig] (:studio/navigate (transition home [:studio/settings-received {:ui {:view "assistant"}}]))))))))

(deftest a-bound-window-names-its-runtime-and-refuses-another-show
  (let [bound (show-window/install (dissoc base :show) {:runtime-id "show-a" :show "Club night"})]
    (testing "commands before the first frame name the bound runtime"
      (is (= "show-a" (:runtime-id (second (events/command bound "blackout/set" {:value true})))))
      (is (= "show-a" (get-in (events/guard-request bound {:path "/api/rig" :body {}}) [:body :runtime-id]))))
    (testing "another runtime's state is neither installed nor acknowledged"
      (let [foreign (full-frame bound (assoc (:show base) :runtime-id "show-b" :show-id :main))]
        (is (nil? (get-in foreign [:db :show])))
        (is (nil? (:studio/stream-reply foreign)))
        (is (false? (get-in foreign [:db :connection :connected?])))
        (is (re-find #"Club night" (get-in foreign [:db :connection :error])))))
    (testing "its own runtime installs as usual"
      (let [own (full-frame bound (assoc (:show base) :runtime-id "show-a" :show-id :main))]
        (is (= "show-a" (get-in own [:db :show :runtime-id])))
        (is (some? (:studio/stream-reply own)))))
    (testing "an unbound browser tab follows its server"
      (is (false? (show-window/refuses? base "anything")))
      (is (= "show-b" (get-in (full-frame (dissoc base :show) (assoc (:show base) :runtime-id "show-b" :show-id :main))
                              [:db :show :runtime-id]))))))

(deftest strip-renders-tabs-as-an-accessible-tablist
  (let [db (with-tabs base (-> (tabs/initial :perform) (tabs/open :rig)))
        html (renderToStaticMarkup (r/as-element [tabs/strip db]))]
    (is (re-find #"role=\"tablist\"" html))
    (is (= 2 (count (re-seq #"role=\"tab\"" html))))
    (is (re-find #"id=\"view-tab-2\"[^>]*aria-selected=\"true\"|aria-selected=\"true\"[^>]*id=\"view-tab-2\"" html))
    (is (re-find #"Close Rig &amp; space tab" html))
    (is (not (re-find #"Close" (renderToStaticMarkup (r/as-element [tabs/strip (with-tabs base (tabs/initial :perform))]))))
        "a single tab has no close button")))

(deftest the-tablist-holds-only-tabs-and-main-is-their-panel
  (let [db (with-tabs base (-> (tabs/initial :perform) (tabs/open :rig)))
        html (renderToStaticMarkup (r/as-element [tabs/strip db]))
        tablist (second (re-find #"role=\"tablist\"[^>]*>(.*?)</div>" html))]
    (is (= 2 (count (re-seq #"<button" tablist))) "only the two tabs are buttons inside the tablist")
    (is (= 2 (count (re-seq #"role=\"tab\"" tablist))))
    (is (re-find #"aria-hidden=\"true\"[^>]*>×" tablist) "a tab's × is hidden from assistive technology")
    (is (re-find #"aria-keyshortcuts=\"Delete\"" tablist))
    (is (not (re-find #"view-tab-close\"[^>]*aria-label" tablist)))
    (is (re-find #"id=\"view-tab-close\"" (subs html (+ (.indexOf html "</div>") 6))) "Close tab sits after the tablist")
    (is (= {:role "tabpanel" :aria-labelledby "view-tab-2"} (tabs/panel-attributes db)))
    (is (nil? (tabs/panel-attributes base)) "no tabs, no tab panel")))

(deftest device-controls-grey-with-the-reason-on-a-preview-only-show
  (let [reason "Club night owns this Mac's devices and output, so this show is preview-only."
        db (assoc-in base [:show :preview-only] reason)]
    (is (= reason (show-window/preview-only db)))
    (is (nil? (show-window/preview-only base)))
    (is (= {:disabled true :title reason :data-preview-only true :on-click :f}
           (show-window/device-control {:disabled false :on-click :f} reason)))
    (is (= {:disabled false} (show-window/device-control {:disabled false} nil)))
    (is (re-find #"role=\"note\"" (renderToStaticMarkup (r/as-element [show-window/device-note reason]))))))
