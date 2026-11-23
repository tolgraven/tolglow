(ns tolglow.studio.signal-picker-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.events-test :refer [transition]]
            [tolglow.studio.signal-picker :as picker]))

(deftest numeric-signal-types-and-manual-paths
  (is (= :vec3 (picker/value-type [0.2 0.4 0.1])))
  (is (= :vec4 (picker/value-type {:op :variable :type :vec4})))
  (is (= [:motion "watch" 0] (picker/read-path "[:motion \"watch\" 0]")))
  (doseq [invalid ["[]" "#js {}" "[:foo] :bar" "[:foo -1]" ":foo" "[[:foo]]" "garbage" ""]]
    (is (nil? (picker/read-path invalid)))))

(deftest searches-expire-after-another-keystroke-close-or-show-replacement
  (let [initial {:server-projection {:view {:runtime-id "venue" :show-id "live" :epoch 1}}}
        opened (transition initial [::picker/open :input :vec3])
        db (:db opened)
        first-fetch (get-in opened [:dispatch-later 0 :dispatch])
        typed (transition db [::picker/filter :input "wrist"])
        latest (:db typed)
        fetch (get-in typed [:dispatch-later 0 :dispatch])
        requested (:studio/http (transition latest fetch))
        result {:sources [{:path [:variables :controller "wrist" :tint] :type :vec3}]}
        found (conj (:on-success requested) result)]
    (is (nil? (:studio/http (transition latest first-fetch))))
    (is (= {:query "wrist" :type :vec3 :readable? true} (:body requested)))
    (is (= (:sources result) (get-in (transition latest found) [:db ::picker/picker :sources])))
    (let [replaced (assoc-in latest [:server-projection :view :epoch] 2)
          closed (:db (transition latest [::picker/close :input]))]
      (is (= replaced (:db (transition replaced found))))
      (is (= closed (:db (transition closed found)))))))
