(rand-nth (apply #(for [time ["atrocious" "unbecoming" "not functional"]
                        is '[man brah fam] due '[damn tots quite]
                        :let [{:keys [ult] :as s} (mapv %1 [due time is] '[\  ", " .])]]
                   (%2 %1 (get-in s ult))) [str apply]))
