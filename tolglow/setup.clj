(ns tolglow.setup "Utility functions to apply settings defined in config.clj"
  (:require
   [afterglow
    [controllers :as ct]
    [core :as core]
    [midi :as midi :refer [sync-to-midi-clock]]
    [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
    [show-context :refer [*show* set-default-show! with-show]]]
   [afterglow.effects.show-variable :as var-fx]
   [clojure.string :as string :refer [capitalize upper-case]]
   [rebel-readline.core :as rlcore]
   [rebel-readline.clojure.line-reader :as rlreader]
   [rebel-readline.clojure.service.local :as rllocal]
   [tolglow
    [config :as config :refer [at cfg param-data var-data]]
    [osc :as osc]
    [page :as page]
    [util :as util :refer []]
    [vars :as vars]]))


(defn init
 ([f id & required]
  (let [run? (or (first required)
                 (not id)
                 (true? (cfg id)) #_(= true (apply cfg id))
                 (:enabled (cfg id)) #_(:enabled (apply cfg id)))]
  (print "\n"(if run? "📗" "❌") (name id) "\t")  ;XXX switch to codepoint
  (when run?
   (if (cfg :debug :force-init)
    (util/catchall f)
    (f))))))

(defn init-by-ks "Resolve ids to fns in setup ns, calling init with that"
 [ids & {:keys [required]}]
 (doseq [k ids]
  (let [f (resolve (symbol "tolglow.setup" (name k)))]
   (if f
    (init f k required)
    (println "\nCouldn't resolve setup fn for key" k)))))


(defn osc []
 (osc/start-server)
 (osc/clear-var-bindings)
 (osc/clear-cue-bindings))

(defn max-msp []
 (apply require (cfg :max :require))
 (reset! (cfg :ns :active) (cfg :max :ns)))

(defn clock-sync "Gracefully attempt to init sync..." []
 (try (sync-to-external-clock (sync-to-midi-clock)) ; throws a shitty assertion, nuke ffs
      (catch IllegalArgumentException e
       (print "No sync sources..."))))

(defn web-server "Wrap since built-in swap! is broken" []
 (when (nil? @core/web-server)
  (core/start-web-server (cfg :web-server :port))))

(defn nrepl [] (core/start-nrepl (cfg :nrepl :port)))
(defn terminal-repl []
 (try
  (with-bindings
   ;; (set! *print-level* 3) ;avoid krazy spew. hurr how set these outside of repl?
   ;; (set! *print-length* 100)
   (rlcore/with-readline-in
    (rlreader/create (rllocal/create))
    (clojure.main/repl :prompt (fn []))))
  (catch clojure.lang.ExceptionInfo e
    (if (-> e ex-data :type (= :rebel-readline.jline-api/bad-terminal))
      (do (println (.getMessage e))
          #_(clojure.main/repl)) ;skip fallback repl as likely due to running lein repl yeah?
      (throw e)))))

(defn fixture-patches []
 (util/patch-cfg!)
 (util/reset-fixture-binds!))

(defn cue-pages
 [& {:keys [origin-page page-data] :or {origin-page 0 #_[0 0] page-data (cfg :pages)}}] ; prob have main page/startup at like 2 2 so plenty space in all directions...
 {:pre [(some? *show*)]}
 (let [#_saved-pos #_(save-view-position)])

 (println "Force" (if (cfg :debug :force-cue-pages) "on" "off"))
 (doseq [[fn-key pages] page-data
         [xb yb & opts] (if (set? pages) pages #{pages})] ;XXX or use whatever :while alternative binding thing...
  (let [[x y] (map #(+ origin-page %) [xb yb])]
   (print "\n" (name fn-key) "\t" x y (or opts "")) ;log
   ;; (puget.printer/cprint (str "\n" (name fn-key) "\t" x y (or opts ""))) ;log
   ;; (pr (puget.printer/cprint-str (str "\n" (name fn-key) "\t" x y (or opts "")))) ;log
   (apply page/create (name fn-key) x y opts))) ;XXX should only generate defs, compare those with saved map, only change updated ones (so dont overwrite active cues)

 #_(osc/refresh-cues) ;prob shouldn't go here, rather modules add hooks to various actions?
 #_(set-view-position (or saved-pos (repeat 2 origin-page))))

(defn controllers "Init controllers from config" ;XXX make actual proper
 []
 (ct/auto-bind *show* :device-filter (cfg :controllers :push :name)
                       #_(push-toggle)))


(defn new-show
 [current]
 (when current
  (println "Replacing show" (:id current))
  (show/unregister-show current)
  (with-show current
   (show/stop!)
   (show/blackout-show)))
 (show/show :description (cfg :description)
            :refresh-interval (/ 1000 (cfg :hz)) ;DMX 29 to 44 hz.
            :universes (cfg :universes)))

(defn show []
 (set-default-show! (swap! (at :show) #(new-show %))))

(defn vars []
 (reset! (at :vars) (var-fx/create-for-show *show*)))

(defn set-ns []
 (reset! (at :ns) (cfg :ns :base)))

(defn set-macro-path
 [& {:keys [path] :or {path (cfg :macro :save-file)}}]
 (reset! afterglow.web.routes.show-control/macro-record-file path))

(defn wavetick "Updates var and runs some shit on incoming midi"
 []
 (let [ks [:bar :beat :tatum]
       w (partial cfg :wavetick)
       [dev ch] (map w [:device :channel])
       tickers (fn [k v] (if (= v 127) (vars/update! k inc)))]
  (doseq [div ks]
   (vars/init! (w :maps div :ticker) 0)
   #_(watch-var (w :maps div :ticker))
   (let [[note ticker] (map #(w :maps div %) [:note :ticker])]
    (util/add-midi-callback dev ch note (partial tickers ticker) :kind :note)
    #_(println device channel note ticker)
    ))))

(defn show-state "Setup vars/effects running from start of show creation, load any existing state..."
 [& {:keys [actions] :or {actions (cfg :show-state :actions)}}]
 (doseq [f actions]
  (f)))

;; XXX for config/measure-data
#_(defn make-points []
 (map #(apply tf/build-distance-measure (% measure-data)) ))
;;  (into {:rig-center [0 (:rig venue) 0 :ignore-z true]
;;                          :walls-x '[(% (:wall venue)) (:rig venue) 0 :ignore-z true]
;;                          :walls-z '[0 (:rig venue) (% (:wall venue))]}
;;                    ;; :wall [((keyword %2) (:wall venue)) (:rig venue) 0 :ignore-z true]
;;
;;                    ;; (conj (map #(into () {(keyword (str % "-wall")) [((keyword %) (:wall venue)) (:rig venue) 0 :ignore-z true]})
;;                    #_(reduce #(assoc %1 (keyword (str (name %2) "-wall"))
;;                                    [(%2 (:wall venue)) (:rig venue) 0 :ignore-z true])
;;                            {} [:left :right])
;;                    #_(reduce #(assoc %1 (keyword (str (name %2) "-wall"))
;;                                    [0 (:rig venue) (%2 (:wall venue))])
;;                            {} [:stage :rear]))
