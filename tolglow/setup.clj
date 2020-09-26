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
   [clojure.pprint]
   ; [rebel-readline.core :as rlcore]
   ; [rebel-readline.clojure.line-reader :as rlreader]
   ; [rebel-readline.clojure.service.local :as rllocal]

   [taoensso.timbre :as timbre]
   [tolglow
    [config :as config :refer [at cfg param-data var-data]]
    [osc :as osc]
    [page :as page]
    [util :as util :refer []]
    [viz :as viz]
    [vars :as vars]]))

(defn init
 ([f id & required]
  (let [run? (or (first required) ;core component, must always run
                 (not id) ;if no id supplied it's a random fn with no settings, so run
                 (:enabled (cfg id) true) ;lookup fallback so will run if there is no :enabled key in map
                 ;; (or (true? (cfg id)) (not (boolean? (cfg id)))))] ;also true if key is missing
                 (true? (cfg id)))
        opts (if (map? (cfg id)) (dissoc (cfg id) :enabled))]
  (print "\n"(if run? "📗" "❌") (name id) "\t")  ;XXX switch to setting/codepoint
         ;; (clojure.pprint/pprint (if (map? (cfg id)) (dissoc (cfg id) :enabled) ""))
  (when (and (seq opts) (cfg :print-options)) (clojure.pprint/pprint opts))
  (print "\t")

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
    (println "\n ❌" (name k) "\tCouldn't resolve setup fn")))))


(defn osc []
 (osc/init))

(defn max-msp []
 (apply require (cfg :max-msp :require))
 (reset! (cfg :ns :active) (cfg :max-msp :ns)))

(defn clock-sync "Gracefully attempt to init sync..." []
 (try (sync-to-external-clock (sync-to-midi-clock)) ; throws a shitty assertion, nuke ffs
      (catch IllegalArgumentException e
       (print "No sync sources..."))))

(defn web-server "Wrap since built-in swap! is broken" []
 (when (nil? @core/web-server)
  (core/start-web-server (cfg :web-server :port))))

(defn nrepl [] (core/start-nrepl (cfg :nrepl :port)))
(defn terminal-repl []
 ; (try
 ;  (with-bindings
 ;   ;; (set! *print-level* 6) ;avoid krazy spew. hurr how set these outside of repl?
 ;   ;; (set! *print-length* 20)
 ;   (rlcore/with-readline-in
 ;    (rlreader/create (rllocal/create))
 ;    (clojure.main/repl :prompt (fn []))))
 ;  (catch clojure.lang.ExceptionInfo e
 ;    (if (-> e ex-data :type (= :rebel-readline.jline-api/bad-terminal))
 ;      (do (println (.getMessage e))
 ;          #_(clojure.main/repl)) ;skip fallback repl as likely due to running lein repl yeah?
 ;      (throw e))))
 )

(defn fixtures []
 (util/patch-cfg!)
 (util/reset-fixture-binds!))

;; XXX we want (here, as elsewhere) fully transparent reloading, so that a change in
;; an fx, fixture, cue def can be reloaded in one go, without having to relaunch cues,
;; restore values etc...
(defn cue-pages
 []
 {:pre [(some? *show*)]}
 (page/reload-all))

(defn controllers "Init controllers from config" ;XXX make actual proper
 []
 (ct/auto-bind *show* :device-filter (cfg :controllers :push :name)
                       #_(push-toggle)))


(defn new-show
 [current]
 (when current
  (print "Replacing show" (:id current))
  (with-show current
   (show/stop!)
   (show/blackout-show))
  (show/unregister-show current))
 (show/show :description (cfg :description)
            :refresh-interval (/ 1000 (cfg :hz)) ;DMX 29 to 44 hz.
            :universes (cfg :universes)))

(defn show [] (set-default-show! (swap! (at :show) #(new-show %))))

(defn vars [] (reset! (at :vars) (var-fx/create-for-show *show*)))

(defn set-ns [] (reset! (at :ns) (cfg :ns :base)))

(defn wavetick "Updates var and runs some shit on incoming midi"
 []
 (let [ks [:bar :beat :tatum]
       w (partial cfg :wavetick)
       [dev ch] (map w [:device :channel])
       tickers (fn [k v] (if (= v 127) (vars/update! k inc)))]
  (doseq [div ks]
   (vars/init! (w :maps div :ticker) 0)
   (let [[note ticker] (map #(w :maps div %) [:note :ticker])]
    (util/add-midi-callback dev ch note (partial tickers ticker) :kind :note)))))

(defn visualizer "Start quil viz"
 []
 (timbre/warn "Moved to frontend...")
 #_(tolglow.viz/init))

(defn show-state "Setup vars/effects running from start of show creation, load any existing state..."
 [& {:keys [actions] :or {actions (cfg :show-state :actions)}}]
 (doseq [f actions]
  (f)))

(defn set-macro-path
 [& {:keys [path] :or {path (cfg :macro :save-file)}}]
 (reset! afterglow.web.routes.show-control/macro-record-file path))

(defn load-macros "Should be part of show-state eventually"
 []
 (try (load-file (cfg :macro :save-file))
      ; (catch java.lang.RuntimeException e (println "Couldn't load cue macros...")))) ;in future instead of straight evals should save parsable data and then can choose what to actually fully load...
      (catch Exception e (println "Couldn't load cue macros...")))) ;in future instead of straight evals should save parsable data and then can choose what to actually fully load...

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
