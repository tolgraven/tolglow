(ns tolglow.osc "OSC aka. Open Sound Protocol tinga"
  (:require [afterglow
             [controllers :as ct]
             [core :as core]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable set-cue! set-variable! start! stop! sync-to-external-clock]]
             [show-context :refer [*show* set-default-show! with-show]]]
            [overtone.osc :as oosc :refer [osc-close osc-handle osc-listen osc-send]]
            [taoensso.timbre :as timbre]
            [tolglow
             [config :as config :refer [at cfg venue wall]]
             [util :as util :refer []]]))


(def binds (at :osc))

(defn send-msg "Send OSC message on path with args, using configured client"
 [path & args]
 (apply oosc/osc-send @(:client binds) path args))

(defn handle "Handle incoming OSC messages on path with handler, using configured server"
 [path handler]
 (oosc/osc-handle @(:server binds) path handler))

(defn done "Return :done, which removes handler" ;#(:done) suffice no?
 [msg] :done)

(defn listen "Create generic handler for all incoming messages"
 [listener key]
 (oosc/osc-listen @(:server binds) listener key))

(defn pass-args "Extract args from message map, add logger" ;XXX make a macro so even easier?
 [{[one two] :args :as msg} f]
 (f one two)
 (when (cfg :osc :debug)
  (timbre/info msg)))


(defn cue-sender "Setup cue send bind and attach controller update fn"
 [x y path f]
 (when f
  (swap! (:cues binds) conj [x y f path])
  (ct/add-cue-fn! (:cue-grid *show*) x y f)))

(defn receiver "Setup handler at path, wrapping f to straight pass :args from message map"
 [path f]
 (print path f )
 (println (f 1 2))

 #_(when (or (not= f :none) f) ; passing :none skips handler (as passing nil means default in other fns)
  (if (= f :done) ;so can pass :done straight to wrap up, instead of a fn returning it.
   (handle path (fn [msg] :done))
   (handle path (fn [msg] (pass-args msg f)))))) ;^ since would get stuck extracting args otherwise


(defn default-cue-send-fn
 [path state & _]
 (case state
  :started (send-msg path 1)
  :ended (send-msg path 0)
  nil))

(defn default-cue-receive-fn
 [x y vel & more]
 (let [[cue active] (show/find-cue-grid-active-effect *show* x y)
       vel (if (integer? vel) vel (* 127 vel))
       [held k] (map cue [:held :key])]
  (when cue
   (if (pos? vel)
    (if (and active (not held))
     (end-effect! k)
     (show/add-effect-from-cue-grid! x y :velocity vel)) ;XXX get var taking velocity and conform to its spec (0-1, 0-255...) but really go all 0-1 everything always yo
    (when (and active held) ;kill if vel 0 and cue mode held
     (end-effect! k)))))) ; XXX set up so msg can set state of cue var(s?) so dont have to manually expose. like max.Cue


(defn cue-binding "Set up bind so state of a cue gets sent per OSC. Record that fact, so can clean up later. Then set up so that incoming OSC messages can start and end that cue."
 [x y path & {:keys [send-fn receive-fn]}]
 (let [send-fn (or send-fn (partial default-cue-send-fn path))
       receive-fn (or receive-fn (partial default-cue-receive-fn x y))] ;XXX add transform-fn and stuff so don't need to write full handlers to change scaling or whatever
  #_(println x y path send-fn)
  (cue-sender x y path send-fn)
  (receiver path receive-fn)))


(defn clear-cue-bindings "Clear out any OSC cue bindings which have been established."
 []
  (doseq [[x y f path] @(:cues binds)]
   #_(println x y f path)
   (ct/clear-cue-fn! (:cue-grid *show*) x y f)
   (receiver path :done))
  (reset! (:cues binds) #{}))



(defn var-out "Update var bind and attach variable update fn"
 [var-key path f be-receiver] ;bit messy lol
 (swap! (:vars binds) conj [var-key f path be-receiver])
 (show/add-variable-set-fn! var-key f))


(defn var-binding "Arrange to send an OSC message whenever the value of a show variable changes, and record that we did that so it can be cleaned up later. Then set things up so incoming OSC messages update the value of that variable.
 If you need to do anything more complicated than send a message with the raw value of the variable, or update the variable with the raw first value from the incoming OSC message, you can pass your own functions with the optional keyword arguments `:send-fn` and `:receive-fn`. `:send-fn` will be called with the keyword identifying the variable that has changed, and its new value.  `:receive-fn` will be called with the incoming OSC message.
 If you want this binding to not affect reception of messages on the OSC path (for example because you have another variable binding set up which processes these messages, since they contain values for multiple show variables), then pass `:none` as the value for `:receive-fn`."
 [var-key path & {:keys [send-fn receive-fn]}]
 (let [be-receiver (not= receive-fn :none)
       send-fn (or send-fn (fn [_ v] (send-msg path v)))
       receive-fn (or receive-fn #(set-variable! var-key %1))]

  (var-out var-key path send-fn (not= receive-fn :none))
  (receiver path receive-fn) ; :none handled bby receiver
   ;; (when be-receiver (receiver path receive-fn))
   ;; (swap! (:vars binds) conj [var-key send-fn path be-receiver])
   ;; (show/add-variable-set-fn! var-key send-fn)
   #_(when be-receiver (handle path receive-fn))))


(defn clear-var-bindings "Clear out any OSC var bindings which have been established."
 []
  (doseq [[k f path be-receiver] @(:vars binds)]
    (show/clear-variable-set-fn! k f)
    (when be-receiver (receiver path :done)))
  (reset! (:vars binds) #{}))


(defn clear-bindings "Call clear-cue-bindings and clear-var-bindings" []
 (clear-var-bindings)
 (clear-cue-bindings))




(defn start-server "Start osc server, if not already running" []
 (when (nil? (:server binds) #_@core/osc-server)
  (core/start-osc-server (cfg :osc :port-in))))

(defn start-client "Start osc client, if not already running" []
 (when (nil? @(:client binds))
  (reset! (:client binds)
          (oosc/osc-client (cfg :osc :address) (cfg :osc :port-out)))))

(defn attach-all-cues "Bind to all active cues on grid, on paths /x/y and by name?"
 []
 (doseq [x (range 32) y (range 32)] (cue-binding x y (str "/" x "/" y))) ;XXX fix per doc
 )

(defn refresh-cues "Ensure OSC client is up, clear any previous bindings & create new ones"
 []
 (clear-cue-bindings)
 (attach-all-cues))


(defn init "Start OSC apparatus n shit" []
 (start-server)
 (start-client)
 (clear-bindings)
 (attach-all-cues)
 (when (cfg :osc :debug) (listen #(timbre/debug %) :debug)) ;or other way round? either way, log msgs if debugging...
 (oosc/zero-conf-on)
 (print "osc up"))


(defn shutdown "Shut down osc server and clean up." []
  (clear-var-bindings)
  (clear-cue-bindings)
  (core/stop-osc-server)
  (swap! (:client binds) #(when % (osc-close %) nil)))


(defn to-vars! "Set show vars from map"
 [prefix m]
 (let [m (zipmap (map #(keyword (str prefix "-" %1)) (keys m)) (vals m))]
;;  (map set-variable! (keys m) (vals m)))
;;  (map #(apply set-variable! (keyword (str prefix "-" %1)) %2)
 (map set-variable! m)
 #_(map #(apply set-variable! (keyword (str prefix "-" %1)) %2)
      m)))



(defn demo "Make examples demo functional"
 []
 (start-server)
 (let [[left right stage rear] (map wall [:left :right :stage :rear])
       ceiling (venue :ceiling)
       width (- right left)
       depth (- rear stage)
       fns {:a #(to-vars! {:aim-a-x (+ left (* width %1))
                            :aim-a-z (+ stage (* depth %2))})
            :a-y #(to-vars! {:aim-a-y (* ceiling %1)})
            :b #(to-vars! {:aim-b-x (+ left (* width %1))
                            :aim-b-z (+ stage (* depth %2))})
            :b-y #(to-vars! {:aim-b-y (* ceiling %1)})}]

  (map (fn [k f] (receiver (str "/1/aim-" (name k)) f))
       (keys fns) (vals fns))

  (receiver (str "/1/aim-a")
            #(to-vars! {:aim-a-x (+ left (* width %1))
                         :aim-a-z (+ stage (* depth %2))}))

 #_(let [f #(if (pos? %1)
           (show/add-effect!
            :sparkle (fun/sparkle (all-fixtures) :chance (/ %1 256.0) :fade-time 20))
           (show/end-effect! :sparkle))]
  (receiver "/1/sparkle" f))

 (listen (fn [msg] (timbre/info msg)) :debug)
 (oosc/zero-conf-on)
 (print "osc up")))


(defn aim-setup
 [group]
 (let [xyz ["x" "y" "z"]
       [key-x key-y key-z] (map #(util/key-str "aim-group-" group "-" %) xyz)
       [addr-x addr-y addr-z] (map #(str "/1/aim-" group "-" %) xyz)
       [left right stage rear] (map wall [:left :right :stage :rear])
       ceiling (venue :ceiling)
       width (- right left)
       depth (- rear stage)]

  (let [send-fn (fn [_ v] (send-msg addr-x
                                    (/ (- v left) width)
                                    (/ (- (get-variable key-z) stage) depth)))
        receive-fn #(to-vars! {key-x (+ left (* width %1))
                                key-z (+ stage (* depth %2))})]
   (var-binding key-x addr-x :send-fn send-fn :receive-fn #(pass-args % receive-fn)))

 (let [send-fn (fn [_ v] (send-msg "/1/aim-a"
                                   (/ (- (get-variable key-x) left) width)
                                   (/ (- v stage) depth)))]
  (var-binding key-z "/1/aim-a" :send-fn send-fn :receive-fn :none))

 (let [send-fn (fn [_ v] (send-msg addr-y (/ v ceiling)))
       receive-fn #(set-variable! key-y (* ceiling %1))]
  (var-binding key-y addr-y :send-fn send-fn :receive-fn #(pass-args % receive-fn)))))

