(ns tolglow.agent-log
  "The per-decision agent record tolglow-agent-log/1 and decision replay (#856).

  A decision is one provider round of tolglow.integrations.agent/run!: what
  the policy received, what was sent, the backend's raw reply, the tool calls
  validated from it with their command IDs and receipts, immediate outcomes
  and timing. decision-record turns the map run! hands its :on-decision
  option into a bounded record plus content-addressed blobs. Everything here
  is pure; tolglow.agent-log.writer owns the files and its thread.

  Decision replay never calls a model. It feeds the logged raw reply through
  the same decoder and validation a live round uses, checks that the show is
  in the record's epoch and on its basis, and reproduces the commands;
  replay! re-applies them through a dispatcher with their logged command IDs.

    (require '[tolglow.agent-log :as log] '[tolglow.agent-log.writer :as writer])
    (def w (writer/open! \"target/agent-log/night-1.jsonl\"))
    (agent/run! config credential messages tools dispatch! emit!
                {:on-decision (log/decision-hook w (log/model-context model {:runtime \"studio-1\"}))})
    (log/replay! (first (writer/read-records \"target/agent-log/night-1.jsonl\"))
                 {:blob (writer/blob-loader \"target/agent-log/night-1.jsonl\")
                  :basis (log/model-basis current-model)}
                 dispatch!)"
  (:require [afterglow.runtime.replay :as replay]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [tolglow.integrations.agent :as agent])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def schema-id "tolglow-agent-log/1")

(def limits
  "Bounds on one record. :record-chars is the whole JSON line; a value that
  would push it past the bound goes to a blob or the record is refused.
  :blob-chars bounds one blob; a larger value is left out and named in
  :omitted."
  {:record-chars 65536
   :blob-chars 4194304
   :inline-arguments-chars 1024
   :errors 16
   :message-chars 240
   :receipt-items 64
   :commands 128})

(defn json-data
  "value as plain JSON data with keyword keys, exactly as a log line reads back."
  [value]
  (json/read-str (json/write-str value) :key-fn keyword))

(defn digest
  "Canonical digest of value as JSON data: afterglow.runtime.replay/digest,
  which sorts keys itself, over the value with string keys, so keyword and
  string keys digest alike."
  [value]
  (str "sha256:" (replay/digest (json/read-str (json/write-str value)))))

(defn text-digest
  "SHA-256 of text's exact UTF-8 bytes."
  [^String text]
  (let [bytes (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text StandardCharsets/UTF_8))]
    (str "sha256:" (apply str (map #(format "%02x" (bit-and 255 %)) bytes)))))

(defn blob-ref
  "The content address of text: blob:sha256:<hex>."
  [text]
  (str "blob:" (text-digest text)))

;; Private data -----------------------------------------------------------------

(def ^:private secret-key?
  #{"authorization" "proxyauthorization" "xapikey" "apikey" "password" "passwd" "secret" "clientsecret"
    "credential" "credentials" "accesstoken" "refreshtoken" "bearertoken" "idtoken" "privatekey" "sessiontoken"})

(def ^:private secret-patterns
  [#"sk-ant-[A-Za-z0-9_\-]{8,}"
   #"sk-(?:proj-|or-v1-)?[A-Za-z0-9_\-]{16,}"
   #"(?i)bearer\s+[A-Za-z0-9._~+/\-]{8,}=*"
   #"AIza[0-9A-Za-z_\-]{35}"
   #"gh[pousr]_[A-Za-z0-9]{20,}"
   #"xox[abprs]-[A-Za-z0-9\-]{10,}"])

(def redacted "[REDACTED]")

(def ^:private secret-field
  "A JSON string field with a secret name, plain or escaped inside another
  JSON string (a streamed tool argument), up to its value."
  #"(?i)(\\?\"(?:authorization|proxy[-_]?authorization|x[-_]?api[-_]?key|api[-_]?key|password|passwd|secret|client[-_]?secret|credentials?|access[-_]?token|refresh[-_]?token|bearer[-_]?token|id[-_]?token|private[-_]?key|session[-_]?token)\\?\"\s*:\s*\\?\")([^\"\\]*)")

(defn scrub-text
  "Replace provider-key and bearer-token shaped text, and the value of a
  secret-named JSON field written in the text, with [REDACTED]. A field split
  across stream fragments is not recognised; credentials are scrubbed by run!
  before this."
  [^String text]
  (str/replace (reduce (fn [t pattern] (str/replace t pattern redacted)) text secret-patterns)
               secret-field (str "$1" redacted)))

(defn- key-text [k] (str/lower-case (str/replace (if (keyword? k) (name k) (str k)) #"[-_ ]" "")))

(defn scrub
  "value with secret-named entries (authorization, api keys, passwords,
  access tokens and the like) and secret-shaped strings replaced. The
  provider credential itself is scrubbed by run! before the log sees it; this
  catches the rest. Observation tokens and other plain IDs are kept."
  [value]
  (walk/prewalk (fn [x]
                  (cond (map-entry? x) (if (secret-key? (key-text (key x))) [(key x) redacted] x)
                        (string? x) (scrub-text x)
                        :else x))
                value))

;; Record ------------------------------------------------------------------------

(defn- clip [s n] (if (and (string? s) (> (count s) n)) (subs s 0 n) s))

(defn- label [x] (cond (keyword? x) (subs (str x) 1) (nil? x) nil :else (str x)))

(defn decision-id
  "<run>/<round>, or a digest of the run when that would pass 64 characters."
  [run round]
  (let [id (str run "/" round)]
    (if (<= (count id) 64) id (str "d-" (subs (text-digest (str run)) 7 31) "/" round))))

(defn tools-digest
  "Digest of tool declarations as declared, order included."
  [tools]
  (digest (mapv #(select-keys % [:name :description :parameters]) tools)))

(defn- portable-item? [x] (or (nil? x) (string? x) (keyword? x) (boolean? x) (number? x)))

(def ^:private receipt-keys
  [:status :ok? :error :code :message :revision :event-ids :due-beats :late? :late-beats :instance-id :replayed?])

(defn- bounded-receipt
  "The receipt fields a reader needs, each a scalar or a short list of
  scalars, and the digest of the whole result."
  [result]
  (if-not (map? result)
    {:digest (digest result)}
    (let [{:keys [message-chars receipt-items]} limits]
      (into {:digest (digest result)}
            (keep (fn [k]
                    (let [v (get result k)]
                      (cond (string? v) [k (clip (scrub-text v) message-chars)]
                            (portable-item? v) (when (some? v) [k v])
                            (and (sequential? v) (every? portable-item? v)) [k (vec (take receipt-items v))]))))
            receipt-keys))))

(defn- rejected? [result]
  (and (map? result) (or (= :rejected (:status result)) (= "rejected" (:status result)) (false? (:ok? result)))))

(defn- command [run i {:keys [id name arguments command-id problems result replayed?]}]
  (let [arguments (scrub arguments)
        text (json/write-str arguments)
        inline? (<= (count text) (:inline-arguments-chars limits))
        valid? (empty? problems)]
    {:entry (cond-> {:command-id (or (clip command-id 256) (str run "/invalid-" i))
                     :type "tool" :op (clip (str name) 128) :valid valid?
                     :arguments_digest (digest arguments)}
              replayed? (assoc :replayed true)
              inline? (assoc :arguments arguments)
              (not inline?) (assoc :arguments_ref (blob-ref text))
              (and valid? (some? result)) (assoc :receipt (bounded-receipt result)))
     :blob (when-not inline? text)
     :errors (for [{:keys [path message]} problems]
               {:call (str id) :path (mapv label path) :message (clip (str message) (:message-chars limits))})
     :outcome (cond (not valid?) {:event (or command-id (str run "/invalid-" i)) :status "rejected" :reason "invalid-tool-call"}
                    (rejected? result) {:event command-id :status "rejected"
                                        :reason (clip (or (label (:code result)) (label (:error result)) "rejected")
                                                      (:message-chars limits))})}))

(defn- add-blob
  "Assoc text under its ref unless it passes the blob bound. Returns
  [blobs ref omitted?]."
  [blobs text]
  (if (> (count text) (:blob-chars limits))
    [blobs (blob-ref text) true]
    (let [ref (blob-ref text)] [(assoc blobs ref text) ref false])))

(defn- drop-nils [m] (into {} (remove (comp nil? val)) m))

(defn- live-shape-errors [record]
  (if (true? (get-in record [:session :offline]))
    (cond-> [] (not (contains? record :outcome)) (conj {:path [:outcome] :message "An offline record has an outcome"})
            (some #(contains? record %) [:clock :commands]) (conj {:path [] :message "An offline record has no clock or commands"}))
    (cond-> [] (not (contains? record :clock)) (conj {:path [:clock] :message "A live record has a clock"})
            (not (contains? record :commands)) (conj {:path [:commands] :message "A live record has commands"}))))

(def schema
  "The implemented record schema, resources/schemas/agent-log-1.schema.json."
  (delay (-> (io/resource "schemas/agent-log-1.schema.json") slurp json/read-str (dissoc "$schema" "$id"))))

(defn validate-record
  "Path errors for record against the schema, the live and offline rule and
  the record-size bound. Empty when the record may be written."
  [record]
  (let [errors (into (agent/validate-arguments @schema record) (live-shape-errors record))
        chars (count (json/write-str record))]
    (cond-> errors
      (> chars (:record-chars limits))
      (conj {:path [] :message (str "The record is " chars " characters, over the bound of " (:record-chars limits))}))))

(defn decision-record
  "Build the record and its blobs for decision, the map run! hands
  :on-decision. context is the host's view at the time the policy observed:
  {:session {:id :show :epoch :runtime :conversation} :actor :policy
   :observation {:token :kind :base :digest :snapshot_ref} :clock {:observed_beat
   :frame :rev} :window}. Without an observation from a resolver, the request
  messages are the observation: their digest, their blob as snapshot_ref.
  Returns {:record :blobs {ref text}} or throws :agent-log/invalid-record."
  [context decision]
  (let [{:keys [run-id round conversation-id provider model messages tools raw text results error timing]} decision
        run (str run-id)
        request (scrub {:messages messages})
        request-text (json/write-str request)
        raw (str raw)
        scrubbed-raw (scrub-text raw)
        parsed-text (json/write-str (scrub {:text text :tool-calls (mapv #(select-keys % [:id :name :arguments]) results)}))
        [blobs request-ref request-omitted?] (add-blob {} request-text)
        [blobs raw-ref raw-omitted?] (add-blob blobs scrubbed-raw)
        [blobs parsed-ref parsed-omitted?] (add-blob blobs parsed-text)
        built (if error [] (vec (take (:commands limits) (map-indexed #(command run %1 %2) results))))
        blobs (into blobs (keep (fn [{:keys [blob]}] (when (and blob (<= (count blob) (:blob-chars limits)))
                                                       [(blob-ref blob) blob]))) built)
        errors (vec (take (:errors limits)
                          (concat (when error [{:path [] :message (str "The reply was rejected before dispatch: " (label error))}])
                                  (mapcat :errors built))))
        observation (merge {:token (or (get-in context [:observation :token]) (decision-id run round))
                            :kind "snapshot"
                            :digest (digest request)
                            :snapshot_ref request-ref}
                           (drop-nils (:observation context)))
        session (drop-nils (merge {:id (or conversation-id run) :show "" :epoch 0 :runtime ""}
                                  (:session context)
                                  {:run run :conversation conversation-id}))
        offline? (true? (:offline session))
        omitted (cond-> [] request-omitted? (conj "request") raw-omitted? (conj "raw") parsed-omitted? (conj "parsed"))
        record (cond-> {:schema schema-id
                        :decision (decision-id run round)
                        :round round
                        :session session
                        :actor (merge {:role "orchestrator" :profile "default"}
                                      (drop-nils {:backend (label provider) :model (some-> model str)})
                                      (:actor context))
                        :observation observation
                        :request (drop-nils {:digest (digest {:provider (label provider) :model model :messages (:messages request)})
                                             :ref request-ref :tools (tools-digest tools) :chars (count request-text)})
                        :output {:raw raw-ref :parsed parsed-ref :chars (count raw)}
                        :validation {:status (if (or error (seq errors)) "rejected" "accepted") :errors errors}}
                 (:policy context) (assoc :policy (:policy context))
                 (:window context) (assoc :window (:window context))
                 (not offline?) (assoc :clock (merge {:observed_beat 0.0 :frame 0 :rev 0}
                                                     (:clock context)
                                                     {:latency_ms (drop-nils {:policy (:policy-ms timing) :admit (:admit-ms timing)})})
                                       :commands (mapv :entry built))
                 (and (not offline?) (seq (keep :outcome built))) (assoc :outcomes (vec (keep :outcome built)))
                 (seq omitted) (assoc :omitted omitted)
                 (or (not= raw scrubbed-raw) (not= request-text (json/write-str {:messages messages}))) (assoc :redacted true))
        record (json-data record)
        problems (validate-record record)]
    (when (seq problems)
      (throw (ex-info "The agent decision does not make a valid bounded record"
                      {:agent-log/error :invalid-record :errors problems})))
    {:record record :blobs blobs}))

(defn model-context
  "Context for decision-record from a committed show model, read at the time
  the policy observed it. extra merges in (:runtime, :actor, :policy,
  :observation and so on)."
  [model extra]
  (merge-with merge
              {:session {:show (str (or (:show-id model) "")) :epoch (or (:epoch model) 0)}
               :clock {:observed_beat (double (get-in model [:transport :beat] 0.0))
                       :frame (long (get model :frame-number 0)) :rev (long (get model :revision 0))}}
              (update extra :session #(merge (select-keys extra [:runtime]) %))))

(defn model-basis
  "The basis a replay compares with a record: the show epoch and revision of
  model, plus an observation :digest or :token when the caller has one."
  ([model] (model-basis model {}))
  ([model observation]
   (merge {:epoch (or (:epoch model) 0) :rev (long (get model :revision 0))}
          (select-keys observation [:digest :token]))))

(defn decision-hook
  "An :on-decision function for run! that builds each record on the run's
  thread and hands it to writer with writer/log!, which never blocks.
  context is a map, or a function of the decision returning one, read before
  the record is built. A decision that makes no valid record is counted by
  the writer as refused, never thrown into the run."
  [writer context]
  (let [log! (requiring-resolve 'tolglow.agent-log.writer/log!)
        refuse! (requiring-resolve 'tolglow.agent-log.writer/refuse!)]
    (fn [decision]
      (try (log! writer (decision-record (if (fn? context) (context decision) context) decision))
           (catch clojure.lang.ExceptionInfo e
             (refuse! writer (ex-data e)))))))

;; Replay ------------------------------------------------------------------------

(defn- rederived-commands [record raw tools]
  (let [provider (keyword (get-in record [:actor :backend]))
        {:keys [errors] :as reply} (agent/decode-reply provider raw)
        calls (:tools reply)
        definitions (into {} (map (juxt :name identity)) tools)
        run (get-in record [:session :run])]
    {:errors errors
     :text (:text reply)
     :commands (if (some #(or (:error %) (not (and (string? (:id %)) (not (str/blank? (:id %)))))) calls)
                 []
                 (vec (map-indexed (fn [i {:keys [id name input] :as call}]
                                     (let [arguments (scrub input)]
                                       {:command-id (str run "/" id) :op (str name)
                                        :valid (empty? (agent/call-problems definitions call))
                                        :arguments input
                                        :arguments_digest (digest arguments)
                                        :index i}))
                                   calls)))}))

(defn- comparable [commands] (mapv #(select-keys % [:command-id :op :valid :arguments_digest]) commands))

(defn- refuse [code message & [data]]
  (merge {:ok? false :code code :message message} data))

(defn replay-decision
  "Reproduce record's commands without a model. options: :blob (ref to text,
  e.g. writer/blob-loader), :tools (the declarations, default
  agent/default-tools) and :basis {:epoch :rev :digest :token} of the show it
  would be applied to. Refuses, in this order: an invalid record (:invalid-
  record), another show epoch (:epoch-mismatch), a basis that differs from the
  record's revision, observation digest or token (:basis-mismatch, or
  :basis-missing when the basis names none of them), other tool declarations
  (:tools-mismatch), a reply that was left out, missing or altered
  (:missing-reply), an undecodable reply (:reply-unreadable), and commands
  that differ from the logged ones (:commands-differ). Otherwise {:ok? true
  :commands [...] :text}."
  [record {:keys [blob tools basis] :or {tools agent/default-tools}}]
  (let [record (json-data record)
        problems (validate-record record)
        epoch (get-in record [:session :epoch])
        recorded {:rev (get-in record [:clock :rev]) :digest (get-in record [:observation :digest])
                  :token (get-in record [:observation :token])}
        compared (filter #(and (contains? basis %) (some? (get recorded %))) [:rev :digest :token])
        differing (vec (filter #(not= (get basis %) (get recorded %)) compared))
        raw-ref (get-in record [:output :raw])
        raw (when (and blob (not (some #{"raw"} (:omitted record)))) (try (blob raw-ref) (catch Exception _ nil)))]
    (cond
      (seq problems) (refuse :invalid-record "The record is not a valid tolglow-agent-log/1 record" {:errors problems})
      (true? (get-in record [:session :offline])) (refuse :offline "An offline record has no commands to replay")
      (not= epoch (:epoch basis))
      (refuse :epoch-mismatch (str "The record belongs to show epoch " epoch ", not " (pr-str (:epoch basis))))
      (empty? compared) (refuse :basis-missing "The basis names no revision, observation digest or token to compare")
      (seq differing)
      (refuse :basis-mismatch (str "The show is not on the record's basis: " (str/join ", " (map name differing)) " differ")
              {:expected (select-keys recorded differing) :actual (select-keys basis differing)})
      (not= (tools-digest tools) (get-in record [:request :tools]))
      (refuse :tools-mismatch "The tool declarations differ from those the decision was made with")
      (not (and (string? raw) (= raw-ref (blob-ref raw)) (< (count raw) agent/max-logged-reply-chars)))
      (refuse :missing-reply "The logged reply is missing, altered or incomplete; a replay never asks a model")
      :else
      (let [{:keys [errors commands text]} (try (rederived-commands record raw tools)
                                                (catch Exception e {:errors [{:message (.getMessage e)}]}))
            logged (:commands record)]
        (cond
          (seq errors) (refuse :reply-unreadable "The logged reply does not decode" {:errors errors})
          (not= (comparable commands) (comparable logged))
          (refuse :commands-differ "The logged reply no longer yields the logged commands"
                  {:expected (comparable logged) :actual (comparable commands)})
          :else {:ok? true :text text
                 :commands (mapv (fn [c l] (cond-> (dissoc c :index) (:replayed l) (assoc :replayed true)))
                                 commands logged)})))))

(defn replay!
  "Re-apply record's decision through dispatch! [tool arguments metadata]
  (show/dispatch-tool! on a runtime) after replay-decision passes. Each valid
  command that the live round dispatched is sent again with its logged
  command ID, run, conversation and :expected-epoch, so the owner also
  refuses another epoch and a repeated delivery returns its first receipt.
  Returns replay-decision's refusal, or {:ok? true :commands :receipts
  :receipts-match?}, comparing the bounded receipts with the logged ones."
  [record options dispatch!]
  (let [checked (replay-decision record options)]
    (if-not (:ok? checked)
      checked
      (let [record (json-data record)
            {:keys [run conversation epoch]} (:session record)
            sent (filterv #(and (:valid %) (not (:replayed %))) (:commands checked))
            receipts (mapv (fn [{:keys [command-id op arguments]}]
                             (dispatch! op arguments
                                        (cond-> {:run-id run :round (:round record) :command-id command-id
                                                 :tool-call-id (subs command-id (inc (count run)))
                                                 :expected-epoch epoch :epoch epoch}
                                          conversation (assoc :conversation-id conversation))))
                           sent)
            logged (into {} (map (juxt :command-id :receipt)) (:commands record))
            strip #(dissoc % :digest)]
        (assoc checked :receipts receipts
               :receipts-match? (= (mapv #(strip (get logged (:command-id %))) sent)
                                   (mapv (comp strip json-data bounded-receipt) receipts)))))))
