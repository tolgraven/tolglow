(ns tolglow.controllers.content-browser-host
  "Read-only Push content from a committed show and its active checkpoint owner.

  Checkpoint timestamps describe availability. Exact load requires the owner to
  open the selected artifact under an exact source token and to fence the
  caller's runtime, show and epoch when admitting the restore. Exact launch and
  attach require the frame owner to check the selected registry version when
  it applies the command, before resolving the cue or binding the waveform."
  (:require [tolglow.controllers.content-browser :as browser]
            [tolglow.show :as show]))

(def ^:private checkpoint-handles
  {:named :checkpoint/named :recovery :checkpoint/recovery})

(defn- session [model]
  (select-keys model [:show-id :epoch]))

(defn- checkpoint-descriptors [saves {:keys [show-id checkpoints]}]
  (let [name (some #(when (= show-id (:show-id %)) (:name %)) saves)]
    (into [] (keep (fn [[kind handle]]
                     (let [modified (get-in checkpoints [kind :modified-at-ms])]
                       (when (nat-int? modified)
                         {:show-id show-id :source-kind kind :handle handle
                          :name name :revision modified}))))
          checkpoint-handles)))

(defn- source-key [{:keys [show-id source-kind]}] [show-id source-kind])

(defn- capture [runtime saves]
  (when-not (vector? saves)
    (throw (ex-info "Expected a vector of configured save descriptors" {})))
  (let [before (:model @(:state runtime))
        status (show/recovery-status runtime)
        model (:model @(:state runtime))
        owned (when (and (= (session before) (session model)) (= (:show-id model) (:show-id status)))
                (checkpoint-descriptors saves status))
        owned-keys (into #{} (map source-key) owned)]
    {:model model :saves (into (filterv #(not (contains? owned-keys (source-key %))) saves) owned)}))

(defn project
  "Project one browser page for a runtime. Configured saves are the trusted
  descriptor vector accepted by content-browser/catalog. The active owner's
  available checkpoint takes precedence for the same show and source kind.
  Call from the UI or display worker to keep checkpoint status reads off the
  frame owner."
  [runtime configured-saves view]
  (let [{:keys [model saves]} (capture runtime configured-saves)]
    (merge (browser/project (browser/catalog model saves) view)
           (session model) {:runtime-id (:id runtime)})))

(defn preview
  "Describe a selected row from project. Pass its exact :selection and the
  enclosing :runtime-id."
  [runtime configured-saves {:keys [runtime-id selection]}]
  (or (when (and @(:running runtime) (= (:id runtime) runtime-id))
        (let [{:keys [model saves]} (capture runtime configured-saves)]
          (when @(:running runtime)
            (browser/plan model saves {:op :preview :selection selection}))))
      {:status :rejected :reason :stale-session}))
