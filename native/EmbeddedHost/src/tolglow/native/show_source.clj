(ns tolglow.native.show-source
  "Read an operator-selected show folder with an isolated Clojure interpreter."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sci.core :as sci]
            [tolglow.data :as data]
            [tolglow.modules :as modules]
            [tolglow.show :as show])
  (:import [java.io File]))

(defn- inside-file [^File folder name]
  (let [file (.getCanonicalFile (io/file folder name))]
    (when (and (.startsWith (.toPath file) (.toPath folder)) (.isFile file))
      file)))

(defn- ns-source [folder {:keys [namespace]}]
  (let [base (-> (str namespace) (str/replace "." "/") (str/replace "-" "_"))]
    (some (fn [suffix]
            (when-let [file (inside-file folder (str base suffix))]
              {:file (str file) :source (slurp file)}))
          [".clj" ".cljc"])))

(defn- interpreter [folder]
  (sci/init {:load-fn (partial ns-source folder)
             :namespaces {'tolglow.show {'submit! show/submit!
                                         'variables show/variables}}}))

(defn- folder-config [ctx folder]
  (if-let [edn (inside-file folder "tolglow.edn")]
    (data/read-edn (slurp edn))
    (if-let [source (inside-file folder "show.clj")]
      (do (sci/eval-string* ctx (slurp source))
          (if-let [config (sci/resolve ctx 'show/config)]
            @config
            (throw (ex-info "show.clj must define show/config" {}))))
      (throw (ex-info "Choose a folder containing tolglow.edn or show.clj" {})))))

(defn- module-source [ctx namespaces id]
  (some (fn [n]
          (sci/eval-form ctx (list 'require (list 'quote n)))
          (some-> (sci/resolve ctx (symbol (str n) (name id))) deref))
        namespaces))

(defn read-show
  "Provide the same config and ordered setup functions to the native show runner.
  Source namespaces load from this folder alone and each open gets a fresh SCI context."
  [folder registry]
  (let [folder (.getCanonicalFile (io/file folder))
        ctx (interpreter folder)
        config (folder-config ctx folder)
        {:keys [components modules post search-in-namespaces]} (:init config)
        ids (concat components (filter #(:enabled (modules/settings config %) true)
                                       (concat modules post)))
        extend (fn [result id]
                 (if (contains? result id)
                   result
                   (if-let [setup (module-source ctx search-in-namespaces id)]
                     (modules/register result id setup)
                     result)))]
    {:config (update config :init dissoc :search-in-namespaces)
     :registry (reduce extend registry ids)}))
