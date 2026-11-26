(ns tolglow.qa.personality-icons
  "Write every built-in personality icon and a contact sheet at 24, 48 and 96 px.
  With a Chromium executable the sheet is also captured as a PNG."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.personalities :as personalities]
            [tolglow.personalities.icon :as icon])
  (:import [java.util.concurrent TimeUnit]))

(def golden-directory "test/tolglow/personalities/golden")

(def ^:private sizes [24 48 96])

(defn- file-name [personality] (str (name (:id personality)) ".svg"))

(defn- blend-example []
  (personalities/blend [{:personality (first (filter #(= :squircle (:id %)) personalities/builtins)) :weight 0.7}
                        {:personality (first (filter #(= :spirals (:id %)) personalities/builtins)) :weight 0.3}]))

(defn- escape [text] (str/escape text {\& "&amp;" \< "&lt;" \> "&gt;"}))

(defn- row [label detail svg]
  (let [image (fn [side] (str "<img src=\"" (icon/data-uri svg) "\" width=\"" side "\" height=\"" side "\" alt=\"\">"))]
    (str "<tr><th scope=\"row\">" (escape label) "<small>" (escape detail) "</small></th>"
         (apply str (map #(str "<td>" (image %) "</td>") sizes))
         "<td class=\"light\">" (apply str (map image sizes)) "</td></tr>\n")))

(defn contact-sheet
  "A self-contained page: each icon is embedded as the data URI a display would load."
  []
  (str "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\"><title>Personality icons</title>\n<style>\n"
       "body{margin:0;padding:24px 28px;width:860px;background:#1b2023;color:#dfe4dd;font:14px/1.35 system-ui,sans-serif}\n"
       "h1{font-size:17px;margin:0 0 14px}table{border-collapse:collapse;width:100%}\n"
       "th,td{padding:12px 14px 12px 0;border-top:1px solid #2c3236;text-align:left;vertical-align:middle}\n"
       "thead th{border-top:0;color:#98a39a;font-weight:500;font-size:12px}\n"
       "th small{display:block;color:#98a39a;font-weight:400;font-size:12px}\n"
       "td.light{background:#e9ebe6;padding-left:14px;white-space:nowrap}td.light img{margin-right:14px;vertical-align:middle}\n"
       "</style></head><body>\n<h1>Personality icons · built-ins at 24, 48 and 96 px</h1>\n<table>\n"
       "<thead><tr><th scope=\"col\">Personality</th><th scope=\"col\">24</th><th scope=\"col\">48</th>"
       "<th scope=\"col\">96</th><th scope=\"col\">On a light surface</th></tr></thead>\n<tbody>\n"
       (apply str (map #(row (:name %) (str (:id %)) (icon/svg %)) personalities/builtins))
       (let [mixed (blend-example)] (row "Blend example" (:name mixed) (icon/svg mixed)))
       "</tbody></table>\n</body></html>\n"))

(defn write-icons! [directory]
  (.mkdirs (io/file directory))
  (doseq [personality personalities/builtins]
    (spit (io/file directory (file-name personality)) (icon/svg personality))))

(defn- capture! [page png profile executable]
  (let [process (-> (ProcessBuilder. ["node" "dev/studio/personality-contact-sheet.mjs" page png profile])
                    (.inheritIO))
        _ (.put (.environment process) "CHROMIUM_PATH" executable)
        process (.start process)]
    (try
      (when-not (and (.waitFor process 120 TimeUnit/SECONDS) (zero? (.exitValue process)))
        (throw (ex-info "Contact sheet capture failed" {:page page})))
      (finally (when (.isAlive process) (.destroyForcibly process))))))

(defn -main
  "golden rewrites the checked-in snapshots. Any other argument is an output
  directory for the icons, contact-sheet.html and, with CHROMIUM_PATH set,
  contact-sheet.png."
  [& [target]]
  (try
    (if (= "golden" target)
      (do (write-icons! golden-directory) (println "Wrote" (count personalities/builtins) "snapshots to" golden-directory))
      (let [directory (io/file (or target "target/personality-icons"))
            page (io/file directory "contact-sheet.html")]
        (write-icons! directory)
        (spit page (contact-sheet))
        (println "Wrote" (str page))
        (when-let [executable (System/getenv "CHROMIUM_PATH")]
          (let [png (io/file directory "contact-sheet.png")]
            (capture! (.getAbsolutePath page) (.getAbsolutePath png)
                      (.getAbsolutePath (io/file directory "chrome-profile")) executable)
            (println "Captured" (str png))))))
    (finally (shutdown-agents))))
