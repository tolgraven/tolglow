(ns tolglow.output
  "Select a physical sink from a Tolglow show's startup options."
  (:require [afterglow.output.artnet :as artnet]
            [afterglow.output.ola :as ola]))

(defn open-sink!
  "Select exactly one explicitly configured physical output; nil means preview."
  [{:keys [ola artnet]}]
  (when (and ola artnet)
    (throw (ex-info "Choose either OLA or direct Art-Net output" {})))
  (when-let [options (or ola artnet)]
    (when-not (map? options)
      (throw (ex-info "Output configuration must be a map" {})))
    (if ola (ola/open-ola! options) (artnet/open-artnet! options))))
