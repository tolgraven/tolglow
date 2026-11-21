(ns tolglow.test-runner
  (:require [clojure.test :as test]))

(def pure-tests
  '[tolglow.build-manifest-test tolglow.utils.iters-test tolglow.registry-test tolglow.effects-test tolglow.signals-test tolglow.modulation-test
    tolglow.clock-test tolglow.system-controls-test
    tolglow.compiler-test tolglow.compiler.batch-test
    tolglow.composition.tween-test tolglow.composition.envelope-test
    tolglow.composition.sequence-test tolglow.composition.macro-test
    tolglow.composition.response-test
    tolglow.automation.random-test tolglow.automation.policy-test
    tolglow.integrations.fixtures-test tolglow.integrations.ableton-test
    tolglow.integrations.audio-test tolglow.integrations.providers-test
    tolglow.integrations.agent-test
    tolglow.integrations.analysis-test
    tolglow.integrations.gdtf-test
    tolglow.studio-test tolglow.studio.web-test tolglow.studio.composition-test tolglow.studio.agents-test
    tolglow.studio.persistence-test tolglow.studio.expressive-test tolglow.studio.audio-test tolglow.studio.credentials-test
    tolglow.studio.fixtures-test tolglow.studio.fixture-service-test
    tolglow.studio.connections-test tolglow.studio.connections-host-test
    tolglow.studio.output-test tolglow.studio.output-options-test
    tolglow.studio.fixture-host-test tolglow.studio.rig-test
    tolglow.studio.sync-test tolglow.studio.sync-host-test tolglow.studio.modulation-host-test
    tolglow.studio.display-test tolglow.integrations.patch-test tolglow.studio.routing-host-test
    tolglow.studio.repl-test tolglow.studio.repl-manager-test tolglow.studio.repl-host-test
    tolglow.studio.workspace-test tolglow.studio.development-test tolglow.studio.restart-host-test
    tolglow.studio.terminal-test tolglow.studio.sensors-test tolglow.studio.sensor-osc-test
    tolglow.studio.sensor-host-test tolglow.studio.output-scheduler-test
    tolglow.studio.waveforms-test tolglow.studio.app-agents-test tolglow.studio.application-agent-host-test])

(def suites
  {"pure" pure-tests
   "parity" (into pure-tests '[tolglow.fixtures-test tolglow.parity-test
                               tolglow.studio.ola-protocol-parity-test])})

(defn -main [& [suite & extra]]
  (let [namespaces (get suites (or suite "pure"))]
    (when (or (seq extra) (nil? namespaces))
      (binding [*out* *err*] (println "Usage: bin/test [pure|parity]"))
      (System/exit 2))
    (let [status (try
                   (doseq [namespace namespaces] (require namespace))
                   (let [{:keys [fail error]} (apply test/run-tests namespaces)]
                     (if (zero? (+ fail error)) 0 1))
                   (catch Throwable error
                     (.printStackTrace error)
                     1)
                   (finally (shutdown-agents)))]
      (System/exit status))))
