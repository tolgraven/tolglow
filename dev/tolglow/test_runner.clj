(ns tolglow.test-runner
  (:require [clojure.test :as test]))

(def pure-tests
  '[tolglow.compute-test tolglow.fixtures.patch-test tolglow.application-test tolglow.build-manifest-test tolglow.utils.iters-test tolglow.registry-test tolglow.effects-test tolglow.signals-test tolglow.modulation-test
    tolglow.clock-test tolglow.system-controls-test tolglow.aim-paths-test tolglow.studio.aim-paths-host-test
    tolglow.studio.http-transit-test tolglow.transit-test tolglow.studio.state-stream-test tolglow.studio.debug-stream-test
    tolglow.compiler-test tolglow.block-bindings-test tolglow.compiler.batch-test tolglow.predictive-keyframes-test
    tolglow.spatial.actions-test tolglow.spatial-cues-test tolglow.spatial-host-test tolglow.authoring.output-patch-test
    tolglow.composition.tween-test tolglow.composition.envelope-test
    tolglow.composition.sequence-test tolglow.composition.macro-test
    tolglow.composition.response-test tolglow.native-composition-test
    tolglow.automation.random-test tolglow.automation.policy-test
    tolglow.integrations.fixtures-test tolglow.integrations.ableton-test
    tolglow.integrations.audio-test tolglow.studio.audio-return-test tolglow.integrations.providers-test
    tolglow.integrations.agent-test
    tolglow.lighting-capacity-test tolglow.lighting-scenes-test tolglow.atmosphere.control-test
    tolglow.lighting-budget-test tolglow.lighting-activity-test tolglow.output-response-test
    tolglow.autopilot.signal-test tolglow.autopilot.statistics-test tolglow.autopilot.actions-test
    tolglow.autopilot.controller-test tolglow.autopilot.target-response-test tolglow.autopilot-host-test tolglow.performance-controls-test
    tolglow.integrations.analysis-test
    tolglow.integrations.gdtf-test
    tolglow.venue-fog-test tolglow.fog-optics-test tolglow.fog-calibration-test tolglow.venue-performance-test
    tolglow.calibration-test tolglow.studio-test tolglow.effects.faults-test tolglow.studio.web-test tolglow.composition-test tolglow.agents-test
    tolglow.persistence-test tolglow.expressive-test tolglow.audio.capture-test tolglow.agents.credentials-test
    tolglow.fixtures.definitions-test tolglow.fixtures.library-test
    tolglow.connections-test tolglow.studio.connections-host-test
    tolglow.studio.output-options-test
    tolglow.studio.fixture-host-test tolglow.rig-test tolglow.pixel-layout-test tolglow.pixel-layout-host-test tolglow.rig-surfaces-test
    tolglow.reflected-surfaces-test tolglow.fixture-photometry-test
    tolglow.rig.rdm-test tolglow.studio.rdm-host-test
    tolglow.integrations.live-sync-test tolglow.studio.link-pause-test tolglow.studio.sync-host-test tolglow.studio.modulation-host-test
    tolglow.studio.display-test tolglow.integrations.patch-test tolglow.studio.routing-host-test
    tolglow.repl-test tolglow.repl.sessions-test tolglow.studio.repl-host-test
    tolglow.development.workspace-test tolglow.development-test tolglow.studio.restart-host-test
    tolglow.development.terminal-test tolglow.sensors-test tolglow.sensors.osc-test
    tolglow.studio.sensor-host-test tolglow.output.scheduler-test
    tolglow.waveforms-test tolglow.development.agents-test tolglow.studio.application-agent-host-test
    tolglow.event-subscribers-test tolglow.studio.projection-test tolglow.studio.active-preview-test])

(def suites
  {"pure" pure-tests
   "parity" (into pure-tests '[tolglow.fixtures.patch-parity-test tolglow.fixtures-test tolglow.parity-test])})

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
