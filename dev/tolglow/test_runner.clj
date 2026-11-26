(ns tolglow.test-runner
  (:require [clojure.test :as test]))

(def pure-tests
  '[tolglow.visualization.cue-pads-test tolglow.visualization.parameter-views-test tolglow.visualization.musical-grid-test
    tolglow.visualization.vector-space-test
    tolglow.visualization.waveform-shapes-test
    tolglow.effects.sampling-quality-test tolglow.effects.selection-test tolglow.compute-test tolglow.fixtures.patch-test tolglow.application-test tolglow.desktop-test tolglow.build-manifest-test tolglow.utils.iters-test tolglow.registry-test tolglow.effects-test tolglow.signals-test tolglow.modulation-test tolglow.parameter-sources-test
    tolglow.clock-test tolglow.transport-pause-test tolglow.observations-test tolglow.system-controls-test tolglow.aim-paths-test tolglow.studio.aim-paths-host-test tolglow.position-hold-test
    tolglow.studio.http-transit-test tolglow.transit-test tolglow.studio.state-stream-test tolglow.studio.debug-stream-test
    tolglow.compiler-test tolglow.block-bindings-test tolglow.material-library-test tolglow.material-inputs-test
    tolglow.legacy-frame-oracle-test tolglow.worker-publication-oracle-test
    tolglow.effects.material-design-test tolglow.effects.variable-edit-test tolglow.effects.group-channels-test
    tolglow.effects.group-channels-host-test tolglow.compiler.batch-test tolglow.compiler.capability-test
    tolglow.predictive-keyframes-test
    tolglow.perf.effect-samples-test
    tolglow.spatial.actions-test tolglow.spatial.sampling-test tolglow.spatial-cues-test tolglow.spatial-host-test tolglow.authoring.output-patch-test
    tolglow.authoring.cue-variations-test tolglow.authoring.variation-control-test
    tolglow.composition.tween-test tolglow.composition.envelope-test
    tolglow.composition.sequence-test tolglow.composition.macro-test
    tolglow.composition.sequence-edit-test
    tolglow.composition.response-test tolglow.native-composition-test
    tolglow.automation.random-test tolglow.automation.policy-test tolglow.automation.launch-params-test
    tolglow.automation.sections-test tolglow.automation.r1o-scenario-test
    tolglow.integrations.fixtures-test tolglow.integrations.ableton-test
    tolglow.integrations.audio-test tolglow.studio.audio-return-test tolglow.integrations.providers-test
    tolglow.integrations.agent-test
    tolglow.lighting-capacity-test tolglow.lighting-scenes-test tolglow.atmosphere.control-test
    tolglow.lighting-budget-test tolglow.mixed-stage-test tolglow.lighting-activity-test tolglow.output-response-test
    tolglow.autopilot.signal-test tolglow.autopilot.statistics-test tolglow.autopilot.actions-test
    tolglow.autopilot.controller-test tolglow.autopilot.target-response-test tolglow.autopilot-host-test tolglow.performance-controls-test
    tolglow.integrations.analysis-test tolglow.integrations.sections-test
    tolglow.integrations.reference-position-test tolglow.integrations.track-profile-test tolglow.integrations.section-patterns-test
    tolglow.integrations.mpv-observations-test tolglow.integrations.mpv-player-test tolglow.studio.section-reference-host-test
    tolglow.integrations.gdtf-test
    tolglow.venue-fog-test tolglow.fog-optics-test tolglow.fog-calibration-test tolglow.venue-performance-test tolglow.perf.beam-admission-test tolglow.perf.independent-effects-test tolglow.perf.weighted-live-test tolglow.stride-break-even-test
    tolglow.calibration-test tolglow.studio-test tolglow.effects.faults-test tolglow.effects.state-migration-test tolglow.effects.modulation-reload-test tolglow.studio.web-test tolglow.studio.pretty-test tolglow.guest-admission-test tolglow.composition-test tolglow.agents-test tolglow.agents.forms-test
    tolglow.persistence-test tolglow.show.recovery-test tolglow.expressive-test tolglow.audio.capture-test tolglow.audio.shape-test tolglow.audio.stems-test tolglow.audio-lfo-test tolglow.agents.credentials-test
    tolglow.fixtures.definitions-test tolglow.fixtures.library-test
    tolglow.connections-test tolglow.studio.connections-host-test
    tolglow.studio.output-options-test
    tolglow.studio.fixture-host-test tolglow.rig-test tolglow.pixel-layout-test tolglow.pixel-layout-host-test tolglow.rig-surfaces-test
    tolglow.reflected-surfaces-test tolglow.fixture-photometry-test
    tolglow.rig.rdm-test tolglow.studio.rdm-host-test
    tolglow.integrations.live-sync-test tolglow.integrations.phrase-capture-test tolglow.integrations.phrase-clip-test
    tolglow.integrations.live-connection-test tolglow.integrations.live-capture-roundtrip-test
    tolglow.integrations.live-companion-contract-test tolglow.integrations.phrase-capture-host-test
    tolglow.studio.link-pause-test tolglow.studio.sync-host-test tolglow.studio.modulation-host-test
    tolglow.studio.display-test tolglow.integrations.patch-test tolglow.studio.routing-host-test tolglow.studio.stem-routing-host-test
    tolglow.repl-test tolglow.repl.sessions-test tolglow.studio.repl-host-test
    tolglow.development.workspace-test tolglow.development-test tolglow.studio.restart-host-test tolglow.cue-timing-host-test
    tolglow.development.terminal-test tolglow.sensors-test tolglow.sensors.osc-test tolglow.sensors.audience-test
    tolglow.studio.sensor-host-test tolglow.output.scheduler-test tolglow.perf.output-deadlines-test
    tolglow.waveforms-test tolglow.development.agents-test tolglow.studio.application-agent-host-test
    tolglow.render.workers-test tolglow.request-archive-test
    tolglow.knobs-test tolglow.knob-sweep-test tolglow.controllers.push-knobs-test tolglow.knob-presets-test
    tolglow.knob-candidates.generate-test tolglow.knob-candidates-test
    tolglow.event-subscribers-test tolglow.beat-scheduler-test tolglow.studio.projection-test tolglow.studio.active-preview-test tolglow.push-scene-test
    tolglow.show.scene-draft-test
    tolglow.controllers.content-browser-test tolglow.controllers.content-browser-host-test
    tolglow.controllers.content-browser-display-test tolglow.surface.link-host-test tolglow.surface.descriptor-test
    tolglow.surface.x-touch-host-test
    tolglow.timeline-test
    tolglow.preview.airplay-test
    tolglow.personalities-test tolglow.personalities.icon-test tolglow.personalities-host-test
    tolglow.showcase-test tolglow.venues.gig-2023-test
    tolglow.video-strips-test tolglow.studio.video-strips-host-test
    tolglow.video-surfaces-test tolglow.studio.video-surfaces-host-test])

(def suites
  {"pure" pure-tests
   "parity" (into pure-tests '[tolglow.fixtures.patch-parity-test tolglow.fixtures-test tolglow.parity-test
                             tolglow.showcase-parity-test tolglow.venues.gig-2023-parity-test])})

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
