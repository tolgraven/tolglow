"use strict";
// Setup, Capture and clip-write operations of the companion (#680, #671),
// exercised through the production Adapter and Session with only the Live
// object graph substituted. Nothing here talks to Live.
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { Adapter } = require("../../../resources/devices/ableton-live/live_api.js");
const { COARSE, OPERATIONS } = require("../../../resources/devices/ableton-live/live_session.js");
const { liveSet, PORTS, CHANNELS } = require("./live_set_fixture.js");

test("operation list matches tolglow.integrations.live-connection/operations", () => {
  assert.deepEqual([...OPERATIONS].sort(), ["capture-midi", "configure-track", "ensure-group", "ensure-launcher", "ensure-track", "extend-clip", "fire-clip",
    "inspect-launcher", "inspect-set", "place-clip", "read-clip", "show-clip", "stop-clips", "watch-run-tracks", "write-clip", "write-pads"]);
  assert.deepEqual(COARSE, Array.from({ length: 31 }, (_, i) => i + 1).filter(c => c !== 6), "same as phrase-clip/coarse-controllers");
});

test("inspect-set reports tracks, routing, the companion's track and the Live version without writing", () => {
  const live = liveSet({ tracks: [{ name: "Bass", arm: true, monitor: 0, port: PORTS[2], channel: CHANNELS[3] }, { name: "MIDI" }], host: 1 });
  const r = live.request("inspect-set");
  assert.equal(r.status, "executed");
  assert.deepEqual(r.result.tracks.map(t => [t.name, t.kind, t["arm?"], t.monitor, t.input]),
    [["Bass", "midi", true, "in", { port: "Keystep", channel: 3 }], ["MIDI", "midi", false, "auto", { port: "All Ins", channel: null }]]);
  assert.equal(r.result.tracks[1]["companion?"], true);
  assert.equal(r.result.tracks[0]["companion?"], undefined);
  assert.equal(r.result["live-version"], "12.4.5");
  assert.equal(r.result["can-capture-midi?"], true);
  assert.deepEqual(live.writes, []);
});

test("setup adopts the companion's track, creates the next and configures both by read-back", () => {
  const live = liveSet({ tracks: [{ name: "Bass", arm: true }, { name: "MIDI" }], host: 1 });
  const cues = live.request("ensure-track", { name: "Tolglow Cues", kind: "midi", "host?": true });
  assert.deepEqual(cues.result, { "track-id": "track:" + live.song.tracks[1].id, "created?": false, "adopted?": true });
  const lanes = live.request("ensure-track", { name: "Tolglow Lanes", kind: "midi" });
  assert.equal(lanes.result["created?"], true);
  assert.deepEqual(live.request("ensure-track", { name: "Tolglow Lanes", kind: "midi" }).result,
    { "track-id": lanes.result["track-id"], "created?": false }, "a rerun finds the track by name");
  const configured = live.request("configure-track", { "track-id": cues.result["track-id"], "expected-name": "Tolglow Cues",
    "arm?": true, monitor: "auto", input: { port: "Tolglow", channel: 16 } });
  assert.equal(configured.status, "executed");
  assert.deepEqual(configured.result.track.input, { port: "Tolglow", channel: 16 });
  assert.equal(configured.result.track["arm?"], true);
  assert.equal(configured.result.track["companion?"], true);
  assert.equal(live.song.tracks[0].props.arm, 1, "the musician's armed track is untouched");
  const lanesConfigured = live.request("configure-track", { "track-id": lanes.result["track-id"], "expected-name": "Tolglow Lanes",
    "arm?": true, input: { port: "Tolglow", channel: 15 } });
  assert.equal(lanesConfigured.status, "executed");
  assert.equal(lanesConfigured.result.track["companion?"], undefined);
});

test("configuration refuses before writing: unknown port, stale track, renamed track, duplicates", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Cues" }, { name: "Twin" }, { name: "Twin" }] });
  const id = "track:" + live.song.tracks[0].id;
  const r = live.request("configure-track", { "track-id": id, "arm?": true, input: { port: "Nope", channel: 16 } });
  assert.equal(r.status, "rejected"); assert.equal(r.error.code, "routing-unavailable");
  assert.equal(live.request("configure-track", { "track-id": "track:9999", "arm?": true }).error.code, "stale-track");
  assert.equal(live.request("configure-track", { "track-id": id, "expected-name": "Old", "arm?": true }).error.code, "track-renamed");
  assert.equal(live.request("ensure-track", { name: "Twin" }).error.code, "duplicate-tracks");
  assert.equal(live.request("configure-track", { "track-id": id, monitor: "loud" }).error.code, "monitor");
  assert.deepEqual(live.writes, []);
});

const clip = {
  version: 1, "length-beats": 4, meter: { "beats-per-bar": 4, "bars-per-phrase": 4 },
  notes: [{ pitch: 36, start: 0, duration: 1.5, velocity: 127 }, { pitch: 37, start: 2, duration: 2, velocity: 127 }],
  controllers: [{ beat: 0, controller: 1, value: 64 }, { beat: 0, controller: 33, value: 0 },
    { beat: 1, controller: 1, value: 32 }, { beat: 1, controller: 33, value: 5 },
    { beat: 0.5, controller: 7, value: 127 }, { beat: 0.5, controller: 39, value: 127 }]
};

test("write-clip puts notes and companion-parameter envelopes in a new clip, and read-clip returns them", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }] });
  const track = "track:" + live.song.tracks[0].id;
  const w = live.request("write-clip", { "track-id": track, "expected-name": "Tolglow Capture", "slot-index": null, name: "Phrase", clip });
  assert.equal(w.status, "executed", JSON.stringify(w.error));
  assert.equal(w.result["slot-index"], 0); assert.equal(w.result.notes, 2); assert.equal(w.result["envelope-steps"], 3);
  const created = live.song.tracks[0].slots[0].clip;
  assert.equal(created.props.name, "Phrase");
  assert.deepEqual(created.notes.map(n => [n.pitch, n.start_time, n.duration]), [[36, 0, 1.5], [37, 2, 2]]);
  const lane1 = created.envelopes[live.device.params[0].id];
  assert.deepEqual(lane1.steps.map(s => [s.time, s.duration, Math.round(s.value * 16383)]), [[0, 1, 8192], [1, 3, 32 * 128 + 5]]);
  assert.deepEqual(created.envelopes[live.device.params[5].id].steps.map(s => [s.time, s.value]), [[0.5, 1]], "controller 7 is lane 6");
  const r = live.request("read-clip", { "clip-id": w.result["clip-id"] });
  assert.equal(r.status, "executed");
  assert.deepEqual(r.result.clip.notes, clip.notes);
  const sorted = cs => [...cs].sort((a, b) => a.controller - b.controller || a.beat - b.beat);
  assert.deepEqual(sorted(r.result.clip.controllers),
    sorted(clip.controllers.map(c => (c.controller === 7 || c.controller === 39) ? { ...c, beat: 0 } : c)),
    "sampled on the quarter-beat grid; before its first step an envelope reads its first value");
  assert.equal(r.result.clip["length-beats"], 4);
});

test("write-clip refuses before writing when controllers have no companion, the slot is taken or a lane is missing", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Cues" }, { name: "Tolglow Lanes" }], slots: 1 });
  const lanes = "track:" + live.song.tracks[1].id, cues = "track:" + live.song.tracks[0].id;
  assert.equal(live.request("write-clip", { "track-id": lanes, clip }).error.code, "controllers-need-companion");
  assert.equal(live.request("write-clip", { "track-id": cues, clip: { ...clip, controllers: [{ beat: 0, controller: 64, value: 1 }] } }).error.code,
    "controller-lane");
  live.device.params.pop();
  assert.equal(live.request("write-clip", { "track-id": cues, clip: { ...clip, controllers: [{ beat: 0, controller: 31, value: 1 }] } }).error.code,
    "lane-parameter-missing");
  assert.equal(live.request("write-clip", { "track-id": cues, clip: { ...clip, notes: [{ pitch: 36, start: 3, duration: 2 }] } }).error.code, "note");
  assert.deepEqual(live.calls.filter(c => c.method === "create_clip"), []);
  assert.equal(live.request("write-clip", { "track-id": lanes, clip: { ...clip, controllers: [] } }).status, "executed");
  assert.equal(live.request("write-clip", { "track-id": lanes, clip: { ...clip, controllers: [] } }).error.code, "no-empty-slot");
  assert.equal(live.request("write-clip", { "track-id": lanes, "slot-index": 0, clip: { ...clip, controllers: [] } }).error.code, "slot-occupied");
});

test("a failure after the clip exists is indeterminate, not rejected", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }] });
  const original = live.session.dictionaryCall;
  live.session.dictionaryCall = () => { throw new Error("Host disconnected"); };
  const r = live.request("write-clip", { "track-id": "track:" + live.song.tracks[0].id, clip: { ...clip, controllers: [] } });
  live.session.dictionaryCall = original;
  assert.equal(r.status, "indeterminate");
  assert.ok(live.song.tracks[0].slots[0].clip, "the clip was created before the failure");
});

test("capture-midi returns the new clips, refuses with nothing to capture and is honest when nothing appears", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture", arm: true, received: true }, { name: "Idle", arm: true }] });
  const r = live.request("capture-midi", { destination: "auto" });
  assert.equal(r.status, "executed");
  assert.deepEqual(r.result.clips.map(c => [c["track-id"], c["slot-index"]]), [["track:" + live.song.tracks[0].id, 0]]);
  assert.deepEqual(live.calls.filter(c => c.method === "capture_midi").map(c => c.args), [[0]]);
  live.song.tracks[0].received = false;
  assert.equal(live.request("capture-midi", {}).status, "indeterminate", "invoked, but no clip is visible");
  live.song.props.can_capture_midi = 0;
  assert.equal(live.request("capture-midi", {}).error.code, "nothing-to-capture");
  assert.equal(live.request("capture-midi", { destination: "elsewhere" }).error.code, "capture-destination");
});

test("show, fire and stop act on the named clip and tracks", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }] });
  const w = live.request("write-clip", { "track-id": "track:" + live.song.tracks[0].id, clip: { ...clip, controllers: [] } });
  const id = w.result["clip-id"], numeric = Number(id.slice(5));
  assert.deepEqual(live.request("show-clip", { "clip-id": id }).result, { "clip-id": id, view: "Detail/Clip" });
  assert.equal(live.view.props.detail_clip, "id " + numeric);
  assert.equal(live.appView.props.focused, "Detail/Clip");
  assert.equal(live.request("fire-clip", { "clip-id": id }).status, "executed");
  assert.equal(live.entities[numeric].fired, 1);
  assert.equal(live.request("stop-clips", { "track-ids": ["track:" + live.song.tracks[0].id] }).status, "executed");
  assert.equal(live.song.tracks[0].props.stopped, true);
  assert.equal(live.request("show-clip", { "clip-id": "clip:424242" }).error.code, "live-object-missing");
});

test("echo rule: the feed channel is dropped, playback is tagged with its track, lanes play back as CC pairs", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }] });
  const track = "track:" + live.song.tracks[0].id;
  live.request("configure-track", { "track-id": track, input: { port: "Tolglow", channel: 16 } });
  live.request("handoff", { mode: "lighting" });
  assert.equal(live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 16 }), null, "Tolglow's own monitored feed");
  live.play(0, 0);
  assert.deepEqual(live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }),
    { type: "note-on", note: 36, velocity: 127, channel: 1, "track-id": track, origin: "live-playback" }, "the track plays a clip");
  assert.deepEqual(live.adapter.lane(6, 1), [
    { type: "cc", controller: 7, value: 127, channel: 1, origin: "live-playback", "track-id": track },
    { type: "cc", controller: 39, value: 127, channel: 1, origin: "live-playback", "track-id": track }]);
  assert.deepEqual(live.adapter.lane(1, 8192 / 16383).map(e => e.value), [64, 0]);
  live.request("handoff", { mode: "live" });
  assert.deepEqual(live.adapter.lane(1, 0.5), [], "Live mode forwards nothing");
});

test("echo rule survives a device reload: the feed channel is read back from the companion's track", () => {
  // A configured Set with a freshly loaded companion: setup's rerun writes nothing.
  const live = liveSet({ tracks: [{ name: "Tolglow Capture", arm: true, port: PORTS[1], channel: CHANNELS[16] }] });
  const track = "track:" + live.song.tracks[0].id;
  live.request("handoff", { mode: "lighting" });
  assert.equal(live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 16 }), null, "feed dropped without configure-track");
  assert.equal(live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 })["track-id"], track);
  // Input on channel 1 is clip playback's channel and never treated as the feed.
  const ch1 = liveSet({ tracks: [{ name: "Keys", port: PORTS[2], channel: CHANNELS[1] }] });
  ch1.request("handoff", { mode: "lighting" });
  assert.ok(ch1.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }));
  // A routing change made in Live is picked up by the next inspect-set.
  live.entities[live.song.tracks[0].id].props.input_routing_channel = CHANNELS[15];
  live.request("inspect-set");
  assert.equal(live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 15 }), null);
  assert.ok(live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 16 }));
});

test("an adapter without the session refuses the new operations and keeps the old ones", () => {
  const adapter = new Adapter({ api: () => ({ id: 0 }), dictionaryCall() {}, readDictionary: JSON.parse });
  assert.equal(adapter.handle({ id: "x", op: "inspect-set", payload: {} }).error.code, "session-unavailable");
  assert.equal(adapter.handle({ id: "y", op: "save-set", payload: {} }).error.code, "operation");
});

test("the patch exposes thirty automatable lane parameters routed through deferlow", () => {
  const patch = JSON.parse(fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live/tolglow-live-bridge.maxpat"), "utf8")).patcher;
  const dials = patch.boxes.map(b => b.box).filter(b => b.maxclass === "live.dial");
  assert.equal(dials.length, 30);
  assert.deepEqual(dials.map(d => d.saved_attribute_attributes.valueof.parameter_longname),
    Array.from({ length: 30 }, (_, i) => "Tolglow lane " + (i + 1)));
  for (const d of dials) {
    const k = d.id.replace("lane-dial-", "");
    const toPrepend = patch.lines.find(l => l.patchline.source[0] === d.id).patchline.destination[0];
    const prepend = patch.boxes.find(b => b.box.id === toPrepend).box;
    assert.equal(prepend.text, "prepend lane " + k);
    assert.equal(patch.lines.find(l => l.patchline.source[0] === toPrepend).patchline.destination[0], "low");
  }
  const source = fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live/max_adapter.js"), "utf8");
  assert.ok(source.includes('include("live_session.js")') && /function lane\(k, value\)/.test(source));
});

test("the companion adopts only an empty or Tolglow track, and follows the device when it moves", () => {
  const busy = liveSet({ tracks: [{ name: "Keys", instrument: true }, { name: "Drums", clip: true }, { name: "Tolglow old", instrument: true }, { name: "MIDI" }], host: 0 });
  const refused = busy.request("ensure-track", { name: "Tolglow Cues", kind: "midi", "host?": true });
  assert.equal(refused.status, "rejected");
  assert.equal(refused.error.code, "host-track-in-use");
  assert.match(refused.error.message, /new, empty MIDI track/);
  busy.moveCompanion(1);
  assert.equal(busy.request("ensure-track", { name: "Tolglow Cues", "host?": true }).error.code, "host-track-in-use", "a track with a clip is the musician's");
  assert.deepEqual(busy.writes, [], "nothing was renamed");
  busy.moveCompanion(2);
  assert.equal(busy.request("ensure-track", { name: "Tolglow Cues", "host?": true }).result["adopted?"], true, "a Tolglow-named track may hold devices");
  busy.moveCompanion(3);
  const moved = busy.request("ensure-track", { name: "Tolglow Cues", "host?": true });
  assert.deepEqual(moved.result, { "track-id": "track:" + busy.song.tracks[3].id, "created?": false, "adopted?": true },
    "after the device moves, the host is its new track, not the old one found by name");
  assert.deepEqual(busy.request("ensure-track", { name: "Tolglow Cues", "host?": true }).result,
    { "track-id": "track:" + busy.song.tracks[3].id, "created?": false });
});

test("lane read-back finds change points off the scan grid and keeps values for a late launch", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }] });
  const track = "track:" + live.song.tracks[0].id;
  const w = live.request("write-clip", { "track-id": track, clip: { "length-beats": 8, notes: [{ pitch: 36, start: 2, duration: 2 }] },
    lanes: [{ controller: 1, points: [[2, 8192], [2.5, 100], [1056 / 960 + 4, 16383]] }, { controller: 2, points: [[2, 16383]] }] });
  assert.equal(w.status, "executed", JSON.stringify(w.error));
  const r = live.request("read-clip", { "clip-id": w.result["clip-id"] }).result.clip;
  const coarse = (c) => r.controllers.filter(e => e.controller === c).map(e => [e.beat, e.value]);
  assert.deepEqual(coarse(1), [[0, 64], [2.5, 0], [5.1, 127]], "1.1 beats past bar 1 stays at 5.1, not the next quarter");
  assert.deepEqual(coarse(2), [[0, 127]], "a lane read from beat 0 reports its value before the note; Tolglow gives it to the next launch");
});

test("extend-clip adds lanes to a clip on the same track and refuses another track's clip", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }, { name: "Other" }] });
  const track = "track:" + live.song.tracks[0].id;
  const w = live.request("write-clip", { "track-id": track, clip: { "length-beats": 4, notes: [{ pitch: 36, start: 0, duration: 1 }] },
    lanes: [{ controller: 1, points: [[0, 100]] }] });
  const e = live.request("extend-clip", { "track-id": track, "clip-id": w.result["clip-id"], notes: [{ pitch: 37, start: 1, duration: 1 }],
    lanes: [{ controller: 2, points: [[1, 200], [2, 300]] }] });
  assert.deepEqual(e.result, { notes: 1, "envelope-steps": 2 });
  const clip = live.song.tracks[0].slots[0].clip;
  assert.deepEqual(clip.notes.map(n => n.pitch), [36, 37]);
  assert.deepEqual(clip.envelopes[live.device.params[1].id].steps.map(s => [s.time, s.duration]), [[1, 1], [2, 2]]);
  assert.equal(live.request("extend-clip", { "track-id": "track:" + live.song.tracks[1].id, "clip-id": w.result["clip-id"], notes: [] }).error.code,
    "stale-clip");
  assert.equal(live.request("extend-clip", { "track-id": track, "clip-id": w.result["clip-id"], lanes: [{ controller: 64, points: [[0, 1]] }] }).error.code,
    "controller-lane");
});

test("MIDI events and lane moves reuse the cached companion track", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow Capture" }] });
  live.request("inspect-set");
  live.request("handoff", { mode: "lighting" });
  let lookups = 0;
  const api = live.session.api;
  live.session.api = (p) => { lookups++; return api(p); };
  for (let i = 0; i < 100; i++) {
    live.adapter.midi({ type: "cc", controller: 1, value: i % 128, channel: 1 });
    live.adapter.lane(1, i / 100);
  }
  assert.equal(lookups, 0, "no LiveAPI object per event");
  live.request("inspect-set");
  assert.ok(lookups > 0, "a Set operation refreshes the cache");
});

// Run tracks, run clips and Group Tracks (#683).

test("inspect-set reads Group Track membership without touching a group's input, arm or monitoring", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow group stage", group: true }, { name: "Tolglow group warm", group: true, parent: "Tolglow group stage" },
    { name: "Tolglow run a", parent: "Tolglow group warm" }, { name: "Bass" }] });
  const r = live.request("inspect-set");
  assert.equal(r.status, "executed", JSON.stringify(r.error));
  const [stage, warm] = live.song.tracks;
  assert.deepEqual(r.result.tracks.map(t => [t.name, t.kind, t["group-track-id"]]),
    [["Tolglow group stage", "group", null], ["Tolglow group warm", "group", "track:" + stage.id],
     ["Tolglow run a", "midi", "track:" + warm.id], ["Bass", "midi", null]]);
  assert.equal(r.result.tracks[0].input, null);
});

test("ensure-group finds a placed Group Track by name and refuses to invent one", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow group stage", group: true }, { name: "Tolglow group stage" }] });
  assert.deepEqual(live.request("ensure-group", { name: "Tolglow group stage" }).result,
    { "track-id": "track:" + live.song.tracks[0].id, "created?": false }, "a MIDI track with the same name is not a group");
  const missing = live.request("ensure-group", { name: "Tolglow group warm" });
  assert.equal(missing.status, "rejected");
  assert.equal(missing.error.code, "group-creation-unavailable");
  assert.deepEqual(live.writes, []);
  assert.deepEqual(live.calls.filter(c => c.method.startsWith("create")), []);
});

test("ensure-track inserts a run track after a group's last member and reports where Live put it", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow group stage", group: true }, { name: "Tolglow group warm", group: true, parent: "Tolglow group stage" },
    { name: "Tolglow run a", parent: "Tolglow group warm" }, { name: "Bass" }] });
  const [stage, warm] = live.song.tracks;
  const b = live.request("ensure-track", { name: "Tolglow run b", kind: "midi", "group-track-id": "track:" + stage.id });
  assert.equal(b.status, "executed", JSON.stringify(b.error));
  assert.equal(b.result["group-track-id"], "track:" + stage.id);
  const a2 = live.request("ensure-track", { name: "Tolglow run a · 2", kind: "midi", "group-track-id": "track:" + warm.id });
  assert.deepEqual(live.song.tracks.map(t => t.props.name),
    ["Tolglow group stage", "Tolglow run b", "Tolglow group warm", "Tolglow run a", "Tolglow run a · 2", "Bass"],
    "never after a nested group's members, where membership would be ambiguous");
  assert.equal(a2.result["group-track-id"], "track:" + warm.id);
  assert.deepEqual(live.request("ensure-track", { name: "Tolglow run b", kind: "midi" }).result,
    { "track-id": b.result["track-id"], "created?": false, "group-track-id": "track:" + stage.id }, "found again by name");
  const notGroup = live.request("ensure-track", { name: "Tolglow run c", kind: "midi", "group-track-id": "track:" + live.song.tracks[5].id });
  assert.equal(notGroup.status, "rejected"); assert.equal(notGroup.error.code, "not-a-group");
});

test("ensure-track reports a track that did not join its group instead of claiming membership", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow group stage", group: true }], joinsGroup: false });
  const r = live.request("ensure-track", { name: "Tolglow run b", kind: "midi", "group-track-id": "track:" + live.song.tracks[0].id });
  assert.equal(r.status, "executed");
  assert.equal(r.result["group-track-id"], null);
});

test("write-clip adds a scene when the run track is full and places the clip in the Arrangement", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a" }], slots: 1 });
  const track = "track:" + live.song.tracks[0].id;
  const clip = { "length-beats": 3, notes: [{ pitch: 60, start: 0, duration: 3, velocity: 127 }] };
  const first = live.request("write-clip", { "track-id": track, "expected-name": "Tolglow run a", name: "A run 1 [r.1]", clip, "create-scene?": true });
  assert.equal(first.result["scene-created?"], false);
  const refused = live.request("write-clip", { "track-id": track, name: "A run 2 [r.2]", clip });
  assert.equal(refused.status, "rejected"); assert.equal(refused.error.code, "no-empty-slot");
  const second = live.request("write-clip", { "track-id": track, name: "A run 2 [r.2]", clip, "create-scene?": true, "looping?": false });
  assert.equal(second.status, "executed", JSON.stringify(second.error));
  assert.deepEqual([second.result["slot-index"], second.result["scene-created?"]], [1, true]);
  assert.equal(live.song.tracks[0].slots[1].clip.props.name, "A run 2 [r.2]");
  assert.equal(live.song.tracks[0].slots[1].clip.props.looping, 0, "run clips do not loop");
  assert.equal(live.song.tracks[0].slots[0].clip.props.looping, undefined, "without looping? Live's default stays");
  const placed = live.request("place-clip", { "track-id": track, "expected-name": "Tolglow run a", "clip-id": second.result["clip-id"], "arrangement-beat": 16 });
  assert.deepEqual(placed.result, { "arrangement-beat": 16 });
  const overlap = live.request("place-clip", { "track-id": track, "clip-id": first.result["clip-id"], "arrangement-beat": 18 });
  assert.equal(overlap.status, "rejected", "an Arrangement overlap is refused before the copy");
  assert.equal(overlap.error.code, "placement-overlap");
  const inspected = live.request("inspect-set").result.tracks[0];
  assert.deepEqual(inspected["arrangement-clips"], [{ name: "A run 2 [r.2]", "start-beat": 16, "end-beat": 19 }]);
});

test("forwarded MIDI is live input only while the companion's track plays, launches and routes in nothing", () => {
  let clock = 1000;
  const live = liveSet({ tracks: [{ name: "Tolglow Cues" }], now: () => clock });
  live.request("handoff", { mode: "lighting" });
  const note = () => live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }).origin;
  live.request("inspect-set");
  assert.equal(note(), "live-input", "no clip playing or launched, no Arrangement clips: a performer");
  live.fire(0, 0);
  assert.equal(note(), "live-playback", "a launched clip waiting for its quantized start");
  live.play(0, 0);
  assert.equal(note(), "live-playback", "a Session clip plays on the companion's track");
  clock += 1000; live.play(0, -1);
  assert.equal(note(), "live-playback", "notes just after a stop may still be the clip's");
  clock += 300;
  assert.equal(note(), "live-input", "after the transition window: a performer again");
  live.session.closeWatches();
  assert.ok(live.observers.every(o => !o.open), "closing the device ends every watch");

  const arranged = liveSet({ tracks: [{ name: "Tolglow Cues" }], now: () => clock });
  arranged.request("handoff", { mode: "lighting" });
  arranged.song.tracks[0].props.arrangement_clips = ["id", 9999];
  arranged.session.hostTrackId(true);
  assert.equal(arranged.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }).origin, "live-playback",
    "a track with Arrangement clips counts as playback even while stopped, so Play's first downbeat is never a performance");

  const routed = liveSet({ tracks: [{ name: "Tolglow Cues" }, { name: "Keystep" }], now: () => clock });
  routed.request("handoff", { mode: "lighting" });
  routed.song.tracks[0].props.input_routing_type = PORTS[2];   // display name "Keystep", like another track
  routed.session.hostTrackId(true);
  assert.equal(routed.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }).origin, "live-playback",
    "MIDI From another track carries that track's clips");

  const sent = liveSet({ tracks: [{ name: "Tolglow Cues" }, { name: "Launcher", output: { display_name: "Tolglow Cues", identifier: 901 } }],
    now: () => clock });
  sent.request("handoff", { mode: "lighting" });
  sent.session.hostTrackId(true);
  assert.equal(sent.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }).origin, "live-playback",
    "another track's MIDI To the companion's track carries that track's clips");

  const grouped = liveSet({ tracks: [{ name: "Tolglow Cues" }, { name: "Tolglow group stage", group: true }, { name: "Bass" }], now: () => clock });
  grouped.request("handoff", { mode: "lighting" });
  grouped.session.hostTrackId(true);
  assert.equal(grouped.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }).origin, "live-input",
    "a Group Track or a track sending to the Master does not route into the companion");
});

test("with notifications delivered later, as LiveAPI does, the value is kept inside the notification", async () => {
  let clock = 1000;
  const live = liveSet({ tracks: [{ name: "Tolglow Cues" }, { name: "Tolglow run a" }], notify: "deferred", now: () => clock });
  live.request("handoff", { mode: "lighting" });
  live.request("inspect-set");
  const note = () => live.adapter.midi({ type: "note-on", note: 36, velocity: 127, channel: 1 }).origin;
  clock += 1000;
  assert.equal(note(), "live-input");
  live.fire(0, 0);
  live.play(0, 0);
  await new Promise(resolve => setImmediate(resolve));   // notifications delivered; their deferred tasks not yet run
  assert.equal(note(), "live-playback", "classified from the notification itself, before any deferred task");
  // Run-track watch: the clip identity is read in the deferred task.
  const clip = { "length-beats": 2, notes: [{ pitch: 60, start: 0, duration: 2, velocity: 127 }] };
  const written = live.request("write-clip", { "track-id": "track:" + live.song.tracks[1].id, name: "A run 1 [r.1]", clip });
  live.request("watch-run-tracks", { "track-ids": ["track:" + live.song.tracks[1].id] });
  live.play(1, 0, 8);
  assert.equal(live.emitted.length, 0, "nothing is emitted inside the call");
  await new Promise(resolve => setTimeout(resolve, 5));
  assert.deepEqual(live.emitted.map(e => [e.type, e["clip-id"]]), [["clip-started", written.result["clip-id"]]]);
});

test("watch-run-tracks reports each clip start and stop once, with the clip identity", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a" }, { name: "Tolglow run b" }] });
  const [a, b] = live.song.tracks;
  const clip = { "length-beats": 2, notes: [{ pitch: 60, start: 0, duration: 2, velocity: 127 }] };
  const written = live.request("write-clip", { "track-id": "track:" + a.id, name: "A run 1 [r.1]", clip });
  const writes = live.writes.length;
  const r = live.request("watch-run-tracks", { "track-ids": ["track:" + a.id, "track:" + b.id] });
  assert.deepEqual(r.result, { watching: 2 });
  assert.equal(live.writes.length, writes, "watching writes nothing");
  live.play(0, 0, 32); live.play(0, 0, 32); live.play(0, -2, 34);
  assert.deepEqual(live.emitted, [
    { type: "clip-started", "track-id": "track:" + a.id, "watch-session": live.session.watchSession, serial: 1, "slot-index": 0,
      "clip-id": written.result["clip-id"], "song-beat": 32 },
    { type: "clip-stopped", "track-id": "track:" + a.id, "watch-session": live.session.watchSession, serial: 2, "song-beat": 34 }]);
  live.request("watch-run-tracks", { "track-ids": ["track:" + b.id] });
  live.play(0, 0);
  assert.equal(live.emitted.length, 2, "a replaced watch no longer reports");
  assert.equal(live.request("watch-run-tracks", { "track-ids": ["track:999"] }).error.code, "stale-track");
});

// Cue launcher (#228): a Drum Rack after the companion on its own track,
// one named and colored chain per page pad.
test("ensure-launcher adopts the companion's empty track, inserts a Drum Rack after it and scrolls the pads to note 36", () => {
  const live = liveSet({ tracks: [{ name: "Bass", arm: true }, { name: "MIDI" }], host: 1, launcher: true });
  const r = live.request("ensure-launcher", { name: "Tolglow launcher page-1", "scroll-row": 9 });
  assert.equal(r.status, "executed");
  assert.deepEqual(r.result, { "track-id": "track:" + live.song.tracks[1].id, "device-index": 1, "rack-created?": true, "renamed?": true });
  assert.deepEqual(live.calls.filter(c => c.method === "insert_device").map(c => c.args), [["Drum Rack", 1]], "right after the companion");
  assert.equal(live.song.tracks[1].props.name, "Tolglow launcher page-1");
  assert.equal(live.song.tracks[0].props.name, "Bass", "the musician's track is untouched");
  const again = live.request("ensure-launcher", { name: "Tolglow launcher page-1", "scroll-row": 9 });
  assert.deepEqual(again.result, { "track-id": "track:" + live.song.tracks[1].id, "device-index": 1 }, "a rerun writes nothing");
  assert.equal(live.calls.filter(c => c.method === "insert_device").length, 1);
  live.rackOf(1).view.props.drum_pads_scroll_position = 3;
  live.request("ensure-launcher", { name: "Tolglow launcher page-1", "scroll-row": 9 });
  assert.equal(live.rackOf(1).view.props.drum_pads_scroll_position, 9, "the pad overview is scrolled back for Push's 64 Pads layout");
});

test("write-pads names and colors existing chains, inserts chains for empty pads and reads them back", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow launcher page-1", drumRack: { 36: "Kick" } }], host: 0, launcher: true });
  const track = "track:" + live.song.tracks[0].id;
  const r = live.request("write-pads", { "track-id": track, "expected-name": "Tolglow launcher page-1",
    pads: [{ note: 36, name: "Ember", color: 0xff2200 }, { note: 99, name: "Moon", color: 0xeeeeee }] });
  assert.equal(r.status, "executed");
  assert.deepEqual(r.result.pads, [{ note: 36, name: "Ember", color: 0xff2200, "chain-created?": false },
    { note: 99, name: "Moon", color: 0xeeeeee, "chain-created?": true }]);
  const rack = live.rackOf(0);
  assert.deepEqual(rack.chains.map(c => [c.props.in_note, c.props.name, c.props.is_auto_colored]), [[36, "Ember", 0], [99, "Moon", 0]]);
  assert.ok(live.writes.every(w => live.entities[w.id].kind === "chain"), "only chains are written: no DrumPad setter");
  const inspected = live.request("inspect-launcher").result;
  assert.deepEqual(inspected.rack.pads, [{ note: 36, name: "Ember", color: 0xff2200, chains: 1 }, { note: 99, name: "Moon", color: 0xeeeeee, chains: 1 }]);
  assert.equal(inspected.rack["scroll-row"], 9);
  const writes = live.writes.length;
  live.request("write-pads", { "track-id": track, pads: [{ note: 36, name: "Ember", color: 0xff2200 }] });
  assert.equal(live.writes.length, writes, "an unchanged pad is not written again");
  assert.equal(live.request("write-pads", { "track-id": track, "expected-name": "Lights", pads: [] }).error.code, "track-renamed");
});

test("an older Live uses a placed Drum Rack, and a musician's track is refused before any write", () => {
  const old = liveSet({ tracks: [{ name: "MIDI" }], host: 0, launcher: true, version: [12, 2, 5] });
  const refused = old.request("ensure-launcher", { name: "Tolglow launcher page-1" });
  assert.equal(refused.status, "rejected");
  assert.equal(refused.error.code, "drum-rack-creation-unavailable");
  assert.deepEqual(old.writes, []); assert.deepEqual(old.calls.filter(c => c.method === "insert_device"), []);
  const placed = liveSet({ tracks: [{ name: "Drums", drumRack: {} }], host: 0, launcher: true, version: [12, 2, 5] });
  assert.equal(placed.request("ensure-launcher", { name: "Tolglow launcher page-1" }).status, "executed", "a performer's Drum Rack is used");
  const skipped = placed.request("write-pads", { pads: [{ note: 36, name: "Ember", color: 0xff2200 }] }).result.skipped;
  assert.deepEqual(skipped, [{ note: 36, code: "chain-creation-unavailable" }]);
  const busy = liveSet({ tracks: [{ name: "Keys", clip: true }], host: 0, launcher: true });
  assert.equal(busy.request("ensure-launcher", { name: "Tolglow launcher page-1" }).error.code, "host-track-in-use");
  assert.deepEqual(busy.writes, []);
  assert.equal(liveSet({ tracks: [{ name: "MIDI" }], host: 0, launcher: true }).request("ensure-launcher", { name: "Launcher" }).error.code,
    "track-name", "launcher tracks are Tolglow's by name");
});

test("pad hits on the launcher track are live input until a clip plays there, then playback", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow launcher page-1", drumRack: {} }], host: 0, launcher: true });
  live.request("inspect-launcher");
  live.request("handoff", { mode: "lighting" });
  const hit = { type: "note-on", note: 36, velocity: 100, channel: 1 };
  assert.equal(live.adapter.midi({ ...hit }).origin, "live-input", "a performer on the pads");
  live.play(0, 0);
  assert.equal(live.adapter.midi({ ...hit }).origin, "live-playback", "the recorded pad clip replaying");
});
