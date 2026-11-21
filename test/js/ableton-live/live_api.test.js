"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { Adapter } = require("../../../resources/devices/ableton-live/live_api.js");

const { fixture } = require("./behavioral_host.js");

test("inspection separates absolute transport beat from clip position", () => {
  const f = fixture(); const response = f.pair(); assert.equal(response.status, "executed");
  assert.equal(response.result.beat, 2); assert.equal(response.result["song-beat"], 10); assert.equal(response.result.clock.beat, 10);
  assert.equal(response.result["looping?"], true); assert.equal(response.result["automation-armed?"], true);
  f.song.props.signature_numerator = 7; f.song.props.signature_denominator = 8;
  assert.equal(f.request("inspect").result.clock.meter["beats-per-bar"], 3.5);
  assert.deepEqual(f.writes, []);
});

test("pairing and every write reject replaced IDs or changed names", () => {
  const f = fixture(); assert.equal(f.pair({ "expected-track-name": "Wrong" }).status, "rejected");
  assert.equal(f.pair({ "expected-track-name": "Lights", "expected-clip-name": "Light sequence" }).status, "executed");
  f.clip.id = 999;
  assert.equal(f.request("record-event", f.intent("note-on", "launch", 2, 10)).error.code, "stale-destination");
  f.clip.id = 3; f.clip.props.name = "Replaced clip";
  assert.equal(f.request("record-event", f.intent("note-on", "launch", 2, 10)).error.code, "destination-name-mismatch");
  assert.deepEqual(f.methods, []);
});

test("recording writes actual note IDs and extends the exact instance across loop wraps", () => {
  const f = fixture(); f.pair();
  const on = f.request("record-event", f.intent("note-on", "launch", 3.8, 10));
  assert.equal(on.status, "executed"); assert.equal(on.result["recorded?"], true); assert.equal(on.result["provisional?"], true);
  const noteId = on.result["note-id"];
  assert.equal(f.notes.get(noteId).start_time, 3.8); assert.equal(f.notes.get(noteId).duration, 1 / 16);
  const off = f.request("record-event", f.intent("note-off", "end", 0.2, 14.4));
  assert.equal(off.status, "executed"); assert.equal(off.result["note-id"], noteId);
  assert.ok(Math.abs(off.result["duration-beats"] - 4.4) < 1e-10);
  assert.equal(f.notes.get(noteId).probability, 1, "existing note properties survive modification");
  assert.deepEqual(f.methods.map(x => x.method), ["add_new_notes", "get_notes_by_id", "apply_note_modifications"]);
});

test("duplicate accepted command is never inserted twice, even after recording stops", () => {
  const f = fixture(); f.pair(); const intent = f.intent("note-on", "launch", 2, 10);
  const first = f.request("record-event", intent); f.track.props.arm = 0;
  const duplicate = f.request("record-event", { ...intent });
  assert.deepEqual(duplicate.result, first.result); assert.equal(f.notes.size, 1);
  assert.equal(f.request("record-event", { ...intent, beat: 3 }).error.code, "command-id-conflict");
});

test("uncertain Live write is remembered and cannot be automatically retried", () => {
  const f = fixture(); f.pair(); const original = f.clip.call;
  f.clip.call = function (method, argument) { const result = original.call(this, method, argument); if (method === "add_new_notes") throw new Error("Host disconnected after write"); return result; };
  const intent = f.intent("note-on", "launch", 2, 10);
  assert.equal(f.request("record-event", intent).status, "indeterminate");
  assert.equal(f.request("record-event", intent).error.code, "prior-write-indeterminate");
  assert.equal(f.notes.size, 1);
});

test("recording guards reject unarmed, stopped, missing and unsupported CC writes", () => {
  const f = fixture(); f.pair(); f.track.props.arm = 0;
  assert.equal(f.request("record-event", f.intent("note-on", "a", 1, 10)).error.code, "not-recording");
  f.track.props.arm = 1; f.clip.props.is_recording = 0;
  assert.equal(f.request("record-event", f.intent("note-on", "b", 1, 10)).error.code, "not-recording");
  f.clip.props.is_recording = 1;
  assert.equal(f.request("record-event", f.intent("note-off", "c", 1, 10)).error.code, "unpaired-note-off");
  assert.equal(f.request("record-event", f.intent("cc", "d", 1, 10)).error.code, "unsupported-cc-recording");
  assert.deepEqual(f.methods, []);
});

test("Drum Rack mirroring writes verified chain properties and reports empty pads", () => {
  const f = fixture(); f.pair();
  const result = f.request("pad-page", { pads: [
    { note: 36, "cue-id": "ember", label: "Ember", "live-color": 0xff2200 },
    { note: 37, "cue-id": "moon", label: "Moon", "live-color": 0xeeeeee }
  ], "clip-color": 0xff2200 });
  assert.equal(result.status, "executed"); assert.equal(f.chain.props.name, "Ember"); assert.equal(f.chain.props.color, 0xff2200);
  assert.equal(f.chain.props.is_auto_colored, 0); assert.equal(f.clip.props.color, 0xff2200);
  assert.deepEqual(result.result.skipped, [{ note: 37, reason: "empty-pad-no-chain" }]);
  assert.equal(result.result["color-policy"], "nearest-live-palette");
  assert.ok(f.writes.every(x => x.id === 20 || x.id === 3), "No invented DrumPad setters are used");
});

test("mode transfer and feedback origin suppress MIDI echo", () => {
  const f = fixture(); const event = { type: "note-on", note: 36, velocity: 127, channel: 1 };
  assert.equal(f.adapter.midi({ ...event }), null, "default is Live pass-through");
  assert.equal(f.request("handoff", { mode: "lighting" }).result["midi-thru?"], false);
  assert.equal(f.adapter.midi({ ...event }).origin, "live-playback");
  assert.equal(f.adapter.midi({ ...event, origin: "tolglow-feedback" }), null);
  assert.equal(f.request("handoff", { mode: "live" }).result["midi-thru?"], true);
  assert.equal(f.adapter.midi({ ...event }), null);
});

test("the editable Max patch routes every LiveAPI message through deferlow", () => {
  const patch = JSON.parse(fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live/tolglow-live-bridge.maxpat"), "utf8")).patcher;
  const boxes = new Map(patch.boxes.map(x => [x.box.id, x.box]));
  assert.ok(boxes.get("node").text.includes("node.script node_bridge.js"));
  assert.equal(boxes.get("low").text, "deferlow");
  const jsInputs = patch.lines.filter(x => x.patchline.destination[0] === "max-js");
  assert.equal(jsInputs.length, 1); assert.equal(jsInputs[0].patchline.source[0], "low");
  for (const { patchline } of patch.lines) { assert.ok(boxes.has(patchline.source[0])); assert.ok(boxes.has(patchline.destination[0])); }
  assert.ok(!fs.existsSync(path.join(__dirname, "../../../resources/devices/ableton-live/tolglow-live-bridge.amxd")), "No fabricated compiled device is shipped");
});

test("parameter samples remain editable through Session overdub without disturbing the cue note", () => {
  const f = fixture();
  f.pair();
  f.clip.props.is_recording = 0;
  f.clip.props.is_overdubbing = 1;
  f.song.props.session_automation_record = 0;
  const start = f.request("record-event", f.intent("note-on", "launch", 0, 4));
  const sample = f.request("record-event", f.intent("note-on", "sample", 0.5, 4.5, {
    event: {type: "note-on", note: 100, velocity: 64, channel: 1, "parameter-sample?": true}
  }));
  assert.equal(sample.result["pairing"], "parameter-lane");
  assert.equal(sample.result["provisional?"], false);
  assert.equal(f.notes.get(sample.result["note-id"]).velocity, 64);
  f.request("record-event", f.intent("note-off", "release", 1.5, 5.5));
  assert.equal(f.notes.get(start.result["note-id"]).duration, 1.5);
  assert.equal(f.notes.get(sample.result["note-id"]).duration, 1/16);
});

test("places a finished clip into empty Arrangement time and validates its recording range", () => {
  const f = fixture();
  f.pair();
  const placement = {"destination-id": "clip:3", "song-beat": 32};
  assert.equal(f.request("place-in-arrangement", placement).error.code, "recording-active");
  f.clip.props.is_recording = 0;
  assert.equal(f.request("place-in-arrangement", placement).result["song-beat"], 32);
  assert.equal(f.request("place-in-arrangement", placement).error.code, "placement-overlap");
  const pair = {"track-index": 0, "arrangement-index": 0};
  assert.equal(f.request("configure-destination", {...pair, "clip-slot-index": 0}).error.code, "destination");
  const placed = f.request("configure-destination", pair);
  assert.equal(placed.result.destination["clip-id"], 200);
  f.song.props.record_mode = 1;
  assert.equal(f.adapter.inspect()["recording?"], false);
  f.song.props.current_song_time = 34;
  assert.equal(f.adapter.inspect()["recording?"], true);
  f.song.props.current_song_time = 36;
  assert.equal(f.adapter.inspect()["recording?"], false);
});
