"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { Adapter } = require("../live_api.js");

function fixture() {
  const writes = [], methods = [], entities = {}, paths = {}, notes = new Map(); let nextNote = 100;
  function object(id, props, canonicalPath) {
    const value = { id, props,
      get(key) { const value = this.props[key]; return Array.isArray(value) ? value : [value]; },
      set(key, value) { writes.push({ id: this.id, key, value }); this.props[key] = value; },
      call(method, argument) {
        methods.push({ method, argument });
        if (method === "add_new_notes") {
          return JSON.parse(argument).notes.map(note => { const id = ++nextNote; notes.set(id, { ...note, note_id: id, probability: 1 }); return id; });
        }
        if (method === "get_notes_by_id") return JSON.stringify({ notes: notes.has(argument) ? [{ ...notes.get(argument) }] : [] });
        if (method === "apply_note_modifications") { for (const note of JSON.parse(argument).notes) notes.set(note.note_id, { ...note }); return null; }
        throw new Error("Unexpected Live method " + method);
      }
    };
    entities[id] = value; if (canonicalPath) paths[canonicalPath] = value; return value;
  }
  const song = object(1, { current_song_time: 10, tempo: 120, is_playing: 1, signature_numerator: 4, signature_denominator: 4, session_automation_record: 1 }, "live_set");
  const track = object(2, { arm: 1, name: "Lights" }, "live_set tracks 0");
  const clip = object(3, { is_midi_clip: 1, is_recording: 1, playing_position: 2, loop_start: 0, loop_end: 4, looping: 1, name: "Light sequence", color: 0 }, "live_set tracks 0 clip_slots 0 clip");
  object(4, { has_drum_pads: 1, drum_pads: ["id", 10, "id", 11] }, "live_set tracks 0 devices 1");
  object(10, { note: 36, name: "Old pad", chains: ["id", 20] }); object(11, { note: 37, name: "Empty", chains: [] });
  const chain = object(20, { name: "Old chain", color: 0, is_auto_colored: 1 });
  const api = key => (typeof key === "object" ? entities[key.id] : paths[key]) || { id: 0 };
  const adapter = new Adapter({ api, dictionaryCall: (object, method, data) => object.call(method, JSON.stringify(data)), readDictionary: JSON.parse });
  function request(op, payload, id = "request-" + Math.random()) { return adapter.handle({ id, op, payload }); }
  function pair(extra = {}) { return request("configure-destination", { "track-index": 0, "clip-slot-index": 0, "rack-device-index": 1, ...extra }); }
  function intent(type, id, beat, songBeat, extra = {}) { return { "protocol-version": 1, "destination-id": "clip:3", epoch: 2, "command-id": id,
    "instance-id": "2:7", beat, "song-beat": songBeat, event: { type, note: 36, velocity: type === "note-on" ? 100 : 0, channel: 1, origin: "tolglow-feedback" }, ...extra }; }
  return { adapter, request, pair, intent, writes, methods, notes, song, track, clip, chain, entities, paths };
}

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
  const patch = JSON.parse(fs.readFileSync(path.join(__dirname, "../tolglow-live-bridge.maxpat"), "utf8")).patcher;
  const boxes = new Map(patch.boxes.map(x => [x.box.id, x.box]));
  assert.ok(boxes.get("node").text.includes("node.script node_bridge.js"));
  assert.equal(boxes.get("low").text, "deferlow");
  const jsInputs = patch.lines.filter(x => x.patchline.destination[0] === "max-js");
  assert.equal(jsInputs.length, 1); assert.equal(jsInputs[0].patchline.source[0], "low");
  for (const { patchline } of patch.lines) { assert.ok(boxes.has(patchline.source[0])); assert.ok(boxes.has(patchline.destination[0])); }
  assert.ok(!fs.existsSync(path.join(__dirname, "../tolglow-live-bridge.amxd")), "No fabricated compiled device is shipped");
});
