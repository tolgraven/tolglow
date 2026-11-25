"use strict";
// A behavioral stand-in only for the Live object graph. The HTTP transport,
// clip adapter, feedback suppression and MIDI translation are production code.
const { Adapter } = require("../../../resources/devices/ableton-live/live_api.js");
const { createBridge } = require("../../../resources/devices/ableton-live/node_bridge.js");
function fixture() {
  const writes = [], methods = [], entities = {}, paths = {}; let nextNote = 100;
  function object(id, props, canonicalPath) {
    const value = { id, path: canonicalPath, props, notes: new Map(), listeners: new Set(),
      get(key) { const value = this.props[key]; return Array.isArray(value) ? value : [value]; },
      set(key, value) { writes.push({ id: this.id, key, value }); this.props[key] = value;
        for (const listener of this.listeners) listener(key, value); },
      call(method, argument, beat) {
        methods.push({ method, argument });
        if (method === "duplicate_clip_to_arrangement") {
          const clipId = Number(/^id (\d+)$/.exec(argument)?.[1]);
          if (!entities[clipId] || typeof beat !== "number") throw Error("Invalid clip reference");
          const source = entities[clipId];
          const placed = object(200 + this.props.arrangement_clips.length,
            {...source.props, start_time: beat, end_time: beat + source.props.length, is_recording: 0},
            "live_set tracks 0 arrangement_clips " + this.props.arrangement_clips.length / 2);
          for (const n of source.notes.values()) {
            const note_id = ++nextNote;
            placed.notes.set(note_id, {...n, note_id});
          }
          this.props.arrangement_clips.push("id", placed.id);
          return;
        }
        if (method === "add_new_notes") {
          return argument.notes.map(note => { const id = ++nextNote; this.notes.set(id, { ...note, note_id: id, probability: 1 }); return id; });
        }
        if (method === "get_notes_by_id") return JSON.stringify({ notes: this.notes.has(argument) ? [{ ...this.notes.get(argument) }] : [] });
        if (method === "apply_note_modifications") { for (const note of argument.notes) this.notes.set(note.note_id, { ...note }); return null; }
        throw new Error("Unexpected Live method " + method);
      }
    };
    entities[id] = value; if (canonicalPath) paths[canonicalPath] = value; return value;
  }
  const song = object(1, { current_song_time: 10, tempo: 120, is_playing: 1, signature_numerator: 4, signature_denominator: 4, session_automation_record: 1 }, "live_set");
  const track = object(2, { arm: 1, name: "Lights", arrangement_clips: [] }, "live_set tracks 0");
  const clip = object(3, { is_midi_clip: 1, length: 4, is_playing: 1, is_recording: 1, playing_position: 2, loop_start: 0, loop_end: 4, looping: 1, name: "Light sequence", color: 0 }, "live_set tracks 0 clip_slots 0 clip");
  object(5, { ...clip.props, name: "Other performance" }, "live_set tracks 0 clip_slots 1 clip");
  object(4, { has_drum_pads: 1, drum_pads: ["id", 10, "id", 11] }, "live_set tracks 0 devices 1");
  object(10, { note: 36, name: "Old pad", chains: ["id", 20] }); object(11, { note: 37, name: "Empty", chains: [] });
  const chain = object(20, { name: "Old chain", color: 0, is_auto_colored: 1 });
  const synthPath = "live_set tracks 0 devices 2 chains 0 devices 0";
  object(30, {name: "Filter"}, synthPath);
  const cutoff = object(31, {name: "Cutoff", min: 20, max: 20000, is_quantized: 0, value: 120, automation_state: 1},
                        synthPath + " parameters 3");
  paths["live_set view selected_parameter"] = cutoff;
  const api = key => (typeof key === "object" ? entities[key.id] : paths[key]) || { id: 0 };
  const adapter = new Adapter({ api, dictionaryCall: (object, method, data) => object.call(method, data), readDictionary: JSON.parse });
  function request(op, payload, id = "request-" + Math.random()) { return adapter.handle({ id, op, payload }); }
  function pair(extra = {}) { return request("configure-destination", { "track-index": 0, "clip-slot-index": 0, "rack-device-index": 1, ...extra }); }
  function intent(type, id, beat, songBeat, extra = {}) { return { "protocol-version": 1, "destination-id": "clip:3", epoch: 2, "command-id": id,
    "instance-id": "2:7", beat, "song-beat": songBeat, event: { type, note: 36, velocity: type === "note-on" ? 100 : 0, channel: 1, origin: "tolglow-feedback" }, ...extra }; }
  return { api, adapter, cutoff, request, pair, intent, writes, methods, notes: clip.notes, song, track, clip, chain, entities, paths };
}

module.exports = { fixture };
if (require.main === module) {
  (async () => {
    const host = fixture();
    const fs = require("node:fs"), vm = require("node:vm"), path = require("node:path");
    let callback;
    const max = vm.createContext({
      include() {}, TolglowLive: require("../../../resources/devices/ableton-live/live_api"),
      TolglowParameters: require("../../../resources/devices/ableton-live/parameter_observers"),
      Task: function (fn) {
        let timer;
        return {schedule(delay) {timer = setTimeout(fn, delay);}, cancel() {clearTimeout(timer);}};
      },
      LiveAPI: function (notify, path) {
        let target = host.api(path || ""), watched, mode = 0;
        const current = () => mode === 1 ? host.api(path) : target;
        const listener = (k, v) => {if (notify && k === watched) notify([k, v]);};
        return {
          get id() {return current().id;}, set id(id) {target.listeners?.delete(listener); target = host.api({id});},
          get path() {return current().path;}, set mode(v) {mode = v;},
          set property(k) {
            target.listeners?.delete(listener); watched = k;
            if (k && notify) target.listeners.add(listener);
          },
          get(k) {return current().get(k);}, set(k, v) {current().set(k, v);},
          call(...args) {return current().call(...args);}
        };
      },
      outlet(port, kind, json) {
        if (port === 0 && kind === "response") bridge.receive(json);
        if (port === 0 && kind === "event") callback = bridge.forward(JSON.parse(json));
      }
    });
    vm.runInContext(fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live/max_adapter.js"), "utf8"), max);
    const bridge = createBridge({sendToMax: (_, json) => max.request(json)});
    const {port} = await bridge.start({port: 0, callback: process.argv[2]});
    const write = result => process.stdout.write(JSON.stringify(result) + "\n");
    write({port});
    for await (const line of require("node:readline").createInterface({input: process.stdin})) {
      const op = JSON.parse(line);
      if (op.stop) break;
      const clip = op.arrangement ? host.paths["live_set tracks 0 arrangement_clips 0"] :
        host.paths[`live_set tracks 0 clip_slots ${op.slot ?? 0} clip`];
      if (op.setParameter !== undefined) {
        host.cutoff.set("value", op.setParameter);
        write({listeners: host.cutoff.listeners.size});
      }
      if (op.listeners) write({listeners: host.cutoff.listeners.size});
      if (op.notes) write(clip ? [...clip.notes.values()] : []);
      if (op.finish) {
        host.clip.props.is_recording = 0;
        max.poll();
        await callback;
        write({finished: true});
      }
      const findNote = () => [...clip.notes.values()].find(n => n.pitch === op.pitch &&
        (op.atBeat === undefined || n.start_time === op.atBeat));
      if (op.edit) {
        const note = findNote();
        note.velocity = op.velocity;
        note.start_time = op.beat;
        write({edited: true});
      }
      if (op.play) {
        const note = findNote();
        max.note(note.pitch, op.release ? 0 : note.velocity, 1);
        write(await callback);
      }
    }
    await bridge.stop();
  })().catch(e => { process.stderr.write(String(e.stack) + "\n"); process.exitCode = 1; });
}
