"use strict";
// A behavioral stand-in for the Live object graph used by the companion's
// setup, Capture and clip operations. Only the Live objects are simulated;
// Adapter and Session are production code.
const { Adapter } = require("../../../resources/devices/ableton-live/live_api.js");
const { Session, COARSE } = require("../../../resources/devices/ableton-live/live_session.js");

const PORTS = [{ display_name: "All Ins", identifier: 1 }, { display_name: "Tolglow", identifier: 2 }, { display_name: "Keystep", identifier: 3 }];
const CHANNELS = [{ display_name: "All Channels", identifier: 100 }].concat(
  Array.from({ length: 16 }, (_, i) => ({ display_name: "Ch. " + (i + 1), identifier: 101 + i })));

// Group Tracks: { name, group: true } is a Group Track and { parent: name }
// places a track inside one. create_midi_track(index) joins the group of the
// track before it (or that track, if it is a Group Track): an assumption about
// Live to verify on a real Set (joinsGroup: false models the opposite).
// notify: "sync" delivers observer notifications inside the call that changes
// the property; "deferred" delivers each one later (a timer, as LiveAPI does),
// with the observer's deferred task later still, as max_adapter.js schedules
// it. now: the Session's millisecond clock.
function liveSet({ tracks = [], host = 0, canCapture = true, slots = 4, joinsGroup = true, notify = "sync", now } = {}) {
  const entities = {}, writes = [], calls = [], observers = [], emitted = []; let next = 10;
  const add = (o) => { o.id = o.id || ++next; entities[o.id] = o; return o; };
  function makeClip(length) {
    return add({ kind: "clip", props: { name: "", length, is_midi_clip: 1 }, notes: [], envelopes: {}, fired: 0 });
  }
  function makeTrack(t) {
    const track = add({ kind: "track", props: { name: t.name, has_midi_input: t.audio || t.group ? 0 : 1, can_be_armed: t.group ? 0 : 1, arm: t.arm ? 1 : 0,
      output_routing_type: t.output ?? { display_name: "Master", identifier: 900 },
      is_foldable: t.group ? 1 : 0, group_track: ["id", 0], playing_slot_index: -1, fired_slot_index: -1,
      current_monitoring_state: t.monitor ?? 1, input_routing_type: t.port ?? PORTS[0], input_routing_channel: t.channel ?? CHANNELS[0],
      devices: t.instrument ? ["id", 7001, "id", 7002] : ["id", 7001], arrangement_clips: [] },
    slots: [], received: !!t.received });
    for (let i = 0; i < slots; i++) track.slots.push(add({ kind: "slot", clip: null }));
    if (t.clip) track.slots[0].clip = makeClip(4);
    return track;
  }
  const song = add({ id: 1, kind: "song", props: { tempo: 120, signature_numerator: 4, signature_denominator: 4, can_capture_midi: canCapture ? 1 : 0,
    current_song_time: 0, is_playing: 0 },
    tracks: tracks.map(makeTrack) });
  tracks.forEach((t, i) => {
    if (t.parent) song.tracks[i].props.group_track = ["id", song.tracks.find(g => g.props.name === t.parent).id];
  });
  const groupOf = (track) => track.props.group_track[1] || null;
  const inGroup = (track, groupId) => { for (let g = groupOf(track); g; g = groupOf(entities[g])) if (g === groupId) return true; return false; };
  const view = add({ kind: "view", props: {} }), appView = add({ kind: "app-view", props: {} });
  const app = add({ kind: "app", props: {} });
  const device = add({ kind: "device", params: COARSE.map((_, i) => add({ kind: "param", props: { name: "Tolglow lane " + (i + 1) } })) });
  let hostIndex = host;
  const hostTrack = () => song.tracks[hostIndex];
  const moveCompanion = (index) => { hostIndex = index; };

  function proxy(o) {
    if (!o) return { id: 0 };
    return {
      id: o.id,
      get(key) {
        if (o.kind === "song" && key === "tracks") return o.tracks.flatMap(t => ["id", t.id]);
        if (o.kind === "track" && o.props.is_foldable && /monitoring|routing|^arm$/.test(key))
          throw new Error("Group Tracks have no " + key);
        if (o.kind === "track" && key === "clip_slots") return o.slots.flatMap(s => ["id", s.id]);
        if (o.kind === "slot" && key === "has_clip") return [o.clip ? 1 : 0];
        if (o.kind === "device" && key === "parameters") return o.params.flatMap(p => ["id", p.id]);
        if (key === "input_routing_type" || key === "input_routing_channel" || key === "output_routing_type") return JSON.stringify({ [key]: o.props[key] });
        if (key === "available_input_routing_types") return JSON.stringify({ [key]: PORTS });
        if (key === "available_input_routing_channels") return JSON.stringify({ [key]: CHANNELS });
        return Array.isArray(o.props[key]) ? o.props[key] : [o.props[key]];
      },
      set(key, value) { writes.push({ id: o.id, key, value }); o.props[key] = value; },
      call(method, ...args) {
        calls.push({ id: o.id, method, args });
        if (o.kind === "song" && method === "create_midi_track") {
          const index = args[0] === -1 || args[0] === undefined ? o.tracks.length : args[0];
          const track = makeTrack({ name: "MIDI" }), before = o.tracks[index - 1];
          if (joinsGroup && before) track.props.group_track = ["id", before.props.is_foldable ? before.id : groupOf(before) || 0];
          o.tracks.splice(index, 0, track);
          return null;
        }
        if (o.kind === "song" && method === "create_scene") { for (const t of o.tracks) t.slots.push(add({ kind: "slot", clip: null })); return null; }
        if (o.kind === "track" && method === "duplicate_clip_to_arrangement") {
          const source = entities[Number(/^id (\d+)$/.exec(args[0])[1])];
          const placed = add({ kind: "clip", props: { name: source.props.name, start_time: args[1], end_time: args[1] + source.props.length, length: source.props.length, is_midi_clip: 1 } });
          o.props.arrangement_clips = o.props.arrangement_clips.concat(["id", placed.id]);
          return null;
        }
        if (o.kind === "song" && method === "capture_midi") {
          for (const t of o.tracks) if (t.received && (t.props.arm || t.props.current_monitoring_state === 0)) {
            const slot = t.slots.find(s => !s.clip); slot.clip = makeClip(8); slot.clip.props.name = "Captured";
          }
          return null;
        }
        if (o.kind === "slot" && method === "create_clip") { o.clip = makeClip(args[0]); return null; }
        if (o.kind === "clip" && method === "add_new_notes") { o.notes.push(...args[0].notes.map(n => ({ ...n, note_id: ++next }))); return null; }
        if (o.kind === "clip" && method === "get_all_notes_extended") return JSON.stringify({ notes: o.notes });
        if (o.kind === "clip" && (method === "automation_envelope" || method === "create_automation_envelope")) {
          const param = Number(/^id (\d+)$/.exec(args[0])[1]);
          if (method === "create_automation_envelope") o.envelopes[param] = add({ kind: "envelope", steps: [] });
          return ["id", o.envelopes[param] ? o.envelopes[param].id : 0];
        }
        if (o.kind === "clip" && method === "fire") { o.fired++; return null; }
        if (o.kind === "envelope" && method === "insert_step") { o.steps.push({ time: args[0], duration: args[1], value: args[2] }); return null; }
        if (o.kind === "envelope" && method === "value_at_time") {
          // Assumed: an envelope holds its first value before its first step.
          const step = o.steps.filter(s => s.time <= args[0] + 1e-12).pop() || o.steps[0];
          return [step ? step.value : 0];
        }
        if (o.kind === "app" && method.startsWith("get_")) return [{ get_major_version: 12, get_minor_version: 4, get_bugfix_version: 5 }[method]];
        if (o.kind === "app-view" && method === "focus_view") { o.props.focused = args[0]; return null; }
        if (o.kind === "track" && method === "stop_all_clips") { o.props.stopped = true; return null; }
        throw new Error("Unexpected Live method " + method);
      }
    };
  }
  function resolve(p) {
    if (typeof p === "object") return entities[p.id];
    if (p === "live_set") return song;
    if (p === "live_set view") return view;
    if (p === "live_app") return app;
    if (p === "live_app view") return appView;
    if (p === "this_device") return device;
    if (p === "this_device canonical_parent") return hostTrack();
    const m = /^live_set tracks (\d+)(?: clip_slots (\d+)( clip)?)?$/.exec(p);
    if (!m) return undefined;
    const track = song.tracks[Number(m[1])];
    if (!track || m[2] === undefined) return track;
    const slot = track.slots[Number(m[2])];
    return m[3] ? slot && slot.clip : slot;
  }
  const api = (p) => proxy(resolve(p));
  const setDictionary = (object, key, value) => {
    const list = key === "input_routing_type" ? PORTS : CHANNELS;
    entities[object.id].props[key] = list.find(o => o.identifier === value.identifier);
    writes.push({ id: object.id, key, value });
  };
  // Like max_adapter.js: `immediate` runs inside the notification, `changed`
  // in a task scheduled from it.
  const observe = (id, property, changed, immediate) => {
    const entry = { id, property, changed, immediate, open: true };
    observers.push(entry);
    return () => { entry.open = false; };
  };
  const notifyAll = (id, property, value) => {
    const deliver = () => {
      for (const o of observers) if (o.open && o.id === id && o.property === property) {
        if (o.immediate) o.immediate([value]);
        if (o.changed) { if (notify === "deferred") setTimeout(() => { if (o.open) o.changed([value]); }, 0); else o.changed([value]); }
      }
    };
    if (notify === "deferred") setTimeout(deliver, 0); else deliver();
  };
  // Start (slot >= 0) or stop (-1) Session playback on a track, as Live would
  // report it through playing_slot_index.
  const play = (index, slot, songTime = 0) => {
    const track = song.tracks[index];
    track.props.playing_slot_index = slot; track.props.fired_slot_index = -1; song.props.current_song_time = songTime;
    notifyAll(track.id, "fired_slot_index", -1);
    notifyAll(track.id, "playing_slot_index", slot);
  };
  // Launch a clip that waits for its quantized start (fired_slot_index).
  const fire = (index, slot) => {
    const track = song.tracks[index];
    track.props.fired_slot_index = slot;
    notifyAll(track.id, "fired_slot_index", slot);
  };
  const session = new Session({ api, setDictionary, dictionaryCall: (o, m, d) => o.call(m, d), readDictionary: v => (typeof v === "string" ? JSON.parse(v) : v),
    observe, emit: (event) => emitted.push(event), now });
  const adapter = new Adapter({ api, session, dictionaryCall: (o, m, d) => o.call(m, d), readDictionary: JSON.parse });
  let n = 0;
  const request = (op, payload = {}) => adapter.handle({ id: "r" + (++n), op, payload });
  return { song, device, view, appView, entities, writes, calls, request, adapter, session, api, setDictionary, moveCompanion,
    observers, emitted, play, fire, inGroup };
}

module.exports = { liveSet, PORTS, CHANNELS };

// Run as a process: the production node bridge and max_adapter.js (in a vm,
// like Max's [js]) in front of a simulated Set with the companion on its only
// track. Prints {"port": n} and serves until stdin closes. Used by
// tolglow.integrations.live-companion-contract-test.
if (require.main === module) {
  (async () => {
    const fs = require("node:fs"), vm = require("node:vm"), path = require("node:path");
    const { createBridge } = require("../../../resources/devices/ableton-live/node_bridge.js");
    // argv[3] "groups" adds two placed Group Tracks for the run mirror (#683).
    const groups = process.argv[3] === "groups"
      ? [{ name: "Tolglow group stage", group: true }, { name: "Tolglow group warm", group: true, parent: "Tolglow group stage" }] : [];
    const live = liveSet({ tracks: [{ name: "Bass", arm: true, port: PORTS[2], channel: CHANNELS[1] }, { name: "MIDI" }].concat(groups), host: 1 });
    const max = vm.createContext({
      include() {}, TolglowLive: require("../../../resources/devices/ableton-live/live_api"),
      TolglowSession: require("../../../resources/devices/ableton-live/live_session"),
      TolglowParameters: require("../../../resources/devices/ableton-live/parameter_observers"),
      Task: function (fn) { let t; return { schedule(d) { t = setTimeout(fn, d); }, cancel() { clearTimeout(t); } }; },
      Dict: function () { throw new Error("Dict is not used by the simulated Set"); },
      LiveAPI: function (_notify, p) {
        let target = live.api(p === undefined || p === null ? "" : p);
        return { get id() { return target.id; }, set id(v) { target = live.api({ id: v }); },
          get(k) { return target.get(k); }, set(k, v) { return target.set(k, v); }, call(...a) { return target.call(...a); } };
      },
      outlet(port, kind, json) { if (port === 0 && kind === "response") bridge.receive(json); }
    });
    vm.runInContext(fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live/max_adapter.js"), "utf8"), max);
    // Max's dictionary-valued property setter is replaced by the simulated Set's.
    max.init();
    vm.runInContext("adapter.session.setDictionary = __setDictionary;", Object.assign(max, { __setDictionary: live.setDictionary }));
    const bridge = createBridge({ sendToMax: (_, json) => max.request(json) });
    const { port } = await bridge.start({ port: 0, callback: process.argv[2] || "http://127.0.0.1:9/api/live", id: "tolglow-m4l" });
    process.stdout.write(JSON.stringify({ port }) + "\n");
    process.stdin.on("data", () => {});
    await new Promise(resolve => process.stdin.on("end", resolve));
    await bridge.stop();
  })().catch(e => { process.stderr.write(String(e.stack) + "\n"); process.exitCode = 1; });
}
