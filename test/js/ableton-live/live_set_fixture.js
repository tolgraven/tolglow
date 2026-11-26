"use strict";
// A behavioral stand-in for the Live object graph used by the companion's
// setup, Capture and clip operations. Only the Live objects are simulated;
// Adapter and Session are production code.
const { Adapter } = require("../../../resources/devices/ableton-live/live_api.js");
const { Session, COARSE } = require("../../../resources/devices/ableton-live/live_session.js");

const PORTS = [{ display_name: "All Ins", identifier: 1 }, { display_name: "Tolglow", identifier: 2 }, { display_name: "Keystep", identifier: 3 }];
const CHANNELS = [{ display_name: "All Channels", identifier: 100 }].concat(
  Array.from({ length: 16 }, (_, i) => ({ display_name: "Ch. " + (i + 1), identifier: 101 + i })));

function liveSet({ tracks = [], host = 0, canCapture = true, slots = 4 } = {}) {
  const entities = {}, writes = [], calls = []; let next = 10;
  const add = (o) => { o.id = o.id || ++next; entities[o.id] = o; return o; };
  function makeClip(length) {
    return add({ kind: "clip", props: { name: "", length, is_midi_clip: 1 }, notes: [], envelopes: {}, fired: 0 });
  }
  function makeTrack(t) {
    const track = add({ kind: "track", props: { name: t.name, has_midi_input: t.audio ? 0 : 1, can_be_armed: 1, arm: t.arm ? 1 : 0,
      current_monitoring_state: t.monitor ?? 1, input_routing_type: t.port ?? PORTS[0], input_routing_channel: t.channel ?? CHANNELS[0] },
    slots: [], received: !!t.received });
    for (let i = 0; i < slots; i++) track.slots.push(add({ kind: "slot", clip: null }));
    return track;
  }
  const song = add({ id: 1, kind: "song", props: { tempo: 120, signature_numerator: 4, signature_denominator: 4, can_capture_midi: canCapture ? 1 : 0 },
    tracks: tracks.map(makeTrack) });
  const view = add({ kind: "view", props: {} }), appView = add({ kind: "app-view", props: {} });
  const app = add({ kind: "app", props: {} });
  const device = add({ kind: "device", params: COARSE.map((_, i) => add({ kind: "param", props: { name: "Tolglow lane " + (i + 1) } })) });
  const hostTrack = () => song.tracks[host];

  function proxy(o) {
    if (!o) return { id: 0 };
    return {
      id: o.id,
      get(key) {
        if (o.kind === "song" && key === "tracks") return o.tracks.flatMap(t => ["id", t.id]);
        if (o.kind === "track" && key === "clip_slots") return o.slots.flatMap(s => ["id", s.id]);
        if (o.kind === "slot" && key === "has_clip") return [o.clip ? 1 : 0];
        if (o.kind === "device" && key === "parameters") return o.params.flatMap(p => ["id", p.id]);
        if (key === "input_routing_type" || key === "input_routing_channel") return JSON.stringify({ [key]: o.props[key] });
        if (key === "available_input_routing_types") return JSON.stringify({ [key]: PORTS });
        if (key === "available_input_routing_channels") return JSON.stringify({ [key]: CHANNELS });
        return [o.props[key]];
      },
      set(key, value) { writes.push({ id: o.id, key, value }); o.props[key] = value; },
      call(method, ...args) {
        calls.push({ id: o.id, method, args });
        if (o.kind === "song" && method === "create_midi_track") { o.tracks.push(makeTrack({ name: "MIDI" })); return null; }
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
  const session = new Session({ api, setDictionary, dictionaryCall: (o, m, d) => o.call(m, d), readDictionary: v => (typeof v === "string" ? JSON.parse(v) : v) });
  const adapter = new Adapter({ api, session, dictionaryCall: (o, m, d) => o.call(m, d), readDictionary: JSON.parse });
  let n = 0;
  const request = (op, payload = {}) => adapter.handle({ id: "r" + (++n), op, payload });
  return { song, device, view, appView, entities, writes, calls, request, adapter, session, api, setDictionary };
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
    const live = liveSet({ tracks: [{ name: "Bass", arm: true, port: PORTS[2], channel: CHANNELS[1] }, { name: "MIDI" }], host: 1 });
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
