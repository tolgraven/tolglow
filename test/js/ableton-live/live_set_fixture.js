"use strict";
// A behavioral stand-in for the Live object graph used by the companion's
// setup, Capture and clip operations. Only the Live objects are simulated;
// Adapter and Session are production code.
const { Adapter } = require("../../../resources/devices/ableton-live/live_api.js");
const { Session, COARSE, SLOTS } = require("../../../resources/devices/ableton-live/live_session.js");
const { Device: ParameterDevice } = require("../../../resources/devices/ableton-live/parameter_device.js");

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
// Cue launcher (#228): launcher: true puts the companion device in its
// track's device chain; { drumRack: { 36: "Kick" } } on a track adds a Drum
// Rack after it with one chain per named pad. version: [12, 2, 0] models a
// Live without Track.insert_device / RackDevice.insert_chain (both 12.3).
// A chain belongs to the pad whose note equals its in_note; insert_chain adds
// a chain with in_note -1 ("All Notes"), which belongs to no pad until set:
// an assumption about Live to verify on a real Set. palette: [0xrrggbb, ..]
// makes Chain.color store the nearest entry, as Live does; null keeps it exact.
// Parameter devices (#625): { parameterDevices: 1 } on a track appends that
// many "Tolglow parameters" devices, each with "Device On" and SLOTS slot
// parameters. The companion's relay reaches the production
// parameter_device.js, whose attribute messages are applied to the slot as
// Live is assumed to apply them (long name, range, quantization): to verify
// in licensed Live. relay: false leaves the Session without a relay.
// Setting a slot's value notifies "value" observers; automate(param, v)
// models automation playback (automation_state 1).
function liveSet({ tracks = [], host = 0, canCapture = true, slots = 4, joinsGroup = true, notify = "sync", now,
  launcher = false, version = [12, 4, 5], palette = null, relay = true, initialCallback = false } = {}) {
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
  const device = add({ kind: "device", props: { name: "Tolglow", can_have_drum_pads: 0 },
    params: COARSE.map((_, i) => add({ kind: "param", props: { name: "Tolglow lane " + (i + 1) } })) });
  function makeRack(named = {}) {
    const rack = add({ kind: "rack", props: { name: "Drum Rack", can_have_drum_pads: 1, has_drum_pads: 1 }, chains: [], pads: [] });
    rack.view = add({ kind: "rack-view", props: { drum_pads_scroll_position: 9 } });
    for (let note = 0; note < 128; note++) rack.pads.push(add({ kind: "pad", props: { note, name: "" }, rack }));
    for (const [note, name] of Object.entries(named))
      rack.chains.push(add({ kind: "chain", props: { name, color: 0x777777, is_auto_colored: 1, in_note: Number(note) } }));
    return rack;
  }
  function makeParameterDevice(ordinal) {
    const slot = (i) => add({ kind: "param", props: { name: "slot " + (i + 1), min: 0, max: 1, value: 0, automation_state: 0, is_quantized: 0 } });
    const pd = add({ kind: "device", props: { name: ordinal ? "Tolglow parameters " + (ordinal + 1) : "Tolglow parameters", can_have_drum_pads: 0 },
      params: [add({ kind: "param", props: { name: "Device On", min: 0, max: 1, value: 1, automation_state: 0, is_quantized: 1 } })]
        .concat(Array.from({ length: SLOTS }, (_, i) => slot(i))) });
    pd.applied = [];
    pd.runtime = new ParameterDevice({ deviceId: () => pd.id, setAttr: (index, attribute, args) => {
      const p = pd.params[index + 1].props;
      pd.applied.push([index, attribute].concat(args));
      if (attribute === "_parameter_longname") p.name = args[0];
      if (attribute === "_parameter_mmin") p.min = args[0];
      if (attribute === "_parameter_mmax") p.max = args[0];
      if (attribute === "_parameter_type") p.is_quantized = args[0] === 0 ? 0 : 1;
      if (attribute === "_parameter_range") { p.min = 0; p.max = args.length - 1; }
      p.value = Math.max(p.min, Math.min(p.max, p.value));
    } });
    return pd;
  }
  tracks.forEach((t, i) => {
    if (launcher && i === host) song.tracks[i].props.devices = ["id", device.id];
    if (t.drumRack) song.tracks[i].props.devices = song.tracks[i].props.devices.concat(["id", makeRack(t.drumRack).id]);
    for (let d = 0; d < (t.parameterDevices || 0); d++)
      song.tracks[i].props.devices = song.tracks[i].props.devices.concat(["id", makeParameterDevice(d).id]);
  });
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
        if (o.kind === "clip" && key === "has_envelopes") return [Object.keys(o.envelopes).length ? 1 : 0];
        if (o.kind === "device" && key === "parameters") return o.params.flatMap(p => ["id", p.id]);
        if (o.kind === "rack" && key === "drum_pads") return o.pads.flatMap(p => ["id", p.id]);
        if (o.kind === "rack" && key === "chains") return o.chains.flatMap(c => ["id", c.id]);
        if (o.kind === "pad" && key === "chains") return o.rack.chains.filter(c => c.props.in_note === o.props.note).flatMap(c => ["id", c.id]);
        if (key === "input_routing_type" || key === "input_routing_channel" || key === "output_routing_type") return JSON.stringify({ [key]: o.props[key] });
        if (key === "available_input_routing_types") return JSON.stringify({ [key]: PORTS });
        if (key === "available_input_routing_channels") return JSON.stringify({ [key]: CHANNELS });
        return Array.isArray(o.props[key]) ? o.props[key] : [o.props[key]];
      },
      set(key, value) {
        writes.push({ id: o.id, key, value });
        if (o.kind === "chain" && key === "color" && palette) {
          const dist = (a, b) => [16, 8, 0].reduce((sum, s) => sum + (((a >> s) & 255) - ((b >> s) & 255)) ** 2, 0);
          value = palette.reduce((best, c) => (dist(c, value) < dist(best, value) ? c : best), palette[0]);
        }
        o.props[key] = value;
        // Live stores a parameter value as a 32-bit float.
        if (o.kind === "param" && key === "value") { o.props.value = Math.fround(value); value = o.props.value; }
        if (o.kind === "param" && key === "value") { o.props.automation_state = o.props.automation_state === 1 ? 2 : o.props.automation_state; notifyAll(o.id, "value", value); }
      },
      call(method, ...args) {
        calls.push({ id: o.id, method, args });
        if (o.kind === "song" && method === "create_midi_track") {
          const index = args[0] === -1 || args[0] === undefined ? o.tracks.length : args[0];
          const track = makeTrack({ name: "MIDI" }), before = o.tracks[index - 1];
          if (joinsGroup && before) track.props.group_track = ["id", before.props.is_foldable ? before.id : groupOf(before) || 0];
          o.tracks.splice(index, 0, track);
          return null;
        }
        if (o.kind === "track" && method === "insert_device") {
          if (version[0] < 12 || (version[0] === 12 && version[1] < 3)) throw new Error("Invalid syntax: insert_device");
          if (args[0] !== "Drum Rack") throw new Error("Only the Drum Rack is simulated");
          const list = o.props.devices.slice(), at = args[1] === undefined ? list.length / 2 : args[1];
          if (at > list.length / 2) throw new Error("Invalid device index");
          list.splice(at * 2, 0, "id", makeRack().id);
          o.props.devices = list;
          return null;
        }
        if (o.kind === "rack" && method === "insert_chain") {
          if (version[0] < 12 || (version[0] === 12 && version[1] < 3)) throw new Error("Invalid syntax: insert_chain");
          o.chains.push(add({ kind: "chain", props: { name: "", color: 0x777777, is_auto_colored: 1, in_note: -1 } }));
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
        if (o.kind === "app" && method.startsWith("get_")) return [{ get_major_version: version[0], get_minor_version: version[1], get_bugfix_version: version[2] }[method]];
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
    const d = /^live_set tracks (\d+) devices (\d+)( view)?$/.exec(p);
    if (d) {
      const track = song.tracks[Number(d[1])], id = track && track.props.devices[Number(d[2]) * 2 + 1], dev = entities[id];
      return d[3] ? dev && dev.view : dev;
    }
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
  // initialCallback: true models LiveAPI calling an observer once with the
  // current value when its property is assigned (to confirm in licensed Live).
  const observe = (id, property, changed, immediate) => {
    const entry = { id, property, changed, immediate, open: true };
    observers.push(entry);
    if (initialCallback && entities[id]) {
      const value = entities[id].props[property];
      if (immediate) immediate([value]);
      if (changed) changed([value]);
    }
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
  // Like max_adapter.js: messnamed reaches every parameter device; only the
  // addressed one applies the spec.
  const relayed = [];
  const relayTo = (deviceId, spec) => {
    relayed.push({ deviceId, spec });
    Object.values(entities).forEach(e => { if (e.runtime) e.runtime.configure(deviceId, JSON.parse(JSON.stringify(spec))); });
  };
  // Automation playback moves a slot: Live sets automation_state 1 and the value.
  const automate = (paramId, value) => {
    const p = entities[paramId];
    p.props.automation_state = 1; p.props.value = value;
    notifyAll(paramId, "automation_state", 1);
    notifyAll(paramId, "value", value);
  };
  // A performer turns a slot's dial in Live: an automated slot becomes overridden (2).
  const turn = (paramId, value) => {
    const p = entities[paramId];
    if (p.props.automation_state === 1) { p.props.automation_state = 2; notifyAll(paramId, "automation_state", 2); }
    p.props.value = value;
    notifyAll(paramId, "value", value);
  };
  const session = new Session({ api, setDictionary, dictionaryCall: (o, m, d) => o.call(m, d), readDictionary: v => (typeof v === "string" ? JSON.parse(v) : v),
    observe, emit: (event) => emitted.push(event), now, relay: relay ? relayTo : null });
  const adapter = new Adapter({ api, session, dictionaryCall: (o, m, d) => o.call(m, d), readDictionary: JSON.parse });
  let n = 0;
  const request = (op, payload = {}) => adapter.handle({ id: "r" + (++n), op, payload });
  const rackOf = (index) => song.tracks[index].props.devices.filter((x, i) => i % 2 === 1).map(id => entities[id]).find(e => e && e.kind === "rack");
  const parameterDevicesOf = (index) => song.tracks[index].props.devices.filter((x, i) => i % 2 === 1).map(id => entities[id])
    .filter(e => e && e.runtime);
  return { song, device, view, appView, entities, writes, calls, request, adapter, session, api, setDictionary, moveCompanion, rackOf,
    observers, emitted, play, fire, inGroup, relayed, relayTo, automate, turn, parameterDevicesOf };
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
      ? [{ name: "Tolglow group stage", group: true }, { name: "Tolglow group warm", group: true, parent: "Tolglow group stage" }]
      // "parameters" adds a run track for cue :a holding one parameter device (#625).
      : process.argv[3] === "parameters" ? [{ name: "Tolglow run a", parameterDevices: 1 }] : [];
    // The companion is in its track's device chain, so the cue launcher (#228)
    // can insert a Drum Rack after it.
    const live = liveSet({ tracks: [{ name: "Bass", arm: true, port: PORTS[2], channel: CHANNELS[1] }, { name: "MIDI" }].concat(groups), host: 1,
      launcher: true });
    const max = vm.createContext({
      include() {}, TolglowLive: require("../../../resources/devices/ableton-live/live_api"),
      TolglowSession: require("../../../resources/devices/ableton-live/live_session"),
      TolglowParameters: require("../../../resources/devices/ableton-live/parameter_observers"),
      Task: function (fn) { let t; return { schedule(d) { t = setTimeout(fn, d); }, cancel() { clearTimeout(t); } }; },
      Dict: function () { throw new Error("Dict is not used by the simulated Set"); },
      messnamed(name, verb, deviceId, json) { if (name === "tolglow-parameter-device" && verb === "configure") live.relayTo(deviceId, JSON.parse(json)); },
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
