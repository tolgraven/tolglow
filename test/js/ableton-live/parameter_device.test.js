"use strict";
// Parameter devices on run tracks (#625): the companion's operations and the
// in-device script, through the production Adapter, Session and
// parameter_device.js with only the Live object graph substituted
// (live_set_fixture.js). How Live applies parameter attributes at run time is
// an assumption of the fixture, listed on the pull request's host checklist.
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { Device, SLOTS, attributes } = require("../../../resources/devices/ableton-live/parameter_device.js");
const { liveSet } = require("./live_set_fixture.js");

const level = { index: 0, name: "Level", "short-name": "Level", type: "float", min: 0, max: 1, places: 3 };
const period = { index: 1, name: "Period", type: "float", min: 0.5, max: 32, places: 3, unit: "beats" };
const steps = { index: 2, name: "Steps", type: "int", min: 1, max: 16, steps: 16 };
const shape = { index: 3, name: "Shape", type: "enum", min: 0, max: 2, choices: ["Sine", "Saw", "Pulse"] };

test("slot attributes follow the shared control projection: names, ranges, decimals, units and choices", () => {
  assert.equal(SLOTS, 32, "same as live_session.js and live-devices/slots-per-device");
  assert.deepEqual(attributes(level), [["_parameter_type", [0]], ["_parameter_longname", ["Level"]], ["_parameter_shortname", ["Level"]],
    ["_parameter_mmin", [0]], ["_parameter_mmax", [1]], ["_parameter_steps", [0]], ["_parameter_unitstyle", [9]], ["_parameter_units", ["%.3f"]]]);
  assert.deepEqual(attributes(period).slice(-1), [["_parameter_units", ["%.3f beats"]]]);
  assert.deepEqual(attributes(steps).slice(3), [["_parameter_mmin", [1]], ["_parameter_mmax", [16]], ["_parameter_steps", [16]], ["_parameter_unitstyle", [0]]]);
  assert.deepEqual(attributes(shape), [["_parameter_type", [2]], ["_parameter_longname", ["Shape"]], ["_parameter_shortname", ["Shape"]],
    ["_parameter_range", ["Sine", "Saw", "Pulse"]]]);
});

test("a device applies only a spec addressed to it, and an invalid spec changes nothing", () => {
  const sent = [];
  const device = new Device({ deviceId: () => 42, setAttr: (i, a, args) => sent.push([i, a].concat(args)) });
  assert.equal(device.configure(41, { slots: [level] }), null);
  assert.equal(sent.length, 0);
  assert.throws(() => device.configure(42, { slots: [level, Object.assign({}, level)] }), /one slot/);
  assert.throws(() => device.configure(42, { slots: [Object.assign({}, level, { min: 2 })] }), /Invalid/);
  assert.equal(sent.length, 0, "refused before any attribute");
  assert.deepEqual(device.configure("42", { slots: [level, shape] }), { applied: 2 });
  assert.deepEqual(sent.filter(s => s[1] === "_parameter_longname"), [[0, "_parameter_longname", "Level"], [3, "_parameter_longname", "Shape"]]);
});

test("inspect-set lists parameter devices on Tolglow tracks only, slots after Device On", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 2 }, { name: "Bass", parameterDevices: 1 }] });
  const r = live.request("inspect-set");
  assert.equal(r.status, "executed");
  const [run, bass] = r.result.tracks;
  assert.equal(bass["parameter-devices"], undefined, "a musician's track is not searched");
  assert.deepEqual(run["parameter-devices"].map(d => [d.name, d.ordinal, d["device-index"], d.parameters.length]),
    [["Tolglow parameters", 0, 1, SLOTS], ["Tolglow parameters 2", 1, 2, SLOTS]]);
  assert.deepEqual(run["parameter-devices"][0].parameters[0], { index: 0, name: "slot 1", min: 0, max: 1, "quantized?": false, value: 0, "automation-state": 0 });
  assert.deepEqual(live.writes, []);
});

test("configure-parameter-device hands the layout to the addressed device and the read-back shows it", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 2 }] });
  const track = live.song.tracks[0], [first, second] = live.parameterDevicesOf(0);
  const payload = { "track-id": "track:" + track.id, "expected-name": "Tolglow run a", "device-id": "device:" + second.id, ordinal: 1,
    slots: [level, period, steps, shape], banks: [[0, 1, 2, 3]] };
  const r = live.request("configure-parameter-device", payload);
  assert.equal(r.status, "executed");
  assert.deepEqual(r.result, { "device-id": "device:" + second.id, relayed: 4 });
  assert.equal(first.applied.length, 0, "the other device ignored it");
  const params = live.request("inspect-set").result.tracks[0]["parameter-devices"][1].parameters;
  assert.deepEqual(params.slice(0, 4).map(p => [p.name, p.min, p.max, p["quantized?"]]),
    [["Level", 0, 1, false], ["Period", 0.5, 32, false], ["Steps", 1, 16, true], ["Shape", 0, 2, true]]);
});

test("configure-parameter-device refuses a stale device, a renumbered device, bad slots or no relay, before writing", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 1 }, { name: "Tolglow run b", parameterDevices: 1 }] });
  const [a, b] = live.song.tracks, [da] = live.parameterDevicesOf(0), [db] = live.parameterDevicesOf(1);
  const base = { "track-id": "track:" + a.id, "expected-name": "Tolglow run a", "device-id": "device:" + da.id, ordinal: 0, slots: [level] };
  const code = (payload, set = live) => { const r = set.request("configure-parameter-device", payload); return [r.status, r.error && r.error.code]; };
  assert.deepEqual(code(Object.assign({}, base, { "device-id": "device:" + db.id })), ["rejected", "stale-device"]);
  assert.deepEqual(code(Object.assign({}, base, { ordinal: 1 })), ["rejected", "device-renamed"]);
  assert.deepEqual(code(Object.assign({}, base, { slots: [Object.assign({}, shape, { choices: ["One"] })] })), ["rejected", "slots"]);
  assert.deepEqual(code(Object.assign({}, base, { "expected-name": "Tolglow run x" })), ["rejected", "track-renamed"]);
  assert.equal(live.relayed.length, 0);
  const norelay = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 1 }], relay: false });
  const [dn] = norelay.parameterDevicesOf(0);
  assert.deepEqual(code(Object.assign({}, base, { "track-id": "track:" + norelay.song.tracks[0].id, "device-id": "device:" + dn.id }), norelay),
    ["rejected", "relay-unavailable"]);
  assert.equal(b.props.name, "Tolglow run b");
});

test("set-parameter-values writes Tolglow's values into the slots, clamped to their ranges", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 1 }] });
  const track = live.song.tracks[0], [device] = live.parameterDevicesOf(0);
  live.request("configure-parameter-device", { "track-id": "track:" + track.id, "device-id": "device:" + device.id, ordinal: 0, slots: [level, period] });
  const r = live.request("set-parameter-values", { "track-id": "track:" + track.id, "expected-name": "Tolglow run a",
    values: [{ device: 0, index: 0, value: 0.25 }, { device: 0, index: 1, value: 64 }] });
  assert.equal(r.status, "executed");
  assert.deepEqual([device.params[1].props.value, device.params[2].props.value], [0.25, 32]);
  const missing = live.request("set-parameter-values", { "track-id": "track:" + track.id, values: [{ device: 1, index: 0, value: 0 }] });
  assert.deepEqual([missing.status, missing.error.code], ["rejected", "parameter-device-missing"]);
});

test("a run clip carries envelopes of its track's parameter device, and inspect-set says it has them", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 1 }] });
  const track = live.song.tracks[0], [device] = live.parameterDevicesOf(0);
  live.request("configure-parameter-device", { "track-id": "track:" + track.id, "device-id": "device:" + device.id, ordinal: 0, slots: [level, period] });
  const clip = { "length-beats": 4, notes: [{ pitch: 60, start: 0, duration: 4, velocity: 127 }] };
  const r = live.request("write-clip", { "track-id": "track:" + track.id, "expected-name": "Tolglow run a", name: "A run 1 [r.1]",
    "looping?": false, clip, "parameter-envelopes": [{ "device-id": "device:" + device.id, index: 0, points: [[0, 0.5], [2, 0.75]] },
      { "device-id": "device:" + device.id, index: 1, points: [[0, 99]] }] });
  assert.equal(r.status, "executed");
  assert.equal(r.result["envelope-steps"], 3);
  const written = live.entities[Number(r.result["clip-id"].slice(5))];
  assert.deepEqual(written.envelopes[device.params[1].id].steps, [{ time: 0, duration: 2, value: 0.5 }, { time: 2, duration: 2, value: 0.75 }]);
  assert.deepEqual(written.envelopes[device.params[2].id].steps, [{ time: 0, duration: 4, value: 32 }], "clamped to the slot's range");
  assert.equal(live.request("inspect-set").result.tracks[0].clips[0]["envelopes?"], true);
  const bad = live.request("write-clip", { "track-id": "track:" + track.id, name: "x", clip,
    "parameter-envelopes": [{ "device-id": "device:" + device.id, index: 0, points: [[2, 0.1], [1, 0.2]] }] });
  assert.deepEqual([bad.status, bad.error.code], ["rejected", "envelope-point"], "refused before the clip is created");
});

test("watch-run-tracks reports slot values with Live's automation state, once per change, until the device stops", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 2 }] });
  const track = live.song.tracks[0], [first, second] = live.parameterDevicesOf(0);
  const r = live.request("watch-run-tracks", { "track-ids": ["track:" + track.id] });
  assert.deepEqual(r.result, { watching: 1, "watching-slots": SLOTS * 2 });
  live.automate(first.params[1].id, 0.5);
  live.automate(first.params[1].id, 0.5);
  live.turn(second.params[3].id, 0.25);
  live.turn(first.params[1].id, 0.6);
  const events = live.emitted.map(e => [e.type, e["track-id"], e.slot, e.value, e["automation-state"]]);
  assert.deepEqual(events, [
    ["parameter-device-value", "track:" + track.id, 0, 0.5, 1],
    ["parameter-device-value", "track:" + track.id, SLOTS + 2, 0.25, 0],
    ["parameter-device-value", "track:" + track.id, 0, 0.5, 2],
    ["parameter-device-value", "track:" + track.id, 0, 0.6, 2]]);
  assert.deepEqual(live.emitted.map(e => e.serial), [1, 2, 3, 4]);
  live.session.closeWatches();
  live.turn(first.params[1].id, 0.9);
  assert.equal(live.emitted.length, 4, "watches end with the device");
});

test("the parameter device patcher declares 32 automatable slot dials behind its receive", () => {
  const patch = JSON.parse(fs.readFileSync(path.join(__dirname, "../../../resources/devices/ableton-live/tolglow-parameters.maxpat"), "utf8"));
  const boxes = patch.patcher.boxes.map(b => b.box);
  const dials = boxes.filter(b => b.maxclass === "live.dial");
  assert.equal(dials.length, SLOTS);
  assert.deepEqual(dials.map(d => d.varname), Array.from({ length: SLOTS }, (_, i) => "slot-" + (i + 1)));
  assert.ok(dials.every(d => d.saved_attribute_attributes.valueof.parameter_invisible === 0), "every slot is automatable");
  assert.deepEqual(dials.map(d => d.saved_attribute_attributes.valueof.parameter_order), Array.from({ length: SLOTS }, (_, i) => i));
  assert.ok(boxes.some(b => b.text === "receive tolglow-parameter-device"));
  assert.ok(boxes.some(b => b.text === "js parameter_device.js"));
  assert.ok(boxes.some(b => b.maxclass === "live.thisdevice" || b.text === "live.thisdevice"));
});

test("a slot watch starts from the current value: LiveAPI's first callback is not a performer move", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 1 }], initialCallback: true });
  const track = live.song.tracks[0], [device] = live.parameterDevicesOf(0);
  device.params[1].props.value = 0.42;
  live.request("watch-run-tracks", { "track-ids": ["track:" + track.id] });
  assert.deepEqual(live.emitted, [], "nothing is reported for values already on the dials");
  live.automate(device.params[1].id, 0.5);
  live.turn(device.params[1].id, 0.5);
  assert.deepEqual(live.emitted.map(e => [e.value, e["automation-state"], e["value-changed?"]]), [[0.5, 1, true], [0.5, 2, false]],
    "an override alone is reported as a state change, not a move");
});

test("values the companion wrote for Tolglow are not reported back, and unchanged slots are not rewritten", () => {
  const live = liveSet({ tracks: [{ name: "Tolglow run a", parameterDevices: 1 }] });
  const track = live.song.tracks[0], [device] = live.parameterDevicesOf(0);
  live.request("configure-parameter-device", { "track-id": "track:" + track.id, "device-id": "device:" + device.id, ordinal: 0, slots: [level] });
  live.request("watch-run-tracks", { "track-ids": ["track:" + track.id] });
  const write = (v) => live.request("set-parameter-values", { "track-id": "track:" + track.id, values: [{ device: 0, index: 0, value: v }] });
  assert.deepEqual(write(0.3).result, { written: 1, skipped: 0 });
  assert.deepEqual(write(0.4).result, { written: 1, skipped: 0 });
  assert.deepEqual(live.emitted, [], "the companion's own writes are not echoed");
  assert.deepEqual(write(0.4).result, { written: 0, skipped: 1 }, "a slot already showing the value is skipped");
  live.turn(device.params[1].id, 0.25);
  assert.deepEqual(live.emitted.map(e => e.value), [0.25], "a performer's turn is still reported");
});
