"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const {Observers} = require("../../../resources/devices/ableton-live/parameter_observers.js");
const {fixture} = require("./behavioral_host");

function setup() {
  const host = fixture(), work = new Set(), emissions = [], diagnostics = [];
  const handlers = new Set(), slots = new Set();
  const observers = new Observers({
    api: host.api,
    follow(path, changed) {
      slots.add(changed);
      return {get id() {return host.api(path).id;}, close() {slots.delete(changed);}};
    },
    defer: fn => {work.add(fn); return () => work.delete(fn);},
    observe: (_, fn) => {handlers.add(fn); return () => handlers.delete(fn);},
    emit: s => emissions.push(s), diagnostic: d => diagnostics.push(d)
  });
  function flush() {for (const fn of [...work]) {work.delete(fn); fn();}}
  return {host, observers, handlers, slots, emissions, diagnostics, flush};
}
test("native-unit samples follow value changes, mode, source replacement and disposal", () => {
  const {host, observers, handlers, slots, emissions, diagnostics, flush} = setup();
  const spec = observers.selected(), sources = {cutoff: spec};
  assert.equal(spec.path, host.cutoff.path);
  assert.deepEqual(spec.range, [20, 20000]);
  observers.replace(sources);
  flush();
  assert.equal(emissions.length, 0);
  observers.setActive(true);
  flush();
  assert.equal(emissions[0].value, 120);
  const [notify] = handlers;
  host.cutoff.props.value = 1420.75;
  notify(false);
  notify(false);
  flush();
  assert.equal(emissions.length, 2);
  assert.equal(emissions[1].value, 1420.75);
  observers.setActive(false);
  host.cutoff.props.value = 555;
  notify(false);
  flush();
  assert.equal(emissions.length, 2);
  observers.setActive(true);
  flush();
  assert.equal(emissions[2].value, 555);
  host.paths[spec.path] = {...host.cutoff, id: 99};
  // A track moved without changing its device parameter value.
  for (const changed of slots) changed();
  flush();
  assert.equal(handlers.size, 0);
  assert.match(diagnostics[0].reason, /replaced/);
  assert.equal(slots.size, 0);
  observers.setActive(true);
  flush();
  assert.equal(emissions.length, 3);
  observers.close();
});
test("renamed and missing saved sources remain disconnected without dropping other observers", () => {
  const {host, observers, handlers, emissions, diagnostics, flush} = setup();
  const spec = observers.selected();
  const result = observers.replace({valid: spec, moved: {...spec, "device-name": "Old name"}});
  assert.deepEqual(result.observing, ["valid"]);
  assert.equal(result.skipped[0].id, "moved");
  observers.setActive(true);
  flush();
  host.track.props.name = "Different";
  const [notify] = handlers;
  notify(true); flush();
  assert.match(diagnostics[0].reason, /renamed/);
  assert.equal(handlers.size, 0);
  assert.equal(emissions.length, 1);
  host.track.props.name = "Lights";
  observers.replace({valid: spec});
  const [old] = handlers;
  observers.replace({});
  old(false); flush();
  assert.equal(emissions.length, 1);
});

test("a malformed replacement keeps current subscriptions and allocates no partial sources", () => {
  const {observers, handlers, slots} = setup(), spec = observers.selected();
  observers.replace({existing: spec});
  assert.throws(() => observers.replace({added: spec, invalid: {path: "no such object"}}), /Invalid/);
  assert.equal(handlers.size, 1);
  assert.equal(slots.size, 1);
  observers.close();
  assert.equal(handlers.size, 0);
  assert.equal(slots.size, 0);
});
