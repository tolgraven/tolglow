/* This script belongs behind [deferlow] in a Max for Live MIDI Effect.
 * Include files remain next to the .maxpat when saved into an .amxd device.
 */
autowatch = 1;
inlets = 1;
outlets = 2; // 0: messages to node.script, 1: MIDI-thru gate/status
include("live_api.js");
include("parameter_observers.js");
include("live_session.js");

var adapter = null, localId = 0;
function liveObject(path) {
  if (typeof path === "object") { var object = new LiveAPI(null); object.id = path.id; return object; }
  return new LiveAPI(null, path);
}
function observeParameter(path, changed) {
  var handles = [];
  function clear() {
    handles.forEach(function (handle) { handle.property = ""; handle.id = 0; });
    handles = [];
  }
  try {
    function watch(address, property, identity) {
      var handle = new LiveAPI(function () { changed(identity); }, address);
      handles.push(handle);
      handle.property = property;
    }
    watch(path, "value", false);
    watch(path, "automation_state", false);
    watch(path, "name", true);
    watch(path.match(/^live_set tracks [0-9]+/)[0], "name", true);
    if (path.indexOf(" mixer_device ") < 0) watch(path.replace(/ parameters [0-9]+$/, ""), "name", true);
    return clear;
  } catch (error) { clear(); throw error; }
}
function readDictionary(value) {
  if (Array.isArray(value) && value[0] === "dictionary") return JSON.parse(new Dict(value[1]).stringify());
  if (Array.isArray(value) && value.length === 1) value = value[0];
  return typeof value === "string" ? JSON.parse(value) : value;
}
function init() {
  if (adapter) return;
  adapter = new TolglowLive.Adapter({
    api: liveObject,
    parameters: new TolglowParameters.Observers({
      api: liveObject, observe: observeParameter,
      follow: function (path, changed) {
        var slot = new LiveAPI(changed, path);
        slot.mode = 1;
        return {get id() { return slot.id; }, close: function () { slot.id = 0; }};
      },
      defer: function (fn) {
        var task = new Task(fn);
        task.schedule(0);
        return function () { task.cancel(); };
      },
      emit: function (sample) { outlet(0, "event", JSON.stringify({kind: "parameter", sample: sample})); },
      diagnostic: function (sample) {
        outlet(1, "diagnostic", sample.reason);
        outlet(0, "event", JSON.stringify({kind: "parameter-error", sample: sample}));
      }
    }),
    // Setup, Capture and clip writes share the same LiveAPI and dictionary
    // boundaries. Dictionary-valued properties (input routing) are set with
    // the dictionary object itself; confirm the encoding in licensed Live.
    session: new TolglowSession.Session({
      api: liveObject,
      dictionaryCall: function (object, method, data) { return object.call(method, data); },
      readDictionary: readDictionary
    }),
    // Live's note methods take a dictionary. Keep the host-specific serialization
    // isolated; do not retry a mutating method with a different encoding.
    dictionaryCall: function (object, method, data) { return object.call(method, data); },
    readDictionary: readDictionary
  });
  outlet(1, "thru", 1);
}
function request(json) {
  init();
  try {
    var result = adapter.handle(JSON.parse(json));
    outlet(0, "response", JSON.stringify(result));
    if (result.status === "executed" && result.result && result.result.mode)
      outlet(1, "thru", result.result.mode === "live" ? 1 : 0);
  } catch (error) { outlet(1, "diagnostic", error.message); }
}
function pair(track, clipSlot, rackDevice) {
  init();
  var payload = {"track-index": track, "clip-slot-index": clipSlot};
  if (rackDevice !== undefined) payload["rack-device-index"] = rackDevice;
  var result = adapter.handle({id: "local-pair-" + (++localId), op: "configure-destination", payload: payload});
  outlet(1, "status", JSON.stringify(result));
  if (result.status === "executed") outlet(0, "event", JSON.stringify({kind: "status", status: result.result}));
}
function poll() {
  init();
  // Refresh the cached host track (one LiveAPI object per poll, not per MIDI
  // event), so a companion moved to another track tags playback correctly.
  if (adapter.session) adapter.session.hostTrackId(true);
  var result = adapter.handle({id: "local-inspect-" + (++localId), op: "inspect", payload: {}});
  if (result.status === "executed") outlet(0, "event", JSON.stringify({kind: "status", status: result.result}));
  else outlet(1, "diagnostic", result.error.message);
}
function mode(value) {
  if (value !== "lighting" && value !== "live") { outlet(1, "diagnostic", "Mode must be live or lighting"); return; }
  // The studio ends held activations before calling this device's handoff op.
  outlet(0, "event", JSON.stringify({kind: "handoff", mode: value}));
}
function forward(event) {
  init();
  try { var result = adapter.midi(event); if (result) outlet(0, "event", JSON.stringify({kind: "midi", event: result})); }
  catch (error) { outlet(1, "diagnostic", error.message); }
}
// A companion lane parameter ("Tolglow lane k") moved, by automation or by hand.
function lane(k, value) {
  init();
  try {
    adapter.lane(k, value).forEach(function (event) { outlet(0, "event", JSON.stringify({kind: "midi", event: event})); });
  } catch (error) { outlet(1, "diagnostic", error.message); }
}
function note(pitch, velocity, channel) { forward({type: velocity > 0 ? "note-on" : "note-off", note: pitch, velocity: velocity, channel: channel}); }
function cc(value, controller, channel) { forward({type: "cc", controller: controller, value: value, channel: channel}); }
function stopped() {
  if (adapter) {
    adapter.handle({id: "local-stopped-" + (++localId), op: "handoff", payload: {mode: "live"}});
    if (adapter.parameters) adapter.parameters.close();
    outlet(1, "thru", 1);
  }
}
function notifydeleted() { stopped(); outlet(0, "stop"); }
