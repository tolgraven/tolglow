/* This script belongs behind [deferlow] in a Max for Live MIDI Effect.
 * Include files remain next to the .maxpat when saved into an .amxd device.
 */
autowatch = 1;
inlets = 1;
outlets = 2; // 0: messages to node.script, 1: MIDI-thru gate/status
include("live_api.js");

var adapter = null, localId = 0;
function init() {
  if (adapter) return;
  adapter = new TolglowLive.Adapter({
    api: function (path) {
      if (typeof path === "object") { var object = new LiveAPI(null); object.id = path.id; return object; }
      return new LiveAPI(null, path);
    },
    // Live's note methods take a dictionary. Keep the host-specific serialization
    // isolated; do not retry a mutating method with a different encoding.
    dictionaryCall: function (object, method, data) { return object.call(method, JSON.stringify(data)); },
    readDictionary: function (value) {
      if (value instanceof Array && value[0] === "dictionary") return JSON.parse(new Dict(value[1]).stringify());
      if (value instanceof Array && value.length === 1) value = value[0];
      return typeof value === "string" ? JSON.parse(value) : value;
    }
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
function note(pitch, velocity, channel) { forward({type: velocity > 0 ? "note-on" : "note-off", note: pitch, velocity: velocity, channel: channel}); }
function cc(value, controller, channel) { forward({type: "cc", controller: controller, value: value, channel: channel}); }
function notifydeleted() { outlet(0, "stop"); }
