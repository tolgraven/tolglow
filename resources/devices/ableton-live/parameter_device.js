/* Max js (ES5) inside a "Tolglow parameters" Max for Live device (#625).
 *
 * The device sits on a Tolglow run track and declares SLOTS automatable
 * live.dial parameters named slot-1 .. slot-32 (tolglow-parameters.maxpat).
 * Tolglow's spec arrives from the companion on the global
 * [receive tolglow-parameter-device] as "configure <device id> <json>"; a
 * device applies only a spec addressed to its own Live object id. Each slot's
 * long and short name, type, range, steps, unit format and choices are set as
 * parameter attributes through the injected setAttr(index, attribute, args)
 * boundary, and Push banks through setBanks(banks).
 *
 * The device sends nothing to Tolglow. The companion reads its parameters
 * through the Live Object Model and observes their values and
 * automation_state (live_session.js watchSlots).
 *
 * Which parameter attributes Live accepts at run time, and whether Live keeps
 * automation on a slot whose range changed, are host questions listed on the
 * #625 pull request's checklist.
 */
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.TolglowParameterDevice = factory();
}(this, function () {
  "use strict";
  var SLOTS = 32;
  // Max's _parameter_type and _parameter_unitstyle codes.
  var TYPE = {"float": 0, "int": 1, "enum": 2};
  var UNITSTYLE = {int: 0, "float": 1, custom: 9};

  function finite(n) { return typeof n === "number" && isFinite(n); }
  function integer(n, lo, hi) { return finite(n) && Math.floor(n) === n && n >= lo && n <= hi; }
  function clean(text, max) { return String(text).replace(/[\r\n\t"]/g, " ").slice(0, max); }

  // The Max attribute messages for one slot, in the order they are sent: the
  // type first, so a range or choice list is read for the right type.
  function attributes(slot) {
    var out = [["_parameter_type", [TYPE[slot.type]]],
      ["_parameter_longname", [clean(slot.name, 64)]],
      ["_parameter_shortname", [clean(slot["short-name"] || slot.name, 12)]]];
    if (slot.type === "enum") {
      out.push(["_parameter_range", slot.choices.map(function (c) { return clean(c, 32); })]);
    } else {
      out.push(["_parameter_mmin", [slot.min]], ["_parameter_mmax", [slot.max]]);
      var places = integer(slot.places, 0, 15) ? slot.places : 3, unit = slot.unit ? " " + clean(slot.unit, 16) : "";
      if (slot.type === "int") {
        out.push(["_parameter_steps", [integer(slot.steps, 2, 65536) ? slot.steps : slot.max - slot.min + 1]]);
        out.push(["_parameter_unitstyle", [unit ? UNITSTYLE.custom : UNITSTYLE.int]]);
        if (unit) out.push(["_parameter_units", ["%d" + unit]]);
      } else {
        out.push(["_parameter_steps", [0]]);
        out.push(["_parameter_unitstyle", [UNITSTYLE.custom]]);
        out.push(["_parameter_units", ["%." + places + "f" + unit]]);
      }
    }
    return out;
  }

  function valid(slot) {
    return slot && integer(slot.index, 0, SLOTS - 1) && typeof slot.name === "string" && slot.name.length > 0 &&
      TYPE[slot.type] !== undefined && finite(slot.min) && finite(slot.max) && slot.min < slot.max &&
      (slot.type !== "enum" || (Array.isArray(slot.choices) && slot.choices.length >= 2 && slot.choices.length <= 128));
  }

  function Device(options) {
    if (!options || typeof options.setAttr !== "function" || typeof options.deviceId !== "function")
      throw new Error("A parameter device needs setAttr and deviceId boundaries");
    this.setAttr = options.setAttr;
    this.setBanks = options.setBanks || null;
    this.deviceId = options.deviceId;
    this.applied = null;
  }

  // Apply a spec addressed to this device. Returns what was applied, or null
  // when the spec is for another device. An invalid spec changes nothing.
  Device.prototype.configure = function (deviceId, spec) {
    if (Number(deviceId) !== Number(this.deviceId())) return null;
    if (!spec || !Array.isArray(spec.slots) || spec.slots.length > SLOTS || !spec.slots.every(valid)) {
      var error = new Error("Invalid parameter device spec"); error.code = "slots"; throw error;
    }
    var self = this, seen = {};
    spec.slots.forEach(function (slot) {
      if (seen[slot.index]) { var e = new Error("Two specs for one slot"); e.code = "slots"; throw e; }
      seen[slot.index] = true;
    });
    spec.slots.forEach(function (slot) {
      attributes(slot).forEach(function (a) { self.setAttr(slot.index, a[0], a[1]); });
    });
    if (this.setBanks && Array.isArray(spec.banks)) this.setBanks(spec.banks);
    this.applied = spec.slots.length;
    return {applied: spec.slots.length};
  };

  return {Device: Device, SLOTS: SLOTS, attributes: attributes};
}));

// Max entry points. Outside Max (Node tests) these are never called.
if (typeof outlet === "function" && typeof LiveAPI === "function") {
  var tolglowDevice = null;
  var tolglowDeviceId = 0;
  // [live.thisdevice] bangs once the device is in a Live Set; only then is the
  // LiveAPI usable.
  var bang = function () {
    tolglowDeviceId = Number(new LiveAPI(null, "this_device").id);
    tolglowDevice = new TolglowParameterDevice.Device({
      deviceId: function () { return tolglowDeviceId; },
      setAttr: function (index, attribute, args) {
        var dial = this.patcher.getnamed("slot-" + (index + 1));
        if (dial) dial.message.apply(dial, [attribute].concat(args));
      }.bind(this),
      // [live.banks] takes the bank layout as a dictionary; the device keeps
      // its patcher's banks until the host checklist confirms the message.
      setBanks: null
    });
  };
  var configure = function (deviceId, json) {
    if (!tolglowDevice) return;
    try {
      var result = tolglowDevice.configure(deviceId, JSON.parse(json));
      if (result) outlet(0, "applied", result.applied);
    } catch (error) { outlet(0, "diagnostic", error.message); }
  };
}
