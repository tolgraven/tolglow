/* Max js (ES5). DeviceParameter callbacks run on the host's low-priority queue. */
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.TolglowParameters = factory();
}(this, function () {
  "use strict";
  var trackPath = /^live_set tracks [0-9]+/;
  var parameterPath = /^live_set tracks [0-9]+ (?:devices [0-9]+ (?:chains|return_chains) [0-9]+ )*(?:devices [0-9]+ parameters [0-9]+|mixer_device (?:volume|panning|sends [0-9]+))$/;
  function fail(message) { var error = new Error(message); error.code = "live-parameter"; throw error; }
  function scalar(value) { return Array.isArray(value) ? value[0] : value; }
  function property(obj, key) { return scalar(obj.get(key)); }
  function usable(obj) { return obj && Number(obj.id) > 0; }
  function pathOf(obj) { return String(obj.path).replace(/^"|"$/g, ""); }
  function Observers(options) {
    this.api = options.api;
    this.observe = options.observe;
    this.follow = options.follow;
    this.defer = options.defer;
    this.emit = options.emit;
    this.diagnostic = options.diagnostic;
    this.entries = Object.create(null);
    this.active = false;
  }
  Observers.prototype.describe = function (parameter) {
    if (!usable(parameter)) fail("Select a device or mixer parameter in Live");
    var path = pathOf(parameter);
    if (!parameterPath.test(path)) fail("Choose a device, nested Rack or mixer parameter on a regular track");
    var track = this.api(path.match(trackPath)[0]);
    var parent = this.api(path.replace(/ (?:parameters [0-9]+|sends [0-9]+|volume|panning)$/, ""));
    if (!usable(track) || !usable(parent)) fail("The selected parameter's track or device is unavailable");
    var range = [Number(property(parameter, "min")), Number(property(parameter, "max"))];
    if (!isFinite(range[0]) || !isFinite(range[1]) || range[0] >= range[1]) fail("Selected parameter has no usable range");
    return {path: path, name: String(property(parameter, "name")), "range": range,
      "track-name": String(property(track, "name")), "quantized?": !!property(parameter, "is_quantized"),
      "device-name": / mixer_device /.test(path) ? "Mixer" : String(property(parent, "name"))};
  };
  Observers.prototype.selected = function () {
    var selected = this.api("live_set view selected_parameter");
    if (!usable(selected)) fail("Select a device or mixer parameter in Live");
    return this.describe(this.api({id: Number(selected.id)}));
  };
  function releaseEntry(entry) {
    if (entry.cancel) entry.cancel();
    if (entry.detach) entry.detach();
    if (entry.slot && entry.slot.close) entry.slot.close();
  }
  Observers.prototype.release = function (entries) {
    Object.keys(entries).forEach(function (id) {
      releaseEntry(entries[id]);
    });
  };
  Observers.prototype.schedule = function (id, entry) {
    var self = this;
    if (!this.active || this.entries[id] !== entry || entry.cancel) return;
    entry.cancel = this.defer(function () {
      entry.cancel = null;
      if (!self.active || self.entries[id] !== entry) return;
      try {
        var current = entry.target;
        if (!usable(current) || pathOf(current) !== entry.spec.path || Number(entry.slot.id) !== entry.objectId)
          fail("Live moved or replaced the parameter; learn its source again");
        if (entry.checkIdentity) {
          entry.checkIdentity = false;
          var spec = self.describe(current);
          if (spec.name !== entry.spec.name || spec["track-name"] !== entry.spec["track-name"] || spec["device-name"] !== entry.spec["device-name"])
            fail("Live renamed the parameter, device or track; learn its source again");
        }
        var value = Number(property(current, "value")), state = Number(property(current, "automation_state"));
        if (!isFinite(value) || value < entry.spec.range[0] || value > entry.spec.range[1] ||
            !isFinite(state) || Math.floor(state) !== state || state < 0 || state > 2) return;
        if (value !== entry.previous || state !== entry.previousState) {
          entry.previous = value; entry.previousState = state;
          self.emit({id: id, generation: entry.generation, path: entry.spec.path, value: value, "automation-state": state});
        }
      } catch (error) {
        if (self.entries[id] === entry) delete self.entries[id];
        releaseEntry(entry);
        self.diagnostic({id: id, generation: entry.generation, reason: error.message});
      }
    });
  };
  Observers.prototype.replace = function (sources, generation) {
    if (!sources || typeof sources !== "object" || Array.isArray(sources) || Object.keys(sources).length > 64)
      fail("Expected up to 64 named Live parameter sources");
    var next = Object.create(null), self = this, skipped = [];
    Object.keys(sources).sort().forEach(function (id) {
      var spec = sources[id];
      if (!/^[A-Za-z0-9][A-Za-z0-9_.-]{0,63}$/.test(id) || !spec || !parameterPath.test(spec.path))
        fail("Invalid parameter source name or canonical Live path");
      var target = self.api(spec.path);
      // An unrelated Set can be opened between runs. One missing source should
      // not prevent the remaining verified sources from being reconnected.
      try {
        var actual = self.describe(target);
        if (actual.name !== spec.name || actual["track-name"] !== spec["track-name"] || actual["device-name"] !== spec["device-name"] ||
            actual.range[0] !== spec.range[0] || actual.range[1] !== spec.range[1] || actual["quantized?"] !== spec["quantized?"])
          fail("The saved parameter identity or range changed");
        var entry = {spec: spec, generation: generation, target: target, objectId: Number(target.id), cancel: null};
        entry.slot = self.follow(spec.path, function () { self.schedule(id, entry); });
        next[id] = entry;
      } catch (error) { skipped.push({id: id, reason: error.message}); }
    });
    try {
      Object.keys(next).forEach(function (id) {
        var entry = next[id];
        entry.detach = self.observe(entry.spec.path, function (identityChanged) {
          if (identityChanged) entry.checkIdentity = true;
          self.schedule(id, entry);
        });
      });
    } catch (error) { this.release(next); throw error; }
    this.release(this.entries);
    this.entries = next;
    Object.keys(next).forEach(function (id) { self.schedule(id, next[id]); });
    return {observing: Object.keys(next), skipped: skipped};
  };
  Observers.prototype.setActive = function (active) {
    this.active = active;
    var self = this;
    if (active) Object.keys(this.entries).forEach(function (id) {
      var entry = self.entries[id];
      entry.previous = null;
      self.schedule(id, entry);
    });
  };
  Observers.prototype.close = function () {
    this.active = false;
    this.release(this.entries);
    this.entries = Object.create(null);
  };
  return {Observers: Observers};
}));
