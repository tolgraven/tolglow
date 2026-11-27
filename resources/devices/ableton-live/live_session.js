/* Live Set operations for automatic setup, Capture, clip writes (#680, #671),
 * the cue launcher's Drum Rack (#228)
 * and the run-track mirror of what Tolglow ran (#683).
 * ES5 for Max's [js]. Runs behind [deferlow] inside the companion's adapter.
 * The Tolglow side of this protocol is tolglow.integrations.live-connection;
 * docs/live-routing.md#capture-the-last-phrase-into-live lists the operations.
 *
 * Every Live object is reached through the injected `api(path | {id})`
 * boundary, dictionaries through `dictionaryCall`/`readDictionary`, and
 * dictionary-valued properties through `setDictionary`. Writes call `mark()`
 * first so a later failure is reported as indeterminate, never as rejected.
 * Run-track playback is watched through the injected `observe(id, property,
 * changed)` boundary and reported through `emit(event)`.
 *
 * Parameter devices (#625): a run track can hold "Tolglow parameters"
 * devices (parameter_device.js), each with SLOTS automatable parameters
 * after Live's "Device On". DeviceParameter.name, min and max are read-only
 * in the LOM, so configure-parameter-device hands the slot layout to the
 * device itself through the injected `relay(deviceId, spec)` boundary (Max's
 * global messnamed); the next inspect-set reads back what it applied.
 * DeviceParameter.value is documented as settable, so set-parameter-values
 * writes values directly, and watch-run-tracks observes value and
 * automation_state of every slot on the watched tracks.
 *
 * Group Tracks: the LOM documents Track.group_track, is_foldable and
 * is_grouped for reading membership, and no call that creates a Group Track or
 * moves a track into one. ensure-group therefore only finds a Group Track by
 * name. ensure-track inserts a new track after a group's last member and reads
 * back where Live put it; that such an insert joins the group is unverified.
 */
(function (root, factory) {
  var exported = factory();
  if (typeof module === "object" && module.exports) module.exports = exported;
  else root.TolglowSession = exported;
}(this, function () {
  "use strict";
  // Coarse controllers for parameter lanes; fine is controller + 32. Lane k
  // (1-based) is the companion parameter "Tolglow lane k". Keep in step with
  // tolglow.integrations.phrase-clip/coarse-controllers.
  var COARSE = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31];
  var MAX14 = 16383;
  var MONITOR = ["in", "auto", "off"];
  var CAPTURE_DESTINATION = {auto: 0, session: 1, arrangement: 2};
  var OPERATIONS = ["inspect-set", "ensure-track", "ensure-group", "configure-track", "write-clip", "extend-clip", "capture-midi",
    "read-clip", "show-clip", "fire-clip", "stop-clips", "watch-run-tracks", "place-clip",
    "ensure-launcher", "write-pads", "inspect-launcher", "configure-parameter-device", "set-parameter-values"];
  // Parameter devices (#625). Keep in step with parameter_device.js SLOTS and
  // tolglow.integrations.live-devices/slots-per-device.
  var SLOTS = 32;
  var PARAMETER_DEVICE = /^Tolglow parameters(?: (\d+))?$/;
  // Envelope read-back: scan at this step, then bisect each change to one
  // Standard MIDI File tick (1/960 beat).
  var TICK = 1 / 960;
  // A track whose name starts with this is Tolglow's to adopt and configure.
  var TOLGLOW_NAME = /^Tolglow(\s|$)/;
  // A cue launcher track (#228). Other Tolglow tracks, such as the Capture
  // cue track the companion hosts after #680 setup, are never adopted by it.
  var LAUNCHER_NAME = /^Tolglow launcher(\s|$)/;
  // Live stores a chain colour as the nearest entry of its palette, so a
  // read-back this close to the requested colour is that colour: rewriting it
  // would dirty the Set on every setup. Squared RGB distance.
  var PALETTE_SNAP = 96 * 96;

  function fault(code, message) { var e = new Error(message); e.code = code; throw e; }
  function finite(n) { return typeof n === "number" && isFinite(n); }
  function integer(n, lo, hi) { return finite(n) && Math.floor(n) === n && n >= lo && n <= hi; }
  function scalar(value) { return Array.isArray(value) ? value[0] : value; }
  function ids(value) {
    var out = [], i;
    if (!Array.isArray(value)) return out;
    for (i = 0; i < value.length; i++) if (typeof value[i] === "number" && value[i] > 0) out.push(value[i]);
    return out;
  }
  function idOf(text, prefix) {
    var m = typeof text === "string" && new RegExp("^" + prefix + ":(\\d+)$").exec(text);
    if (!m) fault("identity", "Expected a " + prefix + " identity such as " + prefix + ":12");
    return Number(m[1]);
  }
  function laneOf(controller) { var i = COARSE.indexOf(controller); return i < 0 ? null : i + 1; }
  // Clip playback reaches the companion on channel 1, so channel 1 (or "All
  // Channels") on the companion's track is never treated as Tolglow's feed.
  function feedOf(channel) { return integer(channel, 2, 16) ? channel : null; }

  function Session(options) {
    if (!options || typeof options.api !== "function" || typeof options.dictionaryCall !== "function" ||
        typeof options.readDictionary !== "function")
      fault("session-options", "LiveAPI and dictionary boundaries are required");
    this.api = options.api; this.dictionaryCall = options.dictionaryCall; this.readDictionary = options.readDictionary;
    this.setDictionary = options.setDictionary || function (object, property, value) { object.set(property, value); };
    this.hostPath = options.hostPath || "this_device canonical_parent";
    this.devicePath = options.devicePath || "this_device";
    this.sampleBeats = options.sampleBeats || 0.0625;
    this.observe = options.observe || null;
    this.emit = options.emit || null;
    // Run-track watches: {trackId: {close, slot}}, replaced as a whole. Events
    // carry this session's ID with their serial, so a reloaded companion that
    // counts from 1 again is not taken for a repeat.
    this.watches = {};
    // Parameter-device slot watches on run tracks: {parameterId: {close, ..}}.
    this.slotWatches = {};
    this.relay = options.relay || null;
    this.watchSerial = 0;
    this.watchSession = options.watchSession || (String(new Date().getTime()) + "-" + Math.floor(Math.random() * 1e9));
    // What the companion's own track is playing, for origin marking:
    // {host, slot, fired, changedAt, closers}. hostArrangement and hostRouted
    // are refreshed with the host track.
    this.hostWatch = null;
    this.hostArrangement = null;
    this.hostRouted = null;
    this.now = options.now || function () { return new Date().getTime(); };
    // MIDI within this many milliseconds after a launch, start or stop on the
    // companion's track was noted counts as playback. It only helps notes
    // handled after the notification stored the change; a note handled
    // before it (launch quantization None) still reads the old slot.
    this.transitionMs = options.transitionMs === undefined ? 250 : options.transitionMs;
    // The companion's own track id, cached: MIDI events and lane moves must not
    // build a LiveAPI object each. Refreshed by every Set operation, so moving
    // the device to another track is picked up at the next setup or write.
    this.host = undefined;
    // {host, channel}: the feed channel of the companion's track. Re-read from
    // the track's input routing when the device reloads or moves, because a
    // rerun of setup on a configured Set writes (and so tells us) nothing.
    this.feed = null;
  }

  Session.prototype.object = function (path) {
    var object = this.api(path);
    if (!object || !Number(object.id)) fault("live-object-missing", "Live object is not available: " + JSON.stringify(path));
    return object;
  };
  Session.prototype.read = function (object, property) { return scalar(object.get(property)); };
  Session.prototype.dict = function (object, property) {
    var value = this.readDictionary(object.get(property));
    return value && value[property] !== undefined ? value[property] : value;
  };

  Session.prototype.trackIds = function () { return ids(this.object("live_set").get("tracks")); };
  Session.prototype.hostTrackId = function (refresh) {
    if (refresh || this.host === undefined) {
      var host = this.api(this.hostPath);
      this.host = host && Number(host.id) ? Number(host.id) : null;
      if (this.host) {
        try { this.hostArrangement = ids(host.get("arrangement_clips")).length > 0; } catch (e) { this.hostArrangement = null; }
        this.hostRouted = this.routedFromTrack(this.host);
      }
      this.watchHost();
    }
    return this.host;
  };
  // Whether MIDI from another track reaches the companion's track: its input
  // names another track ("MIDI From: <track>"), or another track's output
  // names the companion's track ("MIDI To: <companion track>"). That track's
  // clips then arrive as input. Routing is readable in the LOM; a routing
  // display name equal to a track name is taken as that track, which errs
  // toward playback. Unknown counts as routed.
  Session.prototype.routedFromTrack = function (hostId) {
    var self = this;
    try {
      var host = this.object({id: hostId}), hostName = String(this.read(host, "name"));
      var port = this.routing(host).port, others = this.trackIds().filter(function (id) { return id !== hostId; });
      return others.some(function (id) {
        var t = self.object({id: id});
        if (port !== null && String(self.read(t, "name")) === port) return true;
        // Only a MIDI track can send MIDI; Group and audio tracks are skipped.
        if (self.foldable(t) || !self.read(t, "has_midi_input")) return false;
        var out = self.dict(t, "output_routing_type");
        return !!out && out.display_name === hostName;
      });
    } catch (e) { return true; }
  };
  // Follow the companion track's Session playback: playing_slot_index and
  // fired_slot_index (a launched clip waiting for its quantized start). The
  // notification stores the value at once (a plain assignment, not a Set
  // change), so MIDI forwarded after it is already classified correctly.
  Session.prototype.watchHost = function () {
    var self = this, host = this.host;
    if (!this.observe || (this.hostWatch && this.hostWatch.host === host)) return;
    if (this.hostWatch) this.hostWatch.closers.forEach(function (close) { close(); });
    this.hostWatch = null;
    if (!host) return;
    var watch = {host: host, slot: null, fired: null, changedAt: null, closers: []};
    var noted = function (key) {
      return function (v) {
        var n = Number(scalar(v));
        if (n !== watch[key]) watch.changedAt = self.now();
        watch[key] = n;
      };
    };
    try {
      var track = this.object({id: host});
      watch.slot = Number(this.read(track, "playing_slot_index"));
      watch.fired = Number(this.read(track, "fired_slot_index"));
      watch.closers.push(this.observe(host, "playing_slot_index", null, noted("slot")));
      watch.closers.push(this.observe(host, "fired_slot_index", null, noted("fired")));
    } catch (e) { watch.slot = null; }
    this.hostWatch = watch;
  };
  // Live input only when that is known: the track plays and has launched no
  // Session clip, holds no Arrangement clip, takes no MIDI from another track,
  // and nothing changed on it within transitionMs. Anything else, unknown
  // included, is playback.
  Session.prototype.origin = function () {
    var w = this.hostWatch;
    if (!w || w.slot === null || !finite(w.slot) || w.slot >= 0) return "live-playback";
    if (finite(w.fired) && w.fired >= 0) return "live-playback";
    if (this.hostArrangement !== false || this.hostRouted !== false) return "live-playback";
    if (w.changedAt !== null && this.now() - w.changedAt < this.transitionMs) return "live-playback";
    return "live-input";
  };
  Session.prototype.closeWatches = function () {
    var self = this;
    Object.keys(this.watches).forEach(function (id) { self.watches[id].close(); });
    this.watches = {};
    this.closeSlotWatches();
    if (this.hostWatch) this.hostWatch.closers.forEach(function (close) { close(); });
    this.hostWatch = null;
  };
  // Empty: no Session or Arrangement clip and no device other than the companion.
  Session.prototype.emptyTrack = function (track) {
    var self = this;
    if (ids(track.get("clip_slots")).some(function (id) { return !!self.read(self.object({id: id}), "has_clip"); })) return false;
    if (ids(track.get("arrangement_clips")).length) return false;
    return ids(track.get("devices")).length <= 1;
  };
  Session.prototype.track = function (trackId, expectedName) {
    var id = idOf(trackId, "track");
    if (this.trackIds().indexOf(id) < 0) fault("stale-track", "That track is no longer in the Set");
    var track = this.object({id: id});
    if (expectedName !== undefined && expectedName !== null && String(this.read(track, "name")) !== expectedName)
      fault("track-renamed", "The track's name changed; run setup again");
    return track;
  };
  Session.prototype.trackIndex = function (track) { return this.trackIds().indexOf(Number(track.id)); };
  Session.prototype.slotPath = function (track, slot) { return "live_set tracks " + this.trackIndex(track) + " clip_slots " + slot; };

  Session.prototype.routing = function (track) {
    var type = this.dict(track, "input_routing_type") || {}, channel = this.dict(track, "input_routing_channel") || {};
    var m = /(\d+)\s*$/.exec(String(channel.display_name || ""));
    return {port: type.display_name === undefined ? null : String(type.display_name), channel: m ? Number(m[1]) : null};
  };

  Session.prototype.groupOf = function (track) {
    var group = ids(track.get("group_track"));
    return group.length ? group[0] : null;
  };
  Session.prototype.foldable = function (track) { return !!this.read(track, "is_foldable"); };

  Session.prototype.describeTrack = function (id, index) {
    var track = this.object({id: id}), self = this, clips = [], group = this.groupOf(track);
    if (this.foldable(track)) {
      // A Group Track has no input, arm or monitoring of its own; reading them
      // is not attempted, so a Set containing groups can always be inspected.
      return {"track-id": "track:" + id, index: index, name: String(this.read(track, "name")), kind: "group",
        "arm?": false, monitor: null, input: null, clips: [], "group-track-id": group ? "track:" + group : null};
    }
    var midi = !!this.read(track, "has_midi_input");
    var armable = !!this.read(track, "can_be_armed");
    ids(track.get("clip_slots")).forEach(function (slotId, slot) {
      var clipSlot = self.object({id: slotId});
      if (!self.read(clipSlot, "has_clip")) return;
      var clip = self.object(self.slotPathByIndex(index, slot) + " clip");
      clips.push({"clip-id": "clip:" + Number(clip.id), "slot-index": slot, name: String(self.read(clip, "name")),
        "length-beats": Number(self.read(clip, "length")), "envelopes?": !!self.read(clip, "has_envelopes")});
    });
    var result = {"track-id": "track:" + id, index: index, name: String(this.read(track, "name")),
      kind: midi ? "midi" : "audio", "arm?": armable && !!this.read(track, "arm"),
      monitor: MONITOR[Number(this.read(track, "current_monitoring_state"))] || null,
      input: this.routing(track), clips: clips, "group-track-id": group ? "track:" + group : null,
      "arrangement-clips": ids(track.get("arrangement_clips")).map(function (clipId) {
        var c = self.object({id: clipId});
        return {name: String(self.read(c, "name")), "start-beat": Number(self.read(c, "start_time")), "end-beat": Number(self.read(c, "end_time"))};
      })};
    if (id === this.hostTrackId()) { result["companion?"] = true; this.feed = {host: id, channel: feedOf(result.input.channel)}; }
    // Only Tolglow's own tracks are searched for parameter devices (#625).
    if (TOLGLOW_NAME.test(result.name)) result["parameter-devices"] = this.parameterDevices(track);
    return result;
  };

  // ---------------------------------------------------------------------
  // Parameter devices (#625). A Max for Live device lists "Device On" as its
  // first DeviceParameter; the SLOTS slot dials follow in their declared
  // order (to confirm in licensed Live: the patcher sets _parameter_order).
  Session.prototype.slotParameters = function (device) {
    var self = this, list = ids(device.get("parameters"));
    if (list.length && String(this.read(this.object({id: list[0]}), "name")) === "Device On") list = list.slice(1);
    return list.slice(0, SLOTS).map(function (id) { return self.object({id: id}); });
  };
  Session.prototype.parameterDevices = function (track) {
    var self = this, out = [];
    ids(track.get("devices")).forEach(function (id, index) {
      var device = self.api({id: id});
      if (!device || !Number(device.id)) return;
      var name = String(self.read(device, "name")), m = PARAMETER_DEVICE.exec(name);
      if (!m) return;
      out.push({"device-id": "device:" + id, "device-index": index, name: name, ordinal: m[1] ? Number(m[1]) - 1 : 0,
        parameters: self.slotParameters(device).map(function (p, i) {
          return {index: i, name: String(self.read(p, "name")), min: Number(self.read(p, "min")), max: Number(self.read(p, "max")),
            "quantized?": !!self.read(p, "is_quantized"), value: Number(self.read(p, "value")),
            "automation-state": Number(self.read(p, "automation_state"))};
        })});
    });
    return out;
  };
  // The parameter device with this id on this track, by its expected ordinal.
  Session.prototype.parameterDevice = function (track, deviceId, ordinal) {
    var id = idOf(deviceId, "device");
    if (ids(track.get("devices")).indexOf(id) < 0) fault("stale-device", "That parameter device is no longer on the named track");
    var device = this.object({id: id}), m = PARAMETER_DEVICE.exec(String(this.read(device, "name")));
    if (!m) fault("not-parameter-device", "That device is not a Tolglow parameters device");
    if (ordinal !== undefined && ordinal !== null && (m[1] ? Number(m[1]) - 1 : 0) !== ordinal)
      fault("device-renamed", "The parameter device's number changed; run the mirror again");
    return device;
  };
  function validSlot(slot) {
    return slot && integer(slot.index, 0, SLOTS - 1) && typeof slot.name === "string" && slot.name.length > 0 && slot.name.length <= 64 &&
      ["float", "int", "enum"].indexOf(slot.type) >= 0 && finite(slot.min) && finite(slot.max) && slot.min < slot.max &&
      (slot.type !== "enum" || (Array.isArray(slot.choices) && slot.choices.length >= 2 && slot.choices.length <= 128));
  }
  // Hand a slot layout to the device: names, ranges, units, choices and banks
  // are parameter attributes only the device's own patcher can change.
  Session.prototype.configureParameterDevice = function (payload, mark) {
    var track = this.track(payload["track-id"], payload["expected-name"]);
    var device = this.parameterDevice(track, payload["device-id"], payload.ordinal), slots = payload.slots;
    if (!Array.isArray(slots) || slots.length > SLOTS || !slots.every(validSlot)) fault("slots", "Name up to " + SLOTS + " valid parameter slots");
    if (!this.relay) fault("relay-unavailable", "This companion cannot reach parameter devices");
    mark();
    this.relay(Number(device.id), {slots: slots, banks: Array.isArray(payload.banks) ? payload.banks : []});
    return {"device-id": "device:" + Number(device.id), relayed: slots.length};
  };
  // Tolglow's values on the device's dials, clamped to each parameter's range.
  // Watched slots are written through the ids the watch already holds, and a
  // slot already showing the value is skipped. Each written value is kept as
  // the companion's own, so the slot's observer does not report it back
  // (Tolglow also drops the echo; this saves the round trip).
  Session.prototype.setParameterValues = function (payload, mark) {
    var track = this.track(payload["track-id"], payload["expected-name"]), self = this, values = payload.values;
    if (!Array.isArray(values) || values.length > SLOTS * 8) fault("values", "Name up to " + (SLOTS * 8) + " slot values");
    var trackId = Number(track.id), watched = {}, devices = null;
    Object.keys(this.slotWatches).forEach(function (id) {
      var e = self.slotWatches[id];
      if (e.track === trackId) watched[e.slot] = {id: Number(id), entry: e};
    });
    var targets = values.map(function (v) {
      if (!v || !integer(v.device, 0, 7) || !integer(v.index, 0, SLOTS - 1) || !finite(v.value)) fault("value", "A slot value needs a device, an index and a number");
      var w = watched[v.device * SLOTS + v.index];
      if (w) return {id: w.id, entry: w.entry, value: v.value, device: v.device, index: v.index};
      if (!devices) { devices = {}; self.parameterDevices(track).forEach(function (d) { devices[d.ordinal] = d; }); }
      var d = devices[v.device];
      if (!d) fault("parameter-device-missing", "No Tolglow parameters device " + (v.device + 1) + " on that track");
      if (!d.parameters[v.index]) fault("slot-missing", "The parameter device has no slot " + (v.index + 1));
      return {id: Number(self.slotParameters(self.object({id: idOf(d["device-id"], "device")}))[v.index].id), entry: null, value: v.value,
        device: v.device, index: v.index};
    });
    // Live keeps parameter values as 32-bit floats: compare in that precision.
    var written = 0, skipped = 0, own = [];
    targets.forEach(function (t) {
      var parameter = self.object({id: t.id});
      var lo = Number(self.read(parameter, "min")), hi = Number(self.read(parameter, "max")), value = Math.max(lo, Math.min(hi, t.value));
      if (t.entry && Math.fround(t.entry.value) === Math.fround(value)) { skipped++; own.push({device: t.device, index: t.index}); return; }
      if (!written) mark();
      if (t.entry) {
        t.entry.own = (t.entry.own || []).concat([Math.fround(value)]).slice(-16);
        own.push({device: t.device, index: t.index});
      }
      parameter.set("value", value);
      written++;
    });
    // own: slots whose value this write leaves unreported (suppressed or
    // unchanged), so Tolglow stops waiting for their echo.
    return {written: written, skipped: skipped, own: own};
  };
  // Envelope targets of a run clip: the slots of parameter devices on its track.
  Session.prototype.envelopeTargets = function (track, list, length) {
    var self = this;
    if (!Array.isArray(list) || list.length > SLOTS * 8) fault("parameter-envelopes", "Name up to " + (SLOTS * 8) + " slot envelopes");
    return list.map(function (e) {
      if (!e || !integer(e.index, 0, SLOTS - 1) || !Array.isArray(e.points) || !e.points.length || e.points.length > 4096)
        fault("parameter-envelope", "A slot envelope needs a slot index and points");
      var device = self.parameterDevice(track, e["device-id"]), parameter = self.slotParameters(device)[e.index];
      if (!parameter) fault("slot-missing", "The parameter device has no slot " + (e.index + 1));
      var lo = Number(self.read(parameter, "min")), hi = Number(self.read(parameter, "max")), last = -1;
      var points = e.points.map(function (p) {
        if (!Array.isArray(p) || !finite(p[0]) || !finite(p[1]) || p[0] < 0 || p[0] >= length || p[0] <= last) fault("envelope-point", "Envelope points need increasing beats inside the clip");
        last = p[0];
        return {beat: p[0], value: Math.max(lo, Math.min(hi, p[1]))};
      });
      return {parameter: Number(parameter.id), points: points};
    });
  };
  Session.prototype.slotPathByIndex = function (index, slot) { return "live_set tracks " + index + " clip_slots " + slot; };

  Session.prototype.inspectSet = function () {
    var song = this.object("live_set"), self = this;
    this.hostTrackId(true);
    var numerator = Number(this.read(song, "signature_numerator")), denominator = Number(this.read(song, "signature_denominator"));
    var app = this.api("live_app"), version = null;
    if (app && Number(app.id)) version = [app.call("get_major_version"), app.call("get_minor_version"), app.call("get_bugfix_version")].map(scalar).join(".");
    return {tracks: this.trackIds().map(function (id, index) { return self.describeTrack(id, index); }),
      tempo: Number(this.read(song, "tempo")),
      signature: {"beats-per-bar": numerator * 4 / denominator, "bars-per-phrase": 4},
      "can-capture-midi?": !!this.read(song, "can_capture_midi"), "live-version": version};
  };

  Session.prototype.ensureTrack = function (payload, mark) {
    var name = payload.name, self = this;
    if (typeof name !== "string" || !name.length || name.length > 128) fault("track-name", "A capture track needs a name");
    if (payload.kind !== undefined && payload.kind !== "midi") fault("track-kind", "Capture tracks are MIDI tracks");
    if (payload["host?"]) {
      // The host capture track is wherever the companion is now, never a track
      // found by name: after the device moves, the old track is no longer it.
      var hostId = this.hostTrackId(true);
      if (!hostId) fault("no-companion", "The companion is not on a track");
      var host = this.object({id: hostId}), current = String(this.read(host, "name"));
      if (!this.read(host, "has_midi_input")) fault("track-kind", "Place the companion on a MIDI track");
      if (current === name) return {"track-id": "track:" + hostId, "created?": false};
      // Never rename, arm or reroute a musician's track.
      if (!TOLGLOW_NAME.test(current) && !this.emptyTrack(host))
        fault("host-track-in-use", "The companion is on a track with clips or instruments. Put it on a new, empty MIDI track and run setup again");
      mark(); host.set("name", name);
      return {"track-id": "track:" + hostId, "created?": false, "adopted?": true};
    }
    var found = this.trackIds().filter(function (id) {
      var t = self.object({id: id});
      return String(self.read(t, "name")) === name && !self.foldable(t) && !!self.read(t, "has_midi_input");
    });
    if (found.length > 1) fault("duplicate-tracks", "Several MIDI tracks have that name; choose one in Live");
    if (found.length === 1) {
      var existing = {"track-id": "track:" + found[0], "created?": false}, in_ = this.groupOf(this.object({id: found[0]}));
      if (in_) existing["group-track-id"] = "track:" + in_;
      return existing;
    }
    var index = -1, groupId = null;
    if (payload["group-track-id"] !== undefined && payload["group-track-id"] !== null) {
      groupId = idOf(payload["group-track-id"], "track");
      index = this.afterGroup(groupId);
    }
    var before = this.trackIds();
    mark(); this.object("live_set").call("create_midi_track", index);
    var created = this.trackIds().filter(function (id) { return before.indexOf(id) < 0; });
    if (created.length !== 1) fault("track-not-created", "Live did not report exactly one new track; inspect the Set before retrying");
    var track = this.object({id: created[0]});
    track.set("name", name);
    var result = {"track-id": "track:" + created[0], "created?": true};
    if (groupId !== null) { var joined = this.groupOf(track); result["group-track-id"] = joined ? "track:" + joined : null; }
    return result;
  };

  // Where a new member of a Group Track goes, validated before any write:
  // just after the group's last direct member that is not itself a group, or
  // just after the Group Track when it has none. Right after a nested group's
  // members would be ambiguous between the two groups, so it is never chosen.
  Session.prototype.afterGroup = function (groupId) {
    var all = this.trackIds(), self = this, last = all.indexOf(groupId);
    if (last < 0) fault("stale-track", "That Group Track is no longer in the Set");
    if (!this.foldable(this.object({id: groupId}))) fault("not-a-group", "That track is not a Group Track");
    all.forEach(function (id, i) {
      var t = self.object({id: id});
      if (i > last && self.groupOf(t) === groupId && !self.foldable(t)) last = i;
    });
    return last + 1;
  };

  Session.prototype.ensureGroup = function (payload) {
    var name = payload.name, self = this;
    if (typeof name !== "string" || !name.length || name.length > 128) fault("track-name", "A Group Track needs a name");
    var found = this.trackIds().filter(function (id) {
      var t = self.object({id: id});
      return self.foldable(t) && String(self.read(t, "name")) === name;
    });
    if (found.length > 1) fault("duplicate-tracks", "Several Group Tracks have that name; choose one in Live");
    if (found.length === 1) return {"track-id": "track:" + found[0], "created?": false};
    fault("group-creation-unavailable", "Live's API has no documented call that creates a Group Track. Group a track and name the group \"" +
      name + "\", or load the Tolglow group template, then sync again");
  };

  Session.prototype.findRouting = function (track, list, wanted, describe) {
    var options = this.dict(track, list) || [], i;
    for (i = 0; i < options.length; i++) if (describe(options[i])) return options[i];
    fault("routing-unavailable", "Live offers no input routing " + JSON.stringify(wanted));
  };

  Session.prototype.configureTrack = function (payload, mark) {
    var track = this.track(payload["track-id"], payload["expected-name"]), input = payload.input, self = this;
    var monitor = payload.monitor === undefined ? undefined : MONITOR.indexOf(payload.monitor);
    var host = Number(track.id) === this.hostTrackId(true);
    if (monitor !== undefined && monitor < 0) fault("monitor", "Monitoring is in, auto or off");
    if (payload["arm?"] && !this.read(track, "can_be_armed")) fault("not-armable", "This track cannot be armed");
    var type, channel;
    // Resolve the port before the first write, so a missing port changes nothing.
    // The channel list depends on the port, so a missing channel is found only
    // after the port was set and is reported as indeterminate.
    if (input) {
      if (typeof input.port !== "string" || !integer(input.channel, 1, 16)) fault("input", "Input needs a port name and a channel 1-16");
      type = this.findRouting(track, "available_input_routing_types", input.port, function (o) { return o.display_name === input.port; });
    }
    if (type) { mark(); this.setDictionary(track, "input_routing_type", {identifier: type.identifier}); }
    if (input) {
      channel = this.findRouting(track, "available_input_routing_channels", input.channel, function (o) {
        var m = /(\d+)\s*$/.exec(String(o.display_name)); return m && Number(m[1]) === input.channel;
      });
      mark(); this.setDictionary(track, "input_routing_channel", {identifier: channel.identifier});
      if (host) this.feed = {host: Number(track.id), channel: feedOf(input.channel)};
    }
    if (monitor !== undefined) { mark(); track.set("current_monitoring_state", monitor); }
    if (payload["arm?"] !== undefined) { mark(); track.set("arm", payload["arm?"] ? 1 : 0); }
    return {track: this.describeTrack(Number(track.id), this.trackIndex(track))};
  };

  // Combine coarse and fine controller events into 14-bit points per lane.
  function lanePoints(controllers, length) {
    var lanes = {}, byBeat = {}, keys = [];
    controllers.forEach(function (c) {
      if (!finite(c.beat) || c.beat < 0 || c.beat >= length || !integer(c.controller, 0, 127) || !integer(c.value, 0, 127))
        fault("controller", "Invalid controller event");
      var coarse = laneOf(c.controller) ? c.controller : laneOf(c.controller - 32) ? c.controller - 32 : null;
      if (coarse === null) fault("controller-lane", "Controller " + c.controller + " is not a Tolglow lane");
      var key = coarse + "@" + c.beat;
      if (!byBeat[key]) { byBeat[key] = {coarse: coarse, beat: c.beat}; keys.push(key); }
      byBeat[key][c.controller === coarse ? "msb" : "lsb"] = c.value;
    });
    keys.forEach(function (key) {
      var p = byBeat[key];
      (lanes[p.coarse] = lanes[p.coarse] || []).push(p);
    });
    Object.keys(lanes).forEach(function (coarse) {
      var last = {msb: 0, lsb: 0};
      lanes[coarse].sort(function (a, b) { return a.beat - b.beat; });
      lanes[coarse] = lanes[coarse].map(function (p) {
        // A coarse-only edit keeps the previous fine value, as the decoder does.
        var msb = p.msb === undefined ? last.msb : p.msb, lsb = p.lsb === undefined ? last.lsb : p.lsb;
        last = {msb: msb, lsb: lsb};
        return {beat: p.beat, value: (msb * 128 + lsb) / MAX14};
      });
    });
    return lanes;
  }

  // The compact wire form: [{controller: coarse, points: [[beat, v14], ...]}].
  function compactLanes(list, length) {
    var lanes = {};
    if (!Array.isArray(list) || list.length > COARSE.length * 4) fault("lanes", "Lanes must be a bounded list");
    list.forEach(function (lane) {
      if (!lane || !laneOf(lane.controller) || !Array.isArray(lane.points) || lane.points.length > 16384)
        fault("controller-lane", "Controller " + (lane && lane.controller) + " is not a Tolglow lane");
      var points = lanes[lane.controller] = lanes[lane.controller] || [];
      lane.points.forEach(function (p) {
        if (!Array.isArray(p) || !finite(p[0]) || p[0] < 0 || p[0] >= length || !integer(p[1], 0, MAX14))
          fault("controller", "Invalid lane point");
        points.push({beat: p[0], value: p[1] / MAX14});
      });
    });
    Object.keys(lanes).forEach(function (c) { lanes[c].sort(function (a, b) { return a.beat - b.beat; }); });
    return lanes;
  }

  Session.prototype.laneParameters = function () {
    var device = this.object(this.devicePath), self = this, out = {};
    ids(device.get("parameters")).forEach(function (id) {
      var m = /^(?:Tolglow lane|Lane) (\d+)$/.exec(String(self.read(self.object({id: id}), "name")));
      if (m && Number(m[1]) >= 1 && Number(m[1]) <= COARSE.length) out[COARSE[Number(m[1]) - 1]] = id;
    });
    return out;
  };

  Session.prototype.envelope = function (clip, parameterId, create, mark) {
    var found = ids(clip.call("automation_envelope", "id " + parameterId));
    if (found.length) return this.object({id: found[0]});
    if (!create) return null;
    mark();
    found = ids(clip.call("create_automation_envelope", "id " + parameterId));
    if (!found.length) fault("envelope-not-created", "Live did not create the clip envelope");
    return this.object({id: found[0]});
  };

  // Validate notes and lanes for a clip of this length; nothing is written.
  Session.prototype.clipContent = function (track, payload, length) {
    var clip = payload.clip || {}, notes = clip.notes || payload.notes || [];
    if (!Array.isArray(notes) || notes.length > 4096) fault("clip", "Notes must be a bounded list");
    notes.forEach(function (n) {
      if (!integer(n.pitch, 0, 127) || !finite(n.start) || n.start < 0 || !finite(n.duration) || n.duration <= 0 ||
          n.start + n.duration > length + 1e-9 || !integer(n.velocity === undefined ? 127 : n.velocity, 1, 127))
        fault("note", "Invalid clip note");
    });
    var controllers = clip.controllers || [];
    if (!Array.isArray(controllers) || controllers.length > 65536) fault("clip", "Controllers must be a bounded list");
    var lanes = payload.lanes ? compactLanes(payload.lanes, length) : lanePoints(controllers, length);
    var parameters = {}, coarseList = Object.keys(lanes);
    if (coarseList.length) {
      if (Number(track.id) !== this.hostTrackId(true))
        fault("controllers-need-companion", "Controller lanes are written as envelopes of the companion's parameters on its own track");
      var available = this.laneParameters();
      coarseList.forEach(function (coarse) {
        if (!available[coarse]) fault("lane-parameter-missing", "The companion has no parameter for lane controller " + coarse);
        parameters[coarse] = available[coarse];
      });
    }
    return {notes: notes, lanes: lanes, parameters: parameters};
  };

  // Write validated content into a clip. Each lane point becomes an envelope
  // step lasting until the next point of that lane in this request, or to the
  // clip end; a later request's steps overwrite what they cover.
  Session.prototype.fill = function (clip, content, length, mark) {
    var self = this, steps = 0;
    if (content.notes.length) {
      mark();
      this.dictionaryCall(clip, "add_new_notes", {notes: content.notes.map(function (n) {
        return {pitch: n.pitch, start_time: n.start, duration: n.duration, velocity: n.velocity === undefined ? 127 : n.velocity, mute: 0};
      })});
    }
    Object.keys(content.lanes).forEach(function (coarse) {
      var envelope = self.envelope(clip, content.parameters[coarse], true, mark), points = content.lanes[coarse];
      points.forEach(function (p, i) {
        var end = i + 1 < points.length ? points[i + 1].beat : length;
        mark();
        envelope.call("insert_step", p.beat, end - p.beat, p.value);
        steps++;
      });
    });
    return {notes: content.notes.length, "envelope-steps": steps};
  };

  Session.prototype.writeClip = function (payload, mark) {
    var track = this.track(payload["track-id"], payload["expected-name"]), clip = payload.clip || {}, self = this;
    var length = clip["length-beats"];
    if (!finite(length) || length <= 0 || length > 4096) fault("clip-length", "A clip needs a positive length in beats");
    var content = this.clipContent(track, payload, length);
    var envelopes = payload["parameter-envelopes"] ? this.envelopeTargets(track, payload["parameter-envelopes"], length) : [];
    var slotIds = ids(track.get("clip_slots")), slot = payload["slot-index"], newScene = false;
    if (slot === undefined || slot === null) {
      slot = -1;
      slotIds.some(function (id, i) { if (!self.read(self.object({id: id}), "has_clip")) { slot = i; return true; } return false; });
      if (slot < 0 && !payload["create-scene?"]) fault("no-empty-slot", "The capture track has no empty clip slot; add a scene");
      if (slot < 0) newScene = true;
    } else if (!integer(slot, 0, slotIds.length - 1)) fault("slot-index", "No such clip slot");
    if (newScene) {
      mark(); this.object("live_set").call("create_scene", -1);
      slotIds = ids(track.get("clip_slots"));
      slot = slotIds.length - 1;
      if (slot < 0 || this.read(this.object({id: slotIds[slot]}), "has_clip")) fault("scene-not-created", "Live did not add an empty clip slot");
    }
    var clipSlot = this.object({id: slotIds[slot]});
    if (this.read(clipSlot, "has_clip")) fault("slot-occupied", "That clip slot already holds a clip");
    mark();
    clipSlot.call("create_clip", length);
    var created = this.object(this.slotPath(track, slot) + " clip");
    if (payload.name) created.set("name", String(payload.name).slice(0, 128));
    // Run clips play once (Clip.looping); Capture clips keep Live's default.
    if (payload["looping?"] === false) created.set("looping", 0);
    var filled = this.fill(created, content, length, mark), steps = filled["envelope-steps"];
    envelopes.forEach(function (e) {
      var envelope = self.envelope(created, e.parameter, true, mark);
      e.points.forEach(function (p, i) {
        var end = i + 1 < e.points.length ? e.points[i + 1].beat : length;
        mark();
        envelope.call("insert_step", p.beat, end - p.beat, p.value);
        steps++;
      });
    });
    return {"clip-id": "clip:" + Number(created.id), "slot-index": slot, notes: filled.notes, "envelope-steps": steps,
      "scene-created?": newScene};
  };

  // Copy a Session clip on this track into its Arrangement. An overlap or a
  // clip no longer on the track is refused before the write.
  Session.prototype.placeClip = function (payload, mark) {
    var track = this.track(payload["track-id"], payload["expected-name"]), clip = this.clip(payload["clip-id"]), self = this;
    var beat = payload["arrangement-beat"], length = Number(this.read(clip, "length"));
    if (!finite(beat) || beat < 0) fault("placement-range", "Use a nonnegative arrangement beat");
    var onTrack = ids(track.get("clip_slots")).some(function (id, i) {
      return !!self.read(self.object({id: id}), "has_clip") && Number(self.object(self.slotPath(track, i) + " clip").id) === Number(clip.id);
    });
    if (!onTrack) fault("stale-clip", "That clip is no longer on the named track");
    ids(track.get("arrangement_clips")).forEach(function (id) {
      var c = self.object({id: id}), start = Number(self.read(c, "start_time")), end = Number(self.read(c, "end_time"));
      if (!finite(start) || !finite(end) || (start < beat + length && beat < end))
        fault("placement-overlap", "That range of the track's Arrangement already holds a clip");
    });
    mark(); track.call("duplicate_clip_to_arrangement", "id " + Number(clip.id), beat);
    return {"arrangement-beat": beat};
  };

  // Add notes and lane steps to a clip this connection wrote, for phrases too
  // large for one request. The clip must still be on the named track.
  Session.prototype.extendClip = function (payload, mark) {
    var track = this.track(payload["track-id"], payload["expected-name"]), clip = this.clip(payload["clip-id"]);
    var self = this, onTrack = ids(track.get("clip_slots")).some(function (id, i) {
      return !!self.read(self.object({id: id}), "has_clip") && Number(self.object(self.slotPath(track, i) + " clip").id) === Number(clip.id);
    });
    if (!onTrack) fault("stale-clip", "That clip is no longer on the named track");
    var length = Number(this.read(clip, "length"));
    return this.fill(clip, this.clipContent(track, payload, length), length, mark);
  };

  Session.prototype.allClipIds = function () {
    var self = this, out = [];
    this.trackIds().forEach(function (trackId, index) {
      ids(self.object({id: trackId}).get("clip_slots")).forEach(function (slotId, slot) {
        if (self.read(self.object({id: slotId}), "has_clip"))
          out.push({id: Number(self.object(self.slotPathByIndex(index, slot) + " clip").id), "track-id": "track:" + trackId, "slot-index": slot});
      });
    });
    return out;
  };

  Session.prototype.captureMidi = function (payload, mark) {
    var song = this.object("live_set"), destination = CAPTURE_DESTINATION[payload.destination || "auto"];
    if (destination === undefined) fault("capture-destination", "Destination is auto, session or arrangement");
    if (!this.read(song, "can_capture_midi")) fault("nothing-to-capture", "Live has nothing to capture on armed or monitored tracks");
    var before = this.allClipIds().map(function (c) { return c.id; });
    mark(); song.call("capture_midi", destination);
    var clips = this.allClipIds().filter(function (c) { return before.indexOf(c.id) < 0; })
      .map(function (c) { return {"clip-id": "clip:" + c.id, "track-id": c["track-id"], "slot-index": c["slot-index"]}; });
    if (!clips.length) fault("capture-not-observed", "Capture MIDI was invoked but no new Session clip is visible yet");
    return {clips: clips};
  };

  Session.prototype.clip = function (clipId) {
    var clip = this.object({id: idOf(clipId, "clip")});
    if (!this.read(clip, "is_midi_clip")) fault("stale-clip", "That clip is gone or is not a MIDI clip");
    return clip;
  };

  Session.prototype.readClip = function (payload) {
    var clip = this.clip(payload["clip-id"]), self = this, song = this.object("live_set");
    var length = Number(this.read(clip, "length")), step = payload["sample-beats"] || this.sampleBeats;
    if (!finite(step) || step <= 0) fault("sample-beats", "Envelope sampling needs a positive beat step");
    var dict = this.readDictionary(clip.call("get_all_notes_extended")) || {};
    var notes = (dict.notes || []).filter(function (n) { return !n.mute; }).map(function (n) {
      return {pitch: n.pitch, start: n.start_time, duration: n.duration, velocity: n.velocity};
    }).sort(function (a, b) { return a.start - b.start || a.pitch - b.pitch; });
    var controllers = [], parameters = this.laneParameters();
    var v14At = function (envelope, beat) {
      return Math.round(Math.max(0, Math.min(1, Number(scalar(envelope.call("value_at_time", beat))))) * MAX14);
    };
    Object.keys(parameters).map(Number).sort(function (a, b) { return a - b; }).forEach(function (coarse) {
      var envelope = self.envelope(clip, parameters[coarse], false), last, pos, next, lo, hi, mid;
      var stepTicks = Math.max(1, Math.round(step / TICK)), endTick = Math.ceil(length / TICK - 1e-9) - 1;
      var at = function (tick) { return v14At(envelope, tick * TICK); };
      var emit = function (tick, v) {
        controllers.push({beat: tick * TICK, controller: coarse, value: v >> 7}, {beat: tick * TICK, controller: coarse + 32, value: v & 127});
      };
      if (!envelope) return;
      last = at(0); emit(0, last); pos = 0;
      // Scan every `step`; at a change, bisect down to the tick where it begins.
      // A change that reverts within one scan step is not seen.
      while (pos < endTick) {
        next = Math.min(pos + stepTicks, endTick);
        if (at(next) === last) { pos = next; continue; }
        lo = pos; hi = next;
        while (hi - lo > 1) { mid = Math.floor((lo + hi) / 2); if (at(mid) === last) lo = mid; else hi = mid; }
        last = at(hi); emit(hi, last); pos = hi;
      }
    });
    var numerator = Number(this.read(song, "signature_numerator")), denominator = Number(this.read(song, "signature_denominator"));
    return {clip: {version: 1, "length-beats": length, meter: {"beats-per-bar": numerator * 4 / denominator, "bars-per-phrase": 4},
      notes: notes, controllers: controllers}, "sampled-every-beats": step};
  };

  Session.prototype.showClip = function (payload, mark) {
    var clip = this.clip(payload["clip-id"]);
    mark();
    this.object("live_set view").set("detail_clip", "id " + Number(clip.id));
    var appView = this.api("live_app view");
    if (appView && Number(appView.id)) appView.call("focus_view", "Detail/Clip");
    return {"clip-id": payload["clip-id"], view: "Detail/Clip"};
  };

  // Watch Session playback on run tracks: playing_slot_index (documented,
  // observable) changes to a slot when a clip starts and below 0 when the
  // track stops. Each change is emitted once with its own serial. Replaces
  // the previous set of watches; writes nothing to the Set.
  Session.prototype.watchRunTracks = function (payload) {
    var self = this, wanted = payload["track-ids"];
    if (!this.observe || !this.emit) fault("watch-unavailable", "This companion cannot observe track playback");
    if (!Array.isArray(wanted) || wanted.length > 512) fault("track-ids", "Name up to 512 run tracks");
    var tracks = wanted.map(function (id) { return self.track(id); });
    Object.keys(this.watches).forEach(function (id) { self.watches[id].close(); });
    this.watches = {};
    this.hostTrackId();
    tracks.forEach(function (track) {
      var id = Number(track.id), entry = {slot: Number(self.read(track, "playing_slot_index"))};
      entry.close = self.observe(id, "playing_slot_index", function (value) { self.playingChanged(id, value); }, null);
      self.watches[id] = entry;
    });
    return {watching: tracks.length, "watching-slots": this.watchSlots(tracks)};
  };
  Session.prototype.closeSlotWatches = function () {
    var self = this;
    Object.keys(this.slotWatches).forEach(function (id) { self.slotWatches[id].closers.forEach(function (close) { close(); }); });
    this.slotWatches = {};
  };
  // Observe value and automation_state of every slot of the parameter devices
  // on these tracks. Each change is read in the deferred callback and emitted
  // once, with automation_state: 1 is Live automation playing back.
  Session.prototype.watchSlots = function (tracks) {
    var self = this, count = 0;
    this.closeSlotWatches();
    tracks.forEach(function (track) {
      var trackId = Number(track.id);
      self.parameterDevices(track).forEach(function (d) {
        var device = self.object({id: idOf(d["device-id"], "device")});
        self.slotParameters(device).forEach(function (parameter, index) {
          // Seed the known value and state, as the track watch reads
          // playing_slot_index first: LiveAPI's first callback after
          // assigning `property` then reports nothing new.
          var id = Number(parameter.id), entry = {track: trackId, slot: d.ordinal * SLOTS + index,
            value: Number(self.read(parameter, "value")), state: Number(self.read(parameter, "automation_state")), own: [], closers: []};
          entry.reportedState = entry.state;
          var changed = function () { self.slotChanged(id); };
          entry.closers.push(self.observe(id, "value", changed, null));
          entry.closers.push(self.observe(id, "automation_state", changed, null));
          self.slotWatches[id] = entry;
          count++;
        });
      });
    });
    return count;
  };
  Session.prototype.slotChanged = function (parameterId) {
    var entry = this.slotWatches[parameterId];
    if (!entry) return;
    var parameter = this.api({id: parameterId});
    if (!parameter || !Number(parameter.id)) return;
    var value = Number(this.read(parameter, "value")), state = Number(this.read(parameter, "automation_state"));
    if (!finite(value) || !integer(state, 0, 2) || (value === entry.value && state === entry.state)) return;
    var valueChanged = value !== entry.value;
    entry.value = value; entry.state = state;
    // The value this companion last wrote for Tolglow is not reported back.
    // The deferred read sees the newest value, so only the newest write can
    // match; a performer returning to an earlier value is reported.
    var own = entry.own || [];
    if (valueChanged && own.length && own[own.length - 1] === Math.fround(value)) {
      entry.own = [];
      if (state === entry.reportedState) return;
      valueChanged = false;
    } else if (valueChanged) entry.own = [];
    entry.reportedState = state;
    this.emit({type: "parameter-device-value", "track-id": "track:" + entry.track, slot: entry.slot, value: value,
      "automation-state": state, "value-changed?": valueChanged, "watch-session": this.watchSession, serial: ++this.watchSerial});
  };
  Session.prototype.playingChanged = function (trackId, value) {
    var entry = this.watches[trackId], slot = Number(scalar(value)), event;
    if (!entry || !finite(slot) || slot === entry.slot) return;
    entry.slot = slot;
    event = {type: slot >= 0 ? "clip-started" : "clip-stopped", "track-id": "track:" + trackId,
      "watch-session": this.watchSession, serial: ++this.watchSerial};
    if (slot >= 0) {
      var track = this.object({id: trackId}), clip = this.api(this.slotPath(track, slot) + " clip");
      event["slot-index"] = slot;
      event["clip-id"] = clip && Number(clip.id) ? "clip:" + Number(clip.id) : null;
    }
    var song = this.api("live_set");
    if (song && Number(song.id)) event["song-beat"] = Number(this.read(song, "current_song_time"));
    this.emit(event);
  };

  // ---------------------------------------------------------------------
  // Cue launcher (#228): the companion's own track holds a Drum Rack whose
  // pads play a cue page. Track.insert_device, RackDevice.insert_chain and
  // DrumChain.in_note are documented "since Live 12.3"; on an older Live a
  // missing rack or chain is refused and left for the performer. Labels and
  // colors go on chains (Chain.name/color): DrumPad has no color setter and
  // its name is read-only.
  Session.prototype.liveVersion = function () {
    var app = this.api("live_app");
    if (!app || !Number(app.id)) return null;
    return [app.call("get_major_version"), app.call("get_minor_version"), app.call("get_bugfix_version")].map(function (v) { return Number(scalar(v)); });
  };
  Session.prototype.canInsert = function () {
    var v = this.liveVersion();
    return !!v && (v[0] > 12 || (v[0] === 12 && v[1] >= 3));
  };
  // The companion's track and the index of the first Drum Rack after the companion.
  Session.prototype.launcherTrack = function () {
    var hostId = this.hostTrackId(true);
    if (!hostId) fault("no-companion", "The companion is not on a track");
    var track = this.object({id: hostId}), index = this.trackIndex(track), self = this;
    var device = this.api(this.devicePath), deviceIds = ids(track.get("devices"));
    var own = device && Number(device.id) ? deviceIds.indexOf(Number(device.id)) : -1, rack = -1;
    deviceIds.forEach(function (id, i) {
      if (rack < 0 && i > own && !!self.read(self.object({id: id}), "can_have_drum_pads")) rack = i;
    });
    return {id: hostId, track: track, index: index, own: own, rack: rack, devices: deviceIds.length};
  };
  Session.prototype.rackPath = function (where) { return "live_set tracks " + where.index + " devices " + where.rack; };
  function colorDistance(a, b) {
    var d = function (shift) { return ((a >> shift) & 255) - ((b >> shift) & 255); };
    return d(16) * d(16) + d(8) * d(8) + d(0) * d(0);
  }
  Session.prototype.ensureLauncher = function (payload, mark) {
    var name = payload.name, row = payload["scroll-row"];
    if (typeof name !== "string" || !LAUNCHER_NAME.test(name) || name.length > 128)
      fault("track-name", "A launcher track name starts with Tolglow launcher");
    if (row !== undefined && row !== null && !integer(row, 0, 28)) fault("scroll-row", "The pad overview scrolls to rows 0-28");
    var where = this.launcherTrack(), current = String(this.read(where.track, "name")), result = {};
    if (!this.read(where.track, "has_midi_input")) fault("track-kind", "Place the companion on a MIDI track");
    // Adopt only a launcher track, an empty track, or one holding just the
    // companion and a Drum Rack: never take over a musician's instrument or
    // another Tolglow track (Capture's cue track hosts the companion too).
    var self = this, clipless = !ids(where.track.get("arrangement_clips")).length &&
      !ids(where.track.get("clip_slots")).some(function (id) { return !!self.read(self.object({id: id}), "has_clip"); });
    var bare = where.devices <= 1 || (where.rack >= 0 && where.devices === 2);
    if (!LAUNCHER_NAME.test(current) && TOLGLOW_NAME.test(current))
      fault("tolglow-track-in-use", "The companion is on another Tolglow track (" + current + "). One companion hosts either Capture or the cue launcher; put a second companion on a new, empty MIDI track");
    if (!LAUNCHER_NAME.test(current) && !(clipless && bare))
      fault("host-track-in-use", "The companion is on a track with clips or instruments. Put it on a new, empty MIDI track and run setup again");
    if (where.rack < 0) {
      if (!this.canInsert()) fault("drum-rack-creation-unavailable", "This Live cannot insert devices (Track.insert_device needs Live 12.3). Drop a Drum Rack after the companion and run setup again");
      if (where.own < 0) fault("no-companion", "The companion is not in its track's device chain");
      mark(); where.track.call("insert_device", "Drum Rack", where.own + 1);
      where = this.launcherTrack();
      if (where.rack < 0) fault("drum-rack-not-created", "Live did not report a Drum Rack after the companion; inspect the track before retrying");
      result["rack-created?"] = true;
    }
    var rack = this.object(this.rackPath(where));
    if (!this.read(rack, "has_drum_pads")) fault("not-drum-rack", "The Drum Rack after the companion has no pads (a nested Drum Rack?)");
    if (current !== name) { mark(); where.track.set("name", name); result["renamed?"] = true; }
    if (row !== undefined && row !== null) {
      var view = this.object(this.rackPath(where) + " view");
      if (Number(this.read(view, "drum_pads_scroll_position")) !== row) { mark(); view.set("drum_pads_scroll_position", row); }
    }
    result["track-id"] = "track:" + where.id; result["device-index"] = where.rack;
    return result;
  };
  Session.prototype.launcherRack = function (payload) {
    var where = this.launcherTrack();
    if (payload && payload["track-id"] !== undefined) {
      this.track(payload["track-id"], payload["expected-name"]);
      if ("track:" + where.id !== payload["track-id"]) fault("stale-track", "The companion is no longer on that launcher track");
    }
    if (where.rack < 0) fault("no-drum-rack", "No Drum Rack follows the companion; run launcher setup");
    return {where: where, rack: this.object(this.rackPath(where))};
  };
  Session.prototype.padsByNote = function (rack) {
    var self = this, byNote = {};
    ids(rack.get("drum_pads")).forEach(function (id) { var pad = self.object({id: id}); byNote[Number(self.read(pad, "note"))] = pad; });
    return byNote;
  };
  Session.prototype.writePads = function (payload, mark) {
    var pads = payload.pads, self = this;
    if (!Array.isArray(pads) || pads.length > 128) fault("pads", "Name up to 128 pads");
    pads.forEach(function (p) {
      if (!p || !integer(p.note, 0, 127) || typeof p.name !== "string" || !p.name.length || !integer(p.color, 0, 0xffffff))
        fault("pad", "A pad needs a MIDI note, a name and a packed RGB color");
    });
    var found = this.launcherRack(payload), rack = found.rack, byNote = this.padsByNote(rack), insert = this.canInsert();
    var written = [], skipped = [];
    pads.forEach(function (p) {
      var pad = byNote[p.note], chains = pad ? ids(pad.get("chains")) : [], created = false, chain;
      if (!pad) { skipped.push({note: p.note, code: "no-such-pad"}); return; }
      if (!chains.length) {
        if (!insert) { skipped.push({note: p.note, code: "chain-creation-unavailable"}); return; }
        var before = ids(rack.get("chains"));
        mark(); rack.call("insert_chain");
        var added = ids(rack.get("chains")).filter(function (id) { return before.indexOf(id) < 0; });
        if (added.length !== 1) fault("chain-not-created", "Live did not report exactly one new chain; inspect the Drum Rack before retrying");
        chain = self.object({id: added[0]});
        chain.set("in_note", p.note);
        created = true;
      } else chain = self.object({id: chains[0]});
      var name = p.name.slice(0, 128);
      var renamed = String(self.read(chain, "name")) !== name, auto = !!self.read(chain, "is_auto_colored");
      if (renamed) { mark(); chain.set("name", name); }
      if (renamed || auto || colorDistance(Number(self.read(chain, "color")), p.color) > PALETTE_SNAP) {
        mark(); chain.set("is_auto_colored", 0); chain.set("color", p.color);
      }
      written.push({note: p.note, name: String(self.read(chain, "name")), color: Number(self.read(chain, "color")), "chain-created?": created});
    });
    return {pads: written, skipped: skipped, "color-policy": "nearest-live-palette"};
  };
  // Read back the launcher: the companion's track, its Drum Rack, the pad
  // overview's scroll row and every filled pad's first chain. Writes nothing.
  Session.prototype.inspectLauncher = function () {
    var where = this.launcherTrack(), self = this, out = {"track-id": "track:" + where.id, name: String(this.read(where.track, "name")),
      "can-insert?": this.canInsert(), rack: null,
      // Where pad hits come from: Tolglow checks that a performer can reach the companion.
      input: this.routing(where.track), "routed-from-track?": this.routedFromTrack(where.id) !== false,
      "arm?": !!this.read(where.track, "can_be_armed") && !!this.read(where.track, "arm"),
      monitor: MONITOR[Number(this.read(where.track, "current_monitoring_state"))] || null};
    if (where.rack < 0) return out;
    var rack = this.object(this.rackPath(where)), pads = [];
    if (!this.read(rack, "has_drum_pads")) return out;
    ids(rack.get("drum_pads")).forEach(function (id) {
      var pad = self.object({id: id}), chains = ids(pad.get("chains"));
      if (!chains.length) return;
      var chain = self.object({id: chains[0]});
      pads.push({note: Number(self.read(pad, "note")), name: String(self.read(chain, "name")), color: Number(self.read(chain, "color")), chains: chains.length});
    });
    out.rack = {"device-index": where.rack, "scroll-row": Number(this.read(this.object(this.rackPath(where) + " view"), "drum_pads_scroll_position")), pads: pads};
    return out;
  };

  Session.prototype.run = function (op, payload, mark) {
    switch (op) {
      case "inspect-set": return this.inspectSet();
      case "ensure-launcher": return this.ensureLauncher(payload, mark);
      case "write-pads": return this.writePads(payload, mark);
      case "inspect-launcher": return this.inspectLauncher();
      case "configure-parameter-device": return this.configureParameterDevice(payload, mark);
      case "set-parameter-values": return this.setParameterValues(payload, mark);
      case "ensure-track": return this.ensureTrack(payload, mark);
      case "ensure-group": return this.ensureGroup(payload);
      case "watch-run-tracks": return this.watchRunTracks(payload);
      case "place-clip": return this.placeClip(payload, mark);
      case "configure-track": return this.configureTrack(payload, mark);
      case "write-clip": return this.writeClip(payload, mark);
      case "extend-clip": return this.extendClip(payload, mark);
      case "capture-midi": return this.captureMidi(payload, mark);
      case "read-clip": return this.readClip(payload);
      case "show-clip": return this.showClip(payload, mark);
      case "fire-clip": { var clip = this.clip(payload["clip-id"]); mark(); clip.call("fire"); return {"clip-id": payload["clip-id"]}; }
      case "stop-clips": {
        var self = this, tracks = (payload["track-ids"] || []).map(function (id) { return self.track(id); });
        if (tracks.length) mark();
        tracks.forEach(function (t) { t.call("stop_all_clips"); });
        return {};
      }
      default: fault("operation", "Unknown Live Set operation");
    }
  };

  // The echo rule: Tolglow's own feed arrives on the track's feed channel and
  // is dropped. Everything else on the companion's track is clip playback,
  // which carries no MIDI channel in Live, so it is tagged with the source
  // track; Tolglow maps that track to its layout channel.
  Session.prototype.feedChannel = function (host) {
    if (!host) return null;
    if (!this.feed || this.feed.host !== host) {
      var channel = null;
      try { channel = feedOf(this.routing(this.object({id: host})).channel); } catch (e) { channel = null; }
      this.feed = {host: host, channel: channel};
    }
    return this.feed.channel;
  };
  Session.prototype.route = function (event) {
    var host = this.hostTrackId(), feed = this.feedChannel(host);  // cached host, no LiveAPI per event
    if (feed !== null && event.channel === feed) return null;
    if (host) event["track-id"] = "track:" + host;
    event.origin = this.origin();
    return event;
  };

  // An automated lane parameter plays back as the coarse and fine controller pair.
  Session.prototype.laneEvents = function (lane, value) {
    if (!integer(lane, 1, COARSE.length) || !finite(value)) fault("lane", "Unknown lane parameter");
    var v14 = Math.round(Math.max(0, Math.min(1, value)) * MAX14), host = this.hostTrackId(), out = [
      {type: "cc", controller: COARSE[lane - 1], value: v14 >> 7, channel: 1, origin: "live-playback"},
      {type: "cc", controller: COARSE[lane - 1] + 32, value: v14 & 127, channel: 1, origin: "live-playback"}];
    if (host) out.forEach(function (e) { e["track-id"] = "track:" + host; });
    return out;
  };

  return {Session: Session, OPERATIONS: OPERATIONS, COARSE: COARSE, SLOTS: SLOTS, lanePoints: lanePoints};
}));
