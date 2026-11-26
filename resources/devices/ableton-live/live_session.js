/* Live Set operations for automatic setup, Capture, clip writes (#680, #671)
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
    "read-clip", "show-clip", "fire-clip", "stop-clips", "watch-run-tracks", "place-clip"];
  // Envelope read-back: scan at this step, then bisect each change to one
  // Standard MIDI File tick (1/960 beat).
  var TICK = 1 / 960;
  // A track whose name starts with this is Tolglow's to adopt and configure.
  var TOLGLOW_NAME = /^Tolglow(\s|$)/;

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
    this.watchSerial = 0;
    this.watchSession = options.watchSession || (String(new Date().getTime()) + "-" + Math.floor(Math.random() * 1e9));
    // What the companion's own track is playing, for origin marking:
    // {host, slot, closers}; songPlaying and hostArrangement follow the Set.
    this.hostWatch = null;
    this.songPlaying = null;
    this.hostArrangement = null;
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
      }
      this.watchHost();
    }
    return this.host;
  };
  // Follow the companion track's Session playback and the transport, so MIDI
  // it forwards can be told apart: clip playback or a performer's live input.
  Session.prototype.watchHost = function () {
    var self = this, host = this.host;
    if (!this.observe || (this.hostWatch && this.hostWatch.host === host)) return;
    if (this.hostWatch) this.hostWatch.closers.forEach(function (close) { close(); });
    this.hostWatch = null;
    if (!host) return;
    var watch = {host: host, slot: null, closers: []};
    try {
      watch.slot = Number(this.read(this.object({id: host}), "playing_slot_index"));
      watch.closers.push(this.observe(host, "playing_slot_index", function (v) { watch.slot = Number(scalar(v)); }));
      var song = this.api("live_set");
      if (song && Number(song.id)) {
        this.songPlaying = !!this.read(song, "is_playing");
        watch.closers.push(this.observe(Number(song.id), "is_playing", function (v) { self.songPlaying = !!Number(scalar(v)); }));
      }
    } catch (e) { watch.slot = null; }
    this.hostWatch = watch;
  };
  // Live input only when that is known: the track plays no Session clip and
  // no Arrangement clip can be playing. Unknown counts as playback.
  Session.prototype.origin = function () {
    var w = this.hostWatch;
    if (!w || w.slot === null || !finite(w.slot) || w.slot >= 0) return "live-playback";
    if (this.songPlaying === false || this.hostArrangement === false) return "live-input";
    return "live-playback";
  };
  Session.prototype.closeWatches = function () {
    var self = this;
    Object.keys(this.watches).forEach(function (id) { self.watches[id].close(); });
    this.watches = {};
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
        "length-beats": Number(self.read(clip, "length"))});
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
    return result;
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
    var filled = this.fill(created, content, length, mark);
    return {"clip-id": "clip:" + Number(created.id), "slot-index": slot, notes: filled.notes, "envelope-steps": filled["envelope-steps"],
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
      entry.close = self.observe(id, "playing_slot_index", function (value) { self.playingChanged(id, value); });
      self.watches[id] = entry;
    });
    return {watching: tracks.length};
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

  Session.prototype.run = function (op, payload, mark) {
    switch (op) {
      case "inspect-set": return this.inspectSet();
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

  return {Session: Session, OPERATIONS: OPERATIONS, COARSE: COARSE, lanePoints: lanePoints};
}));
