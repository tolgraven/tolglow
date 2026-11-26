/* Max's low-priority LiveAPI adapter. ES5 syntax intentionally supports [js].
 * Network ownership lives in node_bridge.js; no LiveAPI work occurs there.
 * Dependency injection permits protocol tests without pretending Live is installed.
 */
(function (root, factory) {
  var exported = factory();
  if (typeof module === "object" && module.exports) module.exports = exported;
  else root.TolglowLive = exported;
}(this, function () {
  "use strict";
  function fault(code, message) { var e = new Error(message); e.code = code; throw e; }
  function finite(n) { return typeof n === "number" && isFinite(n); }
  function integer(n, lo, hi) { return finite(n) && Math.floor(n) === n && n >= lo && n <= hi; }
  function scalar(value) { return Array.isArray(value) ? value[0] : value; }
  function ids(value) {
    if (!(Array.isArray(value))) return [];
    var out = [], i;
    for (i = 0; i < value.length; i++) if (typeof value[i] === "number" && value[i] > 0) out.push(value[i]);
    return out;
  }
  function canonical(value) {
    if (value === null || typeof value !== "object") return JSON.stringify(value);
    if (Array.isArray(value)) return "[" + value.map(canonical).join(",") + "]";
    return "{" + Object.keys(value).sort().map(function (key) { return JSON.stringify(key) + ":" + canonical(value[key]); }).join(",") + "}";
  }
  var SESSION_OPERATIONS = ["inspect-set", "ensure-track", "configure-track", "write-clip", "extend-clip", "capture-midi",
    "read-clip", "show-clip", "fire-clip", "stop-clips"];
  function sessionOperation(op) { return SESSION_OPERATIONS.indexOf(op) >= 0; }
  function recordingKey(intent) {
    return canonical([intent["destination-id"], intent["show-id"] || null, intent.epoch, intent["command-id"]]);
  }
  function Adapter(options) {
    if (!options || typeof options.api !== "function" || typeof options.dictionaryCall !== "function" || typeof options.readDictionary !== "function")
      fault("adapter-options", "LiveAPI, dictionary-call and dictionary-reader boundaries are required");
    this.api = options.api; this.parameters = options.parameters; this.dictionaryCall = options.dictionaryCall; this.readDictionary = options.readDictionary;
    this.session = options.session || null;
    this.mode = "live"; this.destination = null; this.pendingNotes = {}; this.history = {}; this.order = [];
    this.historyLimit = options.historyLimit || 2048;
    this.provisionalDuration = options.provisionalDuration === undefined ? 1 / 16 : options.provisionalDuration;
    if (!finite(this.provisionalDuration) || this.provisionalDuration <= 0) fault("duration", "Provisional note duration must be positive beats");
  }
  Adapter.prototype.object = function (path) {
    var object = this.api(path);
    if (!object || !Number(object.id)) fault("live-object-missing", "Live object is not available: " + JSON.stringify(path));
    return object;
  };
  Adapter.prototype.read = function (object, property) { return scalar(object.get(property)); };
  Adapter.prototype.remember = function (key, body, result) {
    if (!this.history[key]) this.order.push(key);
    this.history[key] = {body: body, result: result};
    while (this.order.length > this.historyLimit) delete this.history[this.order.shift()];
  };
  Adapter.prototype.pair = function (payload) {
    var track = payload["track-index"], slot = payload["clip-slot-index"], arrangement = payload["arrangement-index"], rack = payload["rack-device-index"];
    if (!integer(track, 0, 9999) || (slot === undefined) === (arrangement === undefined) || !integer(slot === undefined ? arrangement : slot, 0, 9999) || (rack !== undefined && !integer(rack, 0, 9999)))
      fault("destination", "Choose one Session slot or Arrangement clip on a zero-based track");
    var trackPath = "live_set tracks " + track, clipPath = slot === undefined ? trackPath + " arrangement_clips " + arrangement : trackPath + " clip_slots " + slot + " clip";
    var trackObject = this.object(trackPath), clip = this.object(clipPath);
    if (!this.read(clip, "is_midi_clip")) fault("not-midi", "Choose an existing MIDI clip");
    var trackName = String(this.read(trackObject, "name")), clipName = String(this.read(clip, "name"));
    if ((payload["expected-track-name"] !== undefined && payload["expected-track-name"] !== trackName) ||
        (payload["expected-clip-name"] !== undefined && payload["expected-clip-name"] !== clipName))
      fault("destination-name-mismatch", "Saved destination names no longer match; choose the track and clip explicitly");
    this.destination = {"track-index": track, "clip-slot-index": slot, "arrangement-index": arrangement, "rack-device-index": rack,
      "track-id": Number(trackObject.id), "clip-id": Number(clip.id), "track-path": trackPath, "clip-path": clipPath,
      "track-name": trackName, "clip-name": clipName,
      id: "clip:" + Number(clip.id)};
    this.pendingNotes = {};
    return this.inspect();
  };
  Adapter.prototype.destinationObjects = function () {
    var destination = this.destination;
    if (!destination) fault("unpaired", "Pair this bridge with an existing MIDI clip first");
    var track = this.object(destination["track-path"]), clip = this.object(destination["clip-path"]);
    if (Number(track.id) !== destination["track-id"] || Number(clip.id) !== destination["clip-id"])
      fault("stale-destination", "The paired track or clip was replaced; pair the destination again");
    if (String(this.read(track, "name")) !== destination["track-name"] || String(this.read(clip, "name")) !== destination["clip-name"])
      fault("destination-name-mismatch", "The paired track or clip name changed; pair the destination again");
    return {track: track, clip: clip};
  };
  Adapter.prototype.inspect = function () {
    var song = this.object("live_set"), songBeat = Number(this.read(song, "current_song_time"));
    var numerator = Number(this.read(song, "signature_numerator")), denominator = Number(this.read(song, "signature_denominator"));
    var result = {"protocol-version": 1, mode: this.mode, "destination-id": null,
      "track-armed?": false, "recording?": false, "automation-armed?": !!this.read(song, "session_automation_record"),
      "writable?": false, beat: 0, "song-beat": songBeat,
      clock: {beat: songBeat, bpm: Number(this.read(song, "tempo")), "playing?": !!this.read(song, "is_playing"),
        meter: {"beats-per-bar": numerator * 4 / denominator, "bars-per-phrase": 4},
        "time-signature": {numerator: numerator, denominator: denominator}},
      capabilities: {"observe-parameters?": !!this.parameters, "record-notes?": true, "record-cc?": false, "parameter-note-lanes?": true, "arrangement-placement?": true, "chain-colors?": true,
        "direct-pad-color?": false, "provisional-note-beats": this.provisionalDuration}};
    if (this.destination) {
      var objects = this.destinationObjects(), clip = objects.clip;
      result.destination = this.destination; result["destination-id"] = this.destination.id;
      result["track-armed?"] = !!this.read(objects.track, "arm");
      result["recording?"] = !!this.read(clip, "is_recording") || !!this.read(clip, "is_overdubbing");
      if (this.destination["arrangement-index"] !== undefined) {
        var start = Number(this.read(clip, "start_time")), end = Number(this.read(clip, "end_time"));
        result["recording?"] = !!this.read(song, "record_mode") && result.clock["playing?"] &&
          !!this.read(clip, "is_playing") && songBeat >= start && songBeat < end;
      }
      result["writable?"] = !!this.read(clip, "is_midi_clip");
      result.beat = Number(this.read(clip, "playing_position"));
      result["loop-start"] = Number(this.read(clip, "loop_start"));
      result["loop-end"] = Number(this.read(clip, "loop_end"));
      result["looping?"] = !!this.read(clip, "looping");
    }
    return result;
  };
  Adapter.prototype.record = function (intent) {
    if (intent["protocol-version"] !== 1) fault("protocol-version", "Expected record-event protocol version 1");
    var event = intent.event || {}, type = event.type;
    if (type === "cc") fault("unsupported-cc-recording", "The verified Clip note API cannot write MIDI CC envelopes; use the separate native MIDI recording route");
    if (type !== "note-on" && type !== "note-off") fault("event-type", "Only note-on and note-off clip writes are supported");
    if (!integer(event.note, 0, 127) || !integer(event.channel, 1, 16) || !integer(event.velocity || 0, 0, 127))
      fault("midi-value", "Invalid MIDI note event");
    if (!finite(intent.beat) || intent.beat < 0) fault("clip-beat", "Clip-relative beat must be finite and nonnegative");
    var key = recordingKey(intent), body = canonical(intent), previous = this.history[key];
    if (!intent["command-id"]) fault("command-id", "Clip writes require the accepted command identity");
    if (previous) {
      if (previous.body !== body) fault("command-id-conflict", "The command ID was reused with different recording data");
      if (previous.result.indeterminate) { this.mutated = true; fault("prior-write-indeterminate", "A prior write with this command ID has an uncertain outcome; inspect Live before taking further action"); }
      return previous.result;
    }
    if (!this.destination || intent["destination-id"] !== this.destination.id) fault("stale-destination", "Recording destination does not match the paired clip");
    var status = this.inspect();
    if (!status["track-armed?"] || !status["recording?"] || !status["writable?"])
      fault("not-recording", "The paired MIDI clip must still be recording on an armed track");
    var sample = event["parameter-sample?"] === true;
    if (sample && (type !== "note-on" || !event.velocity))
      fault("not-recording-parameter", "Parameter samples must be positive-velocity notes");
    var clip = this.destinationObjects().clip, noteKey = intent["instance-id"] ? "instance:" + intent["instance-id"] : "note:" + event.channel + "/" + event.note;
    var pending = this.pendingNotes[noteKey], result;
    if (type === "note-on" && event.velocity > 0) {
      var durationBeats = intent["duration-beats"] === undefined ? this.provisionalDuration : intent["duration-beats"];
      if (!finite(durationBeats) || durationBeats <= 0) fault("note-length", "Recorded note duration must be positive beats");
      var note = {pitch: event.note, start_time: intent.beat, duration: durationBeats, velocity: event.velocity, mute: 0};
      this.mutated = true;
      var noteIds = this.dictionaryCall(clip, "add_new_notes", {notes: [note]});
      if (!Array.isArray(noteIds)) noteIds = [noteIds];
      if (noteIds.length !== 1 || !integer(noteIds[0], 0, 9007199254740991)) fault("note-id-result", "Live did not return exactly one added note ID; do not retry the write automatically");
      if (!sample) this.pendingNotes[noteKey] = {id: noteIds[0], note: note, "song-beat": finite(intent["song-beat"]) ? intent["song-beat"] : status["song-beat"]};
      result = {"recorded?": true, "note-id": noteIds[0], "provisional?": !sample && intent["duration-beats"] === undefined, "duration-beats": durationBeats,
        "pairing": sample ? "parameter-lane" : intent["instance-id"] ? "instance-id" : "latest-note"};
    } else {
      if (!pending) fault("unpaired-note-off", "No recorded note-on belongs to this note-off");
      var endBeat = finite(intent["song-beat"]) ? intent["song-beat"] : status["song-beat"];
      var duration = endBeat - pending["song-beat"];
      if (!finite(duration) || duration < 0) fault("transport-discontinuity", "The Live transport moved backwards; preserve the provisional note and pair the destination again");
      var dictionary = this.readDictionary(clip.call("get_notes_by_id", pending.id));
      var notes = dictionary && dictionary.notes;
      if (!(Array.isArray(notes)) || notes.length !== 1 || Number(notes[0].note_id) !== pending.id)
        fault("recorded-note-missing", "The previously added note was removed or could not be read");
      notes[0].duration = Math.max(duration, this.provisionalDuration);
      this.mutated = true;
      this.dictionaryCall(clip, "apply_note_modifications", {notes: notes});
      delete this.pendingNotes[noteKey];
      result = {"recorded?": true, "note-id": pending.id, "provisional?": false, "duration-beats": notes[0].duration};
    }
    this.remember(key, body, result);
    return result;
  };
  Adapter.prototype.placeInArrangement = function (payload) {
    if (!this.destination || this.destination.id !== payload["destination-id"])
      fault("stale-destination", "Select the paired source clip before placing it");
    var objects = this.destinationObjects(), beat = payload["song-beat"];
    var length = Number(this.read(objects.clip, "length")), self = this;
    if (!finite(beat) || beat < 0 || !finite(length) || length <= 0)
      fault("placement-range", "Use a nonnegative arrangement beat and a source clip of known length");
    if (this.inspect()["recording?"]) fault("recording-active", "Finish recording the source clip before placing it");
    ids(objects.track.get("arrangement_clips")).forEach(function (id) {
      var clip = self.object({id: id}), start = Number(self.read(clip, "start_time")), end = Number(self.read(clip, "end_time"));
      if (!finite(start) || !finite(end) || (start < beat + length && beat < end))
        fault("placement-overlap", "Choose an empty arrangement range on the paired track");
    });
    this.mutated = true;
    objects.track.call("duplicate_clip_to_arrangement", "id " + objects.clip.id, beat);
    return {"placed?": true, "source-clip-id": Number(objects.clip.id), "song-beat": beat, "duration-beats": length};
  };
  Adapter.prototype.padPage = function (payload) {
    var pads = payload.pads;
    if (!(Array.isArray(pads)) || pads.length > 64) fault("pad-page", "A pad page contains at most 64 mapped cues");
    if (!this.destination || this.destination["rack-device-index"] === undefined) fault("rack-unpaired", "Pair a Drum Rack device index before mirroring its chains");
    this.destinationObjects();
    var rack = this.object(this.destination["track-path"] + " devices " + this.destination["rack-device-index"]);
    if (!this.read(rack, "has_drum_pads")) fault("not-drum-rack", "The selected device has no drum pads");
    var self = this, byNote = {}, updated = [], skipped = [];
    ids(rack.get("drum_pads")).forEach(function (id) { var pad = self.object({id: id}); byNote[self.read(pad, "note")] = pad; });
    pads.forEach(function (mapping) {
      if (!mapping["cue-id"]) return;
      if (!integer(mapping.note, 0, 127) || !integer(mapping["live-color"], 0, 0xffffff)) fault("pad-value", "Pad metadata needs MIDI pitch and packed RGB color");
      var pad = byNote[mapping.note], chains = pad ? ids(pad.get("chains")) : [];
      if (!chains.length) { skipped.push({note: mapping.note, reason: "empty-pad-no-chain"}); return; }
      chains.forEach(function (id) {
        var chain = self.object({id: id}); self.mutated = true;
        chain.set("is_auto_colored", 0); chain.set("name", String(mapping.label || mapping["cue-id"]).slice(0, 128));
        chain.set("color", mapping["live-color"]);
        updated.push({note: mapping.note, "chain-id": id, color: Number(self.read(chain, "color")), name: String(self.read(chain, "name"))});
      });
    });
    if (payload["clip-color"] !== undefined) {
      if (!integer(payload["clip-color"], 0, 0xffffff)) fault("clip-color", "Clip color must be packed RGB");
      this.mutated = true; this.destinationObjects().clip.set("color", payload["clip-color"]);
    }
    return {updated: updated, skipped: skipped, "color-policy": "nearest-live-palette", "direct-pad-color?": false};
  };
  Adapter.prototype.midi = function (event) {
    if (this.mode !== "lighting" || event.origin === "tolglow-feedback") return null;
    if (!integer(event.channel, 1, 16)) fault("midi-channel", "MIDI channels are 1-based");
    if (event.type === "cc") {
      if (!integer(event.controller, 0, 127) || !integer(event.value, 0, 127)) fault("midi-value", "Invalid MIDI controller event");
    } else if ((event.type !== "note-on" && event.type !== "note-off") || !integer(event.note, 0, 127) || !integer(event.velocity, 0, 127))
      fault("midi-value", "Invalid MIDI note event");
    if (this.session) { event = this.session.route(event); if (!event) return null; }
    event.origin = "live-playback";
    return event;
  };
  Adapter.prototype.lane = function (lane, value) {
    if (this.mode !== "lighting" || !this.session) return [];
    return this.session.laneEvents(lane, value);
  };
  Adapter.prototype.handle = function (request) {
    this.mutated = false;
    try {
      if (!request || typeof request.id !== "string" || !request.id.length) fault("request-id", "A request ID is required");
      var payload = request.payload || {}, result;
      switch (request.op) {
        case "inspect":
          result = this.inspect();
          if (this.parameters && this.parameters.refresh) this.parameters.refresh();
          break;
        case "configure-destination": result = this.pair(payload); break;
        case "describe-selected-parameter": case "watch-parameters":
          if (!this.parameters) fault("parameter-observers-unavailable", "Load the parameter observer script in the device");
          result = request.op === "watch-parameters" ? this.parameters.replace(payload.sources, payload.generation) : this.parameters.selected();
          break;
        case "record-event": result = this.record(payload); break;
        case "place-in-arrangement": result = this.placeInArrangement(payload); break;
        case "pad-page": result = this.padPage(payload); break;
        case "handoff":
          if (payload.mode !== "live" && payload.mode !== "lighting") fault("mode", "Mode must be live or lighting");
          this.mode = payload.mode;
          if (this.parameters) this.parameters.setActive(this.mode === "lighting");
          result = {mode: this.mode, "midi-thru?": this.mode === "live"}; break;
        default:
          if (!sessionOperation(request.op)) fault("operation", "Unknown Live bridge operation");
          if (!this.session) fault("session-unavailable", "Load live_session.js in the companion for setup and Capture");
          var self = this;
          result = this.session.run(request.op, payload, function () { self.mutated = true; });
      }
      return {id: request.id, "protocol-version": 1, status: "executed", result: result};
    } catch (error) {
      if (this.mutated && request && request.op === "record-event" && request.payload && request.payload["command-id"])
        this.remember(recordingKey(request.payload), canonical(request.payload), {indeterminate: true});
      return {id: request && request.id, "protocol-version": 1, status: this.mutated ? "indeterminate" : "rejected",
        error: {code: error.code || "live-api-error", message: error.message || String(error)}};
    }
  };
  return {Adapter: Adapter, scalar: scalar, ids: ids, canonical: canonical};
}));
