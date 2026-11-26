"use strict";
// Node for Max owns network I/O only. LiveAPI calls run in the low-priority js object.
const http = require("http");
const crypto = require("crypto");
const { canonical } = require("./live_api.js");

function loopbackUrl(value) {
  const url = new URL(value);
  if (url.protocol !== "http:" || !["127.0.0.1", "localhost", "[::1]"].includes(url.hostname) || url.username || url.password)
    throw new Error("The studio callback must be an HTTP loopback URL without credentials");
  return url;
}
function createBridge({ sendToMax, log = () => {}, timeoutMs = 3000, maxBody = 65536, historyLimit = 1024, maxPending = 32 } = {}) {
  if (typeof sendToMax !== "function") throw new Error("sendToMax is required");
  let server = null, studioUrl = null, bridgeId = "tolglow-m4l", outgoing = false, activeCallback = null, eventCounter = 0;
  let syncSessionId = null, handshakeCounter = 0, adoptedHandshake = 0;
  const processId = crypto.randomBytes(12).toString("hex");
  const requests = new Map(), history = new Map(), events = [];
  const reply = (response, code, value) => {
    if (response.destroyed || response.writableEnded) return;
    response.writeHead(code, { "Content-Type": "application/json", "Cache-Control": "no-store", "X-Content-Type-Options": "nosniff" });
    response.end(JSON.stringify(value));
  };
  function remember(id, body, result, handshake) {
    history.delete(id); history.set(id, { body, result, handshake });
    while (history.size > historyLimit) history.delete(history.keys().next().value);
  }
  function receive(value) {
    let result = typeof value === "string" ? JSON.parse(value) : value;
    if (!result || typeof result.id !== "string" || !["executed", "rejected", "indeterminate"].includes(result.status)) throw new Error("Malformed Max execution response");
    const request = requests.get(result.id), previous = history.get(result.id);
    if (!request && !previous) return false; // A late response from an unrelated session has no owner.
    const owner = request || previous, body = owner.body, handshake = owner.handshake;
    if (result.status === "executed" && handshake && handshake.order >= adoptedHandshake) {
      syncSessionId = handshake.id; adoptedHandshake = handshake.order;
    }
    const ownerSession = handshake ? handshake.id : ((JSON.parse(body).payload || {})["sync-session-id"] || null);
    result = { ...result, "bridge-id": bridgeId, "sync-session-id": ownerSession };
    remember(result.id, body, result, handshake);
    if (request) {
      clearTimeout(request.timer); requests.delete(result.id);
      for (const response of request.responses) reply(response, 200, result);
    }
    return true;
  }
  function handle(request, response) {
    if (request.headers.origin) return reply(response, 403, { status: "rejected", error: "Browser-origin requests are not accepted" });
    if (request.method !== "POST" || request.url !== "/live") return reply(response, 404, { status: "rejected", error: "POST /live is the only bridge endpoint" });
    if (!/^application\/json(?:;|$)/i.test(request.headers["content-type"] || "")) return reply(response, 415, { status: "rejected", error: "Use application/json" });
    let size = 0, chunks = [], oversized = false;
    request.on("data", chunk => {
      size += chunk.length;
      if (size > maxBody) { oversized = true; chunks = []; reply(response, 413, { status: "rejected", error: "Request body is too large" }); }
      else if (!oversized) chunks.push(chunk);
    });
    request.on("end", () => {
      if (oversized) return;
      let envelope;
      try { envelope = JSON.parse(Buffer.concat(chunks).toString("utf8")); }
      catch (_) { return reply(response, 400, { status: "rejected", error: "Malformed JSON" }); }
      if (!envelope || typeof envelope.id !== "string" || !envelope.id.length || envelope.id.length > 256 || typeof envelope.op !== "string")
        return reply(response, 400, { status: "rejected", error: "Request id and op are required" });
      const payload = envelope.payload || {};
      if (Object.prototype.hasOwnProperty.call(payload, "expected-bridge-id") && payload["expected-bridge-id"] !== bridgeId)
        return reply(response, 409, { id: envelope.id, status: "rejected", error: "Configured bridge ID does not match this device" });
      const session = payload["sync-session-id"];
      if (envelope.op === "inspect" && session !== undefined &&
          (typeof session !== "string" || !/^[A-Za-z0-9_.:-]{1,160}$/.test(session) || payload["expected-bridge-id"] !== bridgeId))
        return reply(response, 400, { id: envelope.id, status: "rejected", error: "A sync handshake needs a valid session ID and matching expected bridge ID" });
      if (envelope.op !== "inspect" && (!syncSessionId || session !== syncSessionId || payload["expected-bridge-id"] !== bridgeId))
        return reply(response, 409, { id: envelope.id, status: "rejected", error: "Operation does not belong to the current bridge sync session" });
      const body = canonical(envelope), cached = history.get(envelope.id), pending = requests.get(envelope.id);
      if ((cached && cached.body !== body) || (pending && pending.body !== body))
        return reply(response, 409, { id: envelope.id, status: "rejected", error: "Request ID reused with different data" });
      if (cached) return reply(response, 200, cached.result);
      if (pending) { pending.responses.push(response); return; }
      if (requests.size >= maxPending) return reply(response, 503, { status: "rejected", error: "The Max execution queue is full" });
      const handshake = envelope.op === "inspect" && session !== undefined ? { id: session, order: ++handshakeCounter } : null;
      const entry = { body, responses: [response], timer: null, handshake };
      entry.timer = setTimeout(() => {
        requests.delete(envelope.id);
        const result = { id: envelope.id, "protocol-version": 1, status: "indeterminate", error: { code: "max-timeout", message: "Max did not acknowledge in time; the write may have executed. Do not retry with a new ID." } };
        remember(envelope.id, body, result, handshake);
        for (const client of entry.responses) reply(client, 504, result);
      }, timeoutMs);
      requests.set(envelope.id, entry);
      const deliveryError = error => receive({ id: envelope.id, "protocol-version": 1, status: "indeterminate", error: { code: "max-delivery-unknown", message: error.message } });
      try { Promise.resolve(sendToMax("request", JSON.stringify(envelope))).catch(deliveryError); }
      catch (error) { deliveryError(error); }
    });
    request.on("error", error => log(error.message));
  }
  async function start({ port = 8750, callback = "http://127.0.0.1:8500/api/live", id = "tolglow-m4l" } = {}) {
    if (server) throw new Error("Bridge is already started");
    if (!Number.isInteger(port) || port < 0 || port > 65535) throw new Error("Invalid loopback port");
    if (typeof id !== "string" || !/^[A-Za-z0-9_.:-]{1,80}$/.test(id)) throw new Error("Invalid bridge ID");
    studioUrl = loopbackUrl(callback); bridgeId = id; syncSessionId = null;
    adoptedHandshake = ++handshakeCounter;
    const candidate = http.createServer(handle);
    candidate.requestTimeout = timeoutMs; candidate.headersTimeout = timeoutMs;
    await new Promise((resolve, reject) => { candidate.once("error", reject); candidate.listen(port, "127.0.0.1", resolve); });
    server = candidate;
    return { host: "127.0.0.1", port: server.address().port, path: "/live", "bridge-id": bridgeId };
  }
  function pump() {
    if (outgoing || !events.length || !server) return;
    outgoing = true;
    const entry = events.shift(), body = Buffer.from(JSON.stringify(entry.body));
    const request = http.request(studioUrl, { method: "POST", headers: { "Content-Type": "application/json", "Content-Length": body.length } }, response => {
      let size = 0, chunks = [];
      response.on("data", chunk => { size += chunk.length; if (size > maxBody) request.destroy(new Error("Studio response exceeds the bridge limit")); else chunks.push(chunk); });
      response.on("end", () => {
        if (response.statusCode < 200 || response.statusCode >= 300) entry.reject(new Error("Studio callback returned HTTP " + response.statusCode));
        else { try { entry.resolve(JSON.parse(Buffer.concat(chunks).toString("utf8") || "{}")); } catch (error) { entry.reject(error); } }
        finish();
      });
    });
    let finished = false;
    function finish() { if (finished) return; finished = true; outgoing = false; activeCallback = null; pump(); }
    activeCallback = request;
    request.setTimeout(timeoutMs, () => request.destroy(new Error("Studio callback timed out; it is not retried automatically")));
    request.on("error", error => { entry.reject(error); finish(); });
    request.end(body);
  }
  function forward(event) {
    if (!server && event && event.kind === "status") return Promise.resolve({ "not-forwarded": "bridge-stopped" });
    if (!server) return Promise.reject(new Error("Start the bridge before forwarding Live input"));
    if (!event || !["midi", "run-clip", "status", "handoff", "parameter", "parameter-error"].includes(event.kind)) return Promise.reject(new Error("Unknown Live callback kind"));
    if (!syncSessionId && event.kind === "status") return Promise.resolve({ "not-forwarded": "awaiting-sync-handshake" });
    if (!syncSessionId) return Promise.reject(new Error("A successful inspect sync handshake is required before forwarding Live input"));
    // Capture ownership now. Queued callbacks are never rebound to a newer run.
    const body = { ...event, "bridge-id": bridgeId, "sync-session-id": syncSessionId,
      "protocol-version": 1, "event-id": processId + ":" + (++eventCounter) };
    if (Buffer.byteLength(JSON.stringify(body)) > maxBody) return Promise.reject(new Error("Live event is too large"));
    return new Promise((resolve, reject) => {
      if (event.kind === "status") {
        const index = events.findIndex(item => item.body.kind === "status" && item.body["sync-session-id"] === syncSessionId);
        if (index >= 0) { const old = events[index]; events[index] = { body, resolve, reject }; old.resolve({ coalesced: true }); return; }
      }
      const last = events[events.length - 1];
      if (event.kind === "parameter" && last && last.body.kind === "parameter" &&
          last.body["sync-session-id"] === syncSessionId && last.body.sample && body.sample &&
          last.body.sample.generation === body.sample.generation && last.body.sample.id === body.sample.id) {
        events[events.length - 1] = {body, resolve, reject};
        last.resolve({coalesced: true});
        return;
      }
      if (events.length >= 128) return reject(new Error("Studio event queue is full; reconnect and release held lighting instances"));
      events.push({ body, resolve, reject }); pump();
    });
  }
  async function stop() {
    const current = server; server = null; syncSessionId = null;
    adoptedHandshake = ++handshakeCounter;
    if (activeCallback) activeCallback.destroy(new Error("Bridge stopped"));
    while (events.length) events.shift().reject(new Error("Bridge stopped"));
    for (const [id, pending] of requests) {
      clearTimeout(pending.timer);
      const result = { id, "protocol-version": 1, status: "indeterminate", error: { code: "bridge-stopped", message: "Bridge stopped before execution acknowledgement" } };
      remember(id, pending.body, result, pending.handshake);
      // 504, not 503: the request already reached Max, so this is not a refusal.
      for (const response of pending.responses) reply(response, 504, result);
    }
    requests.clear();
    if (current) await new Promise(resolve => current.close(resolve));
  }
  return { start, stop, receive, forward };
}
module.exports = { createBridge, loopbackUrl };

if (require.main === module) {
  const max = require("max-api");
  const bridge = createBridge({ sendToMax: (...args) => max.outlet(...args), log: text => max.post(text) });
  const report = error => max.outlet("diagnostic", error.message);
  max.addHandler("start", (port, callback, id) => bridge.start({ port: Number(port || 8750), callback, id }).then(value => max.outlet("ready", JSON.stringify(value))).catch(report));
  max.addHandler("stop", () => bridge.forward({ kind: "handoff", mode: "live" })
    .catch(report).then(() => bridge.stop()).then(() => max.outlet("stopped")).catch(report));
  max.addHandler("response", value => { try { bridge.receive(value); } catch (error) { report(error); } });
  max.addHandler("event", value => { try { bridge.forward(JSON.parse(value)).catch(report); } catch (error) { report(error); } });
  process.on("SIGTERM", () => bridge.stop().finally(() => process.exit(0)));
  process.on("SIGINT", () => bridge.stop().finally(() => process.exit(0)));
}
