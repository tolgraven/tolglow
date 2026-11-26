"use strict";
const test = require("node:test");
const assert = require("node:assert/strict");
const http = require("node:http");
const { createBridge, loopbackUrl } = require("../../../resources/devices/ableton-live/node_bridge.js");

function post(port, body, headers = {}) {
  const bytes = Buffer.from(typeof body === "string" ? body : JSON.stringify(body));
  return new Promise((resolve, reject) => {
    const request = http.request({ host: "127.0.0.1", port, path: "/live", method: "POST", headers: { "Content-Type": "application/json", "Content-Length": bytes.length, ...headers } }, response => {
      const chunks = []; response.on("data", x => chunks.push(x));
      response.on("end", () => resolve({ status: response.statusCode, body: JSON.parse(Buffer.concat(chunks)) }));
    }); request.on("error", reject); request.end(bytes);
  });
}
function scoped(body, session = "test-run") {
  return { ...body, payload: { ...(body.payload || {}), "sync-session-id": session, "expected-bridge-id": "tolglow-m4l" } };
}
function testBridge(options) {
  const bridge = createBridge({ ...options, sendToMax: (kind, text) => {
    const request = JSON.parse(text);
    if (request.op === "inspect" && request.payload?.["sync-session-id"] === "test-run")
      bridge.receive({ id: request.id, status: "executed", result: {} });
    else return options.sendToMax(kind, text);
  } });
  return bridge;
}
async function startTestBridge(bridge) {
  const result = await bridge.start({ port: 0 });
  await post(result.port, scoped({ id: "test-handshake", op: "inspect" }));
  return result;
}
function deferred() { let resolve; const promise = new Promise(r => { resolve = r; }); return { promise, resolve }; }

test("actual loopback HTTP waits for Max execution acknowledgement and deduplicates requests", async () => {
  const sent = [], arrived = deferred(); const bridge = testBridge({ sendToMax: (kind, text) => { sent.push(JSON.parse(text)); arrived.resolve(); } });
  const { port } = await startTestBridge(bridge);
  try {
    const first = post(port, scoped({ id: "write-1", op: "record-event", payload: {} })); await arrived.promise;
    const duplicate = post(port, scoped({ id: "write-1", op: "record-event", payload: {} }));
    bridge.receive({ id: "write-1", "protocol-version": 1, status: "executed", result: { "note-id": 7 } });
    assert.equal((await first).body.result["note-id"], 7); assert.equal((await duplicate).body.result["note-id"], 7);
    assert.equal(sent.length, 1);
    assert.equal((await post(port, { id: "write-1", op: "inspect", payload: {} })).status, 409);
  } finally { await bridge.stop(); }
});

test("timeout is indeterminate, duplicate is not retried, and late receipt can resolve it", async () => {
  const sent = []; const bridge = testBridge({ timeoutMs: 25, sendToMax: (_, text) => sent.push(JSON.parse(text)) });
  const { port } = await startTestBridge(bridge);
  try {
    const request = scoped({ id: "slow", op: "record-event", payload: {} });
    assert.equal((await post(port, request)).body.status, "indeterminate");
    assert.equal((await post(port, request)).body.status, "indeterminate"); assert.equal(sent.length, 1);
    bridge.receive({ id: "slow", "protocol-version": 1, status: "executed", result: { "recorded?": true } });
    assert.equal((await post(port, request)).body.status, "executed"); assert.equal(sent.length, 1);
  } finally { await bridge.stop(); }
});

test("loopback boundary rejects origins, wrong media types and oversized bodies", async () => {
  let sent = 0; const bridge = createBridge({ maxBody: 100, sendToMax: () => sent++ }); const { port } = await bridge.start({ port: 0 });
  try {
    assert.equal((await post(port, {}, { Origin: "http://other.example" })).status, 403);
    assert.equal((await post(port, {}, { "Content-Type": "text/plain" })).status, 415);
    assert.equal((await post(port, " ".repeat(101))).status, 413);
    assert.equal((await post(port, "{")).status, 400); assert.equal(sent, 0);
    assert.throws(() => loopbackUrl("http://example.com/api/live"), /loopback/);
    assert.throws(() => loopbackUrl("http://user:secret@127.0.0.1/"), /credentials/);
  } finally { await bridge.stop(); }
});

test("MIDI callbacks reach the real receiving HTTP server in order with unique event IDs", async () => {
  const received = [], firstArrived = deferred(), releaseFirst = deferred();
  const studio = http.createServer((request, response) => {
    const chunks = []; request.on("data", x => chunks.push(x)); request.on("end", async () => {
      received.push(JSON.parse(Buffer.concat(chunks))); if (received.length === 1) { firstArrived.resolve(); await releaseFirst.promise; }
      response.writeHead(200, { "Content-Type": "application/json" }); response.end('{"status":"accepted"}');
    });
  });
  await new Promise(resolve => studio.listen(0, "127.0.0.1", resolve));
  const bridge = createBridge({ sendToMax: (_, text) => bridge.receive({ id: JSON.parse(text).id, status: "executed", result: {} }) });
  const { port } = await bridge.start({ port: 0, callback: "http://127.0.0.1:" + studio.address().port + "/api/live", id: "test-bridge" });
  try {
    await post(port, { id: "handshake", op: "inspect", payload: { "sync-session-id": "run-1", "expected-bridge-id": "test-bridge" } });
    const first = bridge.forward({ kind: "midi", event: { type: "note-on", note: 36, velocity: 127, channel: 1 } });
    await firstArrived.promise;
    const second = bridge.forward({ kind: "midi", event: { type: "note-off", note: 36, velocity: 0, channel: 1 } });
    assert.equal(received.length, 1); releaseFirst.resolve(); await Promise.all([first, second]);
    assert.deepEqual(received.map(x => x.event.type), ["note-on", "note-off"]);
    assert.ok(received.every(x => x["bridge-id"] === "test-bridge" && x["sync-session-id"] === "run-1" && x["protocol-version"] === 1));
    assert.notEqual(received[0]["event-id"], received[1]["event-id"]);
  } finally { releaseFirst.resolve(); await bridge.stop(); await new Promise(resolve => studio.close(resolve)); }
});

test("run-clip playback events are forwarded to the studio like MIDI (#683)", async () => {
  const received = [];
  const studio = http.createServer((request, response) => {
    const chunks = []; request.on("data", x => chunks.push(x)); request.on("end", () => {
      received.push(JSON.parse(Buffer.concat(chunks)));
      response.writeHead(200, { "Content-Type": "application/json" }); response.end('{"accepted":true}');
    });
  });
  await new Promise(resolve => studio.listen(0, "127.0.0.1", resolve));
  const bridge = createBridge({ sendToMax: (_, text) => bridge.receive({ id: JSON.parse(text).id, status: "executed", result: {} }) });
  const { port } = await bridge.start({ port: 0, callback: "http://127.0.0.1:" + studio.address().port + "/api/live", id: "test-bridge" });
  try {
    await post(port, { id: "handshake", op: "inspect", payload: { "sync-session-id": "run-1", "expected-bridge-id": "test-bridge" } });
    const event = { type: "clip-started", "track-id": "track:5", "clip-id": "clip:9", "watch-session": "w1", serial: 1, "song-beat": 32 };
    await bridge.forward({ kind: "run-clip", event });
    assert.equal(received.length, 1);
    assert.equal(received[0].kind, "run-clip");
    assert.deepEqual(received[0].event, event);
    assert.equal(received[0]["sync-session-id"], "run-1");
    await assert.rejects(bridge.forward({ kind: "run-clips", event }), /Unknown Live callback kind/);
  } finally { await bridge.stop(); await new Promise(resolve => studio.close(resolve)); }
});

test("sync handshake rejects wrong bridge and retains the owner of queued callbacks", async () => {
  const received = [], firstArrived = deferred(), releaseFirst = deferred(), sent = [];
  const studio = http.createServer((request, response) => {
    const chunks = []; request.on("data", x => chunks.push(x)); request.on("end", async () => {
      received.push(JSON.parse(Buffer.concat(chunks)));
      if (received.length === 1) { firstArrived.resolve(); await releaseFirst.promise; }
      response.writeHead(200, { "Content-Type": "application/json" }); response.end('{}');
    });
  });
  await new Promise(resolve => studio.listen(0, "127.0.0.1", resolve));
  const bridge = createBridge({ sendToMax: (_, text) => {
    const request = JSON.parse(text); sent.push(request);
    bridge.receive({ id: request.id, status: "executed", result: {} });
  } });
  const { port } = await bridge.start({ port: 0, callback: "http://127.0.0.1:" + studio.address().port + "/api/live" });
  try {
    await assert.rejects(bridge.forward({ kind: "midi", event: {} }), /handshake/);
    assert.deepEqual(await bridge.forward({ kind: "status", status: {} }), { "not-forwarded": "awaiting-sync-handshake" });
    const handshake = id => post(port, { id, op: "inspect", payload: { "sync-session-id": id, "expected-bridge-id": "tolglow-m4l" } });
    assert.equal((await post(port, { id: "wrong", op: "inspect", payload: { "sync-session-id": "bad", "expected-bridge-id": "other" } })).status, 409);
    assert.equal(sent.length, 0);
    assert.equal((await handshake("old-run")).body["sync-session-id"], "old-run");
    const first = bridge.forward({ kind: "midi", event: { type: "note-on" } }); await firstArrived.promise;
    const second = bridge.forward({ kind: "midi", event: { type: "note-off" } });
    await handshake("new-run");
    assert.equal((await post(port, scoped({ id: "stale-write", op: "record-event" }, "old-run"))).status, 409);
    assert.equal((await post(port, { id: "wrong-destination", op: "configure-destination", payload: { "sync-session-id": "new-run", "expected-bridge-id": "other" } })).status, 409);
    assert.equal(sent.length, 2, "only the two inspect handshakes reached Max");
    const third = bridge.forward({ kind: "midi", event: { type: "note-on" } });
    releaseFirst.resolve(); await Promise.all([first, second, third]);
    assert.deepEqual(received.map(x => x["sync-session-id"]), ["old-run", "old-run", "new-run"]);
  } finally { releaseFirst.resolve(); await bridge.stop(); await new Promise(resolve => studio.close(resolve)); }
});

test("late older inspect acknowledgement cannot replace a newer sync owner", async () => {
  const requests = [], arrived = [deferred(), deferred()];
  const bridge = createBridge({ sendToMax: (_, text) => {
    requests.push(JSON.parse(text)); arrived[requests.length - 1].resolve();
  } });
  const { port } = await bridge.start({ port: 0 });
  try {
    const first = post(port, scoped({ id: "old", op: "inspect" }, "old-run")); await arrived[0].promise;
    const second = post(port, scoped({ id: "new", op: "inspect" }, "new-run")); await arrived[1].promise;
    bridge.receive({ id: "new", status: "executed", result: {} }); await second;
    bridge.receive({ id: "old", status: "executed", result: {} }); await first;
    assert.equal((await post(port, scoped({ id: "old-write", op: "record-event" }, "old-run"))).status, 409);
    assert.equal(requests.length, 2);
  } finally { await bridge.stop(); }
});

test("stop resolves outstanding host execution as indeterminate and closes ownership", async () => {
  const arrived = deferred(); const bridge = testBridge({ sendToMax: () => arrived.resolve() }); const { port } = await startTestBridge(bridge);
  const response = post(port, scoped({ id: "stop-me", op: "record-event", payload: {} })); await arrived.promise;
  await bridge.stop(); const stopped = await response;
  assert.equal(stopped.body.status, "indeterminate");
  assert.equal(stopped.status, 504, "a delivered request is not answered with 503, which Tolglow reads as never sent");
  await assert.rejects(bridge.forward({ kind: "midi", event: {} }), /Start the bridge/);
});

test("an asynchronous Max outlet failure has an explicitly uncertain outcome", async () => {
  const bridge = testBridge({ sendToMax: () => Promise.reject(new Error("IPC delivery interrupted")) });
  const { port } = await startTestBridge(bridge);
  try {
    const result = await post(port, scoped({ id: "ipc-failure", op: "record-event", payload: {} }));
    assert.equal(result.body.status, "indeterminate"); assert.equal(result.body.error.code, "max-delivery-unknown");
  } finally { await bridge.stop(); }
});
