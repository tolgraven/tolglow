"""Loopback-only replay of the calibration test's Apple client HTTP boundary."""
import json
from http.server import BaseHTTPRequestHandler, HTTPServer
from statistics import median

state = {"surfaces": [], "rows": [], "step": 0, "phase": None}
class Fixture(BaseHTTPRequestHandler):
    def send(self, obj):
        encoded = json.dumps(obj).encode()
        self.send_response(200); self.send_header("Content-Type","application/json")
        self.send_header("Content-Length", str(len(encoded))); self.end_headers(); self.wfile.write(encoded)
    def do_GET(self):
        identity = {"runtime-id":"simulated-venue","epoch":0}
        if self.path == "/api/calibration":
            self.send({**identity,"show-id":"capture","fixtures":[],"physical-now-ms":0,
                       "output-capable?":False,"saved-surfaces":state["surfaces"]})
        elif self.path == "/api/photometry":
            self.send({**identity,"output-capable?":True,"fixtures":[{"id":"ref","name":"Booth wash","type":"wash","reflectance-capable?":True}],
                       "receivers":[{"id":"scan-booth-wall","revision":"fixture","point":[0,1,-3]}]})
        else: self.send_error(404)
    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        if self.path == "/api/venue-surface":
            assert body["surface-name"]=="booth-wall" and sorted(body["size"])==[2,4]
            state["surfaces"]=[{"name":"booth-wall","revision":"reviewed"}];return self.send({"status":"accepted"})
        if self.path != "/api/photometry": return self.send_error(404)
        op = body["op"]
        if op=="start-surface":
            assert body["fixture-ids"]==["ref"]; state.update(rows=[],step=0,phase=None)
        if op=="step":
            state["step"]+=1;state["phase"]=body["phase"]
        reply = {"status":"accepted","step-index":state["step"],"presentation-generation":0,
                 "fixture-id":"ref","phase":state["phase"],"presented?":True}
        if op=="read-surface":
            assert body["step-index"]==state["step"] and body["sample-index"]==len(state["rows"])
            state["rows"].append(median(body["frame-levels"]))
            if len(state["rows"])==6:
                a,b,c,d,e,f=state["rows"]; ratio=(d-c)/((b-a+f-e)/2)
                reply["fit"]={"estimate":ratio*.18,"relative-to-card":ratio,"range":[ratio*.17,ratio*.19]}
        self.send(reply)
HTTPServer(("127.0.0.1",8396),Fixture).serve_forever()
