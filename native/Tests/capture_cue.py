"""Operate the real demo show and require acknowledgement before capturing it."""
import json
import sys
import urllib.request
import uuid

origin, cue = sys.argv[1:]
with urllib.request.urlopen(origin + "/api/state", timeout=5) as response:
    snapshot = json.load(response)
command = {"op": "cue/launch", "cue-id": cue, "launch": {"mode": "persistent"},
           "runtime-id": snapshot["runtime-id"], "show-id": snapshot["show-id"],
           "epoch": snapshot["epoch"], "expected-epoch": snapshot["epoch"],
           "command-id": str(uuid.uuid4()), "source": {"type": "repl", "session-id": "native-capture"}}
request = urllib.request.Request(origin + "/api/command", data=json.dumps(command).encode(),
                                 headers={"Content-Type": "application/json"})
with urllib.request.urlopen(request, timeout=10) as response:
    result = json.load(response)
    assert result.get("ack", result)["status"] == "accepted", result
