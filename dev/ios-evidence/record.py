"""Drive the installed SwiftUI app on CoreSimulator. Each stage waits for an observed state."""
import os, signal, subprocess as sp, sys, time
from pathlib import Path
mode, udid = sys.argv[1:]
out=Path("native-media");out.mkdir(exist_ok=True)
bundle="org.tolglow.ios"
def sim(*args, **kw):return sp.run(["xcrun","simctl",*args],check=True,**kw)
def output(*args):return sp.check_output(["xcrun","simctl",*args],text=True).strip()
sim("launch",udid,bundle,env={**os.environ,"SIMCTL_CHILD_TOLGLOW_CAPTURE":mode})
doc=Path(output("get_app_container",udid,bundle,"data"))/"Documents"
doc.mkdir(exist_ok=True)
def step(cmd, expect=None, delay=.75):
    stage=doc/"stage"; tmp=doc/"stage.tmp";tmp.write_text(cmd);tmp.replace(stage)
    until=time.monotonic()+75
    while time.monotonic()<until:
        value=(doc/"observed").read_text() if (doc/"observed").exists() else ""
        if not stage.exists() and (not expect or expect in value):
            print(cmd,value,flush=True);time.sleep(delay);return
        time.sleep(.15)
    raise RuntimeError(f"Timed out after {cmd}. Last state {value}")
def screenshot(name):
    sim("io",udid,"screenshot",str(out/(name+".png")))
rec=None
try:
    time.sleep(2)
    step("connect","connected=true" if mode=="plane" else "receivers=1")
    if mode=="meter":step("select")
    rec=sp.Popen(["xcrun","simctl","io",udid,"recordVideo","--codec=h264",str(out/(mode+".mp4"))])
    time.sleep(1.8)
    if mode=="plane":
        step("camera",delay=1.8);screenshot("plane-before");step("measure","measured=true",delay=2.2);screenshot("plane-review")
        step("save","Saved scan-booth-wall",delay=1.8);screenshot("plane-saved")
    else:
        step("arm","running=true",delay=1.0)
        for i in range(1,4):
            step("sample",f"pairs={i} busy=false",delay=1.3)
        step("fit",delay=2);screenshot("meter-review")
        step("save","Measured relative diffuse return saved",delay=1)
        step("result",delay=.9)
except BaseException:
    try: screenshot(mode + "-diagnostic")
    except Exception: pass
    raise
finally:
    if rec:
        rec.send_signal(signal.SIGINT);rec.wait(timeout=35)
    sim("terminate",udid,bundle)
