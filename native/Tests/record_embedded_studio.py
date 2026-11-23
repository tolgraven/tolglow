"""Turn the real on-device Studio XCUITest screenshots into a short demo."""
import json
from pathlib import Path
import shutil
import sys

from PIL import Image

def write_media(output):
    attachments = output / "attachments"
    manifest = json.loads((attachments / "manifest.json").read_text())
    images = {}
    for test in manifest:
        for capture in test["attachments"]:
            label = capture["suggestedHumanReadableName"]
            for step in ("start-1", "running-1", "start-2", "running-2", "closed"):
                if label.startswith(step):
                    images[step] = attachments / capture["exportedFileName"]

    shutil.copyfile(images["running-1"], output / "ipad.png")
    frames = []
    for step in ("start-1", "running-1", "start-2", "running-2", "closed"):
        with Image.open(images[step]) as capture:
            frame = capture.convert("RGB")
        frame.thumbnail((1024, 1024))
        frames.append(frame)
    assert len({frame.tobytes() for frame in frames}) >= 2, "Expected chooser and live show"
    frames[0].save(output / "ipad.gif", save_all=True, append_images=frames[1:], duration=1300, loop=0)


if __name__ == "__main__":
    write_media(Path(sys.argv[1]))
