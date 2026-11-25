import json
import math
import pathlib
import subprocess
import sys

from PIL import Image, ImageDraw, ImageFont

root = pathlib.Path(sys.argv[1])
fixtures = json.loads((root / "fixtures.json").read_text())
trace = json.loads((root / "video-trace.json").read_text())
width, height = 1280, 720
per_frame = len({fixture["universe"] for fixture in fixtures})
regular = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
bold = "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"
mono = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
fonts = {size: ImageFont.truetype(path, count) for size, path, count in [
    ("title", bold, 40), ("section", bold, 23), ("body", regular, 17),
    ("small", regular, 14), ("phase", mono, 25), ("timestamp", mono, 15)]}


def capture_frames(name):
    data = (root / name).read_bytes()
    offset = 0
    current = {}
    while offset < len(data):
        packet = data[offset : offset + 18]
        assert packet[:8] == b"Art-Net\x00"
        length = int.from_bytes(packet[16:18], "big")
        universe = int.from_bytes(packet[14:16], "little") + 1
        current[universe] = data[offset + 18 : offset + 18 + length]
        offset += 18 + length
        if len(current) == per_frame:
            yield current
            current = {}
    assert not current


class Plot:
    def __init__(self):
        camera = [(x + 0.31 * z, 0.65 * z - y) for x, y, z in
                  (fixture["position"] for fixture in fixtures)]
        xa, ya = map(min, zip(*camera))
        xb, yb = map(max, zip(*camera))
        scale = min(490 / (xb - xa), 295 / (yb - ya))
        self.points = [(int(267 + (x - (xa + xb) / 2) * scale),
                        int(163 + (y - (ya + yb) / 2) * scale)) for x, y in camera]
        self.gamma = [round(255 * (i / 255) ** (1 / 2.2)) for i in range(256)]

    def draw(self, canvas, left, top, packets):
        for (x, y), fixture in zip(self.points, fixtures):
            start = fixture["address"] - 1
            channels = fixture["channels"]
            def channel(name):
                offset = start + channels[name] - 1
                return packets[fixture["universe"] + offset // 512][offset % 512]
            red, green, blue, white = (channel(k) for k in
                                        ("red", "green", "blue", "white"))
            rgb = [self.gamma[min(255, v + white)] for v in (red, green, blue)]
            if "dimmer" in channels:
                level = channel("dimmer") / 255
                rgb = [round(v * math.sqrt(level)) for v in rgb]
            color = tuple(max(v, floor) for v, floor in zip(rgb, (21, 27, 42)))
            radius = 3 if "pan" in channels else 2
            canvas.ellipse((left+x-radius, top+y-radius, left+x+radius, top+y+radius), fill=color)


def draw_frame(plot, index, reference, candidate):
    row = trace[index]
    image = Image.new("RGB", (width, height), "#0b101c")
    draw = ImageDraw.Draw(image)
    txt = lambda x, y, body, font, fill="#eef2fa": draw.text((x, y), body, font=fonts[font], fill=fill)
    txt(54, 32, "Beat-locked pulse replay", "title")
    txt(55, 88, f"Captured ArtDMX  ·  {len(fixtures) - 64:,} RGBW pixels  ·  64 moving heads  ·  25 ms logical steps", "body", "#a6b6d0")
    for panel, label, packets in [(54, "Direct waveform evaluation", reference), (654, "Captured pulse cycle", candidate)]:
        draw.rounded_rectangle((panel, 132, panel + 572, 507), radius=18, fill="#111a2c", outline="#2a3851")
        txt(panel + 24, 152, label, "section")
        plot.draw(draw, panel + 19, 184, packets)

    identical = reference == candidate
    txt(55, 530, "ARTDMX MATCH" if identical else "ARTDMX DIFFERENCE", "body", "#69e5b9" if identical else "#fc836e")
    txt(55, 563, f"{index + 1:03}/{len(trace)} frames", "timestamp", "#a6b6d0")
    txt(349, 530, f"{row['beat']:06.3f} beats", "phase")
    txt(349, 567, f"{row['bpm']:.0f} BPM   ·   {row['clock-source']}", "small", "#a6b6d0")
    plans = row["plans"]
    txt(657, 530, f"{plans['captured']} captured pulses", "phase")
    txt(657, 567, f"{plans['hits']} retained  ·  {plans['misses']} rebuilt  ·  {plans['direct']} direct evaluator", "small", "#a6b6d0")
    txt(1046, 538, f"Master {row['master']:.0%}", "body")
    txt(1046, 567, "Blackout active" if row["blackout"] else "Lighting active", "small", "#a6b6d0")

    x0, x1 = 56, 1226
    y = 626
    x = lambda i: x0 + (x1 - x0) * i / (len(trace) - 1)
    draw.line((x0, y, x1, y), fill="#2a3851", width=5)
    draw.line((x0, y, x(index), y), fill="#5de6b6", width=5)
    for location, title in [(30, "Duty and phase"), (45, "Tempo"), (62, "Meter"), (75, "Backward seek"), (90, "Master"), (110, "Blackout")]:
        if location < len(trace):
            px = x(location)
            draw.ellipse((px-4, y-4, px+4, y+4), fill="#e2ecfa")
            txt(px - len(title) * 3.5, y + (14 if location % 2 else 37), title, "small", "#c3d2e8")
    return image


plot = Plot()
args = ["ffmpeg", "-hide_banner", "-loglevel", "error", "-y", "-f", "rawvideo",
        "-pix_fmt", "rgb24", "-s", f"{width}x{height}", "-r", "40", "-i", "-",
        "-an", "-c:v", "libx264", "-pix_fmt", "yuv420p", "-movflags", "+faststart",
        str(root / "pulse-cycle-replay.mp4")]
encoder = subprocess.Popen(args, stdin=subprocess.PIPE)
reference = capture_frames("reference.artdmx")
replay = capture_frames("replay.artdmx")
for i, (direct, captured) in enumerate(zip(reference, replay, strict=True)):
    frame = draw_frame(plot, i, direct, captured)
    if i == 85:
        frame.save(root / "pulse-cycle-replay.png")
    encoder.stdin.write(frame.tobytes())
encoder.stdin.close()
assert encoder.wait() == 0
