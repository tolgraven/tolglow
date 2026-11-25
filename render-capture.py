"""View recorded ArtDMX bytes from the conductor benchmark as a pixel grid."""
import argparse
import math
from pathlib import Path
import subprocess

from PIL import Image, ImageDraw, ImageFont

WIDTH, HEIGHT = 1280, 720
UNIVERSES, PACKET_BYTES, PAYLOAD_OFFSET = 18, 530, 18
PIXELS, MOVERS = 2048, 64
COLORS = {
    "background": "#12171e", "card": "#1d252f", "border": "#354456",
    "primary": "#f6f8fb", "secondary": "#c3cedc", "subtle": "#91a4b9",
    "blue": "#62d7ed", "amber": "#f8c77a",
}
FONT_ROOT = Path("/usr/share/fonts/truetype/dejavu")


def font(size, bold=False, mono=False):
    family = "DejaVuSansMono" if mono else "DejaVuSans"
    filename = f"{family}{'-Bold' if bold else ''}.ttf"
    return ImageFont.truetype(FONT_ROOT / filename, size)


TITLE = font(38, True)
SECTION = font(20, True)
TEXT = font(16)
SMALL = font(13)
DATA = font(15, mono=True)


def frames(path):
    data = path.read_bytes()
    size = PACKET_BYTES * UNIVERSES
    assert data and len(data) % size == 0
    for start in range(0, len(data), size):
        payload = bytearray()
        for universe in range(UNIVERSES):
            offset = start + universe * PACKET_BYTES
            packet = data[offset:offset + PACKET_BYTES]
            assert packet[:8] == b"Art-Net\x00" and packet[8:10] == bytes([0, 0x50])
            assert int.from_bytes(packet[14:16], "little") == universe
            payload.extend(packet[PAYLOAD_OFFSET:])
        yield payload


def emitted(rgbw, dimmer=255):
    r, g, b, w = rgbw
    return tuple(round(min(value + w, 255) * dimmer / 255) for value in (r, g, b))


def panel(draw, box, title, subtitle):
    draw.rounded_rectangle(box, 15, fill=COLORS["card"], outline=COLORS["border"])
    x, y = box[0] + 20, box[1] + 18
    draw.text((x, y), title, font=SECTION, fill=COLORS["primary"])
    draw.text((x, y + 31), subtitle, font=SMALL, fill=COLORS["subtle"])


def swatch(draw, x, y, label, rgbw):
    draw.rounded_rectangle((x, y, x + 37, y + 37), 6, fill=emitted(rgbw), outline=COLORS["border"])
    draw.text((x + 50, y + 1), label, font=TEXT, fill=COLORS["primary"])
    draw.text((x + 50, y + 22), " / ".join(str(value) for value in rgbw[:3]), font=DATA, fill=COLORS["secondary"])


def chart(draw, x, y, samples, current):
    width, height = 1150, 52
    max_cpu = max(25.0, max(sample[0] for sample in samples) * 1.08)
    cutoff_y = y + height * (1 - 25.0 / max_cpu)
    draw.line((x, cutoff_y, x + width, cutoff_y), fill=COLORS["border"], width=2)
    coords = [(x + i * width / (len(samples) - 1), y + height * (1 - s[0] / max_cpu))
              for i, s in enumerate(samples)]
    draw.line(coords, fill=COLORS["blue"], width=1)
    for (px, py), sample in zip(coords, samples):
        if sample[1]:
            draw.ellipse((px - 3, py - 3, px + 3, py + 3), fill=COLORS["amber"])
    px, py = coords[current]
    draw.line((px, y, px, y + height), fill=COLORS["primary"], width=2)
    draw.ellipse((px - 4, py - 4, px + 4, py + 4), fill=COLORS["primary"])
    draw.rectangle((x + width - 51, cutoff_y - 8, x + width, cutoff_y + 8), fill=COLORS["card"])
    draw.text((x + width - 43, cutoff_y - 9), "25 ms", font=SMALL, fill=COLORS["subtle"])


def visual(payload, samples, frame):
    image = Image.new("RGB", (WIDTH, HEIGHT), COLORS["background"])
    draw = ImageDraw.Draw(image)
    draw.text((44, 34), "Musical conductor", font=TITLE, fill=COLORS["primary"])
    draw.text((46, 88), "2,048 RGBW pixels   /   64 moving heads   /   18 ArtDMX universes", font=TEXT, fill=COLORS["secondary"])
    draw.text((992, 55), f"SHOW TICK {samples[frame][2]:04d}", font=DATA, fill=COLORS["primary"])
    draw.text((992, 82), "DECISION TICK" if samples[frame][1] else "BETWEEN DECISIONS", font=DATA,
              fill=COLORS["amber"] if samples[frame][1] else COLORS["blue"])
    panel(draw, (44, 130, 850, 419), "Captured strip pixels", "Display projection from RGBW packet channels")
    for index in range(2, PIXELS):
        x, y = 65 + (index % 100) * 7, 201 + (index // 100) * 9
        start = 4 * index
        draw.rounded_rectangle((x, y, x + 5, y + 6), 2, fill=emitted(payload[start:start+4]))
    panel(draw, (868, 130, 1237, 419), "Moving-head output", "Emitter color and coarse/fine pan and tilt")
    for index in range(MOVERS):
        x, y = 912 + (index % 8) * 40, 211 + (index // 8) * 25
        start = PIXELS * 4 + 12 * index
        lamp = payload[start:start+12]
        rgb = emitted(lamp[:4], lamp[4])
        pan = int.from_bytes(lamp[5:7], "big") / 65535 * 2 * math.pi
        tilt = int.from_bytes(lamp[7:9], "big") / 65535
        length = 6 + tilt * 8
        draw.ellipse((x - 4, y - 4, x + 4, y + 4), fill=rgb, outline=COLORS["border"])
        draw.line((x, y, x + math.cos(pan) * length, y + math.sin(pan) * length),
                  fill=rgb if max(rgb) > 32 else COLORS["border"], width=2)
    panel(draw, (44, 435, 1237, 535), "Highlight probes", "Captured RGB bytes")
    swatch(draw, 475, 467, "Overdriven source", payload[0:4])
    swatch(draw, 806, 467, "Nearby spill receiver", payload[4:8])
    panel(draw, (44, 551, 1237, 683), "Owner CPU per tick", "Shared development host · amber marks conductor decisions")
    chart(draw, 65, 619, samples, frame)
    draw.text((46, 697), "In-memory output replay · 40 Hz logical clock · no physical DMX interface", font=SMALL, fill=COLORS["subtle"])
    return image


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("capture", type=Path)
    parser.add_argument("timing_csv", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    samples = [(float(cpu), decision == "true", int(tick))
               for cpu, decision, tick in (line.split(",") for line in args.timing_csv.read_text().splitlines())]
    captured = list(frames(args.capture))
    assert len(captured) == len(samples)
    still = next(i for i, sample in enumerate(samples) if i > len(samples) // 2 and sample[1])
    visual(captured[still], samples, still).save(args.output / "conductor-capture.png")
    command = ["ffmpeg", "-hide_banner", "-loglevel", "error", "-y", "-f", "rawvideo", "-pixel_format", "rgb24",
               "-video_size", f"{WIDTH}x{HEIGHT}", "-framerate", "40", "-i", "-", "-an", "-c:v", "libx264",
               "-threads", "1", "-crf", "20", "-pix_fmt", "yuv420p", "-movflags", "+faststart",
               str(args.output / "conductor-capture.mp4")]
    with subprocess.Popen(command, stdin=subprocess.PIPE) as encoder:
        for i, payload in enumerate(captured):
            encoder.stdin.write(visual(payload, samples, i).tobytes())
        encoder.stdin.close()
        assert encoder.wait() == 0


if __name__ == "__main__":
    main()
