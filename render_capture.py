"""Draw the recorded packet values and replay them at 1/32 speed."""

import json
import subprocess
import tempfile
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt


ROOT = Path(__file__).resolve().parent
BACKGROUND = "#101823"
FOREGROUND = "#F2F6FC"
MUTED = "#AEC1D6"
GRID = "#2C4054"
LANES = (
    (26, "#FF887F", "Tracked follower · red"),
    (33, "#77DDD4", "Unrelated lamp · dimmer"),
)
SLOWDOWN = 32
FINAL_HOLD_SECONDS = 1.6


def draw(capture, visible, destination, *, playback=False):
    samples = capture["samples"]
    targets = [sample["at"] - 50 for sample in samples]
    figure = plt.figure(figsize=(9.6, 5.4), dpi=100, facecolor=BACKGROUND)
    axes = figure.add_axes([0.105, 0.205, 0.84, 0.51], facecolor=BACKGROUND)
    axes.set_xlim(144, 332)
    axes.set_ylim(-23, 282)
    axes.set_xticks(targets, [f"{target:g}" for target in targets])
    axes.set_yticks([0, 64, 128, 192, 255])
    axes.grid(axis="y", color=GRID, linewidth=0.8)
    axes.tick_params(length=0, pad=7, labelcolor=MUTED, labelsize=10)
    for border in axes.spines.values():
        border.set_visible(False)
    axes.set_xlabel("Buffered timeline target (ms)", color=MUTED, labelpad=9, fontsize=11)
    axes.set_ylabel("DMX byte", color=MUTED, labelpad=10, fontsize=11)

    figure.text(0.10, 0.911, "Buffered DMX through tracking rejection",
                color=FOREGROUND, fontsize=25, weight="bold")
    figure.text(0.10, 0.856, "Universe 1 · Seven recorded packets · 50 ms presentation latency",
                color=MUTED, fontsize=11)
    for x, (_, color, label) in zip((0.10, 0.33), LANES):
        figure.text(x, 0.785, label, color=color, fontsize=11, weight="medium")
    figure.text(0.58, 0.785, "● Exact endpoint   ◇ Buffered midpoint", color=MUTED, fontsize=10)

    if playback:
        current = samples[visible - 1]
        axes.axvline(targets[visible - 1], color=MUTED, linewidth=1, alpha=0.38)
        kind = "buffered midpoint" if current["weight"] else "exact endpoint"
        figure.text(0.945, 0.077, f'Published at {current["at"]:g} ms · {kind}',
                    color=FOREGROUND, ha="right", fontsize=10)

    for slot, color, _ in LANES:
        for target, sample in zip(targets[:visible], samples[:visible]):
            value = sample["bytes"]["1"][slot]
            midpoint = sample["weight"] > 0
            axes.scatter(target, value, marker="D" if midpoint else "o",
                         facecolors=BACKGROUND if midpoint else color, edgecolors=color,
                         linewidths=1.6, s=52, zorder=3)
            axes.annotate(str(value), (target, value), xytext=(0, 9 if slot == 26 else -15),
                          textcoords="offset points", ha="center", color=color, fontsize=10)

    revisions = f'Tolglow {capture["tolglow"][:8]}  /  Afterglow {capture["afterglow"][:8]}'
    figure.text(0.10, 0.077, revisions, color=MUTED, fontsize=10)
    figure.savefig(destination, facecolor=BACKGROUND)
    plt.close(figure)


def main():
    capture = json.loads((ROOT.parent / "captured.json").read_text())
    samples = capture["samples"]
    assert capture["levels"] == [[sample["bytes"]["1"][slot] for slot, _, _ in LANES]
                                 for sample in samples]
    draw(capture, len(samples), ROOT / "dmx-playback.png")
    with tempfile.TemporaryDirectory(prefix="tolglow-dmx-replay-") as temp:
        directory = Path(temp)
        manifest = ["ffconcat version 1.0"]
        duration = (samples[-1]["at"] - samples[0]["at"]) * SLOWDOWN / 1000 + FINAL_HOLD_SECONDS
        for i, sample in enumerate(samples):
            name = f"packet-{i}.png"
            draw(capture, i + 1, directory / name, playback=True)
            seconds = ((samples[i + 1]["at"] - sample["at"]) * SLOWDOWN / 1000
                       if i + 1 < len(samples) else FINAL_HOLD_SECONDS)
            manifest.extend([f"file '{name}'", f"duration {seconds:g}"])
        manifest.append(f"file '{name}'")
        playlist = directory / "packets.txt"
        playlist.write_text("\n".join(manifest) + "\n")
        subprocess.run([
            "ffmpeg", "-hide_banner", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0",
            "-i", str(playlist), "-vf", "fps=24,format=yuv420p", "-c:v", "libx264", "-threads", "2",
            "-crf", "18", "-t", f"{duration:g}", "-movflags", "+faststart", str(ROOT / "dmx-playback.mp4"),
        ], check=True)


if __name__ == "__main__":
    main()
