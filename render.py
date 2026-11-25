import json
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

root = Path(__file__).resolve().parent
trace = json.loads((root / 'trace.json').read_text())
W, H = 1160, 640
font_root = Path('/usr/share/fonts/truetype/dejavu')

def font(size, bold=False, mono=False):
    name = 'DejaVuSansMono' if mono else 'DejaVuSans'
    return ImageFont.truetype(str(font_root / (name + ('-Bold' if bold else '') + '.ttf')), size)

ink = '#f4ecf0'
muted = '#b1a9bd'
rule = '#343246'
accent = '#f18e77'
aqua = '#82dbc8'


def txt(draw, x, y, text, size=15, color=ink, bold=False, mono=False, anchor=None):
    draw.text((x, y), text, font=font(size, bold, mono), fill=color, anchor=anchor)


def xpos(beat):
    return 190 + (beat - 2) * 354


def clip_panel(draw, y, title, notes, frame=None):
    draw.rounded_rectangle((32, y, 702, y + 196), radius=16, fill='#181724', outline=rule)
    txt(draw, 52, y + 15, title, 18, bold=True)
    cue, *samples = notes
    txt(draw, 52, y + 60, f'CUE  /  {cue["pitch"]}', 13, muted, mono=True)
    txt(draw, 52, y + 111, f'LEVEL / {samples[0]["pitch"]}', 13, muted, mono=True)
    for i in range(6):
        beat = 2 + i / 4
        xx = xpos(beat)
        draw.line((xx, y + 50, xx, y + 151), fill=rule)
        txt(draw, xx, y + 162, f'{beat:.2f}', 12, muted, mono=True, anchor='mt')
    draw.rounded_rectangle((xpos(cue['start_time']), y + 59, xpos(cue['start_time'] + cue['duration']), y + 76),
                           radius=4, fill='#c96664')
    for sample in samples:
        xx = xpos(sample['start_time'])
        spotlight = sample['velocity'] == 1
        draw.rounded_rectangle((xx, y + 110, xx + 20, y + 130), radius=4, fill=aqua if spotlight else '#72638d')
        txt(draw, xx + 10, y + 146, str(sample['velocity']), 13, aqua if spotlight else ink, True, True, 'ms')
    if frame:
        xx = xpos(frame['beat'])
        draw.line((xx, y + 44, xx, y + 105), fill=aqua, width=2)
        draw.polygon([(xx-5, y+44), (xx+5, y+44), (xx, y+50)], fill=aqua)


def compose(frame=None):
    pic = Image.new('RGB', (W, H), '#10101a')
    draw = ImageDraw.Draw(pic)
    txt(draw, 32, 29, 'Record and edit a Session cue', 36, bold=True)
    txt(draw, 34, 82, 'Tolglow show owner · captured notes and frame output from a behavioral Live host', 16, muted)
    clip_panel(draw, 125, 'Recorded encoder movement', trace['original'])
    clip_panel(draw, 340, 'Edited Session clip', trace['edited'], frame)
    draw.rounded_rectangle((723, 125, 1128, 536), radius=16, fill='#181724', outline=rule)
    txt(draw, 745, 143, 'Replay through the show owner', 20, bold=True)
    txt(draw, 745, 183, 'Ember cue level', 14, muted)
    if frame:
        txt(draw, 745, 204, f'{frame["level"]:.4f}', 36, aqua, True, True)
        txt(draw, 745, 260, f'Clip beat {frame["beat"]:.3f}  /  {frame["phase"]}', 14, muted, mono=True)
    else:
        txt(draw, 745, 210, 'Beat 2.500  →  2.625', 19, aqua, True, True)
        txt(draw, 745, 250, 'Velocity 96  →  1', 19, aqua, True, True)
    draw.line((745, 299, 1106, 299), fill=rule)
    txt(draw, 745, 320, 'Captured fixture emission', 14, muted)
    for idx in range(8):
        x, y = 747 + (idx % 4) * 91, 354 + (idx // 4) * 75
        draw.rounded_rectangle((x, y, x + 67, y + 52), radius=8, fill='#211e2d', outline=rule)
        rgb = frame['heads'][idx]['emitted-color'] if frame else [0, 0, 0]
        fixture = tuple(int(255 * min(1, max(0, c)) ** 0.45) for c in rgb)
        draw.ellipse((x+22, y+9, x+45, y+32), fill=fixture, outline='#715060')
        txt(draw, x + 33, y+42, str(idx + 1), 11, muted, mono=True, anchor='mm')
    txt(draw, 34, 563, 'Accent recorded its temporary level and the restored level on lift.', 16)
    txt(draw, 34, 591, 'The edited clip replayed while armed and added no notes.', 16, muted)
    txt(draw, 1128, 591, f'{trace["assertions"]} checks passed', 14, aqua, mono=True, anchor='ra')
    return pic

frames = [None] + [f for f in trace['frames'] if f['pitch'] == trace['edit']['pitch'] or f['release']]
folder = root / 'frames'
folder.mkdir(exist_ok=True)
for idx, frame in enumerate(frames):
    compose(frame).save(folder / f'{idx:02d}.png')
compose(next(f for f in frames[1:] if f['beat'] == trace['edit']['beat'])).save(root / 'session-encoder-replay.png')
