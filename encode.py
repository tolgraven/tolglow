import json
from pathlib import Path
import subprocess

from PIL import Image, ImageDraw, ImageFont

root = Path(__file__).resolve().parent
receipts = json.loads((root / 'receipts.json').read_text())
font_path = '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf'
font = ImageFont.truetype(font_path, 18)
small = ImageFont.truetype(font_path, 12)
bg = '#131a14'
text = '#eff5ea'
subtle = '#a8bba7'
movie = []

stages = {
    'push-2': [('preview', 1.4), ('confirm', 2), ('changed', 1.7), ('confirm', 1.5),
               ('pending', 1.5), ('restored', 2), ('back', 0.9)],
    'push-3': [('preview', 1.4), ('confirm', 2), ('pending', 1.5), ('restored', 2), ('back', 0.9)],
}
headings = {'push-2': 'Push 2  ·  Latest named', 'push-3': 'Push 3  ·  Working recovery'}


def card(kind, phase):
    image = Image.new('RGB', (960, 220), bg)
    draw = ImageDraw.Draw(image)
    draw.text((12, 9), headings[kind], font=font, fill=text)
    draw.text((12, 202), 'Captured display  ·  Virtual MIDI  ·  Tolglow ' + receipts['tolglow'][:8], font=small, fill=subtle)
    with Image.open(root / f'{kind}-{phase}.png') as captured:
        image.paste(captured, (0, 35))
    return image


stills = Image.new('RGB', (960, 440), bg)
frames = root / 'video-frames'
frames.mkdir(exist_ok=True)
for index, (kind, phases) in enumerate(stages.items()):
    stills.paste(card(kind, 'restored'), (0, index * 220))
    for phase, duration in phases:
        output = frames / f'{kind}-{phase}.png'
        card(kind, phase).save(output)
        movie.extend([f"file '{output.relative_to(root)}'", f'duration {duration}'])

stills.save(root / 'checkpoint-restore.png')
movie.append(movie[-2])
(root / 'frames.txt').write_text('\n'.join(movie) + '\n')
subprocess.run([
    'ffmpeg', '-hide_banner', '-loglevel', 'error', '-y', '-f', 'concat', '-safe', '0', '-i', 'frames.txt',
    '-bufsize', '1600k', '-c:v', 'libx264', '-r', '24', '-pix_fmt', 'yuv420p', '-movflags',
    '+faststart', 'checkpoint-restore.mp4',
], cwd=root, check=True)
