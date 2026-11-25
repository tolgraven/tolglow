import json
import subprocess
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parent
WIDTH, HEIGHT = 1040, 620
FONTS = Path('/usr/share/fonts/truetype/dejavu')
REGULAR = FONTS / 'DejaVuSans.ttf'
BOLD = FONTS / 'DejaVuSans-Bold.ttf'
MONO = FONTS / 'DejaVuSansMono.ttf'
NAMES = {
    'first-wait': 'Inverted dimmer waiting',
    'live': 'Inverted dimmer live',
    'blackout': 'Blackout',
    'reopen-wait': 'Reopened and waiting',
    'reopened': 'Reopened and live',
    'withdrawn': 'Inverted addresses retired',
    'reused-wait': 'Normal dimmer waiting',
    'reused': 'Normal dimmer live',
    'inverted-again-wait': 'Inverted dimmer restored and waiting',
    'inverted-again': 'Inverted dimmer restored and live',
    'shutdown': 'Show shutdown',
}


def label(draw, xy, value, size, color, face=REGULAR, anchor='la'):
    draw.text(xy, value, fill=color, font=ImageFont.truetype(str(face), size), anchor=anchor)


def render(packets, active=None):
    image = Image.new('RGB', (WIDTH, HEIGHT), '#101721')
    draw = ImageDraw.Draw(image)
    label(draw, (45, 33), 'Idle output for inverted dimmers', 34, '#ecf2fa', BOLD)
    label(draw, (47, 85), 'Captured DMX bytes across two universes', 18, '#a9bac9')
    for x, value in [(48, 'TIME'), (165, 'OUTPUT STATE'), (676, 'COARSE'), (794, 'FINE'), (906, 'FOG')]:
        label(draw, (x, 133), value, 14, '#b1c4d4', BOLD)
    for index, packet in enumerate(packets):
        if active is not None and packet['at'] > active:
            break
        y = 167 + 35 * index
        selected = active == packet['at']
        if selected:
            draw.rounded_rectangle((35, y - 6, 1004, y + 27), radius=8, fill='#253849')
        elif index % 2 == 0:
            draw.rounded_rectangle((35, y - 6, 1004, y + 27), radius=8, fill='#151f2a')
        coarse, fine, fog = packet['levels']
        stage = packet['stage']
        normal = stage in {'reused-wait', 'reused'}
        off = (coarse == 0 if normal else coarse == 255)
        color = '#77d7c4' if off else '#ffa78b'
        label(draw, (49, y), f"{packet['at']:>3} ms", 16, '#b1c4d4', MONO)
        label(draw, (164, y), NAMES[stage], 16, '#ecf2fa', BOLD if selected else REGULAR)
        label(draw, (714, y), str(coarse), 17, color, MONO, 'ma')
        label(draw, (814, y), str(fine), 17, color, MONO, 'ma')
        label(draw, (925, y), str(fog), 17, '#c2b3f0', MONO, 'ma')
    label(draw, (46, 570), 'Teal · encoded off     Coral · encoded live     Violet · fog off', 15, '#bac6d2')
    label(draw, (46, 594), 'In-memory publisher capture  ·  No fixture connected', 13, '#92a6b8')
    return image


def main():
    data = json.loads((ROOT / 'captured.json').read_text())
    packets = data['ordinary']
    for packet in packets:
        assert packet['levels'] == [packet['bytes'][str(universe)][slot] for universe, slot in data['slots']]
    render(packets).save(ROOT / 'idle-output.png')
    frames_per_second = 24
    with subprocess.Popen(['ffmpeg', '-v', 'error', '-y', '-f', 'rawvideo', '-pixel_format', 'rgb24',
                           '-video_size', f'{WIDTH}x{HEIGHT}', '-framerate', str(frames_per_second),
                           '-i', '-', '-c:v', 'libx264', '-pix_fmt', 'yuv420p', '-movflags', '+faststart',
                           str(ROOT / 'idle-output.mp4')], stdin=subprocess.PIPE) as worker:
        for index, packet in enumerate(packets):
            image = render(packets, packet['at']).tobytes()
            seconds = 1.5 if index == len(packets) - 1 else (packets[index + 1]['at'] - packet['at']) * .024
            for _ in range(round(seconds * frames_per_second)):
                worker.stdin.write(image)
        worker.stdin.close()
        if worker.wait():
            raise RuntimeError('ffmpeg did not encode the recorded packet playback')


if __name__ == '__main__':
    main()
