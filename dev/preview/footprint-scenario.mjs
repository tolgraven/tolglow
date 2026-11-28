// Synthetic moving rigs for the footprint culling bench and tests (#782).

// Rooms and faked-light counts: the #793 review's big room (128 heads, 8 full)
// and the default room with 64 heads.
export const SCENES = [
  {name: 'bigroom-24x7x20-120', room: [24, 7, 20], lights: 120},
  {name: 'room-12x5x10-56', room: [12, 5, 10], lights: 56},
  {name: 'room-12x5x10-120', room: [12, 5, 10], lights: 120},
];

/** Seeded moving rig: heads on trusses sweeping pan/tilt, zoom 4–30°. */
export function scenario(room, count, frameCount, seed = 7) {
  let state = seed >>> 0;
  const random = () => ((state = (state * 1664525 + 1013904223) >>> 0) / 4294967296);
  const [w, h, d] = room;
  const heads = Array.from({length: count}, (_, i) => ({
    x: (random() - .5) * w * .9, y: h * (.6 + random() * .35), z: (random() - .5) * d * .9,
    pan: random() * Math.PI * 2, tilt: .2 + random() * 1.1, speed: .2 + random() * 1.5,
    half: (2 + random() * 13) * Math.PI / 180, radius: .02 + random() * .06, penumbra: .1 + random() * .4,
    wall: i % 9 === 0}));
  return Array.from({length: frameCount}, (_, frame) => heads.map(head => {
    const pan = head.pan + frame * .02 * head.speed;
    const tilt = head.wall ? 1.35 + .2 * Math.sin(frame * .03) : head.tilt + .3 * Math.sin(frame * .05 * head.speed);
    const direction = [Math.sin(tilt) * Math.cos(pan), -Math.cos(tilt), Math.sin(tilt) * Math.sin(pan)];
    const apexDistance = head.radius / Math.tan(head.half);
    const virtualPosition = [head.x - direction[0] * apexDistance, head.y - direction[1] * apexDistance, head.z - direction[2] * apexDistance];
    return {virtualPosition, apexDistance, direction, outerCos: Math.cos(head.half), innerCos: Math.cos(head.half * (1 - head.penumbra)),
      color: [40 * (1 + Math.sin(pan)), 30, 20 * (1 + Math.cos(tilt))], range: Math.max(...room) * 2.5 + apexDistance};
  }));
}
