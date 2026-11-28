import test from 'node:test';
import assert from 'node:assert/strict';
import {footprintTileLights, stageBounds, footprintTileSize, footprintTilePlanes, cullFootprintTiles} from '../../../js/preview/footprints.js';
import {SCENES, scenario} from '../../../dev/preview/footprint-scenario.mjs';
import {stageDimensions} from '../../../js/preview/room.js';

const light = (position, direction, degrees = 10) => {
  const n = Math.hypot(...direction);
  return {virtualPosition: position, direction: direction.map(v => v / n), outerCos: Math.cos(degrees * Math.PI / 180), range: 30};
};

test('footprint tiles receive only lights whose cones can reach them', () => {
  const room = [12, 5, 10], stage = stageBounds(room);
  const lists = footprintTileLights([
    light([0, 3.5, 2], [0, -1, 0]),            // straight down onto open floor
    light([0, 3.5, 0], [0, -.2, -1]),           // at the back wall
    light([-4, 3.5, 0], [-1, -.1, 0]),          // at the left wall
    light([0, 3.5, -3.6], [0, -1, .02], 6),     // down onto the stage top
    light([0, 3.5, 2], [0, 1, 0]),              // up at the ceiling: no tile
  ], room, stage);
  // Tiles: floor, back, left, right, stage top, stage front.
  assert.ok(lists[0].includes(0)); assert.ok(!lists[1].includes(0) && !lists[2].includes(0) && !lists[3].includes(0));
  assert.ok(lists[1].includes(1)); assert.ok(!lists[2].includes(1) && !lists[3].includes(1));
  assert.ok(lists[2].includes(2)); assert.ok(!lists[3].includes(2) && !lists[1].includes(2));
  assert.ok(lists[4].includes(3)); assert.ok(!lists[5].includes(3));
  assert.ok(lists.every(list => !list.includes(4)));
  // A light behind a wall's lit side never reaches that wall.
  assert.deepEqual(footprintTileLights([light([7, 3, 0], [-1, 0, 0])], room, stage)[3], []);
});

test('stage tiles follow the stage geometry and the atlas scales with the room', () => {
  for (const room of [[12, 5, 10], [24, 7, 20], [6, 3, 4]]) {
    const {width, height, depth, z} = stageDimensions(room);
    assert.deepEqual(stageBounds(room), [width / 2, height, z - depth / 2, z + depth / 2]);
  }
  assert.equal(footprintTileSize([12, 5, 10]), 256);
  assert.equal(footprintTileSize([24, 7, 20]), 512);
});

// The culling before it was made allocation-free (#793 head), kept verbatim as
// the reference the per-tile lists must match.
function referenceTileLights(lights, [w, h, d], [hw, top, back, front]) {
  const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  const planes = [
    {origin: [-w / 2, 0, -d / 2], u: [w, 0, 0], v: [0, 0, d], normal: [0, 1, 0]},
    {origin: [-w / 2, 0, -d / 2], u: [w, 0, 0], v: [0, h, 0], normal: [0, 0, 1]},
    {origin: [-w / 2, 0, -d / 2], u: [0, 0, d], v: [0, h, 0], normal: [1, 0, 0]},
    {origin: [w / 2, 0, -d / 2], u: [0, 0, d], v: [0, h, 0], normal: [-1, 0, 0]},
    {origin: [-hw, top, back], u: [2 * hw, 0, 0], v: [0, 0, front - back], normal: [0, 1, 0]},
    {origin: [-hw, 0, front], u: [2 * hw, 0, 0], v: [0, top, 0], normal: [0, 0, 1]},
  ], lists = planes.map(() => []);
  lights.forEach((light, index) => {
    const axis = light.direction, apex = light.virtualPosition;
    const helper = Math.abs(axis[1]) < .95 ? [0, 1, 0] : [1, 0, 0];
    const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
    const p = cross(helper, axis), pl = Math.hypot(...p), e1 = p.map(v => v / pl), e2 = cross(axis, e1);
    const sine = Math.sqrt(Math.max(0, 1 - light.outerCos ** 2)), cosine = light.outerCos;
    const rays = [axis];
    for (let k = 0; k < 8; k++) {
      const a = k * Math.PI / 4, c = Math.cos(a) * sine, s = Math.sin(a) * sine;
      rays.push([0, 1, 2].map(i => axis[i] * cosine + e1[i] * c + e2[i] * s));
    }
    planes.forEach((plane, tile) => {
      const toApex = [0, 1, 2].map(i => apex[i] - plane.origin[i]);
      if (dot(toApex, plane.normal) <= 0) return;
      const uu = dot(plane.u, plane.u), vv = dot(plane.v, plane.v);
      let lo = [Infinity, Infinity], hi = [-Infinity, -Infinity], hits = 0;
      for (const ray of rays) {
        const facing = dot(ray, plane.normal);
        if (facing >= -1e-6) continue;
        const t = -dot(toApex, plane.normal) / facing;
        if (light.range > 0 && t > light.range) continue;
        const hit = [0, 1, 2].map(i => toApex[i] + ray[i] * t);
        const uvx = dot(hit, plane.u) / uu, uvy = dot(hit, plane.v) / vv;
        lo = [Math.min(lo[0], uvx), Math.min(lo[1], uvy)]; hi = [Math.max(hi[0], uvx), Math.max(hi[1], uvy)]; hits++;
      }
      const pad = .05 + .1 * Math.max(hi[0] - lo[0], hi[1] - lo[1], 0);
      if (hits && lo[0] - pad <= 1 && hi[0] + pad >= 0 && lo[1] - pad <= 1 && hi[1] + pad >= 0) { lists[tile].push(index); return; }
      const widened = Math.cos(Math.min(Math.PI / 2, Math.acos(Math.min(1, cosine)) + .05));
      for (let gy = 0; gy <= 6; gy++) for (let gx = 0; gx <= 6; gx++) {
        const point = [0, 1, 2].map(i => -toApex[i] + plane.u[i] * gx / 6 + plane.v[i] * gy / 6);
        const distance = Math.hypot(...point);
        if (distance < 1e-6 || (light.range > 0 && distance > light.range)) continue;
        if (dot(point, axis) / distance >= widened) { lists[tile].push(index); return; }
      }
    });
  });
  return lists;
}

test('allocation-free culling gives the same per-tile lists as the reference', () => {
  for (const {room, lights} of SCENES) {
    const stage = stageBounds(room);
    for (const frame of scenario(room, lights, 120)) assert.deepEqual(footprintTileLights(frame, room, stage), referenceTileLights(frame, room, stage));
  }
  // Adversarial poses: anywhere in or around the room, any direction, wide and
  // narrow cones, short and unbounded ranges.
  let state = 11;
  const random = () => ((state = (state * 1664525 + 1013904223) >>> 0) / 4294967296);
  for (const room of [[12, 5, 10], [24, 7, 20], [6, 3, 4]]) {
    const stage = stageBounds(room), lights = [];
    for (let i = 0; i < 4000; i++) {
      const z = random() * 2 - 1, phi = random() * Math.PI * 2, r = Math.sqrt(1 - z * z);
      const half = (.5 + random() * 70) * Math.PI / 180;
      lights.push({virtualPosition: room.map((n, k) => (random() * 1.3 - .65) * n + (k === 1 ? n / 2 : 0)),
        direction: [r * Math.cos(phi), z, r * Math.sin(phi)], outerCos: Math.cos(half),
        range: random() < .3 ? 0 : random() * 40});
    }
    for (let from = 0; from < lights.length; from += 128) {
      const batch = lights.slice(from, from + 128);
      assert.deepEqual(footprintTileLights(batch, room, stage), referenceTileLights(batch, room, stage));
    }
  }
});

test('the atlas culls into reused buffers', () => {
  const room = [24, 7, 20], stage = stageBounds(room), planes = footprintTilePlanes(room, stage);
  const indices = new Int32Array(6 * 128), counts = new Int32Array(6), frames = scenario(room, 120, 40);
  const returned = cullFootprintTiles(frames[0], 120, planes, indices, counts);
  assert.equal(returned, counts);
  const lists = footprintTileLights(frames[0], room, stage);
  lists.forEach((list, tile) => assert.deepEqual(Array.from(indices.subarray(tile * 128, tile * 128 + counts[tile])), list));
  // Most heads reach the floor; every ninth aims at a wall.
  assert.ok(counts[0] > 100, String(counts[0]));
});
