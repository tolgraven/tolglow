import test from 'node:test';
import assert from 'node:assert/strict';
import {footprintTileLights, stageBounds, footprintTileSize} from '../../../js/preview/footprints.js';
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
