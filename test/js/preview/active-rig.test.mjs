import assert from 'node:assert/strict';
import test from 'node:test';
import {existsSync, readFileSync} from 'node:fs';
import {chromium} from 'playwright-core';
import {fixtureSlots} from '../../../js/preview/fixture-selection.js';
import {tourSubjects} from '../../../js/preview/camera-navigation.js';

test('mesh, light and shadow budgets follow emitted values and keep their slots on ties', () => {
  for (const budget of [32, 64, 128]) {
    const ids = Array.from({length:256}, (_, i) => String(i));
    const fixtures = new Map(ids.map(id => [id, {id, channels: id === '252' ? {pan:1, tilt:2} : {}}]));
    const values = Object.fromEntries(ids.slice(140).map(id => [id, {'emitted-color':[0, 1, 0]}]));
    values['252'] = {'emitted-color':[.1, 0, 0]};
    const selected = '251'; delete values[selected];
    const initial = fixtureSlots(fixtures, values, selected, ids.slice(0, budget), ids.slice(0,16), 2);
    assert.ok(initial.meshes.includes(selected));
    assert.ok(initial.lights.slice(0,2).includes('252'));
    assert.equal(new Set(initial.meshes).size, budget);
    assert.equal(new Set(initial.lights).size, 16);
    assert.ok(initial.lights.every(id => values[id] && initial.meshes.includes(id)));
    const reversed = Object.fromEntries(Object.entries(values).reverse());
    assert.deepEqual(fixtureSlots(fixtures, reversed, selected, initial.meshes, initial.lights, 2), initial);
    const dark = fixtureSlots(fixtures, {}, selected, initial.meshes, initial.lights, 2);
    assert.deepEqual(dark, initial);
  }
  const manyMovers = new Map(Array.from({length:128}, (_, i) => [String(i), {channels:{pan:1,tilt:2}}]));
  manyMovers.set('strip', {channels:{green:1}});
  const rows = Object.fromEntries([...manyMovers.keys()].map(id => [id, {'emitted-color':[0,.5,0]}]));
  const meshes = [...manyMovers.keys()].slice(0,32);
  const slots = fixtureSlots(manyMovers, rows, null, meshes, meshes.slice(0,16), 2);
  assert.ok(slots.lights.includes('strip'));
});

test('late solo illuminates the rendered stage and fog without reallocating light or shadow slots', {timeout:120000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for the WebGL scene test');
  const asset = new URL('../../../resources/studio/preview.js', import.meta.url);
  const browser = await chromium.launch({executablePath, args:['--use-gl=angle','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage({viewport:{width:1024,height:768},hasTouch:true});
    await page.route('http://localhost/rig', route => route.fulfill({contentType:'text/html',
      body:'<canvas style="width:192px;height:120px"></canvas><output></output>'}));
    await page.route('http://localhost/preview.js', route => route.fulfill({contentType:'text/javascript',body:readFileSync(asset)}));
    await page.goto('http://localhost/rig');
    const result = await page.evaluate(async () => {
      const {RoomPreview} = await import('/preview.js');
      const engine = new RoomPreview(document.querySelector('canvas'),document.querySelector('output'),{worker:false});
      try {
        const fixtures = Array.from({length:128}, (_, i) => ({id:String(i), channels:{pan:1,tilt:2},
          position: i === 127 || i === 126 ? [0,3,0] : [5,(i%4)*.8+.4,-4+(i%8)]}));
        const rig = {room:{width:12,height:5,depth:10}, fixtures,trusses:[]};
        const frame = (n, values) => ({rig, 'show-id':'many movers',epoch:1,revision:1,'frame-number':n,'time-ms':25*n,
          editor:{selection:{kind:'fixtures',id:'125'}},output:{values}});
        engine.update(frame(1,{}));
        await engine.whenReady; engine.pause(); engine.setQuality('mobile'); engine.setFog(.55);
        // At equal physical pose, a red then blue emitter isolates real lighting
        // changes from a mesh moving. Read actual postprocessed WebGL pixels.
        const capture = (n, values) => {
          engine.update(frame(n,values)); engine.draw(0);
          engine.renderer.setRenderTarget(null);
          const gl = engine.renderer.getContext();
          const pixels = new Uint8Array(gl.drawingBufferWidth*gl.drawingBufferHeight*4);
          gl.readPixels(0,0,gl.drawingBufferWidth,gl.drawingBufferHeight,gl.RGBA,gl.UNSIGNED_BYTE,pixels);
          return pixels;
        };
        const resources = () => [engine.spots.map(spot => [spot.uuid,spot.shadow.map?.texture.uuid]), engine.renderer.info.programs.length,
          engine.fixtures.map(entry => entry.root.uuid)];
        const dark = capture(2,{}), initial = resources();
        const red = capture(3,{'127':{direction:[0,-1,0],'emitted-color':[1,0,0]}});
        const shadowIndex = engine.lightIds.indexOf('127');
        const shadow = engine.spots[shadowIndex], fog = engine.fog.lights.map(l => l.color.toArray());
        const solo = {id:engine.lightIds[shadowIndex], shadow:shadow.castShadow, intensity:shadow.color.toArray(), fog,
          selected:engine.fixtures.some(entry => entry.id === '125' && entry.lensMaterial.emissive.r === 0)};
        const blue = capture(4,{'126':{direction:[0,-1,0],'emitted-color':[0,0,1]}});
        let redPixels=0, bluePixels=0;
        for (let i=0;i<red.length;i+=4) {
          if (red[i] > dark[i]+5 && red[i] > blue[i]+5) redPixels++;
          if (blue[i+2] > dark[i+2]+5 && blue[i+2] > red[i+2]+5) bluePixels++;
        }
        return {redPixels,bluePixels,initial,after:resources(),solo,final:engine.fixtures.find(entry => entry.light?.color.b === 1)?.id,
          edited:engine.focusFixture('124'), geometry:engine.fixtures.length};
      } finally {engine.dispose();}
    });
    assert.equal(result.geometry,32);
    assert.equal(result.solo.id,'127'); assert.ok(result.solo.shadow); assert.deepEqual(result.solo.intensity,[1,0,0]);
    assert.deepEqual(result.solo.fog,[[1,0,0]]); assert.ok(result.solo.selected); assert.ok(result.edited);
    assert.equal(result.final,'126'); assert.ok(result.redPixels > 20, JSON.stringify(result.redPixels));
    assert.ok(result.bluePixels > 20, JSON.stringify(result.bluePixels));
    assert.deepEqual(result.after,result.initial);
    t.diagnostic(`Late solo changed ${result.redPixels} red and ${result.bluePixels} blue framebuffer pixels on mobile quality.`);
    const snapshot = {rig:{fixtures:[{id:'1',position:[0,3,0]}, {id:'127',position:[1,3,0]}]},
      output:{values:{'127':{'emitted-color':[1,0,0]}}}};
    assert.equal(tourSubjects(snapshot)[0].id,'127');
  } finally {await browser.close();}
});
