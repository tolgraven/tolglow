import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {chromium} from 'playwright-core';
import {apertureRadius} from '../../../js/preview/optics.js';
import {fixtureSlots} from '../../../js/preview/fixture-selection.js';
import {beamAngle, roomDimensions} from '../../../js/preview/math.js';

test('full included fixed beam angle and zoom range use fixture data in degrees', () => {
  const degrees = half => half * 360 / Math.PI;
  assert.equal(degrees(beamAngle({'beam-angle':20}, {zoom:.75})),20);
  assert.equal(degrees(beamAngle({'beam-angle':20,'zoom-range-deg':[8,40]}, {channels:{zoom:.25}})),16);
  assert.equal(degrees(beamAngle({}, {zoom:.5})),29);
  for (const radius of [0,.001,.8]) assert.equal(apertureRadius({'lens-radius-m':radius}),radius);
  for (const full of [.2,175]) assert.ok(Math.abs(degrees(beamAngle({'beam-angle':full})) - full) < 1e-10);
});

test('room receiver and camera coordinates keep the physical dimensions', () => {
  assert.deepEqual(roomDimensions({width:120,height:.8,depth:18}),[120,.8,18]);
  assert.deepEqual(roomDimensions({}),[12,5,10]);
});

test('calibration weights the scarce visible light slots, including a fixture with no visible emitters', () => {
  const fixtures = new Map([['uv',{'relative-brightness':0}],['dim',{}],['bright',{'relative-brightness':3}]]);
  const output = Object.fromEntries([...fixtures.keys()].map(id => [id,{'emitted-color':[id === 'bright' ? .5 : 1,0,0]}]));
  assert.deepEqual(fixtureSlots(fixtures,output,null,['uv'],['uv'],1).lights,['bright']);
});

test('a close floor patch casts colored spill onto a nearby panel; a nearer blocker and blackout retire it',
  {timeout:120000}, async t => {
    const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
    if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for WebGL');
    const bundle = readFileSync(new URL('../../../resources/studio/preview.js',import.meta.url));
    const browser = await chromium.launch({executablePath,args:['--use-gl=angle','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
    try {
      const page = await browser.newPage();
      await page.route('http://localhost/room',route => route.fulfill({contentType:'text/html',body:'<canvas style="width:320px;height:224px"></canvas>'}));
      await page.route('http://localhost/preview.js',route => route.fulfill({contentType:'text/javascript',body:bundle}));
      await page.goto('http://localhost/room');
      const observations = await page.evaluate(async () => {
        const {SceneRenderer} = await import('/preview.js');
        const engine = new SceneRenderer(document.querySelector('canvas'), null, {autoStart:false});
        try {
          engine.visible = true; if (!engine.renderer) engine.initialize();
          engine.pause(); engine.setQuality('mobile'); engine.resize(); engine.setWorkLight(0); engine.setFog(0);
          engine.camera.position.set(-1.8,1.4,2.1); engine.controls.target.set(0,.35,0); engine.controls.update();
          const panel = {id:'screen',point:[.42,.56,0],normal:[-2,0,0],'u-axis':[1,0,3],size:[1.6,1.1]};
          const blocker = {id:'intercept',point:[0,.45,0],normal:[0,1,0],'u-axis':[1,0,0],size:[1,1]};
          const fixture = {id:'mover',position:[0,1,0],'beam-angle':18,'lens-radius-m':.05,
            'pseudo-fixture':true,'relative-brightness':2};
          const radius = .05 + Math.tan(Math.PI / 20);
          const footprint = {'surface-id':'floor',hit:[0,0,0],center:[0,0,0],normal:[0,1,0],
            'major-axis':[1,0,0],'minor-axis':[0,0,-1],'major-radius-m':radius,'minor-radius-m':radius,'distance-m':1};
          let frame=0;
          function render(brightness, rgb, surfaces=[panel], projected=footprint) {
            engine.update({'show-id':'optics',epoch:1,revision:1,'frame-number':++frame,'time-ms':25*frame,
              rig:{room:{width:6,height:3,depth:6},trusses:[],fixtures:[{...fixture,'relative-brightness':brightness}],surfaces},
              output:{values:{mover:{'emitted-color':rgb,direction:[0,-1,0],'beam-footprint':projected}}}});
            engine.draw();
          }
          function panelRed() {
            const camera = engine.camera, point = camera.position.clone().set(.419,.15,.06).project(camera);
            const pixels = new Uint16Array(4), x = Math.floor((point.x + 1)*.5*engine.width), y = Math.floor((point.y + 1)*.5*engine.height);
            engine.renderer.readRenderTargetPixels(engine.sceneTarget,x,y,1,1,pixels);
            // Scene target is linear RGBA16F. Positive half bits order monotonically.
            return Array.from(pixels);
          }
          render(2,[1,0,0]);
          const patch = engine.getTelemetry().reflectedPatches, reflected = engine.reflections.lights[0];
          const clearPower = reflected.intensity, rgb = reflected.color.toArray(), directIntensity = engine.spots[0].intensity;
          const angle = engine.spots[0].angle * 360/Math.PI, litPanel = panelRed();
          engine.fog.field.clear(2); engine.setFog(1); engine.draw();
          const fogPower = reflected.intensity;
          engine.setFog(0);
          render(2,[1,0,0],[panel],null); const darkPanel = panelRed();
          render(2,[1,0,0],[panel,blocker]); const occluded = engine.getTelemetry().reflectedPatches;
          render(0,[1,0,0]); const disabled = engine.reflections.lights[0].intensity;
          render(2,[0,0,0]); const blackout = engine.getTelemetry().reflectedPatches;
          return {patch,clearPower,fogPower,rgb,directIntensity,angle,litPanel,darkPanel,occluded,disabled,blackout,error:engine.error};
        } finally {engine.dispose();}
      });
      assert.equal(observations.error,undefined);
      assert.equal(observations.patch[0]?.surface,'floor',JSON.stringify(observations));
      assert.deepEqual(observations.patch[0].center,[0,0,0]);
      assert.equal(observations.directIntensity,290);
      assert.equal(observations.angle,18);
      assert.ok(observations.clearPower > observations.fogPower && observations.fogPower > 0);
      assert.ok(observations.rgb[0] > 0 && observations.rgb[1] === 0 && observations.rgb[2] === 0);
      assert.ok(observations.litPanel[0] > observations.darkPanel[0],JSON.stringify(observations));
      assert.deepEqual(observations.occluded,[]); assert.deepEqual(observations.blackout,[]); assert.equal(observations.disabled,0);
    } finally {await browser.close();}
  });
