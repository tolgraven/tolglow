import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {CylindricalLens, cylindricalCamera, fitCamera} from '../../../js/preview/push-scene.js';

test('cylindrical sampling covers both extreme corners and retains angular scale at the centre', () => {
  const ratio = 6, angle = 68*Math.PI/180;
  const config = cylindricalCamera(angle, ratio), tangent = Math.tan(config.fov*Math.PI/360);
  const ray = (x,y) => {
    const sx=Math.tan(x*angle)/Math.tan(angle), sy=y*Math.cos(angle)/Math.cos(x*angle);
    assert.ok(Math.abs(sx)<=1.00001 && Math.abs(sy)<=1.00001);
    return new THREE.Vector3(sx*config.aspect*tangent,sy*tangent,-1).normalize();
  };
  for (const x of [-1,0,1]) for (const y of [-1,0,1]) {
    const r=ray(x,y);
    assert.ok(Math.abs(Math.atan2(r.x,-r.z)-x*angle)<1e-8);
    assert.ok(Math.abs(r.y/Math.hypot(r.x,r.z)-y*angle/ratio)<1e-8);
  }
  const centre=ray(0,0);
  assert.ok(Math.abs(centre.angleTo(ray(1/480,0))-centre.angleTo(ray(0,1/80)))<1e-7);
});

test('wide fit contains offset truss extremes, floor and raised fixtures', () => {
  const camera=new THREE.PerspectiveCamera(21,6,.08,100);
  const controls={target:new THREE.Vector3(),update(){camera.lookAt(this.target);camera.updateMatrixWorld();}};
  const bounds={min:[-13,2,-4],max:[6,6,3]};
  fitCamera(camera,controls,bounds,[30,8,18]); camera.updateProjectionMatrix();
  for (const x of [-13,6]) for (const y of [0,6]) for (const z of [-4,8.4]) {
    const p=new THREE.Vector3(x,y,z).project(camera);
    assert.ok(Math.abs(p.x)<1 && Math.abs(p.y)<1 && Math.abs(p.z)<1);
  }
});

test('panorama reads and presents the finished canvas, then restores the previous target', () => {
  const scene = {}, image = {}, sampled = [];
  let bound = scene;
  const renderer = {
    getRenderTarget: () => bound,
    setRenderTarget(target) { bound = target; },
    copyFramebufferToTexture(texture) { assert.equal(texture, image); sampled.push(bound); }
  };
  const lens = {image, quad: {render() { sampled.push(bound); }}};
  CylindricalLens.prototype.render.call(lens, renderer);
  assert.deepEqual(sampled, [null, null]); assert.equal(bound, scene);
  lens.quad.render = () => {throw Error('lost');};
  assert.throws(() => CylindricalLens.prototype.render.call(lens, renderer), /lost/);
  assert.equal(bound, scene);
});
