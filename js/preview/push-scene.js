import * as THREE from 'three';
import {FullScreenQuad} from 'three/addons/postprocessing/Pass.js';
import {SceneRenderer} from './scene-renderer.js';
import {cameraPreset} from './camera.js';

const width = 960, height = 160, aspect = width / height;
const bend = 68 * Math.PI / 180;
const rectilinearWidth = 100 * Math.PI / 180;

// Equidistant horizontally, perspective vertically. Source coverage includes
// all four corners. Pixels at the centre have identical horizontal/vertical scale.
export function cylindricalCamera(halfAngle, ratio) {
  const vertical = halfAngle / (ratio * Math.cos(halfAngle));
  return {aspect: Math.tan(halfAngle) / vertical, fov: 2 * Math.atan(vertical) * 180 / Math.PI};
}

export function fitCamera(camera, controls, bounds, room) {
  const [w, h, d] = room;
  const low = bounds?.min || [-w / 3, 0, -d / 4];
  const high = bounds?.max || [w / 3, h * .8, 0];
  const box = new THREE.Box3(new THREE.Vector3(...low), new THREE.Vector3(...high));
  // Include floor immediately downstage of the rig, where the beams land.
  box.expandByPoint(new THREE.Vector3((low[0]+high[0])/2, 0, Math.min(d/2, high[2]+d*.3)));
  box.expandByScalar(.35);
  const centre = box.getCenter(new THREE.Vector3());
  const backward = new THREE.Vector3(0, .08, 1).normalize();
  const up = new THREE.Vector3(0, backward.z, -backward.y);
  const vertical = Math.tan(camera.fov * Math.PI / 360), horizontal = vertical * camera.aspect;
  let distance = 1;
  for (const x of [box.min.x, box.max.x]) for (const y of [box.min.y, box.max.y])
    for (const z of [box.min.z, box.max.z]) {
      const p = new THREE.Vector3(x, y, z).sub(centre);
      distance = Math.max(distance, p.dot(backward) + 1.08 * Math.max(Math.abs(p.x)/horizontal, Math.abs(p.dot(up))/vertical));
    }
  controls.target.copy(centre);
  camera.position.copy(centre).addScaledVector(backward, distance);
  camera.far = Math.max(100, distance + box.getSize(new THREE.Vector3()).length() + d);
  controls.update();
}

// Stand inside the audience cutaway. The panorama follows the hanging rig;
// the separate full-rig lens also includes the floor and its receiver patches.
export function panoramaCamera(camera, controls, bounds, room) {
  const [width, height, depth] = room;
  const low = bounds?.min || [-width / 3, 0, -depth / 4];
  const high = bounds?.max || [width / 3, height * .8, 0];
  const x = (low[0] + high[0]) / 2, z = (low[2] + high[2]) / 2;
  const distance = Math.max(depth * .24, (high[0] - low[0]) / (2 * Math.tan(Math.PI * .265)));
  camera.position.set(x, height * .38, z + distance);
  controls.target.set(x, low[1] + (high[1] - low[1]) * .74, z);
  camera.far = Math.max(100, distance + width + depth);
  controls.update();
}

// Apply the lens after the regular tone map, fog and editor overlays. This
// samples one rendered view and avoids rerendering the whole space per sector.
class CylindricalLens {
  constructor(renderer) {
    const size = renderer.getDrawingBufferSize(new THREE.Vector2());
    this.image = new THREE.FramebufferTexture(size.x, size.y);
    this.image.minFilter = this.image.magFilter = THREE.LinearFilter;
    this.material = new THREE.ShaderMaterial({
      uniforms: {image: {value: this.image}, bend: {value: bend}}, depthTest: false, depthWrite: false, toneMapped: false,
      vertexShader: 'varying vec2 p; void main(){p=uv*2.0-1.0; gl_Position=vec4(position.xy,0.0,1.0);}',
      fragmentShader: `uniform sampler2D image; uniform float bend; varying vec2 p;
        void main(){
          float theta=p.x*bend;
          vec2 source=vec2(tan(theta)/tan(bend),p.y*cos(bend)/cos(theta));
          gl_FragColor=texture2D(image,0.5+0.5*source);
        }`
    });
    this.quad = new FullScreenQuad(this.material);
  }
  render(renderer) {
    renderer.copyFramebufferToTexture(this.image);
    this.quad.render(renderer);
  }
  dispose() {this.image.dispose(); this.material.dispose(); this.quad.dispose();}
}

export class PushScene {
  constructor(canvas) {
    this.canvas = canvas;
    this.engine = new SceneRenderer(canvas, null, {worker: true, autoStart: false, coarsePointer: true,
      reducedMotion: false, viewport: {width, height, pixelRatio: 1}});
    if (this.engine.error || !this.engine.renderer) {
      const reason = this.engine.error || 'Venue graphics are unavailable';
      this.dispose();
      throw new Error(reason);
    }
    this.engine.setQuality(this.engine.softwareRenderer ? 'low' : 'mobile');
    this.engine.setLightingMode('interpolate');
  }

  update(frame) {
    if (!this.engine || document.hidden || !this.canvas.isConnected) return;
    const engine = this.engine, now = performance.now();
    // Every worker capture crosses JSON. Preserve structural identity for
    // unchanged rigs so lighting interpolation and the overlay cache remain valid.
    const key = JSON.stringify(frame.rig);
    if (this.rigKey !== key) {this.rigKey = key; this.rig = frame.rig;}
    frame.rig = this.rig;
    const lens = frame.camera || 'fit';
    if (engine.lost) return;
    engine.update(frame, false, now);
    if (lens !== this.camera || this.framedRig !== this.rig) {
      const camera = engine.camera;
      if (lens === 'panorama') Object.assign(camera, cylindricalCamera(bend, aspect));
      else {camera.aspect = aspect; camera.fov = 2 * Math.atan(Math.tan(rectilinearWidth/2)/aspect)*180/Math.PI;}
      if (lens === 'fit') fitCamera(camera, engine.controls, frame.bounds, engine.room);
      else if (lens === 'panorama') panoramaCamera(camera, engine.controls, frame.bounds, engine.room);
      else cameraPreset(camera, engine.controls, engine.room, 'audience');
      camera.updateProjectionMatrix();
      this.camera = lens; this.framedRig = this.rig;
    }
    engine.setWorkLight(frame.editor ? .42 : .12);
    const dt = this.lastFrame === undefined ? 0 : Math.min(.1,(now-this.lastFrame)/1000);
    this.lastFrame = now; engine.clock += dt;
    engine.draw(dt);
    if (lens === 'panorama') {
      this.lens ||= new CylindricalLens(engine.renderer);
      this.lens.render(engine.renderer);
    }
  }

  dispose() {
    this.lens?.dispose();
    if (this.engine) {
      const renderer = this.engine.renderer;
      this.engine.dispose();
      renderer?.forceContextLoss();
      this.engine = null;
    }
  }
}
