import * as THREE from 'three';
import {clamp} from './math.js';

export function apertureRadius(fixture) {
  return clamp(Number(fixture['lens-radius-m']) || .119, .005, .5);
}
export function opticalFrustum(position, direction, halfAngle, radius) {
  const axis = new THREE.Vector3(...direction).normalize();
  const optical = new THREE.Vector3(...position);
  const apexDistance = radius / Math.tan(halfAngle);
  return {optical, virtual: optical.clone().addScaledVector(axis, -apexDistance), axis, apexDistance, radius};
}

// Three's punctual spotlight represents a finite optical frustum with a virtual
// apex. Suppress illumination behind the physical aperture plane. The aperture
// uniform order mirrors Three's shadow-first ordering of visible spotlights.
export function finiteApertureMaterial(material, uniform) {
  material.onBeforeCompile = shader => {
    shader.uniforms.tolglowApexDepth = uniform;
    shader.fragmentShader = 'uniform float tolglowApexDepth[16];\n' + shader.fragmentShader;
    const lighting = THREE.ShaderChunk.lights_fragment_begin.replace(
      'getSpotLightInfo( spotLight, geometryPosition, directLight );',
      `getSpotLightInfo( spotLight, geometryPosition, directLight );
       directLight.color *= step(tolglowApexDepth[i], dot(spotLight.position - geometryPosition, spotLight.direction));`);
    shader.fragmentShader = shader.fragmentShader.replace('#include <lights_fragment_begin>', lighting);
  };
  material.customProgramCacheKey = () => 'tolglow-finite-aperture-v1';
}
