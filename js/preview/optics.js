import * as THREE from 'three';

export function apertureRadius(fixture) {
  const radius = fixture['lens-radius-m'];
  return Number.isFinite(radius) && radius >= 0 ? radius : .119;
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
export function finiteApertureMaterial(material, uniform, patchRadii) {
  material.onBeforeCompile = shader => {
    shader.uniforms.tolglowApexDepth = uniform;
    shader.uniforms.tolglowPatchRadius = patchRadii;
    shader.fragmentShader = 'uniform float tolglowApexDepth[18]; uniform float tolglowPatchRadius[18];\n' + shader.fragmentShader;
    const lighting = THREE.ShaderChunk.lights_fragment_begin.replace(
      'getSpotLightInfo( spotLight, geometryPosition, directLight );',
      `getSpotLightInfo( spotLight, geometryPosition, directLight );
       directLight.color *= step(tolglowApexDepth[i], dot(spotLight.position - geometryPosition, spotLight.direction));
       { float patchD2 = dot(spotLight.position - geometryPosition, spotLight.position - geometryPosition);
       directLight.color *= patchD2 / max(1.e-9, patchD2 + tolglowPatchRadius[i]*tolglowPatchRadius[i]); }`);
    shader.fragmentShader = shader.fragmentShader.replace('#include <lights_fragment_begin>', lighting);
  };
  material.customProgramCacheKey = () => 'tolglow-finite-aperture-patch-v2';
}
