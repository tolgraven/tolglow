// Private measurement adapter for the production Room postprocessing chain.
import {AdditiveBlending, GLSL3, HalfFloatType, REVISION, RGBAFormat} from 'three';
import {UnrealBloomPass} from 'three/addons/postprocessing/UnrealBloomPass.js';
import {CopyShader} from 'three/addons/shaders/CopyShader.js';
import {displayTransform} from '../../js/preview/film.js';

function requireShape(value, reason) {
  if (!value) throw new Error(`Bloom/display experiment requires Three r187: ${reason}`);
}

function fusedDisplayPass() {
  const pass = displayTransform();
  const material = pass.material;
  const sample = 'gl_FragColor = texture2D( tDiffuse, vUv );';
  requireShape(material.fragmentShader.includes(sample), 'unrecognized display input');
  pass.uniforms.bloomTexture = {value: null};
  material.glslVersion = GLSL3;
  material.vertexShader = material.vertexShader.replaceAll('attribute ', 'in ').replace('varying ', 'out ');
  material.fragmentShader = material.fragmentShader
    .replace('varying vec2 vUv;', 'in vec2 vUv;\n out vec4 displayColor;\n uniform sampler2D bloomTexture;')
    .replace(sample, `vec4 sum = texture(tDiffuse, vUv) + texture(bloomTexture, vUv);
      // The original additive quad stores RGBA16F before OutputPass samples it.
      gl_FragColor = vec4(unpackHalf2x16(packHalf2x16(sum.xy)),
                          unpackHalf2x16(packHalf2x16(sum.zw)));`)
    .replaceAll('gl_FragColor', 'displayColor');
  return pass;
}

export function bloomDisplayShaderSources() {
  const baseline = displayTransform();
  const fused = fusedDisplayPass();
  try {
    return Object.fromEntries(Object.entries({baseline, fused}).map(([mode, {material}]) => [mode,
      [material.glslVersion, material.vertexShader, material.fragmentShader]]));
  } finally {
    baseline.dispose();
    fused.dispose();
  }
}

export function installBloomDisplayExperiment(engine) {
  const {bloom, outputPass, composer} = engine;
  const quad = bloom?._fsQuad;
  const blend = bloom?.blendMaterial;
  requireShape(REVISION === '187' && bloom instanceof UnrealBloomPass && bloom.nMips === 5 &&
    quad && blend?.fragmentShader === CopyShader.fragmentShader && blend.blending === AdditiveBlending &&
    blend.premultipliedAlpha && blend.uniforms.opacity.value === 1 && bloom.needsSwap === false,
  'unrecognized bloom additive pass');
  requireShape(composer.passes[composer.passes.indexOf(bloom) + 1] === outputPass,
    'bloom must immediately precede the display transform');

  const fused = fusedDisplayPass();
  const originalBloom = bloom.render;
  const originalOutput = outputPass.render;
  let enabled = false;
  let input;
  let texture;
  bloom.render = function (...args) {
    texture = input = null;
    if (!enabled) return originalBloom.apply(this, args);
    const [, write, read, , mask] = args;
    requireShape(!mask && !bloom.renderToScreen && !read.samples && read.texture.type === HalfFloatType &&
      read.texture.format === RGBAFormat && read.width === write.width && read.height === write.height,
    'the composer must be offscreen, unmasked and sample-matched RGBA16F');
    const draw = quad.render;
    // Reuse the actual bright extraction, five H/V blur pairs and mip composite.
    // Only suppress the final premultiplied ONE/ONE draw into the room buffer.
    quad.render = function (renderer) {
      if (this.material !== blend) return draw.call(this, renderer);
      requireShape(!texture && bloom.copyUniforms.tDiffuse.value === bloom.renderTargetsHorizontal[0].texture,
        'unrecognized bloom result texture');
      texture = bloom.copyUniforms.tDiffuse.value;
      input = read;
    };
    try {
      originalBloom.apply(this, args);
      requireShape(texture, 'the additive draw did not run');
    } finally {
      quad.render = draw;
    }
  };
  outputPass.render = function (renderer, write, read, ...rest) {
    if (!enabled || !bloom.enabled) return originalOutput.call(this, renderer, write, read, ...rest);
    requireShape(texture && input === read && !this.renderToScreen, 'unexpected display input');
    fused.uniforms.bloomTexture.value = texture;
    fused.uniforms.extendedDisplay.value = this.uniforms.extendedDisplay.value;
    try {
      return fused.render(renderer, write, read, ...rest);
    } finally {
      input = texture = fused.uniforms.bloomTexture.value = null;
    }
  };

  return {
    setEnabled(value) { enabled = Boolean(value); },
    dispose() {
      bloom.render = originalBloom;
      outputPass.render = originalOutput;
      fused.dispose();
    }
  };
}
