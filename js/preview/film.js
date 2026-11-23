import {ShaderPass} from 'three/addons/postprocessing/ShaderPass.js';
import {FXAAShader} from 'three/addons/shaders/FXAAShader.js';
import {OutputPass} from 'three/addons/postprocessing/OutputPass.js';

// ACES is the SDR display transform. On an extended FP16 display use a
// luminance-preserving shoulder with four times diffuse-white headroom.
export function displayTransform() {
  const pass = new OutputPass();
  pass.uniforms.extendedDisplay = {value: false};
  pass.material.fragmentShader = 'uniform bool extendedDisplay;\n' + pass.material.fragmentShader.replace(
    'gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );',
    `if (extendedDisplay) {
       vec3 exposed = max(vec3(0.0), gl_FragColor.rgb * toneMappingExposure);
       gl_FragColor.rgb = exposed / (1.0 + max(exposed.r, max(exposed.g, exposed.b)) * 0.25);
     } else {
       gl_FragColor.rgb = ACESFilmicToneMapping(gl_FragColor.rgb);
     }`
  );
  return pass;
}

// For a heavily downscaled room, evaluate the expensive edge search once per
// source pixel. The native-size film pass can then do one filtered lookup.
export class EdgePass extends ShaderPass {
  constructor() { super(FXAAShader); this.enabled = false; }
  setSize(width, height) { this.uniforms.resolution.value.set(1 / width, 1 / height); }
}

// FXAA uses the source texel size after display encoding. Dither and grain use
// physical presentation pixels, so scaling the fog or room cannot enlarge them.
export class FilmPass extends ShaderPass {
  constructor() {
    super({...FXAAShader,
      uniforms: {...FXAAShader.uniforms, grain: {value: 0.0015}, phase: {value: 0}, resolveAtDisplay: {value: true}},
      fragmentShader: FXAAShader.fragmentShader.replace('void main()', 'void resolveEdges()') + `
        uniform float grain;
        uniform float phase;
        uniform bool resolveAtDisplay;
        void main() {
          if (resolveAtDisplay) resolveEdges();
          else gl_FragColor = texture2D(tDiffuse, vUv);
          vec2 pixel = gl_FragCoord.xy;
          float dither = fract(52.9829189 * fract(dot(pixel, vec2(0.06711056, 0.00583715)))) - 0.5;
          float noise = fract(52.9829189 * fract(dot(pixel + phase, vec2(0.06711056, 0.00583715)))) - 0.5;
          gl_FragColor.rgb += vec3(dither / 255.0 + noise * grain);
          gl_FragColor.a = 1.0;
        }`
    });
  }
  setSize(width, height) { this.uniforms.resolution.value.set(1 / width, 1 / height); }
}
