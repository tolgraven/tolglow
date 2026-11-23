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

// Resolve every composited edge at source resolution. Both programs run from
// frame one, so an adaptive resize cannot compile shaders during a cue launch.
export class EdgePass extends ShaderPass {
  constructor() { super(FXAAShader); }
  setSize(width, height) { this.uniforms.resolution.value.set(1 / width, 1 / height); }
}

// A single bilinear lookup presents the resolved image at panel resolution.
// Dither and grain use physical pixels even when the scene budget changes.
export class FilmPass extends ShaderPass {
  constructor() {
    super({
      uniforms: {tDiffuse: {value: null}, grain: {value: 0.0015}, phase: {value: 0}},
      vertexShader: FXAAShader.vertexShader,
      fragmentShader: `
        uniform sampler2D tDiffuse;
        uniform float grain;
        uniform float phase;
        varying vec2 vUv;
        void main() {
          vec3 color = texture2D(tDiffuse, vUv).rgb;
          vec2 pixel = gl_FragCoord.xy;
          float dither = fract(52.9829189 * fract(dot(pixel, vec2(0.06711056, 0.00583715)))) - 0.5;
          float noise = fract(52.9829189 * fract(dot(pixel + phase, vec2(0.06711056, 0.00583715)))) - 0.5;
          gl_FragColor = vec4(color + dither / 255.0 + noise * grain, 1.0);
        }`
    });
  }
}
