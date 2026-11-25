import assert from 'node:assert/strict';
import test from 'node:test';
import {eligiblePlan, findSource, schema, sourceKey} from './plan.mjs';

const parameters = {
  shaderType: 'ShaderMaterial', precision: 'highp', outputColorSpace: 'srgb',
  defines: {STEPS: 24, MAX_LIGHTS: 'STEPS'}, numSpotLights: 8, numSpotLightShadows: 1,
  vertexShader: 'assembled vertex input', fragmentShader: 'assembled fragment input',
  customVertexShaderID: 4, customFragmentShaderID: 17, rendererExtensionParallelShaderCompile: true
};
const source = {baseVertex: parameters.vertexShader, baseFragment: parameters.fragmentShader,
  vertexGlsl: 'cached vertex', fragmentGlsl: 'cached fragment'};
const key = sourceKey(parameters);
const build = 'release-digest';
const plan = {schema, build, entries: [[key, [source]]]};

test('context-local IDs, object property order and uniform values leave the source identity unchanged', () => {
  const reordered = Object.fromEntries(Object.entries(parameters).reverse());
  assert.equal(sourceKey({
    ...reordered, customVertexShaderID: 12, customFragmentShaderID: 0,
    rendererExtensionParallelShaderCompile: false, uniforms: {color: {value: 1}}
  }), key);
});

test('variant features and ordered defines change identity', () => {
  for (const changes of [
    {precision: 'mediump'}, {outputColorSpace: 'display-p3'}, {numSpotLights: 3}, {numSpotLightShadows: 2},
    {shaderType: 'RawShaderMaterial'}, {defines: {MAX_LIGHTS: 'STEPS', STEPS: 24}},
    {defines: {STEPS: 32, MAX_LIGHTS: 'STEPS'}}
  ]) assert.notEqual(sourceKey({...parameters, ...changes}), key);
});

test('compatible current-build source hits and edited input misses', () => {
  const cache = eligiblePlan(JSON.parse(JSON.stringify(plan)), build);
  assert.deepEqual(findSource(cache, key, parameters), source);
  for (const stage of ['vertexShader', 'fragmentShader'])
    assert.equal(findSource(cache, key, {...parameters, [stage]: 'edited'}), undefined);
});

test('a legacy show or stale plan does not supply reusable source', () => {
  assert.equal(eligiblePlan(undefined, build), null);
  assert.equal(eligiblePlan({...plan, schema: schema + 1}, build), null);
  assert.equal(eligiblePlan({...plan, build: 'older-release'}, build), null);
});
