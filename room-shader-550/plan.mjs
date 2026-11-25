export const schema = 1;

// Keep the ordered define list. Macro values can reference earlier definitions.
export function sourceKey(parameters) {
  const {
    customVertexShaderID, customFragmentShaderID, rendererExtensionParallelShaderCompile,
    vertexShader, fragmentShader, uniforms, defines, ...variant
  } = parameters;
  return JSON.stringify([
    Object.keys(variant).sort().map(name => {
      const value = variant[name];
      if (value === undefined || value === null ||
        ['boolean', 'number', 'string'].includes(typeof value)) return [name, value];
      throw new Error('Unexpected Three shader parameter: ' + name);
    }),
    Object.entries(defines || {})
  ], (_, value) => value === undefined ? ['undefined'] : value);
}

export function eligiblePlan(plan, build) {
  if (plan?.schema !== schema || plan.build !== build || !Array.isArray(plan.entries)) return null;
  return new Map(plan.entries);
}

export function findSource(entries, key, parameters) {
  const bucket = entries?.get(key);
  return bucket?.find(entry => entry.baseVertex === parameters.vertexShader &&
    entry.baseFragment === parameters.fragmentShader);
}
