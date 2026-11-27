// Environment options of dev/preview/measure-room.mjs, parsed without side
// effects so the validation rules can be tested directly.
export const allowedPasses = ['full', 'full-no-timers', 'no-composite-fallback', 'simple-fixed-loop',
  'cached-shadows', 'skip-fog-raymarch', 'present-cached-frame'];

// Auto accepts exactly these sequences. Each is bracketed by full renders;
// full,full,full is the null control for the brackets' noise.
export const controlledSequences = {'full,full,full': 'null', 'full,simple-fixed-loop,full': 'loop',
  'full,full-no-timers,full': 'timer', 'full,no-composite-fallback,full': 'composite',
  'full,present-cached-frame,skip-fog-raymarch,cached-shadows,full': 'attribution'};

export function parseRoomOptions(environment) {
  // An empty variable means the default, the same as an unset one.
  const env = name => environment[name]?.trim() || undefined;
  const flag = name => {
    const value = env(name)?.toLowerCase();
    if (value == null || ['0', 'false', 'no'].includes(value)) return false;
    if (['1', 'true', 'yes'].includes(value)) return true;
    throw new Error(name+' must be 1, true, 0 or false.');
  };
  const fraction = (name, fallback) => {
    const value = Number(env(name) ?? fallback);
    if (!Number.isFinite(value) || value <= 0 || value >= 1) throw new Error(name+' must be a fraction between zero and one.');
    return value;
  };
  const quality = env('ROOM_QUALITY') || 'auto';
  if (!['auto', 'mobile', 'desktop'].includes(quality)) throw new Error('ROOM_QUALITY must be auto, mobile or desktop.');
  const workLight = Number(env('ROOM_WORK_LIGHT') ?? .22);
  const lightingMode = env('ROOM_LIGHTING_MODE') || 'hold';
  const minimumWarmupMs = Number(env('ROOM_WARMUP_MS') ?? 8000);
  if (!Number.isFinite(workLight) || workLight < 0 || workLight > 1) throw new Error('ROOM_WORK_LIGHT must be between zero and one.');
  if (!['hold', 'interpolate'].includes(lightingMode)) throw new Error('ROOM_LIGHTING_MODE must be hold or interpolate.');
  if (!Number.isInteger(minimumWarmupMs) || minimumWarmupMs < 8000 || minimumWarmupMs > 120000)
    throw new Error('ROOM_WARMUP_MS must be between 8000 and 120000.');
  const passes = env('ROOM_PASSES')?.split(',').map(s => s.trim()) ?? null;
  const frozenAuto = quality === 'auto' && Boolean(passes);
  const comparison = frozenAuto ? controlledSequences[passes.join(',')] ?? null : null;
  if (passes && passes.some(p => !allowedPasses.includes(p))) throw new Error('Unknown ROOM_PASSES entry.');
  if (frozenAuto && !comparison)
    throw new Error('ROOM_PASSES at Auto requires exactly one of: '+Object.keys(controlledSequences).join(' | ')+'.');
  if (passes && !frozenAuto && passes.some(p => ['simple-fixed-loop', 'full-no-timers', 'no-composite-fallback'].includes(p)))
    throw new Error('The loop, timer and composite probes require Auto and their controlled sequence.');
  const scene = env('ROOM_SCENE') || 'both';
  if (!['both', 'dense', 'fixed'].includes(scene)) throw new Error('ROOM_SCENE must be both, dense or fixed.');
  if (comparison && (scene !== 'dense' || minimumWarmupMs < 20000))
    throw new Error('Controlled comparisons require the dense moving scene and ≥20 s warmup.');
  const plumeScene = flag('ROOM_PLUME_SCENE');
  if (plumeScene && (scene === 'fixed' || passes)) throw new Error('ROOM_PLUME_SCENE requires the full moving renderer.');
  // The manual preview's fog field decays toward empty air. Pass diagnostics
  // hold it at one captured state so every sample renders the same medium;
  // `decay` reproduces the unheld behaviour, which the bracket check rejects.
  const fogField = env('ROOM_FOG_FIELD') || (passes ? 'hold' : 'decay');
  if (!['hold', 'decay'].includes(fogField)) throw new Error('ROOM_FOG_FIELD must be hold or decay.');
  if (fogField === 'hold' && !passes) throw new Error('ROOM_FOG_FIELD=hold requires the ROOM_PASSES diagnostic worker.');
  // Stated tolerances. Every pass sample must be stationary: the medians of
  // its ten 2-second blocks of worker frame time may spread by at most
  // `stationarity` of their median. The first and last `full` samples of a
  // sequence may then differ by at most `bracket` in frame cost and 1% in density.
  const bracket = fraction('ROOM_BRACKET_TOLERANCE', .02);
  const stationarity = fraction('ROOM_STATIONARITY_TOLERANCE', .05);
  return {quality, workLight, lightingMode, minimumWarmupMs, passes, frozenAuto, comparison, scene,
    plumeScene, fogField, keepPage: flag('ROOM_KEEP_PAGE'), pageTitle: env('ROOM_PAGE_TITLE') ?? null,
    bracketTolerance: {meanDensity: .01, roomHz: bracket, workerFrameP50Ms: bracket},
    stationarity: {blocks: 10, tolerance: stationarity}};
}
