// Prominence-ranked light level of detail (#782). Pure functions: no Three.js
// objects, clocks or GPU state. The scene owner supplies plain candidate and
// view data and keeps the returned state for the next call.
//
// A full light gets a real SpotLight (forward shading, optional shadow map) and
// a raymarched fog beam. Every other emitting fixture is drawn as a faked beam
// cone with a painted floor/wall footprint. Ranking only chooses presentation
// detail; it never changes, drops or invents committed fixture output.

const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const sub = (a, b) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
const length = v => Math.hypot(v[0], v[1], v[2]);

export const LOD_DEFAULTS = Object.freeze({
  // A resident keeps its slot unless a challenger's score exceeds the
  // resident's by this factor (and the resident has held its slot long enough).
  margin: 1.35,
  // Seconds a newly promoted light keeps its slot before it can be demoted.
  hold: .6,
  // Weight of looking into the lens: score multiplier at a beam aimed at the eye.
  facing: 1.5,
  // Lights at or below this emission level are never promoted.
  minimumLevel: 1e-4,
});

/**
 * Projected screen fraction of a beam: a bounding sphere around the lit cone
 * segment, culled against the view frustum planes. Returns 0 outside the view.
 * view: {eye: [x,y,z], planes: [[nx,ny,nz,d] x6] (inside when n·p + d >= 0),
 *        tanHalfFov: number}
 * beam: {position, direction (unit), angle (half-angle radians), length}
 */
export function screenCoverage(beam, view) {
  const reach = Math.max(.05, beam.length ?? 6);
  const half = reach / 2;
  const center = beam.position.map((v, i) => v + beam.direction[i] * half);
  const radius = Math.hypot(half, reach * Math.tan(Math.min(beam.angle, 1.4)));
  for (const plane of view.planes || []) {
    if (plane[0] * center[0] + plane[1] * center[1] + plane[2] * center[2] + plane[3] < -radius) return 0;
  }
  const distance = Math.max(radius * .5, length(sub(center, view.eye)), 1e-3);
  // Fraction of the half view height subtended by the sphere, clamped at the
  // whole view so a light surrounding the camera does not dominate everything.
  const fraction = Math.min(1, radius / (distance * Math.max(1e-3, view.tanHalfFov)));
  return fraction * fraction;
}

/**
 * Prominence of one candidate from the current camera: emitted level × screen
 * coverage × a bonus for beams aimed toward the viewer (visible lens/hot spot).
 */
export function prominence(candidate, view, options = LOD_DEFAULTS) {
  const level = Math.max(0, candidate.level || 0);
  if (level <= (options.minimumLevel ?? LOD_DEFAULTS.minimumLevel)) return 0;
  const coverage = screenCoverage(candidate, view);
  if (coverage <= 0) return 0;
  const toEye = sub(view.eye, candidate.position), distance = length(toEye);
  const facing = distance > 1e-6 ? Math.max(0, dot(candidate.direction, toEye) / distance) : 0;
  const bonus = 1 + (options.facing ?? LOD_DEFAULTS.facing) * facing ** 4;
  // Square root keeps a huge near beam from overwhelming level differences.
  const score = level * Math.sqrt(coverage) * bonus;
  return Number.isFinite(score) ? score : 0;
}

/**
 * Choose up to `budget` full lights with hysteresis.
 * candidates: [{id, level, position, direction, angle, length, pinned?}]
 * state: previous result's `state` (or undefined); now: seconds.
 * Returns {full: [ids], faked: [ids], scores: Map, state}. `full` lists the
 * shadow group first (its first `options.shadowSlots` ids), then the rest,
 * each by score. The shadow group has its own hold and margin, so lights near
 * the shadow boundary do not trade shadow maps as their scores cross.
 * Pinned candidates (the editor selection) are always full while they emit.
 */
export function rankLights(candidates, view, budget, state = null, now = 0, options = LOD_DEFAULTS) {
  const margin = options.margin ?? LOD_DEFAULTS.margin, hold = options.hold ?? LOD_DEFAULTS.hold;
  const residents = state?.since || new Map();
  const scores = new Map(), emitting = [];
  for (const candidate of candidates) {
    const score = prominence(candidate, view, options);
    scores.set(candidate.id, score);
    if ((candidate.level || 0) > (options.minimumLevel ?? LOD_DEFAULTS.minimumLevel)) emitting.push(candidate);
  }
  const limit = Math.max(0, Math.floor(budget));
  // Residents compete with a boosted score; a resident within its hold time
  // cannot be displaced at all while it still has positive prominence.
  const effective = candidate => {
    const score = scores.get(candidate.id);
    if (!residents.has(candidate.id) || score <= 0) return score;
    return now - residents.get(candidate.id) < hold ? Infinity : score * margin;
  };
  const ordered = emitting.filter(c => scores.get(c.id) > 0 || c.pinned)
    .map(c => ({id: c.id, pinned: Boolean(c.pinned), key: effective(c), score: scores.get(c.id)}))
    .sort((a, b) => Number(b.pinned) - Number(a.pinned) || b.key - a.key || b.score - a.score || (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
  const full = ordered.slice(0, limit).map(entry => entry.id);
  // Free slots cost the same forward shading whether used or not: fill them
  // with emitters that are out of view (they can still light what is seen
  // through bounce), brightest first. They never displace a visible light.
  if (full.length < limit) {
    const taken = new Set(full);
    const hidden = emitting.filter(c => !taken.has(c.id))
      .sort((a, b) => Number(residents.has(b.id)) - Number(residents.has(a.id)) || b.level - a.level || (a.id < b.id ? -1 : a.id > b.id ? 1 : 0));
    for (const c of hidden.slice(0, limit - full.length)) full.push(c.id);
  }
  const chosen = new Set(full);
  const since = new Map();
  for (const id of full) since.set(id, residents.has(id) ? residents.get(id) : now);
  const faked = emitting.map(c => c.id).filter(id => !chosen.has(id));
  // The shadow group: same hysteresis as the full/faked split, among full lights.
  const shadowResidents = state?.shadowSince || new Map();
  const shadowCount = Math.min(full.length, Math.max(0, Math.floor(options.shadowSlots ?? 0)));
  const pinned = new Set(candidates.filter(c => c.pinned).map(c => c.id));
  const byScore = (a, b) => scores.get(b) - scores.get(a) || (a < b ? -1 : a > b ? 1 : 0);
  const shadowKey = id => {
    const score = scores.get(id);
    if (!shadowResidents.has(id)) return score;
    return now - shadowResidents.get(id) < hold ? Infinity : score * margin;
  };
  const shadowGroup = [...full].sort((a, b) => Number(pinned.has(b)) - Number(pinned.has(a))
    || shadowKey(b) - shadowKey(a) || byScore(a, b)).slice(0, shadowCount).sort(byScore);
  const shadowSet = new Set(shadowGroup);
  const shadowSince = new Map(shadowGroup.map(id => [id, shadowResidents.has(id) ? shadowResidents.get(id) : now]));
  const ordered2 = [...shadowGroup, ...full.filter(id => !shadowSet.has(id)).sort(byScore)];
  return {full: ordered2, faked, scores, state: {since, shadowSince}};
}

/**
 * Map chosen full lights onto persistent GPU slots. Slots keep their occupant
 * while it stays chosen; the first `shadowSlots` slots (allocated shadow maps)
 * take the most prominent lights first. Returns an array of ids or null.
 */
export function assignSlots(previous, full, slotCount, shadowSlots) {
  const slots = Array.from({length: slotCount}, (_, i) => previous?.[i] ?? null);
  const wanted = full.slice(0, slotCount);
  const shadowWanted = wanted.slice(0, shadowSlots), rest = wanted.slice(shadowSlots);
  const place = (from, to, ids) => {
    const set = new Set(ids);
    for (let i = from; i < to; i++) if (slots[i] != null && !set.has(slots[i])) slots[i] = null;
    const present = new Set(slots.slice(from, to));
    const newcomers = ids.filter(id => !present.has(id));
    for (let i = from; i < to && newcomers.length; i++) if (slots[i] == null) slots[i] = newcomers.shift();
    return newcomers;
  };
  // A light that moves between the shadow and plain groups leaves its old slot.
  const shadowSet = new Set(shadowWanted), restSet = new Set(rest);
  for (let i = 0; i < slotCount; i++) {
    if (i < shadowSlots ? restSet.has(slots[i]) : shadowSet.has(slots[i])) slots[i] = null;
  }
  place(0, Math.min(shadowSlots, slotCount), shadowWanted);
  place(Math.min(shadowSlots, slotCount), slotCount, rest);
  return slots;
}

/**
 * Emission envelope for ranking: a peak hold that decays with time constant
 * `hold` and keeps a light that went dark for up to `hold` seconds. A strobing
 * key light therefore keeps its rank and slot through its off frames (its
 * output is still zero there; only presentation detail is retained). `levels`
 * maps id to the current level (0 or absent when dark). Returns Map id →
 * {peak, lastLit}.
 */
export function holdEmission(previous, levels, now, hold = LOD_DEFAULTS.hold) {
  const next = new Map();
  for (const [id, state] of previous || []) {
    const age = now - state.lastLit;
    if (age < hold) next.set(id, {peak: state.peak, lastLit: state.lastLit});
  }
  for (const [id, level] of levels) {
    if (!(level > 0)) continue;
    const prior = previous?.get(id);
    const decayed = prior ? prior.peak * Math.exp(-Math.max(0, now - prior.lastLit) / hold) : 0;
    next.set(id, {peak: Math.max(level, decayed), lastLit: now});
  }
  return next;
}

/**
 * Hand persistent slots over without a one-frame pop. Each slot holds
 * {id, weight}: weight 1 is a fully lit SpotLight/raymarched beam, 0 means the
 * fixture is still drawn as its faked beam. A slot whose desired occupant
 * changes first fades its current light out (its faked beam fades in), then
 * fades the newcomer in. A light that stops emitting releases its slot at once:
 * committed blackouts are never smoothed. A light entering a slot fades in only
 * if it was already visible as a faked beam (`wasFaked`); a light that has just
 * turned on, or the first frame, takes the slot at full weight. `elapsed` and
 * `fade` are seconds.
 */
export function crossfadeSlots(previous, desired, emitting, elapsed, fade = .3, wasFaked = null) {
  const step = fade > 0 ? Math.max(0, Math.min(1, elapsed / fade)) : 1;
  const current = desired.map((_, i) => {
    const slot = previous?.[i];
    return slot && slot.id != null && emitting.has(slot.id) ? {...slot} : {id: null, weight: 0};
  });
  for (let i = 0; i < current.length; i++) {
    const slot = current[i], want = emitting.has(desired[i]) ? desired[i] : null;
    if (slot.id === want) { if (want != null) slot.weight = Math.min(1, slot.weight + step); continue; }
    if (slot.id != null && slot.weight > step) { slot.weight -= step; continue; }
    // The light still fading out of another slot must finish there first.
    const elsewhere = want != null && current.some((other, j) => j !== i && other.id === want);
    current[i] = elsewhere || want == null ? {id: null, weight: 0} : {id: want, weight: wasFaked?.has(want) ? 0 : 1};
  }
  return current;
}

/** Frustum planes and eye from a column-major view-projection matrix. */
export function viewFromMatrix(elements, eye, tanHalfFov) {
  const m = elements, planes = [];
  const rows = [0, 1, 2, 3].map(r => [m[r], m[r + 4], m[r + 8], m[r + 12]]);
  for (const [row, sign] of [[0, 1], [0, -1], [1, 1], [1, -1], [2, 1], [2, -1]]) {
    const p = rows[3].map((v, i) => v + sign * rows[row][i]);
    const n = Math.hypot(p[0], p[1], p[2]) || 1;
    planes.push(p.map(v => v / n));
  }
  return {eye, planes, tanHalfFov};
}
