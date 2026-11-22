// Project accepted equivalent fog volumes into the visual density grid. The
// host owns emission and mixing; parcel positions and shapes are display data.
const clamp = (x, a, b) => Math.max(a, Math.min(b, x));
const nonnegative = (x) => Number.isFinite(x) && x >= 0;
const vector = (v) => Array.isArray(v) && v.length === 3 && v.every(Number.isFinite);
const MAX_PARCELS = 8;

function parcel(source, time, mass) {
  return {
    position: [...source.position],
    direction: [...source.direction],
    flow: source['flow-m3-s'],
    time,
    mass
  };
}
function mergeParcels(a, b) {
  const total = a.mass + b.mass,
    share = b.mass / total;
  const mix = (x, y) => x + share * (y - x);
  return {
    mass: total,
    time: mix(a.time, b.time),
    flow: mix(a.flow, b.flow),
    position: a.position.map((x, i) => mix(x, b.position[i])),
    direction: a.direction.map((x, i) => mix(x, b.direction[i]))
  };
}
function reconcile(source, previous, time) {
  const local = source['local-m3'],
    emitted = source['emitted-m3'];
  if (!local) return { emitted, parcels: [] };
  if (!previous || emitted < previous.emitted) {
    // A join has no spatial history. Reconstruct the currently present local
    // mass, with younger material near an operating nozzle.
    const ages = source['flow-m3-s'] > 0 ? [0.15, 1, 2.8] : [1, 2.8, 4.5];
    return {
      emitted,
      parcels: ages
        .map((age, i) => parcel(source, time - age * 1000, local * [0.25, 0.45, 0.3][i]))
        .reverse()
    };
  }
  const fresh = Math.min(local, emitted - previous.emitted),
    old = local - fresh;
  const parcels = previous.parcels,
    total = parcels.reduce((sum, p) => sum + p.mass, 0);
  if (!old) parcels.length = 0;
  else if (total) for (const p of parcels) p.mass *= old / total;
  else parcels.push(parcel(source, time - 1500, old));
  if (fresh) {
    const p = parcel(source, time, fresh),
      last = parcels.at(-1);
    if (last && Math.floor(time / 180) === Math.floor(last.time / 180))
      parcels[parcels.length - 1] = mergeParcels(last, p);
    else parcels.push(p);
  }
  if (parcels.length > MAX_PARCELS) parcels.splice(0, 2, mergeParcels(parcels[0], parcels[1]));
  return { emitted, parcels };
}

// Deposit a normalized ellipsoid. Width and direction evolve with age and
// output rate, while integrating the voxel density returns the parcel volume.
function deposit(field, p, time) {
  const age = Math.max(0, (time - p.time) * 0.001),
    grid = field.grid,
    spacing = field.spacing,
    room = field.room;
  const length = Math.hypot(...p.direction) || 1,
    axis = p.direction.map((x) => x / length);
  const speed = Math.min(4.5, 0.45 + Math.sqrt(p.flow));
  const travel = speed * (1 - Math.exp(-age / 1.4)) + 0.12;
  const r = 0.12 + 0.15 * Math.sqrt(age) + 0.035 * Math.min(age, 15);
  const longitudinal = r + 0.3 * Math.exp(-age / 3);
  const drift = Math.min(age, 12),
    phase = p.position[0] * 0.9 + p.position[2] * 0.65;
  const movement = [
    0.07 * drift + 0.1 * Math.sin(phase + drift) * Math.min(1, drift),
    0.11 * drift,
    0.05 * drift + 0.09 * Math.sin(phase + drift * 0.6) * Math.min(1, drift)
  ];
  const min = [-room[0] / 2, 0, -room[2] / 2];
  const center = p.position.map((v, i) =>
    clamp(v + travel * axis[i] + movement[i], min[i], min[i] + room[i])
  );
  // Integrate a sub-voxel nozzle over the cell, including in a large hall
  // where a cell can be wider than the initial stream.
  const sigma = spacing.map((s) => Math.max(r, 0.48 * s)),
    reach = sigma.map((s) => 3 * Math.max(s, longitudinal));
  const index = (i, v) => clamp(Math.floor((v - min[i]) / spacing[i]), 0, grid[i] - 1);
  const lo = center.map((v, i) => index(i, v - reach[i])),
    hi = center.map((v, i) => index(i, v + reach[i]));
  const cells = field.advected,
    axialSigma = Math.max(longitudinal, 0.48 * Math.hypot(...spacing));
  let sum = 0;
  for (let z = lo[2]; z <= hi[2]; z++)
    for (let y = lo[1]; y <= hi[1]; y++)
      for (let x = lo[0]; x <= hi[0]; x++) {
        const dx = (x + 0.5) * spacing[0] + min[0] - center[0],
          dy = (y + 0.5) * spacing[1] + min[1] - center[1],
          dz = (z + 0.5) * spacing[2] + min[2] - center[2];
        const along = dx * axis[0] + dy * axis[1] + dz * axis[2];
        const weight = Math.exp(
          -0.5 *
            (((dx - along * axis[0]) / sigma[0]) ** 2 +
              ((dy - along * axis[1]) / sigma[1]) ** 2 +
              ((dz - along * axis[2]) / sigma[2]) ** 2 +
              (along / axialSigma) ** 2)
        );
        cells[x + grid[0] * (y + grid[1] * z)] = weight;
        sum += weight;
      }
  if (!sum) return;
  const density = p.mass / (sum * spacing[0] * spacing[1] * spacing[2]);
  for (let z = lo[2]; z <= hi[2]; z++)
    for (let y = lo[1]; y <= hi[1]; y++)
      for (let x = lo[0]; x <= hi[0]; x++) {
        const i = x + grid[0] * (y + grid[1] * z);
        field.data[i] += density * cells[i];
      }
}

export class ShowAtmosphere {
  constructor() {
    this.sources = new Map();
    this.sample = null;
    this.rebase = true;
    this.paintAt = null;
  }
  suspend() {
    this.rebase = true;
  }
  accept(identity, sample) {
    const time = sample['sample-time-ms'];
    if (
      !Number.isFinite(time) ||
      !nonnegative(sample['mixed-density']) ||
      !Array.isArray(sample.sources)
    )
      return false;
    const epoch = JSON.stringify([identity, sample.epoch]),
      prior = this.sample?.['sample-time-ms'];
    if (epoch === this.epoch && !this.rebase && time <= prior) return false;
    const fresh = this.rebase || epoch !== this.epoch || time - prior > 1000;
    const next = new Map(),
      priorVolume = this.localVolume,
      priorDensity = this.sample?.['mixed-density'];
    this.localVolume = 0;
    for (const s of sample.sources) {
      const id = s['fixture-id'];
      if (
        id == null ||
        !vector(s.position) ||
        !vector(s.direction) ||
        !['local-m3', 'emitted-m3', 'flow-m3-s'].every((k) => nonnegative(s[k]))
      )
        continue;
      this.localVolume += s['local-m3'];
      next.set(id, reconcile(s, fresh ? null : this.sources.get(id), time));
    }
    this.sources = next;
    this.epoch = epoch;
    this.sample = sample;
    this.rebase = false;
    this.elapsed = 0;
    this.dirty ||=
      fresh || this.localVolume > 0 || priorVolume > 0 || priorDensity !== sample['mixed-density'];
    return true;
  }
  paint(field, dt) {
    if (!this.sample) return false;
    this.elapsed = Math.min(0.1, this.elapsed + Math.max(0, dt));
    const time = this.sample['sample-time-ms'] + 1000 * this.elapsed;
    if (
      !this.dirty &&
      this.field === field &&
      (!this.localVolume || Math.abs(time - this.paintAt) < 49.99)
    )
      return false;
    field.data.fill(this.sample['mixed-density']);
    field.temperature.fill(0);
    field.sources = [];
    for (const { parcels } of this.sources.values()) for (const p of parcels) deposit(field, p, time);
    field.time = (time * 0.001) % 10000;
    field.steps++;
    this.dirty = false;
    this.paintAt = time;
    this.field = field;
    return true;
  }
  stats() {
    return {
      sampleTimeMs: this.sample?.['sample-time-ms'],
      mixedDensity: this.sample?.['mixed-density'],
      mixedVolumeM3: this.sample?.['mixed-m3'],
      roomVolumeM3: this.sample?.['volume-m3'],
      localVolumeM3: this.localVolume,
      foggerCount: this.sources.size
    };
  }
}
