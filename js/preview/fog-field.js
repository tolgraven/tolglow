import {fogTransmission, fogAbsorptance} from './fog-optics.generated.js';

// Local visual fluid approximation; never authoritative fixture/effect state.
// Fixed steps, semi-Lagrangian transport and stable explicit diffusion. This is
// deliberately bounded rather than a pressure-projected CFD solver.
const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
const finite = (v, name) => { if (!Number.isFinite(v)) throw new Error(`Invalid ${name}`); return v; };
export const FOG_QUALITY = Object.freeze({
  high: Object.freeze({ grid: [48, 24, 48], scale: 0.67, steps: 72, shadows: 4 }),
  desktop: Object.freeze({ grid: [32, 18, 32], scale: 0.5, steps: 48, shadows: 4 }),
  mobile: Object.freeze({ grid: [24, 14, 24], scale: 0.4, steps: 32, shadows: 2 }),
  phone: Object.freeze({ grid: [20, 12, 20], scale: 0.33, steps: 24, shadows: 1 })
});

export function intersectRoom(origin, direction, room) {
  const lo = [-room[0] / 2, 0, -room[2] / 2], hi = [room[0] / 2, room[1], room[2] / 2];
  let near = 0, far = Infinity;
  for (let axis = 0; axis < 3; axis++) {
    if (Math.abs(direction[axis]) < 1e-9) { if (origin[axis] < lo[axis] || origin[axis] > hi[axis]) return null; }
    else {
      let a = (lo[axis] - origin[axis]) / direction[axis], b = (hi[axis] - origin[axis]) / direction[axis];
      if (a > b) [a, b] = [b, a];
      near = Math.max(near, a); far = Math.min(far, b);
      if (far <= near) return null;
    }
  }
  return [near, far];
}

export function integrateHomogeneous(density, distance, incident = 0, albedo = 0.92) {
  const transmittance = fogTransmission(1, density, distance);
  return { transmittance, inscatter: incident * albedo * fogAbsorptance(1, density, distance) };
}

// Convex positive cone, clipped to the requested view-ray interval. Range is
// spherical, matching Three's distance attenuation. This CPU reference mirrors
// the shader and catches very narrow shafts that point sampling would miss.
export function intersectCone(origin, direction, position, axis, angle, interval, range = 0, minimumAxial = 0) {
  let [lo, hi] = interval;
  const dot = (a, b) => a.reduce((sum, value, i) => sum + value * b[i], 0);
  const q = origin.map((v, i) => v - position[i]), axial = dot(q, axis), slope = dot(direction, axis);
  if (Math.abs(slope) < 1e-9) { if (axial < minimumAxial) return null; }
  else if (slope > 0) lo = Math.max(lo, (minimumAxial - axial) / slope);
  else hi = Math.min(hi, (minimumAxial - axial) / slope);
  if (range > 0) {
    const b = dot(q, direction), discriminant = b * b - dot(q, q) + range * range;
    if (discriminant < 0) return null;
    lo = Math.max(lo, -b - Math.sqrt(discriminant)); hi = Math.min(hi, -b + Math.sqrt(discriminant));
  }
  const cos2 = Math.cos(angle) ** 2, a = slope * slope - cos2;
  const b = 2 * (axial * slope - cos2 * dot(q, direction)), c = axial * axial - cos2 * dot(q, q);
  if (Math.abs(a) < 1e-9) {
    if (Math.abs(b) < 1e-9) { if (c < 0) return null; }
    else if (b > 0) lo = Math.max(lo, -c / b); else hi = Math.min(hi, -c / b);
  } else {
    const discriminant = b * b - 4 * a * c;
    if (discriminant < 0) { if (a < 0) return null; }
    else {
      let first = (-b - Math.sqrt(discriminant)) / (2 * a), last = (-b + Math.sqrt(discriminant)) / (2 * a);
      if (first > last) [first, last] = [last, first];
      if (a < 0) { lo = Math.max(lo, first); hi = Math.min(hi, last); }
      else if (slope > 0) lo = Math.max(lo, last); else hi = Math.min(hi, first);
    }
  }
  return hi > lo ? [lo, hi] : null;
}

export class FogField {
  constructor({ grid = FOG_QUALITY.desktop.grid, room = [12, 5, 10], initialHaze = 0.07 } = {}) {
    if (!Array.isArray(grid) || grid.length !== 3 || grid.some(n => !Number.isInteger(n) || n < 4 || n > 64)) throw new Error('Invalid fog grid');
    this.grid = [...grid]; this.initialHaze = clamp(finite(initialHaze, 'haze'), 0, 1);
    const count = grid[0] * grid[1] * grid[2];
    this.data = new Float32Array(count); this.temperature = new Float32Array(count);
    this.advected = new Float32Array(count); this.advectedHeat = new Float32Array(count);
    this.next = new Float32Array(count); this.nextHeat = new Float32Array(count);
    this.steps = 0; this.time = 0; this.accumulator = 0; this.droppedSeconds = 0; this.sources = [];
    this.setRoom(room);
  }

  setRoom(room) {
    if (!Array.isArray(room) || room.length !== 3 || room.some(v => !Number.isFinite(v) || v <= 0)) throw new Error('Invalid fog room');
    if (this.room?.every((v, i) => v === room[i])) return false;
    this.room = [...room]; this.spacing = room.map((v, i) => v / this.grid[i]);
    this.clear(this.initialHaze);
    return true;
  }

  clear(haze = 0) {
    finite(haze, 'haze');
    this.data.fill(clamp(haze, 0, 6)); this.temperature.fill(0);
    this.sources = []; this.accumulator = 0; this.time = 0; this.steps = 0; this.droppedSeconds = 0;
  }

  copyFrom(field) {
    if (!(field instanceof FogField)) throw new Error('Expected an owned fog field');
    this.setRoom(field.room);
    const [nx, ny, nz] = this.grid, [ox, oy, oz] = field.grid;
    for (let z = 0; z < nz; z++) for (let y = 0; y < ny; y++) for (let x = 0; x < nx; x++) {
      const i = x + y * nx + z * nx * ny;
      const px = (x + .5) * ox / nx - .5, py = (y + .5) * oy / ny - .5, pz = (z + .5) * oz / nz - .5;
      this.data[i] = field.sample(px, py, pz);
      this.temperature[i] = field.sample(px, py, pz, field.temperature);
    }
    this.sources = field.sources.map(source => ({...source, position: [...source.position], direction: [...source.direction]}));
    for (const key of ['time', 'steps', 'accumulator', 'droppedSeconds']) this[key] = field[key];
  }

  burst({ position = [-this.room[0] * 0.34, 0.22, -this.room[2] * 0.24], direction = [0.7, 0.12, 0.7], duration = 2.8, strength = 1.2 } = {}) {
    if (![position, direction].every(v => Array.isArray(v) && v.length === 3 && v.every(Number.isFinite))) throw new Error('Invalid fog source');
    const length = Math.hypot(...direction);
    if (length < 1e-6) throw new Error('Fog source needs a direction');
    const normalized = direction.map(v => v / length);
    const source = { position: [...position], direction: normalized,
      remaining: clamp(finite(duration, 'burst duration'), 0, 8), strength: clamp(finite(strength, 'burst strength'), 0, 8) };
    // A bounded number of independent machines/pulses. Repeated default bursts
    // extend the same source instead of accumulating an unbounded emitter list.
    const existing = this.sources.find(s => s.position.every((v, i) => v === position[i]));
    if (existing) { existing.remaining = Math.min(8, existing.remaining + source.remaining); existing.strength = source.strength; }
    else { if (this.sources.length >= 4) this.sources.shift(); this.sources.push(source); }
  }

  sample(x, y, z, values = this.data) {
    const [nx, ny, nz] = this.grid;
    x = clamp(x, 0, nx - 1); y = clamp(y, 0, ny - 1); z = clamp(z, 0, nz - 1);
    const ix = Math.floor(x), iy = Math.floor(y), iz = Math.floor(z);
    const jx = Math.min(ix + 1, nx - 1), jy = Math.min(iy + 1, ny - 1), jz = Math.min(iz + 1, nz - 1);
    const fx = x - ix, fy = y - iy, fz = z - iz, stride = nx * ny;
    const a = ix + iy * nx + iz * stride, b = jx + iy * nx + iz * stride;
    const c = ix + jy * nx + iz * stride, d = jx + jy * nx + iz * stride;
    const offset = (jz - iz) * stride;
    const near = (values[a] * (1 - fx) + values[b] * fx) * (1 - fy) + (values[c] * (1 - fx) + values[d] * fx) * fy;
    const far = (values[a + offset] * (1 - fx) + values[b + offset] * fx) * (1 - fy) + (values[c + offset] * (1 - fx) + values[d + offset] * fx) * fy;
    return near * (1 - fz) + far * fz;
  }

  advance(dt) {
    if (!Number.isFinite(dt) || dt < 0) throw new Error('Fog dt must be finite and nonnegative');
    const accepted = Math.min(dt, 0.1);
    this.droppedSeconds += dt - accepted; this.accumulator += accepted;
    let steps = 0;
    while (this.accumulator + 1e-10 >= 0.05 && steps < 2) {
      this.step(0.05); this.accumulator = Math.max(0, this.accumulator - 0.05); steps++;
    }
    return steps;
  }

  step(dt) {
    const [nx, ny, nz] = this.grid, [sx, sy, sz] = this.spacing, stride = nx * ny;
    const a = this.data, heat = this.temperature, b = this.advected, h = this.advectedHeat;
    const time = this.time, w = this.room[0], d = this.room[2];
    for (let z = 0; z < nz; z++) for (let y = 0; y < ny; y++) for (let x = 0; x < nx; x++) {
      const i = x + y * nx + z * stride, px = (x + 0.5) * sx - w / 2, py = (y + 0.5) * sy, pz = (z + 0.5) * sz - d / 2;
      // Divergence-free analytic curl components plus cooling-dependent buoyancy.
      const turbulence = Math.min(1, heat[i]) * .17;
      let vx = 0.12 + 0.11 * (Math.cos(py * 1.7 + time * 0.43) - Math.sin(pz * 1.3 - time * 0.31))
        + turbulence * (Math.cos(py * 4.1 - time * .8) - Math.sin(pz * 3.7 + time * .7));
      let vy = heat[i] * 0.38 + 0.035 + 0.08 * (Math.sin(pz * 1.3 + time * 0.31) - Math.cos(px * 1.5 + time * 0.43))
        + turbulence * (Math.sin(pz * 3.7 + time * .7) - Math.cos(px * 4.3 - time * .8));
      let vz = 0.025 + 0.11 * (Math.cos(px * 1.5 - time * 0.43) - Math.sin(py * 1.7 + time * 0.31))
        + turbulence * (Math.cos(px * 4.3 - time * .8) - Math.sin(py * 4.1 + time * .7));
      let injection = 0;
      for (const source of this.sources) {
        const dx = px - source.position[0], dy = py - source.position[1], dz = pz - source.position[2];
        const along = dx * source.direction[0] + dy * source.direction[1] + dz * source.direction[2];
        if (along < -0.2 || along > 1.8) continue;
        const radius = 0.2 + Math.max(0, along) * 0.24;
        const radial = Math.max(0, dx * dx + dy * dy + dz * dz - along * along);
        const shape = Math.exp(-radial / (2 * radius * radius) - ((along - 0.45) / 0.85) ** 2);
        // Pulses are deposited into the evolving density, not drawn as a moving
        // overlay. Hot, concentrated jet material folds before room diffusion.
        const pulse = Math.sin(along * 8 - time * 5.5 + py * 3);
        const billow = 0.72 + 0.28 * pulse * pulse;
        const amount = shape * billow * source.strength;
        injection += amount;
        vx += source.direction[0] * amount * 0.9; vy += source.direction[1] * amount * 0.9; vz += source.direction[2] * amount * 0.9;
      }
      b[i] = Math.min(6, this.sample(x - dt * vx / sx, y - dt * vy / sy, z - dt * vz / sz, a) + injection * dt * 3.2);
      h[i] = Math.min(4, this.sample(x - dt * vx / sx, y - dt * vy / sy, z - dt * vz / sz, heat) + injection * dt * 2.4);
    }
    // First-order transport already has numerical diffusion. Adding the old
    // .075m²/s diffusivity erased fresh jet folds within a few timesteps.
    let cx = dt * 0.025 / (sx * sx), cy = dt * 0.025 / (sy * sy), cz = dt * 0.025 / (sz * sz);
    const stability = Math.min(1, 0.45 / (cx + cy + cz)); cx *= stability; cy *= stability; cz *= stability;
    const densityDecay = Math.exp(-dt / 100), cooling = Math.exp(-dt / 4.5);
    for (let z = 0; z < nz; z++) for (let y = 0; y < ny; y++) for (let x = 0; x < nx; x++) {
      const i = x + y * nx + z * stride;
      const left = x ? i - 1 : i, right = x < nx - 1 ? i + 1 : i;
      const down = y ? i - nx : i, up = y < ny - 1 ? i + nx : i;
      const back = z ? i - stride : i, front = z < nz - 1 ? i + stride : i;
      this.next[i] = Math.max(0, (b[i] + cx * (b[left] + b[right] - 2 * b[i]) + cy * (b[down] + b[up] - 2 * b[i]) + cz * (b[back] + b[front] - 2 * b[i])) * densityDecay);
      this.nextHeat[i] = Math.max(0, (h[i] + cx * (h[left] + h[right] - 2 * h[i]) + cy * (h[down] + h[up] - 2 * h[i]) + cz * (h[back] + h[front] - 2 * h[i])) * cooling);
    }
    [this.data, this.next] = [this.next, this.data]; [this.temperature, this.nextHeat] = [this.nextHeat, this.temperature];
    for (const source of this.sources) source.remaining -= dt;
    this.sources = this.sources.filter(s => s.remaining > 1e-8);
    this.time += dt; this.steps++;
  }

  stats() {
    let sum = 0, maximum = 0, heated = 0;
    for (let i = 0; i < this.data.length; i++) { sum += this.data[i]; maximum = Math.max(maximum, this.data[i]); heated += this.temperature[i]; }
    return { cells: this.data.length, steps: this.steps, time: this.time, droppedSeconds: this.droppedSeconds,
      meanDensity: sum / this.data.length, maximumDensity: maximum, heat: heated / this.data.length,
      activeSources: this.sources.length, massEstimate: sum * this.spacing[0] * this.spacing[1] * this.spacing[2] };
  }
}
