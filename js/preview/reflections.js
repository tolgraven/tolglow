import * as THREE from 'three';

// Sample the finite patch rather than giving the entire cone's power to an
// axis hit. Samples obscured by truss, scenery or another head carry no flux.
const DISK_SAMPLES = [[0, 0], [.68, 0], [-.68, 0], [0, .68], [0, -.68]];
const vector = value => Array.isArray(value) && value.length === 3 && value.every(Number.isFinite);
const peak = color => Math.max(color.r, color.g, color.b);

export function incidentTransmission(field, amount, origin, hit) {
  if (amount <= 0) return 1;
  const steps = 6, [w, h, d] = field.room, [nx, ny, nz] = field.grid;
  let density = 0;
  for (let i = 0; i < steps; i++) {
    const t = (i + .5) / steps;
    const x = (origin.x + (hit.x - origin.x) * t) / w + .5;
    const y = (origin.y + (hit.y - origin.y) * t) / h;
    const z = (origin.z + (hit.z - origin.z) * t) / d + .5;
    density += field.sample(x * nx - .5, y * ny - .5, z * nz - .5);
  }
  return Math.exp(-1.25 * amount * origin.distanceTo(hit) * density / steps);
}

function candidate(entry, light, footprint) {
  if (!footprint || !['hit','center','normal','major-axis','minor-axis'].every(k => vector(footprint[k]))) return;
  const a = footprint['major-radius-m'], b = footprint['minor-radius-m'];
  if (!(a > 0 && b > 0 && Number.isFinite(a * b))) return;
  const normal = new THREE.Vector3(...footprint.normal).normalize();
  const incidence = -light.direction.dot(normal);
  if (incidence <= 1e-5) return;
  const hit = new THREE.Vector3(...footprint.hit);
  const distance = hit.clone().sub(light.position).dot(normal) / -incidence;
  if (!(distance > 0 && distance < light.distance - light.apexDistance)) return;
  // Follow an interpolated pose on the same plane. Visibility checks below
  // reject samples crossing to a different receiver between committed states.
  const axisHit = light.position.clone().addScaledVector(light.direction, distance);
  const center = new THREE.Vector3(...footprint.center).add(axisHit.clone().sub(hit));
  const irradiance = light.intensity * incidence / (distance + light.apexDistance) ** 2;
  return {id:entry.id, fixture:entry.root, surface:String(footprint['surface-id']), origin:light.position,
    center, normal, major:new THREE.Vector3(...footprint['major-axis']).multiplyScalar(a),
    minor:new THREE.Vector3(...footprint['minor-axis']).multiplyScalar(b),
    radius:Math.sqrt(a * b), power:irradiance * a * b, color:light.color};
}

export class SurfaceReflections {
  constructor(content, count, shadowSize) {
    this.objects = [];
    if (count) content.traverse(o => {if (o.isMesh) this.objects.push(o);});
    this.ray = new THREE.Raycaster(); this.hits = [];
    this.lights = Array.from({length:count}, () => {
      // Approximate diffuse patch emission with a broad outward lobe. Its own
      // shadow map keeps secondary illumination on the visible side of objects.
      const light = new THREE.SpotLight(0, 0, 15, Math.PI * .47, 1, 2);
      light.castShadow = true; light.shadow.autoUpdate = false; light.shadow.needsUpdate = true;
      light.shadow.mapSize.set(shadowSize, shadowSize);
      light.shadow.camera.near = .015; light.shadow.bias = -.0001; light.shadow.normalBias = .005;
      content.add(light, light.target); return light;
    });
    this.patches = [];
  }
  addCandidate(candidates, entry, light, footprint) {
    if (!this.lights.length || !entry.fixture['pseudo-fixture']) return;
    const patch = candidate(entry, light, footprint);
    if (patch) candidates.push(patch);
  }
  sample(patch, point) {
    const direction = point.clone().sub(patch.origin), distance = direction.length();
    this.ray.set(patch.origin, direction.normalize()); this.ray.near = .003; this.ray.far = distance + .04;
    this.hits.length = 0;
    const hits = this.ray.intersectObjects(this.objects, false, this.hits);
    const hit = hits.find(({object}) => {
      for (let parent = object; parent; parent = parent.parent) if (parent === patch.fixture) return false;
      return true;
    });
    if (hit?.object.userData.receiver !== patch.surface || Math.abs(hit.distance - distance) > .04) return;
    return hit.object.material;
  }
  resolve(candidates) {
    this.patches.length = 0;
    candidates.sort((a,b) => peak(b.color) * b.power - peak(a.color) * a.power);
    for (const patch of candidates) {
      const position = new THREE.Vector3(), reflected = new THREE.Color(0,0,0);
      let count = 0;
      for (const [x,y] of DISK_SAMPLES) {
        const point = patch.center.clone().addScaledVector(patch.major,x).addScaledVector(patch.minor,y);
        const material = this.sample(patch, point);
        if (!material) continue;
        const albedo = material.color.clone().multiplyScalar((1 - material.metalness) * (material.userData.meanMapReflectance ?? 1));
        reflected.add(albedo); position.add(point); count++;
      }
      if (!count || peak(reflected) <= 0) continue;
      patch.position = position.divideScalar(count);
      patch.reflected = reflected.multiplyScalar(1 / DISK_SAMPLES.length).multiply(patch.color);
      this.patches.push(patch);
      if (this.patches.length === this.lights.length) break;
    }
    for (let i = 0; i < this.patches.length; i++) this.lights[i].shadow.needsUpdate = true;
  }
  illuminate(field, amount) {
    this.lights.forEach((light, index) => {
      const patch = this.patches[index];
      if (!patch) {light.intensity = 0; light.color.setRGB(0,0,0); return;}
      light.color.copy(patch.reflected);
      light.intensity = patch.power * incidentTransmission(field, amount, patch.origin, patch.position);
      // A finite patch has bounded near-field irradiance. Store its radius
      // for the material shader, which softens the inverse-square denominator.
      light.userData.patchRadius = patch.radius;
      light.position.copy(patch.position).addScaledVector(patch.normal,.018);
      light.target.position.copy(light.position).add(patch.normal);
      light.target.updateMatrixWorld();
    });
  }
  dispose() { for (const light of this.lights) light.dispose(); }
}
