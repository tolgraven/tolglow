// Choose identities only when a committed sample or the editor selection changes.
// GPU resources keep their persistent slots, including the shadow-mapped slots.
function keepSlots(previous, preferred) {
  const desired = new Set(preferred), residents = new Set(previous);
  const newcomers = preferred.filter(id => !residents.has(id));
  let next = 0;
  return previous.map(id => desired.has(id) ? id : newcomers[next++]);
}

export function fixtureSlots(fixtures, values, selectedId, meshes, lights, shadows) {
  const residentLights = new Set(lights);
  const active = [];
  for (const [id, row] of Object.entries(values)) {
    const spec = fixtures.get(id);
    const rgb = row?.['emitted-color'];
    if (!spec || !Array.isArray(rgb) || rgb.length !== 3 || !rgb.every(Number.isFinite)) continue;
    const level = Math.max(...rgb);
    if (level <= 0) continue;
    const moving = spec.channels?.pan != null && spec.channels?.tilt != null;
    active.push({id, level, moving});
  }
  active.sort((a, b) => Number(b.id === selectedId) - Number(a.id === selectedId)
    || Number(b.moving) - Number(a.moving) || b.level - a.level
    || Number(residentLights.has(b.id)) - Number(residentLights.has(a.id))
    || (a.id < b.id ? -1 : 1));
  // Keep an emitting strip/static fixture visible among a wall of movers.
  // Resolve mixed occupancy before assigning expensive shadow and mesh slots.
  const lit = Array.from(new Set([active[0], active.find(entry => !entry.moving), ...active]
    .filter(Boolean).map(entry => entry.id)));
  const preferred = new Set();
  if (fixtures.has(selectedId)) preferred.add(selectedId);
  for (const id of lit) preferred.add(id);
  for (const id of meshes) preferred.add(id);
  const meshIds = keepSlots(meshes, Array.from(preferred).slice(0, meshes.length));
  const meshSet = new Set(meshIds);

  // The slots with allocated shadow maps get first choice. Reuse occupants
  // within each group and only fill gaps when an emitter actually moves.
  const order = new Set(lit.filter(id => meshSet.has(id)));
  for (const id of lights) if (meshSet.has(id)) order.add(id);
  for (const id of meshIds) order.add(id);
  const desired = Array.from(order).slice(0, lights.length);
  return {meshes: meshIds, lights: [
    ...keepSlots(lights.slice(0, shadows), desired.slice(0, shadows)),
    ...keepSlots(lights.slice(shadows), desired.slice(shadows))
  ]};
}
