// This probe runs with the generated preview in CI and on physical WebGL.
export async function measureShadowCache() {
  const {SceneRenderer} = await import('/preview.js');
  const engine = new SceneRenderer(new OffscreenCanvas(1, 1), null, {worker: true, autoStart: false,
    reducedMotion: true, coarsePointer: false, displayHz: 60, viewport: {width: 240, height: 150, pixelRatio: 1}});
  const key = {id: 'key', position: [-1, 3, .4], channels: {pan: 1, tilt: 2}, 'zoom-range-deg': [28, 64]};
  const fill = {...key, id: 'fill', position: [1, 3, .4]};
  const dark = {...key, id: 'dark', position: [-2, 2, 0]};
  let snapshot = {'show-id': 'shadow-cache', epoch: 1, revision: 1, 'frame-number': 1, 'time-ms': 25,
    rig: {room: {width: 12, height: 5, depth: 10}, fixtures: [key, fill, dark],
      trusses: [{id: 'truss', position: [0, 3.8, -1], length: 3}]},
    output: {values: {key: {direction: [.12, -1, -.2], 'emitted-color': [1, .7, .4]},
      fill: {direction: [-.12, -1, -.2], 'emitted-color': [.8, .4, .7]},
      dark: {direction: [0, -1, 0], 'emitted-color': [0, 0, 0]}}}};
  try {
    if (engine.error) throw Error(engine.error);
    engine.setQuality('mobile'); engine.setWorkLight(0); engine.update(snapshot); engine.draw(0);
    const renderer = engine.renderer, gl = renderer.getContext();
    const shadowIds = () => engine.spots.filter(light => light.castShadow).map(light => light.shadow.map.depthTexture.uuid);
    const originalMaps = shadowIds();
    const setTarget = renderer.setRenderTarget.bind(renderer);
    let refreshed = new Set();
    renderer.setRenderTarget = (target, ...args) => {
      const slot = target ? engine.spots.findIndex(light => light.shadow.map === target) : -1;
      if (slot >= 0) refreshed.add(engine.lightIds[slot]);
      return setTarget(target, ...args);
    };
    const draw = () => {
      refreshed = new Set(); engine.draw(0); renderer.setRenderTarget(null);
      const {drawingBufferWidth: width, drawingBufferHeight: height} = gl;
      const pixels = new Uint8Array(width * height * 4);
      gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
      return {pixels, refreshed: Array.from(refreshed).sort()};
    };
    const compare = () => {
      const cached = draw();
      const shadows = engine.spots.map(light => light.shadow);
      for (const shadow of shadows) shadow.autoUpdate = true;
      const forced = draw();
      for (const shadow of shadows) shadow.autoUpdate = false;
      return {cached: cached.refreshed, forced: forced.refreshed,
        identical: cached.pixels.every((value, i) => value === forced.pixels[i])};
    };
    const edit = change => {
      snapshot = structuredClone(snapshot);
      snapshot['frame-number']++; snapshot['time-ms'] += 25;
      change(snapshot);
      engine.update(snapshot);
      return compare();
    };

    const fixed = compare();
    engine.camera.position.x += .35; const camera = compare();
    engine.setFog(.8); const fog = compare();
    const color = edit(frame => {frame.output.values.key['emitted-color'] = [1, .6, .5];});
    const zoom = edit(frame => {frame.output.values.key.zoom = .55;});
    const aim = edit(frame => {frame.output.values.key.direction = [.2, -1, .1];});
    const unlit = edit(frame => {frame.output.values.dark.direction = [.2, -1, .1];});
    const truss = edit(frame => {frame.rig.trusses[0].position = [-.6, 1.9, .1];});
    const mount = edit(frame => {frame.rig.fixtures[1].position = [1.3, 2.9, .5];});
    const mapsStayedResident = originalMaps.every((id, index) => id === shadowIds()[index]);
    engine.restoreContext(); const restore = compare();
    return {fixed, camera, fog, color, zoom, aim, unlit, truss, mount, restore,
      mapsStayedResident, device: engine.deviceRenderer, glError: gl.getError()};
  } finally {engine.dispose();}
}
