import assert from 'node:assert/strict';
import {readFile, writeFile} from 'node:fs/promises';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

async function compareCachedPlume({auto, grid, motion}) {
  const {telemetry} = configureShadow({auto, phase: 'fresh-plume'});
  const {fog, renderer, deviceRenderer} = engine;
  const gl = renderer.getContext();
  const ext = gl.getExtension('EXT_disjoint_timer_query_webgl2');
  if (!ext || gl.getParameter(ext.GPU_DISJOINT_EXT)) throw Error('GPU timing unavailable');
  const volume = fog.volumeMaterial, composite = fog.compositeMaterial;
  const materials = [volume, composite], originals = materials.map(material => material.fragmentShader);
  const source = originals[0];
  const math = source.slice(source.indexOf('float hash('), source.indexOf('vec2 boxHit('));
  const density = source.slice(source.indexOf('float densityAt('), source.indexOf('void integrateFog('));
  if (!math.includes('float noise(') || !density.includes('texture(densityTexture,'))
    throw Error('Missing production density functions');
  const minimum=fog.uniforms.detailMin.value, maximum=fog.uniforms.detailMax.value;
  if(minimum.x>=maximum.x || minimum.y>=maximum.y || minimum.z>=maximum.z)
    throw Error('Test did not contain active plume detail');
  const extent=maximum.clone().sub(minimum).toArray();
  const maxGrid=Number(String(grid).replace(/^iso/,''));
  const dimensions=typeof grid==='string'
    ? extent.map(length=>1+Math.ceil((maxGrid-1)*length/Math.max(...extent)))
    : [maxGrid,maxGrid,maxGrid];
  const [gx,gy,gz]=dimensions;
  const columns = Math.ceil(Math.sqrt(gz * gy / gx)), rows = Math.ceil(gz / columns);
  const width = gx * columns, height = gy * rows;
  const atlas = new THREE.WebGLRenderTarget(width, height, {
    type: THREE.HalfFloatType, format: THREE.RedFormat, minFilter: THREE.LinearFilter,
    magFilter: THREE.LinearFilter, depthBuffer: false, stencilBuffer: false,
  });
  const uniforms = Object.fromEntries(['densityTexture', 'room', 'amount', 'time', 'mixedDensity',
    'detailMin', 'detailMax'].map(name => [name, fog.uniforms[name]]));
  const atlasMaterial = new THREE.ShaderMaterial({
    uniforms,
    vertexShader: 'void main(){gl_Position=vec4(position.xy,0.,1.);}',
    fragmentShader: 'precision highp float; precision highp sampler3D;\n' +
      'uniform sampler3D densityTexture;uniform vec3 room,detailMin,detailMax;\n' +
      'uniform float amount,time,mixedDensity;out float storedDensity;\n' + math + density +
      'void main(){ivec2 texel=ivec2(gl_FragCoord.xy);\n' +
      'int z=(texel.y/' + gy + ')*' + columns + '+texel.x/' + gx + ';\n' +
      'if(z>=' + gz + '){storedDensity=0.;return;}\n' +
      'vec3 unit=vec3(texel.x%' + gx + ',texel.y%' + gy + ',z)/vec3(' + [gx-1,gy-1,gz-1].join(',') + ');\n' +
      'storedDensity=densityAt(mix(detailMin,detailMax,unit));}',
    glslVersion: THREE.GLSL3, depthTest: false, depthWrite: false, blending: THREE.NoBlending,
  });
  const geometry = new THREE.PlaneGeometry(2, 2), atlasScene = new THREE.Scene();
  const quad = new THREE.Mesh(geometry, atlasMaterial); quad.frustumCulled = false;
  atlasScene.add(quad); const atlasCamera = new THREE.Camera();
  const injected = 'uniform sampler2D plumeDensityAtlas;\n' +
    'float atlasSlice(vec2 xy,int z){ivec2 tile=ivec2(z%' + columns + ',z/' + columns + ');\n' +
    'return texture(plumeDensityAtlas,(vec2(tile*ivec2(' + gx + ',' + gy + '))+xy+.5)/vec2(' + width + '.,' + height + '. )).r;}\n' +
    'float densityAt(vec3 position){\n' +
    'if(any(lessThan(position,detailMin))||any(greaterThan(position,detailMax)))return exactDensityAt(position);\n' +
    'vec3 p=clamp((position-detailMin)/(detailMax-detailMin),vec3(0.),vec3(1.))*vec3(' + [gx-1,gy-1,gz-1].join(',') + ');\n' +
    'int first=int(p.z);int second=min(first+1,' + (gz-1) + ');\n' +
    'return mix(atlasSlice(p.xy,first),atlasSlice(p.xy,second),fract(p.z));}\n';
  const candidates = originals.map(shader => shader.replace('float densityAt(', 'float exactDensityAt(')
    .replace('void integrateFog(', injected + 'void integrateFog('));
  const draw = () => window.crossing.draw({mask: -1, bloom: .22, measure: false});
  const originalRender = renderer.render;
  const samples = [];
  let enabled = false, nextQuery = null, activeQuery = null;
  const configure = flag => {
    enabled = flag;
    materials.forEach((material, i) => {
      const next = (flag ? candidates : originals)[i];
      if (material.fragmentShader !== next) {material.fragmentShader = next; material.needsUpdate = true;}
    });
  };
  const capture = () => {
    draw();
    renderer.setRenderTarget(null);
    const rgb = new Uint8Array(gl.drawingBufferWidth * gl.drawingBufferHeight * 4);
    gl.readPixels(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight, gl.RGBA, gl.UNSIGNED_BYTE, rgb);
    const raw = new Uint16Array(fog.target.width * fog.target.height * 4);
    renderer.readRenderTargetPixels(fog.target, 0, 0, fog.target.width, fog.target.height, raw);
    return {rgb, raw};
  };
  function compare(a,b) {
    let changed=0, peak=0, squared=0, illuminated=0, noticeable=0, maxScattering=0, maxTransmission=0;
    for (let i=0;i<a.rgb.length;i+=4) {
      let error=0, level=0;
      for (let c=0;c<3;c++) {
        error=Math.max(error,Math.abs(a.rgb[i+c]-b.rgb[i+c]));
        level=Math.max(level,a.rgb[i+c],b.rgb[i+c]);
      }
      if(error)changed++;
      if(error>3)noticeable++;
      peak=Math.max(peak,error);
      if(level>=8){illuminated++;squared+=error*error;}
    }
    for(let i=0;i<a.raw.length;i++){
      const error=Math.abs(THREE.DataUtils.fromHalfFloat(a.raw[i])-THREE.DataUtils.fromHalfFloat(b.raw[i]));
      if(!Number.isFinite(error))throw Error('Nonfinite voxel comparison');
      if(i%4===3)maxTransmission=Math.max(maxTransmission,error);
      else maxScattering=Math.max(maxScattering,error);
    }
    return {changed,peak,noticeable,illuminated,litPeakChannelRms:Math.sqrt(squared/illuminated),maxScattering,maxTransmission};
  }
  for(const material of materials)material.uniforms.plumeDensityAtlas={value:atlas.texture};
  renderer.render = function (scene, camera) {
    if(scene!==fog.scene)return originalRender.call(renderer,scene,camera);
    if(fog.quad.material===volume){
      if(nextQuery){gl.finish();activeQuery=nextQuery;nextQuery=null;gl.beginQuery(ext.TIME_ELAPSED_EXT,activeQuery);}
      if(enabled) {
        const prior=renderer.getRenderTarget();
        renderer.setRenderTarget(atlas);
        try{originalRender.call(renderer,atlasScene,atlasCamera);}finally{renderer.setRenderTarget(prior);}
      }
    }
    const result=originalRender.call(renderer,scene,camera);
    if(fog.quad.material===composite && activeQuery){gl.endQuery(ext.TIME_ELAPSED_EXT);activeQuery=null;}
    return result;
  };
  try {
    const first=capture(), repeat=capture();
    configure(true);
    const candidate=capture();
    for(let iteration=0;iteration<8;iteration++)for(const cached of [false,true,true,false]){
      configure(cached);
      nextQuery=gl.createQuery();
      samples.push({cached,query:nextQuery});
      draw();
      if(nextQuery||activeQuery)throw Error('Fog timing boundaries did not close');
    }
    gl.finish();
    const deadline=performance.now()+8000;
    while(samples.some(row=>!gl.getQueryParameter(row.query,gl.QUERY_RESULT_AVAILABLE))){
      if(performance.now()>deadline)throw Error('GPU query timeout');
      await new Promise(resolve=>setTimeout(resolve,20));
    }
    if(gl.getParameter(ext.GPU_DISJOINT_EXT)||gl.getError())throw Error('Invalid GPU query or rendering');
    const elapsed=samples.map(row=>({cached:row.cached,ms:gl.getQueryParameter(row.query,gl.QUERY_RESULT)/1e6}));
    const evolving=[];
    let previousResidual;
    const initialCamera=engine.camera.position.clone();
    if(motion)for(let frame=0;frame<12;frame++){
      fog.field.advance(.05);
      fog.upload();
      engine.camera.position.copy(initialCamera);
      engine.camera.position.x+=Math.sin(frame*.24)*.28;
      engine.controls.update();
      configure(false);
      const referenceFrame=capture();
      configure(true);
      const cachedFrame=capture();
      const pixels=compare(referenceFrame,cachedFrame);
      const residual=new Int16Array(referenceFrame.rgb.length);
      let accumulated=0, count=0, peak=0;
      for(let i=0;i<residual.length;i+=4)for(let channel=0;channel<3;channel++){
        const j=i+channel;
        residual[j]=cachedFrame.rgb[j]-referenceFrame.rgb[j];
        if(previousResidual&&Math.max(referenceFrame.rgb[j],cachedFrame.rgb[j])>=8){
          const error=residual[j]-previousResidual[j];
          accumulated+=error*error; peak=Math.max(peak,Math.abs(error));count++;
        }
      }
      evolving.push({frame,time:fog.field.time,bounds:fog.uniforms.detailMax.value.clone().sub(fog.uniforms.detailMin.value).toArray(),
        pixels,temporalResidualRms:count?Math.sqrt(accumulated/count):null,temporalResidualPeak:count?peak:null});
      previousResidual=residual;
    }
    return {auto,grid,dimensions,extent,fogRaster:telemetry.fog.targetSize,deviceRenderer,
      repeat:compare(first,repeat),candidate:compare(first,candidate),
      elapsed,evolving};
  } finally {
    renderer.render=originalRender;
    configure(false);
    samples.forEach(row=>gl.deleteQuery(row.query));
    for(const material of materials)delete material.uniforms.plumeDensityAtlas;
    atlas.dispose();atlasMaterial.dispose();geometry.dispose();
  }
}

const fixture=new URL('../../dev/preview/beam-crossing-fixture.mjs',import.meta.url);
const bundle=await build({stdin:{contents:(await readFile(fixture,'utf8'))+'\nwindow.compareCachedPlume='+compareCachedPlume.toString()+';',
  resolveDir:new URL('.',fixture).pathname},bundle:true,format:'iife',write:false});
const browser=await chromium.connectOverCDP('http://127.0.0.1:9223');
const controller=await browser.newBrowserCDPSession(), context=browser.contexts()[0];
const opened=context.waitForEvent('page');
const {targetId}=await controller.send('Target.createTarget',{url:'about:blank',background:true});
let page;
try{
  page=await opened;
  const session=await context.newCDPSession(page);
  assert.equal((await session.send('Target.getTargetInfo')).targetInfo.targetId,targetId);
  const errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  page.on('console',message=>{if(message.type()==='error')errors.push(message.text());});
  await page.setContent('<title>Tolglow plume density cache comparison</title><body style="margin:0"><canvas></canvas>');
  await page.addScriptTag({content:bundle.outputFiles[0].text});
  const results=[];
  const motion=process.argv.includes('--motion');
  const args=process.argv.slice(3).filter(arg=>arg!=='--motion');
  assert.ok(args.every(arg=>/^(iso)?\d+$/.test(arg)&&Number(arg.replace(/^iso/,''))>=16&&Number(arg.replace(/^iso/,''))<=96));
  const grids=args.map(arg=>arg.startsWith('iso')?arg:Number(arg));
  for(const auto of [true,false])for(const grid of (grids.length ? grids : [32,48])){
    const result=await page.evaluate(options=>window.compareCachedPlume(options),{auto,grid,motion});
    assert.match(result.deviceRenderer,/ANGLE Metal Renderer: Apple M4/);
    assert.equal(result.repeat.changed,0);
    assert.equal(result.repeat.maxScattering,0);
    assert.equal(result.repeat.maxTransmission,0);
    results.push(result);
  }
  assert.deepEqual(errors,[]);
  const output=process.argv[2]??'./plume-density-cache.json';
  await writeFile(new URL(output,import.meta.url),JSON.stringify({browser:browser.version(),results},null,2)+'\n');
} finally {
  if(page)await page.evaluate(()=>window.crossing?.dispose()).catch(()=>{});
  await controller.send('Target.closeTarget',{targetId});
  await browser.close();
}
