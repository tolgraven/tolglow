import assert from 'node:assert/strict';
import {readFile, writeFile} from 'node:fs/promises';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

async function compareCapacity({auto, motion, wide}) {
  const {telemetry} = configureShadow({auto, phase: 'fresh-plume'});
  const {fog, renderer, deviceRenderer} = engine;
  if (wide) {
    fog.field.clear(.15);
    fog.burst({position: [-4.6,.25,2.8], direction: [1,.4,-.45], duration: 5, strength: 6});
    fog.burst({position: [4.6,.4,-2.8], direction: [-1,.65,.55], duration: 5, strength: 6});
    fog.burst({position: [-2.3,1.75,-2.8], direction: [.65,-.15,1], duration: 5, strength: 4});
    for (let step = 0; step < 70; step++) fog.field.advance(.05);
    fog.upload();
  }
  const gl = renderer.getContext();
  const ext = gl.getExtension('EXT_disjoint_timer_query_webgl2');
  if (!ext || gl.getParameter(ext.GPU_DISJOINT_EXT)) throw Error('GPU timing unavailable');
  const volume = fog.volumeMaterial, composite = fog.compositeMaterial;
  const materials = [volume, composite], originals = materials.map(material => material.fragmentShader);
  const rayArrays = 'vec4 lightIntervals[16];int rayLights[16];';
  const buildRays = 'for(int i=0;i<16;i++){\n  if(i>=lightCount)break;';
  const lightLoop = 'for(int light=0;light<16;light++){';
  const candidates = originals.map(shader => {
    for (const token of [rayArrays,buildRays,lightLoop]) {
      if (shader.split(token).length !== 2) throw Error('Expected matched fog shader component: '+token);
      shader = shader.replace(token,token.replaceAll('16','8'));
    }
    return shader;
  });
  const draw = () => {
    const value = window.crossing.draw({mask: -1, bloom: .22, measure: false});
    if (fog.uniforms.lightCount.value !== 8) throw Error('This capacity test needs exactly eight lights');
    return value;
  };
  const originalRender = renderer.render;
  const samples = [];
  let nextQuery = null, activeQuery = null;
  const configure = flag => {
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
  renderer.render = function (scene, camera) {
    if(scene!==fog.scene)return originalRender.call(renderer,scene,camera);
    if(fog.quad.material===volume){
      if(nextQuery){gl.finish();activeQuery=nextQuery;nextQuery=null;gl.beginQuery(ext.TIME_ELAPSED_EXT,activeQuery);}

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
    if(motion)for(let frame=0;frame<(wide?30:12);frame++){
      for(let step=0;step<(wide?10:1);step++)fog.field.advance(.05);
      fog.upload();
      engine.camera.position.copy(initialCamera);
      engine.camera.position.x+=Math.sin(frame*.24)*(wide?1.2:.28);
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
    return {auto,wide,fogRaster:telemetry.fog.targetSize,deviceRenderer,
      repeat:compare(first,repeat),candidate:compare(first,candidate),
      elapsed,evolving};
  } finally {
    renderer.render=originalRender;
    configure(false);
    samples.forEach(row=>gl.deleteQuery(row.query));
  }
}

const fixture=new URL('../../dev/preview/beam-crossing-fixture.mjs',import.meta.url);
const bundle=await build({stdin:{contents:(await readFile(fixture,'utf8'))+'\nwindow.compareCapacity='+compareCapacity.toString()+';',
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
  await page.setContent('<title>Tolglow exact fog light capacity</title><body style="margin:0"><canvas></canvas>');
  await page.addScriptTag({content:bundle.outputFiles[0].text});
  const results=[];
  const motion=process.argv.includes('--motion');
  for (const wide of [false,true]) for (const auto of [true,false]) {
    const result=await page.evaluate(options=>window.compareCapacity(options),{auto,motion,wide});
    assert.match(result.deviceRenderer,/ANGLE Metal Renderer: Apple M4/);
    assert.equal(result.repeat.changed,0);
    assert.equal(result.repeat.maxScattering,0);
    assert.equal(result.repeat.maxTransmission,0);
    results.push(result);
  }
  assert.deepEqual(errors,[]);
  const output=process.argv[2]??'./fog-eight-light-capacity.json';
  await writeFile(new URL(output,import.meta.url),JSON.stringify({browser:browser.version(),results},null,2)+'\n');
} finally {
  if(page)await page.evaluate(()=>window.crossing?.dispose()).catch(()=>{});
  await controller.send('Target.closeTarget',{targetId});
  await browser.close();
}
