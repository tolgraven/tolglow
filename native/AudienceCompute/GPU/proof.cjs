// Run the iPhone's bundled artifact against the paired Afterglow ES300 fixture.
const {chromium} = require('playwright-core');
const {readFileSync} = require('node:fs');
const {resolve} = require('node:path');
const assert = require('node:assert/strict');
const transit = require('transit-js');
const k = transit.keyword;
const write = x => transit.writer('json').write(x);
const map = (...items) => transit.map(items);
const src = resolve(process.env.TOLGLOW_COMPUTE_SOURCE || '.', 'test/js/compute/fixture.json');
const fixture = JSON.parse(readFileSync(src));
const entries = x => Array.isArray(x) ? x.map(entries) : x && typeof x === 'object' ?
  transit.map(Object.entries(x).flatMap(([key,value]) => [k(key),entries(value)])) : x;
const bytes = xs => new Uint8Array(new Float32Array(xs).buffer);
async function main() {
  const browser=await chromium.launch({executablePath:process.argv[2],headless:true,
    args:['--no-sandbox','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
  try {
    const page=await browser.newPage();
    await page.addScriptTag({path:resolve(__dirname,'../Phone/audience-compute.js')});
    assert(await page.evaluate(async () => {window.gpu=AudienceGPU.start(document.createElement('canvas'));return gpu.probe();}));
    async function call(packet) {
      const result=await page.evaluate(s=>gpu.compute(s),write(packet));
      return transit.reader('json').read(result[0]);
    }
    const prepared=await call(map(k('op'),k('prepare'),k('id'),'proof',k('positions'),bytes(fixture.positions),
      k('surface-uvs'),bytes(fixture.surfaceUvs),k('plan'),entries(fixture.plan)));
    assert.equal(prepared.get(k('op')).name(),'ready');
    const reply=await call(map(k('op'),k('run'),k('id'),'proof',k('job'),'1',k('start'),0,k('count'),4,
      k('uniforms'),transit.map(Object.entries(fixture.uniforms).flatMap(([key,value])=>[key,value]))));
    assert.equal(reply.get(k('op')).name(),'result');
    const raw=reply.get(k('values')), view=new DataView(raw.buffer,raw.byteOffset,raw.byteLength);
    assert.equal(raw.byteLength,fixture.expected.length*4);
    fixture.expected.forEach((n,i)=>assert(Math.abs(view.getFloat32(i*4,true)-n)<1e-5));
    console.log('Bundled guest WebGL compiled the paired GLSL, retained the program and matched',fixture.expected.length,'CPU fields.');
  } finally {await browser.close();}
}
main().catch(e=>{console.error(e);process.exitCode=1;});
