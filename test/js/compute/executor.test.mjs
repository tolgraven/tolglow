import test from 'node:test';
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {existsSync, readFileSync} from 'node:fs';
import {chromium} from 'playwright-core';

// The stored shader and reference fields were produced by emit-glsl-batch and
// evaluate-batch from the expressions, meter, coordinates and params in this file.
const fixture = JSON.parse(readFileSync(new URL('./fixture.json', import.meta.url)));
const modules = new Map(['worker','executor','dispatcher'].map(name => [`/${name}.mjs`, readFileSync(new URL(`../../../js/compute/${name}.mjs`, import.meta.url))]));

test('two real browser workers shard an Afterglow CPU reference, retain the rig and retire invalid work',
  {timeout: 90000}, async t => {
    const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
    if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for browser GPU tests');
    const server = createServer((request, response) => {
      const module = modules.get(request.url);
      response.setHeader('Content-Type', module ? 'text/javascript' : 'text/html');
      response.end(module || '<!doctype html><title>GPU compute fixture</title>');
    });
    await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
    let browser;
    try {
      browser = await chromium.launch({executablePath, args:['--use-angle=swiftshader','--enable-unsafe-swiftshader']});
      const pages = await Promise.all([0,1].map(async () => {
        const context = await browser.newContext(), page = await context.newPage();
        await page.goto(`http://127.0.0.1:${server.address().port}`);
        await page.evaluate(() => {
          const worker = new Worker('/worker.mjs', {type:'module'}), waiting = new Map();
          let next = 0;
          worker.onmessage = ({data}) => {
            const receive = waiting.get(data.requestId);
            if (receive) {waiting.delete(data.requestId); receive(data);}
          };
          window.requestCompute = data => new Promise(resolve => {
            const requestId = ++next; waiting.set(requestId, resolve);
            worker.postMessage({...data, requestId});
          });
        });
        return page;
      }));
      const command = (page, data) => page.evaluate(data => window.requestCompute(data), data);
      const group = {op:'prepare', id:'rig/one', plan:fixture.plan, positions:fixture.positions, surfaceUvs:fixture.surfaceUvs};
      const run = {op:'run', id:group.id, uniforms:fixture.uniforms, start:0, count:4};
      for (const page of pages) {
        const probe = await command(page, {op:'probe'});
        assert.equal(probe.ok, true, JSON.stringify(probe));
        assert.equal(probe.capabilities.backend, 'webgl2-transform-feedback');
        assert.ok(probe.capabilities.probeMs >= 0);
        assert.equal((await command(page, group)).stride, 6);
      }
      const shards = await Promise.all(pages.map((page, i) => page.evaluate(async data => {
        const result = await window.requestCompute(data);
        return {ok:result.ok, typed:result.values instanceof Float32Array, values:Array.from(result.values || [])};
      }, {...run, start:2*i, count:2})));
      assert.ok(shards.every(part => part.ok && part.typed));
      assert.deepEqual(shards.flatMap(part => part.values), fixture.expected);

      const [a,b] = pages;
      assert.equal((await command(a, {...group, plan:{...fixture.plan,stride:1}})).error.code, 'invalid-plan');
      assert.equal((await command(a, {...group, surfaceUvs:[]})).error.code, 'invalid-input');
      assert.equal((await command(a, {...run, start:3, count:2})).error.code, 'invalid-input');
      assert.equal((await command(a, {...run, uniforms:{}})).error.code, 'invalid-input');
      assert.equal((await command(a, {...run, uniforms:{...fixture.uniforms,u_param_1:[Infinity]}})).error.code, 'invalid-input');
      // Failed re-preparation retains the previous committed program and rig.
      assert.ok((await command(a, run)).ok);

      const nanPlan = {outputs:[{varying:'out_0',offset:0,components:1}], stride:1,
        source:'#version 300 es\nprecision highp float; out float out_0; void main(){out_0=uintBitsToFloat(0x7fc00000u);gl_Position=vec4(0,0,0,1);}'};
      assert.ok((await command(b, {...group, id:'nan',plan:nanPlan})).ok);
      assert.equal((await command(b, {...run,id:'nan', uniforms:{}})).error.code,'invalid-result');
      assert.ok((await command(b, {op:'suspend'})).ok);
      assert.equal((await command(b, run)).error.code, 'unavailable');
      assert.ok((await command(b, {op:'resume'})).ok);
      assert.equal((await command(b, run)).error.code, 'not-prepared');
      assert.ok((await command(b, group)).ok);

      const lifecycle = await a.evaluate(async ({group,run}) => {
        const {createComputeExecutor} = await import('/executor.mjs');
        const canvas = document.createElement('canvas'), changes = [];
        const local = createComputeExecutor({canvas, onAvailability:(...args) => changes.push(args)});
        try {
          local.prepare(group);
          const pending = local.run(run);
          const busy = await local.run(run).catch(e => e.code);
          local.suspend(); const cancelled = await pending.catch(e => e.code);
          local.resume(); local.prepare(group);
          const extension = canvas.getContext('webgl2').getExtension('WEBGL_lose_context');
          const lost = new Promise(resolve => canvas.addEventListener('webglcontextlost', resolve, {once:true}));
          extension.loseContext(); await lost;
          const unavailable = await local.run(run).catch(e => e.code);
          const restored = new Promise(resolve => canvas.addEventListener('webglcontextrestored', resolve, {once:true}));
          await new Promise(resolve => setTimeout(resolve, 100));
          extension.restoreContext();
          await Promise.race([restored, new Promise((_, reject) => setTimeout(() => reject(new Error('Context did not restore')), 5000))]);
          const expired = await local.run(run).catch(e => e.code);
          local.prepare(group); const values = await local.run(run);
          return {busy,cancelled,unavailable,expired,changes,values:Array.from(values)};
        } finally {local.dispose();}
      }, {group,run});
      assert.deepEqual(lifecycle, {busy:'busy', cancelled:'cancelled', unavailable:'unavailable', expired:'not-prepared',
        changes:[[false,'context-lost'],[true,'context-restored']],values:fixture.expected});
      for (const page of pages) assert.ok((await command(page, {op:'dispose'})).ok);
    } finally {await browser?.close(); await new Promise(resolve => server.close(resolve));}
  });
