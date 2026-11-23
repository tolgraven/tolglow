// Compile the exported answers and compare floating-point GPU samples with independent oracles.
const assert = require('node:assert/strict');
const {readFileSync} = require('node:fs');
const {join} = require('node:path');
const {chromium} = require('playwright-core');

async function verify(directory, executablePath) {
  const rows = [], families = new Map(), upstreams = new Map();
  for (const split of ['train', 'valid', 'test']) {
    for (const line of readFileSync(join(directory, split + '.jsonl'), 'utf8').trim().split('\n')) {
      const row = JSON.parse(line);
      assert.deepEqual(row.messages.map(m => m.role), ['system', 'user', 'assistant']);
      for (const [index, key] of [[families, row.family], [upstreams, row.source.repository]]) {
        assert(!index.has(key) || index.get(key) === split, 'upstream or family leaked between splits');
        index.set(key, split);
      }
      rows.push(row);
    }
  }
  assert.equal(new Set(rows.map(r => r.id)).size, rows.length);
  const browser = await chromium.launch({executablePath, args: ['--no-sandbox', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage();
    const checked = await page.evaluate(rows => {
      const canvas = document.createElement('canvas'); canvas.width = canvas.height = 1;
      const g = canvas.getContext('webgl2');
      if (!g?.getExtension('EXT_color_buffer_float')) throw Error('Need a float WebGL2 render target');
      const fbo = g.createFramebuffer(), target = g.createTexture();
      g.bindFramebuffer(g.FRAMEBUFFER, fbo); g.bindTexture(g.TEXTURE_2D, target);
      g.texStorage2D(g.TEXTURE_2D, 1, g.RGBA32F, 1, 1);
      g.framebufferTexture2D(g.FRAMEBUFFER, g.COLOR_ATTACHMENT0, g.TEXTURE_2D, target, 0);
      if (g.checkFramebufferStatus(g.FRAMEBUFFER) !== g.FRAMEBUFFER_COMPLETE) throw Error('Incomplete render target');
      const emitter = g.createTexture(); g.bindTexture(g.TEXTURE_2D, emitter);
      const impulse = new Uint8Array(7*4); impulse.fill(255, 3*4, 4*4);
      g.texImage2D(g.TEXTURE_2D, 0, g.RGBA8, 7, 1, 0, g.RGBA, g.UNSIGNED_BYTE, impulse);
      g.texParameteri(g.TEXTURE_2D, g.TEXTURE_MIN_FILTER, g.LINEAR); g.texParameteri(g.TEXTURE_2D, g.TEXTURE_MAG_FILTER, g.LINEAR);
      for (const edge of [g.TEXTURE_WRAP_S, g.TEXTURE_WRAP_T]) g.texParameteri(g.TEXTURE_2D, edge, g.CLAMP_TO_EDGE);

      function shader(type, source) {
        const s = g.createShader(type); g.shaderSource(s, source); g.compileShader(s);
        if (!g.getShaderParameter(s, g.COMPILE_STATUS)) throw Error(g.getShaderInfoLog(s));
        return s;
      }
      const vertex = shader(g.VERTEX_SHADER, '#version 300 es\nout vec2 vUv; void main(){vUv=vec2((gl_VertexID<<1)&2,gl_VertexID&2);gl_Position=vec4(vUv*2.0-1.0,0.,1.);}');
      const checked = [];
      for (const row of rows) {
        const key = row.id.split('/').at(-1), kind = row.family.split('/').at(-1);
        const fragment = shader(g.FRAGMENT_SHADER, row.messages[2].content);
        const program = g.createProgram(); g.attachShader(program, vertex); g.attachShader(program, fragment); g.linkProgram(program);
        if (!g.getProgramParameter(program,g.LINK_STATUS)) throw Error(row.id+': '+g.getProgramInfoLog(program));
        g.useProgram(program);
        function sample(changes={}) {
          for (const [name, value] of Object.entries({...row.fixture, ...changes})) {
            const u = g.getUniformLocation(program, name);
            if (typeof value === 'number') g.uniform1f(u, value);
            else if (Array.isArray(value)) g['uniform'+value.length+'fv'](u, value);
            else g.uniform1i(u, 0);
          }
          g.drawArrays(g.TRIANGLES, 0, 3);
          const pixel = new Float32Array(4); g.readPixels(0,0,1,1,g.RGBA,g.FLOAT,pixel);
          if (!pixel.every(Number.isFinite) || Math.abs(pixel[3]-1)>0.0001 || g.getError()!==g.NO_ERROR)
            throw Error(row.id+' returned invalid radiance '+pixel);
          return Array.from(pixel.slice(0,3));
        }
        const values = sample();
        function close(actual, expected, tolerance=.001) {
          if (!expected.every((v,i) => Math.abs(actual[i]-v)<tolerance)) throw Error(row.id+' got '+actual+' expected '+expected);
        }
        if (kind === 'easing') {
          close(sample({beat:0}), row.fixture.originColor); close(sample({beat:1}), row.fixture.targetColor);
          if (!values.some((v,i) => Math.abs(v-row.fixture.originColor[i])>.00001)) throw Error(row.id+' is constant');
        } else if (kind === 'noise') {
          close(sample({wet:0}), row.fixture.emittedColor);
          const gains = [[.11,.3,.82], [2.7,3.5,.51], [.61,.82,.45]].map(worldPosition =>
            sample({wet:1,worldPosition})[0]/row.fixture.emittedColor[0]);
          if (!gains.every(v=>v>=0 && v<=1.001) || Math.max(...gains)-Math.min(...gains)<.0001)
            throw Error(row.id+' has invalid or constant gain');
        } else if (kind === 'blur') {
          const weight = {5:.294117647,9:.227027027,13:.19648255}[key];
          close(values,[weight,weight,weight],.003); close(sample({direction:[0,1]}),[1,1,1]);
        } else if (kind === 'transfer') {
          close(values,row.fixture.emittedColor.map(v=>key==='encode' ? 1.055*Math.pow(v,1/2.4)-.055 : Math.pow((v+.055)/1.055,2.4)));
        } else if (kind === 'palette') {
          const f = row.fixture;
          const phase = key==='beat-lookup' ? f.beat : key==='fixture-world-phase' ? f.beat+f.worldPosition[1]*.2
            : 1-Math.exp(-f.extinctionPerM*f.mixedDensity*f.pathMetres);
          close(values,[0,.22,.55].map(d=>(.5+.5*Math.cos(2*Math.PI*(phase+d)))*f.emissionGain));
        }
        checked.push(row.id); g.deleteProgram(program); g.deleteShader(fragment);
      }
      return checked;
    }, rows);
    console.log(checked.length+' WebGL2 programs compiled, linked and sampled; '+families.size+' split-isolated upstreams.');
  } finally {await browser.close();}
}
verify(...process.argv.slice(2)).catch(error => {console.error(error);process.exitCode=1;});
