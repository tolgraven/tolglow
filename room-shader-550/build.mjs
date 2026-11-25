import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {cp, mkdir, readFile, writeFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';

const dirname = fileURLToPath(new URL('.', import.meta.url));
const [repoArg, depsArg, scratchArg] = process.argv.slice(2);
if (![repoArg, depsArg, scratchArg].every(Boolean)) throw new Error('Pass the Tolglow repo, cached node_modules and scratch directory');
const repo = resolve(repoArg), deps = resolve(depsArg), scratch = resolve(scratchArg);
const require = createRequire(join(deps, 'probe.cjs'));
const {build, version: esbuild} = require('esbuild');
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const git = (...args) => execFileSync('git', ['-C', repo, ...args], {maxBuffer: 8 * 1024 * 1024});
const ref = '56a785c527034a5914f0f1833c10741c2f4c8282';
const threePath = join(deps, 'three', 'build', 'three.module.js');
const threeVersion = JSON.parse(await readFile(join(deps, 'three', 'package.json'))).version;
const packageJson = JSON.parse(git('show', ref + ':package.json'));
if (threeVersion !== packageJson.dependencies.three || esbuild !== packageJson.devDependencies.esbuild)
  throw new Error('Cached dependency versions differ from the pinned release');
const sourceDir = join(scratch, 'source'), www = join(scratch, 'www');
await mkdir(join(www, 'studio'), {recursive: true});
await mkdir(sourceDir, {recursive: true});
const selected = ['js/preview', 'resources/studio/preview.js', 'resources/studio/preview-worker.js', 'package-lock.json'];
const source = git('archive', ref, ...selected);
execFileSync('tar', ['-x', '-C', sourceDir], {input: source});
for (const file of ['plan.mjs', 'probe.mjs']) await cp(join(dirname, file), join(sourceDir, 'js/preview', file));
await cp(join(dirname, 'page.html'), join(www, 'index.html'));
const main = await readFile(join(sourceDir, 'resources/studio/preview.js'));
const worker = await readFile(join(sourceDir, 'resources/studio/preview-worker.js'));
const identity = {
  ref, mainSha256: hash(main), bundledWorkerSha256: hash(worker),
  sourceSnapshotSha256: hash(source), three: threeVersion, esbuild,
  threeWebGlSha256: hash(await readFile(threePath))
};
const buildId = hash(JSON.stringify(identity));
await writeFile(join(www, 'studio/preview.js'), main);
await writeFile(join(www, 'studio/release-worker.js'), worker);

function replaceOnce(source, from, to) {
  if (source.split(from).length !== 2) throw new Error('Expected one source anchor: ' + from);
  return source.replace(from, to);
}

function instrumentThree(source) {
  const begin = '\tconst defines = parameters.defines;';
  const end = '\t// log( \'*VERTEX*\', vertexGlsl );';
  const startIndex = source.indexOf(begin, source.indexOf('function WebGLProgram('));
  const endIndex = source.indexOf(end, startIndex);
  if (startIndex < 0 || endIndex < 0) throw new Error('Three source assembly anchors moved');
  const assembly = source.slice(startIndex, endIndex).replace('\tconst program = gl.createProgram();\n\n', '');
  source = source.slice(0, startIndex) + `
    const program = gl.createProgram();
    const track = globalThis.roomProbe.start( parameters );
    const started = performance.now();
    const buildGlsl = () => {
      ${assembly}
      return {vertexGlsl, fragmentGlsl, prefixVertex, prefixFragment};
    };
    const assembled = track.cached || buildGlsl();
    const {vertexGlsl, fragmentGlsl, prefixVertex, prefixFragment} = globalThis.roomProbe.assembled(
      track, parameters, assembled, track.hit ? 0 : performance.now() - started);
    const submissionStart = performance.now();
` + source.slice(endIndex);
  source = replaceOnce(source, '\tgl.linkProgram( program );',
    '\tgl.linkProgram( program );\n\ttrack.submitMs = performance.now() - submissionStart;');
  source = replaceOnce(source, '\tfunction onFirstUse( self ) {',
    '\tfunction onFirstUse( self ) {\n\t\tconst firstUseStart = performance.now();');
  source = replaceOnce(source, '\t\tcachedAttributes = fetchAttributeLocations( gl, program );',
    '\t\tcachedAttributes = fetchAttributeLocations( gl, program );\n\t\ttrack.firstUseMs = performance.now() - firstUseStart;');
  return replaceOnce(source, 'material.onBeforeCompile( parameters, _this );',
    `const beforeHook = performance.now();
              material.onBeforeCompile( parameters, _this );
              globalThis.roomProbe.hook(performance.now() - beforeHook);`);
}

function instrumentWorker(source) {
  source = replaceOnce(source, 'engine = new SceneRenderer(canvas, status, {worker: true, autoStart: false,',
    'const initStarted = performance.now();\n    engine = new SceneRenderer(canvas, status, {worker: true, autoStart: false,');
  source = replaceOnce(source, 'if (engine.error) throw new Error(engine.error);',
    'initMs = performance.now() - initStarted;\n    if (engine.error) throw new Error(engine.error);');
  source = replaceOnce(source, 'lastDraw = started; engine.clock += dt; engine.draw(dt); renderOnce = false;',
    `probe.phase = 'draw-' + (frameId + 1);
    lastDraw = started; engine.clock += dt; engine.draw(dt); renderOnce = false;`);
  source = replaceOnce(source, 'bitmapTransferMs: engine.lastBitmapTransfer}}, [bitmap]);',
    `bitmapTransferMs: engine.lastBitmapTransfer}}, [bitmap]);
    probe.publish(frameId, {initMs, drawSubmitMs: submitted - started, gpuFenceMs: engine.lastGPUWait, bitmapMs: engine.lastBitmapTransfer,
      workerFrameMs: engine.lastWorkerFrame, workerClockMs: completed});`);
  source = replaceOnce(source, "send({type: 'ready'});",
    "await probe.load();\nsend({type: 'ready'});");
  return `import {probe} from './probe.mjs';
globalThis.roomProbe = probe;
let initMs;
` + source;
}

const instrument = {
  name: 'private-room-startup-timings',
  setup(build) {
    build.onLoad({filter: /three\.module\.js$/}, async ({path}) => ({
      contents: instrumentThree(await readFile(path, 'utf8')), loader: 'js'
    }));
    build.onLoad({filter: /[/\\]worker\.js$/}, async ({path}) => {
      if (path !== join(sourceDir, 'js/preview/worker.js')) return undefined;
      return {contents: instrumentWorker(await readFile(path, 'utf8')), loader: 'js'};
    });
  }
};

const result = await build({
  entryPoints: [join(sourceDir, 'js/preview/worker.js')], bundle: true, format: 'esm',
  platform: 'browser', target: ['es2022'], minify: true, write: false, legalComments: 'eof',
  nodePaths: [deps], plugins: [instrument], define: {ROOM_BUILD: JSON.stringify(buildId)}
});
const bytes = result.outputFiles[0].contents;
await writeFile(join(www, 'studio/preview-worker.js'), bytes);
await writeFile(join(www, 'identity.json'), JSON.stringify({...identity, buildId, measuredWorkerSha256: hash(bytes)}));
