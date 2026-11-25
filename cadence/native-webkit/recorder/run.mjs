import assert from 'node:assert/strict';
import {execFileSync, spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {createWriteStream} from 'node:fs';
import {mkdtemp, readFile, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {setTimeout as pause} from 'node:timers/promises';
import {readDesktopSession} from './desktop-session.mjs';

const [application, checkout, evidence, activateApp] = process.argv.slice(2, 6).map(value => resolve(value));
const expanded = process.argv.includes('--expanded');
const processScope = await mkdtemp(join(evidence, 'run-'));
const receiptPath = join(processScope, 'room.json');
const log = createWriteStream(join(processScope, 'app.log'));
const show = await mkdtemp(join(processScope, 'show-'));
await writeFile(join(show, 'show.clj'), `(ns show)
(defn ready [context _]
  (spit "window-port" (get-in context [:modules :web-server :port]))
  {})
(def config
  {:hz 40
   :show {:credential-path "credentials.edn" :workspace-options nil}
   :init {:components [:show]
          :modules [:web-server :ready]
          :search-in-namespaces ['show]}})
`);
const metadata = {application, expanded, checks: []};
const desktopCheck = async () => {
  const check = await readDesktopSession();
  metadata.checks.push(check);
  if (check.status !== 'unlocked') throw new Error('Desktop not verified unlocked. '+check.reason);
};
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
let app, origin;
try {
  await desktopCheck();
  app = spawn(join(application, 'Contents/MacOS/TolglowStudio'),
    ['--open-show', show, '--record-room', receiptPath, ...(expanded ? ['--room-expanded'] : [])],
    {stdio: ['ignore', 'pipe', 'pipe']});
  app.stdout.pipe(log, {end:false});
  app.stderr.pipe(log, {end:false});
  let failed;
  app.once('error', error => {failed = error;});
  const deadline = Date.now() + 260000;
  while (Date.now() < deadline) {
    if (failed) throw failed;
    if (app.exitCode !== null) throw new Error('The packaged window exited early. Code '+app.exitCode);
    await desktopCheck();
    if (!origin) {
      const port = await readFile(join(show, 'window-port'),'utf8').catch(() => '');
      if (/^\d+$/.test(port)) {
        metadata.windowID = execFileSync(activateApp, [String(app.pid)], {encoding: 'utf8', timeout: 5000}).trim();
        assert.match(metadata.windowID, /^\d+$/);
        metadata.activatedAt = new Date().toISOString();
        origin = 'http://127.0.0.1:'+port;
        const url = origin + '/studio/preview-worker.js';
        const response = await fetch(url, {signal: AbortSignal.timeout(10000)});
        assert.equal(response.ok, true);
        metadata.workerSHA256 = hash(Buffer.from(await response.arrayBuffer()));
        assert.equal(metadata.workerSHA256, hash(await readFile(join(checkout, 'resources/studio/preview-worker.js'))),
          'packaged worker asset must equal the reviewed build');
        metadata.packagedRevisions = (await readFile(join(application, 'Contents/Resources/source-version.txt'), 'utf8')).trim();
      }
    }
    const payload = await readFile(receiptPath, 'utf8').catch(() => null);
    if (payload) {
      const receipt = JSON.parse(payload);
      if (receipt.phase !== 'finished') {
        await pause(1000);
        continue;
      }
      assert.ok(origin, 'The embedded loopback show must be observed');
      assert.equal(receipt.bundleID, 'se.tolgraven.tolglow.studio');
      assert.equal(receipt.expanded, expanded);
      assert.ok(!receipt.error, receipt.error);
      assert.equal(receipt.samples.length, 3);
      assert.equal(receipt.changed.length, 0, 'focus changed after startup');
      for (const sample of receipt.samples) {
        assert.deepEqual(sample.invalid, []);
        assert.equal(sample.rendererThread, 'worker');
        assert.equal(sample.start.targetRefreshHz, 60);
        for (const boundary of ['start','end'])
          assert.ok(metadata.checks.some(row => Math.abs(Date.parse(row.checkedAt)-sample.sampleBoundaryEpochMs[boundary]) < 2500),
            'OS lock qualification must bracket the sample');
        console.log(JSON.stringify({name: sample.name, roomHz:sample.presented.hz, displayHz:sample.windowRaf.hz,
          backing:sample.backing, quality:sample.qualities, scene:sample.sceneSizes, renderer:sample.end.deviceRenderer,
          plumes:sample.plumes}));
      }
      metadata.qualified = true;
      break;
    }
    await pause(1000);
  }
  assert.ok(metadata.qualified, 'The packaged app never emitted a qualified receipt');
} catch (error) {
  metadata.error = String(error);
  console.error(error);
  process.exitCode = 2;
} finally {
  if (app && app.exitCode === null) {
    app.kill('SIGTERM');
    for (let check = 0; check < 30 && app.exitCode === null && app.signalCode === null; check++) await pause(200);
    if (app.exitCode === null && app.signalCode === null) app.kill('SIGKILL');
  }
  if (origin) {
    for (let check = 0; check < 25; check++) {
      const stillListening = await fetch(origin, {signal: AbortSignal.timeout(1200)}).then(() => true, () => false);
      if (!stillListening) {metadata.stopped = true; break;}
      await pause(300);
    }
    if (!metadata.stopped) process.exitCode = 2;
  }
  log.end();
  await writeFile(join(processScope, 'metadata.json'), JSON.stringify(metadata, null, 2));
  console.log(processScope);
}
