// Clone the paired distributed-compute source before rebuilding this Xcode resource.
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { resolve, dirname } from 'node:path';

const here = dirname(fileURLToPath(import.meta.url));
const repo = resolve(here, '../../..');
const require = createRequire(resolve(repo, 'package.json'));
const { build } = require('esbuild');
await build({
  entryPoints: [resolve(here,'entry.mjs')], platform:'browser', format:'iife', globalName:'AudienceGPU',
  bundle:true, minify:true, target:'safari17.6', outfile:resolve(here,'../Phone/audience-compute.js'),
  alias: {'@tolglow/compute-dispatcher': resolve(process.env.TOLGLOW_COMPUTE_SOURCE || repo, 'js/compute/dispatcher.mjs')}
});
