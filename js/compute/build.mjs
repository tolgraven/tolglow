import {build} from 'esbuild';

// A dedicated ClojureScript worker loads this script with importScripts.
// It shares no DOM, context or render loop with the operator's Studio.
await build({entryPoints: ['js/compute/dispatcher.mjs'], bundle: true, format: 'iife',
  globalName: 'TolglowCompute', outfile: 'resources/studio/compute/dispatcher.js',
  platform: 'browser', target: 'es2022', minify: true});
