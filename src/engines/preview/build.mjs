import {build} from 'esbuild';
import {writeFile} from 'node:fs/promises';
const result = await build({entryPoints: ['src/engines/preview/index.js'], bundle: true, format: 'esm', platform: 'browser',
  target: ['es2022'], outfile: 'resources/studio/preview.js', sourcemap: false, minify: true,
  legalComments: 'eof', logLevel: 'info', write: false});
// Three's multiline GLSL chunks contain mixed indentation/trailing whitespace.
// Normalizing physical source lines preserves shader tokens and legal notices;
// escaped whitespace inside ordinary JS strings is untouched.
for (const file of result.outputFiles) {
  await writeFile(file.path, file.text.replace(/[\t ]+$/gm, '').replace(/^ +(?=\t)/gm, ''));
}
