// This code implements the `-sMODULARIZE` settings by taking the generated
// JS program code (INNER_JS_CODE) and wrapping it in a factory function.

// Single threaded MINIMAL_RUNTIME programs do not need access to
// document.currentScript, so a simple export declaration is enough.
var createNvim = (() => {
  // When MODULARIZE this JS may be executed later,
  // after document.currentScript is gone, so we save it.
  // In EXPORT_ES6 mode we can just use 'import.meta.url'.
  var _scriptName = globalThis.document?.currentScript?.src;
  return async function(moduleArg = {}) {
    var Module = moduleArg;
// include: shell.js
// include: minimum_runtime_check.js
(function() {
  // "30.0.0" -> 300000
  function humanReadableVersionToPacked(str) {
    str = str.split('-')[0]; // Remove any trailing part from e.g. "12.53.3-alpha"
    var vers = str.split('.').slice(0, 3);
    while(vers.length < 3) vers.push('00');
    vers = vers.map((n, i, arr) => n.padStart(2, '0'));
    return vers.join('');
  }
  // 300000 -> "30.0.0"
  var packedVersionToHumanReadable = n => [n / 10000 | 0, (n / 100 | 0) % 100, n % 100].join('.');

  var TARGET_NOT_SUPPORTED = 2147483647;

  // Note: We use a typeof check here instead of optional chaining using
  // globalThis because older browsers might not have globalThis defined.
  var currentNodeVersion = typeof process !== 'undefined' && process.versions?.node ? humanReadableVersionToPacked(process.versions.node) : TARGET_NOT_SUPPORTED;
  if (currentNodeVersion < 180300) {
    throw new Error(`This emscripten-generated code requires node v${ packedVersionToHumanReadable(180300) } (detected v${packedVersionToHumanReadable(currentNodeVersion)})`);
  }

  var userAgent = typeof navigator !== 'undefined' && navigator.userAgent;
  if (!userAgent) {
    return;
  }

  var currentSafariVersion = userAgent.includes("Safari/") && !userAgent.includes("Chrome/") && userAgent.match(/Version\/(\d+\.?\d*\.?\d*)/) ? humanReadableVersionToPacked(userAgent.match(/Version\/(\d+\.?\d*\.?\d*)/)[1]) : TARGET_NOT_SUPPORTED;
  if (currentSafariVersion < 150000) {
    throw new Error(`This emscripten-generated code requires Safari v${ packedVersionToHumanReadable(150000) } (detected v${currentSafariVersion})`);
  }

  var currentFirefoxVersion = userAgent.match(/Firefox\/(\d+(?:\.\d+)?)/) ? parseFloat(userAgent.match(/Firefox\/(\d+(?:\.\d+)?)/)[1]) : TARGET_NOT_SUPPORTED;
  if (currentFirefoxVersion < 79) {
    throw new Error(`This emscripten-generated code requires Firefox v79 (detected v${currentFirefoxVersion})`);
  }

  var currentChromeVersion = userAgent.match(/Chrome\/(\d+(?:\.\d+)?)/) ? parseFloat(userAgent.match(/Chrome\/(\d+(?:\.\d+)?)/)[1]) : TARGET_NOT_SUPPORTED;
  if (currentChromeVersion < 85) {
    throw new Error(`This emscripten-generated code requires Chrome v85 (detected v${currentChromeVersion})`);
  }
})();

// end include: minimum_runtime_check.js
// The Module object: Our interface to the outside world. We import
// and export values on it. There are various ways Module can be used:
// 1. Not defined. We create it here
// 2. A function parameter, function(moduleArg) => Promise<Module>
// 3. pre-run appended it, var Module = {}; ..generated code..
// 4. External script tag defines var Module.
// We need to check if Module already exists (e.g. case 3 above).
// Substitution will be replaced with actual code on later stage of the build,
// this way Closure Compiler will not mangle it (e.g. case 4. above).
// Note that if you want to run closure, and also to use Module
// after the generated code, you will need to define   var Module = {};
// before the code. Then that object will be used in the code, and you
// can continue to use Module afterwards as well.

// Determine the runtime environment we are in. You can customize this by
// setting the ENVIRONMENT setting at compile time (see settings.js).

// Attempt to auto-detect the environment
var ENVIRONMENT_IS_WEB = !!globalThis.window;
var ENVIRONMENT_IS_WORKER = !!globalThis.WorkerGlobalScope;
// N.b. Electron.js environment is simultaneously a NODE-environment, but
// also a web environment.
var ENVIRONMENT_IS_NODE = globalThis.process?.versions?.node && globalThis.process?.type != 'renderer';
var ENVIRONMENT_IS_SHELL = !ENVIRONMENT_IS_WEB && !ENVIRONMENT_IS_NODE && !ENVIRONMENT_IS_WORKER;

// Three configurations we can be running in:
// 1) We could be the application main() thread running in the main JS UI thread. (ENVIRONMENT_IS_WORKER == false and ENVIRONMENT_IS_PTHREAD == false)
// 2) We could be the application main() running directly in a worker. (ENVIRONMENT_IS_WORKER == true, ENVIRONMENT_IS_PTHREAD == false)
// 3) We could be an application pthread running in a worker. (ENVIRONMENT_IS_WORKER == true and ENVIRONMENT_IS_PTHREAD == true)

// The way we signal to a worker that it is hosting a pthread is to construct
// it with a specific name.
var ENVIRONMENT_IS_PTHREAD = ENVIRONMENT_IS_WORKER && globalThis.name?.startsWith('em-pthread')

if (ENVIRONMENT_IS_PTHREAD) {
  assert(!globalThis.moduleLoaded, 'module should only be loaded once on each pthread worker');
  globalThis.moduleLoaded = true;
}

if (ENVIRONMENT_IS_NODE) {

  var worker_threads = require('node:worker_threads');
  globalThis.Worker = worker_threads.Worker;
  ENVIRONMENT_IS_WORKER = !worker_threads.isMainThread;
  // Under node we set `workerData` to `em-pthread` to signal that the worker
  // is hosting a pthread.
  ENVIRONMENT_IS_PTHREAD = ENVIRONMENT_IS_WORKER && worker_threads.workerData == 'em-pthread'
}

// --pre-jses are emitted after the Module integration code, so that they can
// refer to Module (if they choose; they can also define Module)
// include: /tmp/tmp8_eg_1yy.js

  if (!Module['expectedDataFileDownloads']) Module['expectedDataFileDownloads'] = 0;
  Module['expectedDataFileDownloads']++;
  (() => {
    // Do not attempt to redownload the virtual filesystem data when in a pthread or a Wasm Worker context.
    var isPthread = typeof ENVIRONMENT_IS_PTHREAD != 'undefined' && ENVIRONMENT_IS_PTHREAD;
    var isWasmWorker = typeof ENVIRONMENT_IS_WASM_WORKER != 'undefined' && ENVIRONMENT_IS_WASM_WORKER;
    if (isPthread || isWasmWorker) return;
    var isNode = globalThis.process && globalThis.process.versions && globalThis.process.versions.node && globalThis.process.type != 'renderer';
    async function loadPackage(metadata) {

      var PACKAGE_PATH = '';
      if (typeof window === 'object') {
        PACKAGE_PATH = window['encodeURIComponent'](window.location.pathname.substring(0, window.location.pathname.lastIndexOf('/')) + '/');
      } else if (typeof process === 'undefined' && typeof location !== 'undefined') {
        // web worker
        PACKAGE_PATH = encodeURIComponent(location.pathname.substring(0, location.pathname.lastIndexOf('/')) + '/');
      }
      var PACKAGE_NAME = './.zig-cache/o/22c853d50f365595c725b12b0658905f/nvim.data';
      var REMOTE_PACKAGE_BASE = 'nvim.data';
      var REMOTE_PACKAGE_NAME = Module['locateFile'] ? Module['locateFile'](REMOTE_PACKAGE_BASE, '') : REMOTE_PACKAGE_BASE;
      var REMOTE_PACKAGE_SIZE = metadata['remote_package_size'];

      async function fetchRemotePackage(packageName, packageSize) {
        if (isNode) {
          var contents = require('fs').readFileSync(packageName);
          return new Uint8Array(contents).buffer;
        }
        if (!Module['dataFileDownloads']) Module['dataFileDownloads'] = {};
        try {
          var response = await fetch(packageName);
        } catch (e) {
          throw new Error(`Network Error: ${packageName}`, {e});
        }
        if (!response.ok) {
          throw new Error(`${response.status}: ${response.url}`);
        }

        const chunks = [];
        const headers = response.headers;
        const total = Number(headers.get('Content-Length') || packageSize);
        let loaded = 0;

        Module['setStatus'] && Module['setStatus']('Downloading data...');
        const reader = response.body.getReader();

        while (1) {
          var {done, value} = await reader.read();
          if (done) break;
          chunks.push(value);
          loaded += value.length;
          Module['dataFileDownloads'][packageName] = {loaded, total};

          let totalLoaded = 0;
          let totalSize = 0;

          for (const download of Object.values(Module['dataFileDownloads'])) {
            totalLoaded += download.loaded;
            totalSize += download.total;
          }

          Module['setStatus'] && Module['setStatus'](`Downloading data... (${totalLoaded}/${totalSize})`);
        }

        const packageData = new Uint8Array(chunks.map((c) => c.length).reduce((a, b) => a + b, 0));
        let offset = 0;
        for (const chunk of chunks) {
          packageData.set(chunk, offset);
          offset += chunk.length;
        }
        return packageData.buffer;
      }

      var fetchPromise;
      var fetched = Module['getPreloadedPackage'] && Module['getPreloadedPackage'](REMOTE_PACKAGE_NAME, REMOTE_PACKAGE_SIZE);

      if (!fetched) {
        // Note that we don't use await here because we want to execute the
        // the rest of this function immediately.
        fetchPromise = fetchRemotePackage(REMOTE_PACKAGE_NAME, REMOTE_PACKAGE_SIZE);
      }

    async function runWithFS(Module) {

      function assert(check, msg) {
        if (!check) throw new Error(msg);
      }
Module['FS_createPath']("/", "runtime", true, true);
Module['FS_createPath']("/runtime", "autoload", true, true);
Module['FS_createPath']("/runtime/autoload", "cargo", true, true);
Module['FS_createPath']("/runtime/autoload", "dist", true, true);
Module['FS_createPath']("/runtime/autoload", "provider", true, true);
Module['FS_createPath']("/runtime/autoload", "remote", true, true);
Module['FS_createPath']("/runtime/autoload", "rust", true, true);
Module['FS_createPath']("/runtime/autoload", "xml", true, true);
Module['FS_createPath']("/runtime", "colors", true, true);
Module['FS_createPath']("/runtime", "compiler", true, true);
Module['FS_createPath']("/runtime", "doc", true, true);
Module['FS_createPath']("/runtime", "ftplugin", true, true);
Module['FS_createPath']("/runtime", "indent", true, true);
Module['FS_createPath']("/runtime", "keymap", true, true);
Module['FS_createPath']("/runtime", "lua", true, true);
Module['FS_createPath']("/runtime/lua", "nvim", true, true);
Module['FS_createPath']("/runtime/lua/nvim", "dir", true, true);
Module['FS_createPath']("/runtime/lua/nvim", "zip", true, true);
Module['FS_createPath']("/runtime/lua", "uv", true, true);
Module['FS_createPath']("/runtime/lua", "vim", true, true);
Module['FS_createPath']("/runtime/lua/vim", "_core", true, true);
Module['FS_createPath']("/runtime/lua/vim/_core", "ui2", true, true);
Module['FS_createPath']("/runtime/lua/vim", "_ftplugin", true, true);
Module['FS_createPath']("/runtime/lua/vim", "_meta", true, true);
Module['FS_createPath']("/runtime/lua/vim", "async", true, true);
Module['FS_createPath']("/runtime/lua/vim", "deprecated", true, true);
Module['FS_createPath']("/runtime/lua/vim", "diagnostic", true, true);
Module['FS_createPath']("/runtime/lua/vim", "filetype", true, true);
Module['FS_createPath']("/runtime/lua/vim", "func", true, true);
Module['FS_createPath']("/runtime/lua/vim", "health", true, true);
Module['FS_createPath']("/runtime/lua/vim", "lsp", true, true);
Module['FS_createPath']("/runtime/lua/vim/lsp", "_meta", true, true);
Module['FS_createPath']("/runtime/lua/vim", "net", true, true);
Module['FS_createPath']("/runtime/lua/vim", "pack", true, true);
Module['FS_createPath']("/runtime/lua/vim", "pos", true, true);
Module['FS_createPath']("/runtime/lua/vim", "provider", true, true);
Module['FS_createPath']("/runtime/lua/vim", "treesitter", true, true);
Module['FS_createPath']("/runtime/lua/vim/treesitter", "_meta", true, true);
Module['FS_createPath']("/runtime/lua/vim", "ui", true, true);
Module['FS_createPath']("/runtime/lua/vim/ui", "clipboard", true, true);
Module['FS_createPath']("/runtime/lua/vim/ui", "img", true, true);
Module['FS_createPath']("/runtime", "pack", true, true);
Module['FS_createPath']("/runtime/pack", "dist", true, true);
Module['FS_createPath']("/runtime/pack/dist", "opt", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "cfilter", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/cfilter", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "justify", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/justify", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "matchit", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/matchit", "autoload", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/matchit", "doc", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/matchit", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "netrw", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/netrw", "autoload", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/netrw/autoload", "netrw", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/netrw", "doc", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/netrw", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/netrw", "syntax", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "nohlsearch", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nohlsearch", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "nvim.difftool", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nvim.difftool", "lua", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nvim.difftool", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "nvim.tohtml", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nvim.tohtml", "lua", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nvim.tohtml", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "nvim.undotree", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nvim.undotree", "lua", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/nvim.undotree", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "old-zip", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/old-zip", "autoload", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/old-zip", "doc", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/old-zip", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "swapmouse", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/swapmouse", "plugin", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt", "termdebug", true, true);
Module['FS_createPath']("/runtime/pack/dist/opt/termdebug", "plugin", true, true);
Module['FS_createPath']("/runtime", "plugin", true, true);
Module['FS_createPath']("/runtime", "queries", true, true);
Module['FS_createPath']("/runtime/queries", "c", true, true);
Module['FS_createPath']("/runtime/queries", "diff", true, true);
Module['FS_createPath']("/runtime/queries", "lua", true, true);
Module['FS_createPath']("/runtime/queries", "markdown", true, true);
Module['FS_createPath']("/runtime/queries", "markdown_inline", true, true);
Module['FS_createPath']("/runtime/queries", "query", true, true);
Module['FS_createPath']("/runtime/queries", "vim", true, true);
Module['FS_createPath']("/runtime/queries", "vimdoc", true, true);
Module['FS_createPath']("/runtime", "scripts", true, true);
Module['FS_createPath']("/runtime", "spell", true, true);
Module['FS_createPath']("/runtime", "syntax", true, true);
Module['FS_createPath']("/runtime/syntax", "modula2", true, true);
Module['FS_createPath']("/runtime/syntax/modula2", "opt", true, true);
Module['FS_createPath']("/runtime/syntax", "shared", true, true);
Module['FS_createPath']("/runtime/syntax", "vim", true, true);
Module['FS_createPath']("/runtime", "tutor", true, true);
Module['FS_createPath']("/runtime/tutor", "en", true, true);
Module['FS_createPath']("/runtime/tutor", "ja", true, true);
Module['FS_createPath']("/runtime/tutor", "zh", true, true);

      async function processPackageData(arrayBuffer) {
        assert(arrayBuffer, 'Loading data file failed.');
        assert(arrayBuffer.constructor.name === ArrayBuffer.name, 'bad input to processPackageData ' + arrayBuffer.constructor.name);
        var byteArray = new Uint8Array(arrayBuffer);
        var curr;
        // Reuse the bytearray from the XHR as the source for file reads.
          for (var file of metadata['files']) {
            var name = file['filename'];
            var data = byteArray.subarray(file['start'], file['end']);
            // canOwn this data in the filesystem, it is a slice into the heap that will never change
        Module['FS_createDataFile'](name, null, data, true, true, true);
          }
          Module['removeRunDependency']('datafile_./.zig-cache/o/22c853d50f365595c725b12b0658905f/nvim.data');
      }
      Module['addRunDependency']('datafile_./.zig-cache/o/22c853d50f365595c725b12b0658905f/nvim.data');

      if (!Module['preloadResults']) Module['preloadResults'] = {};

      Module['preloadResults'][PACKAGE_NAME] = {fromCache: false};
      if (!fetched) {
        fetched = await fetchPromise;
      }
      await processPackageData(fetched);

    }
    // Detect whether the module JS file has already been loaded.
    if (Module['FS_createPath']) {
      runWithFS(Module);
    } else {
      if (!Module['preRun']) Module['preRun'] = [];
      Module['preRun'].push(runWithFS); // FS is not initialized yet, wait for it
    }

    }
    loadPackage({"files": [{"filename": "/runtime/CMakeLists.txt", "start": 0, "end": 4834}, {"filename": "/runtime/autoload/RstFold.vim", "start": 4834, "end": 6734}, {"filename": "/runtime/autoload/ada.vim", "start": 6734, "end": 29381}, {"filename": "/runtime/autoload/adacomplete.vim", "start": 29381, "end": 33051}, {"filename": "/runtime/autoload/beancount.vim", "start": 33051, "end": 35722}, {"filename": "/runtime/autoload/beancountcomplete.vim", "start": 35722, "end": 43094}, {"filename": "/runtime/autoload/bitbake.vim", "start": 43094, "end": 46191}, {"filename": "/runtime/autoload/cargo.vim", "start": 46191, "end": 50250}, {"filename": "/runtime/autoload/cargo/quickfix.vim", "start": 50250, "end": 51393}, {"filename": "/runtime/autoload/ccomplete.vim", "start": 51393, "end": 71140}, {"filename": "/runtime/autoload/clojurecomplete.vim", "start": 71140, "end": 80011}, {"filename": "/runtime/autoload/context.vim", "start": 80011, "end": 85476}, {"filename": "/runtime/autoload/contextcomplete.vim", "start": 85476, "end": 86132}, {"filename": "/runtime/autoload/csscomplete.vim", "start": 86132, "end": 129484}, {"filename": "/runtime/autoload/decada.vim", "start": 129484, "end": 132482}, {"filename": "/runtime/autoload/dist/vim.vim", "start": 132482, "end": 133448}, {"filename": "/runtime/autoload/freebasic.vim", "start": 133448, "end": 134488}, {"filename": "/runtime/autoload/gnat.vim", "start": 134488, "end": 139950}, {"filename": "/runtime/autoload/gzip.vim", "start": 139950, "end": 146727}, {"filename": "/runtime/autoload/hare.vim", "start": 146727, "end": 147509}, {"filename": "/runtime/autoload/haskellcomplete.vim", "start": 147509, "end": 253275}, {"filename": "/runtime/autoload/hcl.vim", "start": 253275, "end": 255173}, {"filename": "/runtime/autoload/htmlcomplete.vim", "start": 255173, "end": 280655}, {"filename": "/runtime/autoload/htmlfold.vim", "start": 280655, "end": 285240}, {"filename": "/runtime/autoload/javaformat.vim", "start": 285240, "end": 287549}, {"filename": "/runtime/autoload/javascriptcomplete.vim", "start": 287549, "end": 315773}, {"filename": "/runtime/autoload/modula2.vim", "start": 315773, "end": 316464}, {"filename": "/runtime/autoload/msgpack.vim", "start": 316464, "end": 342786}, {"filename": "/runtime/autoload/paste.vim", "start": 342786, "end": 343509}, {"filename": "/runtime/autoload/phpcomplete.vim", "start": 343509, "end": 697633}, {"filename": "/runtime/autoload/provider.vim", "start": 697633, "end": 698274}, {"filename": "/runtime/autoload/provider/clipboard.vim", "start": 698274, "end": 710008}, {"filename": "/runtime/autoload/provider/node.vim", "start": 710008, "end": 715109}, {"filename": "/runtime/autoload/provider/perl.vim", "start": 715109, "end": 715536}, {"filename": "/runtime/autoload/provider/python3.vim", "start": 715536, "end": 715993}, {"filename": "/runtime/autoload/provider/ruby.vim", "start": 715993, "end": 716487}, {"filename": "/runtime/autoload/provider/script_host.rb", "start": 716487, "end": 716669}, {"filename": "/runtime/autoload/python.vim", "start": 716669, "end": 724650}, {"filename": "/runtime/autoload/python3complete.vim", "start": 724650, "end": 749060}, {"filename": "/runtime/autoload/pythoncomplete.vim", "start": 749060, "end": 773929}, {"filename": "/runtime/autoload/remote/define.vim", "start": 773929, "end": 782302}, {"filename": "/runtime/autoload/remote/host.vim", "start": 782302, "end": 789315}, {"filename": "/runtime/autoload/rubycomplete.vim", "start": 789315, "end": 815350}, {"filename": "/runtime/autoload/rust.vim", "start": 815350, "end": 832400}, {"filename": "/runtime/autoload/rust/debugging.vim", "start": 832400, "end": 835603}, {"filename": "/runtime/autoload/rustfmt.vim", "start": 835603, "end": 844197}, {"filename": "/runtime/autoload/shada.vim", "start": 844197, "end": 867761}, {"filename": "/runtime/autoload/spotbugs.vim", "start": 867761, "end": 878542}, {"filename": "/runtime/autoload/sqlcomplete.vim", "start": 878542, "end": 917817}, {"filename": "/runtime/autoload/syntaxcomplete.vim", "start": 917817, "end": 951468}, {"filename": "/runtime/autoload/tar.vim", "start": 951468, "end": 979871}, {"filename": "/runtime/autoload/tutor.vim", "start": 979871, "end": 986982}, {"filename": "/runtime/autoload/typst.vim", "start": 986982, "end": 988990}, {"filename": "/runtime/autoload/vimexpect.vim", "start": 988990, "end": 993936}, {"filename": "/runtime/autoload/xml/html32.vim", "start": 993936, "end": 1012266}, {"filename": "/runtime/autoload/xml/html401f.vim", "start": 1012266, "end": 1066388}, {"filename": "/runtime/autoload/xml/html401s.vim", "start": 1066388, "end": 1107926}, {"filename": "/runtime/autoload/xml/html401t.vim", "start": 1107926, "end": 1161617}, {"filename": "/runtime/autoload/xml/html40f.vim", "start": 1161617, "end": 1215646}, {"filename": "/runtime/autoload/xml/html40s.vim", "start": 1215646, "end": 1257091}, {"filename": "/runtime/autoload/xml/html40t.vim", "start": 1257091, "end": 1310688}, {"filename": "/runtime/autoload/xml/xhtml10f.vim", "start": 1310688, "end": 1366740}, {"filename": "/runtime/autoload/xml/xhtml10s.vim", "start": 1366740, "end": 1410703}, {"filename": "/runtime/autoload/xml/xhtml10t.vim", "start": 1410703, "end": 1467031}, {"filename": "/runtime/autoload/xml/xhtml11.vim", "start": 1467031, "end": 1515180}, {"filename": "/runtime/autoload/xml/xsd.vim", "start": 1515180, "end": 1520291}, {"filename": "/runtime/autoload/xml/xsl.vim", "start": 1520291, "end": 1524970}, {"filename": "/runtime/autoload/xmlcomplete.vim", "start": 1524970, "end": 1539907}, {"filename": "/runtime/autoload/xmlformat.vim", "start": 1539907, "end": 1546340}, {"filename": "/runtime/colors/README.txt", "start": 1546340, "end": 1550954}, {"filename": "/runtime/colors/blue.vim", "start": 1550954, "end": 1567596}, {"filename": "/runtime/colors/catppuccin.vim", "start": 1567596, "end": 1621608}, {"filename": "/runtime/colors/darkblue.vim", "start": 1621608, "end": 1638078}, {"filename": "/runtime/colors/default.vim", "start": 1638078, "end": 1638465}, {"filename": "/runtime/colors/delek.vim", "start": 1638465, "end": 1654971}, {"filename": "/runtime/colors/desert.vim", "start": 1654971, "end": 1671273}, {"filename": "/runtime/colors/elflord.vim", "start": 1671273, "end": 1688363}, {"filename": "/runtime/colors/evening.vim", "start": 1688363, "end": 1704879}, {"filename": "/runtime/colors/habamax.vim", "start": 1704879, "end": 1724972}, {"filename": "/runtime/colors/industry.vim", "start": 1724972, "end": 1741268}, {"filename": "/runtime/colors/koehler.vim", "start": 1741268, "end": 1757479}, {"filename": "/runtime/colors/lunaperche.vim", "start": 1757479, "end": 1800505}, {"filename": "/runtime/colors/morning.vim", "start": 1800505, "end": 1816416}, {"filename": "/runtime/colors/murphy.vim", "start": 1816416, "end": 1832268}, {"filename": "/runtime/colors/pablo.vim", "start": 1832268, "end": 1848341}, {"filename": "/runtime/colors/peachpuff.vim", "start": 1848341, "end": 1864785}, {"filename": "/runtime/colors/quiet.vim", "start": 1864785, "end": 1899183}, {"filename": "/runtime/colors/retrobox.vim", "start": 1899183, "end": 1941849}, {"filename": "/runtime/colors/ron.vim", "start": 1941849, "end": 1958765}, {"filename": "/runtime/colors/shine.vim", "start": 1958765, "end": 1975569}, {"filename": "/runtime/colors/slate.vim", "start": 1975569, "end": 1992529}, {"filename": "/runtime/colors/sorbet.vim", "start": 1992529, "end": 2010875}, {"filename": "/runtime/colors/torte.vim", "start": 2010875, "end": 2026940}, {"filename": "/runtime/colors/unokai.vim", "start": 2026940, "end": 2048304}, {"filename": "/runtime/colors/vim.lua", "start": 2048304, "end": 2068382}, {"filename": "/runtime/colors/wildcharm.vim", "start": 2068382, "end": 2104682}, {"filename": "/runtime/colors/zaibatsu.vim", "start": 2104682, "end": 2121492}, {"filename": "/runtime/colors/zellner.vim", "start": 2121492, "end": 2137908}, {"filename": "/runtime/compiler/README.txt", "start": 2137908, "end": 2138447}, {"filename": "/runtime/compiler/ant.vim", "start": 2138447, "end": 2139336}, {"filename": "/runtime/compiler/bash.vim", "start": 2139336, "end": 2139595}, {"filename": "/runtime/compiler/bcc.vim", "start": 2139595, "end": 2139992}, {"filename": "/runtime/compiler/bdf.vim", "start": 2139992, "end": 2140595}, {"filename": "/runtime/compiler/bean_check.vim", "start": 2140595, "end": 2141069}, {"filename": "/runtime/compiler/biome.vim", "start": 2141069, "end": 2142082}, {"filename": "/runtime/compiler/cabal.vim", "start": 2142082, "end": 2143142}, {"filename": "/runtime/compiler/cargo.vim", "start": 2143142, "end": 2144658}, {"filename": "/runtime/compiler/checkstyle.vim", "start": 2144658, "end": 2145329}, {"filename": "/runtime/compiler/cm3.vim", "start": 2145329, "end": 2145842}, {"filename": "/runtime/compiler/context.vim", "start": 2145842, "end": 2147727}, {"filename": "/runtime/compiler/cppcheck.vim", "start": 2147727, "end": 2149464}, {"filename": "/runtime/compiler/cs.vim", "start": 2149464, "end": 2150104}, {"filename": "/runtime/compiler/csslint.vim", "start": 2150104, "end": 2150594}, {"filename": "/runtime/compiler/cucumber.vim", "start": 2150594, "end": 2151103}, {"filename": "/runtime/compiler/dart.vim", "start": 2151103, "end": 2151532}, {"filename": "/runtime/compiler/dart2js.vim", "start": 2151532, "end": 2152047}, {"filename": "/runtime/compiler/dart2native.vim", "start": 2152047, "end": 2152554}, {"filename": "/runtime/compiler/dartanalyser.vim", "start": 2152554, "end": 2152989}, {"filename": "/runtime/compiler/dartdevc.vim", "start": 2152989, "end": 2153562}, {"filename": "/runtime/compiler/dartdoc.vim", "start": 2153562, "end": 2154060}, {"filename": "/runtime/compiler/dartfmt.vim", "start": 2154060, "end": 2154507}, {"filename": "/runtime/compiler/decada.vim", "start": 2154507, "end": 2156375}, {"filename": "/runtime/compiler/dot.vim", "start": 2156375, "end": 2156887}, {"filename": "/runtime/compiler/dotnet.vim", "start": 2156887, "end": 2157940}, {"filename": "/runtime/compiler/erlang.vim", "start": 2157940, "end": 2158284}, {"filename": "/runtime/compiler/eruby.vim", "start": 2158284, "end": 2159019}, {"filename": "/runtime/compiler/eslint.vim", "start": 2159019, "end": 2159404}, {"filename": "/runtime/compiler/fbc.vim", "start": 2159404, "end": 2159920}, {"filename": "/runtime/compiler/fortran_F.vim", "start": 2159920, "end": 2160570}, {"filename": "/runtime/compiler/fortran_cv.vim", "start": 2160570, "end": 2161267}, {"filename": "/runtime/compiler/fortran_elf90.vim", "start": 2161267, "end": 2162017}, {"filename": "/runtime/compiler/fortran_g77.vim", "start": 2162017, "end": 2163114}, {"filename": "/runtime/compiler/fortran_lf95.vim", "start": 2163114, "end": 2163745}, {"filename": "/runtime/compiler/fpc.vim", "start": 2163745, "end": 2164156}, {"filename": "/runtime/compiler/g95.vim", "start": 2164156, "end": 2164793}, {"filename": "/runtime/compiler/gawk.vim", "start": 2164793, "end": 2165538}, {"filename": "/runtime/compiler/gcc.vim", "start": 2165538, "end": 2167098}, {"filename": "/runtime/compiler/gfortran.vim", "start": 2167098, "end": 2167717}, {"filename": "/runtime/compiler/ghc.vim", "start": 2167717, "end": 2168258}, {"filename": "/runtime/compiler/gjs.vim", "start": 2168258, "end": 2168701}, {"filename": "/runtime/compiler/gleam_build.vim", "start": 2168701, "end": 2169375}, {"filename": "/runtime/compiler/gm2.vim", "start": 2169375, "end": 2169868}, {"filename": "/runtime/compiler/gnat.vim", "start": 2169868, "end": 2172127}, {"filename": "/runtime/compiler/go.vim", "start": 2172127, "end": 2172648}, {"filename": "/runtime/compiler/groff.vim", "start": 2172648, "end": 2174259}, {"filename": "/runtime/compiler/haml.vim", "start": 2174259, "end": 2174758}, {"filename": "/runtime/compiler/hare.vim", "start": 2174758, "end": 2175332}, {"filename": "/runtime/compiler/hp_acc.vim", "start": 2175332, "end": 2176332}, {"filename": "/runtime/compiler/iar.vim", "start": 2176332, "end": 2177175}, {"filename": "/runtime/compiler/icc.vim", "start": 2177175, "end": 2177708}, {"filename": "/runtime/compiler/icon.vim", "start": 2177708, "end": 2178425}, {"filename": "/runtime/compiler/ifort.vim", "start": 2178425, "end": 2179063}, {"filename": "/runtime/compiler/intel.vim", "start": 2179063, "end": 2179606}, {"filename": "/runtime/compiler/irix5_c.vim", "start": 2179606, "end": 2180216}, {"filename": "/runtime/compiler/irix5_cpp.vim", "start": 2180216, "end": 2180835}, {"filename": "/runtime/compiler/javac.vim", "start": 2180835, "end": 2181494}, {"filename": "/runtime/compiler/jest.vim", "start": 2181494, "end": 2182168}, {"filename": "/runtime/compiler/jikes.vim", "start": 2182168, "end": 2182638}, {"filename": "/runtime/compiler/jjs.vim", "start": 2182638, "end": 2183012}, {"filename": "/runtime/compiler/jq.vim", "start": 2183012, "end": 2183556}, {"filename": "/runtime/compiler/jshint.vim", "start": 2183556, "end": 2183993}, {"filename": "/runtime/compiler/jsonlint.vim", "start": 2183993, "end": 2184457}, {"filename": "/runtime/compiler/just.vim", "start": 2184457, "end": 2184836}, {"filename": "/runtime/compiler/make.vim", "start": 2184836, "end": 2185230}, {"filename": "/runtime/compiler/maven.vim", "start": 2185230, "end": 2188654}, {"filename": "/runtime/compiler/mcs.vim", "start": 2188654, "end": 2189637}, {"filename": "/runtime/compiler/mips_c.vim", "start": 2189637, "end": 2190276}, {"filename": "/runtime/compiler/mipspro_c89.vim", "start": 2190276, "end": 2190945}, {"filename": "/runtime/compiler/mipspro_cpp.vim", "start": 2190945, "end": 2191594}, {"filename": "/runtime/compiler/modelsim_vcom.vim", "start": 2191594, "end": 2192387}, {"filename": "/runtime/compiler/msbuild.vim", "start": 2192387, "end": 2192864}, {"filename": "/runtime/compiler/msvc.vim", "start": 2192864, "end": 2193291}, {"filename": "/runtime/compiler/mypy.vim", "start": 2193291, "end": 2193828}, {"filename": "/runtime/compiler/neato.vim", "start": 2193828, "end": 2194346}, {"filename": "/runtime/compiler/ocaml.vim", "start": 2194346, "end": 2196387}, {"filename": "/runtime/compiler/onsgmls.vim", "start": 2196387, "end": 2196840}, {"filename": "/runtime/compiler/pandoc.vim", "start": 2196840, "end": 2199130}, {"filename": "/runtime/compiler/pbx.vim", "start": 2199130, "end": 2199547}, {"filename": "/runtime/compiler/perl.vim", "start": 2199547, "end": 2200921}, {"filename": "/runtime/compiler/perlcritic.vim", "start": 2200921, "end": 2201693}, {"filename": "/runtime/compiler/php.vim", "start": 2201693, "end": 2202280}, {"filename": "/runtime/compiler/phpstan.vim", "start": 2202280, "end": 2202624}, {"filename": "/runtime/compiler/pip_compile.vim", "start": 2202624, "end": 2204355}, {"filename": "/runtime/compiler/podchecker.vim", "start": 2204355, "end": 2205189}, {"filename": "/runtime/compiler/powershell.vim", "start": 2205189, "end": 2208143}, {"filename": "/runtime/compiler/pylint.vim", "start": 2208143, "end": 2208991}, {"filename": "/runtime/compiler/pyright.vim", "start": 2208991, "end": 2209631}, {"filename": "/runtime/compiler/pytest.vim", "start": 2209631, "end": 2213439}, {"filename": "/runtime/compiler/pyunit.vim", "start": 2213439, "end": 2213794}, {"filename": "/runtime/compiler/raco.vim", "start": 2213794, "end": 2214167}, {"filename": "/runtime/compiler/racomake.vim", "start": 2214167, "end": 2214562}, {"filename": "/runtime/compiler/racosetup.vim", "start": 2214562, "end": 2214953}, {"filename": "/runtime/compiler/racotest.vim", "start": 2214953, "end": 2215350}, {"filename": "/runtime/compiler/rake.vim", "start": 2215350, "end": 2216306}, {"filename": "/runtime/compiler/rhino.vim", "start": 2216306, "end": 2216981}, {"filename": "/runtime/compiler/rime_deployer.vim", "start": 2216981, "end": 2217978}, {"filename": "/runtime/compiler/rspec.vim", "start": 2217978, "end": 2218694}, {"filename": "/runtime/compiler/rst.vim", "start": 2218694, "end": 2219545}, {"filename": "/runtime/compiler/rubocop.vim", "start": 2219545, "end": 2219923}, {"filename": "/runtime/compiler/ruby.vim", "start": 2219923, "end": 2220831}, {"filename": "/runtime/compiler/rubyunit.vim", "start": 2220831, "end": 2221548}, {"filename": "/runtime/compiler/ruff.vim", "start": 2221548, "end": 2222326}, {"filename": "/runtime/compiler/rustc.vim", "start": 2222326, "end": 2223836}, {"filename": "/runtime/compiler/sass.vim", "start": 2223836, "end": 2224374}, {"filename": "/runtime/compiler/scdoc.vim", "start": 2224374, "end": 2224799}, {"filename": "/runtime/compiler/se.vim", "start": 2224799, "end": 2225421}, {"filename": "/runtime/compiler/shellcheck.vim", "start": 2225421, "end": 2225897}, {"filename": "/runtime/compiler/sml.vim", "start": 2225897, "end": 2226424}, {"filename": "/runtime/compiler/spectral.vim", "start": 2226424, "end": 2226816}, {"filename": "/runtime/compiler/splint.vim", "start": 2226816, "end": 2229035}, {"filename": "/runtime/compiler/spotbugs.vim", "start": 2229035, "end": 2238954}, {"filename": "/runtime/compiler/stack.vim", "start": 2238954, "end": 2239824}, {"filename": "/runtime/compiler/standard.vim", "start": 2239824, "end": 2240204}, {"filename": "/runtime/compiler/stylelint.vim", "start": 2240204, "end": 2240735}, {"filename": "/runtime/compiler/svelte-check.vim", "start": 2240735, "end": 2241593}, {"filename": "/runtime/compiler/tcl.vim", "start": 2241593, "end": 2241887}, {"filename": "/runtime/compiler/tex.vim", "start": 2241887, "end": 2243906}, {"filename": "/runtime/compiler/tidy.vim", "start": 2243906, "end": 2244396}, {"filename": "/runtime/compiler/tombi.vim", "start": 2244396, "end": 2246518}, {"filename": "/runtime/compiler/ts-node.vim", "start": 2246518, "end": 2247032}, {"filename": "/runtime/compiler/tsc.vim", "start": 2247032, "end": 2247666}, {"filename": "/runtime/compiler/ty.vim", "start": 2247666, "end": 2248248}, {"filename": "/runtime/compiler/typedoc.vim", "start": 2248248, "end": 2248738}, {"filename": "/runtime/compiler/typst.vim", "start": 2248738, "end": 2249323}, {"filename": "/runtime/compiler/vimdoc.vim", "start": 2249323, "end": 2249962}, {"filename": "/runtime/compiler/xbuild.vim", "start": 2249962, "end": 2250419}, {"filename": "/runtime/compiler/xmllint.vim", "start": 2250419, "end": 2250953}, {"filename": "/runtime/compiler/xmlwf.vim", "start": 2250953, "end": 2251357}, {"filename": "/runtime/compiler/xo.vim", "start": 2251357, "end": 2251852}, {"filename": "/runtime/compiler/yamllint.vim", "start": 2251852, "end": 2252355}, {"filename": "/runtime/compiler/zig.vim", "start": 2252355, "end": 2253659}, {"filename": "/runtime/compiler/zig_build.vim", "start": 2253659, "end": 2254315}, {"filename": "/runtime/compiler/zig_build_exe.vim", "start": 2254315, "end": 2254893}, {"filename": "/runtime/compiler/zig_cc.vim", "start": 2254893, "end": 2255310}, {"filename": "/runtime/compiler/zig_test.vim", "start": 2255310, "end": 2255874}, {"filename": "/runtime/compiler/zsh.vim", "start": 2255874, "end": 2256236}, {"filename": "/runtime/delmenu.vim", "start": 2256236, "end": 2257511}, {"filename": "/runtime/doc/api-ui-events.txt", "start": 2257511, "end": 2298074}, {"filename": "/runtime/doc/api.txt", "start": 2298074, "end": 2477306}, {"filename": "/runtime/doc/autocmd.txt", "start": 2477306, "end": 2556952}, {"filename": "/runtime/doc/change.txt", "start": 2556952, "end": 2637661}, {"filename": "/runtime/doc/channel.txt", "start": 2637661, "end": 2648624}, {"filename": "/runtime/doc/cmdline.txt", "start": 2648624, "end": 2704069}, {"filename": "/runtime/doc/credits.txt", "start": 2704069, "end": 2725614}, {"filename": "/runtime/doc/deprecated.txt", "start": 2725614, "end": 2744667}, {"filename": "/runtime/doc/dev.txt", "start": 2744667, "end": 2779629}, {"filename": "/runtime/doc/dev_arch.txt", "start": 2779629, "end": 2811836}, {"filename": "/runtime/doc/dev_style.txt", "start": 2811836, "end": 2843326}, {"filename": "/runtime/doc/dev_test.txt", "start": 2843326, "end": 2867239}, {"filename": "/runtime/doc/dev_theme.txt", "start": 2867239, "end": 2871894}, {"filename": "/runtime/doc/dev_tools.txt", "start": 2871894, "end": 2891531}, {"filename": "/runtime/doc/dev_vimpatch.txt", "start": 2891531, "end": 2907785}, {"filename": "/runtime/doc/diagnostic.txt", "start": 2907785, "end": 2958840}, {"filename": "/runtime/doc/diff.txt", "start": 2958840, "end": 2981231}, {"filename": "/runtime/doc/digraph.txt", "start": 2981231, "end": 3046144}, {"filename": "/runtime/doc/editing.txt", "start": 3046144, "end": 3118608}, {"filename": "/runtime/doc/faq.txt", "start": 3118608, "end": 3137965}, {"filename": "/runtime/doc/filetype.txt", "start": 3137965, "end": 3186924}, {"filename": "/runtime/doc/fold.txt", "start": 3186924, "end": 3213307}, {"filename": "/runtime/doc/ft_ada.txt", "start": 3213307, "end": 3230527}, {"filename": "/runtime/doc/ft_hare.txt", "start": 3230527, "end": 3233485}, {"filename": "/runtime/doc/ft_ps1.txt", "start": 3233485, "end": 3235700}, {"filename": "/runtime/doc/ft_raku.txt", "start": 3235700, "end": 3239964}, {"filename": "/runtime/doc/ft_rust.txt", "start": 3239964, "end": 3256821}, {"filename": "/runtime/doc/ft_sql.txt", "start": 3256821, "end": 3287257}, {"filename": "/runtime/doc/gui.txt", "start": 3287257, "end": 3335525}, {"filename": "/runtime/doc/health.txt", "start": 3335525, "end": 3341352}, {"filename": "/runtime/doc/help.txt", "start": 3341352, "end": 3347024}, {"filename": "/runtime/doc/helphelp.txt", "start": 3347024, "end": 3373799}, {"filename": "/runtime/doc/if_perl.txt", "start": 3373799, "end": 3382712}, {"filename": "/runtime/doc/if_pyth.txt", "start": 3382712, "end": 3407746}, {"filename": "/runtime/doc/if_ruby.txt", "start": 3407746, "end": 3414070}, {"filename": "/runtime/doc/indent.txt", "start": 3414070, "end": 3457922}, {"filename": "/runtime/doc/index.txt", "start": 3457922, "end": 3537926}, {"filename": "/runtime/doc/insert.txt", "start": 3537926, "end": 3628560}, {"filename": "/runtime/doc/intro.txt", "start": 3628560, "end": 3658019}, {"filename": "/runtime/doc/job_control.txt", "start": 3658019, "end": 3663364}, {"filename": "/runtime/doc/l10n-arabic.txt", "start": 3663364, "end": 3676012}, {"filename": "/runtime/doc/l10n-hebrew.txt", "start": 3676012, "end": 3679918}, {"filename": "/runtime/doc/l10n-russian.txt", "start": 3679918, "end": 3682557}, {"filename": "/runtime/doc/l10n-vietnamese.txt", "start": 3682557, "end": 3685812}, {"filename": "/runtime/doc/lsp.txt", "start": 3685812, "end": 3831241}, {"filename": "/runtime/doc/lua-async.txt", "start": 3831241, "end": 3855224}, {"filename": "/runtime/doc/lua-bit.txt", "start": 3855224, "end": 3872338}, {"filename": "/runtime/doc/lua-guide.txt", "start": 3872338, "end": 3902139}, {"filename": "/runtime/doc/lua-plugin.txt", "start": 3902139, "end": 3916425}, {"filename": "/runtime/doc/lua.txt", "start": 3916425, "end": 4161104}, {"filename": "/runtime/doc/luaref.txt", "start": 4161104, "end": 4388199}, {"filename": "/runtime/doc/luvref.txt", "start": 4388199, "end": 4626795}, {"filename": "/runtime/doc/map.txt", "start": 4626795, "end": 4701487}, {"filename": "/runtime/doc/mbyte.txt", "start": 4701487, "end": 4730820}, {"filename": "/runtime/doc/message.txt", "start": 4730820, "end": 4763478}, {"filename": "/runtime/doc/mlang.txt", "start": 4763478, "end": 4770878}, {"filename": "/runtime/doc/motion.txt", "start": 4770878, "end": 4828129}, {"filename": "/runtime/doc/news-0.10.txt", "start": 4828129, "end": 4852188}, {"filename": "/runtime/doc/news-0.11.txt", "start": 4852188, "end": 4876344}, {"filename": "/runtime/doc/news-0.12.txt", "start": 4876344, "end": 4899895}, {"filename": "/runtime/doc/news-0.9.txt", "start": 4899895, "end": 4911993}, {"filename": "/runtime/doc/news.txt", "start": 4911993, "end": 4945596}, {"filename": "/runtime/doc/nvim.txt", "start": 4945596, "end": 4949384}, {"filename": "/runtime/doc/options.txt", "start": 4949384, "end": 5295311}, {"filename": "/runtime/doc/pack.txt", "start": 5295311, "end": 5325343}, {"filename": "/runtime/doc/pattern.txt", "start": 5325343, "end": 5390598}, {"filename": "/runtime/doc/pi_gzip.txt", "start": 5390598, "end": 5392128}, {"filename": "/runtime/doc/pi_msgpack.txt", "start": 5392128, "end": 5397924}, {"filename": "/runtime/doc/pi_paren.txt", "start": 5397924, "end": 5400674}, {"filename": "/runtime/doc/pi_spec.txt", "start": 5400674, "end": 5405101}, {"filename": "/runtime/doc/pi_tar.txt", "start": 5405101, "end": 5412363}, {"filename": "/runtime/doc/pi_tutor.txt", "start": 5412363, "end": 5414161}, {"filename": "/runtime/doc/plugins.txt", "start": 5414161, "end": 5433547}, {"filename": "/runtime/doc/provider.txt", "start": 5433547, "end": 5447608}, {"filename": "/runtime/doc/quickfix.txt", "start": 5447608, "end": 5551694}, {"filename": "/runtime/doc/quickref.txt", "start": 5551694, "end": 5619265}, {"filename": "/runtime/doc/recover.txt", "start": 5619265, "end": 5627841}, {"filename": "/runtime/doc/remote.txt", "start": 5627841, "end": 5632060}, {"filename": "/runtime/doc/remote_plugin.txt", "start": 5632060, "end": 5638269}, {"filename": "/runtime/doc/repeat.txt", "start": 5638269, "end": 5659251}, {"filename": "/runtime/doc/rileft.txt", "start": 5659251, "end": 5664445}, {"filename": "/runtime/doc/scroll.txt", "start": 5664445, "end": 5675165}, {"filename": "/runtime/doc/sign.txt", "start": 5675165, "end": 5688383}, {"filename": "/runtime/doc/spell.txt", "start": 5688383, "end": 5756661}, {"filename": "/runtime/doc/starting.txt", "start": 5756661, "end": 5836256}, {"filename": "/runtime/doc/support.txt", "start": 5836256, "end": 5838387}, {"filename": "/runtime/doc/syntax.txt", "start": 5838387, "end": 6063362}, {"filename": "/runtime/doc/tabpage.txt", "start": 6063362, "end": 6081023}, {"filename": "/runtime/doc/tags", "start": 6081023, "end": 6501071}, {"filename": "/runtime/doc/tagsrch.txt", "start": 6501071, "end": 6541801}, {"filename": "/runtime/doc/terminal.txt", "start": 6541801, "end": 6572882}, {"filename": "/runtime/doc/tips.txt", "start": 6572882, "end": 6590793}, {"filename": "/runtime/doc/treesitter.txt", "start": 6590793, "end": 6670708}, {"filename": "/runtime/doc/tui.txt", "start": 6670708, "end": 6687662}, {"filename": "/runtime/doc/uganda.txt", "start": 6687662, "end": 6699184}, {"filename": "/runtime/doc/undo.txt", "start": 6699184, "end": 6716404}, {"filename": "/runtime/doc/userfunc.txt", "start": 6716404, "end": 6735351}, {"filename": "/runtime/doc/usr_01.txt", "start": 6735351, "end": 6739462}, {"filename": "/runtime/doc/usr_02.txt", "start": 6739462, "end": 6763193}, {"filename": "/runtime/doc/usr_03.txt", "start": 6763193, "end": 6786849}, {"filename": "/runtime/doc/usr_04.txt", "start": 6786849, "end": 6806076}, {"filename": "/runtime/doc/usr_05.txt", "start": 6806076, "end": 6827641}, {"filename": "/runtime/doc/usr_06.txt", "start": 6827641, "end": 6834595}, {"filename": "/runtime/doc/usr_07.txt", "start": 6834595, "end": 6850622}, {"filename": "/runtime/doc/usr_08.txt", "start": 6850622, "end": 6870194}, {"filename": "/runtime/doc/usr_09.txt", "start": 6870194, "end": 6880627}, {"filename": "/runtime/doc/usr_10.txt", "start": 6880627, "end": 6910250}, {"filename": "/runtime/doc/usr_11.txt", "start": 6910250, "end": 6922995}, {"filename": "/runtime/doc/usr_12.txt", "start": 6922995, "end": 6935896}, {"filename": "/runtime/doc/usr_20.txt", "start": 6935896, "end": 6949904}, {"filename": "/runtime/doc/usr_21.txt", "start": 6949904, "end": 6967956}, {"filename": "/runtime/doc/usr_22.txt", "start": 6967956, "end": 6978752}, {"filename": "/runtime/doc/usr_23.txt", "start": 6978752, "end": 6987941}, {"filename": "/runtime/doc/usr_24.txt", "start": 6987941, "end": 7008884}, {"filename": "/runtime/doc/usr_25.txt", "start": 7008884, "end": 7028272}, {"filename": "/runtime/doc/usr_26.txt", "start": 7028272, "end": 7036281}, {"filename": "/runtime/doc/usr_27.txt", "start": 7036281, "end": 7054050}, {"filename": "/runtime/doc/usr_28.txt", "start": 7054050, "end": 7070100}, {"filename": "/runtime/doc/usr_29.txt", "start": 7070100, "end": 7090199}, {"filename": "/runtime/doc/usr_30.txt", "start": 7090199, "end": 7116236}, {"filename": "/runtime/doc/usr_31.txt", "start": 7116236, "end": 7123845}, {"filename": "/runtime/doc/usr_32.txt", "start": 7123845, "end": 7129251}, {"filename": "/runtime/doc/usr_40.txt", "start": 7129251, "end": 7152794}, {"filename": "/runtime/doc/usr_41.txt", "start": 7152794, "end": 7248077}, {"filename": "/runtime/doc/usr_42.txt", "start": 7248077, "end": 7261691}, {"filename": "/runtime/doc/usr_43.txt", "start": 7261691, "end": 7269188}, {"filename": "/runtime/doc/usr_44.txt", "start": 7269188, "end": 7298196}, {"filename": "/runtime/doc/usr_45.txt", "start": 7298196, "end": 7314018}, {"filename": "/runtime/doc/usr_toc.txt", "start": 7314018, "end": 7322885}, {"filename": "/runtime/doc/various.txt", "start": 7322885, "end": 7346987}, {"filename": "/runtime/doc/vi_diff.txt", "start": 7346987, "end": 7363817}, {"filename": "/runtime/doc/vim_diff.txt", "start": 7363817, "end": 7404229}, {"filename": "/runtime/doc/vimeval.txt", "start": 7404229, "end": 7544030}, {"filename": "/runtime/doc/vimfn.txt", "start": 7544030, "end": 8024805}, {"filename": "/runtime/doc/vimscript.txt", "start": 8024805, "end": 8052864}, {"filename": "/runtime/doc/visual.txt", "start": 8052864, "end": 8076414}, {"filename": "/runtime/doc/vvars.txt", "start": 8076414, "end": 8109387}, {"filename": "/runtime/doc/windows.txt", "start": 8109387, "end": 8168699}, {"filename": "/runtime/doc/zip.txt", "start": 8168699, "end": 8170726}, {"filename": "/runtime/embedded_data.zig", "start": 8170726, "end": 8170916}, {"filename": "/runtime/example_init.lua", "start": 8170916, "end": 8174740}, {"filename": "/runtime/filetype.lua", "start": 8174740, "end": 8176410}, {"filename": "/runtime/ftoff.vim", "start": 8176410, "end": 8176756}, {"filename": "/runtime/ftplugin.vim", "start": 8176756, "end": 8178026}, {"filename": "/runtime/ftplugin/8th.vim", "start": 8178026, "end": 8178859}, {"filename": "/runtime/ftplugin/README.txt", "start": 8178859, "end": 8179728}, {"filename": "/runtime/ftplugin/a2ps.vim", "start": 8179728, "end": 8180198}, {"filename": "/runtime/ftplugin/aap.vim", "start": 8180198, "end": 8181506}, {"filename": "/runtime/ftplugin/abap.vim", "start": 8181506, "end": 8182947}, {"filename": "/runtime/ftplugin/abaqus.vim", "start": 8182947, "end": 8187298}, {"filename": "/runtime/ftplugin/abnf.vim", "start": 8187298, "end": 8187677}, {"filename": "/runtime/ftplugin/ada.vim", "start": 8187677, "end": 8194763}, {"filename": "/runtime/ftplugin/algol68.vim", "start": 8194763, "end": 8196085}, {"filename": "/runtime/ftplugin/alsaconf.vim", "start": 8196085, "end": 8196523}, {"filename": "/runtime/ftplugin/ant.vim", "start": 8196523, "end": 8198027}, {"filename": "/runtime/ftplugin/antlr4.vim", "start": 8198027, "end": 8198408}, {"filename": "/runtime/ftplugin/apache.vim", "start": 8198408, "end": 8198769}, {"filename": "/runtime/ftplugin/arch.vim", "start": 8198769, "end": 8199200}, {"filename": "/runtime/ftplugin/arduino.lua", "start": 8199200, "end": 8199310}, {"filename": "/runtime/ftplugin/arduino.vim", "start": 8199310, "end": 8201571}, {"filename": "/runtime/ftplugin/art.vim", "start": 8201571, "end": 8202080}, {"filename": "/runtime/ftplugin/ascendc.vim", "start": 8202080, "end": 8202304}, {"filename": "/runtime/ftplugin/asciidoc.vim", "start": 8202304, "end": 8204314}, {"filename": "/runtime/ftplugin/asm.vim", "start": 8204314, "end": 8205307}, {"filename": "/runtime/ftplugin/aspvbs.vim", "start": 8205307, "end": 8207386}, {"filename": "/runtime/ftplugin/astro.vim", "start": 8207386, "end": 8213312}, {"filename": "/runtime/ftplugin/asy.vim", "start": 8213312, "end": 8213591}, {"filename": "/runtime/ftplugin/autohotkey.vim", "start": 8213591, "end": 8213945}, {"filename": "/runtime/ftplugin/automake.vim", "start": 8213945, "end": 8214302}, {"filename": "/runtime/ftplugin/autopkgtest.vim", "start": 8214302, "end": 8214770}, {"filename": "/runtime/ftplugin/awk.vim", "start": 8214770, "end": 8216490}, {"filename": "/runtime/ftplugin/bash.vim", "start": 8216490, "end": 8217014}, {"filename": "/runtime/ftplugin/basic.vim", "start": 8217014, "end": 8219181}, {"filename": "/runtime/ftplugin/bats.vim", "start": 8219181, "end": 8219639}, {"filename": "/runtime/ftplugin/bazelrc.vim", "start": 8219639, "end": 8219993}, {"filename": "/runtime/ftplugin/bdf.vim", "start": 8219993, "end": 8220442}, {"filename": "/runtime/ftplugin/beancount.vim", "start": 8220442, "end": 8221445}, {"filename": "/runtime/ftplugin/bicep-params.vim", "start": 8221445, "end": 8221647}, {"filename": "/runtime/ftplugin/bicep.vim", "start": 8221647, "end": 8221963}, {"filename": "/runtime/ftplugin/bindzone.vim", "start": 8221963, "end": 8222328}, {"filename": "/runtime/ftplugin/bitbake.vim", "start": 8222328, "end": 8222789}, {"filename": "/runtime/ftplugin/bp.vim", "start": 8222789, "end": 8223135}, {"filename": "/runtime/ftplugin/bpftrace.vim", "start": 8223135, "end": 8223539}, {"filename": "/runtime/ftplugin/brighterscript.vim", "start": 8223539, "end": 8223773}, {"filename": "/runtime/ftplugin/brightscript.vim", "start": 8223773, "end": 8224039}, {"filename": "/runtime/ftplugin/bst.vim", "start": 8224039, "end": 8224372}, {"filename": "/runtime/ftplugin/btm.vim", "start": 8224372, "end": 8224774}, {"filename": "/runtime/ftplugin/bzl.vim", "start": 8224774, "end": 8228075}, {"filename": "/runtime/ftplugin/c.lua", "start": 8228075, "end": 8228517}, {"filename": "/runtime/ftplugin/c.vim", "start": 8228517, "end": 8231010}, {"filename": "/runtime/ftplugin/c3.vim", "start": 8231010, "end": 8231336}, {"filename": "/runtime/ftplugin/cabal.vim", "start": 8231336, "end": 8231939}, {"filename": "/runtime/ftplugin/calendar.vim", "start": 8231939, "end": 8232500}, {"filename": "/runtime/ftplugin/cdrdaoconf.vim", "start": 8232500, "end": 8232883}, {"filename": "/runtime/ftplugin/cedar.vim", "start": 8232883, "end": 8233143}, {"filename": "/runtime/ftplugin/cfg.vim", "start": 8233143, "end": 8233531}, {"filename": "/runtime/ftplugin/cgdbrc.vim", "start": 8233531, "end": 8234111}, {"filename": "/runtime/ftplugin/ch.lua", "start": 8234111, "end": 8234221}, {"filename": "/runtime/ftplugin/ch.vim", "start": 8234221, "end": 8234713}, {"filename": "/runtime/ftplugin/changelog.vim", "start": 8234713, "end": 8244220}, {"filename": "/runtime/ftplugin/chatito.vim", "start": 8244220, "end": 8244603}, {"filename": "/runtime/ftplugin/checkhealth.lua", "start": 8244603, "end": 8245832}, {"filename": "/runtime/ftplugin/chicken.vim", "start": 8245832, "end": 8247597}, {"filename": "/runtime/ftplugin/clojure.vim", "start": 8247597, "end": 8251080}, {"filename": "/runtime/ftplugin/cmake.vim", "start": 8251080, "end": 8252084}, {"filename": "/runtime/ftplugin/cmakecache.vim", "start": 8252084, "end": 8252395}, {"filename": "/runtime/ftplugin/cobol.vim", "start": 8252395, "end": 8262262}, {"filename": "/runtime/ftplugin/codeowners.vim", "start": 8262262, "end": 8262572}, {"filename": "/runtime/ftplugin/conf.vim", "start": 8262572, "end": 8263006}, {"filename": "/runtime/ftplugin/config.vim", "start": 8263006, "end": 8264448}, {"filename": "/runtime/ftplugin/confini.vim", "start": 8264448, "end": 8264662}, {"filename": "/runtime/ftplugin/context.vim", "start": 8264662, "end": 8270251}, {"filename": "/runtime/ftplugin/cook.vim", "start": 8270251, "end": 8270515}, {"filename": "/runtime/ftplugin/corn.vim", "start": 8270515, "end": 8270972}, {"filename": "/runtime/ftplugin/cpp.vim", "start": 8270972, "end": 8271732}, {"filename": "/runtime/ftplugin/crm.vim", "start": 8271732, "end": 8272146}, {"filename": "/runtime/ftplugin/crontab.vim", "start": 8272146, "end": 8272507}, {"filename": "/runtime/ftplugin/cs.lua", "start": 8272507, "end": 8272617}, {"filename": "/runtime/ftplugin/cs.vim", "start": 8272617, "end": 8274261}, {"filename": "/runtime/ftplugin/csc.vim", "start": 8274261, "end": 8274958}, {"filename": "/runtime/ftplugin/csh.vim", "start": 8274958, "end": 8277274}, {"filename": "/runtime/ftplugin/css.vim", "start": 8277274, "end": 8277969}, {"filename": "/runtime/ftplugin/csv.vim", "start": 8277969, "end": 8278480}, {"filename": "/runtime/ftplugin/cucumber.vim", "start": 8278480, "end": 8283789}, {"filename": "/runtime/ftplugin/cuda.vim", "start": 8283789, "end": 8284012}, {"filename": "/runtime/ftplugin/cvsrc.vim", "start": 8284012, "end": 8284411}, {"filename": "/runtime/ftplugin/d.lua", "start": 8284411, "end": 8284521}, {"filename": "/runtime/ftplugin/dart.vim", "start": 8284521, "end": 8284918}, {"filename": "/runtime/ftplugin/dax.vim", "start": 8284918, "end": 8285266}, {"filename": "/runtime/ftplugin/deb822sources.vim", "start": 8285266, "end": 8285741}, {"filename": "/runtime/ftplugin/debchangelog.vim", "start": 8285741, "end": 8297773}, {"filename": "/runtime/ftplugin/debcontrol.vim", "start": 8297773, "end": 8299693}, {"filename": "/runtime/ftplugin/debsources.vim", "start": 8299693, "end": 8300165}, {"filename": "/runtime/ftplugin/denyhosts.vim", "start": 8300165, "end": 8300548}, {"filename": "/runtime/ftplugin/desktop.vim", "start": 8300548, "end": 8300930}, {"filename": "/runtime/ftplugin/dictconf.vim", "start": 8300930, "end": 8301364}, {"filename": "/runtime/ftplugin/dictdconf.vim", "start": 8301364, "end": 8301799}, {"filename": "/runtime/ftplugin/diff.vim", "start": 8301799, "end": 8302651}, {"filename": "/runtime/ftplugin/dircolors.vim", "start": 8302651, "end": 8303082}, {"filename": "/runtime/ftplugin/dnsmasq.vim", "start": 8303082, "end": 8303301}, {"filename": "/runtime/ftplugin/docbk.vim", "start": 8303301, "end": 8303871}, {"filename": "/runtime/ftplugin/dockerfile.vim", "start": 8303871, "end": 8304258}, {"filename": "/runtime/ftplugin/dosbatch.vim", "start": 8304258, "end": 8305668}, {"filename": "/runtime/ftplugin/dosini.vim", "start": 8305668, "end": 8306206}, {"filename": "/runtime/ftplugin/dot.lua", "start": 8306206, "end": 8306316}, {"filename": "/runtime/ftplugin/dtd.vim", "start": 8306316, "end": 8307670}, {"filename": "/runtime/ftplugin/dtrace.vim", "start": 8307670, "end": 8308908}, {"filename": "/runtime/ftplugin/dts.vim", "start": 8308908, "end": 8309443}, {"filename": "/runtime/ftplugin/dune.vim", "start": 8309443, "end": 8310116}, {"filename": "/runtime/ftplugin/editorconfig.vim", "start": 8310116, "end": 8310429}, {"filename": "/runtime/ftplugin/eiffel.vim", "start": 8310429, "end": 8314610}, {"filename": "/runtime/ftplugin/elinks.vim", "start": 8314610, "end": 8315046}, {"filename": "/runtime/ftplugin/elixir.vim", "start": 8315046, "end": 8316204}, {"filename": "/runtime/ftplugin/elm.vim", "start": 8316204, "end": 8316573}, {"filename": "/runtime/ftplugin/env.vim", "start": 8316573, "end": 8316979}, {"filename": "/runtime/ftplugin/erlang.vim", "start": 8316979, "end": 8320323}, {"filename": "/runtime/ftplugin/eruby.vim", "start": 8320323, "end": 8325341}, {"filename": "/runtime/ftplugin/eterm.vim", "start": 8325341, "end": 8325811}, {"filename": "/runtime/ftplugin/expect.vim", "start": 8325811, "end": 8326415}, {"filename": "/runtime/ftplugin/exports.vim", "start": 8326415, "end": 8326756}, {"filename": "/runtime/ftplugin/falcon.vim", "start": 8326756, "end": 8328440}, {"filename": "/runtime/ftplugin/faust.lua", "start": 8328440, "end": 8328550}, {"filename": "/runtime/ftplugin/fennel.vim", "start": 8328550, "end": 8329327}, {"filename": "/runtime/ftplugin/fetchmail.vim", "start": 8329327, "end": 8329724}, {"filename": "/runtime/ftplugin/fga.vim", "start": 8329724, "end": 8329981}, {"filename": "/runtime/ftplugin/fish.vim", "start": 8329981, "end": 8331155}, {"filename": "/runtime/ftplugin/flexwiki.vim", "start": 8331155, "end": 8333031}, {"filename": "/runtime/ftplugin/forth.vim", "start": 8333031, "end": 8335062}, {"filename": "/runtime/ftplugin/fortran.vim", "start": 8335062, "end": 8341006}, {"filename": "/runtime/ftplugin/fpcmake.vim", "start": 8341006, "end": 8341685}, {"filename": "/runtime/ftplugin/framescript.vim", "start": 8341685, "end": 8342465}, {"filename": "/runtime/ftplugin/freebasic.vim", "start": 8342465, "end": 8345152}, {"filename": "/runtime/ftplugin/fstab.vim", "start": 8345152, "end": 8345871}, {"filename": "/runtime/ftplugin/fvwm.vim", "start": 8345871, "end": 8346261}, {"filename": "/runtime/ftplugin/gdb.vim", "start": 8346261, "end": 8347993}, {"filename": "/runtime/ftplugin/gdscript.vim", "start": 8347993, "end": 8350239}, {"filename": "/runtime/ftplugin/gdshader.vim", "start": 8350239, "end": 8350749}, {"filename": "/runtime/ftplugin/gel.vim", "start": 8350749, "end": 8351224}, {"filename": "/runtime/ftplugin/ghostty.vim", "start": 8351224, "end": 8351438}, {"filename": "/runtime/ftplugin/git.vim", "start": 8351438, "end": 8351743}, {"filename": "/runtime/ftplugin/gitattributes.vim", "start": 8351743, "end": 8352021}, {"filename": "/runtime/ftplugin/gitcommit.vim", "start": 8352021, "end": 8354099}, {"filename": "/runtime/ftplugin/gitconfig.vim", "start": 8354099, "end": 8354475}, {"filename": "/runtime/ftplugin/gitignore.vim", "start": 8354475, "end": 8354944}, {"filename": "/runtime/ftplugin/gitrebase.vim", "start": 8354944, "end": 8356732}, {"filename": "/runtime/ftplugin/gitrevlist.vim", "start": 8356732, "end": 8357167}, {"filename": "/runtime/ftplugin/gitsendemail.vim", "start": 8357167, "end": 8357332}, {"filename": "/runtime/ftplugin/gleam.vim", "start": 8357332, "end": 8358373}, {"filename": "/runtime/ftplugin/glsl.lua", "start": 8358373, "end": 8358483}, {"filename": "/runtime/ftplugin/go.vim", "start": 8358483, "end": 8361470}, {"filename": "/runtime/ftplugin/goaccess.vim", "start": 8361470, "end": 8361745}, {"filename": "/runtime/ftplugin/gomod.vim", "start": 8361745, "end": 8362129}, {"filename": "/runtime/ftplugin/gpg.vim", "start": 8362129, "end": 8363085}, {"filename": "/runtime/ftplugin/gprof.vim", "start": 8363085, "end": 8364205}, {"filename": "/runtime/ftplugin/graphql.vim", "start": 8364205, "end": 8364920}, {"filename": "/runtime/ftplugin/groff.vim", "start": 8364920, "end": 8365381}, {"filename": "/runtime/ftplugin/groovy.vim", "start": 8365381, "end": 8365796}, {"filename": "/runtime/ftplugin/group.vim", "start": 8365796, "end": 8366221}, {"filename": "/runtime/ftplugin/grub.vim", "start": 8366221, "end": 8366655}, {"filename": "/runtime/ftplugin/gyp.vim", "start": 8366655, "end": 8366966}, {"filename": "/runtime/ftplugin/haml.vim", "start": 8366966, "end": 8369125}, {"filename": "/runtime/ftplugin/hamster.vim", "start": 8369125, "end": 8371249}, {"filename": "/runtime/ftplugin/hare.vim", "start": 8371249, "end": 8372809}, {"filename": "/runtime/ftplugin/haredoc.vim", "start": 8372809, "end": 8374001}, {"filename": "/runtime/ftplugin/haskell.vim", "start": 8374001, "end": 8375021}, {"filename": "/runtime/ftplugin/hcl.vim", "start": 8375021, "end": 8375206}, {"filename": "/runtime/ftplugin/heex.vim", "start": 8375206, "end": 8376342}, {"filename": "/runtime/ftplugin/help.lua", "start": 8376342, "end": 8382315}, {"filename": "/runtime/ftplugin/help.vim", "start": 8382315, "end": 8383719}, {"filename": "/runtime/ftplugin/hgcommit.vim", "start": 8383719, "end": 8384215}, {"filename": "/runtime/ftplugin/hip.vim", "start": 8384215, "end": 8384491}, {"filename": "/runtime/ftplugin/hlsl.vim", "start": 8384491, "end": 8384942}, {"filename": "/runtime/ftplugin/hlsplaylist.vim", "start": 8384942, "end": 8385791}, {"filename": "/runtime/ftplugin/hog.vim", "start": 8385791, "end": 8387245}, {"filename": "/runtime/ftplugin/hostconf.vim", "start": 8387245, "end": 8387628}, {"filename": "/runtime/ftplugin/hostsaccess.vim", "start": 8387628, "end": 8388064}, {"filename": "/runtime/ftplugin/html.vim", "start": 8388064, "end": 8391310}, {"filename": "/runtime/ftplugin/htmlangular.vim", "start": 8391310, "end": 8391616}, {"filename": "/runtime/ftplugin/htmldjango.vim", "start": 8391616, "end": 8391963}, {"filename": "/runtime/ftplugin/http.vim", "start": 8391963, "end": 8392221}, {"filename": "/runtime/ftplugin/hurl.vim", "start": 8392221, "end": 8392498}, {"filename": "/runtime/ftplugin/hyprlang.vim", "start": 8392498, "end": 8392755}, {"filename": "/runtime/ftplugin/i3config.vim", "start": 8392755, "end": 8393088}, {"filename": "/runtime/ftplugin/icalendar.vim", "start": 8393088, "end": 8394585}, {"filename": "/runtime/ftplugin/icon.vim", "start": 8394585, "end": 8395655}, {"filename": "/runtime/ftplugin/idris2.vim", "start": 8395655, "end": 8396821}, {"filename": "/runtime/ftplugin/indent.lua", "start": 8396821, "end": 8396931}, {"filename": "/runtime/ftplugin/indent.vim", "start": 8396931, "end": 8397497}, {"filename": "/runtime/ftplugin/initex.vim", "start": 8397497, "end": 8398589}, {"filename": "/runtime/ftplugin/ipkg.vim", "start": 8398589, "end": 8399134}, {"filename": "/runtime/ftplugin/ishd.vim", "start": 8399134, "end": 8400894}, {"filename": "/runtime/ftplugin/j.vim", "start": 8400894, "end": 8404558}, {"filename": "/runtime/ftplugin/java.vim", "start": 8404558, "end": 8418704}, {"filename": "/runtime/ftplugin/javacc.vim", "start": 8418704, "end": 8419267}, {"filename": "/runtime/ftplugin/javascript.vim", "start": 8419267, "end": 8422042}, {"filename": "/runtime/ftplugin/javascriptreact.vim", "start": 8422042, "end": 8422294}, {"filename": "/runtime/ftplugin/jinja.vim", "start": 8422294, "end": 8422532}, {"filename": "/runtime/ftplugin/jjdescription.vim", "start": 8422532, "end": 8423170}, {"filename": "/runtime/ftplugin/jproperties.vim", "start": 8423170, "end": 8423462}, {"filename": "/runtime/ftplugin/jq.vim", "start": 8423462, "end": 8424019}, {"filename": "/runtime/ftplugin/json.vim", "start": 8424019, "end": 8424379}, {"filename": "/runtime/ftplugin/json5.vim", "start": 8424379, "end": 8425194}, {"filename": "/runtime/ftplugin/jsonc.vim", "start": 8425194, "end": 8425967}, {"filename": "/runtime/ftplugin/jsonld.vim", "start": 8425967, "end": 8426167}, {"filename": "/runtime/ftplugin/jsonnet.vim", "start": 8426167, "end": 8426598}, {"filename": "/runtime/ftplugin/jsp.vim", "start": 8426598, "end": 8428796}, {"filename": "/runtime/ftplugin/julia.vim", "start": 8428796, "end": 8432546}, {"filename": "/runtime/ftplugin/just.vim", "start": 8432546, "end": 8433093}, {"filename": "/runtime/ftplugin/karel.vim", "start": 8433093, "end": 8433489}, {"filename": "/runtime/ftplugin/kconfig.vim", "start": 8433489, "end": 8434176}, {"filename": "/runtime/ftplugin/kdl.vim", "start": 8434176, "end": 8434585}, {"filename": "/runtime/ftplugin/kerml.vim", "start": 8434585, "end": 8435442}, {"filename": "/runtime/ftplugin/kitty.vim", "start": 8435442, "end": 8435745}, {"filename": "/runtime/ftplugin/kivy.vim", "start": 8435745, "end": 8435986}, {"filename": "/runtime/ftplugin/kotlin.vim", "start": 8435986, "end": 8437057}, {"filename": "/runtime/ftplugin/kwt.vim", "start": 8437057, "end": 8438095}, {"filename": "/runtime/ftplugin/lc.vim", "start": 8438095, "end": 8438355}, {"filename": "/runtime/ftplugin/ld.vim", "start": 8438355, "end": 8438920}, {"filename": "/runtime/ftplugin/ldapconf.vim", "start": 8438920, "end": 8439206}, {"filename": "/runtime/ftplugin/leex.vim", "start": 8439206, "end": 8439535}, {"filename": "/runtime/ftplugin/leo.vim", "start": 8439535, "end": 8439794}, {"filename": "/runtime/ftplugin/less.vim", "start": 8439794, "end": 8440296}, {"filename": "/runtime/ftplugin/lex.vim", "start": 8440296, "end": 8440618}, {"filename": "/runtime/ftplugin/lf.vim", "start": 8440618, "end": 8441142}, {"filename": "/runtime/ftplugin/lftp.vim", "start": 8441142, "end": 8441576}, {"filename": "/runtime/ftplugin/libao.vim", "start": 8441576, "end": 8442016}, {"filename": "/runtime/ftplugin/limits.vim", "start": 8442016, "end": 8442452}, {"filename": "/runtime/ftplugin/liquid.vim", "start": 8442452, "end": 8444475}, {"filename": "/runtime/ftplugin/lisp.vim", "start": 8444475, "end": 8445320}, {"filename": "/runtime/ftplugin/livebook.vim", "start": 8445320, "end": 8445610}, {"filename": "/runtime/ftplugin/llvm.vim", "start": 8445610, "end": 8445896}, {"filename": "/runtime/ftplugin/lnk.vim", "start": 8445896, "end": 8446388}, {"filename": "/runtime/ftplugin/lnkmap.vim", "start": 8446388, "end": 8446786}, {"filename": "/runtime/ftplugin/logcheck.vim", "start": 8446786, "end": 8447237}, {"filename": "/runtime/ftplugin/loginaccess.vim", "start": 8447237, "end": 8447679}, {"filename": "/runtime/ftplugin/logindefs.vim", "start": 8447679, "end": 8448119}, {"filename": "/runtime/ftplugin/logtalk.dict", "start": 8448119, "end": 8451345}, {"filename": "/runtime/ftplugin/logtalk.vim", "start": 8451345, "end": 8451761}, {"filename": "/runtime/ftplugin/lprolog.vim", "start": 8451761, "end": 8453695}, {"filename": "/runtime/ftplugin/lua.lua", "start": 8453695, "end": 8454063}, {"filename": "/runtime/ftplugin/lua.vim", "start": 8454063, "end": 8456536}, {"filename": "/runtime/ftplugin/luau.vim", "start": 8456536, "end": 8462883}, {"filename": "/runtime/ftplugin/lynx.vim", "start": 8462883, "end": 8463700}, {"filename": "/runtime/ftplugin/m17ndb.vim", "start": 8463700, "end": 8464116}, {"filename": "/runtime/ftplugin/m3build.vim", "start": 8464116, "end": 8464757}, {"filename": "/runtime/ftplugin/m3quake.vim", "start": 8464757, "end": 8465806}, {"filename": "/runtime/ftplugin/m4.vim", "start": 8465806, "end": 8466232}, {"filename": "/runtime/ftplugin/mail.vim", "start": 8466232, "end": 8467838}, {"filename": "/runtime/ftplugin/mailaliases.vim", "start": 8467838, "end": 8468221}, {"filename": "/runtime/ftplugin/mailcap.vim", "start": 8468221, "end": 8468655}, {"filename": "/runtime/ftplugin/make.vim", "start": 8468655, "end": 8469780}, {"filename": "/runtime/ftplugin/man.vim", "start": 8469780, "end": 8471137}, {"filename": "/runtime/ftplugin/manconf.vim", "start": 8471137, "end": 8471581}, {"filename": "/runtime/ftplugin/markdown.lua", "start": 8471581, "end": 8472304}, {"filename": "/runtime/ftplugin/markdown.vim", "start": 8472304, "end": 8475410}, {"filename": "/runtime/ftplugin/masm.vim", "start": 8475410, "end": 8476514}, {"filename": "/runtime/ftplugin/matlab.vim", "start": 8476514, "end": 8477349}, {"filename": "/runtime/ftplugin/mbsync.vim", "start": 8477349, "end": 8477649}, {"filename": "/runtime/ftplugin/mediawiki.vim", "start": 8477649, "end": 8479367}, {"filename": "/runtime/ftplugin/mermaid.vim", "start": 8479367, "end": 8481304}, {"filename": "/runtime/ftplugin/meson.vim", "start": 8481304, "end": 8482702}, {"filename": "/runtime/ftplugin/mf.vim", "start": 8482702, "end": 8485951}, {"filename": "/runtime/ftplugin/mlir.vim", "start": 8485951, "end": 8486169}, {"filename": "/runtime/ftplugin/mma.vim", "start": 8486169, "end": 8486605}, {"filename": "/runtime/ftplugin/modconf.vim", "start": 8486605, "end": 8487624}, {"filename": "/runtime/ftplugin/modula2.vim", "start": 8487624, "end": 8489201}, {"filename": "/runtime/ftplugin/modula3.vim", "start": 8489201, "end": 8490586}, {"filename": "/runtime/ftplugin/mojo.vim", "start": 8490586, "end": 8491957}, {"filename": "/runtime/ftplugin/mp.vim", "start": 8491957, "end": 8495669}, {"filename": "/runtime/ftplugin/mplayerconf.vim", "start": 8495669, "end": 8496141}, {"filename": "/runtime/ftplugin/mrxvtrc.vim", "start": 8496141, "end": 8496939}, {"filename": "/runtime/ftplugin/msmessages.vim", "start": 8496939, "end": 8498252}, {"filename": "/runtime/ftplugin/mss.vim", "start": 8498252, "end": 8498719}, {"filename": "/runtime/ftplugin/muttrc.vim", "start": 8498719, "end": 8499717}, {"filename": "/runtime/ftplugin/mysql.vim", "start": 8499717, "end": 8499900}, {"filename": "/runtime/ftplugin/nanorc.vim", "start": 8499900, "end": 8500347}, {"filename": "/runtime/ftplugin/nasm.vim", "start": 8500347, "end": 8500927}, {"filename": "/runtime/ftplugin/neomuttrc.vim", "start": 8500927, "end": 8501459}, {"filename": "/runtime/ftplugin/netrc.vim", "start": 8501459, "end": 8501939}, {"filename": "/runtime/ftplugin/nginx.vim", "start": 8501939, "end": 8502190}, {"filename": "/runtime/ftplugin/nickel.vim", "start": 8502190, "end": 8502450}, {"filename": "/runtime/ftplugin/nim.vim", "start": 8502450, "end": 8502732}, {"filename": "/runtime/ftplugin/nix.vim", "start": 8502732, "end": 8503311}, {"filename": "/runtime/ftplugin/nroff.vim", "start": 8503311, "end": 8504884}, {"filename": "/runtime/ftplugin/nsis.vim", "start": 8504884, "end": 8506224}, {"filename": "/runtime/ftplugin/nu.vim", "start": 8506224, "end": 8506622}, {"filename": "/runtime/ftplugin/nvim-pack.lua", "start": 8506622, "end": 8509333}, {"filename": "/runtime/ftplugin/objc.lua", "start": 8509333, "end": 8509443}, {"filename": "/runtime/ftplugin/objc.vim", "start": 8509443, "end": 8509825}, {"filename": "/runtime/ftplugin/objcpp.vim", "start": 8509825, "end": 8509860}, {"filename": "/runtime/ftplugin/objdump.vim", "start": 8509860, "end": 8510131}, {"filename": "/runtime/ftplugin/obse.vim", "start": 8510131, "end": 8511854}, {"filename": "/runtime/ftplugin/ocaml.vim", "start": 8511854, "end": 8535744}, {"filename": "/runtime/ftplugin/occam.vim", "start": 8535744, "end": 8537590}, {"filename": "/runtime/ftplugin/octave.vim", "start": 8537590, "end": 8539890}, {"filename": "/runtime/ftplugin/odin.vim", "start": 8539890, "end": 8540543}, {"filename": "/runtime/ftplugin/ondir.vim", "start": 8540543, "end": 8540921}, {"filename": "/runtime/ftplugin/opencl.vim", "start": 8540921, "end": 8541283}, {"filename": "/runtime/ftplugin/openscad.vim", "start": 8541283, "end": 8541626}, {"filename": "/runtime/ftplugin/openvpn.vim", "start": 8541626, "end": 8542005}, {"filename": "/runtime/ftplugin/org.vim", "start": 8542005, "end": 8543073}, {"filename": "/runtime/ftplugin/pamconf.vim", "start": 8543073, "end": 8543506}, {"filename": "/runtime/ftplugin/pascal.vim", "start": 8543506, "end": 8545224}, {"filename": "/runtime/ftplugin/passwd.vim", "start": 8545224, "end": 8545648}, {"filename": "/runtime/ftplugin/pbtxt.vim", "start": 8545648, "end": 8546126}, {"filename": "/runtime/ftplugin/pdf.vim", "start": 8546126, "end": 8548811}, {"filename": "/runtime/ftplugin/perl.vim", "start": 8548811, "end": 8552941}, {"filename": "/runtime/ftplugin/php.vim", "start": 8552941, "end": 8558520}, {"filename": "/runtime/ftplugin/pinfo.vim", "start": 8558520, "end": 8558955}, {"filename": "/runtime/ftplugin/pkl.vim", "start": 8558955, "end": 8559356}, {"filename": "/runtime/ftplugin/plaintex.vim", "start": 8559356, "end": 8560499}, {"filename": "/runtime/ftplugin/plsql.vim", "start": 8560499, "end": 8561132}, {"filename": "/runtime/ftplugin/pod.vim", "start": 8561132, "end": 8563616}, {"filename": "/runtime/ftplugin/poefilter.vim", "start": 8563616, "end": 8563899}, {"filename": "/runtime/ftplugin/poke.vim", "start": 8563899, "end": 8564752}, {"filename": "/runtime/ftplugin/postscr.vim", "start": 8564752, "end": 8566032}, {"filename": "/runtime/ftplugin/pq.vim", "start": 8566032, "end": 8566362}, {"filename": "/runtime/ftplugin/prisma.vim", "start": 8566362, "end": 8566629}, {"filename": "/runtime/ftplugin/procmail.vim", "start": 8566629, "end": 8567108}, {"filename": "/runtime/ftplugin/prolog.vim", "start": 8567108, "end": 8567548}, {"filename": "/runtime/ftplugin/proto.vim", "start": 8567548, "end": 8567944}, {"filename": "/runtime/ftplugin/protocols.vim", "start": 8567944, "end": 8568401}, {"filename": "/runtime/ftplugin/ps1.vim", "start": 8568401, "end": 8571332}, {"filename": "/runtime/ftplugin/ps1xml.vim", "start": 8571332, "end": 8572630}, {"filename": "/runtime/ftplugin/ptx.vim", "start": 8572630, "end": 8573078}, {"filename": "/runtime/ftplugin/purescript.vim", "start": 8573078, "end": 8573376}, {"filename": "/runtime/ftplugin/pymanifest.vim", "start": 8573376, "end": 8573653}, {"filename": "/runtime/ftplugin/pyrex.vim", "start": 8573653, "end": 8574604}, {"filename": "/runtime/ftplugin/python.vim", "start": 8574604, "end": 8583015}, {"filename": "/runtime/ftplugin/qb64.vim", "start": 8583015, "end": 8583610}, {"filename": "/runtime/ftplugin/qf.vim", "start": 8583610, "end": 8584118}, {"filename": "/runtime/ftplugin/qml.vim", "start": 8584118, "end": 8585189}, {"filename": "/runtime/ftplugin/quake.vim", "start": 8585189, "end": 8585628}, {"filename": "/runtime/ftplugin/quarto.vim", "start": 8585628, "end": 8585697}, {"filename": "/runtime/ftplugin/query.lua", "start": 8585697, "end": 8586735}, {"filename": "/runtime/ftplugin/r.vim", "start": 8586735, "end": 8587806}, {"filename": "/runtime/ftplugin/racc.vim", "start": 8587806, "end": 8588255}, {"filename": "/runtime/ftplugin/racket.vim", "start": 8588255, "end": 8591168}, {"filename": "/runtime/ftplugin/radvd.vim", "start": 8591168, "end": 8591471}, {"filename": "/runtime/ftplugin/raku.vim", "start": 8591471, "end": 8595672}, {"filename": "/runtime/ftplugin/rasi.vim", "start": 8595672, "end": 8596141}, {"filename": "/runtime/ftplugin/readline.vim", "start": 8596141, "end": 8597788}, {"filename": "/runtime/ftplugin/registry.vim", "start": 8597788, "end": 8598732}, {"filename": "/runtime/ftplugin/remind.vim", "start": 8598732, "end": 8599123}, {"filename": "/runtime/ftplugin/requirements.vim", "start": 8599123, "end": 8600873}, {"filename": "/runtime/ftplugin/rescript.vim", "start": 8600873, "end": 8601178}, {"filename": "/runtime/ftplugin/reva.vim", "start": 8601178, "end": 8601983}, {"filename": "/runtime/ftplugin/rhelp.vim", "start": 8601983, "end": 8602935}, {"filename": "/runtime/ftplugin/rmd.vim", "start": 8602935, "end": 8605552}, {"filename": "/runtime/ftplugin/rnc.vim", "start": 8605552, "end": 8605983}, {"filename": "/runtime/ftplugin/rnoweb.vim", "start": 8605983, "end": 8607783}, {"filename": "/runtime/ftplugin/robot.vim", "start": 8607783, "end": 8608189}, {"filename": "/runtime/ftplugin/roc.vim", "start": 8608189, "end": 8608489}, {"filename": "/runtime/ftplugin/routeros.vim", "start": 8608489, "end": 8609309}, {"filename": "/runtime/ftplugin/rpl.vim", "start": 8609309, "end": 8609975}, {"filename": "/runtime/ftplugin/rrst.vim", "start": 8609975, "end": 8611833}, {"filename": "/runtime/ftplugin/rst.vim", "start": 8611833, "end": 8613508}, {"filename": "/runtime/ftplugin/ruby.vim", "start": 8613508, "end": 8631606}, {"filename": "/runtime/ftplugin/rust.vim", "start": 8631606, "end": 8641114}, {"filename": "/runtime/ftplugin/samba.vim", "start": 8641114, "end": 8641459}, {"filename": "/runtime/ftplugin/sass.vim", "start": 8641459, "end": 8642683}, {"filename": "/runtime/ftplugin/sbt.vim", "start": 8642683, "end": 8643037}, {"filename": "/runtime/ftplugin/scala.vim", "start": 8643037, "end": 8644510}, {"filename": "/runtime/ftplugin/scdoc.vim", "start": 8644510, "end": 8645123}, {"filename": "/runtime/ftplugin/scheme.vim", "start": 8645123, "end": 8646805}, {"filename": "/runtime/ftplugin/screen.vim", "start": 8646805, "end": 8647241}, {"filename": "/runtime/ftplugin/scss.vim", "start": 8647241, "end": 8647488}, {"filename": "/runtime/ftplugin/sed.vim", "start": 8647488, "end": 8648243}, {"filename": "/runtime/ftplugin/sensors.vim", "start": 8648243, "end": 8648698}, {"filename": "/runtime/ftplugin/services.vim", "start": 8648698, "end": 8649150}, {"filename": "/runtime/ftplugin/setserial.vim", "start": 8649150, "end": 8649589}, {"filename": "/runtime/ftplugin/sexplib.vim", "start": 8649589, "end": 8650040}, {"filename": "/runtime/ftplugin/sgml.vim", "start": 8650040, "end": 8651413}, {"filename": "/runtime/ftplugin/sh.vim", "start": 8651413, "end": 8654270}, {"filename": "/runtime/ftplugin/shada.vim", "start": 8654270, "end": 8654684}, {"filename": "/runtime/ftplugin/shaderslang.vim", "start": 8654684, "end": 8656271}, {"filename": "/runtime/ftplugin/sieve.vim", "start": 8656271, "end": 8657080}, {"filename": "/runtime/ftplugin/skhd.vim", "start": 8657080, "end": 8657530}, {"filename": "/runtime/ftplugin/slint.vim", "start": 8657530, "end": 8657928}, {"filename": "/runtime/ftplugin/slpconf.vim", "start": 8657928, "end": 8658405}, {"filename": "/runtime/ftplugin/slpreg.vim", "start": 8658405, "end": 8658881}, {"filename": "/runtime/ftplugin/slpspi.vim", "start": 8658881, "end": 8659348}, {"filename": "/runtime/ftplugin/sml.vim", "start": 8659348, "end": 8660499}, {"filename": "/runtime/ftplugin/snakemake.vim", "start": 8660499, "end": 8660762}, {"filename": "/runtime/ftplugin/solidity.vim", "start": 8660762, "end": 8661156}, {"filename": "/runtime/ftplugin/solution.vim", "start": 8661156, "end": 8662215}, {"filename": "/runtime/ftplugin/soy.vim", "start": 8662215, "end": 8662567}, {"filename": "/runtime/ftplugin/spajson.vim", "start": 8662567, "end": 8662877}, {"filename": "/runtime/ftplugin/spec.vim", "start": 8662877, "end": 8668700}, {"filename": "/runtime/ftplugin/sql.vim", "start": 8668700, "end": 8690324}, {"filename": "/runtime/ftplugin/squirrel.vim", "start": 8690324, "end": 8690739}, {"filename": "/runtime/ftplugin/ssa.vim", "start": 8690739, "end": 8691027}, {"filename": "/runtime/ftplugin/sshconfig.vim", "start": 8691027, "end": 8691968}, {"filename": "/runtime/ftplugin/sshdconfig.vim", "start": 8691968, "end": 8692216}, {"filename": "/runtime/ftplugin/stata.lua", "start": 8692216, "end": 8692326}, {"filename": "/runtime/ftplugin/stylus.vim", "start": 8692326, "end": 8693738}, {"filename": "/runtime/ftplugin/sudoers.vim", "start": 8693738, "end": 8694647}, {"filename": "/runtime/ftplugin/supercollider.lua", "start": 8694647, "end": 8694757}, {"filename": "/runtime/ftplugin/svelte.vim", "start": 8694757, "end": 8695976}, {"filename": "/runtime/ftplugin/svg.vim", "start": 8695976, "end": 8697339}, {"filename": "/runtime/ftplugin/sway.vim", "start": 8697339, "end": 8697665}, {"filename": "/runtime/ftplugin/swayconfig.vim", "start": 8697665, "end": 8698002}, {"filename": "/runtime/ftplugin/swift.lua", "start": 8698002, "end": 8698112}, {"filename": "/runtime/ftplugin/swift.vim", "start": 8698112, "end": 8699004}, {"filename": "/runtime/ftplugin/swiftgyb.vim", "start": 8699004, "end": 8699433}, {"filename": "/runtime/ftplugin/swig.vim", "start": 8699433, "end": 8699938}, {"filename": "/runtime/ftplugin/sysctl.vim", "start": 8699938, "end": 8700391}, {"filename": "/runtime/ftplugin/sysml.vim", "start": 8700391, "end": 8700731}, {"filename": "/runtime/ftplugin/systemd.vim", "start": 8700731, "end": 8702033}, {"filename": "/runtime/ftplugin/systemverilog.vim", "start": 8702033, "end": 8703395}, {"filename": "/runtime/ftplugin/tap.vim", "start": 8703395, "end": 8704242}, {"filename": "/runtime/ftplugin/tcl.vim", "start": 8704242, "end": 8705543}, {"filename": "/runtime/ftplugin/tcsh.vim", "start": 8705543, "end": 8706717}, {"filename": "/runtime/ftplugin/tera.vim", "start": 8706717, "end": 8708037}, {"filename": "/runtime/ftplugin/terminfo.vim", "start": 8708037, "end": 8708467}, {"filename": "/runtime/ftplugin/terraform.vim", "start": 8708467, "end": 8708772}, {"filename": "/runtime/ftplugin/tex.vim", "start": 8708772, "end": 8710407}, {"filename": "/runtime/ftplugin/text.vim", "start": 8710407, "end": 8710905}, {"filename": "/runtime/ftplugin/tf.vim", "start": 8710905, "end": 8711176}, {"filename": "/runtime/ftplugin/thrift.vim", "start": 8711176, "end": 8711718}, {"filename": "/runtime/ftplugin/tiasm.vim", "start": 8711718, "end": 8712511}, {"filename": "/runtime/ftplugin/tidy.vim", "start": 8712511, "end": 8713481}, {"filename": "/runtime/ftplugin/tmux.vim", "start": 8713481, "end": 8713853}, {"filename": "/runtime/ftplugin/tolk.vim", "start": 8713853, "end": 8714512}, {"filename": "/runtime/ftplugin/toml.vim", "start": 8714512, "end": 8715012}, {"filename": "/runtime/ftplugin/treetop.vim", "start": 8715012, "end": 8715428}, {"filename": "/runtime/ftplugin/tt2html.vim", "start": 8715428, "end": 8715993}, {"filename": "/runtime/ftplugin/tutor.vim", "start": 8715993, "end": 8717007}, {"filename": "/runtime/ftplugin/twig.vim", "start": 8717007, "end": 8717305}, {"filename": "/runtime/ftplugin/typescript.vim", "start": 8717305, "end": 8718709}, {"filename": "/runtime/ftplugin/typescriptreact.vim", "start": 8718709, "end": 8719504}, {"filename": "/runtime/ftplugin/typst.vim", "start": 8719504, "end": 8721149}, {"filename": "/runtime/ftplugin/uc.vim", "start": 8721149, "end": 8721463}, {"filename": "/runtime/ftplugin/uci.vim", "start": 8721463, "end": 8721987}, {"filename": "/runtime/ftplugin/udevconf.vim", "start": 8721987, "end": 8722421}, {"filename": "/runtime/ftplugin/udevperm.vim", "start": 8722421, "end": 8722853}, {"filename": "/runtime/ftplugin/udevrules.vim", "start": 8722853, "end": 8723793}, {"filename": "/runtime/ftplugin/unison.vim", "start": 8723793, "end": 8724107}, {"filename": "/runtime/ftplugin/updatedb.vim", "start": 8724107, "end": 8724550}, {"filename": "/runtime/ftplugin/urlshortcut.vim", "start": 8724550, "end": 8725025}, {"filename": "/runtime/ftplugin/usd.vim", "start": 8725025, "end": 8725404}, {"filename": "/runtime/ftplugin/v.vim", "start": 8725404, "end": 8725798}, {"filename": "/runtime/ftplugin/vb.vim", "start": 8725798, "end": 8729361}, {"filename": "/runtime/ftplugin/vdf.vim", "start": 8729361, "end": 8729672}, {"filename": "/runtime/ftplugin/verilog.vim", "start": 8729672, "end": 8731923}, {"filename": "/runtime/ftplugin/vhdl.vim", "start": 8731923, "end": 8736314}, {"filename": "/runtime/ftplugin/vim.vim", "start": 8736314, "end": 8743350}, {"filename": "/runtime/ftplugin/vroom.vim", "start": 8743350, "end": 8744274}, {"filename": "/runtime/ftplugin/vue.vim", "start": 8744274, "end": 8745220}, {"filename": "/runtime/ftplugin/wat.vim", "start": 8745220, "end": 8745777}, {"filename": "/runtime/ftplugin/wget.vim", "start": 8745777, "end": 8746604}, {"filename": "/runtime/ftplugin/wget2.vim", "start": 8746604, "end": 8747439}, {"filename": "/runtime/ftplugin/wks.vim", "start": 8747439, "end": 8747779}, {"filename": "/runtime/ftplugin/xcompose.vim", "start": 8747779, "end": 8748055}, {"filename": "/runtime/ftplugin/xdefaults.vim", "start": 8748055, "end": 8748595}, {"filename": "/runtime/ftplugin/xf86conf.vim", "start": 8748595, "end": 8749029}, {"filename": "/runtime/ftplugin/xhtml.vim", "start": 8749029, "end": 8751249}, {"filename": "/runtime/ftplugin/xinetd.vim", "start": 8751249, "end": 8751725}, {"filename": "/runtime/ftplugin/xkb.vim", "start": 8751725, "end": 8752028}, {"filename": "/runtime/ftplugin/xml.vim", "start": 8752028, "end": 8754273}, {"filename": "/runtime/ftplugin/xmodmap.vim", "start": 8754273, "end": 8754707}, {"filename": "/runtime/ftplugin/xs.lua", "start": 8754707, "end": 8754817}, {"filename": "/runtime/ftplugin/xs.vim", "start": 8754817, "end": 8755386}, {"filename": "/runtime/ftplugin/xsd.vim", "start": 8755386, "end": 8756721}, {"filename": "/runtime/ftplugin/xslt.vim", "start": 8756721, "end": 8757257}, {"filename": "/runtime/ftplugin/yacc.vim", "start": 8757257, "end": 8757667}, {"filename": "/runtime/ftplugin/yaml.vim", "start": 8757667, "end": 8758870}, {"filename": "/runtime/ftplugin/yara.vim", "start": 8758870, "end": 8759519}, {"filename": "/runtime/ftplugin/zathurarc.vim", "start": 8759519, "end": 8760133}, {"filename": "/runtime/ftplugin/zig.vim", "start": 8760133, "end": 8761657}, {"filename": "/runtime/ftplugin/zimbu.vim", "start": 8761657, "end": 8769270}, {"filename": "/runtime/ftplugin/zsh.vim", "start": 8769270, "end": 8770827}, {"filename": "/runtime/ftplugof.vim", "start": 8770827, "end": 8771230}, {"filename": "/runtime/gen_runtime.zig", "start": 8771230, "end": 8772751}, {"filename": "/runtime/indent.vim", "start": 8772751, "end": 8773663}, {"filename": "/runtime/indent/README.txt", "start": 8773663, "end": 8775397}, {"filename": "/runtime/indent/aap.vim", "start": 8775397, "end": 8775728}, {"filename": "/runtime/indent/ada.vim", "start": 8775728, "end": 8787052}, {"filename": "/runtime/indent/ant.vim", "start": 8787052, "end": 8787342}, {"filename": "/runtime/indent/arduino.vim", "start": 8787342, "end": 8787698}, {"filename": "/runtime/indent/ascendc.vim", "start": 8787698, "end": 8788015}, {"filename": "/runtime/indent/astro.vim", "start": 8788015, "end": 8790449}, {"filename": "/runtime/indent/automake.vim", "start": 8790449, "end": 8790692}, {"filename": "/runtime/indent/awk.vim", "start": 8790692, "end": 8798555}, {"filename": "/runtime/indent/bash.vim", "start": 8798555, "end": 8798945}, {"filename": "/runtime/indent/basic.vim", "start": 8798945, "end": 8799195}, {"filename": "/runtime/indent/bats.vim", "start": 8799195, "end": 8799590}, {"filename": "/runtime/indent/beancount.vim", "start": 8799590, "end": 8801074}, {"filename": "/runtime/indent/bib.vim", "start": 8801074, "end": 8801420}, {"filename": "/runtime/indent/bitbake.vim", "start": 8801420, "end": 8802003}, {"filename": "/runtime/indent/bpftrace.vim", "start": 8802003, "end": 8802492}, {"filename": "/runtime/indent/bst.vim", "start": 8802492, "end": 8804399}, {"filename": "/runtime/indent/bzl.vim", "start": 8804399, "end": 8807324}, {"filename": "/runtime/indent/c.vim", "start": 8807324, "end": 8807715}, {"filename": "/runtime/indent/cabal.vim", "start": 8807715, "end": 8810245}, {"filename": "/runtime/indent/cdl.vim", "start": 8810245, "end": 8814651}, {"filename": "/runtime/indent/ch.vim", "start": 8814651, "end": 8815207}, {"filename": "/runtime/indent/chaiscript.vim", "start": 8815207, "end": 8816411}, {"filename": "/runtime/indent/changelog.vim", "start": 8816411, "end": 8816675}, {"filename": "/runtime/indent/chatito.vim", "start": 8816675, "end": 8817406}, {"filename": "/runtime/indent/clojure.vim", "start": 8817406, "end": 8828963}, {"filename": "/runtime/indent/cmake.vim", "start": 8828963, "end": 8832012}, {"filename": "/runtime/indent/cobol.vim", "start": 8832012, "end": 8840844}, {"filename": "/runtime/indent/config.vim", "start": 8840844, "end": 8843066}, {"filename": "/runtime/indent/context.vim", "start": 8843066, "end": 8843818}, {"filename": "/runtime/indent/cpp.vim", "start": 8843818, "end": 8844213}, {"filename": "/runtime/indent/cs.vim", "start": 8844213, "end": 8846141}, {"filename": "/runtime/indent/css.vim", "start": 8846141, "end": 8847954}, {"filename": "/runtime/indent/cucumber.vim", "start": 8847954, "end": 8850914}, {"filename": "/runtime/indent/cuda.vim", "start": 8850914, "end": 8851285}, {"filename": "/runtime/indent/d.vim", "start": 8851285, "end": 8851890}, {"filename": "/runtime/indent/dictconf.vim", "start": 8851890, "end": 8852301}, {"filename": "/runtime/indent/dictdconf.vim", "start": 8852301, "end": 8852715}, {"filename": "/runtime/indent/docbk.vim", "start": 8852715, "end": 8853051}, {"filename": "/runtime/indent/dosbatch.vim", "start": 8853051, "end": 8854433}, {"filename": "/runtime/indent/dtd.vim", "start": 8854433, "end": 8866505}, {"filename": "/runtime/indent/dtrace.vim", "start": 8866505, "end": 8866956}, {"filename": "/runtime/indent/dts.vim", "start": 8866956, "end": 8868691}, {"filename": "/runtime/indent/dune.vim", "start": 8868691, "end": 8869121}, {"filename": "/runtime/indent/dylan.vim", "start": 8869121, "end": 8871932}, {"filename": "/runtime/indent/eiffel.vim", "start": 8871932, "end": 8875249}, {"filename": "/runtime/indent/elm.vim", "start": 8875249, "end": 8878526}, {"filename": "/runtime/indent/erlang.vim", "start": 8878526, "end": 8930481}, {"filename": "/runtime/indent/eruby.vim", "start": 8930481, "end": 8933439}, {"filename": "/runtime/indent/eterm.vim", "start": 8933439, "end": 8934182}, {"filename": "/runtime/indent/expect.vim", "start": 8934182, "end": 8934389}, {"filename": "/runtime/indent/falcon.vim", "start": 8934389, "end": 8948560}, {"filename": "/runtime/indent/fennel.vim", "start": 8948560, "end": 8948833}, {"filename": "/runtime/indent/fish.vim", "start": 8948833, "end": 8951552}, {"filename": "/runtime/indent/fortran.vim", "start": 8951552, "end": 8959458}, {"filename": "/runtime/indent/framescript.vim", "start": 8959458, "end": 8960349}, {"filename": "/runtime/indent/freebasic.vim", "start": 8960349, "end": 8960586}, {"filename": "/runtime/indent/gdscript.vim", "start": 8960586, "end": 8965136}, {"filename": "/runtime/indent/gitconfig.vim", "start": 8965136, "end": 8965977}, {"filename": "/runtime/indent/gitolite.vim", "start": 8965977, "end": 8967298}, {"filename": "/runtime/indent/glsl.vim", "start": 8967298, "end": 8967618}, {"filename": "/runtime/indent/go.vim", "start": 8967618, "end": 8969385}, {"filename": "/runtime/indent/graphql.vim", "start": 8969385, "end": 8972286}, {"filename": "/runtime/indent/gyp.vim", "start": 8972286, "end": 8972455}, {"filename": "/runtime/indent/haml.vim", "start": 8972455, "end": 8974702}, {"filename": "/runtime/indent/hamster.vim", "start": 8974702, "end": 8976394}, {"filename": "/runtime/indent/handlebars.vim", "start": 8976394, "end": 8980358}, {"filename": "/runtime/indent/hare.vim", "start": 8980358, "end": 8984896}, {"filename": "/runtime/indent/hcl.vim", "start": 8984896, "end": 8986121}, {"filename": "/runtime/indent/hip.vim", "start": 8986121, "end": 8986490}, {"filename": "/runtime/indent/hlsl.vim", "start": 8986490, "end": 8986794}, {"filename": "/runtime/indent/hog.vim", "start": 8986794, "end": 8988686}, {"filename": "/runtime/indent/html.vim", "start": 8988686, "end": 9022778}, {"filename": "/runtime/indent/htmldjango.vim", "start": 9022778, "end": 9023051}, {"filename": "/runtime/indent/idlang.vim", "start": 9023051, "end": 9024774}, {"filename": "/runtime/indent/idris2.vim", "start": 9024774, "end": 9029304}, {"filename": "/runtime/indent/ishd.vim", "start": 9029304, "end": 9031181}, {"filename": "/runtime/indent/j.vim", "start": 9031181, "end": 9032993}, {"filename": "/runtime/indent/java.vim", "start": 9032993, "end": 9037280}, {"filename": "/runtime/indent/javascript.vim", "start": 9037280, "end": 9052844}, {"filename": "/runtime/indent/javascriptreact.vim", "start": 9052844, "end": 9052953}, {"filename": "/runtime/indent/json.vim", "start": 9052953, "end": 9057597}, {"filename": "/runtime/indent/json5.vim", "start": 9057597, "end": 9057828}, {"filename": "/runtime/indent/jsonc.vim", "start": 9057828, "end": 9062685}, {"filename": "/runtime/indent/jsonld.vim", "start": 9062685, "end": 9062835}, {"filename": "/runtime/indent/jsp.vim", "start": 9062835, "end": 9063297}, {"filename": "/runtime/indent/julia.vim", "start": 9063297, "end": 9079128}, {"filename": "/runtime/indent/just.vim", "start": 9079128, "end": 9080509}, {"filename": "/runtime/indent/karel.vim", "start": 9080509, "end": 9084522}, {"filename": "/runtime/indent/kdl.vim", "start": 9084522, "end": 9085341}, {"filename": "/runtime/indent/kotlin.vim", "start": 9085341, "end": 9086907}, {"filename": "/runtime/indent/krl.vim", "start": 9086907, "end": 9091263}, {"filename": "/runtime/indent/ld.vim", "start": 9091263, "end": 9093130}, {"filename": "/runtime/indent/less.vim", "start": 9093130, "end": 9093373}, {"filename": "/runtime/indent/lf.vim", "start": 9093373, "end": 9093700}, {"filename": "/runtime/indent/lifelines.vim", "start": 9093700, "end": 9094338}, {"filename": "/runtime/indent/liquid.vim", "start": 9094338, "end": 9096415}, {"filename": "/runtime/indent/lisp.vim", "start": 9096415, "end": 9096764}, {"filename": "/runtime/indent/livebook.vim", "start": 9096764, "end": 9096970}, {"filename": "/runtime/indent/logtalk.vim", "start": 9096970, "end": 9098929}, {"filename": "/runtime/indent/lua.vim", "start": 9098929, "end": 9101299}, {"filename": "/runtime/indent/luau.vim", "start": 9101299, "end": 9105942}, {"filename": "/runtime/indent/m17ndb.vim", "start": 9105942, "end": 9106231}, {"filename": "/runtime/indent/mail.vim", "start": 9106231, "end": 9106616}, {"filename": "/runtime/indent/make.vim", "start": 9106616, "end": 9110511}, {"filename": "/runtime/indent/matlab.vim", "start": 9110511, "end": 9115434}, {"filename": "/runtime/indent/meson.vim", "start": 9115434, "end": 9120753}, {"filename": "/runtime/indent/mf.vim", "start": 9120753, "end": 9120909}, {"filename": "/runtime/indent/mma.vim", "start": 9120909, "end": 9123276}, {"filename": "/runtime/indent/mojo.vim", "start": 9123276, "end": 9123413}, {"filename": "/runtime/indent/mp.vim", "start": 9123413, "end": 9134734}, {"filename": "/runtime/indent/nginx.vim", "start": 9134734, "end": 9136471}, {"filename": "/runtime/indent/nsis.vim", "start": 9136471, "end": 9139777}, {"filename": "/runtime/indent/nu.vim", "start": 9139777, "end": 9141088}, {"filename": "/runtime/indent/objc.vim", "start": 9141088, "end": 9142776}, {"filename": "/runtime/indent/obse.vim", "start": 9142776, "end": 9144213}, {"filename": "/runtime/indent/ocaml.vim", "start": 9144213, "end": 9153500}, {"filename": "/runtime/indent/occam.vim", "start": 9153500, "end": 9158238}, {"filename": "/runtime/indent/odin.vim", "start": 9158238, "end": 9162045}, {"filename": "/runtime/indent/pascal.vim", "start": 9162045, "end": 9167838}, {"filename": "/runtime/indent/perl.vim", "start": 9167838, "end": 9173921}, {"filename": "/runtime/indent/php.vim", "start": 9173921, "end": 9200343}, {"filename": "/runtime/indent/pkl.vim", "start": 9200343, "end": 9208153}, {"filename": "/runtime/indent/postscr.vim", "start": 9208153, "end": 9209832}, {"filename": "/runtime/indent/pov.vim", "start": 9209832, "end": 9212602}, {"filename": "/runtime/indent/prolog.vim", "start": 9212602, "end": 9214556}, {"filename": "/runtime/indent/proto.vim", "start": 9214556, "end": 9214961}, {"filename": "/runtime/indent/ps1.vim", "start": 9214961, "end": 9215371}, {"filename": "/runtime/indent/pyrex.vim", "start": 9215371, "end": 9215697}, {"filename": "/runtime/indent/python.vim", "start": 9215697, "end": 9216583}, {"filename": "/runtime/indent/qb64.vim", "start": 9216583, "end": 9216815}, {"filename": "/runtime/indent/qml.vim", "start": 9216815, "end": 9218151}, {"filename": "/runtime/indent/quarto.vim", "start": 9218151, "end": 9218174}, {"filename": "/runtime/indent/query.lua", "start": 9218174, "end": 9218281}, {"filename": "/runtime/indent/r.vim", "start": 9218281, "end": 9232706}, {"filename": "/runtime/indent/racket.vim", "start": 9232706, "end": 9236136}, {"filename": "/runtime/indent/raku.vim", "start": 9236136, "end": 9239698}, {"filename": "/runtime/indent/raml.vim", "start": 9239698, "end": 9239983}, {"filename": "/runtime/indent/rapid.vim", "start": 9239983, "end": 9248149}, {"filename": "/runtime/indent/readline.vim", "start": 9248149, "end": 9248935}, {"filename": "/runtime/indent/rhelp.vim", "start": 9248935, "end": 9251960}, {"filename": "/runtime/indent/rmd.vim", "start": 9251960, "end": 9254392}, {"filename": "/runtime/indent/rnoweb.vim", "start": 9254392, "end": 9255644}, {"filename": "/runtime/indent/rpl.vim", "start": 9255644, "end": 9257507}, {"filename": "/runtime/indent/rrst.vim", "start": 9257507, "end": 9258859}, {"filename": "/runtime/indent/rst.vim", "start": 9258859, "end": 9260996}, {"filename": "/runtime/indent/ruby.vim", "start": 9260996, "end": 9292170}, {"filename": "/runtime/indent/rust.vim", "start": 9292170, "end": 9302576}, {"filename": "/runtime/indent/sas.vim", "start": 9302576, "end": 9307885}, {"filename": "/runtime/indent/sass.vim", "start": 9307885, "end": 9308811}, {"filename": "/runtime/indent/scala.vim", "start": 9308811, "end": 9328576}, {"filename": "/runtime/indent/scheme.vim", "start": 9328576, "end": 9329055}, {"filename": "/runtime/indent/scss.vim", "start": 9329055, "end": 9329246}, {"filename": "/runtime/indent/sdl.vim", "start": 9329246, "end": 9332075}, {"filename": "/runtime/indent/sh.vim", "start": 9332075, "end": 9342979}, {"filename": "/runtime/indent/sml.vim", "start": 9342979, "end": 9349577}, {"filename": "/runtime/indent/solidity.vim", "start": 9349577, "end": 9362490}, {"filename": "/runtime/indent/spajson.vim", "start": 9362490, "end": 9362636}, {"filename": "/runtime/indent/sql.vim", "start": 9362636, "end": 9363840}, {"filename": "/runtime/indent/sqlanywhere.vim", "start": 9363840, "end": 9377109}, {"filename": "/runtime/indent/sshconfig.vim", "start": 9377109, "end": 9377905}, {"filename": "/runtime/indent/stylus.vim", "start": 9377905, "end": 9381066}, {"filename": "/runtime/indent/systemverilog.vim", "start": 9381066, "end": 9391870}, {"filename": "/runtime/indent/tcl.vim", "start": 9391870, "end": 9394412}, {"filename": "/runtime/indent/tcsh.vim", "start": 9394412, "end": 9395793}, {"filename": "/runtime/indent/teraterm.vim", "start": 9395793, "end": 9397206}, {"filename": "/runtime/indent/terraform.vim", "start": 9397206, "end": 9397429}, {"filename": "/runtime/indent/tex.vim", "start": 9397429, "end": 9411277}, {"filename": "/runtime/indent/tf.vim", "start": 9411277, "end": 9412881}, {"filename": "/runtime/indent/thrift.vim", "start": 9412881, "end": 9414597}, {"filename": "/runtime/indent/tilde.vim", "start": 9414597, "end": 9415733}, {"filename": "/runtime/indent/treetop.vim", "start": 9415733, "end": 9416518}, {"filename": "/runtime/indent/typescript.vim", "start": 9416518, "end": 9429604}, {"filename": "/runtime/indent/typescriptreact.vim", "start": 9429604, "end": 9429713}, {"filename": "/runtime/indent/typst.vim", "start": 9429713, "end": 9430186}, {"filename": "/runtime/indent/vb.vim", "start": 9430186, "end": 9434987}, {"filename": "/runtime/indent/verilog.vim", "start": 9434987, "end": 9443256}, {"filename": "/runtime/indent/vhdl.vim", "start": 9443256, "end": 9458034}, {"filename": "/runtime/indent/vim.vim", "start": 9458034, "end": 9464785}, {"filename": "/runtime/indent/vroom.vim", "start": 9464785, "end": 9465164}, {"filename": "/runtime/indent/vue.vim", "start": 9465164, "end": 9465549}, {"filename": "/runtime/indent/wat.vim", "start": 9465549, "end": 9466007}, {"filename": "/runtime/indent/xf86conf.vim", "start": 9466007, "end": 9466793}, {"filename": "/runtime/indent/xhtml.vim", "start": 9466793, "end": 9467118}, {"filename": "/runtime/indent/xinetd.vim", "start": 9467118, "end": 9468426}, {"filename": "/runtime/indent/xml.vim", "start": 9468426, "end": 9477196}, {"filename": "/runtime/indent/xsd.vim", "start": 9477196, "end": 9477449}, {"filename": "/runtime/indent/xslt.vim", "start": 9477449, "end": 9477746}, {"filename": "/runtime/indent/yacc.vim", "start": 9477746, "end": 9478604}, {"filename": "/runtime/indent/yaml.vim", "start": 9478604, "end": 9485006}, {"filename": "/runtime/indent/zig.vim", "start": 9485006, "end": 9487145}, {"filename": "/runtime/indent/zimbu.vim", "start": 9487145, "end": 9491158}, {"filename": "/runtime/indent/zsh.vim", "start": 9491158, "end": 9491569}, {"filename": "/runtime/indoff.vim", "start": 9491569, "end": 9491917}, {"filename": "/runtime/keymap/README.txt", "start": 9491917, "end": 9492872}, {"filename": "/runtime/keymap/accents.vim", "start": 9492872, "end": 9493551}, {"filename": "/runtime/keymap/arabic.vim", "start": 9493551, "end": 9493660}, {"filename": "/runtime/keymap/arabic_buckwalter.vim", "start": 9493660, "end": 9495876}, {"filename": "/runtime/keymap/arabic_utf-8.vim", "start": 9495876, "end": 9498882}, {"filename": "/runtime/keymap/armenian-eastern_utf-8.vim", "start": 9498882, "end": 9499775}, {"filename": "/runtime/keymap/armenian-western_utf-8.vim", "start": 9499775, "end": 9500668}, {"filename": "/runtime/keymap/azerbaijani.vim", "start": 9500668, "end": 9500980}, {"filename": "/runtime/keymap/belarusian-jcuken.vim", "start": 9500980, "end": 9503498}, {"filename": "/runtime/keymap/brazilian_portuguese-abnt.vim", "start": 9503498, "end": 9505882}, {"filename": "/runtime/keymap/brazilian_portuguese-compact.vim", "start": 9505882, "end": 9508314}, {"filename": "/runtime/keymap/bulgarian-bds.vim", "start": 9508314, "end": 9514815}, {"filename": "/runtime/keymap/bulgarian-phonetic.vim", "start": 9514815, "end": 9520621}, {"filename": "/runtime/keymap/canfr-win.vim", "start": 9520621, "end": 9521787}, {"filename": "/runtime/keymap/croatian.vim", "start": 9521787, "end": 9522127}, {"filename": "/runtime/keymap/croatian_cp1250.vim", "start": 9522127, "end": 9522679}, {"filename": "/runtime/keymap/croatian_iso-8859-2.vim", "start": 9522679, "end": 9523384}, {"filename": "/runtime/keymap/croatian_utf-8.vim", "start": 9523384, "end": 9523994}, {"filename": "/runtime/keymap/czech.vim", "start": 9523994, "end": 9524029}, {"filename": "/runtime/keymap/czech_utf-8.vim", "start": 9524029, "end": 9524593}, {"filename": "/runtime/keymap/dvorak.vim", "start": 9524593, "end": 9525100}, {"filename": "/runtime/keymap/esperanto.vim", "start": 9525100, "end": 9525139}, {"filename": "/runtime/keymap/esperanto_utf-8.vim", "start": 9525139, "end": 9527541}, {"filename": "/runtime/keymap/french-azerty.vim", "start": 9527541, "end": 9529817}, {"filename": "/runtime/keymap/georgian-qwerty.vim", "start": 9529817, "end": 9530255}, {"filename": "/runtime/keymap/german-qwertz.vim", "start": 9530255, "end": 9532652}, {"filename": "/runtime/keymap/greek.vim", "start": 9532652, "end": 9532886}, {"filename": "/runtime/keymap/greek_cp1253.vim", "start": 9532886, "end": 9535090}, {"filename": "/runtime/keymap/greek_cp737.vim", "start": 9535090, "end": 9537045}, {"filename": "/runtime/keymap/greek_iso-8859-7.vim", "start": 9537045, "end": 9539177}, {"filename": "/runtime/keymap/greek_utf-8.vim", "start": 9539177, "end": 9565477}, {"filename": "/runtime/keymap/hebrew.vim", "start": 9565477, "end": 9565799}, {"filename": "/runtime/keymap/hebrew_cp1255.vim", "start": 9565799, "end": 9567540}, {"filename": "/runtime/keymap/hebrew_iso-8859-8.vim", "start": 9567540, "end": 9568546}, {"filename": "/runtime/keymap/hebrew_utf-8.vim", "start": 9568546, "end": 9572362}, {"filename": "/runtime/keymap/hebrewp.vim", "start": 9572362, "end": 9572687}, {"filename": "/runtime/keymap/hebrewp_cp1255.vim", "start": 9572687, "end": 9574258}, {"filename": "/runtime/keymap/hebrewp_iso-8859-8.vim", "start": 9574258, "end": 9575055}, {"filename": "/runtime/keymap/hebrewp_utf-8.vim", "start": 9575055, "end": 9578718}, {"filename": "/runtime/keymap/kana.vim", "start": 9578718, "end": 9587733}, {"filename": "/runtime/keymap/kazakh-jcuken.vim", "start": 9587733, "end": 9590998}, {"filename": "/runtime/keymap/korean-dubeolsik_utf-8.vim", "start": 9590998, "end": 9689463}, {"filename": "/runtime/keymap/korean.vim", "start": 9689463, "end": 9690562}, {"filename": "/runtime/keymap/lithuanian-baltic.vim", "start": 9690562, "end": 9691525}, {"filename": "/runtime/keymap/magyar_utf-8.vim", "start": 9691525, "end": 9691851}, {"filename": "/runtime/keymap/mongolian_utf-8.vim", "start": 9691851, "end": 9694413}, {"filename": "/runtime/keymap/oldturkic-orkhon_utf-8.vim", "start": 9694413, "end": 9700215}, {"filename": "/runtime/keymap/oldturkic-yenisei_utf-8.vim", "start": 9700215, "end": 9706829}, {"filename": "/runtime/keymap/persian-iranian_utf-8.vim", "start": 9706829, "end": 9707581}, {"filename": "/runtime/keymap/persian.vim", "start": 9707581, "end": 9707725}, {"filename": "/runtime/keymap/pinyin.vim", "start": 9707725, "end": 9708264}, {"filename": "/runtime/keymap/polish-slash.vim", "start": 9708264, "end": 9708973}, {"filename": "/runtime/keymap/polish-slash_cp1250.vim", "start": 9708973, "end": 9710345}, {"filename": "/runtime/keymap/polish-slash_cp852.vim", "start": 9710345, "end": 9711716}, {"filename": "/runtime/keymap/polish-slash_iso-8859-2.vim", "start": 9711716, "end": 9713088}, {"filename": "/runtime/keymap/polish-slash_utf-8.vim", "start": 9713088, "end": 9714513}, {"filename": "/runtime/keymap/russian-dvorak.vim", "start": 9714513, "end": 9717107}, {"filename": "/runtime/keymap/russian-jcuken.vim", "start": 9717107, "end": 9719642}, {"filename": "/runtime/keymap/russian-jcukenmac.vim", "start": 9719642, "end": 9722455}, {"filename": "/runtime/keymap/russian-jcukenwin.vim", "start": 9722455, "end": 9725099}, {"filename": "/runtime/keymap/russian-jcukenwintype.vim", "start": 9725099, "end": 9727899}, {"filename": "/runtime/keymap/russian-typograph.vim", "start": 9727899, "end": 9745015}, {"filename": "/runtime/keymap/russian-yawerty.vim", "start": 9745015, "end": 9747478}, {"filename": "/runtime/keymap/serbian-latin.vim", "start": 9747478, "end": 9747521}, {"filename": "/runtime/keymap/serbian-latin_utf-8.vim", "start": 9747521, "end": 9748077}, {"filename": "/runtime/keymap/serbian.vim", "start": 9748077, "end": 9748544}, {"filename": "/runtime/keymap/serbian_cp1250.vim", "start": 9748544, "end": 9748995}, {"filename": "/runtime/keymap/serbian_cp1251.vim", "start": 9748995, "end": 9749649}, {"filename": "/runtime/keymap/serbian_iso-8859-2.vim", "start": 9749649, "end": 9750122}, {"filename": "/runtime/keymap/serbian_iso-8859-5.vim", "start": 9750122, "end": 9750782}, {"filename": "/runtime/keymap/serbian_utf-8.vim", "start": 9750782, "end": 9751575}, {"filename": "/runtime/keymap/sinhala-phonetic_utf-8.vim", "start": 9751575, "end": 9756248}, {"filename": "/runtime/keymap/sinhala.vim", "start": 9756248, "end": 9756338}, {"filename": "/runtime/keymap/slovak.vim", "start": 9756338, "end": 9756660}, {"filename": "/runtime/keymap/slovak_cp1250.vim", "start": 9756660, "end": 9757420}, {"filename": "/runtime/keymap/slovak_iso-8859-2.vim", "start": 9757420, "end": 9758180}, {"filename": "/runtime/keymap/slovak_utf-8.vim", "start": 9758180, "end": 9758992}, {"filename": "/runtime/keymap/tamil_tscii.vim", "start": 9758992, "end": 9773803}, {"filename": "/runtime/keymap/thaana-phonetic_utf-8.vim", "start": 9773803, "end": 9776625}, {"filename": "/runtime/keymap/thaana.vim", "start": 9776625, "end": 9776696}, {"filename": "/runtime/keymap/turkish-f.vim", "start": 9776696, "end": 9779457}, {"filename": "/runtime/keymap/turkish-q.vim", "start": 9779457, "end": 9780851}, {"filename": "/runtime/keymap/ukrainian-dvorak.vim", "start": 9780851, "end": 9783591}, {"filename": "/runtime/keymap/ukrainian-enhanced.vim", "start": 9783591, "end": 9786287}, {"filename": "/runtime/keymap/ukrainian-jcuken.vim", "start": 9786287, "end": 9789025}, {"filename": "/runtime/keymap/vietnamese-telex_utf-8.vim", "start": 9789025, "end": 9790382}, {"filename": "/runtime/keymap/vietnamese-viqr_utf-8.vim", "start": 9790382, "end": 9791629}, {"filename": "/runtime/keymap/vietnamese-vni_utf-8.vim", "start": 9791629, "end": 9792984}, {"filename": "/runtime/lua/coxpcall.lua", "start": 9792984, "end": 9797493}, {"filename": "/runtime/lua/editorconfig.lua", "start": 9797493, "end": 9809919}, {"filename": "/runtime/lua/man.lua", "start": 9809919, "end": 9836422}, {"filename": "/runtime/lua/nvim/autoread.lua", "start": 9836422, "end": 9842692}, {"filename": "/runtime/lua/nvim/dir.lua", "start": 9842692, "end": 9853877}, {"filename": "/runtime/lua/nvim/dir/fs.lua", "start": 9853877, "end": 9856901}, {"filename": "/runtime/lua/nvim/matchparen.lua", "start": 9856901, "end": 9866378}, {"filename": "/runtime/lua/nvim/spellfile.lua", "start": 9866378, "end": 9874583}, {"filename": "/runtime/lua/nvim/tutor.lua", "start": 9874583, "end": 9877919}, {"filename": "/runtime/lua/nvim/zip.lua", "start": 9877919, "end": 9896082}, {"filename": "/runtime/lua/nvim/zip/health.lua", "start": 9896082, "end": 9897687}, {"filename": "/runtime/lua/uv/_meta.lua", "start": 9897687, "end": 10078105}, {"filename": "/runtime/lua/vim/F.lua", "start": 10078105, "end": 10079995}, {"filename": "/runtime/lua/vim/_comment.lua", "start": 10079995, "end": 10089226}, {"filename": "/runtime/lua/vim/_core/cmdwin.lua", "start": 10089226, "end": 10096395}, {"filename": "/runtime/lua/vim/_core/defaults.lua", "start": 10096395, "end": 10137292}, {"filename": "/runtime/lua/vim/_core/editor.lua", "start": 10137292, "end": 10185301}, {"filename": "/runtime/lua/vim/_core/ex_cmd.lua", "start": 10185301, "end": 10196876}, {"filename": "/runtime/lua/vim/_core/exmode.lua", "start": 10196876, "end": 10205232}, {"filename": "/runtime/lua/vim/_core/exrc.lua", "start": 10205232, "end": 10205768}, {"filename": "/runtime/lua/vim/_core/help.lua", "start": 10205768, "end": 10225365}, {"filename": "/runtime/lua/vim/_core/log.lua", "start": 10225365, "end": 10226067}, {"filename": "/runtime/lua/vim/_core/marks.lua", "start": 10226067, "end": 10231581}, {"filename": "/runtime/lua/vim/_core/mcursor.lua", "start": 10231581, "end": 10241883}, {"filename": "/runtime/lua/vim/_core/options.lua", "start": 10241883, "end": 10259903}, {"filename": "/runtime/lua/vim/_core/proc.lua", "start": 10259903, "end": 10260479}, {"filename": "/runtime/lua/vim/_core/server.lua", "start": 10260479, "end": 10270679}, {"filename": "/runtime/lua/vim/_core/shared.lua", "start": 10270679, "end": 10322875}, {"filename": "/runtime/lua/vim/_core/spell.lua", "start": 10322875, "end": 10324487}, {"filename": "/runtime/lua/vim/_core/stringbuffer.lua", "start": 10324487, "end": 10327485}, {"filename": "/runtime/lua/vim/_core/swapfile.lua", "start": 10327485, "end": 10330268}, {"filename": "/runtime/lua/vim/_core/system.lua", "start": 10330268, "end": 10349573}, {"filename": "/runtime/lua/vim/_core/table.lua", "start": 10349573, "end": 10350079}, {"filename": "/runtime/lua/vim/_core/tag.lua", "start": 10350079, "end": 10352085}, {"filename": "/runtime/lua/vim/_core/time.lua", "start": 10352085, "end": 10353584}, {"filename": "/runtime/lua/vim/_core/ui.lua", "start": 10353584, "end": 10354335}, {"filename": "/runtime/lua/vim/_core/ui2.lua", "start": 10354335, "end": 10366253}, {"filename": "/runtime/lua/vim/_core/ui2/cmdline.lua", "start": 10366253, "end": 10373973}, {"filename": "/runtime/lua/vim/_core/ui2/messages.lua", "start": 10373973, "end": 10409206}, {"filename": "/runtime/lua/vim/_core/util.lua", "start": 10409206, "end": 10418543}, {"filename": "/runtime/lua/vim/_core/vimfn.lua", "start": 10418543, "end": 10419356}, {"filename": "/runtime/lua/vim/_ftplugin/lua.lua", "start": 10419356, "end": 10419909}, {"filename": "/runtime/lua/vim/_init_packages.lua", "start": 10419909, "end": 10423411}, {"filename": "/runtime/lua/vim/_inspector.lua", "start": 10423411, "end": 10437586}, {"filename": "/runtime/lua/vim/_meta.lua", "start": 10437586, "end": 10439145}, {"filename": "/runtime/lua/vim/_meta/api.gen.lua", "start": 10439145, "end": 10557206}, {"filename": "/runtime/lua/vim/_meta/api_keysets.gen.lua", "start": 10557206, "end": 10572050}, {"filename": "/runtime/lua/vim/_meta/api_keysets_extra.lua", "start": 10572050, "end": 10580367}, {"filename": "/runtime/lua/vim/_meta/base64.lua", "start": 10580367, "end": 10580783}, {"filename": "/runtime/lua/vim/_meta/builtin.lua", "start": 10580783, "end": 10591801}, {"filename": "/runtime/lua/vim/_meta/events.lua", "start": 10591801, "end": 10594627}, {"filename": "/runtime/lua/vim/_meta/json.lua", "start": 10594627, "end": 10597375}, {"filename": "/runtime/lua/vim/_meta/lpeg.lua", "start": 10597375, "end": 10613140}, {"filename": "/runtime/lua/vim/_meta/misc.lua", "start": 10613140, "end": 10613837}, {"filename": "/runtime/lua/vim/_meta/mpack.lua", "start": 10613837, "end": 10614437}, {"filename": "/runtime/lua/vim/_meta/options.gen.lua", "start": 10614437, "end": 10964675}, {"filename": "/runtime/lua/vim/_meta/re.lua", "start": 10964675, "end": 10966994}, {"filename": "/runtime/lua/vim/_meta/regex.lua", "start": 10966994, "end": 10968771}, {"filename": "/runtime/lua/vim/_meta/spell.lua", "start": 10968771, "end": 10969807}, {"filename": "/runtime/lua/vim/_meta/tui.lua", "start": 10969807, "end": 10970143}, {"filename": "/runtime/lua/vim/_meta/vimfn.gen.lua", "start": 10970143, "end": 11414248}, {"filename": "/runtime/lua/vim/_meta/vimfn_types.lua", "start": 11414248, "end": 11424834}, {"filename": "/runtime/lua/vim/_meta/vvars.gen.lua", "start": 11424834, "end": 11454541}, {"filename": "/runtime/lua/vim/_meta/vvars_extra.lua", "start": 11454541, "end": 11458844}, {"filename": "/runtime/lua/vim/_watch.lua", "start": 11458844, "end": 11478892}, {"filename": "/runtime/lua/vim/_watchdirs.lua", "start": 11478892, "end": 11485362}, {"filename": "/runtime/lua/vim/async.lua", "start": 11485362, "end": 11496126}, {"filename": "/runtime/lua/vim/async/_core.lua", "start": 11496126, "end": 11532856}, {"filename": "/runtime/lua/vim/async/_event.lua", "start": 11532856, "end": 11536149}, {"filename": "/runtime/lua/vim/async/_future.lua", "start": 11536149, "end": 11540056}, {"filename": "/runtime/lua/vim/async/_queue.lua", "start": 11540056, "end": 11542834}, {"filename": "/runtime/lua/vim/async/_runtime.lua", "start": 11542834, "end": 11544335}, {"filename": "/runtime/lua/vim/async/_semaphore.lua", "start": 11544335, "end": 11548271}, {"filename": "/runtime/lua/vim/deprecated/health.lua", "start": 11548271, "end": 11549730}, {"filename": "/runtime/lua/vim/diagnostic.lua", "start": 11549730, "end": 11589888}, {"filename": "/runtime/lua/vim/diagnostic/_config.lua", "start": 11589888, "end": 11594671}, {"filename": "/runtime/lua/vim/diagnostic/_display.lua", "start": 11594671, "end": 11601182}, {"filename": "/runtime/lua/vim/diagnostic/_float.lua", "start": 11601182, "end": 11612025}, {"filename": "/runtime/lua/vim/diagnostic/_handlers.lua", "start": 11612025, "end": 11635806}, {"filename": "/runtime/lua/vim/diagnostic/_jump.lua", "start": 11635806, "end": 11644234}, {"filename": "/runtime/lua/vim/diagnostic/_severity.lua", "start": 11644234, "end": 11647886}, {"filename": "/runtime/lua/vim/diagnostic/_shared.lua", "start": 11647886, "end": 11653027}, {"filename": "/runtime/lua/vim/diagnostic/_store.lua", "start": 11653027, "end": 11662699}, {"filename": "/runtime/lua/vim/filetype.lua", "start": 11662699, "end": 11761099}, {"filename": "/runtime/lua/vim/filetype/detect.lua", "start": 11761099, "end": 11833870}, {"filename": "/runtime/lua/vim/filetype/options.lua", "start": 11833870, "end": 11836534}, {"filename": "/runtime/lua/vim/fs.lua", "start": 11836534, "end": 11872429}, {"filename": "/runtime/lua/vim/func.lua", "start": 11872429, "end": 11874336}, {"filename": "/runtime/lua/vim/func/_memoize.lua", "start": 11874336, "end": 11876444}, {"filename": "/runtime/lua/vim/glob.lua", "start": 11876444, "end": 11889531}, {"filename": "/runtime/lua/vim/health.lua", "start": 11889531, "end": 11908297}, {"filename": "/runtime/lua/vim/health/health.lua", "start": 11908297, "end": 11936856}, {"filename": "/runtime/lua/vim/hl.lua", "start": 11936856, "end": 11944299}, {"filename": "/runtime/lua/vim/inspect.lua", "start": 11944299, "end": 11956375}, {"filename": "/runtime/lua/vim/iter.lua", "start": 11956375, "end": 11991076}, {"filename": "/runtime/lua/vim/keymap.lua", "start": 11991076, "end": 11996423}, {"filename": "/runtime/lua/vim/loader.lua", "start": 11996423, "end": 12011976}, {"filename": "/runtime/lua/vim/log.lua", "start": 12011976, "end": 12020183}, {"filename": "/runtime/lua/vim/lsp.lua", "start": 12020183, "end": 12074577}, {"filename": "/runtime/lua/vim/lsp/_capability.lua", "start": 12074577, "end": 12083645}, {"filename": "/runtime/lua/vim/lsp/_changetracking.lua", "start": 12083645, "end": 12097211}, {"filename": "/runtime/lua/vim/lsp/_folding_range.lua", "start": 12097211, "end": 12111348}, {"filename": "/runtime/lua/vim/lsp/_meta.lua", "start": 12111348, "end": 12112006}, {"filename": "/runtime/lua/vim/lsp/_meta/protocol.lua", "start": 12112006, "end": 12305608}, {"filename": "/runtime/lua/vim/lsp/_snippet_grammar.lua", "start": 12305608, "end": 12311861}, {"filename": "/runtime/lua/vim/lsp/_watchfiles.lua", "start": 12311861, "end": 12318721}, {"filename": "/runtime/lua/vim/lsp/buf.lua", "start": 12318721, "end": 12372096}, {"filename": "/runtime/lua/vim/lsp/client.lua", "start": 12372096, "end": 12428212}, {"filename": "/runtime/lua/vim/lsp/codelens.lua", "start": 12428212, "end": 12443503}, {"filename": "/runtime/lua/vim/lsp/completion.lua", "start": 12443503, "end": 12491383}, {"filename": "/runtime/lua/vim/lsp/diagnostic.lua", "start": 12491383, "end": 12508628}, {"filename": "/runtime/lua/vim/lsp/document_color.lua", "start": 12508628, "end": 12521248}, {"filename": "/runtime/lua/vim/lsp/handlers.lua", "start": 12521248, "end": 12548972}, {"filename": "/runtime/lua/vim/lsp/health.lua", "start": 12548972, "end": 12559086}, {"filename": "/runtime/lua/vim/lsp/inlay_hint.lua", "start": 12559086, "end": 12571451}, {"filename": "/runtime/lua/vim/lsp/inline_completion.lua", "start": 12571451, "end": 12587691}, {"filename": "/runtime/lua/vim/lsp/linked_editing_range.lua", "start": 12587691, "end": 12594993}, {"filename": "/runtime/lua/vim/lsp/log.lua", "start": 12594993, "end": 12598692}, {"filename": "/runtime/lua/vim/lsp/on_type_formatting.lua", "start": 12598692, "end": 12606308}, {"filename": "/runtime/lua/vim/lsp/protocol.lua", "start": 12606308, "end": 12671055}, {"filename": "/runtime/lua/vim/lsp/rpc.lua", "start": 12671055, "end": 12694307}, {"filename": "/runtime/lua/vim/lsp/semantic_tokens.lua", "start": 12694307, "end": 12729221}, {"filename": "/runtime/lua/vim/lsp/sync.lua", "start": 12729221, "end": 12745611}, {"filename": "/runtime/lua/vim/lsp/tagfunc.lua", "start": 12745611, "end": 12749420}, {"filename": "/runtime/lua/vim/lsp/util.lua", "start": 12749420, "end": 12820905}, {"filename": "/runtime/lua/vim/net.lua", "start": 12820905, "end": 12830068}, {"filename": "/runtime/lua/vim/net/_autoinstall.lua", "start": 12830068, "end": 12833970}, {"filename": "/runtime/lua/vim/net/_ssh.lua", "start": 12833970, "end": 12863531}, {"filename": "/runtime/lua/vim/net/_transport.lua", "start": 12863531, "end": 12871864}, {"filename": "/runtime/lua/vim/pack.lua", "start": 12871864, "end": 12934468}, {"filename": "/runtime/lua/vim/pack/_lsp.lua", "start": 12934468, "end": 12947068}, {"filename": "/runtime/lua/vim/pack/health.lua", "start": 12947068, "end": 12961038}, {"filename": "/runtime/lua/vim/pos.lua", "start": 12961038, "end": 12970006}, {"filename": "/runtime/lua/vim/pos/_util.lua", "start": 12970006, "end": 12975175}, {"filename": "/runtime/lua/vim/provider.lua", "start": 12975175, "end": 12975377}, {"filename": "/runtime/lua/vim/provider/health.lua", "start": 12975377, "end": 13006020}, {"filename": "/runtime/lua/vim/provider/perl.lua", "start": 13006020, "end": 13008336}, {"filename": "/runtime/lua/vim/provider/python.lua", "start": 13008336, "end": 13013016}, {"filename": "/runtime/lua/vim/provider/ruby.lua", "start": 13013016, "end": 13014931}, {"filename": "/runtime/lua/vim/range.lua", "start": 13014931, "end": 13029367}, {"filename": "/runtime/lua/vim/re.lua", "start": 13029367, "end": 13036078}, {"filename": "/runtime/lua/vim/secure.lua", "start": 13036078, "end": 13043207}, {"filename": "/runtime/lua/vim/snippet.lua", "start": 13043207, "end": 13066182}, {"filename": "/runtime/lua/vim/text.lua", "start": 13066182, "end": 13073700}, {"filename": "/runtime/lua/vim/treesitter.lua", "start": 13073700, "end": 13093458}, {"filename": "/runtime/lua/vim/treesitter/_fold.lua", "start": 13093458, "end": 13109504}, {"filename": "/runtime/lua/vim/treesitter/_headings.lua", "start": 13109504, "end": 13113606}, {"filename": "/runtime/lua/vim/treesitter/_meta/misc.lua", "start": 13113606, "end": 13116411}, {"filename": "/runtime/lua/vim/treesitter/_meta/tsnode.lua", "start": 13116411, "end": 13122401}, {"filename": "/runtime/lua/vim/treesitter/_meta/tsquery.lua", "start": 13122401, "end": 13124306}, {"filename": "/runtime/lua/vim/treesitter/_meta/tstree.lua", "start": 13124306, "end": 13125588}, {"filename": "/runtime/lua/vim/treesitter/_query_linter.lua", "start": 13125588, "end": 13133535}, {"filename": "/runtime/lua/vim/treesitter/_range.lua", "start": 13133535, "end": 13139392}, {"filename": "/runtime/lua/vim/treesitter/_select.lua", "start": 13139392, "end": 13156975}, {"filename": "/runtime/lua/vim/treesitter/dev.lua", "start": 13156975, "end": 13179208}, {"filename": "/runtime/lua/vim/treesitter/health.lua", "start": 13179208, "end": 13182510}, {"filename": "/runtime/lua/vim/treesitter/highlighter.lua", "start": 13182510, "end": 13206595}, {"filename": "/runtime/lua/vim/treesitter/language.lua", "start": 13206595, "end": 13212604}, {"filename": "/runtime/lua/vim/treesitter/languagetree.lua", "start": 13212604, "end": 13257427}, {"filename": "/runtime/lua/vim/treesitter/query.lua", "start": 13257427, "end": 13295786}, {"filename": "/runtime/lua/vim/tty.lua", "start": 13295786, "end": 13303879}, {"filename": "/runtime/lua/vim/ui.lua", "start": 13303879, "end": 13317801}, {"filename": "/runtime/lua/vim/ui/clipboard/osc52.lua", "start": 13317801, "end": 13319976}, {"filename": "/runtime/lua/vim/ui/img.lua", "start": 13319976, "end": 13324425}, {"filename": "/runtime/lua/vim/ui/img/_kitty.lua", "start": 13324425, "end": 13330350}, {"filename": "/runtime/lua/vim/uri.lua", "start": 13330350, "end": 13350848}, {"filename": "/runtime/lua/vim/version.lua", "start": 13350848, "end": 13367547}, {"filename": "/runtime/makemenu.vim", "start": 13367547, "end": 13392974}, {"filename": "/runtime/menu.vim", "start": 13392974, "end": 13435909}, {"filename": "/runtime/neovim.ico", "start": 13435909, "end": 13558264}, {"filename": "/runtime/nvim.png", "start": 13558264, "end": 13564151}, {"filename": "/runtime/org.neovim.nvim.appdata.xml", "start": 13564151, "end": 13567744}, {"filename": "/runtime/org.neovim.nvim.desktop", "start": 13567744, "end": 13571898}, {"filename": "/runtime/pack/dist/opt/cfilter/plugin/cfilter.vim", "start": 13571898, "end": 13573827}, {"filename": "/runtime/pack/dist/opt/justify/plugin/justify.vim", "start": 13573827, "end": 13582986}, {"filename": "/runtime/pack/dist/opt/matchit/autoload/matchit.vim", "start": 13582986, "end": 13614278}, {"filename": "/runtime/pack/dist/opt/matchit/doc/matchit.txt", "start": 13614278, "end": 13634702}, {"filename": "/runtime/pack/dist/opt/matchit/plugin/matchit.vim", "start": 13634702, "end": 13639807}, {"filename": "/runtime/pack/dist/opt/netrw/LICENSE.txt", "start": 13639807, "end": 13640556}, {"filename": "/runtime/pack/dist/opt/netrw/README.md", "start": 13640556, "end": 13665611}, {"filename": "/runtime/pack/dist/opt/netrw/autoload/netrw.vim", "start": 13665611, "end": 14063074}, {"filename": "/runtime/pack/dist/opt/netrw/autoload/netrw/fs.vim", "start": 14063074, "end": 14068882}, {"filename": "/runtime/pack/dist/opt/netrw/autoload/netrw/msg.vim", "start": 14068882, "end": 14071002}, {"filename": "/runtime/pack/dist/opt/netrw/autoload/netrw/os.vim", "start": 14071002, "end": 14072578}, {"filename": "/runtime/pack/dist/opt/netrw/autoload/netrw_gitignore.vim", "start": 14072578, "end": 14073985}, {"filename": "/runtime/pack/dist/opt/netrw/doc/netrw.txt", "start": 14073985, "end": 14221536}, {"filename": "/runtime/pack/dist/opt/netrw/plugin/netrwPlugin.vim", "start": 14221536, "end": 14229262}, {"filename": "/runtime/pack/dist/opt/netrw/syntax/netrw.vim", "start": 14229262, "end": 14236222}, {"filename": "/runtime/pack/dist/opt/nohlsearch/plugin/nohlsearch.vim", "start": 14236222, "end": 14236736}, {"filename": "/runtime/pack/dist/opt/nvim.difftool/lua/difftool.lua", "start": 14236736, "end": 14251047}, {"filename": "/runtime/pack/dist/opt/nvim.difftool/plugin/difftool.lua", "start": 14251047, "end": 14251968}, {"filename": "/runtime/pack/dist/opt/nvim.tohtml/lua/tohtml.lua", "start": 14251968, "end": 14296957}, {"filename": "/runtime/pack/dist/opt/nvim.tohtml/plugin/tohtml.lua", "start": 14296957, "end": 14297418}, {"filename": "/runtime/pack/dist/opt/nvim.undotree/lua/undotree.lua", "start": 14297418, "end": 14309144}, {"filename": "/runtime/pack/dist/opt/nvim.undotree/plugin/undotree.lua", "start": 14309144, "end": 14309331}, {"filename": "/runtime/pack/dist/opt/old-zip/autoload/zip.vim", "start": 14309331, "end": 14332111}, {"filename": "/runtime/pack/dist/opt/old-zip/doc/pi_zip.txt", "start": 14332111, "end": 14341387}, {"filename": "/runtime/pack/dist/opt/old-zip/plugin/zipPlugin.vim", "start": 14341387, "end": 14344282}, {"filename": "/runtime/pack/dist/opt/swapmouse/plugin/swapmouse.vim", "start": 14344282, "end": 14345138}, {"filename": "/runtime/pack/dist/opt/termdebug/plugin/termdebug.vim", "start": 14345138, "end": 14405020}, {"filename": "/runtime/plugin/autoread.lua", "start": 14405020, "end": 14405460}, {"filename": "/runtime/plugin/dir.lua", "start": 14405460, "end": 14407742}, {"filename": "/runtime/plugin/editorconfig.lua", "start": 14407742, "end": 14408147}, {"filename": "/runtime/plugin/gzip.vim", "start": 14408147, "end": 14411824}, {"filename": "/runtime/plugin/man.lua", "start": 14411824, "end": 14412989}, {"filename": "/runtime/plugin/matchit.vim", "start": 14412989, "end": 14413127}, {"filename": "/runtime/plugin/matchparen.lua", "start": 14413127, "end": 14413216}, {"filename": "/runtime/plugin/net.lua", "start": 14413216, "end": 14416772}, {"filename": "/runtime/plugin/netrwPlugin.vim", "start": 14416772, "end": 14416924}, {"filename": "/runtime/plugin/osc52.lua", "start": 14416924, "end": 14420490}, {"filename": "/runtime/plugin/rplugin.vim", "start": 14420490, "end": 14422507}, {"filename": "/runtime/plugin/shada.lua", "start": 14422507, "end": 14425051}, {"filename": "/runtime/plugin/spellfile.lua", "start": 14425051, "end": 14425406}, {"filename": "/runtime/plugin/tarPlugin.vim", "start": 14425406, "end": 14427983}, {"filename": "/runtime/plugin/tutor.vim", "start": 14427983, "end": 14428439}, {"filename": "/runtime/plugin/zip.lua", "start": 14428439, "end": 14430402}, {"filename": "/runtime/queries/c/folds.scm", "start": 14430402, "end": 14430808}, {"filename": "/runtime/queries/c/highlights.scm", "start": 14430808, "end": 14437140}, {"filename": "/runtime/queries/c/injections.scm", "start": 14437140, "end": 14437200}, {"filename": "/runtime/queries/diff/folds.scm", "start": 14437200, "end": 14437239}, {"filename": "/runtime/queries/diff/highlights.scm", "start": 14437239, "end": 14438154}, {"filename": "/runtime/queries/lua/folds.scm", "start": 14438154, "end": 14438357}, {"filename": "/runtime/queries/lua/highlights.scm", "start": 14438357, "end": 14442693}, {"filename": "/runtime/queries/lua/injections.scm", "start": 14442693, "end": 14445930}, {"filename": "/runtime/queries/markdown/folds.scm", "start": 14445930, "end": 14446085}, {"filename": "/runtime/queries/markdown/highlights.scm", "start": 14446085, "end": 14448739}, {"filename": "/runtime/queries/markdown/injections.scm", "start": 14448739, "end": 14449398}, {"filename": "/runtime/queries/markdown_inline/highlights.scm", "start": 14449398, "end": 14451657}, {"filename": "/runtime/queries/markdown_inline/injections.scm", "start": 14451657, "end": 14451864}, {"filename": "/runtime/queries/query/folds.scm", "start": 14451864, "end": 14451925}, {"filename": "/runtime/queries/query/highlights.scm", "start": 14451925, "end": 14453592}, {"filename": "/runtime/queries/vim/folds.scm", "start": 14453592, "end": 14453643}, {"filename": "/runtime/queries/vim/highlights.scm", "start": 14453643, "end": 14458174}, {"filename": "/runtime/queries/vim/injections.scm", "start": 14458174, "end": 14459038}, {"filename": "/runtime/queries/vimdoc/highlights.scm", "start": 14459038, "end": 14460531}, {"filename": "/runtime/queries/vimdoc/injections.scm", "start": 14460531, "end": 14460643}, {"filename": "/runtime/scripts/emoji_list.lua", "start": 14460643, "end": 14461049}, {"filename": "/runtime/scripts/less.bat", "start": 14461049, "end": 14461341}, {"filename": "/runtime/scripts/less.sh", "start": 14461341, "end": 14461933}, {"filename": "/runtime/scripts/less.vim", "start": 14461933, "end": 14469911}, {"filename": "/runtime/scripts/mswin.vim", "start": 14469911, "end": 14474125}, {"filename": "/runtime/scripts/optwin.lua", "start": 14474125, "end": 14508490}, {"filename": "/runtime/scripts/remote_ssh.sh", "start": 14508490, "end": 14512250}, {"filename": "/runtime/spell/cleanadd.vim", "start": 14512250, "end": 14513184}, {"filename": "/runtime/spell/en.utf-8.spl", "start": 14513184, "end": 15134801}, {"filename": "/runtime/synmenu.vim", "start": 15134801, "end": 15185585}, {"filename": "/runtime/syntax/8th.vim", "start": 15185585, "end": 15232341}, {"filename": "/runtime/syntax/README.txt", "start": 15232341, "end": 15233926}, {"filename": "/runtime/syntax/a2ps.vim", "start": 15233926, "end": 15236352}, {"filename": "/runtime/syntax/a65.vim", "start": 15236352, "end": 15242745}, {"filename": "/runtime/syntax/aap.vim", "start": 15242745, "end": 15248508}, {"filename": "/runtime/syntax/abap.vim", "start": 15248508, "end": 15257664}, {"filename": "/runtime/syntax/abaqus.vim", "start": 15257664, "end": 15258757}, {"filename": "/runtime/syntax/abc.vim", "start": 15258757, "end": 15260599}, {"filename": "/runtime/syntax/abel.vim", "start": 15260599, "end": 15265712}, {"filename": "/runtime/syntax/abnf.vim", "start": 15265712, "end": 15266664}, {"filename": "/runtime/syntax/acedb.vim", "start": 15266664, "end": 15271535}, {"filename": "/runtime/syntax/ada.vim", "start": 15271535, "end": 15284586}, {"filename": "/runtime/syntax/aflex.vim", "start": 15284586, "end": 15288954}, {"filename": "/runtime/syntax/ahdl.vim", "start": 15288954, "end": 15291724}, {"filename": "/runtime/syntax/aidl.vim", "start": 15291724, "end": 15292353}, {"filename": "/runtime/syntax/algol68.vim", "start": 15292353, "end": 15326469}, {"filename": "/runtime/syntax/alsaconf.vim", "start": 15326469, "end": 15328015}, {"filename": "/runtime/syntax/amiga.vim", "start": 15328015, "end": 15336956}, {"filename": "/runtime/syntax/aml.vim", "start": 15336956, "end": 15361093}, {"filename": "/runtime/syntax/ampl.vim", "start": 15361093, "end": 15365201}, {"filename": "/runtime/syntax/ant.vim", "start": 15365201, "end": 15370670}, {"filename": "/runtime/syntax/antlr.vim", "start": 15370670, "end": 15372463}, {"filename": "/runtime/syntax/antlr4.vim", "start": 15372463, "end": 15373573}, {"filename": "/runtime/syntax/apache.vim", "start": 15373573, "end": 15396057}, {"filename": "/runtime/syntax/apachestyle.vim", "start": 15396057, "end": 15397474}, {"filename": "/runtime/syntax/apkbuild.vim", "start": 15397474, "end": 15397873}, {"filename": "/runtime/syntax/aptconf.vim", "start": 15397873, "end": 15420314}, {"filename": "/runtime/syntax/arch.vim", "start": 15420314, "end": 15421463}, {"filename": "/runtime/syntax/arduino.vim", "start": 15421463, "end": 15425437}, {"filename": "/runtime/syntax/art.vim", "start": 15425437, "end": 15426734}, {"filename": "/runtime/syntax/ascendc.vim", "start": 15426734, "end": 15428067}, {"filename": "/runtime/syntax/asciidoc.vim", "start": 15428067, "end": 15439213}, {"filename": "/runtime/syntax/asm.vim", "start": 15439213, "end": 15445348}, {"filename": "/runtime/syntax/asm68k.vim", "start": 15445348, "end": 15459628}, {"filename": "/runtime/syntax/asmh8300.vim", "start": 15459628, "end": 15461346}, {"filename": "/runtime/syntax/asn.vim", "start": 15461346, "end": 15463924}, {"filename": "/runtime/syntax/aspperl.vim", "start": 15463924, "end": 15464735}, {"filename": "/runtime/syntax/aspvbs.vim", "start": 15464735, "end": 15473834}, {"filename": "/runtime/syntax/asterisk.vim", "start": 15473834, "end": 15479463}, {"filename": "/runtime/syntax/asteriskvm.vim", "start": 15479463, "end": 15481795}, {"filename": "/runtime/syntax/astro.vim", "start": 15481795, "end": 15487965}, {"filename": "/runtime/syntax/asy.vim", "start": 15487965, "end": 15501240}, {"filename": "/runtime/syntax/atlas.vim", "start": 15501240, "end": 15503842}, {"filename": "/runtime/syntax/autodoc.vim", "start": 15503842, "end": 15507572}, {"filename": "/runtime/syntax/autohotkey.vim", "start": 15507572, "end": 15519712}, {"filename": "/runtime/syntax/autoit.vim", "start": 15519712, "end": 15567758}, {"filename": "/runtime/syntax/automake.vim", "start": 15567758, "end": 15571694}, {"filename": "/runtime/syntax/autopkgtest.vim", "start": 15571694, "end": 15575999}, {"filename": "/runtime/syntax/ave.vim", "start": 15575999, "end": 15577899}, {"filename": "/runtime/syntax/avra.vim", "start": 15577899, "end": 15580582}, {"filename": "/runtime/syntax/awk.vim", "start": 15580582, "end": 15588762}, {"filename": "/runtime/syntax/ayacc.vim", "start": 15588762, "end": 15591347}, {"filename": "/runtime/syntax/b.vim", "start": 15591347, "end": 15596249}, {"filename": "/runtime/syntax/baan.vim", "start": 15596249, "end": 15669844}, {"filename": "/runtime/syntax/bash.vim", "start": 15669844, "end": 15670255}, {"filename": "/runtime/syntax/basic.vim", "start": 15670255, "end": 15679665}, {"filename": "/runtime/syntax/bats.vim", "start": 15679665, "end": 15681074}, {"filename": "/runtime/syntax/bazelrc.vim", "start": 15681074, "end": 15684887}, {"filename": "/runtime/syntax/bc.vim", "start": 15684887, "end": 15686678}, {"filename": "/runtime/syntax/bdf.vim", "start": 15686678, "end": 15690519}, {"filename": "/runtime/syntax/beancount.vim", "start": 15690519, "end": 15694978}, {"filename": "/runtime/syntax/bib.vim", "start": 15694978, "end": 15700982}, {"filename": "/runtime/syntax/bindzone.vim", "start": 15700982, "end": 15705914}, {"filename": "/runtime/syntax/bitbake.vim", "start": 15705914, "end": 15712158}, {"filename": "/runtime/syntax/blank.vim", "start": 15712158, "end": 15712952}, {"filename": "/runtime/syntax/bpftrace.vim", "start": 15712952, "end": 15715191}, {"filename": "/runtime/syntax/bsdl.vim", "start": 15715191, "end": 15715568}, {"filename": "/runtime/syntax/bst.vim", "start": 15715568, "end": 15718393}, {"filename": "/runtime/syntax/btm.vim", "start": 15718393, "end": 15727678}, {"filename": "/runtime/syntax/bzl.vim", "start": 15727678, "end": 15728052}, {"filename": "/runtime/syntax/bzr.vim", "start": 15728052, "end": 15729612}, {"filename": "/runtime/syntax/c.vim", "start": 15729612, "end": 15765082}, {"filename": "/runtime/syntax/cabal.vim", "start": 15765082, "end": 15773184}, {"filename": "/runtime/syntax/cabalconfig.vim", "start": 15773184, "end": 15774363}, {"filename": "/runtime/syntax/cabalproject.vim", "start": 15774363, "end": 15775307}, {"filename": "/runtime/syntax/calendar.vim", "start": 15775307, "end": 15780349}, {"filename": "/runtime/syntax/cangjie.vim", "start": 15780349, "end": 15789613}, {"filename": "/runtime/syntax/catalog.vim", "start": 15789613, "end": 15790534}, {"filename": "/runtime/syntax/cdl.vim", "start": 15790534, "end": 15793879}, {"filename": "/runtime/syntax/cdrdaoconf.vim", "start": 15793879, "end": 15797526}, {"filename": "/runtime/syntax/cdrtoc.vim", "start": 15797526, "end": 15820282}, {"filename": "/runtime/syntax/cf.vim", "start": 15820282, "end": 15838926}, {"filename": "/runtime/syntax/cfg.vim", "start": 15838926, "end": 15840378}, {"filename": "/runtime/syntax/cgdbrc.vim", "start": 15840378, "end": 15840813}, {"filename": "/runtime/syntax/ch.vim", "start": 15840813, "end": 15841735}, {"filename": "/runtime/syntax/chaiscript.vim", "start": 15841735, "end": 15844622}, {"filename": "/runtime/syntax/change.vim", "start": 15844622, "end": 15845523}, {"filename": "/runtime/syntax/changelog.vim", "start": 15845523, "end": 15847620}, {"filename": "/runtime/syntax/chaskell.vim", "start": 15847620, "end": 15847953}, {"filename": "/runtime/syntax/chatito.vim", "start": 15847953, "end": 15849807}, {"filename": "/runtime/syntax/checkhealth.vim", "start": 15849807, "end": 15850402}, {"filename": "/runtime/syntax/cheetah.vim", "start": 15850402, "end": 15852154}, {"filename": "/runtime/syntax/chicken.vim", "start": 15852154, "end": 15855461}, {"filename": "/runtime/syntax/chill.vim", "start": 15855461, "end": 15863486}, {"filename": "/runtime/syntax/chordpro.vim", "start": 15863486, "end": 15870119}, {"filename": "/runtime/syntax/chuck.vim", "start": 15870119, "end": 15876980}, {"filename": "/runtime/syntax/cl.vim", "start": 15876980, "end": 15880885}, {"filename": "/runtime/syntax/clean.vim", "start": 15880885, "end": 15884036}, {"filename": "/runtime/syntax/clipper.vim", "start": 15884036, "end": 15889612}, {"filename": "/runtime/syntax/clojure.vim", "start": 15889612, "end": 15937309}, {"filename": "/runtime/syntax/cmacro.vim", "start": 15937309, "end": 15942440}, {"filename": "/runtime/syntax/cmake.vim", "start": 15942440, "end": 16107643}, {"filename": "/runtime/syntax/cmakecache.vim", "start": 16107643, "end": 16110182}, {"filename": "/runtime/syntax/cmod.vim", "start": 16110182, "end": 16116453}, {"filename": "/runtime/syntax/cmusrc.vim", "start": 16116453, "end": 16129374}, {"filename": "/runtime/syntax/cobol.vim", "start": 16129374, "end": 16142966}, {"filename": "/runtime/syntax/coco.vim", "start": 16142966, "end": 16144296}, {"filename": "/runtime/syntax/codeowners.vim", "start": 16144296, "end": 16144967}, {"filename": "/runtime/syntax/colortest.vim", "start": 16144967, "end": 16148234}, {"filename": "/runtime/syntax/conaryrecipe.vim", "start": 16148234, "end": 16156270}, {"filename": "/runtime/syntax/conf.vim", "start": 16156270, "end": 16157149}, {"filename": "/runtime/syntax/config.vim", "start": 16157149, "end": 16159468}, {"filename": "/runtime/syntax/confini.vim", "start": 16159468, "end": 16159886}, {"filename": "/runtime/syntax/context.vim", "start": 16159886, "end": 16166085}, {"filename": "/runtime/syntax/cpp.vim", "start": 16166085, "end": 16174165}, {"filename": "/runtime/syntax/crm.vim", "start": 16174165, "end": 16175362}, {"filename": "/runtime/syntax/crontab.vim", "start": 16175362, "end": 16177610}, {"filename": "/runtime/syntax/cs.vim", "start": 16177610, "end": 16194071}, {"filename": "/runtime/syntax/csc.vim", "start": 16194071, "end": 16202522}, {"filename": "/runtime/syntax/csdl.vim", "start": 16202522, "end": 16226277}, {"filename": "/runtime/syntax/csh.vim", "start": 16226277, "end": 16232680}, {"filename": "/runtime/syntax/csp.vim", "start": 16232680, "end": 16239832}, {"filename": "/runtime/syntax/css.vim", "start": 16239832, "end": 16275273}, {"filename": "/runtime/syntax/csv.vim", "start": 16275273, "end": 16276569}, {"filename": "/runtime/syntax/cterm.vim", "start": 16276569, "end": 16282975}, {"filename": "/runtime/syntax/ctrlh.vim", "start": 16282975, "end": 16283729}, {"filename": "/runtime/syntax/cucumber.vim", "start": 16283729, "end": 16336328}, {"filename": "/runtime/syntax/cuda.vim", "start": 16336328, "end": 16338639}, {"filename": "/runtime/syntax/cupl.vim", "start": 16338639, "end": 16342511}, {"filename": "/runtime/syntax/cuplsim.vim", "start": 16342511, "end": 16344427}, {"filename": "/runtime/syntax/cvs.vim", "start": 16344427, "end": 16345309}, {"filename": "/runtime/syntax/cvsrc.vim", "start": 16345309, "end": 16346563}, {"filename": "/runtime/syntax/cweb.vim", "start": 16346563, "end": 16349455}, {"filename": "/runtime/syntax/cynlib.vim", "start": 16349455, "end": 16352003}, {"filename": "/runtime/syntax/cynpp.vim", "start": 16352003, "end": 16353266}, {"filename": "/runtime/syntax/d.vim", "start": 16353266, "end": 16381898}, {"filename": "/runtime/syntax/dart.vim", "start": 16381898, "end": 16385190}, {"filename": "/runtime/syntax/datascript.vim", "start": 16385190, "end": 16388997}, {"filename": "/runtime/syntax/dax.vim", "start": 16388997, "end": 16396476}, {"filename": "/runtime/syntax/dcd.vim", "start": 16396476, "end": 16398279}, {"filename": "/runtime/syntax/dcl.vim", "start": 16398279, "end": 16404346}, {"filename": "/runtime/syntax/deb822sources.vim", "start": 16404346, "end": 16408001}, {"filename": "/runtime/syntax/debchangelog.vim", "start": 16408001, "end": 16410844}, {"filename": "/runtime/syntax/debcontrol.vim", "start": 16410844, "end": 16420357}, {"filename": "/runtime/syntax/debcopyright.vim", "start": 16420357, "end": 16421507}, {"filename": "/runtime/syntax/debsources.vim", "start": 16421507, "end": 16423528}, {"filename": "/runtime/syntax/def.vim", "start": 16423528, "end": 16424625}, {"filename": "/runtime/syntax/denyhosts.vim", "start": 16424625, "end": 16431990}, {"filename": "/runtime/syntax/dep3patch.vim", "start": 16431990, "end": 16434993}, {"filename": "/runtime/syntax/desc.vim", "start": 16434993, "end": 16438070}, {"filename": "/runtime/syntax/desktop.vim", "start": 16438070, "end": 16447454}, {"filename": "/runtime/syntax/dictconf.vim", "start": 16447454, "end": 16450572}, {"filename": "/runtime/syntax/dictdconf.vim", "start": 16450572, "end": 16457037}, {"filename": "/runtime/syntax/diff.vim", "start": 16457037, "end": 16474513}, {"filename": "/runtime/syntax/dircolors.vim", "start": 16474513, "end": 16481649}, {"filename": "/runtime/syntax/directory.vim", "start": 16481649, "end": 16481926}, {"filename": "/runtime/syntax/dirpager.vim", "start": 16481926, "end": 16483716}, {"filename": "/runtime/syntax/diva.vim", "start": 16483716, "end": 16488452}, {"filename": "/runtime/syntax/django.vim", "start": 16488452, "end": 16493017}, {"filename": "/runtime/syntax/dns.vim", "start": 16493017, "end": 16493149}, {"filename": "/runtime/syntax/dnsmasq.vim", "start": 16493149, "end": 16506705}, {"filename": "/runtime/syntax/docbk.vim", "start": 16506705, "end": 16517026}, {"filename": "/runtime/syntax/docbksgml.vim", "start": 16517026, "end": 16517219}, {"filename": "/runtime/syntax/docbkxml.vim", "start": 16517219, "end": 16517410}, {"filename": "/runtime/syntax/dockerfile.vim", "start": 16517410, "end": 16519962}, {"filename": "/runtime/syntax/dosbatch.vim", "start": 16519962, "end": 16527112}, {"filename": "/runtime/syntax/dosini.vim", "start": 16527112, "end": 16528665}, {"filename": "/runtime/syntax/dot.vim", "start": 16528665, "end": 16533158}, {"filename": "/runtime/syntax/doxygen.vim", "start": 16533158, "end": 16572244}, {"filename": "/runtime/syntax/dracula.vim", "start": 16572244, "end": 16574912}, {"filename": "/runtime/syntax/dsl.vim", "start": 16574912, "end": 16576224}, {"filename": "/runtime/syntax/dtd.vim", "start": 16576224, "end": 16581214}, {"filename": "/runtime/syntax/dtml.vim", "start": 16581214, "end": 16597827}, {"filename": "/runtime/syntax/dtrace.vim", "start": 16597827, "end": 16604388}, {"filename": "/runtime/syntax/dts.vim", "start": 16604388, "end": 16609661}, {"filename": "/runtime/syntax/dune.vim", "start": 16609661, "end": 16611739}, {"filename": "/runtime/syntax/dylan.vim", "start": 16611739, "end": 16615153}, {"filename": "/runtime/syntax/dylanintr.vim", "start": 16615153, "end": 16616231}, {"filename": "/runtime/syntax/dylanlid.vim", "start": 16616231, "end": 16616895}, {"filename": "/runtime/syntax/ecd.vim", "start": 16616895, "end": 16618124}, {"filename": "/runtime/syntax/ed.vim", "start": 16618124, "end": 16637217}, {"filename": "/runtime/syntax/edif.vim", "start": 16637217, "end": 16638495}, {"filename": "/runtime/syntax/editorconfig.vim", "start": 16638495, "end": 16639016}, {"filename": "/runtime/syntax/eiffel.vim", "start": 16639016, "end": 16645942}, {"filename": "/runtime/syntax/elf.vim", "start": 16645942, "end": 16648291}, {"filename": "/runtime/syntax/elinks.vim", "start": 16648291, "end": 16658785}, {"filename": "/runtime/syntax/elm.vim", "start": 16658785, "end": 16662436}, {"filename": "/runtime/syntax/elmfilt.vim", "start": 16662436, "end": 16665373}, {"filename": "/runtime/syntax/env.vim", "start": 16665373, "end": 16666221}, {"filename": "/runtime/syntax/erlang.vim", "start": 16666221, "end": 16684876}, {"filename": "/runtime/syntax/eruby.vim", "start": 16684876, "end": 16687956}, {"filename": "/runtime/syntax/esmtprc.vim", "start": 16687956, "end": 16688894}, {"filename": "/runtime/syntax/esqlc.vim", "start": 16688894, "end": 16690988}, {"filename": "/runtime/syntax/esterel.vim", "start": 16690988, "end": 16694041}, {"filename": "/runtime/syntax/eterm.vim", "start": 16694041, "end": 16713213}, {"filename": "/runtime/syntax/euphoria3.vim", "start": 16713213, "end": 16719316}, {"filename": "/runtime/syntax/euphoria4.vim", "start": 16719316, "end": 16733854}, {"filename": "/runtime/syntax/eviews.vim", "start": 16733854, "end": 16738858}, {"filename": "/runtime/syntax/exim.vim", "start": 16738858, "end": 16756943}, {"filename": "/runtime/syntax/expect.vim", "start": 16756943, "end": 16760677}, {"filename": "/runtime/syntax/exports.vim", "start": 16760677, "end": 16762469}, {"filename": "/runtime/syntax/falcon.vim", "start": 16762469, "end": 16774482}, {"filename": "/runtime/syntax/fan.vim", "start": 16774482, "end": 16779927}, {"filename": "/runtime/syntax/fasm.vim", "start": 16779927, "end": 16788684}, {"filename": "/runtime/syntax/fdcc.vim", "start": 16788684, "end": 16794332}, {"filename": "/runtime/syntax/fetchmail.vim", "start": 16794332, "end": 16797336}, {"filename": "/runtime/syntax/fgl.vim", "start": 16797336, "end": 16804610}, {"filename": "/runtime/syntax/fish.vim", "start": 16804610, "end": 16814041}, {"filename": "/runtime/syntax/flexwiki.vim", "start": 16814041, "end": 16819478}, {"filename": "/runtime/syntax/focexec.vim", "start": 16819478, "end": 16823005}, {"filename": "/runtime/syntax/form.vim", "start": 16823005, "end": 16829390}, {"filename": "/runtime/syntax/forth.vim", "start": 16829390, "end": 16846022}, {"filename": "/runtime/syntax/fortran.vim", "start": 16846022, "end": 16882186}, {"filename": "/runtime/syntax/foxpro.vim", "start": 16882186, "end": 16915042}, {"filename": "/runtime/syntax/fpcmake.vim", "start": 16915042, "end": 16917086}, {"filename": "/runtime/syntax/framescript.vim", "start": 16917086, "end": 16928765}, {"filename": "/runtime/syntax/freebasic.vim", "start": 16928765, "end": 16955386}, {"filename": "/runtime/syntax/fstab.vim", "start": 16955386, "end": 16986765}, {"filename": "/runtime/syntax/fvwm.vim", "start": 16986765, "end": 17012040}, {"filename": "/runtime/syntax/fvwm2m4.vim", "start": 17012040, "end": 17012751}, {"filename": "/runtime/syntax/gdb.vim", "start": 17012751, "end": 17128587}, {"filename": "/runtime/syntax/gdmo.vim", "start": 17128587, "end": 17131886}, {"filename": "/runtime/syntax/gdresource.vim", "start": 17131886, "end": 17134175}, {"filename": "/runtime/syntax/gdscript.vim", "start": 17134175, "end": 17138053}, {"filename": "/runtime/syntax/gdshader.vim", "start": 17138053, "end": 17140070}, {"filename": "/runtime/syntax/gedcom.vim", "start": 17140070, "end": 17142345}, {"filename": "/runtime/syntax/gel.vim", "start": 17142345, "end": 17145017}, {"filename": "/runtime/syntax/gemtext.vim", "start": 17145017, "end": 17145699}, {"filename": "/runtime/syntax/gift.vim", "start": 17145699, "end": 17155938}, {"filename": "/runtime/syntax/git.vim", "start": 17155938, "end": 17163405}, {"filename": "/runtime/syntax/gitattributes.vim", "start": 17163405, "end": 17165847}, {"filename": "/runtime/syntax/gitcommit.vim", "start": 17165847, "end": 17172465}, {"filename": "/runtime/syntax/gitconfig.vim", "start": 17172465, "end": 17174061}, {"filename": "/runtime/syntax/gitignore.vim", "start": 17174061, "end": 17174966}, {"filename": "/runtime/syntax/gitolite.vim", "start": 17174966, "end": 17179420}, {"filename": "/runtime/syntax/gitrebase.vim", "start": 17179420, "end": 17182620}, {"filename": "/runtime/syntax/gitrevlist.vim", "start": 17182620, "end": 17183117}, {"filename": "/runtime/syntax/gitsendemail.vim", "start": 17183117, "end": 17183697}, {"filename": "/runtime/syntax/gkrellmrc.vim", "start": 17183697, "end": 17187813}, {"filename": "/runtime/syntax/gleam.vim", "start": 17187813, "end": 17190621}, {"filename": "/runtime/syntax/glimmer.vim", "start": 17190621, "end": 17192076}, {"filename": "/runtime/syntax/glsl.vim", "start": 17192076, "end": 17224879}, {"filename": "/runtime/syntax/gnash.vim", "start": 17224879, "end": 17228341}, {"filename": "/runtime/syntax/gnuplot.vim", "start": 17228341, "end": 17252166}, {"filename": "/runtime/syntax/go.vim", "start": 17252166, "end": 17277715}, {"filename": "/runtime/syntax/goaccess.vim", "start": 17277715, "end": 17278979}, {"filename": "/runtime/syntax/godoc.vim", "start": 17278979, "end": 17279331}, {"filename": "/runtime/syntax/gp.vim", "start": 17279331, "end": 17282711}, {"filename": "/runtime/syntax/gpg.vim", "start": 17282711, "end": 17291200}, {"filename": "/runtime/syntax/gprof.vim", "start": 17291200, "end": 17293336}, {"filename": "/runtime/syntax/grads.vim", "start": 17293336, "end": 17295078}, {"filename": "/runtime/syntax/graphql.vim", "start": 17295078, "end": 17299029}, {"filename": "/runtime/syntax/gretl.vim", "start": 17299029, "end": 17302759}, {"filename": "/runtime/syntax/groff.vim", "start": 17302759, "end": 17303080}, {"filename": "/runtime/syntax/groovy.vim", "start": 17303080, "end": 17325563}, {"filename": "/runtime/syntax/group.vim", "start": 17325563, "end": 17327282}, {"filename": "/runtime/syntax/grub.vim", "start": 17327282, "end": 17331164}, {"filename": "/runtime/syntax/gsp.vim", "start": 17331164, "end": 17333195}, {"filename": "/runtime/syntax/gtkrc.vim", "start": 17333195, "end": 17342513}, {"filename": "/runtime/syntax/gvpr.vim", "start": 17342513, "end": 17345891}, {"filename": "/runtime/syntax/gyp.vim", "start": 17345891, "end": 17347545}, {"filename": "/runtime/syntax/haml.vim", "start": 17347545, "end": 17354616}, {"filename": "/runtime/syntax/hamster.vim", "start": 17354616, "end": 17368456}, {"filename": "/runtime/syntax/handlebars.vim", "start": 17368456, "end": 17375844}, {"filename": "/runtime/syntax/hare.vim", "start": 17375844, "end": 17381140}, {"filename": "/runtime/syntax/haredoc.vim", "start": 17381140, "end": 17382065}, {"filename": "/runtime/syntax/haskell.vim", "start": 17382065, "end": 17391538}, {"filename": "/runtime/syntax/haste.vim", "start": 17391538, "end": 17396532}, {"filename": "/runtime/syntax/hastepreproc.vim", "start": 17396532, "end": 17397954}, {"filename": "/runtime/syntax/hb.vim", "start": 17397954, "end": 17401112}, {"filename": "/runtime/syntax/hcl.vim", "start": 17401112, "end": 17404284}, {"filename": "/runtime/syntax/help.vim", "start": 17404284, "end": 17414521}, {"filename": "/runtime/syntax/help_it.vim", "start": 17414521, "end": 17415182}, {"filename": "/runtime/syntax/help_ru.vim", "start": 17415182, "end": 17418703}, {"filename": "/runtime/syntax/hercules.vim", "start": 17418703, "end": 17424423}, {"filename": "/runtime/syntax/hex.vim", "start": 17424423, "end": 17428434}, {"filename": "/runtime/syntax/hgcommit.vim", "start": 17428434, "end": 17429847}, {"filename": "/runtime/syntax/hip.vim", "start": 17429847, "end": 17432047}, {"filename": "/runtime/syntax/hitest.vim", "start": 17432047, "end": 17435690}, {"filename": "/runtime/syntax/hlsl.vim", "start": 17435690, "end": 17440891}, {"filename": "/runtime/syntax/hlsplaylist.vim", "start": 17440891, "end": 17448511}, {"filename": "/runtime/syntax/hog.vim", "start": 17448511, "end": 17460623}, {"filename": "/runtime/syntax/hollywood.vim", "start": 17460623, "end": 17511156}, {"filename": "/runtime/syntax/hostconf.vim", "start": 17511156, "end": 17515026}, {"filename": "/runtime/syntax/hostsaccess.vim", "start": 17515026, "end": 17515575}, {"filename": "/runtime/syntax/html.vim", "start": 17515575, "end": 17535517}, {"filename": "/runtime/syntax/htmlangular.vim", "start": 17535517, "end": 17535888}, {"filename": "/runtime/syntax/htmlcheetah.vim", "start": 17535888, "end": 17536376}, {"filename": "/runtime/syntax/htmldjango.vim", "start": 17536376, "end": 17537387}, {"filename": "/runtime/syntax/htmlm4.vim", "start": 17537387, "end": 17538057}, {"filename": "/runtime/syntax/htmlos.vim", "start": 17538057, "end": 17546370}, {"filename": "/runtime/syntax/hyprlang.vim", "start": 17546370, "end": 17548926}, {"filename": "/runtime/syntax/i3config.vim", "start": 17548926, "end": 17572150}, {"filename": "/runtime/syntax/ia64.vim", "start": 17572150, "end": 17582623}, {"filename": "/runtime/syntax/ibasic.vim", "start": 17582623, "end": 17589659}, {"filename": "/runtime/syntax/icalendar.vim", "start": 17589659, "end": 17594625}, {"filename": "/runtime/syntax/icemenu.vim", "start": 17594625, "end": 17595449}, {"filename": "/runtime/syntax/icon.vim", "start": 17595449, "end": 17604375}, {"filename": "/runtime/syntax/idl.vim", "start": 17604375, "end": 17624276}, {"filename": "/runtime/syntax/idlang.vim", "start": 17624276, "end": 17637818}, {"filename": "/runtime/syntax/idris2.vim", "start": 17637818, "end": 17642245}, {"filename": "/runtime/syntax/indent.vim", "start": 17642245, "end": 17649445}, {"filename": "/runtime/syntax/inform.vim", "start": 17649445, "end": 17668928}, {"filename": "/runtime/syntax/initex.vim", "start": 17668928, "end": 17688152}, {"filename": "/runtime/syntax/initng.vim", "start": 17688152, "end": 17691527}, {"filename": "/runtime/syntax/inittab.vim", "start": 17691527, "end": 17694068}, {"filename": "/runtime/syntax/ipfilter.vim", "start": 17694068, "end": 17695724}, {"filename": "/runtime/syntax/ipkg.vim", "start": 17695724, "end": 17697321}, {"filename": "/runtime/syntax/ishd.vim", "start": 17697321, "end": 17724378}, {"filename": "/runtime/syntax/iss.vim", "start": 17724378, "end": 17729382}, {"filename": "/runtime/syntax/ist.vim", "start": 17729382, "end": 17731382}, {"filename": "/runtime/syntax/j.vim", "start": 17731382, "end": 17740823}, {"filename": "/runtime/syntax/jal.vim", "start": 17740823, "end": 17749786}, {"filename": "/runtime/syntax/jam.vim", "start": 17749786, "end": 17776497}, {"filename": "/runtime/syntax/jargon.vim", "start": 17776497, "end": 17777237}, {"filename": "/runtime/syntax/java.vim", "start": 17777237, "end": 17834276}, {"filename": "/runtime/syntax/javacc.vim", "start": 17834276, "end": 17836651}, {"filename": "/runtime/syntax/javascript.vim", "start": 17836651, "end": 17843005}, {"filename": "/runtime/syntax/javascriptreact.vim", "start": 17843005, "end": 17843138}, {"filename": "/runtime/syntax/jess.vim", "start": 17843138, "end": 17849368}, {"filename": "/runtime/syntax/jgraph.vim", "start": 17849368, "end": 17850580}, {"filename": "/runtime/syntax/jinja.vim", "start": 17850580, "end": 17855162}, {"filename": "/runtime/syntax/jjdescription.vim", "start": 17855162, "end": 17856429}, {"filename": "/runtime/syntax/jovial.vim", "start": 17856429, "end": 17860536}, {"filename": "/runtime/syntax/jproperties.vim", "start": 17860536, "end": 17866032}, {"filename": "/runtime/syntax/jq.vim", "start": 17866032, "end": 17871712}, {"filename": "/runtime/syntax/json.vim", "start": 17871712, "end": 17877986}, {"filename": "/runtime/syntax/json5.vim", "start": 17877986, "end": 17880541}, {"filename": "/runtime/syntax/jsonc.vim", "start": 17880541, "end": 17882274}, {"filename": "/runtime/syntax/jsonld.vim", "start": 17882274, "end": 17883472}, {"filename": "/runtime/syntax/jsp.vim", "start": 17883472, "end": 17886150}, {"filename": "/runtime/syntax/julia.vim", "start": 17886150, "end": 17924341}, {"filename": "/runtime/syntax/just.vim", "start": 17924341, "end": 17945384}, {"filename": "/runtime/syntax/karel.vim", "start": 17945384, "end": 17951831}, {"filename": "/runtime/syntax/kconfig.vim", "start": 17951831, "end": 17989270}, {"filename": "/runtime/syntax/kdl.vim", "start": 17989270, "end": 17990919}, {"filename": "/runtime/syntax/kitty.vim", "start": 17990919, "end": 17995865}, {"filename": "/runtime/syntax/kivy.vim", "start": 17995865, "end": 17996856}, {"filename": "/runtime/syntax/kix.vim", "start": 17996856, "end": 18003185}, {"filename": "/runtime/syntax/kotlin.vim", "start": 18003185, "end": 18012619}, {"filename": "/runtime/syntax/krl.vim", "start": 18012619, "end": 18033283}, {"filename": "/runtime/syntax/kscript.vim", "start": 18033283, "end": 18035268}, {"filename": "/runtime/syntax/kwt.vim", "start": 18035268, "end": 18037722}, {"filename": "/runtime/syntax/lace.vim", "start": 18037722, "end": 18041797}, {"filename": "/runtime/syntax/latte.vim", "start": 18041797, "end": 18045308}, {"filename": "/runtime/syntax/lc.vim", "start": 18045308, "end": 18046103}, {"filename": "/runtime/syntax/ld.vim", "start": 18046103, "end": 18049050}, {"filename": "/runtime/syntax/ldapconf.vim", "start": 18049050, "end": 18062352}, {"filename": "/runtime/syntax/ldif.vim", "start": 18062352, "end": 18063366}, {"filename": "/runtime/syntax/leex.vim", "start": 18063366, "end": 18066067}, {"filename": "/runtime/syntax/less.vim", "start": 18066067, "end": 18070262}, {"filename": "/runtime/syntax/lex.vim", "start": 18070262, "end": 18079001}, {"filename": "/runtime/syntax/lf.vim", "start": 18079001, "end": 18083533}, {"filename": "/runtime/syntax/lftp.vim", "start": 18083533, "end": 18091004}, {"filename": "/runtime/syntax/lhaskell.vim", "start": 18091004, "end": 18095977}, {"filename": "/runtime/syntax/libao.vim", "start": 18095977, "end": 18096613}, {"filename": "/runtime/syntax/lidris2.vim", "start": 18096613, "end": 18097417}, {"filename": "/runtime/syntax/lifelines.vim", "start": 18097417, "end": 18104057}, {"filename": "/runtime/syntax/lilo.vim", "start": 18104057, "end": 18114484}, {"filename": "/runtime/syntax/limits.vim", "start": 18114484, "end": 18115801}, {"filename": "/runtime/syntax/liquid.vim", "start": 18115801, "end": 18122258}, {"filename": "/runtime/syntax/lisp.vim", "start": 18122258, "end": 18158380}, {"filename": "/runtime/syntax/lite.vim", "start": 18158380, "end": 18163225}, {"filename": "/runtime/syntax/litestep.vim", "start": 18163225, "end": 18169901}, {"filename": "/runtime/syntax/livebook.vim", "start": 18169901, "end": 18170055}, {"filename": "/runtime/syntax/lnk.vim", "start": 18170055, "end": 18172018}, {"filename": "/runtime/syntax/lnkmap.vim", "start": 18172018, "end": 18173213}, {"filename": "/runtime/syntax/log.vim", "start": 18173213, "end": 18182811}, {"filename": "/runtime/syntax/loginaccess.vim", "start": 18182811, "end": 18187008}, {"filename": "/runtime/syntax/logindefs.vim", "start": 18187008, "end": 18195635}, {"filename": "/runtime/syntax/logtalk.vim", "start": 18195635, "end": 18212868}, {"filename": "/runtime/syntax/lotos.vim", "start": 18212868, "end": 18215229}, {"filename": "/runtime/syntax/lout.vim", "start": 18215229, "end": 18219555}, {"filename": "/runtime/syntax/lpc.vim", "start": 18219555, "end": 18242693}, {"filename": "/runtime/syntax/lprolog.vim", "start": 18242693, "end": 18246672}, {"filename": "/runtime/syntax/lscript.vim", "start": 18246672, "end": 18258771}, {"filename": "/runtime/syntax/lsl.vim", "start": 18258771, "end": 18274571}, {"filename": "/runtime/syntax/lsp_markdown.vim", "start": 18274571, "end": 18276022}, {"filename": "/runtime/syntax/lss.vim", "start": 18276022, "end": 18280891}, {"filename": "/runtime/syntax/lua.vim", "start": 18280891, "end": 18296941}, {"filename": "/runtime/syntax/luau.vim", "start": 18296941, "end": 18309216}, {"filename": "/runtime/syntax/lynx.vim", "start": 18309216, "end": 18316532}, {"filename": "/runtime/syntax/lyrics.vim", "start": 18316532, "end": 18317710}, {"filename": "/runtime/syntax/m17ndb.vim", "start": 18317710, "end": 18318731}, {"filename": "/runtime/syntax/m3build.vim", "start": 18318731, "end": 18324508}, {"filename": "/runtime/syntax/m3quake.vim", "start": 18324508, "end": 18327142}, {"filename": "/runtime/syntax/m4.vim", "start": 18327142, "end": 18331679}, {"filename": "/runtime/syntax/mail.vim", "start": 18331679, "end": 18337874}, {"filename": "/runtime/syntax/mailaliases.vim", "start": 18337874, "end": 18340641}, {"filename": "/runtime/syntax/mailcap.vim", "start": 18340641, "end": 18341776}, {"filename": "/runtime/syntax/make.vim", "start": 18341776, "end": 18351296}, {"filename": "/runtime/syntax/mallard.vim", "start": 18351296, "end": 18353022}, {"filename": "/runtime/syntax/man.vim", "start": 18353022, "end": 18355364}, {"filename": "/runtime/syntax/manconf.vim", "start": 18355364, "end": 18359708}, {"filename": "/runtime/syntax/manual.vim", "start": 18359708, "end": 18360683}, {"filename": "/runtime/syntax/maple.vim", "start": 18360683, "end": 18389347}, {"filename": "/runtime/syntax/markdown.vim", "start": 18389347, "end": 18399931}, {"filename": "/runtime/syntax/marko.vim", "start": 18399931, "end": 18431662}, {"filename": "/runtime/syntax/masm.vim", "start": 18431662, "end": 18460288}, {"filename": "/runtime/syntax/mason.vim", "start": 18460288, "end": 18464216}, {"filename": "/runtime/syntax/master.vim", "start": 18464216, "end": 18465238}, {"filename": "/runtime/syntax/matlab.vim", "start": 18465238, "end": 18469554}, {"filename": "/runtime/syntax/maxima.vim", "start": 18469554, "end": 18488356}, {"filename": "/runtime/syntax/mbsync.vim", "start": 18488356, "end": 18502640}, {"filename": "/runtime/syntax/mediawiki.vim", "start": 18502640, "end": 18518926}, {"filename": "/runtime/syntax/mel.vim", "start": 18518926, "end": 18522331}, {"filename": "/runtime/syntax/mermaid.vim", "start": 18522331, "end": 18525196}, {"filename": "/runtime/syntax/meson.vim", "start": 18525196, "end": 18529255}, {"filename": "/runtime/syntax/messages.vim", "start": 18529255, "end": 18531970}, {"filename": "/runtime/syntax/mf.vim", "start": 18531970, "end": 18547393}, {"filename": "/runtime/syntax/mgl.vim", "start": 18547393, "end": 18551117}, {"filename": "/runtime/syntax/mgp.vim", "start": 18551117, "end": 18553117}, {"filename": "/runtime/syntax/mib.vim", "start": 18553117, "end": 18555813}, {"filename": "/runtime/syntax/mix.vim", "start": 18555813, "end": 18558755}, {"filename": "/runtime/syntax/mma.vim", "start": 18558755, "end": 18570618}, {"filename": "/runtime/syntax/mmix.vim", "start": 18570618, "end": 18575587}, {"filename": "/runtime/syntax/mmp.vim", "start": 18575587, "end": 18577073}, {"filename": "/runtime/syntax/modconf.vim", "start": 18577073, "end": 18578492}, {"filename": "/runtime/syntax/model.vim", "start": 18578492, "end": 18580020}, {"filename": "/runtime/syntax/modsim3.vim", "start": 18580020, "end": 18583643}, {"filename": "/runtime/syntax/modula2.vim", "start": 18583643, "end": 18584178}, {"filename": "/runtime/syntax/modula2/opt/iso.vim", "start": 18584178, "end": 18599484}, {"filename": "/runtime/syntax/modula2/opt/pim.vim", "start": 18599484, "end": 18614603}, {"filename": "/runtime/syntax/modula2/opt/r10.vim", "start": 18614603, "end": 18633377}, {"filename": "/runtime/syntax/modula3.vim", "start": 18633377, "end": 18637913}, {"filename": "/runtime/syntax/mojo.vim", "start": 18637913, "end": 18650560}, {"filename": "/runtime/syntax/monk.vim", "start": 18650560, "end": 18660526}, {"filename": "/runtime/syntax/moo.vim", "start": 18660526, "end": 18669760}, {"filename": "/runtime/syntax/mp.vim", "start": 18669760, "end": 18714290}, {"filename": "/runtime/syntax/mplayerconf.vim", "start": 18714290, "end": 18721777}, {"filename": "/runtime/syntax/mrxvtrc.vim", "start": 18721777, "end": 18731380}, {"filename": "/runtime/syntax/msidl.vim", "start": 18731380, "end": 18734844}, {"filename": "/runtime/syntax/msmessages.vim", "start": 18734844, "end": 18741162}, {"filename": "/runtime/syntax/msql.vim", "start": 18741162, "end": 18743552}, {"filename": "/runtime/syntax/mss.vim", "start": 18743552, "end": 18744404}, {"filename": "/runtime/syntax/mupad.vim", "start": 18744404, "end": 18759911}, {"filename": "/runtime/syntax/murphi.vim", "start": 18759911, "end": 18764490}, {"filename": "/runtime/syntax/mush.vim", "start": 18764490, "end": 18777677}, {"filename": "/runtime/syntax/muttrc.vim", "start": 18777677, "end": 18847224}, {"filename": "/runtime/syntax/mysql.vim", "start": 18847224, "end": 18866577}, {"filename": "/runtime/syntax/n1ql.vim", "start": 18866577, "end": 18879476}, {"filename": "/runtime/syntax/named.vim", "start": 18879476, "end": 18889481}, {"filename": "/runtime/syntax/nanorc.vim", "start": 18889481, "end": 18900101}, {"filename": "/runtime/syntax/nasm.vim", "start": 18900101, "end": 18973591}, {"filename": "/runtime/syntax/nastran.vim", "start": 18973591, "end": 18982095}, {"filename": "/runtime/syntax/natural.vim", "start": 18982095, "end": 18989072}, {"filename": "/runtime/syntax/ncf.vim", "start": 18989072, "end": 19002488}, {"filename": "/runtime/syntax/neomuttlog.vim", "start": 19002488, "end": 19005970}, {"filename": "/runtime/syntax/neomuttrc.vim", "start": 19005970, "end": 19078299}, {"filename": "/runtime/syntax/netrc.vim", "start": 19078299, "end": 19080349}, {"filename": "/runtime/syntax/nginx.vim", "start": 19080349, "end": 19197084}, {"filename": "/runtime/syntax/ninja.vim", "start": 19197084, "end": 19199973}, {"filename": "/runtime/syntax/nix.vim", "start": 19199973, "end": 19211798}, {"filename": "/runtime/syntax/nosyntax.vim", "start": 19211798, "end": 19212619}, {"filename": "/runtime/syntax/nqc.vim", "start": 19212619, "end": 19226687}, {"filename": "/runtime/syntax/nroff.vim", "start": 19226687, "end": 19234292}, {"filename": "/runtime/syntax/nsis.vim", "start": 19234292, "end": 19273607}, {"filename": "/runtime/syntax/nu.vim", "start": 19273607, "end": 19302044}, {"filename": "/runtime/syntax/obj.vim", "start": 19302044, "end": 19304830}, {"filename": "/runtime/syntax/objc.vim", "start": 19304830, "end": 19346595}, {"filename": "/runtime/syntax/objcpp.vim", "start": 19346595, "end": 19347195}, {"filename": "/runtime/syntax/obse.vim", "start": 19347195, "end": 19433674}, {"filename": "/runtime/syntax/ocaml.vim", "start": 19433674, "end": 19464217}, {"filename": "/runtime/syntax/occam.vim", "start": 19464217, "end": 19468533}, {"filename": "/runtime/syntax/odin.vim", "start": 19468533, "end": 19472423}, {"filename": "/runtime/syntax/omnimark.vim", "start": 19472423, "end": 19478800}, {"filename": "/runtime/syntax/ondir.vim", "start": 19478800, "end": 19479854}, {"filename": "/runtime/syntax/opam.vim", "start": 19479854, "end": 19482236}, {"filename": "/runtime/syntax/opencl.vim", "start": 19482236, "end": 19482486}, {"filename": "/runtime/syntax/openroad.vim", "start": 19482486, "end": 19496465}, {"filename": "/runtime/syntax/openscad.vim", "start": 19496465, "end": 19501400}, {"filename": "/runtime/syntax/openvpn.vim", "start": 19501400, "end": 19503706}, {"filename": "/runtime/syntax/opl.vim", "start": 19503706, "end": 19507834}, {"filename": "/runtime/syntax/ora.vim", "start": 19507834, "end": 19534975}, {"filename": "/runtime/syntax/org.vim", "start": 19534975, "end": 19537882}, {"filename": "/runtime/syntax/pacmanlog.vim", "start": 19537882, "end": 19539910}, {"filename": "/runtime/syntax/pamconf.vim", "start": 19539910, "end": 19546238}, {"filename": "/runtime/syntax/pamenv.vim", "start": 19546238, "end": 19547373}, {"filename": "/runtime/syntax/pandoc.vim", "start": 19547373, "end": 19583838}, {"filename": "/runtime/syntax/papp.vim", "start": 19583838, "end": 19587627}, {"filename": "/runtime/syntax/pascal.vim", "start": 19587627, "end": 19603611}, {"filename": "/runtime/syntax/passwd.vim", "start": 19603611, "end": 19606054}, {"filename": "/runtime/syntax/pbtxt.vim", "start": 19606054, "end": 19607427}, {"filename": "/runtime/syntax/pcap.vim", "start": 19607427, "end": 19608923}, {"filename": "/runtime/syntax/pccts.vim", "start": 19608923, "end": 19613016}, {"filename": "/runtime/syntax/pdf.vim", "start": 19613016, "end": 19616555}, {"filename": "/runtime/syntax/perl.vim", "start": 19616555, "end": 19661740}, {"filename": "/runtime/syntax/pf.vim", "start": 19661740, "end": 19672111}, {"filename": "/runtime/syntax/pfmain.vim", "start": 19672111, "end": 19765029}, {"filename": "/runtime/syntax/php.vim", "start": 19765029, "end": 19879595}, {"filename": "/runtime/syntax/phtml.vim", "start": 19879595, "end": 19879746}, {"filename": "/runtime/syntax/pic.vim", "start": 19879746, "end": 19883079}, {"filename": "/runtime/syntax/pike.vim", "start": 19883079, "end": 19904886}, {"filename": "/runtime/syntax/pilrc.vim", "start": 19904886, "end": 19910242}, {"filename": "/runtime/syntax/pine.vim", "start": 19910242, "end": 19925653}, {"filename": "/runtime/syntax/pinfo.vim", "start": 19925653, "end": 19930949}, {"filename": "/runtime/syntax/pkl.vim", "start": 19930949, "end": 19942310}, {"filename": "/runtime/syntax/plaintex.vim", "start": 19942310, "end": 19952611}, {"filename": "/runtime/syntax/pli.vim", "start": 19952611, "end": 19965833}, {"filename": "/runtime/syntax/plm.vim", "start": 19965833, "end": 19971194}, {"filename": "/runtime/syntax/plp.vim", "start": 19971194, "end": 19972461}, {"filename": "/runtime/syntax/plsql.vim", "start": 19972461, "end": 20035319}, {"filename": "/runtime/syntax/po.vim", "start": 20035319, "end": 20043573}, {"filename": "/runtime/syntax/pod.vim", "start": 20043573, "end": 20054460}, {"filename": "/runtime/syntax/poefilter.vim", "start": 20054460, "end": 20060247}, {"filename": "/runtime/syntax/poke.vim", "start": 20060247, "end": 20065436}, {"filename": "/runtime/syntax/postscr.vim", "start": 20065436, "end": 20111609}, {"filename": "/runtime/syntax/pov.vim", "start": 20111609, "end": 20120932}, {"filename": "/runtime/syntax/povini.vim", "start": 20120932, "end": 20124420}, {"filename": "/runtime/syntax/ppd.vim", "start": 20124420, "end": 20125290}, {"filename": "/runtime/syntax/ppwiz.vim", "start": 20125290, "end": 20128070}, {"filename": "/runtime/syntax/pq.vim", "start": 20128070, "end": 20144152}, {"filename": "/runtime/syntax/prescribe.vim", "start": 20144152, "end": 20146535}, {"filename": "/runtime/syntax/privoxy.vim", "start": 20146535, "end": 20151923}, {"filename": "/runtime/syntax/procmail.vim", "start": 20151923, "end": 20153942}, {"filename": "/runtime/syntax/progress.vim", "start": 20153942, "end": 20186005}, {"filename": "/runtime/syntax/prolog.vim", "start": 20186005, "end": 20190499}, {"filename": "/runtime/syntax/promela.vim", "start": 20190499, "end": 20192227}, {"filename": "/runtime/syntax/proto.vim", "start": 20192227, "end": 20195770}, {"filename": "/runtime/syntax/protocols.vim", "start": 20195770, "end": 20197199}, {"filename": "/runtime/syntax/prql.vim", "start": 20197199, "end": 20203635}, {"filename": "/runtime/syntax/ps1.vim", "start": 20203635, "end": 20211144}, {"filename": "/runtime/syntax/ps1xml.vim", "start": 20211144, "end": 20212420}, {"filename": "/runtime/syntax/psf.vim", "start": 20212420, "end": 20216710}, {"filename": "/runtime/syntax/psl.vim", "start": 20216710, "end": 20219238}, {"filename": "/runtime/syntax/ptcap.vim", "start": 20219238, "end": 20223367}, {"filename": "/runtime/syntax/ptx.vim", "start": 20223367, "end": 20225131}, {"filename": "/runtime/syntax/purifylog.vim", "start": 20225131, "end": 20228488}, {"filename": "/runtime/syntax/pymanifest.vim", "start": 20228488, "end": 20229560}, {"filename": "/runtime/syntax/pyrex.vim", "start": 20229560, "end": 20231223}, {"filename": "/runtime/syntax/python.vim", "start": 20231223, "end": 20250838}, {"filename": "/runtime/syntax/python2.vim", "start": 20250838, "end": 20266674}, {"filename": "/runtime/syntax/qb64.vim", "start": 20266674, "end": 20274639}, {"filename": "/runtime/syntax/qf.lua", "start": 20274639, "end": 20274994}, {"filename": "/runtime/syntax/qf.vim", "start": 20274994, "end": 20276027}, {"filename": "/runtime/syntax/qml.vim", "start": 20276027, "end": 20329798}, {"filename": "/runtime/syntax/quake.vim", "start": 20329798, "end": 20338352}, {"filename": "/runtime/syntax/quarto.vim", "start": 20338352, "end": 20339219}, {"filename": "/runtime/syntax/query.lua", "start": 20339219, "end": 20339326}, {"filename": "/runtime/syntax/r.vim", "start": 20339326, "end": 20354432}, {"filename": "/runtime/syntax/racc.vim", "start": 20354432, "end": 20360051}, {"filename": "/runtime/syntax/racket.vim", "start": 20360051, "end": 20395047}, {"filename": "/runtime/syntax/radiance.vim", "start": 20395047, "end": 20402202}, {"filename": "/runtime/syntax/radvd.vim", "start": 20402202, "end": 20404674}, {"filename": "/runtime/syntax/raku.vim", "start": 20404674, "end": 20480367}, {"filename": "/runtime/syntax/raml.vim", "start": 20480367, "end": 20484817}, {"filename": "/runtime/syntax/rapid.vim", "start": 20484817, "end": 20531826}, {"filename": "/runtime/syntax/rasi.vim", "start": 20531826, "end": 20544851}, {"filename": "/runtime/syntax/ratpoison.vim", "start": 20544851, "end": 20562881}, {"filename": "/runtime/syntax/rc.vim", "start": 20562881, "end": 20572905}, {"filename": "/runtime/syntax/rcs.vim", "start": 20572905, "end": 20575038}, {"filename": "/runtime/syntax/rcslog.vim", "start": 20575038, "end": 20575595}, {"filename": "/runtime/syntax/readline.vim", "start": 20575595, "end": 20593082}, {"filename": "/runtime/syntax/rebol.vim", "start": 20593082, "end": 20600249}, {"filename": "/runtime/syntax/redif.vim", "start": 20600249, "end": 20688190}, {"filename": "/runtime/syntax/registry.vim", "start": 20688190, "end": 20691545}, {"filename": "/runtime/syntax/rego.vim", "start": 20691545, "end": 20697227}, {"filename": "/runtime/syntax/remind.vim", "start": 20697227, "end": 20699940}, {"filename": "/runtime/syntax/requirements.vim", "start": 20699940, "end": 20703326}, {"filename": "/runtime/syntax/resolv.vim", "start": 20703326, "end": 20709687}, {"filename": "/runtime/syntax/reva.vim", "start": 20709687, "end": 20717278}, {"filename": "/runtime/syntax/rexx.vim", "start": 20717278, "end": 20732609}, {"filename": "/runtime/syntax/rhelp.vim", "start": 20732609, "end": 20744872}, {"filename": "/runtime/syntax/rib.vim", "start": 20744872, "end": 20747064}, {"filename": "/runtime/syntax/rmd.vim", "start": 20747064, "end": 20758124}, {"filename": "/runtime/syntax/rnc.vim", "start": 20758124, "end": 20760387}, {"filename": "/runtime/syntax/rng.vim", "start": 20760387, "end": 20761204}, {"filename": "/runtime/syntax/rnoweb.vim", "start": 20761204, "end": 20763119}, {"filename": "/runtime/syntax/robots.vim", "start": 20763119, "end": 20764649}, {"filename": "/runtime/syntax/routeros.vim", "start": 20764649, "end": 20768011}, {"filename": "/runtime/syntax/rpcgen.vim", "start": 20768011, "end": 20769773}, {"filename": "/runtime/syntax/rpl.vim", "start": 20769773, "end": 20795473}, {"filename": "/runtime/syntax/rrst.vim", "start": 20795473, "end": 20796779}, {"filename": "/runtime/syntax/rst.vim", "start": 20796779, "end": 20809565}, {"filename": "/runtime/syntax/rtf.vim", "start": 20809565, "end": 20811896}, {"filename": "/runtime/syntax/ruby.vim", "start": 20811896, "end": 20853741}, {"filename": "/runtime/syntax/rust.vim", "start": 20853741, "end": 20876526}, {"filename": "/runtime/syntax/salt.vim", "start": 20876526, "end": 20876831}, {"filename": "/runtime/syntax/samba.vim", "start": 20876831, "end": 20882953}, {"filename": "/runtime/syntax/sas.vim", "start": 20882953, "end": 20917040}, {"filename": "/runtime/syntax/sass.vim", "start": 20917040, "end": 20923452}, {"filename": "/runtime/syntax/sather.vim", "start": 20923452, "end": 20926941}, {"filename": "/runtime/syntax/sbt.vim", "start": 20926941, "end": 20927821}, {"filename": "/runtime/syntax/scala.vim", "start": 20927821, "end": 20939500}, {"filename": "/runtime/syntax/scdoc.vim", "start": 20939500, "end": 20942243}, {"filename": "/runtime/syntax/scheme.vim", "start": 20942243, "end": 20961873}, {"filename": "/runtime/syntax/scilab.vim", "start": 20961873, "end": 20965083}, {"filename": "/runtime/syntax/screen.vim", "start": 20965083, "end": 20975931}, {"filename": "/runtime/syntax/scss.vim", "start": 20975931, "end": 20976457}, {"filename": "/runtime/syntax/sd.vim", "start": 20976457, "end": 20978680}, {"filename": "/runtime/syntax/sdc.vim", "start": 20978680, "end": 20981912}, {"filename": "/runtime/syntax/sdl.vim", "start": 20981912, "end": 20988475}, {"filename": "/runtime/syntax/sed.vim", "start": 20988475, "end": 20994354}, {"filename": "/runtime/syntax/sendpr.vim", "start": 20994354, "end": 20995292}, {"filename": "/runtime/syntax/sensors.vim", "start": 20995292, "end": 20996805}, {"filename": "/runtime/syntax/services.vim", "start": 20996805, "end": 20998628}, {"filename": "/runtime/syntax/setserial.vim", "start": 20998628, "end": 21003857}, {"filename": "/runtime/syntax/sexplib.vim", "start": 21003857, "end": 21007542}, {"filename": "/runtime/syntax/sgf.vim", "start": 21007542, "end": 21009127}, {"filename": "/runtime/syntax/sgml.vim", "start": 21009127, "end": 21017998}, {"filename": "/runtime/syntax/sgmldecl.vim", "start": 21017998, "end": 21020039}, {"filename": "/runtime/syntax/sgmllnx.vim", "start": 21020039, "end": 21021890}, {"filename": "/runtime/syntax/sh.vim", "start": 21021890, "end": 21090025}, {"filename": "/runtime/syntax/shada.vim", "start": 21090025, "end": 21095615}, {"filename": "/runtime/syntax/shaderslang.vim", "start": 21095615, "end": 21125306}, {"filename": "/runtime/syntax/shared/README.txt", "start": 21125306, "end": 21125428}, {"filename": "/runtime/syntax/shared/debarchitectures.vim", "start": 21125428, "end": 21126520}, {"filename": "/runtime/syntax/shared/debversions.vim", "start": 21126520, "end": 21128984}, {"filename": "/runtime/syntax/shared/hgcommitDiff.vim", "start": 21128984, "end": 21150784}, {"filename": "/runtime/syntax/shared/typescriptcommon.vim", "start": 21150784, "end": 21288107}, {"filename": "/runtime/syntax/sicad.vim", "start": 21288107, "end": 21306582}, {"filename": "/runtime/syntax/sieve.vim", "start": 21306582, "end": 21308253}, {"filename": "/runtime/syntax/sil.vim", "start": 21308253, "end": 21316794}, {"filename": "/runtime/syntax/simula.vim", "start": 21316794, "end": 21320165}, {"filename": "/runtime/syntax/sinda.vim", "start": 21320165, "end": 21323831}, {"filename": "/runtime/syntax/sindacmp.vim", "start": 21323831, "end": 21325105}, {"filename": "/runtime/syntax/sindaout.vim", "start": 21325105, "end": 21327066}, {"filename": "/runtime/syntax/sisu.vim", "start": 21327066, "end": 21349246}, {"filename": "/runtime/syntax/skhd.vim", "start": 21349246, "end": 21354679}, {"filename": "/runtime/syntax/skill.vim", "start": 21354679, "end": 21380524}, {"filename": "/runtime/syntax/sl.vim", "start": 21380524, "end": 21384608}, {"filename": "/runtime/syntax/slang.vim", "start": 21384608, "end": 21387910}, {"filename": "/runtime/syntax/slice.vim", "start": 21387910, "end": 21390838}, {"filename": "/runtime/syntax/slpconf.vim", "start": 21390838, "end": 21402629}, {"filename": "/runtime/syntax/slpreg.vim", "start": 21402629, "end": 21407975}, {"filename": "/runtime/syntax/slpspi.vim", "start": 21407975, "end": 21409156}, {"filename": "/runtime/syntax/slrnrc.vim", "start": 21409156, "end": 21421967}, {"filename": "/runtime/syntax/slrnsc.vim", "start": 21421967, "end": 21425065}, {"filename": "/runtime/syntax/sm.vim", "start": 21425065, "end": 21428011}, {"filename": "/runtime/syntax/smarty.vim", "start": 21428011, "end": 21430894}, {"filename": "/runtime/syntax/smcl.vim", "start": 21430894, "end": 21442937}, {"filename": "/runtime/syntax/smil.vim", "start": 21442937, "end": 21448553}, {"filename": "/runtime/syntax/smith.vim", "start": 21448553, "end": 21449346}, {"filename": "/runtime/syntax/sml.vim", "start": 21449346, "end": 21459965}, {"filename": "/runtime/syntax/snnsnet.vim", "start": 21459965, "end": 21462447}, {"filename": "/runtime/syntax/snnspat.vim", "start": 21462447, "end": 21464675}, {"filename": "/runtime/syntax/snnsres.vim", "start": 21464675, "end": 21466608}, {"filename": "/runtime/syntax/snobol4.vim", "start": 21466608, "end": 21471942}, {"filename": "/runtime/syntax/solidity.vim", "start": 21471942, "end": 21494622}, {"filename": "/runtime/syntax/spajson.vim", "start": 21494622, "end": 21496514}, {"filename": "/runtime/syntax/spec.vim", "start": 21496514, "end": 21509812}, {"filename": "/runtime/syntax/specman.vim", "start": 21509812, "end": 21517161}, {"filename": "/runtime/syntax/spice.vim", "start": 21517161, "end": 21519404}, {"filename": "/runtime/syntax/splint.vim", "start": 21519404, "end": 21535190}, {"filename": "/runtime/syntax/spup.vim", "start": 21535190, "end": 21549107}, {"filename": "/runtime/syntax/spyce.vim", "start": 21549107, "end": 21553562}, {"filename": "/runtime/syntax/sql.vim", "start": 21553562, "end": 21554682}, {"filename": "/runtime/syntax/sqlanywhere.vim", "start": 21554682, "end": 21596611}, {"filename": "/runtime/syntax/sqlforms.vim", "start": 21596611, "end": 21603345}, {"filename": "/runtime/syntax/sqlhana.vim", "start": 21603345, "end": 21614372}, {"filename": "/runtime/syntax/sqlinformix.vim", "start": 21614372, "end": 21620274}, {"filename": "/runtime/syntax/sqlj.vim", "start": 21620274, "end": 21623244}, {"filename": "/runtime/syntax/sqloracle.vim", "start": 21623244, "end": 21631008}, {"filename": "/runtime/syntax/sqr.vim", "start": 21631008, "end": 21644020}, {"filename": "/runtime/syntax/squid.vim", "start": 21644020, "end": 21650951}, {"filename": "/runtime/syntax/squirrel.vim", "start": 21650951, "end": 21652451}, {"filename": "/runtime/syntax/srec.vim", "start": 21652451, "end": 21656266}, {"filename": "/runtime/syntax/srt.vim", "start": 21656266, "end": 21658406}, {"filename": "/runtime/syntax/ssa.vim", "start": 21658406, "end": 21660761}, {"filename": "/runtime/syntax/sshallowedsigners.vim", "start": 21660761, "end": 21662368}, {"filename": "/runtime/syntax/sshauthorizedkeys.vim", "start": 21662368, "end": 21663527}, {"filename": "/runtime/syntax/sshconfig.vim", "start": 21663527, "end": 21677046}, {"filename": "/runtime/syntax/sshdconfig.vim", "start": 21677046, "end": 21691078}, {"filename": "/runtime/syntax/sshknownhosts.vim", "start": 21691078, "end": 21692365}, {"filename": "/runtime/syntax/sshpublickey.vim", "start": 21692365, "end": 21693850}, {"filename": "/runtime/syntax/st.vim", "start": 21693850, "end": 21696604}, {"filename": "/runtime/syntax/stata.vim", "start": 21696604, "end": 21723660}, {"filename": "/runtime/syntax/stp.vim", "start": 21723660, "end": 21729087}, {"filename": "/runtime/syntax/strace.vim", "start": 21729087, "end": 21731123}, {"filename": "/runtime/syntax/structurizr.vim", "start": 21731123, "end": 21734357}, {"filename": "/runtime/syntax/stylus.vim", "start": 21734357, "end": 21737562}, {"filename": "/runtime/syntax/sudoers.vim", "start": 21737562, "end": 21776640}, {"filename": "/runtime/syntax/svelte.vim", "start": 21776640, "end": 21782531}, {"filename": "/runtime/syntax/svg.vim", "start": 21782531, "end": 21782967}, {"filename": "/runtime/syntax/svn.vim", "start": 21782967, "end": 21784819}, {"filename": "/runtime/syntax/swayconfig.vim", "start": 21784819, "end": 21799808}, {"filename": "/runtime/syntax/swift.vim", "start": 21799808, "end": 21808636}, {"filename": "/runtime/syntax/swiftgyb.vim", "start": 21808636, "end": 21809458}, {"filename": "/runtime/syntax/swig.vim", "start": 21809458, "end": 21813973}, {"filename": "/runtime/syntax/synload.vim", "start": 21813973, "end": 21816019}, {"filename": "/runtime/syntax/syntax.vim", "start": 21816019, "end": 21817473}, {"filename": "/runtime/syntax/sysctl.vim", "start": 21817473, "end": 21818626}, {"filename": "/runtime/syntax/systemd.vim", "start": 21818626, "end": 21818810}, {"filename": "/runtime/syntax/systemverilog.vim", "start": 21818810, "end": 21822565}, {"filename": "/runtime/syntax/tads.vim", "start": 21822565, "end": 21829427}, {"filename": "/runtime/syntax/tags.vim", "start": 21829427, "end": 21830678}, {"filename": "/runtime/syntax/tak.vim", "start": 21830678, "end": 21833598}, {"filename": "/runtime/syntax/takcmp.vim", "start": 21833598, "end": 21835191}, {"filename": "/runtime/syntax/takout.vim", "start": 21835191, "end": 21837191}, {"filename": "/runtime/syntax/tap.vim", "start": 21837191, "end": 21841331}, {"filename": "/runtime/syntax/tar.vim", "start": 21841331, "end": 21841734}, {"filename": "/runtime/syntax/taskdata.vim", "start": 21841734, "end": 21842941}, {"filename": "/runtime/syntax/taskedit.vim", "start": 21842941, "end": 21844071}, {"filename": "/runtime/syntax/tasm.vim", "start": 21844071, "end": 21849994}, {"filename": "/runtime/syntax/tcl.vim", "start": 21849994, "end": 21867268}, {"filename": "/runtime/syntax/tcsh.vim", "start": 21867268, "end": 21880849}, {"filename": "/runtime/syntax/template.vim", "start": 21880849, "end": 21881364}, {"filename": "/runtime/syntax/tera.vim", "start": 21881364, "end": 21885855}, {"filename": "/runtime/syntax/teraterm.vim", "start": 21885855, "end": 21890835}, {"filename": "/runtime/syntax/terminfo.vim", "start": 21890835, "end": 21895638}, {"filename": "/runtime/syntax/terraform.vim", "start": 21895638, "end": 21896804}, {"filename": "/runtime/syntax/tex.vim", "start": 21896804, "end": 21966211}, {"filename": "/runtime/syntax/texinfo.vim", "start": 21966211, "end": 21967795}, {"filename": "/runtime/syntax/texmf.vim", "start": 21967795, "end": 21970076}, {"filename": "/runtime/syntax/tf.vim", "start": 21970076, "end": 21976981}, {"filename": "/runtime/syntax/thrift.vim", "start": 21976981, "end": 21979557}, {"filename": "/runtime/syntax/tiasm.vim", "start": 21979557, "end": 21982567}, {"filename": "/runtime/syntax/tidy.vim", "start": 21982567, "end": 21996126}, {"filename": "/runtime/syntax/tilde.vim", "start": 21996126, "end": 21997665}, {"filename": "/runtime/syntax/tli.vim", "start": 21997665, "end": 21999323}, {"filename": "/runtime/syntax/tmux.vim", "start": 21999323, "end": 22008965}, {"filename": "/runtime/syntax/tolk.vim", "start": 22008965, "end": 22009982}, {"filename": "/runtime/syntax/toml.vim", "start": 22009982, "end": 22013106}, {"filename": "/runtime/syntax/tpp.vim", "start": 22013106, "end": 22016522}, {"filename": "/runtime/syntax/trasys.vim", "start": 22016522, "end": 22021555}, {"filename": "/runtime/syntax/treetop.vim", "start": 22021555, "end": 22025223}, {"filename": "/runtime/syntax/trustees.vim", "start": 22025223, "end": 22026805}, {"filename": "/runtime/syntax/tsalt.vim", "start": 22026805, "end": 22035129}, {"filename": "/runtime/syntax/tsscl.vim", "start": 22035129, "end": 22040249}, {"filename": "/runtime/syntax/tssgm.vim", "start": 22040249, "end": 22043036}, {"filename": "/runtime/syntax/tssop.vim", "start": 22043036, "end": 22044821}, {"filename": "/runtime/syntax/tsv.vim", "start": 22044821, "end": 22045119}, {"filename": "/runtime/syntax/tt2.vim", "start": 22045119, "end": 22053074}, {"filename": "/runtime/syntax/tt2html.vim", "start": 22053074, "end": 22053692}, {"filename": "/runtime/syntax/tt2js.vim", "start": 22053692, "end": 22054284}, {"filename": "/runtime/syntax/tutor.lua", "start": 22054284, "end": 22054334}, {"filename": "/runtime/syntax/tutor.vim", "start": 22054334, "end": 22057776}, {"filename": "/runtime/syntax/typescript.vim", "start": 22057776, "end": 22059410}, {"filename": "/runtime/syntax/typescriptreact.vim", "start": 22059410, "end": 22063505}, {"filename": "/runtime/syntax/typst.vim", "start": 22063505, "end": 22072884}, {"filename": "/runtime/syntax/uc.vim", "start": 22072884, "end": 22079518}, {"filename": "/runtime/syntax/uci.vim", "start": 22079518, "end": 22080753}, {"filename": "/runtime/syntax/udevconf.vim", "start": 22080753, "end": 22081980}, {"filename": "/runtime/syntax/udevperm.vim", "start": 22081980, "end": 22084389}, {"filename": "/runtime/syntax/udevrules.vim", "start": 22084389, "end": 22091349}, {"filename": "/runtime/syntax/uil.vim", "start": 22091349, "end": 22094170}, {"filename": "/runtime/syntax/unison.vim", "start": 22094170, "end": 22100051}, {"filename": "/runtime/syntax/updatedb.vim", "start": 22100051, "end": 22101271}, {"filename": "/runtime/syntax/upstart.vim", "start": 22101271, "end": 22104422}, {"filename": "/runtime/syntax/upstreamdat.vim", "start": 22104422, "end": 22118470}, {"filename": "/runtime/syntax/upstreaminstalllog.vim", "start": 22118470, "end": 22119441}, {"filename": "/runtime/syntax/upstreamlog.vim", "start": 22119441, "end": 22122061}, {"filename": "/runtime/syntax/upstreamrpt.vim", "start": 22122061, "end": 22136258}, {"filename": "/runtime/syntax/urlshortcut.vim", "start": 22136258, "end": 22136606}, {"filename": "/runtime/syntax/usserverlog.vim", "start": 22136606, "end": 22139724}, {"filename": "/runtime/syntax/usw2kagtlog.vim", "start": 22139724, "end": 22142426}, {"filename": "/runtime/syntax/valgrind.vim", "start": 22142426, "end": 22145642}, {"filename": "/runtime/syntax/vb.vim", "start": 22145642, "end": 22166764}, {"filename": "/runtime/syntax/vdf.vim", "start": 22166764, "end": 22168165}, {"filename": "/runtime/syntax/vera.vim", "start": 22168165, "end": 22187717}, {"filename": "/runtime/syntax/verilog.vim", "start": 22187717, "end": 22192772}, {"filename": "/runtime/syntax/verilogams.vim", "start": 22192772, "end": 22198855}, {"filename": "/runtime/syntax/vgrindefs.vim", "start": 22198855, "end": 22200135}, {"filename": "/runtime/syntax/vhdl.vim", "start": 22200135, "end": 22209793}, {"filename": "/runtime/syntax/vim.vim", "start": 22209793, "end": 22353624}, {"filename": "/runtime/syntax/vim/generated.vim", "start": 22353624, "end": 22385612}, {"filename": "/runtime/syntax/viminfo.vim", "start": 22385612, "end": 22386919}, {"filename": "/runtime/syntax/vimnormal.vim", "start": 22386919, "end": 22387627}, {"filename": "/runtime/syntax/virata.vim", "start": 22387627, "end": 22399240}, {"filename": "/runtime/syntax/vmasm.vim", "start": 22399240, "end": 22407752}, {"filename": "/runtime/syntax/voscm.vim", "start": 22407752, "end": 22414727}, {"filename": "/runtime/syntax/vrml.vim", "start": 22414727, "end": 22427515}, {"filename": "/runtime/syntax/vroom.vim", "start": 22427515, "end": 22431007}, {"filename": "/runtime/syntax/vsejcl.vim", "start": 22431007, "end": 22432011}, {"filename": "/runtime/syntax/vue.vim", "start": 22432011, "end": 22432444}, {"filename": "/runtime/syntax/wat.vim", "start": 22432444, "end": 22437310}, {"filename": "/runtime/syntax/wdiff.vim", "start": 22437310, "end": 22438026}, {"filename": "/runtime/syntax/wdl.vim", "start": 22438026, "end": 22439672}, {"filename": "/runtime/syntax/web.vim", "start": 22439672, "end": 22440786}, {"filename": "/runtime/syntax/webmacro.vim", "start": 22440786, "end": 22443871}, {"filename": "/runtime/syntax/wget.vim", "start": 22443871, "end": 22448100}, {"filename": "/runtime/syntax/wget2.vim", "start": 22448100, "end": 22452578}, {"filename": "/runtime/syntax/whitespace.vim", "start": 22452578, "end": 22452935}, {"filename": "/runtime/syntax/winbatch.vim", "start": 22452935, "end": 22465087}, {"filename": "/runtime/syntax/wks.vim", "start": 22465087, "end": 22465805}, {"filename": "/runtime/syntax/wml.vim", "start": 22465805, "end": 22471536}, {"filename": "/runtime/syntax/wsh.vim", "start": 22471536, "end": 22472778}, {"filename": "/runtime/syntax/wsml.vim", "start": 22472778, "end": 22478178}, {"filename": "/runtime/syntax/wvdial.vim", "start": 22478178, "end": 22479271}, {"filename": "/runtime/syntax/xbl.vim", "start": 22479271, "end": 22479839}, {"filename": "/runtime/syntax/xcompose.vim", "start": 22479839, "end": 22481122}, {"filename": "/runtime/syntax/xdefaults.vim", "start": 22481122, "end": 22487009}, {"filename": "/runtime/syntax/xf86conf.vim", "start": 22487009, "end": 22502109}, {"filename": "/runtime/syntax/xhtml.vim", "start": 22502109, "end": 22502295}, {"filename": "/runtime/syntax/xinetd.vim", "start": 22502295, "end": 22516503}, {"filename": "/runtime/syntax/xkb.vim", "start": 22516503, "end": 22519816}, {"filename": "/runtime/syntax/xmath.vim", "start": 22519816, "end": 22530742}, {"filename": "/runtime/syntax/xml.vim", "start": 22530742, "end": 22539613}, {"filename": "/runtime/syntax/xmodmap.vim", "start": 22539613, "end": 22586124}, {"filename": "/runtime/syntax/xpm.vim", "start": 22586124, "end": 22591107}, {"filename": "/runtime/syntax/xpm2.vim", "start": 22591107, "end": 22595804}, {"filename": "/runtime/syntax/xquery.vim", "start": 22595804, "end": 22602995}, {"filename": "/runtime/syntax/xs.vim", "start": 22602995, "end": 22849947}, {"filename": "/runtime/syntax/xsd.vim", "start": 22849947, "end": 22851152}, {"filename": "/runtime/syntax/xslt.vim", "start": 22851152, "end": 22852581}, {"filename": "/runtime/syntax/xxd.vim", "start": 22852581, "end": 22853443}, {"filename": "/runtime/syntax/yacc.vim", "start": 22853443, "end": 22859117}, {"filename": "/runtime/syntax/yaml.vim", "start": 22859117, "end": 22875411}, {"filename": "/runtime/syntax/z8a.vim", "start": 22875411, "end": 22878305}, {"filename": "/runtime/syntax/zathurarc.vim", "start": 22878305, "end": 22882512}, {"filename": "/runtime/syntax/zig.vim", "start": 22882512, "end": 22893542}, {"filename": "/runtime/syntax/zimbu.vim", "start": 22893542, "end": 22900056}, {"filename": "/runtime/syntax/zip.lua", "start": 22900056, "end": 22900281}, {"filename": "/runtime/syntax/zir.vim", "start": 22900281, "end": 22901892}, {"filename": "/runtime/syntax/zserio.vim", "start": 22901892, "end": 22906286}, {"filename": "/runtime/syntax/zsh.vim", "start": 22906286, "end": 22929928}, {"filename": "/runtime/tutor/en/vim-01-beginner.tutor", "start": 22929928, "end": 22964889}, {"filename": "/runtime/tutor/en/vim-02-beginner.tutor", "start": 22964889, "end": 22975410}, {"filename": "/runtime/tutor/ja/vim-01-beginner.tutor", "start": 22975410, "end": 23022918}, {"filename": "/runtime/tutor/ja/vim-02-beginner.tutor", "start": 23022918, "end": 23030958}, {"filename": "/runtime/tutor/tutor.tutor", "start": 23030958, "end": 23037011}, {"filename": "/runtime/tutor/zh/vim-01-beginner.tutor", "start": 23037011, "end": 23072701}, {"filename": "/runtime/tutor/zh/vim-02-beginner.tutor", "start": 23072701, "end": 23079325}, {"filename": "/runtime/windows_icon.rc", "start": 23079325, "end": 23079553}], "remote_package_size": 23079553});

  })();

// end include: /tmp/tmp8_eg_1yy.js
// include: /tmp/tmpu14khby8.js

    // All the pre-js content up to here must remain later on, we need to run
    // it.
    if ((typeof ENVIRONMENT_IS_WASM_WORKER != 'undefined' && ENVIRONMENT_IS_WASM_WORKER) || (typeof ENVIRONMENT_IS_PTHREAD != 'undefined' && ENVIRONMENT_IS_PTHREAD) || (typeof ENVIRONMENT_IS_AUDIO_WORKLET != 'undefined' && ENVIRONMENT_IS_AUDIO_WORKLET)) Module['preRun'] = [];
    var necessaryPreJSTasks = Module['preRun'].slice();
  // end include: /tmp/tmpu14khby8.js
// include: /tmp/tmph_3m0_q7.js

    if (!Module['preRun']) throw 'Module.preRun should exist because file support used it; did a pre-js delete it?';
    necessaryPreJSTasks.forEach((task) => {
      if (Module['preRun'].indexOf(task) < 0) throw 'All preRun tasks that exist before user pre-js code should remain after; did you replace Module or modify Module.preRun?';
    });
  // end include: /tmp/tmph_3m0_q7.js


var programArgs = [];
var thisProgram = './this.program';
var quit_ = (status, toThrow) => {
  throw toThrow;
};

if (typeof __filename != 'undefined') { // Node
  _scriptName = __filename;
} else
if (ENVIRONMENT_IS_WORKER) {
  _scriptName = self.location.href;
}

// `/` should be present at the end if `scriptDirectory` is not empty
var scriptDirectory = '';
function locateFile(path) {
  if (Module['locateFile']) {
    return Module['locateFile'](path, scriptDirectory);
  }
  return scriptDirectory + path;
}

// Hooks that are implemented differently in different runtime environments.
var readAsync, readBinary;

if (ENVIRONMENT_IS_NODE) {
  const isNode = globalThis.process?.versions?.node && globalThis.process?.type != 'renderer';
  if (!isNode) throw new Error('not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)');

  // These modules will usually be used on Node.js. Load them eagerly to avoid
  // the complexity of lazy-loading.
  var fs = require('node:fs');

  scriptDirectory = __dirname + '/';

// include: node_shell_read.js
readBinary = (filename) => {
  // We need to re-wrap `file://` strings to URLs.
  filename = isFileURI(filename) ? new URL(filename) : filename;
  var ret = fs.readFileSync(filename);
  assert(Buffer.isBuffer(ret));
  return ret;
};

readAsync = async (filename, binary = true) => {
  // See the comment in the `readBinary` function.
  filename = isFileURI(filename) ? new URL(filename) : filename;
  var ret = fs.readFileSync(filename, binary ? undefined : 'utf8');
  assert(binary ? Buffer.isBuffer(ret) : typeof ret == 'string');
  return ret;
};
// end include: node_shell_read.js
  if (process.argv.length > 1) {
    thisProgram = process.argv[1].replace(/\\/g, '/');
  }

  programArgs = process.argv.slice(2);

  quit_ = (status, toThrow) => {
    process.exitCode = status;
    throw toThrow;
  };

} else
if (ENVIRONMENT_IS_SHELL) {

} else

// Note that this includes Node.js workers when relevant (pthreads is enabled).
// Node.js workers are detected as a combination of ENVIRONMENT_IS_WORKER and
// ENVIRONMENT_IS_NODE.
if (ENVIRONMENT_IS_WEB || ENVIRONMENT_IS_WORKER) {
  try {
    scriptDirectory = new URL('.', _scriptName).href; // includes trailing slash
  } catch {
    // Must be a `blob:` or `data:` URL (e.g. `blob:http://site.com/etc/etc`), we cannot
    // infer anything from them.
  }

  if (!(globalThis.window || globalThis.WorkerGlobalScope)) throw new Error('not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)');

  // Differentiate the Web Worker from the Node Worker case, as reading must
  // be done differently.
  if (!ENVIRONMENT_IS_NODE)
  {
// include: web_or_worker_shell_read.js
if (ENVIRONMENT_IS_WORKER) {
    readBinary = (url) => {
      var xhr = new XMLHttpRequest();
      xhr.open('GET', url, false);
      xhr.responseType = 'arraybuffer';
      xhr.send(null);
      return new Uint8Array(/** @type{!ArrayBuffer} */(xhr.response));
    };
  }

  readAsync = async (url) => {
    // Fetch has some additional restrictions over XHR, like it can't be used on a file:// url.
    // See https://github.com/github/fetch/pull/92#issuecomment-140665932
    // Cordova or Electron apps are typically loaded from a file:// url.
    // So use XHR on webview if URL is a file URL.
    if (isFileURI(url)) {
      return new Promise((resolve, reject) => {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', url, true);
        xhr.responseType = 'arraybuffer';
        xhr.onload = () => {
          if (xhr.status == 200 || (xhr.status == 0 && xhr.response)) { // file URLs can return 0
            resolve(xhr.response);
            return;
          }
          reject(xhr.status);
        };
        xhr.onerror = reject;
        xhr.send(null);
      });
    }
    var response = await fetch(url, { credentials: 'same-origin' });
    if (response.ok) {
      return response.arrayBuffer();
    }
    throw new Error(response.status + ' : ' + response.url);
  };
// end include: web_or_worker_shell_read.js
  }
} else
{
  throw new Error('environment detection error');
}

// Set up the out() and err() hooks, which are how we can print to stdout or
// stderr, respectively.
// Normally just binding console.log/console.error here works fine, but
// under node (with workers) we see missing/out-of-order messages so route
// directly to stdout and stderr.
// See https://github.com/emscripten-core/emscripten/issues/14804
var defaultPrint = console.log.bind(console);
var defaultPrintErr = console.error.bind(console);
if (ENVIRONMENT_IS_NODE) {
  var utils = require('node:util');
  var stringify = (a) => typeof a == 'object' ? utils.inspect(a) : a;
  defaultPrint = (...args) => fs.writeSync(1, args.map(stringify).join(' ') + '\n');
  defaultPrintErr = (...args) => fs.writeSync(2, args.map(stringify).join(' ') + '\n');
}
var out = defaultPrint;
var err = defaultPrintErr;


var PROXYFS = 'PROXYFS is no longer included by default; build with -lproxyfs.js';
var WORKERFS = 'WORKERFS is no longer included by default; build with -lworkerfs.js';
var FETCHFS = 'FETCHFS is no longer included by default; build with -lfetchfs.js';
var ICASEFS = 'ICASEFS is no longer included by default; build with -licasefs.js';
var JSFILEFS = 'JSFILEFS is no longer included by default; build with -ljsfilefs.js';
var OPFS = 'OPFS is no longer included by default; build with -lopfs.js';

var NODEFS = 'NODEFS is no longer included by default; build with -lnodefs.js';

// perform assertions in shell.js after we set up out() and err(), as otherwise
// if an assertion fails it cannot print the message
assert(
  ENVIRONMENT_IS_WEB || ENVIRONMENT_IS_WORKER || ENVIRONMENT_IS_NODE, 'pthreads do not work in this environment yet (need Web Workers, or an alternative to them)');

assert(!ENVIRONMENT_IS_SHELL, 'shell environment detected but not enabled at build time (add `shell` to `-sENVIRONMENT` to enable)');

// end include: shell.js

// include: preamble.js
// === Preamble library stuff ===

// Documentation for the public APIs defined in this file must be updated in:
//    site/source/docs/api_reference/preamble.js.rst
// A prebuilt local version of the documentation is available at:
//    site/build/text/docs/api_reference/preamble.js.txt
// You can also build docs locally as HTML or other formats in site/
// An online HTML version (which may be of a different version of Emscripten)
//    is up at http://kripken.github.io/emscripten-site/docs/api_reference/preamble.js.html

var wasmBinary;

if (!globalThis.WebAssembly) {
  err('no native wasm support detected');
}

// Wasm globals

// For sending to workers.
var wasmModule;

//========================================
// Runtime essentials
//========================================

// whether we are quitting the application. no code should run after this.
// set in exit() and abort()
var ABORT = false;

// set by exit() and abort().  Passed to 'onExit' handler.
// NOTE: This is also used as the process return code in shell environments
// but only when noExitRuntime is false.
var EXITSTATUS;

// In STRICT mode, we only define assert() when ASSERTIONS is set.  i.e. we
// don't define it at all in release modes.  This matches the behaviour of
// MINIMAL_RUNTIME.
// TODO(sbc): Make this the default even without STRICT enabled.
/** @type {function(*, string=)} */
function assert(condition, text) {
  if (!condition) {
    abort('Assertion failed' + (text ? ': ' + text : ''));
  }
}

// We used to include malloc/free by default in the past. Show a helpful error in
// builds with assertions.

/**
 * Indicates whether filename is delivered via file protocol (as opposed to http/https)
 * @noinline
 */
var isFileURI = (filename) => filename.startsWith('file://');

// include: runtime_common.js
// include: runtime_stack_check.js
// Initializes the stack cookie. Called at the startup of main and at the startup of each thread in pthreads mode.
function writeStackCookie() {
  var max = _emscripten_stack_get_end();
  assert((max & 3) == 0);
  // If the stack ends at address zero we write our cookies 4 bytes into the
  // stack.  This prevents interference with SAFE_HEAP and ASAN which also
  // monitor writes to address zero.
  if (max == 0) {
    max += 4;
  }
  // The stack grow downwards towards _emscripten_stack_get_end.
  // We write cookies to the final two words in the stack and detect if they are
  // ever overwritten.
  HEAPU32[((max)>>2)] = 0x02135467;
  HEAPU32[(((max)+(4))>>2)] = 0x89BACDFE;
  // Also test the global address 0 for integrity.
  HEAPU32[((0)>>2)] = 1668509029;
}

function checkStackCookie() {
  if (ABORT) return;
  var max = _emscripten_stack_get_end();
  // See writeStackCookie().
  if (max == 0) {
    max += 4;
  }
  var cookie1 = HEAPU32[((max)>>2)];
  var cookie2 = HEAPU32[(((max)+(4))>>2)];
  if (cookie1 != 0x02135467 || cookie2 != 0x89BACDFE) {
    abort(`Stack overflow! Stack cookie has been overwritten at ${ptrToString(max)}, expected hex dwords 0x89BACDFE and 0x2135467, but received ${ptrToString(cookie2)} ${ptrToString(cookie1)}`);
  }
  // Also test the global address 0 for integrity.
  if (HEAPU32[((0)>>2)] != 0x63736d65 /* 'emsc' */) {
    abort('Runtime error: The application has corrupted its heap memory area (address zero)!');
  }
}
// end include: runtime_stack_check.js
// include: runtime_exceptions.js
// Base Emscripten EH error class
class EmscriptenEH extends Error {}

class EmscriptenSjLj extends EmscriptenEH {}

class CppException extends EmscriptenEH {
  constructor(excPtr) {
    super(excPtr);
    this.excPtr = excPtr;
    const excInfo = getExceptionMessage(this);
    this.name = excInfo[0];
    this.message = excInfo[1];
  }
}

// end include: runtime_exceptions.js
// include: runtime_debug.js
var runtimeDebug = true; // Switch to false at runtime to disable logging at the right times

// Used by XXXXX_DEBUG settings to output debug messages.
function dbg(...args) {
  if (!runtimeDebug && typeof runtimeDebug != 'undefined') return;
  // Avoid using the console for debugging in multi-threaded node applications
  // See https://github.com/emscripten-core/emscripten/issues/14804
  if (ENVIRONMENT_IS_NODE) {
    // TODO(sbc): Unify with err/out implementation in shell.sh.
    var fs = require('node:fs');
    var utils = require('node:util');
    function stringify(a) {
      switch (typeof a) {
        case 'object': return utils.inspect(a);
        case 'undefined': return 'undefined';
      }
      return a;
    }
    fs.writeSync(2, args.map(stringify).join(' ') + '\n');
  } else
  // TODO(sbc): Make this configurable somehow.  Its not always convenient for
  // logging to show up as warnings.
  console.warn(...args);
}

// Endianness check
(() => {
  var h16 = new Int16Array(1);
  var h8 = new Int8Array(h16.buffer);
  h16[0] = 0x6373;
  if (h8[0] !== 0x73 || h8[1] !== 0x63) abort('Runtime error: expected the system to be little-endian! (Run with -sSUPPORT_BIG_ENDIAN to bypass)');
})();

function consumedModuleProp(prop) {
  var value = Module[prop];
  var msg = `Attempt to modify \`Module.${prop}\` after it has already been processed.  This can happen, for example, when code is injected via '--post-js' rather than '--pre-js'`;
  if (Array.isArray(value)) {
    value = new Proxy(value, {
      set(target, key, val) {
        abort(msg);
        return false;
      },
      defineProperty(target, key, descriptor) {
        abort(msg);
        return false;
      },
      deleteProperty(target, key) {
        abort(msg);
        return false;
      }
    });
  }
  Object.defineProperty(Module, prop, {
    configurable: true,
    get() { return value; },
    set() {
      abort(msg);
    }
  });
}

function makeInvalidEarlyAccess(name) {
  return () => assert(false, `call to '${name}' via reference taken before Wasm module initialization`);

}

function ignoredModuleProp(prop) {
  if (Object.getOwnPropertyDescriptor(Module, prop)) {
    abort(`\`Module.${prop}\` was supplied but \`${prop}\` not included in INCOMING_MODULE_JS_API`);
  }
}

// forcing the filesystem exports a few things by default
function isExportedByForceFilesystem(name) {
  return name === 'FS_createPath' ||
         name === 'FS_createDataFile' ||
         name === 'FS_createPreloadedFile' ||
         name === 'FS_preloadFile' ||
         name === 'FS_unlink' ||
         name === 'addRunDependency' ||
         // The old FS has some functionality that WasmFS lacks.
         name === 'FS_createLazyFile' ||
         name === 'FS_createDevice' ||
         name === 'removeRunDependency';
}

function missingLibrarySymbol(sym) {

  // Any symbol that is not included from the JS library is also (by definition)
  // not exported on the Module object.
  unexportedRuntimeSymbol(sym);
}

function unexportedRuntimeSymbol(sym) {
  if (ENVIRONMENT_IS_PTHREAD) {
    return;
  }
  if (!Object.getOwnPropertyDescriptor(Module, sym)) {
    Object.defineProperty(Module, sym, {
      configurable: true,
      get() {
        var msg = `'${sym}' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the Emscripten FAQ)`;
        if (isExportedByForceFilesystem(sym)) {
          msg += '. Alternatively, forcing filesystem support (-sFORCE_FILESYSTEM) can export this for you';
        }
        abort(msg);
      },
    });
  }
}

/**
 * Override `err`/`out`/`dbg` to report thread / worker information
 */
function initWorkerLogging() {
  function getLogPrefix() {
    var t = 0;
    if (runtimeInitialized && typeof _pthread_self != 'undefined'
    ) {
      t = _pthread_self();
    }
    return `w:${workerID},t:${ptrToString(t)}:`;
  }

  // Prefix all dbg() messages with the calling thread info.
  var origDbg = dbg;
  dbg = (...args) => origDbg(getLogPrefix(), ...args);
}

initWorkerLogging();

// end include: runtime_debug.js
if (ENVIRONMENT_IS_NODE && (ENVIRONMENT_IS_PTHREAD)) {
  // Create as web-worker-like an environment as we can.
  globalThis.self = globalThis;
  var parentPort = worker_threads.parentPort;
  // Deno and Bun already have `postMessage` defined on the global scope and
  // deliver messages to `globalThis.onmessage`, so we must not duplicate that
  // behavior here if `postMessage` is already present.
  if (!globalThis.postMessage) {
    parentPort.on('message', (msg) => globalThis.onmessage?.({ data: msg }));
    globalThis.postMessage = (msg) => parentPort.postMessage(msg);
  }
  // Node.js Workers do not pass postMessage()s and uncaught exception events to the parent
  // thread necessarily in the same order where they were generated in sequential program order.
  // See https://github.com/nodejs/node/issues/59617
  // To remedy this, capture all uncaughtExceptions in the Worker, and sequentialize those over
  // to the same postMessage pipe that other messages use.
  process.on("uncaughtException", (err) => {
    postMessage({ cmd: 8, error: err });
    // Also shut down the Worker to match the same semantics as if this uncaughtException
    // handler was not registered.
    // (n.b. this will not shut down the whole Node.js app process, but just the Worker)
    process.exit(1);
  });
}

// include: runtime_pthread.js
// Pthread Web Worker handling code.
// This code runs only on pthread web workers and handles pthread setup
// and communication with the main thread via postMessage.

// Unique ID of the current pthread worker (zero on non-pthread-workers
// including the main thread).
var workerID = 0;

var startWorker;

if (ENVIRONMENT_IS_PTHREAD) {
  // Thread-local guard variable for one-time init of the JS state
  var initializedJS = false;

  // Turn unhandled rejected promises into errors so that the main thread will be
  // notified about them.
  self.onunhandledrejection = (e) => { throw e.reason || e; };

  function handleMessage(e) {
    try {
      var msgData = e.data;
      //dbg('msgData: ' + Object.keys(msgData));
      var cmd = msgData.cmd;
      if (cmd == 1) { // Preload command that is called once per worker to parse and load the Emscripten code.
        workerID = msgData.workerID;

        // Until we initialize the runtime, queue up any further incoming messages.
        let messageQueue = [];
        self.onmessage = (e) => messageQueue.push(e);

        // And add a callback for when the runtime is initialized.
        startWorker = () => {
          // Notify the main thread that this thread has loaded.
          postMessage({ cmd: 3 });
          // Process any messages that were queued before the thread was ready.
          for (let msg of messageQueue) {
            handleMessage(msg);
          }
          // Restore the real message handler.
          self.onmessage = handleMessage;
        };

        // Use `const` here to ensure that the variable is scoped only to
        // that iteration, allowing safe reference from a closure.
        for (const handler of msgData.handlers) {
          // If the main module has a handler for a certain event, but no
          // handler exists on the pthread worker, then proxy that handler
          // back to the main thread.
          if (!Module[handler] || Module[handler].proxy) {
            Module[handler] = (...args) => {
              postMessage({ cmd: 9, handler, args: args });
            }
            // Rebind the out / err handlers if needed
            if (handler == 'print') out = Module[handler];
            if (handler == 'printErr') err = Module[handler];
          }
        }

        wasmMemory = msgData.wasmMemory;
        updateMemoryViews();

        wasmModule = msgData.wasmModule;
        createWasm();
        run();
      } else if (cmd == 2) {
        assert(msgData.pthread_ptr);
        assert(wasmMemory, "CMD_RUN received before CMD_LOAD");
        // Call inside JS module to set up the stack frame for this pthread in JS module scope.
        // This needs to be the first thing that we do, as we cannot call to any C/C++ functions
        // until the thread stack is initialized.
        establishStackSpace(msgData.pthread_ptr);

        // Pass the thread address to wasm to store it for fast access.
        __emscripten_thread_init(msgData.pthread_ptr, /*is_main=*/0, /*is_runtime=*/0, /*can_block=*/1, 0, 0);

        PThread.threadInitTLS();

        // Await mailbox notifications with `Atomics.waitAsync` so we can start
        // using the fast `Atomics.notify` notification path.
        __emscripten_thread_mailbox_await(msgData.pthread_ptr);

        if (!initializedJS) {
          initializedJS = true;
        }

        try {
          invokeEntryPoint(msgData.start_routine, msgData.arg);
        } catch(ex) {
          if (ex != 'unwind') {
            // The pthread "crashed".  Do not call `_emscripten_thread_exit` (which
            // would make this thread joinable).  Instead, re-throw the exception
            // and let the top level handler propagate it back to the main thread.
            throw ex;
          }
        }
      } else if (cmd == 4) {
        if (initializedJS) {
          checkMailbox();
        }
      } else if (cmd) {
        // The received message looks like something that should be handled by this message
        // handler, (since there is a cmd field present), but is not one of the
        // recognized commands:
        err(`worker: received unknown command ${cmd}`);
        err(msgData);
      }
    } catch(ex) {
      err(`worker: onmessage() captured an uncaught exception: ${ex}`);
      if (ex?.stack) err(ex.stack);
      if (runtimeInitialized) __emscripten_thread_crashed();
      throw ex;
    }
  };

  self.onmessage = handleMessage;

} // ENVIRONMENT_IS_PTHREAD
// end include: runtime_pthread.js
// Memory management

var runtimeInitialized = false;



function updateMemoryViews() {
  // When memory growth is disabled this function should be called exactly once.
  assert(!HEAP8, 'updateMemoryViews should only be called once when ALLOW_MEMORY_GROWTH=0');
  var b = wasmMemory.buffer;
  HEAP8 = new Int8Array(b);
  HEAP16 = new Int16Array(b);
  Module['HEAPU8'] = HEAPU8 = new Uint8Array(b);
  HEAPU16 = new Uint16Array(b);
  HEAP32 = new Int32Array(b);
  HEAPU32 = new Uint32Array(b);
  HEAPF32 = new Float32Array(b);
  HEAPF64 = new Float64Array(b);
  HEAP64 = new BigInt64Array(b);
  HEAPU64 = new BigUint64Array(b);
}

// In non-standalone/normal mode, we create the memory here.
// include: runtime_init_memory.js
// Create the wasm memory. (Note: this only applies if IMPORTED_MEMORY is defined)

// check for full engine support (use string 'subarray' to avoid closure compiler confusion)

function initMemory() {

  if ((ENVIRONMENT_IS_PTHREAD)) { return }

  {
    var INITIAL_MEMORY = 268435456;

    assert(INITIAL_MEMORY >= 8388608, `INITIAL_MEMORY should be larger than STACK_SIZE, was ${INITIAL_MEMORY}! (STACK_SIZE=8388608)`);
    /** @suppress {checkTypes} */
    wasmMemory = new WebAssembly.Memory({
      'initial': INITIAL_MEMORY / 65536,
      'maximum': INITIAL_MEMORY / 65536,
      'shared': true,
    });
  }

  updateMemoryViews();
}

// end include: runtime_init_memory.js

// include: memoryprofiler.js
// end include: memoryprofiler.js
// end include: runtime_common.js
assert(globalThis.Int32Array && globalThis.Float64Array && Int32Array.prototype.subarray && Int32Array.prototype.set,
       'JS engine does not provide full typed array support');

function preRun() {
  assert(!ENVIRONMENT_IS_PTHREAD); // PThreads reuse the runtime from the main thread.
  var preRun = Module['preRun'];
  if (preRun) {
    if (typeof preRun == 'function') preRun = [preRun];
    onPreRuns.push(...preRun);
  }
  consumedModuleProp('preRun');
  // Begin ATPRERUNS hooks
  callRuntimeCallbacks(onPreRuns);
  // End ATPRERUNS hooks
}

function initRuntime() {
  assert(!runtimeInitialized);
  runtimeInitialized = true;

  if (ENVIRONMENT_IS_PTHREAD) return startWorker();

  setStackLimits();

  checkStackCookie();

  // Begin ATINITS hooks
  SOCKFS.root = FS.mount(SOCKFS, {}, null);
if (!Module['noFSInit'] && !FS.initialized) FS.init();
TTY.init();
PIPEFS.root = FS.mount(PIPEFS, {}, null);
  // End ATINITS hooks

  wasmExports['__wasm_call_ctors']();

  // Begin ATPOSTCTORS hooks
  FS.ignorePermissions = false;
  // End ATPOSTCTORS hooks

  checkStackCookie();
}

function postRun() {
  checkStackCookie();

  var postRun = Module['postRun'];
  if (postRun) {
    if (typeof postRun == 'function') postRun = [postRun];
    onPostRuns.push(...postRun);
  }
  consumedModuleProp('postRun');

  // Begin ATPOSTRUNS hooks
  callRuntimeCallbacks(onPostRuns);
  // End ATPOSTRUNS hooks
}

/**
 * @param {string|number=} what
 */
function abort(what) {
  Module['onAbort']?.(what);

  what = `Aborted(${what})`;
  // TODO(sbc): Should we remove printing and leave it up to whoever
  // catches the exception?
  err(what);

  ABORT = true;

  // Use a wasm runtime error, because a JS error might be seen as a foreign
  // exception, which means we'd run destructors on it. We need the error to
  // simply make the program stop.
  // FIXME This approach does not work in Wasm EH because it currently does not assume
  // all RuntimeErrors are from traps; it decides whether a RuntimeError is from
  // a trap or not based on a hidden field within the object. So at the moment
  // we don't have a way of throwing a wasm trap from JS. TODO Make a JS API that
  // allows this in the wasm spec.

  // Suppress closure compiler warning here. Closure compiler's builtin extern
  // definition for WebAssembly.RuntimeError claims it takes no arguments even
  // though it can.
  // TODO(https://github.com/google/closure-compiler/pull/3913): Remove if/when upstream closure gets fixed.
  /** @suppress {checkTypes} */
  var e = new WebAssembly.RuntimeError(what);

  // Throw the error whether or not MODULARIZE is set because abort is used
  // in code paths apart from instantiation where an exception is expected
  // to be thrown when abort is called.
  throw e;
}

function createExportWrapper(name, func, nargs) {
  assert(func);
  return (...args) => {
    assert(runtimeInitialized, `native function \`${name}\` called before runtime initialization`);
    // Only assert for too many arguments. Too few can be valid since the missing arguments will be zero filled.
    assert(args.length <= nargs, `native function \`${name}\` called with ${args.length} args but expects ${nargs}`);
    return func(...args);
  };
}

var wasmBinaryFile;

function findWasmBinary() {
  return locateFile('nvim.wasm');
}

function getBinarySync(file) {
  if (readBinary) {
    return readBinary(file);
  }
  // Throwing a plain string here, even though it not normally advisable since
  // this gets turning into an `abort` in instantiateArrayBuffer.
  throw 'both async and sync fetching of the wasm failed';
}

async function getWasmBinary(binaryFile) {
  // If we don't have the binary yet, load it asynchronously using readAsync.
  if (!wasmBinary) {
    // Fetch the binary using readAsync
    try {
      var response = await readAsync(binaryFile);
      return new Uint8Array(response);
    } catch {
      // Fall back to getBinarySync below;
    }
  }

  // Otherwise, getBinarySync should be able to get it synchronously
  return getBinarySync(binaryFile);
}

async function instantiateArrayBuffer(binaryFile, imports) {
  try {
    var binary = await getWasmBinary(binaryFile);
    var instance = await WebAssembly.instantiate(binary, imports);
    return instance;
  } catch (reason) {
    err(`failed to asynchronously prepare wasm: ${reason}`);

    // Warn on some common problems.
    if (isFileURI(binaryFile)) {
      err(`warning: Loading from a file URI (${binaryFile}) is not supported in most browsers. See https://emscripten.org/docs/getting_started/FAQ.html#how-do-i-run-a-local-webserver-for-testing-why-does-my-program-stall-in-downloading-or-preparing`);
    }
    abort(reason);
  }
}

async function instantiateAsync(binary, binaryFile, imports) {
  if (!binary
      // Don't use streaming for file:// delivered objects in a webview, fetch them synchronously.
      && !isFileURI(binaryFile)
      // Avoid instantiateStreaming() on Node.js environment for now, as while
      // Node.js v18.1.0 implements it, it does not have a full fetch()
      // implementation yet.
      //
      // Reference:
      //   https://github.com/emscripten-core/emscripten/pull/16917
      && !ENVIRONMENT_IS_NODE
     ) {
    try {
      var response = fetch(binaryFile, { credentials: 'same-origin' });
      var instantiationResult = await WebAssembly.instantiateStreaming(response, imports);
      return instantiationResult;
    } catch (reason) {
      // We expect the most common failure cause to be a bad MIME type for the binary,
      // in which case falling back to ArrayBuffer instantiation should work.
      err(`wasm streaming compile failed: ${reason}`);
      err('falling back to ArrayBuffer instantiation');
      // fall back of instantiateArrayBuffer below
    };
  }
  return instantiateArrayBuffer(binaryFile, imports);
}

function getWasmImports() {
  assignWasmImports();
  // prepare imports
  var imports = {
    'env': wasmImports,
    'wasi_snapshot_preview1': wasmImports,
  };
  return imports;
}

// Create the wasm instance.
// Receives the wasm imports, returns the exports.
async function createWasm() {
  // Load the wasm module and create an instance of using native support in the JS engine.
  // handle a generated wasm instance, receiving its exports and
  // performing other necessary setup
  function receiveInstance(instance, module) {
    wasmExports = instance.exports;

    registerTLSInit(wasmExports['_emscripten_tls_init']);

    assignWasmExports(wasmExports);

    // We now have the Wasm module loaded up, keep a reference to the compiled module so we can post it to the workers.
    wasmModule = module;
    return wasmExports;
  }

  // Prefer streaming instantiation if available.
  // Async compilation can be confusing when an error on the page overwrites Module
  // (for example, if the order of elements is wrong, and the one defining Module is
  // later), so we save Module and check it later.
  var trueModule = Module;
  function receiveInstantiationResult(result) {
    // 'result' is a ResultObject object which has both the module and instance.
    // receiveInstance() will swap in the exports (to Module.asm) so they can be called
    assert(Module === trueModule, 'the Module object should not be replaced during async compilation - perhaps the order of HTML elements is wrong?');
    trueModule = null;
    return receiveInstance(result['instance'], result['module']);
  }

  var info = getWasmImports();

  // User shell pages can write their own Module.instantiateWasm = function(imports, successCallback) callback
  // to manually instantiate the Wasm module themselves. This allows pages to
  // run the instantiation parallel to any other async startup actions they are
  // performing.
  // Also pthreads and wasm workers initialize the wasm instance through this
  // path.
  var instantiateWasm = Module['instantiateWasm'];
  if (instantiateWasm) {
    return new Promise((resolve) => {
      try {
        instantiateWasm(info, (inst, mod) => resolve(receiveInstance(inst, mod)));
      } catch(e) {
        err(`Module.instantiateWasm callback failed with error: ${e}`);
        throw e;
      }
    });
  }

  if ((ENVIRONMENT_IS_PTHREAD)) {
    // Instantiate from the module that was received via postMessage from
    // the main thread. We can just use sync instantiation in the worker.
    assert(wasmModule, "wasmModule should have been received via postMessage");
    var instance = new WebAssembly.Instance(wasmModule, getWasmImports());
    return receiveInstance(instance, wasmModule);
  }

  wasmBinaryFile ??= findWasmBinary();
  var result = await instantiateAsync(wasmBinary, wasmBinaryFile, info);
  var exports = receiveInstantiationResult(result);
  return exports;
}

// end include: preamble.js

// Begin JS library code


  class ExitStatus {
      name = 'ExitStatus';
      constructor(status) {
        this.message = `Program terminated with exit(${status})`;
        this.status = status;
      }
    }

  /** @type {!Int16Array} */
  var HEAP16;

  /** @type {!Int32Array} */
  var HEAP32;

  /** not-@type {!BigInt64Array} */
  var HEAP64;

  /** @type {!Int8Array} */
  var HEAP8;

  /** @type {!Float32Array} */
  var HEAPF32;

  /** @type {!Float64Array} */
  var HEAPF64;

  /** @type {!Uint16Array} */
  var HEAPU16;

  /** @type {!Uint32Array} */
  var HEAPU32;

  /** not-@type {!BigUint64Array} */
  var HEAPU64;

  /** @type {!Uint8Array} */
  var HEAPU8;

  
  var terminateWorker = (worker) => {
      worker.terminate();
      // terminate() can be asynchronous, so in theory the worker can continue
      // to run for some amount of time after termination.  However from our POV
      // the worker is now dead and we don't want to hear from it again, so we stub
      // out its message handler here.  This avoids having to check in each of
      // the onmessage handlers if the message was coming from a valid worker.
      worker.onmessage = (e) => {
        var cmd = e.data.cmd;
        err(`received "${cmd}" command from terminated worker: ${worker.workerID}`);
      };
    };
  
  var cleanupThread = (pthread_ptr) => {
      assert(!ENVIRONMENT_IS_PTHREAD, 'cleanupThread() should only be called from the main thread');
      assert(pthread_ptr, 'null pthread_ptr passed to cleanupThread');
      var worker = PThread.pthreads[pthread_ptr];
      assert(worker);
      PThread.returnWorkerToPool(worker);
    };
  
  var callRuntimeCallbacks = (callbacks) => {
      while (callbacks.length > 0) {
        // Pass the module as the first argument.
        callbacks.shift()(Module);
      }
    };
  var onPreRuns = [];
  var addOnPreRun = (cb) => onPreRuns.push(cb);
  
  var spawnThread = (threadParams) => {
      assert(!ENVIRONMENT_IS_PTHREAD, 'spawnThread() should only be called from the main thread');
      assert(threadParams.pthread_ptr, 'spawnThread called with null pthread ptr');
  
      var worker = PThread.getNewWorker();
      if (!worker) {
        // No available workers in the PThread pool.
        return 6;
      }
      assert(!worker.pthread_ptr);
  
      // Add to pthreads map
      PThread.pthreads[threadParams.pthread_ptr] = worker;
  
      worker.pthread_ptr = threadParams.pthread_ptr;
      var msg = {
          cmd: 2,
          start_routine: threadParams.startRoutine,
          arg: threadParams.arg,
          pthread_ptr: threadParams.pthread_ptr,
      };
      // Ask the worker to start executing its pthread entry point function.
      worker.postMessage(msg, threadParams.transferList);
      return 0;
    };
  
  
  
  var runtimeKeepaliveCounter = 0;
  var keepRuntimeAlive = () => noExitRuntime || runtimeKeepaliveCounter > 0;
  
  var stackSave = () => _emscripten_stack_get_current();
  
  var stackRestore = (val) => __emscripten_stack_restore(val);
  
  var stackAlloc = (sz) => __emscripten_stack_alloc(sz);
  
  
  /** @type{function(number, (number|boolean), ...number)} */
  var proxyToMainThread = (funcIndex, emAsmAddr, proxyMode, ...callArgs) => {
      // EM_ASM proxying is done by passing a pointer to the address of the EM_ASM
      // content as `emAsmAddr`.  JS library proxying is done by passing an index
      // into `proxiedJSCallArgs` as `funcIndex`. If `emAsmAddr` is non-zero then
      // `funcIndex` will be ignored.
      // Additional arguments are passed after the first three are the actual
      // function arguments.
      // The serialization buffer contains the number of call params, and then
      // all the args here.
      //
      // We also pass 'proxyMode' to C separately, since C needs to look at it.
      //
      // Allocate a buffer (on the stack), which will be copied if necessary by
      // the C code.
      //
      // First passed parameter specifies the number of arguments to the function.
      // When BigInt support is enabled, we must handle types in a more complex
      // way, detecting at runtime if a value is a BigInt or not (as we have no
      // type info here). To do that, add a "prefix" before each value that
      // indicates if it is a BigInt, which effectively doubles the number of
      // values we serialize for proxying. TODO: pack this?
      var bufSize = 8 * callArgs.length * 2;
      var sp = stackSave();
      var args = stackAlloc(bufSize);
      var b = ((args)>>3);
      for (var arg of callArgs) {
        if (typeof arg == 'bigint') {
          // The prefix is non-zero to indicate a bigint.
          HEAP64[b++] = 1n;
          HEAP64[b++] = arg;
        } else {
          // The prefix is zero to indicate a JS Number.
          HEAP64[b++] = 0n;
          HEAPF64[b++] = arg;
        }
      }
      var rtn = __emscripten_run_js_on_main_thread(funcIndex, emAsmAddr, bufSize, args, proxyMode);
      stackRestore(sp);
      return rtn;
    };
  
  function _proc_exit(code) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(0, 0, 1, code);
  
      EXITSTATUS = code;
      if (!keepRuntimeAlive()) {
        PThread.terminateAllThreads();
        Module['onExit']?.(code);
        ABORT = true;
      }
      quit_(code, new ExitStatus(code));
    
  }
  
  
  
  
  
  
  function exitOnMainThread(returnCode) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(1, 0, 0, returnCode);
  
      _exit(returnCode);
    
  }
  
  
  /** @param {boolean|number=} implicit */
  var exitJS = (status, implicit) => {
      EXITSTATUS = status;
  
      checkUnflushedContent();
  
      if (ENVIRONMENT_IS_PTHREAD) {
        // implicit exit can never happen on a pthread
        assert(!implicit);
        // When running in a pthread we propagate the exit back to the main thread
        // where it can decide if the whole process should be shut down or not.
        // The pthread may have decided not to exit its own runtime, for example
        // because it runs a main loop, but that doesn't affect the main thread.
        exitOnMainThread(status);
        throw 'unwind';
      }
  
      // if exit() was called explicitly, warn the user if the runtime isn't actually being shut down
      if (keepRuntimeAlive() && !implicit) {
        var msg = `program exited (with status: ${status}), but keepRuntimeAlive() is set (counter=${runtimeKeepaliveCounter}) due to an async operation, so halting execution but not exiting the runtime or preventing further async execution (you can use emscripten_force_exit, if you want to force a true shutdown)`;
        err(msg);
      }
  
      _proc_exit(status);
    };
  var _exit = exitJS;
  
  
  
  var waitAsyncPolyfilled = (!Atomics.waitAsync || (globalThis.navigator?.userAgent && Number((navigator.userAgent.match(/Chrom(e|ium)\/([0-9]+)\./)||[])[2]) < 91));;
  
  function ptrToString(ptr) {
      assert(typeof ptr === 'number', `ptrToString expects a number, got ${typeof ptr}`);
      // Convert to 32-bit unsigned value
      ptr >>>= 0;
      return '0x' + ptr.toString(16).padStart(8, '0');
    }
  
  var PThread = {
  unusedWorkers:[],
  tlsInitFunctions:[],
  pthreads:{
  },
  nextWorkerID:1,
  init() {
        if ((!(ENVIRONMENT_IS_PTHREAD))) {
          PThread.initMainThread();
        }
      },
  initMainThread() {
      },
  terminateAllThreads:() => {
        assert(!ENVIRONMENT_IS_PTHREAD, 'terminateAllThreads() should only be called from the main thread');
        // Attempt to kill all workers.  Sadly (at least on the web) there is no
        // way to terminate a worker synchronously, or to be notified when a
        // worker is actually terminated.  This means there is some risk that
        // pthreads will continue to be executing after `worker.terminate` has
        // returned.  For this reason, we don't call `returnWorkerToPool` here or
        // free the underlying pthread data structures.
        for (var worker of Object.values(PThread.pthreads)) {
          terminateWorker(worker);
        }
        for (var worker of PThread.unusedWorkers) {
          terminateWorker(worker);
        }
        PThread.unusedWorkers = [];
        PThread.pthreads = {};
      },
  terminateRuntime:() => {
        assert(!ENVIRONMENT_IS_PTHREAD, 'terminateRuntime() should only be called from the main thread');
        PThread.terminateAllThreads();
        var pthread_ptr = _pthread_self();
        ___set_thread_state(0, 0, 0, 1);
        if (!waitAsyncPolyfilled) {
          // Break the waitAsync loop.  Note that checkMailbox will not
          // re-register since the `___set_thread_state` above causes _pthread_self
          // to return 0.
          Atomics.notify(HEAP32, ((pthread_ptr)>>2));
        }
      },
  returnWorkerToPool:(worker) => {
        // We don't want to run main thread queued calls here, since we are doing
        // some operations that leave the worker queue in an invalid state until
        // we are completely done (it would be bad if free() ends up calling a
        // queued pthread_create which looks at the global data structures we are
        // modifying). To achieve that, defer the free() until the very end, when
        // we are all done.
        var pthread_ptr = worker.pthread_ptr;
        delete PThread.pthreads[pthread_ptr];
        // Note: worker is intentionally not terminated so the pool can
        // dynamically grow.
        PThread.unusedWorkers.push(worker);
        // Not a running Worker anymore
        // Detach the worker from the pthread object, and return it to the
        // worker pool as an unused worker.
        worker.pthread_ptr = 0;
  
        // Finally, free the underlying (and now-unused) pthread structure in
        // linear memory.
        __emscripten_thread_free_data(pthread_ptr);
      },
  threadInitTLS() {
        // Call thread init functions (these are the _emscripten_tls_init for each
        // module loaded.
        PThread.tlsInitFunctions.forEach((f) => f());
      },
  loadWasmModuleToWorker:(worker) => new Promise((onFinishedLoading) => {
        worker.onmessage = (e) => {
          var d = e.data;
          var cmd = d.cmd;
  
          // If this message is intended to a recipient that is not the main
          // thread, forward it to the target thread. This is currently only
          // used by `CMD_CHECK_MAILBOX`.
          if (d.targetThread) {
            // pthreads should not be relaying messages to themselves.
            assert(d.targetThread != _pthread_self());
            var targetWorker = PThread.pthreads[d.targetThread];
            if (!targetWorker) err(`worker sent message (${cmd}) to pthread (${d.targetThread}) that no longer exists`);
            targetWorker?.postMessage(d);
            return;
          }
  
          if (d === 'setimmediate' || d === '_si') {
            // Worker wants to postMessage() to itself to implement setImmediate()
            // emulation.
            worker.postMessage(d);
            return;
          }
  
          switch (cmd) {
            case 4:
              checkMailbox();
              break;
            case 5:
              spawnThread(d);
              break;
            case 6:
              // cleanupThread needs to be run via callUserCallback since it calls
              // back into user code to free thread data. Without this it's possible
              // the unwind or ExitStatus exception could escape here.
              callUserCallback(() => cleanupThread(d.thread));
              break;
            case 3:
              if (ENVIRONMENT_IS_NODE && !worker.strongref) {
                // Once worker is loaded & idle, mark it as weakly referenced,
                // so that mere existence of a Worker in the pool does not prevent
                // Node.js from exiting the app.
                worker.unref();
              }
              onFinishedLoading(worker);
              break;
            case 8:
              // Message handler for Node.js specific out-of-order behavior:
              // https://github.com/nodejs/node/issues/59617
              // A pthread sent an uncaught exception event. Re-raise it on the main thread.
              worker.onerror(d.error);
              break;
            case 9:
              Module[d.handler](...d.args);
              break;
            default:
              // The received message looks like something that should be handled by this message
              // handler, (since there is a e.data.cmd field present), but is not one of the
              // recognized commands:
              if (cmd) err(`worker sent an unknown command ${cmd}`);
          }
        };
  
        worker.onerror = (e) => {
          var message = 'worker sent an error!';
          if (worker.pthread_ptr) {
            message = `Pthread ${ptrToString(worker.pthread_ptr)} sent an error!`;
          }
          err(`${message} ${e.filename}:${e.lineno}: ${e.message}`);
          throw e;
        };
  
        if (ENVIRONMENT_IS_NODE) {
          worker.on('message', (data) => worker.onmessage({ data: data }));
          worker.on('error', (e) => worker.onerror(e));
  
        }
  
        assert(wasmMemory instanceof WebAssembly.Memory, 'wasmMemory should have been loaded by now');
        assert(wasmModule instanceof WebAssembly.Module, 'wasmModule should have been loaded by now');
  
        // When running on a pthread, none of the incoming parameters on the module
        // object are present. Proxy known handlers back to the main thread if specified.
        var handlers = [];
        var knownHandlers = [
          'onExit',
          'onAbort',
          'print',
          'printErr',
        ];
        for (var handler of knownHandlers) {
          if (Module.propertyIsEnumerable(handler)) {
            handlers.push(handler);
          }
        }
  
        // Ask the new worker to load up the Emscripten-compiled page. This is a heavy operation.
        worker.postMessage({
          cmd: 1,
          handlers: handlers,
          wasmMemory,
          wasmModule,
          workerID: worker.workerID,
        });
      }),
  allocateUnusedWorker() {
        var worker;
        var pthreadMainJs = _scriptName;
        worker = new Worker(pthreadMainJs, {
          // This is the way that we signal to the node worker that it is hosting
          // a pthread.
          'workerData': 'em-pthread',
          // This is the way that we signal to the Web Worker that it is hosting
          // a pthread.
          'name': 'em-pthread-' + PThread.nextWorkerID,
  });
        worker.workerID = PThread.nextWorkerID++;
        PThread.unusedWorkers.push(worker);
        return worker;
      },
  getNewWorker() {
        if (PThread.unusedWorkers.length == 0) {
  // PTHREAD_POOL_SIZE_STRICT should show a warning and, if set to level `2`, return from the function.
  // However, if we're in Node.js, then we can create new workers on the fly and PTHREAD_POOL_SIZE_STRICT
  // should be ignored altogether.
          if (!ENVIRONMENT_IS_NODE) {
              err('Tried to spawn a new thread, but the thread pool is exhausted.\n' +
              'This might result in a deadlock unless some threads eventually exit or the code explicitly breaks out to the event loop.\n' +
              'If you want to increase the pool size, use setting `-sPTHREAD_POOL_SIZE=...`.'
                + '\nIf you want to throw an explicit error instead of the risk of deadlocking in those cases, use setting `-sPTHREAD_POOL_SIZE_STRICT=2`.'
              );
          }
          var newWorker = PThread.allocateUnusedWorker();
          PThread.loadWasmModuleToWorker(newWorker);
        }
        return PThread.unusedWorkers.pop();
      },
  };

  var onPostRuns = [];
  var addOnPostRun = (cb) => onPostRuns.push(cb);



  
  
  function establishStackSpace(pthread_ptr) {
      var stackHigh = HEAPU32[(((pthread_ptr)+(48))>>2)];
      var stackSize = HEAPU32[(((pthread_ptr)+(52))>>2)];
      var stackLow = stackHigh - stackSize;
      assert(stackHigh != 0);
      assert(stackLow != 0);
      assert(stackHigh > stackLow, 'stackHigh must be higher then stackLow');
      // Set stack limits used by `emscripten/stack.h` function.  These limits are
      // cached in wasm-side globals to make checks as fast as possible.
      _emscripten_stack_set_limits(stackHigh, stackLow);
  
      setStackLimits();
  
      // Call inside wasm module to set up the stack frame for this pthread in wasm module scope
      stackRestore(stackHigh);
  
      // Write the stack cookie last, after we have set up the proper bounds and
      // current position of the stack.
      writeStackCookie();
    }

  
    /**
   * @param {number} ptr
   * @param {string} type
   */
  function getValue(ptr, type = 'i8') {
    if (type.endsWith('*')) type = '*';
    switch (type) {
      case 'i1': return HEAP8[ptr];
      case 'i8': return HEAP8[ptr];
      case 'i16': return HEAP16[((ptr)>>1)];
      case 'i32': return HEAP32[((ptr)>>2)];
      case 'i64': return HEAP64[((ptr)>>3)];
      case 'float': return HEAPF32[((ptr)>>2)];
      case 'double': return HEAPF64[((ptr)>>3)];
      case '*': return HEAPU32[((ptr)>>2)];
      default: abort(`invalid type for getValue: ${type}`);
    }
  }

  
  
  
  
  var wasmTableMirror = [];
  
  
  var getWasmTableEntry = (funcPtr) => {
      var func = wasmTableMirror[funcPtr];
      if (!func) {
        /** @suppress {checkTypes} */
        wasmTableMirror[funcPtr] = func = wasmTable.get(funcPtr);
      }
      /** @suppress {checkTypes} */
      assert(wasmTable.get(funcPtr) == func, 'table mirror is out of date');
      return func;
    };
  var invokeEntryPoint = (ptr, arg) => {
      // An old thread on this worker may have been canceled without returning the
      // `runtimeKeepaliveCounter` to zero. Reset it now so the new thread won't
      // be affected.
      runtimeKeepaliveCounter = 0;
  
      // Same for noExitRuntime.  The default for pthreads should always be false
      // otherwise pthreads would never complete and attempts to pthread_join to
      // them would block forever.
      // pthreads can still choose to set `noExitRuntime` explicitly, or
      // call emscripten_unwind_to_js_event_loop to extend their lifetime beyond
      // their main function.  See comment in src/runtime_pthread.js for more.
      noExitRuntime = 0;
  
      // pthread entry points are always of signature 'void *ThreadMain(void *arg)'
      // Native codebases sometimes spawn threads with other thread entry point
      // signatures, such as void ThreadMain(void *arg), void *ThreadMain(), or
      // void ThreadMain().  That is not acceptable per C/C++ specification, but
      // x86 compiler ABI extensions enable that to work. If you find the
      // following line to crash, either change the signature to "proper" void
      // *ThreadMain(void *arg) form, or try linking with the Emscripten linker
      // flag -sEMULATE_FUNCTION_POINTER_CASTS to add in emulation for this x86
      // ABI extension.
  
      var result = getWasmTableEntry(ptr)(arg);
  
      checkStackCookie();
      function finish(result) {
        // In MINIMAL_RUNTIME the noExitRuntime concept does not apply to
        // pthreads. To exit a pthread with live runtime, use the function
        // emscripten_unwind_to_js_event_loop() in the pthread body.
        if (keepRuntimeAlive()) {
          EXITSTATUS = result;
          return;
        }
        __emscripten_thread_exit(result);
      }
      finish(result);
    };

  var noExitRuntime = true;


  var registerTLSInit = (tlsInitFunc) => PThread.tlsInitFunctions.push(tlsInitFunc);

  var setStackLimits = () => {
      var stackLow = _emscripten_stack_get_base();
      var stackHigh = _emscripten_stack_get_end();
      ___set_stack_limits(stackLow, stackHigh);
    };

  
    /**
   * @param {number} ptr
   * @param {number} value
   * @param {string} type
   */
  function setValue(ptr, value, type = 'i8') {
    if (type.endsWith('*')) type = '*';
    switch (type) {
      case 'i1': HEAP8[ptr] = value; break;
      case 'i8': HEAP8[ptr] = value; break;
      case 'i16': HEAP16[((ptr)>>1)] = value; break;
      case 'i32': HEAP32[((ptr)>>2)] = value; break;
      case 'i64': HEAP64[((ptr)>>3)] = BigInt(value); break;
      case 'float': HEAPF32[((ptr)>>2)] = value; break;
      case 'double': HEAPF64[((ptr)>>3)] = value; break;
      case '*': HEAPU32[((ptr)>>2)] = value; break;
      default: abort(`invalid type for setValue: ${type}`);
    }
  }



  var warnOnce = (text) => {
      warnOnce.shown ||= {};
      if (!warnOnce.shown[text]) {
        warnOnce.shown[text] = 1;
        if (ENVIRONMENT_IS_NODE) text = 'warning: ' + text;
        err(text);
      }
    };

  var wasmMemory;

  var UTF8Decoder = globalThis.TextDecoder && new TextDecoder();
  
  
    /**
   * heapOrArray is either a regular array, or a JavaScript typed array view.
   * @param {number} idx
   * @param {number=} maxBytesToRead
   * @param {boolean=} ignoreNul
   * @return {number}
   */
  var findStringEnd = (heapOrArray, idx, maxBytesToRead, ignoreNul) => {
      var maxIdx = idx + maxBytesToRead;
      if (ignoreNul) return maxIdx;
      // TextDecoder needs to know the byte length in advance, it doesn't stop on
      // null terminator by itself.
      // As a tiny code save trick, compare idx against maxIdx using a negation,
      // so that maxBytesToRead=undefined/NaN means Infinity.
      while (heapOrArray[idx] && !(idx >= maxIdx)) ++idx;
      return idx;
    };
  
  
    /**
   * Given a pointer 'idx' to a null-terminated UTF8-encoded string in the given
   * array that contains uint8 values, returns a copy of that string as a
   * Javascript String object.
   * heapOrArray is either a regular array, or a JavaScript typed array view.
   * @param {number=} idx
   * @param {number=} maxBytesToRead
   * @param {boolean=} ignoreNul - If true, the function will not stop on a NUL character.
   * @return {string}
   */
  var UTF8ArrayToString = (heapOrArray, idx = 0, maxBytesToRead, ignoreNul) => {
  
      var endPtr = findStringEnd(heapOrArray, idx, maxBytesToRead, ignoreNul);
  
      // When using conditional TextDecoder, skip it for short strings as the overhead of the native call is not worth it.
      if (endPtr - idx > 16 && heapOrArray.buffer && UTF8Decoder) {
        return UTF8Decoder.decode(heapOrArray.buffer instanceof ArrayBuffer ? heapOrArray.subarray(idx, endPtr) : heapOrArray.slice(idx, endPtr));
      }
      var str = '';
      while (idx < endPtr) {
        // For UTF8 byte structure, see:
        // http://en.wikipedia.org/wiki/UTF-8#Description
        // https://www.ietf.org/rfc/rfc2279.txt
        // https://tools.ietf.org/html/rfc3629
        var u0 = heapOrArray[idx++];
        if (!(u0 & 0x80)) { str += String.fromCharCode(u0); continue; }
        var u1 = heapOrArray[idx++] & 63;
        if ((u0 & 0xE0) == 0xC0) { str += String.fromCharCode(((u0 & 31) << 6) | u1); continue; }
        var u2 = heapOrArray[idx++] & 63;
        if ((u0 & 0xF0) == 0xE0) {
          u0 = ((u0 & 15) << 12) | (u1 << 6) | u2;
        } else {
          if ((u0 & 0xF8) != 0xF0) warnOnce(`Invalid UTF-8 leading byte ${ptrToString(u0)} encountered when deserializing a UTF-8 string in wasm memory to a JS string!`);
          u0 = ((u0 & 7) << 18) | (u1 << 12) | (u2 << 6) | (heapOrArray[idx++] & 63);
        }
  
        if (u0 < 0x10000) {
          str += String.fromCharCode(u0);
        } else {
          var ch = u0 - 0x10000;
          str += String.fromCharCode(0xD800 | (ch >> 10), 0xDC00 | (ch & 0x3FF));
        }
      }
      return str;
    };
  
    /**
   * Given a pointer 'ptr' to a null-terminated UTF8-encoded string in the
   * emscripten HEAP, returns a copy of that string as a Javascript String object.
   *
   * @param {number} ptr
   * @param {number=} maxBytesToRead - An optional length that specifies the
   *   maximum number of bytes to read. You can omit this parameter to scan the
   *   string until the first 0 byte. If maxBytesToRead is passed, and the string
   *   at [ptr, ptr+maxBytesToReadr[ contains a null byte in the middle, then the
   *   string will cut short at that byte index.
   * @param {boolean=} ignoreNul - If true, the function will not stop on a NUL character.
   * @return {string}
   */
  var UTF8ToString = (ptr, maxBytesToRead, ignoreNul) => {
      assert(typeof ptr == 'number', `UTF8ToString expects a number (got ${typeof ptr})`);
      return ptr ? UTF8ArrayToString(HEAPU8, ptr, maxBytesToRead, ignoreNul) : '';
    };
  var ___assert_fail = (condition, filename, line, func) =>
      abort(`Assertion failed: ${UTF8ToString(condition)}, at: ` + [filename ? UTF8ToString(filename) : 'unknown filename', line, func ? UTF8ToString(func) : 'unknown function']);

  var ___call_sighandler = (fp, sig) => getWasmTableEntry(fp)(sig);

  var exceptionCaught =  [];
  
  
  var uncaughtExceptionCount = 0;
  var ___cxa_begin_catch = (ptr) => {
      var info = new ExceptionInfo(ptr);
      if (!info.get_caught()) {
        info.set_caught(true);
        uncaughtExceptionCount--;
      }
      info.set_rethrown(false);
      exceptionCaught.push(info);
      return ___cxa_get_exception_ptr(ptr);
    };

  var exceptionLast = null;
  
  class ExceptionInfo {
      // excPtr - Thrown object pointer to wrap. Metadata pointer is calculated from it.
      constructor(excPtr) {
        this.excPtr = excPtr;
        this.ptr = excPtr - 24;
      }
  
      set_type(type) {
        HEAPU32[(((this.ptr)+(4))>>2)] = type;
      }
  
      get_type() {
        return HEAPU32[(((this.ptr)+(4))>>2)];
      }
  
      set_destructor(destructor) {
        HEAPU32[(((this.ptr)+(8))>>2)] = destructor;
      }
  
      get_destructor() {
        return HEAPU32[(((this.ptr)+(8))>>2)];
      }
  
      set_caught(caught) {
        caught = caught ? 1 : 0;
        HEAP8[(this.ptr)+(12)] = caught;
      }
  
      get_caught() {
        return HEAP8[(this.ptr)+(12)] != 0;
      }
  
      set_rethrown(rethrown) {
        rethrown = rethrown ? 1 : 0;
        HEAP8[(this.ptr)+(13)] = rethrown;
      }
  
      get_rethrown() {
        return HEAP8[(this.ptr)+(13)] != 0;
      }
  
      // Initialize native structure fields. Should be called once after allocated.
      init(type, destructor) {
        this.set_adjusted_ptr(0);
        this.set_type(type);
        this.set_destructor(destructor);
      }
  
      set_adjusted_ptr(adjustedPtr) {
        HEAPU32[(((this.ptr)+(16))>>2)] = adjustedPtr;
      }
  
      get_adjusted_ptr() {
        return HEAPU32[(((this.ptr)+(16))>>2)];
      }
    }
  
  
  var setTempRet0 = (val) => __emscripten_tempret_set(val);
  var findMatchingCatch = (args) => {
      var thrown = exceptionLast?.excPtr;
      if (!thrown) {
        // just pass through the null ptr
        setTempRet0(0);
        return 0;
      }
      var info = new ExceptionInfo(thrown);
      info.set_adjusted_ptr(thrown);
      var thrownType = info.get_type();
      if (!thrownType) {
        // just pass through the thrown ptr
        setTempRet0(0);
        return thrown;
      }
  
      // can_catch receives a **, add indirection
      // The different catch blocks are denoted by different types.
      // Due to inheritance, those types may not precisely match the
      // type of the thrown object. Find one which matches, and
      // return the type of the catch block which should be called.
      for (var caughtType of args) {
        if (caughtType === 0 || caughtType === thrownType) {
          // Catch all clause matched or exactly the same type is caught
          break;
        }
        var adjusted_ptr_addr = info.ptr + 16;
        if (___cxa_can_catch(caughtType, thrownType, adjusted_ptr_addr)) {
          setTempRet0(caughtType);
          return thrown;
        }
      }
      setTempRet0(thrownType);
      return thrown;
    };
  var ___cxa_find_matching_catch_2 = () => findMatchingCatch([]);

  var ___cxa_find_matching_catch_3 = (arg0) => findMatchingCatch([arg0]);

  
  
  var ___handle_stack_overflow = (requested) => {
      var base = _emscripten_stack_get_base();
      var end = _emscripten_stack_get_end();
      abort(`stack overflow (Attempt to set SP to ${ptrToString(requested)}` +
            `, with stack limits [${ptrToString(end)} - ${ptrToString(base)}` +
            ']). If you require more stack space build with -sSTACK_SIZE=<bytes>');
    };

  
  
  
  
  function pthreadCreateProxied(pthread_ptr, attr, startRoutine, arg) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(2, 0, 1, pthread_ptr, attr, startRoutine, arg);
  return ___pthread_create_js(pthread_ptr, attr, startRoutine, arg)
  }
  
  
  var _emscripten_has_threading_support = () => !!globalThis.SharedArrayBuffer;
  
  var ___pthread_create_js = (pthread_ptr, attr, startRoutine, arg) => {
      if (!_emscripten_has_threading_support()) {
        dbg('pthread_create: environment does not support SharedArrayBuffer, pthreads are not available');
        return 6;
      }
  
      // List of JS objects that will transfer ownership to the Worker hosting the thread
      var transferList = [];
      var error = 0;
  
      // Synchronously proxy the thread creation to main thread if possible. If we
      // need to transfer ownership of objects, then proxy asynchronously via
      // postMessage.
      if (ENVIRONMENT_IS_PTHREAD && (transferList.length === 0 || error)) {
        return pthreadCreateProxied(pthread_ptr, attr, startRoutine, arg);
      }
  
      // If on the main thread, and accessing Canvas/OffscreenCanvas failed, abort
      // with the detected error.
      if (error) return error;
  
      var threadParams = {
        startRoutine,
        pthread_ptr,
        arg,
        transferList,
      };
  
      if (ENVIRONMENT_IS_PTHREAD) {
        // The prepopulated pool of web workers that can host pthreads is stored
        // in the main JS thread. Therefore if a pthread is attempting to spawn a
        // new thread, the thread creation must be deferred to the main JS thread.
        threadParams.cmd = 5;
        postMessage(threadParams, transferList);
        // When we defer thread creation this way, we have no way to detect thread
        // creation synchronously today, so we have to assume success and return 0.
        return 0;
      }
  
      // We are the main thread, so we have the pthread warmup pool in this
      // thread and can fire off JS thread creation directly ourselves.
      return spawnThread(threadParams);
    };

  var ___resumeException = (ptr) => {
      if (!exceptionLast) {
        exceptionLast = new CppException(ptr);
      }
      throw exceptionLast;
    };

  var initRandomFill = () => {
      // This block is not needed on v19+ since crypto.getRandomValues is builtin
      if (ENVIRONMENT_IS_NODE) {
        var nodeCrypto = require('node:crypto');
        return (view) => (nodeCrypto.randomFillSync(view), 0);
      }
  
      // like with most Web APIs, we can't use Web Crypto API directly on shared memory,
      // so we need to create an intermediate buffer and copy it to the destination
      return (view) => (view.set(crypto.getRandomValues(new Uint8Array(view.byteLength))), 0);
    };
  var randomFill = (view) => (randomFill = initRandomFill())(view);
  
  var PATH = {
  isAbs:(path) => path.charAt(0) === '/',
  splitPath:(filename) => {
        var splitPathRe = /^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/;
        return splitPathRe.exec(filename).slice(1);
      },
  normalizeArray:(parts, allowAboveRoot) => {
        // if the path tries to go above the root, `up` ends up > 0
        var up = 0;
        for (var i = parts.length - 1; i >= 0; i--) {
          var last = parts[i];
          if (last === '.') {
            parts.splice(i, 1);
          } else if (last === '..') {
            parts.splice(i, 1);
            up++;
          } else if (up) {
            parts.splice(i, 1);
            up--;
          }
        }
        // if the path is allowed to go above the root, restore leading ..s
        if (allowAboveRoot) {
          for (; up; up--) {
            parts.unshift('..');
          }
        }
        return parts;
      },
  normalize:(path) => {
        var isAbsolute = PATH.isAbs(path),
            trailingSlash = path.slice(-1) === '/';
        // Normalize the path
        path = PATH.normalizeArray(path.split('/').filter((p) => !!p), !isAbsolute).join('/');
        if (!path && !isAbsolute) {
          path = '.';
        }
        if (path && trailingSlash) {
          path += '/';
        }
        return (isAbsolute ? '/' : '') + path;
      },
  dirname:(path) => {
        var result = PATH.splitPath(path),
            root = result[0],
            dir = result[1];
        if (!root && !dir) {
          // No dirname whatsoever
          return '.';
        }
        if (dir) {
          // It has a dirname, strip trailing slash
          dir = dir.slice(0, -1);
        }
        return root + dir;
      },
  basename:(path) => path && path.match(/([^\/]+|\/)\/*$/)[1],
join:(...paths) => PATH.normalize(paths.join('/')),
join2:(l, r) => PATH.normalize(l + '/' + r),
};


var PATH_FS = {
resolve:(...args) => {
      var resolvedPath = '',
        resolvedAbsolute = false;
      for (var i = args.length - 1; i >= -1 && !resolvedAbsolute; i--) {
        var path = (i >= 0) ? args[i] : FS.cwd();
        // Skip empty and invalid entries
        if (typeof path != 'string') {
          throw new TypeError('Arguments to path.resolve must be strings');
        } else if (!path) {
          return ''; // an invalid portion invalidates the whole thing
        }
        resolvedPath = path + '/' + resolvedPath;
        resolvedAbsolute = PATH.isAbs(path);
      }
      // At this point the path should be resolved to a full absolute path, but
      // handle relative paths to be safe (might happen when process.cwd() fails)
      resolvedPath = PATH.normalizeArray(resolvedPath.split('/').filter((p) => !!p), !resolvedAbsolute).join('/');
      return ((resolvedAbsolute ? '/' : '') + resolvedPath) || '.';
    },
relative:(from, to) => {
      from = PATH_FS.resolve(from).slice(1);
      to = PATH_FS.resolve(to).slice(1);
      function trim(arr) {
        var start = 0;
        for (; start < arr.length; start++) {
          if (arr[start] !== '') break;
        }
        var end = arr.length - 1;
        for (; end >= 0; end--) {
          if (arr[end] !== '') break;
        }
        if (start > end) return [];
        return arr.slice(start, end - start + 1);
      }
      var fromParts = trim(from.split('/'));
      var toParts = trim(to.split('/'));
      var length = Math.min(fromParts.length, toParts.length);
      var samePartsLength = length;
      for (var i = 0; i < length; i++) {
        if (fromParts[i] !== toParts[i]) {
          samePartsLength = i;
          break;
        }
      }
      var outputParts = [];
      for (var i = samePartsLength; i < fromParts.length; i++) {
        outputParts.push('..');
      }
      outputParts = outputParts.concat(toParts.slice(samePartsLength));
      return outputParts.join('/');
    },
};



var FS_stdin_getChar_buffer = [];

var lengthBytesUTF8 = (str) => {
    var len = 0;
    for (var i = 0; i < str.length; ++i) {
      // Gotcha: charCodeAt returns a 16-bit word that is a UTF-16 encoded code
      // unit, not a Unicode code point of the character! So decode
      // UTF16->UTF32->UTF8.
      // See http://unicode.org/faq/utf_bom.html#utf16-3
      var c = str.charCodeAt(i); // possibly a lead surrogate
      if (c <= 0x7F) {
        len++;
      } else if (c <= 0x7FF) {
        len += 2;
      } else if (c >= 0xD800 && c <= 0xDFFF) {
        len += 4; ++i;
      } else {
        len += 3;
      }
    }
    return len;
  };

var stringToUTF8Array = (str, heap, outIdx, maxBytesToWrite) => {
    assert(typeof str === 'string', `stringToUTF8Array expects a string (got ${typeof str})`);
    // Parameter maxBytesToWrite is not optional. Negative values, 0, null,
    // undefined and false each don't write out any bytes.
    if (!(maxBytesToWrite > 0))
      return 0;

    var startIdx = outIdx;
    var endIdx = outIdx + maxBytesToWrite - 1; // -1 for string null terminator.
    for (var i = 0; i < str.length; ++i) {
      // For UTF8 byte structure, see http://en.wikipedia.org/wiki/UTF-8#Description
      // and https://www.ietf.org/rfc/rfc2279.txt
      // and https://tools.ietf.org/html/rfc3629
      var u = str.codePointAt(i);
      if (u <= 0x7F) {
        if (outIdx >= endIdx) break;
        heap[outIdx++] = u;
      } else if (u <= 0x7FF) {
        if (outIdx + 1 >= endIdx) break;
        heap[outIdx++] = 0xC0 | (u >> 6);
        heap[outIdx++] = 0x80 | (u & 63);
      } else if (u <= 0xFFFF) {
        if (outIdx + 2 >= endIdx) break;
        heap[outIdx++] = 0xE0 | (u >> 12);
        heap[outIdx++] = 0x80 | ((u >> 6) & 63);
        heap[outIdx++] = 0x80 | (u & 63);
      } else {
        if (outIdx + 3 >= endIdx) break;
        if (u > 0x10FFFF) warnOnce(`Invalid Unicode code point ${ptrToString(u)} encountered when serializing a JS string to a UTF-8 string in wasm memory! (Valid unicode code points should be in range 0-0x10FFFF).`);
        heap[outIdx++] = 0xF0 | (u >> 18);
        heap[outIdx++] = 0x80 | ((u >> 12) & 63);
        heap[outIdx++] = 0x80 | ((u >> 6) & 63);
        heap[outIdx++] = 0x80 | (u & 63);
        // Gotcha: if codePoint is over 0xFFFF, it is represented as a surrogate pair in UTF-16.
        // We need to manually skip over the second code unit for correct iteration.
        i++;
      }
    }
    // Null-terminate the pointer to the buffer.
    heap[outIdx] = 0;
    return outIdx - startIdx;
  };
/** @type {function(string, boolean=, number=)} */
  var intArrayFromString = (stringy, dontAddNull, length) => {
      var len = length > 0 ? length : lengthBytesUTF8(stringy)+1;
      var u8array = new Array(len);
      var numBytesWritten = stringToUTF8Array(stringy, u8array, 0, u8array.length);
      if (dontAddNull) u8array.length = numBytesWritten;
      return u8array;
    };
  var FS_stdin_getChar = () => {
      if (!FS_stdin_getChar_buffer.length) {
        var result = null;
        if (ENVIRONMENT_IS_NODE) {
          // we will read data by chunks of BUFSIZE
          var BUFSIZE = 256;
          var buf = Buffer.alloc(BUFSIZE);
          var bytesRead = 0;
  
          // For some reason we must suppress a closure warning here, even though
          // fd definitely exists on process.stdin, and is even the proper way to
          // get the fd of stdin,
          // https://github.com/nodejs/help/issues/2136#issuecomment-523649904
          // This started to happen after moving this logic out of library_tty.js,
          // so it is related to the surrounding code in some unclear manner.
          /** @suppress {missingProperties} */
          var fd = process.stdin.fd;
  
          try {
            bytesRead = fs.readSync(fd, buf, 0, BUFSIZE);
          } catch(e) {
            // Cross-platform differences: on Windows, reading EOF throws an
            // exception, but on other OSes, reading EOF returns 0. Uniformize
            // behavior by treating the EOF exception to return 0.
            if (e.toString().includes('EOF')) bytesRead = 0;
            else throw e;
          }
  
          if (bytesRead > 0) {
            result = buf.slice(0, bytesRead).toString('utf-8');
          }
        } else
        if (globalThis.window?.prompt) {
          // Browser.
          result = window.prompt('Input: ');  // returns null on cancel
          if (result !== null) {
            result += '\n';
          }
        } else
        {}
        if (!result) {
          return null;
        }
        FS_stdin_getChar_buffer = intArrayFromString(result, true);
      }
      return FS_stdin_getChar_buffer.shift();
    };
  var TTY = {
  ttys:[],
  init() {
        // https://github.com/emscripten-core/emscripten/pull/1555
        // if (ENVIRONMENT_IS_NODE) {
        //   // currently, FS.init does not distinguish if process.stdin is a file or TTY
        //   // device, it always assumes it's a TTY device. because of this, we're forcing
        //   // process.stdin to UTF8 encoding to at least make stdin reading compatible
        //   // with text files until FS.init can be refactored.
        //   process.stdin.setEncoding('utf8');
        // }
      },
  shutdown() {
        // https://github.com/emscripten-core/emscripten/pull/1555
        // if (ENVIRONMENT_IS_NODE) {
        //   // inolen: any idea as to why node -e 'process.stdin.read()' wouldn't exit immediately (with process.stdin being a tty)?
        //   // isaacs: because now it's reading from the stream, you've expressed interest in it, so that read() kicks off a _read() which creates a ReadReq operation
        //   // inolen: I thought read() in that case was a synchronous operation that just grabbed some amount of buffered data if it exists?
        //   // isaacs: it is. but it also triggers a _read() call, which calls readStart() on the handle
        //   // isaacs: do process.stdin.pause() and i'd think it'd probably close the pending call
        //   process.stdin.pause();
        // }
      },
  register(dev, ops) {
        TTY.ttys[dev] = { input: [], output: [], ops: ops };
        FS.registerDevice(dev, TTY.stream_ops);
      },
  stream_ops:{
  open(stream) {
          var tty = TTY.ttys[stream.node.rdev];
          if (!tty) {
            throw new FS.ErrnoError(43);
          }
          stream.tty = tty;
          stream.seekable = false;
        },
  close(stream) {
          // flush any pending line data
          stream.tty.ops.fsync(stream.tty);
        },
  fsync(stream) {
          stream.tty.ops.fsync(stream.tty);
        },
  read(stream, buffer, offset, length, pos /* ignored */) {
          if (!stream.tty || !stream.tty.ops.get_char) {
            throw new FS.ErrnoError(60);
          }
          var bytesRead = 0;
          for (var i = 0; i < length; i++) {
            var result;
            try {
              result = stream.tty.ops.get_char(stream.tty);
            } catch (e) {
              throw new FS.ErrnoError(29);
            }
            if (result === undefined && bytesRead === 0) {
              throw new FS.ErrnoError(6);
            }
            if (result === null || result === undefined) break;
            bytesRead++;
            buffer[offset+i] = result;
          }
          if (bytesRead) {
            stream.node.atime = Date.now();
          }
          return bytesRead;
        },
  write(stream, buffer, offset, length, pos) {
          if (!stream.tty || !stream.tty.ops.put_char) {
            throw new FS.ErrnoError(60);
          }
          try {
            for (var i = 0; i < length; i++) {
              stream.tty.ops.put_char(stream.tty, buffer[offset+i]);
            }
          } catch (e) {
            throw new FS.ErrnoError(29);
          }
          if (length) {
            stream.node.mtime = stream.node.ctime = Date.now();
          }
          return i;
        },
  },
  default_tty_ops:{
  get_char(tty) {
          return FS_stdin_getChar();
        },
  put_char(tty, val) {
          if (val === null || val === 10) {
            out(UTF8ArrayToString(tty.output));
            tty.output = [];
          } else {
            if (val != 0) tty.output.push(val); // val == 0 would cut text output off in the middle.
          }
        },
  fsync(tty) {
          if (tty.output?.length > 0) {
            out(UTF8ArrayToString(tty.output));
            tty.output = [];
          }
        },
  ioctl_tcgets(tty) {
          // typical setting
          return {
            c_iflag: 25856,
            c_oflag: 5,
            c_cflag: 191,
            c_lflag: 35387,
            c_cc: [
              0x03, 0x1c, 0x7f, 0x15, 0x04, 0x00, 0x01, 0x00, 0x11, 0x13, 0x1a, 0x00,
              0x12, 0x0f, 0x17, 0x16, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ]
          };
        },
  ioctl_tcsets(tty, optional_actions, data) {
          // currently just ignore
          return 0;
        },
  ioctl_tiocgwinsz(tty) {
          return [24, 80];
        },
  },
  default_tty1_ops:{
  put_char(tty, val) {
          if (val === null || val === 10) {
            err(UTF8ArrayToString(tty.output));
            tty.output = [];
          } else {
            if (val != 0) tty.output.push(val);
          }
        },
  fsync(tty) {
          if (tty.output?.length > 0) {
            err(UTF8ArrayToString(tty.output));
            tty.output = [];
          }
        },
  },
  };
  
  
  var mmapAlloc = (size) => {
      abort('internal error: mmapAlloc called but `emscripten_builtin_memalign` native symbol not exported');
    };
  var MEMFS = {
  ops_table:null,
  mount(mount) {
        return MEMFS.createNode(null, '/', 16895, 0);
      },
  createNode(parent, name, mode, dev) {
        if (FS.isBlkdev(mode) || FS.isFIFO(mode)) {
          // not supported
          throw new FS.ErrnoError(63);
        }
        MEMFS.ops_table ||= {
          dir: {
            node: {
              getattr: MEMFS.node_ops.getattr,
              setattr: MEMFS.node_ops.setattr,
              lookup: MEMFS.node_ops.lookup,
              mknod: MEMFS.node_ops.mknod,
              rename: MEMFS.node_ops.rename,
              unlink: MEMFS.node_ops.unlink,
              rmdir: MEMFS.node_ops.rmdir,
              readdir: MEMFS.node_ops.readdir,
              symlink: MEMFS.node_ops.symlink
            },
            stream: {
              llseek: MEMFS.stream_ops.llseek
            }
          },
          file: {
            node: {
              getattr: MEMFS.node_ops.getattr,
              setattr: MEMFS.node_ops.setattr
            },
            stream: {
              llseek: MEMFS.stream_ops.llseek,
              read: MEMFS.stream_ops.read,
              write: MEMFS.stream_ops.write,
              mmap: MEMFS.stream_ops.mmap,
              msync: MEMFS.stream_ops.msync
            }
          },
          link: {
            node: {
              getattr: MEMFS.node_ops.getattr,
              setattr: MEMFS.node_ops.setattr,
              readlink: MEMFS.node_ops.readlink
            },
            stream: {}
          },
          chrdev: {
            node: {
              getattr: MEMFS.node_ops.getattr,
              setattr: MEMFS.node_ops.setattr
            },
            stream: FS.chrdev_stream_ops
          }
        };
        var node = FS.createNode(parent, name, mode, dev);
        if (FS.isDir(node.mode)) {
          node.node_ops = MEMFS.ops_table.dir.node;
          node.stream_ops = MEMFS.ops_table.dir.stream;
          node.contents = {};
        } else if (FS.isFile(node.mode)) {
          node.node_ops = MEMFS.ops_table.file.node;
          node.stream_ops = MEMFS.ops_table.file.stream;
          // The actual number of bytes used in the typed array, as opposed to
          // contents.length which gives the whole capacity.
          node.usedBytes = 0;
          // The byte data of the file is stored in a typed array.
          // Note: typed arrays are not resizable like normal JS arrays are, so
          // there is a small penalty involved for appending file writes that
          // continuously grow a file similar to std::vector capacity vs used.
          node.contents = MEMFS.emptyFileContents ??= new Uint8Array(0);
        } else if (FS.isLink(node.mode)) {
          node.node_ops = MEMFS.ops_table.link.node;
          node.stream_ops = MEMFS.ops_table.link.stream;
        } else if (FS.isChrdev(node.mode)) {
          node.node_ops = MEMFS.ops_table.chrdev.node;
          node.stream_ops = MEMFS.ops_table.chrdev.stream;
        }
        node.atime = node.mtime = node.ctime = Date.now();
        // add the new node to the parent
        if (parent) {
          parent.contents[name] = node;
          parent.atime = parent.mtime = parent.ctime = node.atime;
        }
        return node;
      },
  getFileDataAsTypedArray(node) {
        assert(FS.isFile(node.mode), 'getFileDataAsTypedArray called on non-file');
        return node.contents.subarray(0, node.usedBytes); // Make sure to not return excess unused bytes.
      },
  expandFileStorage(node, newCapacity) {
        var prevCapacity = node.contents.length;
        if (prevCapacity >= newCapacity) return; // No need to expand, the storage was already large enough.
        // Don't expand strictly to the given requested limit if it's only a very
        // small increase, but instead geometrically grow capacity.
        // For small filesizes (<1MB), perform size*2 geometric increase, but for
        // large sizes, do a much more conservative size*1.125 increase to avoid
        // overshooting the allocation cap by a very large margin.
        var CAPACITY_DOUBLING_MAX = 1024 * 1024;
        newCapacity = Math.max(newCapacity, (prevCapacity * (prevCapacity < CAPACITY_DOUBLING_MAX ? 2.0 : 1.125)) >>> 0);
        if (prevCapacity) newCapacity = Math.max(newCapacity, 256); // At minimum allocate 256b for each file when expanding.
        var oldContents = MEMFS.getFileDataAsTypedArray(node);
        node.contents = new Uint8Array(newCapacity); // Allocate new storage.
        node.contents.set(oldContents);
      },
  resizeFileStorage(node, newSize) {
        if (node.usedBytes == newSize) return;
        var oldContents = node.contents;
        node.contents = new Uint8Array(newSize); // Allocate new storage.
        node.contents.set(oldContents.subarray(0, Math.min(newSize, node.usedBytes))); // Copy old data over to the new storage.
        node.usedBytes = newSize;
      },
  node_ops:{
  getattr(node) {
          var attr = {};
          // device numbers reuse inode numbers.
          attr.dev = FS.isChrdev(node.mode) ? node.id : 1;
          attr.ino = node.id;
          attr.mode = node.mode;
          attr.nlink = 1;
          attr.uid = 0;
          attr.gid = 0;
          attr.rdev = node.rdev;
          if (FS.isDir(node.mode)) {
            attr.size = 4096;
          } else if (FS.isFile(node.mode)) {
            attr.size = node.usedBytes;
          } else if (FS.isLink(node.mode)) {
            attr.size = node.link.length;
          } else {
            attr.size = 0;
          }
          attr.atime = new Date(node.atime);
          attr.mtime = new Date(node.mtime);
          attr.ctime = new Date(node.ctime);
          // NOTE: In our implementation, st_blocks = Math.ceil(st_size/st_blksize),
          //       but this is not required by the standard.
          attr.blksize = 4096;
          attr.blocks = Math.ceil(attr.size / attr.blksize);
          return attr;
        },
  setattr(node, attr) {
          for (const key of ["mode", "atime", "mtime", "ctime"]) {
            if (attr[key] != null) {
              node[key] = attr[key];
            }
          }
          if (attr.size !== undefined) {
            MEMFS.resizeFileStorage(node, attr.size);
          }
        },
  lookup(parent, name) {
          throw new FS.ErrnoError(44);
        },
  mknod(parent, name, mode, dev) {
          return MEMFS.createNode(parent, name, mode, dev);
        },
  rename(old_node, new_dir, new_name) {
          var new_node;
          try {
            new_node = FS.lookupNode(new_dir, new_name);
          } catch (e) {}
          if (new_node) {
            if (FS.isDir(old_node.mode)) {
              // if we're overwriting a directory at new_name, make sure it's empty.
              for (var i in new_node.contents) {
                throw new FS.ErrnoError(55);
              }
            }
            FS.hashRemoveNode(new_node);
          }
          // do the internal rewiring
          delete old_node.parent.contents[old_node.name];
          new_dir.contents[new_name] = old_node;
          old_node.name = new_name;
          new_dir.ctime = new_dir.mtime = old_node.parent.ctime = old_node.parent.mtime = Date.now();
        },
  unlink(parent, name) {
          delete parent.contents[name];
          parent.ctime = parent.mtime = Date.now();
        },
  rmdir(parent, name) {
          var node = FS.lookupNode(parent, name);
          for (var i in node.contents) {
            throw new FS.ErrnoError(55);
          }
          delete parent.contents[name];
          parent.ctime = parent.mtime = Date.now();
        },
  readdir(node) {
          return ['.', '..', ...Object.keys(node.contents)];
        },
  symlink(parent, newname, oldpath) {
          var node = MEMFS.createNode(parent, newname, 0o777 | 40960, 0);
          node.link = oldpath;
          return node;
        },
  readlink(node) {
          if (!FS.isLink(node.mode)) {
            throw new FS.ErrnoError(28);
          }
          return node.link;
        },
  },
  stream_ops:{
  read(stream, buffer, offset, length, position) {
          var contents = stream.node.contents;
          if (position >= stream.node.usedBytes) return 0;
          var size = Math.min(stream.node.usedBytes - position, length);
          assert(size >= 0);
          buffer.set(contents.subarray(position, position + size), offset);
          return size;
        },
  write(stream, buffer, offset, length, position, canOwn) {
          assert(buffer.subarray, 'FS.write expects a TypedArray');
  
          if (!length) return 0;
          var node = stream.node;
          node.mtime = node.ctime = Date.now();
  
          if (canOwn) {
            assert(position === 0, 'canOwn must imply no weird position inside the file');
            node.contents = buffer.subarray(offset, offset + length);
            node.usedBytes = length;
          } else if (node.usedBytes === 0 && position === 0) { // If this is a simple first write to an empty file, do a fast set since we don't need to care about old data.
            node.contents = buffer.slice(offset, offset + length);
            node.usedBytes = length;
          } else {
            MEMFS.expandFileStorage(node, position+length);
            // Use typed array write which is available.
            node.contents.set(buffer.subarray(offset, offset + length), position);
            node.usedBytes = Math.max(node.usedBytes, position + length);
          }
          return length;
        },
  llseek(stream, offset, whence) {
          var position = offset;
          if (whence === 1) {
            position += stream.position;
          } else if (whence === 2) {
            if (FS.isFile(stream.node.mode)) {
              position += stream.node.usedBytes;
            }
          }
          if (position < 0) {
            throw new FS.ErrnoError(28);
          }
          return position;
        },
  mmap(stream, length, position, prot, flags) {
          if (!FS.isFile(stream.node.mode)) {
            throw new FS.ErrnoError(43);
          }
          var ptr;
          var allocated;
          var contents = stream.node.contents;
          // Only make a new copy when MAP_PRIVATE is specified.
          if (!(flags & 2) && contents.buffer === HEAP8.buffer) {
            // We can't emulate MAP_SHARED when the file is not backed by the
            // buffer we're mapping to (e.g. the HEAP buffer).
            allocated = false;
            ptr = contents.byteOffset;
          } else {
            allocated = true;
            ptr = mmapAlloc(length);
            if (!ptr) {
              throw new FS.ErrnoError(48);
            }
            if (contents) {
              // Try to avoid unnecessary slices.
              if (position > 0 || position + length < contents.length) {
                if (contents.subarray) {
                  contents = contents.subarray(position, position + length);
                } else {
                  contents = Array.prototype.slice.call(contents, position, position + length);
                }
              }
              HEAP8.set(contents, ptr);
            }
          }
          return { ptr, allocated };
        },
  msync(stream, buffer, offset, length, mmapFlags) {
          MEMFS.stream_ops.write(stream, buffer, 0, length, offset, false);
          // should we check if bytesWritten and length are the same?
          return 0;
        },
  },
  };
  
  var FS_modeStringToFlags = (str) => {
      if (typeof str != 'string') return str;
      var flagModes = {
        'r': 0,
        'r+': 2,
        'w': 512 | 64 | 1,
        'w+': 512 | 64 | 2,
        'a': 1024 | 64 | 1,
        'a+': 1024 | 64 | 2,
      };
      var flags = flagModes[str];
      if (typeof flags == 'undefined') {
        throw new Error(`Unknown file open mode: ${str}`);
      }
      return flags;
    };
  
  var FS_fileDataToTypedArray = (data) => {
      if (typeof data == 'string') {
        data = intArrayFromString(data, true);
      }
      if (!data.subarray) {
        data = new Uint8Array(data);
      }
      return data;
    };
  
  var FS_getMode = (canRead, canWrite) => {
      var mode = 0;
      if (canRead) mode |= 292 | 73;
      if (canWrite) mode |= 146;
      return mode;
    };
  
  
  
  
  var IDBFS = {
  dbs:{
  },
  indexedDB:() => {
        assert(typeof indexedDB != 'undefined', 'IDBFS used, but indexedDB not supported');
        return indexedDB;
      },
  DB_VERSION:21,
  DB_STORE_NAME:"FILE_DATA",
  queuePersist:(mount) => {
        function onPersistComplete() {
          if (mount.idbPersistState === 'again') startPersist(); // If a new sync request has appeared in between, kick off a new sync
          else {
            mount.idbPersistState = 0; // Otherwise reset sync state back to idle to wait for a new sync later
            IDBFS.onAutoPersistStateChanged?.(false);
          }
        }
        function startPersist() {
          mount.idbPersistState = 'idb'; // Mark that we are currently running a sync operation
          IDBFS.onAutoPersistStateChanged?.(true);
          IDBFS.syncfs(mount, /*populate:*/false, onPersistComplete);
        }
  
        if (!mount.idbPersistState) {
          // Programs typically write/copy/move multiple files in the in-memory
          // filesystem within a single app frame, so when a filesystem sync
          // command is triggered, do not start it immediately, but only after
          // the current frame is finished. This way all the modified files
          // inside the main loop tick will be batched up to the same sync.
          mount.idbPersistState = setTimeout(startPersist, 0);
        } else if (mount.idbPersistState === 'idb') {
          // There is an active IndexedDB sync operation in-flight, but we now
          // have accumulated more files to sync. We should therefore queue up
          // a new sync after the current one finishes so that all writes
          // will be properly persisted.
          mount.idbPersistState = 'again';
        }
      },
  mount:(mount) => {
        // reuse core MEMFS functionality
        var mnt = MEMFS.mount(mount);
        // If the automatic IDBFS persistence option has been selected, then automatically persist
        // all modifications to the filesystem as they occur.
        if (mount?.opts?.autoPersist) {
          mount.idbPersistState = 0; // IndexedDB sync starts in idle state
          var memfs_node_ops = mnt.node_ops;
          mnt.node_ops = {...mnt.node_ops}; // Clone node_ops to inject write tracking
          mnt.node_ops.mknod = (parent, name, mode, dev) => {
            var node = memfs_node_ops.mknod(parent, name, mode, dev);
            // Propagate injected node_ops to the newly created child node
            node.node_ops = mnt.node_ops;
            // Remember for each IDBFS node which IDBFS mount point they came from so we know which mount to persist on modification.
            node.idbfs_mount = mnt.mount;
            // Remember original MEMFS stream_ops for this node
            node.memfs_stream_ops = node.stream_ops;
            // Clone stream_ops to inject write tracking
            node.stream_ops = {...node.stream_ops};
  
            // Track all file writes
            node.stream_ops.write = (stream, buffer, offset, length, position, canOwn) => {
              // This file has been modified, we must persist IndexedDB when this file closes
              stream.node.isModified = true;
              return node.memfs_stream_ops.write(stream, buffer, offset, length, position, canOwn);
            };
  
            // Persist IndexedDB on file close
            node.stream_ops.close = (stream) => {
              var n = stream.node;
              if (n.isModified) {
                IDBFS.queuePersist(n.idbfs_mount);
                n.isModified = false;
              }
              if (n.memfs_stream_ops.close) return n.memfs_stream_ops.close(stream);
            };
  
            // Persist the node we just created to IndexedDB
            IDBFS.queuePersist(mnt.mount);
  
            return node;
          };
          // Also kick off persisting the filesystem on other operations that modify the filesystem.
          mnt.node_ops.rmdir   = (...args) => (IDBFS.queuePersist(mnt.mount), memfs_node_ops.rmdir(...args));
          mnt.node_ops.symlink = (...args) => (IDBFS.queuePersist(mnt.mount), memfs_node_ops.symlink(...args));
          mnt.node_ops.unlink  = (...args) => (IDBFS.queuePersist(mnt.mount), memfs_node_ops.unlink(...args));
          mnt.node_ops.rename  = (...args) => (IDBFS.queuePersist(mnt.mount), memfs_node_ops.rename(...args));
        }
        return mnt;
      },
  syncfs:(mount, populate, callback) => {
        IDBFS.getLocalSet(mount, (err, local) => {
          if (err) return callback(err);
  
          IDBFS.getRemoteSet(mount, (err, remote) => {
            if (err) return callback(err);
  
            var src = populate ? remote : local;
            var dst = populate ? local : remote;
  
            IDBFS.reconcile(src, dst, callback);
          });
        });
      },
  quit:() => {
        for (var value of Object.values(IDBFS.dbs)) {
          value.close()
        }
        IDBFS.dbs = {};
      },
  getDB:(name, callback) => {
        // check the cache first
        var db = IDBFS.dbs[name];
        if (db) {
          return callback(null, db);
        }
  
        var req;
        try {
          req = IDBFS.indexedDB().open(name, IDBFS.DB_VERSION);
        } catch (e) {
          return callback(e);
        }
        if (!req) {
          return callback("Unable to connect to IndexedDB");
        }
        req.onupgradeneeded = (e) => {
          var db = /** @type {IDBDatabase} */ (e.target.result);
          var transaction = e.target.transaction;
  
          var fileStore;
  
          if (db.objectStoreNames.contains(IDBFS.DB_STORE_NAME)) {
            fileStore = transaction.objectStore(IDBFS.DB_STORE_NAME);
          } else {
            fileStore = db.createObjectStore(IDBFS.DB_STORE_NAME);
          }
  
          if (!fileStore.indexNames.contains('timestamp')) {
            fileStore.createIndex('timestamp', 'timestamp', { unique: false });
          }
        };
        req.onsuccess = () => {
          db = /** @type {IDBDatabase} */ (req.result);
  
          // add to the cache
          IDBFS.dbs[name] = db;
          callback(null, db);
        };
        req.onerror = (e) => {
          callback(e.target.error);
          e.preventDefault();
        };
      },
  getLocalSet:(mount, callback) => {
        var entries = {};
  
        function isRealDir(p) {
          return p !== '.' && p !== '..';
        };
        function toAbsolute(root) {
          return (p) => PATH.join2(root, p);
        };
  
        var check = FS.readdir(mount.mountpoint).filter(isRealDir).map(toAbsolute(mount.mountpoint));
  
        while (check.length) {
          var path = check.pop();
          var stat;
  
          try {
            stat = FS.lstat(path);
          } catch (e) {
            return callback(e);
          }
  
          if (FS.isDir(stat.mode)) {
            check.push(...FS.readdir(path).filter(isRealDir).map(toAbsolute(path)));
          }
  
          entries[path] = { 'timestamp': stat.mtime };
        }
  
        return callback(null, { type: 'local', entries: entries });
      },
  getRemoteSet:(mount, callback) => {
        var entries = {};
  
        IDBFS.getDB(mount.mountpoint, (err, db) => {
          if (err) return callback(err);
  
          try {
            var transaction = db.transaction([IDBFS.DB_STORE_NAME], 'readonly');
            transaction.onerror = (e) => {
              callback(e.target.error);
              e.preventDefault();
            };
  
            var store = transaction.objectStore(IDBFS.DB_STORE_NAME);
            var index = store.index('timestamp');
  
            index.openKeyCursor().onsuccess = (event) => {
              var cursor = event.target.result;
  
              if (!cursor) {
                return callback(null, { type: 'remote', db, entries });
              }
  
              entries[cursor.primaryKey] = { 'timestamp': cursor.key };
  
              cursor.continue();
            };
          } catch (e) {
            return callback(e);
          }
        });
      },
  loadLocalEntry:(path, callback) => {
        var stat, node;
  
        try {
          var lookup = FS.lookupPath(path);
          node = lookup.node;
          stat = FS.lstat(path);
        } catch (e) {
          return callback(e);
        }
  
        if (FS.isDir(stat.mode)) {
          return callback(null, { 'timestamp': stat.mtime, 'mode': stat.mode });
        } else if (FS.isLink(stat.mode)) {
          return callback(null, { 'timestamp': stat.mtime, 'mode': stat.mode, 'link': node.link, });
        } else if (FS.isFile(stat.mode)) {
          // Performance consideration: storing a normal JavaScript array to a IndexedDB is much slower than storing a typed array.
          // Therefore always convert the file contents to a typed array first before writing the data to IndexedDB.
          node.contents = MEMFS.getFileDataAsTypedArray(node);
          return callback(null, { 'timestamp': stat.mtime, 'mode': stat.mode, 'contents': node.contents });
        } else {
          return callback(new Error('node type not supported'));
        }
      },
  storeLocalEntry:(path, entry, callback) => {
        try {
          if (FS.isDir(entry['mode'])) {
            FS.mkdirTree(path, entry['mode']);
          } else if (FS.isLink(entry['mode'])) {
            FS.symlink(entry['link'], path);
          } else if (FS.isFile(entry['mode'])) {
            FS.writeFile(path, entry['contents'], { canOwn: true });
          } else {
            return callback(new Error('node type not supported'));
          }
  
          FS.chmod(path, entry['mode']);
          FS.utime(path, entry['timestamp'], entry['timestamp']);
        } catch (e) {
          return callback(e);
        }
  
        callback(null);
      },
  removeLocalEntry:(path, callback) => {
        try {
          var stat = FS.lstat(path);
  
          if (FS.isDir(stat.mode)) {
            FS.rmdir(path);
          } else {
            FS.unlink(path);
          }
        } catch (e) {
          return callback(e);
        }
  
        callback(null);
      },
  loadRemoteEntry:(store, path, callback) => {
        var req = store.get(path);
        req.onsuccess = (event) => callback(null, event.target.result);
        req.onerror = (e) => {
          callback(e.target.error);
          e.preventDefault();
        };
      },
  storeRemoteEntry:(store, path, entry, callback) => {
        try {
          var req = store.put(entry, path);
        } catch (e) {
          callback(e);
          return;
        }
        req.onsuccess = (event) => callback();
        req.onerror = (e) => {
          callback(e.target.error);
          e.preventDefault();
        };
      },
  removeRemoteEntry:(store, path, callback) => {
        var req = store.delete(path);
        req.onsuccess = (event) => callback();
        req.onerror = (e) => {
          callback(e.target.error);
          e.preventDefault();
        };
      },
  reconcile:(src, dst, callback) => {
        var total = 0;
  
        var create = [];
        for (var [key, e] of Object.entries(src.entries)) {
          var e2 = dst.entries[key];
          if (!e2 || e['timestamp'].getTime() != e2['timestamp'].getTime()) {
            create.push(key);
            total++;
          }
        }
  
        var remove = [];
        for (var key of Object.keys(dst.entries)) {
          if (!src.entries[key]) {
            remove.push(key);
            total++;
          }
        }
  
        if (!total) {
          return callback(null);
        }
  
        var errored = false;
        var db = src.type === 'remote' ? src.db : dst.db;
        var transaction = db.transaction([IDBFS.DB_STORE_NAME], 'readwrite');
        var store = transaction.objectStore(IDBFS.DB_STORE_NAME);
  
        function done(err) {
          if (err && !errored) {
            errored = true;
            return callback(err);
          }
        };
  
        // transaction may abort if (for example) there is a QuotaExceededError
        transaction.onerror = transaction.onabort = (e) => {
          done(e.target.error);
          e.preventDefault();
        };
  
        transaction.oncomplete = (e) => {
          if (!errored) {
            callback(null);
          }
        };
  
        // sort paths in ascending order so directory entries are created
        // before the files inside them
        for (const path of create.sort()) {
          if (dst.type === 'local') {
            IDBFS.loadRemoteEntry(store, path, (err, entry) => {
              if (err) return done(err);
              IDBFS.storeLocalEntry(path, entry, done);
            });
          } else {
            IDBFS.loadLocalEntry(path, (err, entry) => {
              if (err) return done(err);
              IDBFS.storeRemoteEntry(store, path, entry, done);
            });
          }
        }
  
        // sort paths in descending order so files are deleted before their
        // parent directories
        for (var path of remove.sort().reverse()) {
          if (dst.type === 'local') {
            IDBFS.removeLocalEntry(path, done);
          } else {
            IDBFS.removeRemoteEntry(store, path, done);
          }
        }
      },
  };
  
  
  
  var strError = (errno) => UTF8ToString(_strerror(errno));
  
  var ERRNO_CODES = {
      'EPERM': 63,
      'ENOENT': 44,
      'ESRCH': 71,
      'EINTR': 27,
      'EIO': 29,
      'ENXIO': 60,
      'E2BIG': 1,
      'ENOEXEC': 45,
      'EBADF': 8,
      'ECHILD': 12,
      'EAGAIN': 6,
      'EWOULDBLOCK': 6,
      'ENOMEM': 48,
      'EACCES': 2,
      'EFAULT': 21,
      'ENOTBLK': 105,
      'EBUSY': 10,
      'EEXIST': 20,
      'EXDEV': 75,
      'ENODEV': 43,
      'ENOTDIR': 54,
      'EISDIR': 31,
      'EINVAL': 28,
      'ENFILE': 41,
      'EMFILE': 33,
      'ENOTTY': 59,
      'ETXTBSY': 74,
      'EFBIG': 22,
      'ENOSPC': 51,
      'ESPIPE': 70,
      'EROFS': 69,
      'EMLINK': 34,
      'EPIPE': 64,
      'EDOM': 18,
      'ERANGE': 68,
      'ENOMSG': 49,
      'EIDRM': 24,
      'ECHRNG': 106,
      'EL2NSYNC': 156,
      'EL3HLT': 107,
      'EL3RST': 108,
      'ELNRNG': 109,
      'EUNATCH': 110,
      'ENOCSI': 111,
      'EL2HLT': 112,
      'EDEADLK': 16,
      'ENOLCK': 46,
      'EBADE': 113,
      'EBADR': 114,
      'EXFULL': 115,
      'ENOANO': 104,
      'EBADRQC': 103,
      'EBADSLT': 102,
      'EDEADLOCK': 16,
      'EBFONT': 101,
      'ENOSTR': 100,
      'ENODATA': 116,
      'ETIME': 117,
      'ENOSR': 118,
      'ENONET': 119,
      'ENOPKG': 120,
      'EREMOTE': 121,
      'ENOLINK': 47,
      'EADV': 122,
      'ESRMNT': 123,
      'ECOMM': 124,
      'EPROTO': 65,
      'EMULTIHOP': 36,
      'EDOTDOT': 125,
      'EBADMSG': 9,
      'ENOTUNIQ': 126,
      'EBADFD': 127,
      'EREMCHG': 128,
      'ELIBACC': 129,
      'ELIBBAD': 130,
      'ELIBSCN': 131,
      'ELIBMAX': 132,
      'ELIBEXEC': 133,
      'ENOSYS': 52,
      'ENOTEMPTY': 55,
      'ENAMETOOLONG': 37,
      'ELOOP': 32,
      'EOPNOTSUPP': 138,
      'EPFNOSUPPORT': 139,
      'ECONNRESET': 15,
      'ENOBUFS': 42,
      'EAFNOSUPPORT': 5,
      'EPROTOTYPE': 67,
      'ENOTSOCK': 57,
      'ENOPROTOOPT': 50,
      'ESHUTDOWN': 140,
      'ECONNREFUSED': 14,
      'EADDRINUSE': 3,
      'ECONNABORTED': 13,
      'ENETUNREACH': 40,
      'ENETDOWN': 38,
      'ETIMEDOUT': 73,
      'EHOSTDOWN': 142,
      'EHOSTUNREACH': 23,
      'EINPROGRESS': 26,
      'EALREADY': 7,
      'EDESTADDRREQ': 17,
      'EMSGSIZE': 35,
      'EPROTONOSUPPORT': 66,
      'ESOCKTNOSUPPORT': 137,
      'EADDRNOTAVAIL': 4,
      'ENETRESET': 39,
      'EISCONN': 30,
      'ENOTCONN': 53,
      'ETOOMANYREFS': 141,
      'EUSERS': 136,
      'EDQUOT': 19,
      'ESTALE': 72,
      'ENOTSUP': 138,
      'ENOMEDIUM': 148,
      'EILSEQ': 25,
      'EOVERFLOW': 61,
      'ECANCELED': 11,
      'ENOTRECOVERABLE': 56,
      'EOWNERDEAD': 62,
      'ESTRPIPE': 135,
    };
  
  var asyncLoad = async (url) => {
      var arrayBuffer = await readAsync(url);
      assert(arrayBuffer, `Loading data file "${url}" failed (no arrayBuffer).`);
      return new Uint8Array(arrayBuffer);
    };
  
  
  var FS_createDataFile = (...args) => FS.createDataFile(...args);
  
  var getUniqueRunDependency = (id) => {
      var orig = id;
      while (1) {
        if (!runDependencyTracking[id]) return id;
        id = orig + Math.random();
      }
    };
  
  var dependenciesPromise = null;
  var resolveRunDependencies = async () => dependenciesPromise;
  var runDependencies = 0;
  
  
  
  var runDependencyTracking = {
  };
  
  var runDependencyWatcher = null;
  var removeRunDependency = (id) => {
      runDependencies--;
  
      Module['monitorRunDependencies']?.(runDependencies);
  
      assert(id, 'removeRunDependency requires an ID');
      assert(runDependencyTracking[id]);
      delete runDependencyTracking[id];
      if (!runDependencies) {
        if (runDependencyWatcher !== null) {
          clearInterval(runDependencyWatcher);
          runDependencyWatcher = null;
        }
        dependenciesPromise.resolve();
      }
    };
  
  
  
  var addRunDependency = (id) => {
      if (!runDependencies) {
        var resolve;
        dependenciesPromise = new Promise((r) => resolve = r);
        dependenciesPromise.resolve = resolve;
      }
      runDependencies++;
  
      Module['monitorRunDependencies']?.(runDependencies);
  
      assert(id, 'addRunDependency requires an ID')
      assert(!runDependencyTracking[id]);
      runDependencyTracking[id] = 1;
      if (runDependencyWatcher === null && globalThis.setInterval) {
        // Check for missing dependencies every few seconds
        runDependencyWatcher = setInterval(() => {
          if (ABORT) {
            clearInterval(runDependencyWatcher);
            runDependencyWatcher = null;
            return;
          }
          var shown = false;
          for (var dep in runDependencyTracking) {
            if (!shown) {
              shown = true;
              err('still waiting on run dependencies:');
            }
            err(`dependency: ${dep}`);
          }
          if (shown) {
            err('(end of list)');
          }
        }, 10000);
        // Prevent this timer from keeping the runtime alive if nothing
        // else is.
        runDependencyWatcher.unref?.()
      }
    };
  
  
  var preloadPlugins = [];
  var FS_handledByPreloadPlugin = async (byteArray, fullname) => {
      // Ensure plugins are ready.
      if (typeof Browser != 'undefined') Browser.init();
  
      for (var plugin of preloadPlugins) {
        if (plugin['canHandle'](fullname)) {
          assert(plugin['handle'].constructor.name === 'AsyncFunction', 'Filesystem plugin handlers must be async functions (See #24914)')
          return plugin['handle'](byteArray, fullname);
        }
      }
      // If no plugin handled this file then return the original/unmodified
      // byteArray.
      return byteArray;
    };
  var FS_preloadFile = async (parent, name, url, canRead, canWrite, dontCreateFile, canOwn, preFinish) => {
      // TODO we should allow people to just pass in a complete filename instead
      // of parent and name being that we just join them anyways
      var fullname = name ? PATH_FS.resolve(PATH.join2(parent, name)) : parent;
      var dep = getUniqueRunDependency(`cp ${fullname}`); // might have several active requests for the same fullname
      addRunDependency(dep);
  
      try {
        var byteArray = url;
        if (typeof url == 'string') {
          byteArray = await asyncLoad(url);
        }
  
        byteArray = await FS_handledByPreloadPlugin(byteArray, fullname);
        preFinish?.();
        if (!dontCreateFile) {
          FS_createDataFile(parent, name, byteArray, canRead, canWrite, canOwn);
        }
      } finally {
        removeRunDependency(dep);
      }
    };
  var FS_createPreloadedFile = (parent, name, url, canRead, canWrite, onload, onerror, dontCreateFile, canOwn, preFinish) => {
      FS_preloadFile(parent, name, url, canRead, canWrite, dontCreateFile, canOwn, preFinish).then(onload).catch(onerror);
    };
  var FS = {
  root:null,
  mounts:[],
  devices:{
  },
  streams:[],
  nextInode:1,
  nameTable:null,
  currentPath:"/",
  initialized:false,
  ignorePermissions:true,
  filesystems:null,
  syncFSRequests:0,
  ErrnoError:class extends Error {
        name = 'ErrnoError';
        // We set the `name` property to be able to identify `FS.ErrnoError`
        // - the `name` is a standard ECMA-262 property of error objects. Kind of good to have it anyway.
        // - when using PROXYFS, an error can come from an underlying FS
        // as different FS objects have their own FS.ErrnoError each,
        // the test `err instanceof FS.ErrnoError` won't detect an error coming from another filesystem, causing bugs.
        // we'll use the reliable test `err.name == "ErrnoError"` instead
        constructor(errno) {
          super(runtimeInitialized ? strError(errno) : '');
          this.errno = errno;
          for (var key in ERRNO_CODES) {
            if (ERRNO_CODES[key] === errno) {
              this.code = key;
              break;
            }
          }
        }
      },
  FSStream:class {
        shared = {};
        get object() {
          return this.node;
        }
        set object(val) {
          this.node = val;
        }
        get isRead() {
          return (this.flags & 2097155) !== 1;
        }
        get isWrite() {
          return (this.flags & 2097155) !== 0;
        }
        get isAppend() {
          return (this.flags & 1024);
        }
        get flags() {
          return this.shared.flags;
        }
        set flags(val) {
          this.shared.flags = val;
        }
        get position() {
          return this.shared.position;
        }
        set position(val) {
          this.shared.position = val;
        }
      },
  FSNode:class {
        node_ops = {};
        stream_ops = {};
        readMode = 292 | 73;
        writeMode = 146;
        mounted = null;
        constructor(parent, name, mode, rdev) {
          if (!parent) {
            parent = this;  // root node sets parent to itself
          }
          this.parent = parent;
          this.mount = parent.mount;
          this.id = FS.nextInode++;
          this.name = name;
          this.mode = mode;
          this.rdev = rdev;
          this.atime = this.mtime = this.ctime = Date.now();
        }
        get read() {
          return (this.mode & this.readMode) === this.readMode;
        }
        set read(val) {
          val ? this.mode |= this.readMode : this.mode &= ~this.readMode;
        }
        get write() {
          return (this.mode & this.writeMode) === this.writeMode;
        }
        set write(val) {
          val ? this.mode |= this.writeMode : this.mode &= ~this.writeMode;
        }
        get isFolder() {
          return FS.isDir(this.mode);
        }
        get isDevice() {
          return FS.isChrdev(this.mode);
        }
      },
  lookupPath(path, opts = {}) {
        if (!path) {
          throw new FS.ErrnoError(44);
        }
        opts.follow_mount ??= true
  
        if (!PATH.isAbs(path)) {
          path = FS.cwd() + '/' + path;
        }
  
        // limit max consecutive symlinks to SYMLOOP_MAX.
        linkloop: for (var nlinks = 0; nlinks < 40; nlinks++) {
          // split the absolute path
          var parts = path.split('/').filter((p) => !!p);
  
          // start at the root
          var current = FS.root;
          var current_path = '/';
  
          for (var i = 0; i < parts.length; i++) {
            var islast = (i === parts.length-1);
            if (islast && opts.parent) {
              // stop resolving
              break;
            }
  
            if (parts[i] === '.') {
              continue;
            }
  
            if (parts[i] === '..') {
              current_path = PATH.dirname(current_path);
              if (FS.isRoot(current)) {
                path = current_path + '/' + parts.slice(i + 1).join('/');
                // We're making progress here, don't let many consecutive ..'s
                // lead to ELOOP
                nlinks--;
                continue linkloop;
              } else {
                current = current.parent;
              }
              continue;
            }
  
            current_path = PATH.join2(current_path, parts[i]);
            try {
              current = FS.lookupNode(current, parts[i]);
            } catch (e) {
              // if noent_okay is true, suppress a ENOENT in the last component
              // and return an object with an undefined node. This is needed for
              // resolving symlinks in the path when creating a file.
              if ((e?.errno === 44) && islast && opts.noent_okay) {
                return { path: current_path };
              }
              throw e;
            }
  
            // jump to the mount's root node if this is a mountpoint
            if (FS.isMountpoint(current) && (!islast || opts.follow_mount)) {
              current = current.mounted.root;
            }
  
            // by default, lookupPath will not follow a symlink if it is the final path component.
            // setting opts.follow = true will override this behavior.
            if (FS.isLink(current.mode) && (!islast || opts.follow)) {
              if (!current.node_ops.readlink) {
                throw new FS.ErrnoError(52);
              }
              var link = current.node_ops.readlink(current);
              if (!PATH.isAbs(link)) {
                link = PATH.dirname(current_path) + '/' + link;
              }
              path = link + '/' + parts.slice(i + 1).join('/');
              continue linkloop;
            }
          }
          return { path: current_path, node: current };
        }
        throw new FS.ErrnoError(32);
      },
  getPath(node) {
        var path;
        while (true) {
          if (FS.isRoot(node)) {
            var mount = node.mount.mountpoint;
            if (!path) return mount;
            return mount[mount.length-1] !== '/' ? `${mount}/${path}` : mount + path;
          }
          path = path ? `${node.name}/${path}` : node.name;
          node = node.parent;
        }
      },
  hashName(parentid, name) {
        var hash = 0;
  
        for (var i = 0; i < name.length; i++) {
          hash = ((hash << 5) - hash + name.charCodeAt(i)) | 0;
        }
        return ((parentid + hash) >>> 0) % FS.nameTable.length;
      },
  hashAddNode(node) {
        var hash = FS.hashName(node.parent.id, node.name);
        node.name_next = FS.nameTable[hash];
        FS.nameTable[hash] = node;
      },
  hashRemoveNode(node) {
        var hash = FS.hashName(node.parent.id, node.name);
        if (FS.nameTable[hash] === node) {
          FS.nameTable[hash] = node.name_next;
        } else {
          var current = FS.nameTable[hash];
          while (current) {
            if (current.name_next === node) {
              current.name_next = node.name_next;
              break;
            }
            current = current.name_next;
          }
        }
      },
  lookupNode(parent, name) {
        var errCode = FS.mayLookup(parent);
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        var hash = FS.hashName(parent.id, name);
        for (var node = FS.nameTable[hash]; node; node = node.name_next) {
          var nodeName = node.name;
          if (node.parent.id === parent.id && nodeName === name) {
            return node;
          }
        }
        // if we failed to find it in the cache, call into the VFS
        return FS.lookup(parent, name);
      },
  createNode(parent, name, mode, rdev) {
        assert(typeof parent == 'object')
        var node = new FS.FSNode(parent, name, mode, rdev);
  
        FS.hashAddNode(node);
  
        return node;
      },
  destroyNode(node) {
        FS.hashRemoveNode(node);
      },
  isRoot(node) {
        return node === node.parent;
      },
  isMountpoint(node) {
        return !!node.mounted;
      },
  isFile(mode) {
        return (mode & 61440) === 32768;
      },
  isDir(mode) {
        return (mode & 61440) === 16384;
      },
  isLink(mode) {
        return (mode & 61440) === 40960;
      },
  isChrdev(mode) {
        return (mode & 61440) === 8192;
      },
  isBlkdev(mode) {
        return (mode & 61440) === 24576;
      },
  isFIFO(mode) {
        return (mode & 61440) === 4096;
      },
  isSocket(mode) {
        return (mode & 49152) === 49152;
      },
  flagsToPermissionString(flag) {
        var perms = ['r', 'w', 'rw'][flag & 3];
        if ((flag & 512)) {
          perms += 'w';
        }
        return perms;
      },
  nodePermissions(node, perms) {
        if (FS.ignorePermissions) {
          return 0;
        }
        // return 0 if any user, group or owner bits are set.
        if (perms.includes('r') && !(node.mode & 292)) {
          return 2;
        }
        if (perms.includes('w') && !(node.mode & 146)) {
          return 2;
        }
        if (perms.includes('x') && !(node.mode & 73)) {
          return 2;
        }
        return 0;
      },
  mayLookup(dir) {
        if (!FS.isDir(dir.mode)) return 54;
        var errCode = FS.nodePermissions(dir, 'x');
        if (errCode) return errCode;
        if (!dir.node_ops.lookup) return 2;
        return 0;
      },
  mayCreate(dir, name) {
        if (!FS.isDir(dir.mode)) {
          return 54;
        }
        try {
          var node = FS.lookupNode(dir, name);
          return 20;
        } catch (e) {
        }
        return FS.nodePermissions(dir, 'wx');
      },
  mayDelete(dir, name, isdir) {
        var node;
        try {
          node = FS.lookupNode(dir, name);
        } catch (e) {
          return e.errno;
        }
        var errCode = FS.nodePermissions(dir, 'wx');
        if (errCode) {
          return errCode;
        }
        if (isdir) {
          if (!FS.isDir(node.mode)) {
            return 54;
          }
          if (FS.isRoot(node) || FS.getPath(node) === FS.cwd()) {
            return 10;
          }
        } else if (FS.isDir(node.mode)) {
          return 31;
        }
        return 0;
      },
  mayOpen(node, flags) {
        if (!node) {
          return 44;
        }
        if (FS.isLink(node.mode)) {
          return 32;
        }
        var mode = FS.flagsToPermissionString(flags);
        if (FS.isDir(node.mode)) {
          // opening for write
          // TODO: check for O_SEARCH? (== search for dir only)
          if (mode !== 'r' || (flags & (512 | 64))) {
            return 31;
          }
        }
        return FS.nodePermissions(node, mode);
      },
  checkOpExists(op, err) {
        if (!op) {
          throw new FS.ErrnoError(err);
        }
        return op;
      },
  MAX_OPEN_FDS:4096,
  nextfd() {
        for (var fd = 0; fd <= FS.MAX_OPEN_FDS; fd++) {
          if (!FS.streams[fd]) {
            return fd;
          }
        }
        throw new FS.ErrnoError(33);
      },
  getStreamChecked(fd) {
        var stream = FS.getStream(fd);
        if (!stream) {
          throw new FS.ErrnoError(8);
        }
        return stream;
      },
  getStream:(fd) => FS.streams[fd],
  createStream(stream, fd = -1) {
        assert(fd >= -1);
  
        // clone it, so we can return an instance of FSStream
        stream = Object.assign(new FS.FSStream(), stream);
        if (fd == -1) {
          fd = FS.nextfd();
        }
        stream.fd = fd;
        FS.streams[fd] = stream;
        return stream;
      },
  closeStream(fd) {
        FS.streams[fd] = null;
      },
  dupStream(origStream, fd = -1) {
        var stream = FS.createStream(origStream, fd);
        stream.stream_ops?.dup?.(stream);
        return stream;
      },
  doSetAttr(stream, node, attr) {
        var setattr = stream?.stream_ops.setattr;
        var arg = setattr ? stream : node;
        setattr ??= node.node_ops.setattr;
        FS.checkOpExists(setattr, 63)
        try {
          setattr(arg, attr);
        } catch (e) {
          if (e instanceof RangeError) {
            throw new FS.ErrnoError(22);
          }
          throw e;
        }
      },
  chrdev_stream_ops:{
  open(stream) {
          var device = FS.getDevice(stream.node.rdev);
          // override node's stream ops with the device's
          stream.stream_ops = device.stream_ops;
          // forward the open call
          stream.stream_ops.open?.(stream);
        },
  llseek() {
          throw new FS.ErrnoError(70);
        },
  },
  major:(dev) => ((dev) >> 8),
  minor:(dev) => ((dev) & 0xff),
  makedev:(ma, mi) => ((ma) << 8 | (mi)),
  registerDevice(dev, ops) {
        FS.devices[dev] = { stream_ops: ops };
      },
  getDevice:(dev) => FS.devices[dev],
  getMounts(mount) {
        var mounts = [];
        var check = [mount];
  
        while (check.length) {
          var m = check.pop();
  
          mounts.push(m);
  
          check.push(...m.mounts);
        }
  
        return mounts;
      },
  syncfs(populate, callback) {
        if (typeof populate == 'function') {
          callback = populate;
          populate = false;
        }
  
        FS.syncFSRequests++;
  
        if (FS.syncFSRequests > 1) {
          err(`warning: ${FS.syncFSRequests} FS.syncfs operations in flight at once, probably just doing extra work`);
        }
  
        var mounts = FS.getMounts(FS.root.mount);
        var completed = 0;
  
        function doCallback(errCode) {
          assert(FS.syncFSRequests > 0);
          FS.syncFSRequests--;
          return callback(errCode);
        }
  
        function done(errCode) {
          if (errCode) {
            if (!done.errored) {
              done.errored = true;
              return doCallback(errCode);
            }
            return;
          }
          if (++completed >= mounts.length) {
            doCallback(null);
          }
        };
  
        // sync all mounts
        for (var mount of mounts) {
          if (mount.type.syncfs) {
            mount.type.syncfs(mount, populate, done);
          } else {
            done(null);
          }
        }
      },
  mount(type, opts, mountpoint) {
        if (typeof type == 'string') {
          // The filesystem was not included, and instead we have an error
          // message stored in the variable.
          throw type;
        }
        var root = mountpoint === '/';
        var pseudo = !mountpoint;
        var node;
  
        if (root && FS.root) {
          throw new FS.ErrnoError(10);
        } else if (!root && !pseudo) {
          var lookup = FS.lookupPath(mountpoint, { follow_mount: false });
  
          mountpoint = lookup.path;  // use the absolute path
          node = lookup.node;
  
          if (FS.isMountpoint(node)) {
            throw new FS.ErrnoError(10);
          }
  
          if (!FS.isDir(node.mode)) {
            throw new FS.ErrnoError(54);
          }
        }
  
        var mount = {
          type,
          opts,
          mountpoint,
          mounts: []
        };
  
        // create a root node for the fs
        var mountRoot = type.mount(mount);
        mountRoot.mount = mount;
        mount.root = mountRoot;
  
        if (root) {
          FS.root = mountRoot;
        } else if (node) {
          // set as a mountpoint
          node.mounted = mount;
  
          // add the new mount to the current mount's children
          if (node.mount) {
            node.mount.mounts.push(mount);
          }
        }
  
        return mountRoot;
      },
  unmount(mountpoint) {
        var lookup = FS.lookupPath(mountpoint, { follow_mount: false });
  
        if (!FS.isMountpoint(lookup.node)) {
          throw new FS.ErrnoError(28);
        }
  
        // destroy the nodes for this mount, and all its child mounts
        var node = lookup.node;
        var mount = node.mounted;
        var mounts = FS.getMounts(mount);
  
        for (var [hash, current] of Object.entries(FS.nameTable)) {
          while (current) {
            var next = current.name_next;
  
            if (mounts.includes(current.mount)) {
              FS.destroyNode(current);
            }
  
            current = next;
          }
        }
  
        // no longer a mountpoint
        node.mounted = null;
  
        // remove this mount from the child mounts
        var idx = node.mount.mounts.indexOf(mount);
        assert(idx !== -1);
        node.mount.mounts.splice(idx, 1);
      },
  lookup(parent, name) {
        return parent.node_ops.lookup(parent, name);
      },
  mknod(path, mode, dev) {
        var lookup = FS.lookupPath(path, { parent: true });
        var parent = lookup.node;
        var name = PATH.basename(path);
        if (!name) {
          throw new FS.ErrnoError(28);
        }
        if (name === '.' || name === '..') {
          throw new FS.ErrnoError(20);
        }
        var errCode = FS.mayCreate(parent, name);
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        if (!parent.node_ops.mknod) {
          throw new FS.ErrnoError(63);
        }
        return parent.node_ops.mknod(parent, name, mode, dev);
      },
  statfs(path) {
        return FS.statfsNode(FS.lookupPath(path, {follow: true}).node);
      },
  statfsStream(stream) {
        // We keep a separate statfsStream function because noderawfs overrides
        // it. In noderawfs, stream.node is sometimes null. Instead, we need to
        // look at stream.path.
        return FS.statfsNode(stream.node);
      },
  statfsNode(node) {
        // NOTE: None of the defaults here are true. We're just returning safe and
        //       sane values. Currently nodefs and rawfs replace these defaults,
        //       other file systems leave them alone.
        var rtn = {
          bsize: 4096,
          frsize: 4096,
          blocks: 1e6,
          bfree: 5e5,
          bavail: 5e5,
          files: FS.nextInode,
          ffree: FS.nextInode - 1,
          fsid: 42,
          flags: 2,
          namelen: 255,
        };
  
        if (node.node_ops.statfs) {
          Object.assign(rtn, node.node_ops.statfs(node.mount.opts.root));
        }
        return rtn;
      },
  create(path, mode = 0o666) {
        mode &= 4095;
        mode |= 32768;
        return FS.mknod(path, mode, 0);
      },
  mkdir(path, mode = 0o777) {
        mode &= 511 | 512;
        mode |= 16384;
        return FS.mknod(path, mode, 0);
      },
  mkdirTree(path, mode) {
        var dirs = path.split('/');
        var d = '';
        for (var dir of dirs) {
          if (!dir) continue;
          if (d || PATH.isAbs(path)) d += '/';
          d += dir;
          try {
            FS.mkdir(d, mode);
          } catch(e) {
            if (e.errno != 20) throw e;
          }
        }
      },
  mkdev(path, mode, dev) {
        if (typeof dev == 'undefined') {
          dev = mode;
          mode = 0o666;
        }
        mode |= 8192;
        return FS.mknod(path, mode, dev);
      },
  symlink(oldpath, newpath) {
        if (!PATH_FS.resolve(oldpath)) {
          throw new FS.ErrnoError(44);
        }
        var lookup = FS.lookupPath(newpath, { parent: true });
        var parent = lookup.node;
        if (!parent) {
          throw new FS.ErrnoError(44);
        }
        var newname = PATH.basename(newpath);
        var errCode = FS.mayCreate(parent, newname);
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        if (!parent.node_ops.symlink) {
          throw new FS.ErrnoError(63);
        }
        return parent.node_ops.symlink(parent, newname, oldpath);
      },
  rename(old_path, new_path) {
        var old_dirname = PATH.dirname(old_path);
        var new_dirname = PATH.dirname(new_path);
        var old_name = PATH.basename(old_path);
        var new_name = PATH.basename(new_path);
        // parents must exist
        var lookup, old_dir, new_dir;
  
        // let the errors from non existent directories percolate up
        lookup = FS.lookupPath(old_path, { parent: true });
        old_dir = lookup.node;
        lookup = FS.lookupPath(new_path, { parent: true });
        new_dir = lookup.node;
  
        if (!old_dir || !new_dir) throw new FS.ErrnoError(44);
        // need to be part of the same mount
        if (old_dir.mount !== new_dir.mount) {
          throw new FS.ErrnoError(75);
        }
        // source must exist
        var old_node = FS.lookupNode(old_dir, old_name);
        // old path should not be an ancestor of the new path
        var relative = PATH_FS.relative(old_path, new_dirname);
        if (relative.charAt(0) !== '.') {
          throw new FS.ErrnoError(28);
        }
        // new path should not be an ancestor of the old path
        relative = PATH_FS.relative(new_path, old_dirname);
        if (relative.charAt(0) !== '.') {
          throw new FS.ErrnoError(55);
        }
        // see if the new path already exists
        var new_node;
        try {
          new_node = FS.lookupNode(new_dir, new_name);
        } catch (e) {
          // not fatal
        }
        // early out if nothing needs to change
        if (old_node === new_node) {
          return;
        }
        // we'll need to delete the old entry
        var isdir = FS.isDir(old_node.mode);
        var errCode = FS.mayDelete(old_dir, old_name, isdir);
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        // need delete permissions if we'll be overwriting.
        // need create permissions if new doesn't already exist.
        errCode = new_node ?
          FS.mayDelete(new_dir, new_name, isdir) :
          FS.mayCreate(new_dir, new_name);
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        if (!old_dir.node_ops.rename) {
          throw new FS.ErrnoError(63);
        }
        if (FS.isMountpoint(old_node) || (new_node && FS.isMountpoint(new_node))) {
          throw new FS.ErrnoError(10);
        }
        // if we are going to change the parent, check write permissions
        if (new_dir !== old_dir) {
          errCode = FS.nodePermissions(old_dir, 'w');
          if (errCode) {
            throw new FS.ErrnoError(errCode);
          }
        }
        // remove the node from the lookup hash
        FS.hashRemoveNode(old_node);
        // do the underlying fs rename
        try {
          old_dir.node_ops.rename(old_node, new_dir, new_name);
          // update old node (we do this here to avoid each backend
          // needing to)
          old_node.parent = new_dir;
        } catch (e) {
          throw e;
        } finally {
          // add the node back to the hash (in case node_ops.rename
          // changed its name)
          FS.hashAddNode(old_node);
        }
      },
  rmdir(path) {
        var lookup = FS.lookupPath(path, { parent: true });
        var parent = lookup.node;
        var name = PATH.basename(path);
        var node = FS.lookupNode(parent, name);
        var errCode = FS.mayDelete(parent, name, true);
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        if (!parent.node_ops.rmdir) {
          throw new FS.ErrnoError(63);
        }
        if (FS.isMountpoint(node)) {
          throw new FS.ErrnoError(10);
        }
        parent.node_ops.rmdir(parent, name);
        FS.destroyNode(node);
      },
  readdir(path) {
        var lookup = FS.lookupPath(path, { follow: true });
        var node = lookup.node;
        var readdir = FS.checkOpExists(node.node_ops.readdir, 54);
        return readdir(node);
      },
  unlink(path) {
        var lookup = FS.lookupPath(path, { parent: true });
        var parent = lookup.node;
        if (!parent) {
          throw new FS.ErrnoError(44);
        }
        var name = PATH.basename(path);
        var node = FS.lookupNode(parent, name);
        var errCode = FS.mayDelete(parent, name, false);
        if (errCode) {
          // According to POSIX, we should map EISDIR to EPERM, but
          // we instead do what Linux does (and we must, as we use
          // the musl linux libc).
          throw new FS.ErrnoError(errCode);
        }
        if (!parent.node_ops.unlink) {
          throw new FS.ErrnoError(63);
        }
        if (FS.isMountpoint(node)) {
          throw new FS.ErrnoError(10);
        }
        parent.node_ops.unlink(parent, name);
        FS.destroyNode(node);
      },
  readlink(path) {
        var lookup = FS.lookupPath(path);
        var link = lookup.node;
        if (!link) {
          throw new FS.ErrnoError(44);
        }
        if (!link.node_ops.readlink) {
          throw new FS.ErrnoError(28);
        }
        return link.node_ops.readlink(link);
      },
  stat(path, dontFollow) {
        var lookup = FS.lookupPath(path, { follow: !dontFollow });
        var node = lookup.node;
        var getattr = FS.checkOpExists(node.node_ops.getattr, 63);
        return getattr(node);
      },
  fstat(fd) {
        var stream = FS.getStreamChecked(fd);
        var node = stream.node;
        var getattr = stream.stream_ops.getattr;
        var arg = getattr ? stream : node;
        getattr ??= node.node_ops.getattr;
        FS.checkOpExists(getattr, 63)
        return getattr(arg);
      },
  lstat(path) {
        return FS.stat(path, true);
      },
  doChmod(stream, node, mode, dontFollow) {
        FS.doSetAttr(stream, node, {
          mode: (mode & 4095) | (node.mode & ~4095),
          ctime: Date.now(),
          dontFollow
        });
      },
  chmod(path, mode, dontFollow) {
        var node;
        if (typeof path == 'string') {
          var lookup = FS.lookupPath(path, { follow: !dontFollow });
          node = lookup.node;
        } else {
          node = path;
        }
        FS.doChmod(null, node, mode, dontFollow);
      },
  lchmod(path, mode) {
        FS.chmod(path, mode, true);
      },
  fchmod(fd, mode) {
        var stream = FS.getStreamChecked(fd);
        FS.doChmod(stream, stream.node, mode, false);
      },
  doChown(stream, node, dontFollow) {
        FS.doSetAttr(stream, node, {
          timestamp: Date.now(),
          dontFollow
          // we ignore the uid / gid for now
        });
      },
  chown(path, uid, gid, dontFollow) {
        var node;
        if (typeof path == 'string') {
          var lookup = FS.lookupPath(path, { follow: !dontFollow });
          node = lookup.node;
        } else {
          node = path;
        }
        FS.doChown(null, node, dontFollow);
      },
  lchown(path, uid, gid) {
        FS.chown(path, uid, gid, true);
      },
  fchown(fd, uid, gid) {
        var stream = FS.getStreamChecked(fd);
        FS.doChown(stream, stream.node, false);
      },
  doTruncate(stream, node, len) {
        if (FS.isDir(node.mode)) {
          throw new FS.ErrnoError(31);
        }
        if (!FS.isFile(node.mode)) {
          throw new FS.ErrnoError(28);
        }
        var errCode = FS.nodePermissions(node, 'w');
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        FS.doSetAttr(stream, node, {
          size: len,
          timestamp: Date.now()
        });
      },
  truncate(path, len) {
        if (len < 0) {
          throw new FS.ErrnoError(28);
        }
        var node;
        if (typeof path == 'string') {
          var lookup = FS.lookupPath(path, { follow: true });
          node = lookup.node;
        } else {
          node = path;
        }
        FS.doTruncate(null, node, len);
      },
  ftruncate(fd, len) {
        var stream = FS.getStreamChecked(fd);
        if (len < 0 || (stream.flags & 2097155) === 0) {
          throw new FS.ErrnoError(28);
        }
        FS.doTruncate(stream, stream.node, len);
      },
  utime(path, atime, mtime) {
        var lookup = FS.lookupPath(path, { follow: true });
        var node = lookup.node;
        var setattr = FS.checkOpExists(node.node_ops.setattr, 63);
        setattr(node, {
          atime: atime,
          mtime: mtime
        });
      },
  open(path, flags, mode = 0o666) {
        if (path === "") {
          throw new FS.ErrnoError(44);
        }
        flags = FS_modeStringToFlags(flags);
        if ((flags & 64)) {
          mode = (mode & 4095) | 32768;
        } else {
          mode = 0;
        }
        var node;
        var isDirPath;
        if (typeof path == 'object') {
          node = path;
        } else {
          isDirPath = path.endsWith("/");
          // noent_okay makes it so that if the final component of the path
          // doesn't exist, lookupPath returns `node: undefined`. `path` will be
          // updated to point to the target of all symlinks.
          var lookup = FS.lookupPath(path, {
            follow: !(flags & 131072),
            noent_okay: true
          });
          node = lookup.node;
          path = lookup.path;
        }
        // perhaps we need to create the node
        var created = false;
        if ((flags & 64)) {
          if (node) {
            // if O_CREAT and O_EXCL are set, error out if the node already exists
            if ((flags & 128)) {
              throw new FS.ErrnoError(20);
            }
          } else if (isDirPath) {
            throw new FS.ErrnoError(31);
          } else {
            // node doesn't exist, try to create it
            // Ignore the permission bits here to ensure we can `open` this new
            // file below. We use chmod below to apply the permissions once the
            // file is open.
            node = FS.mknod(path, mode | 0o777, 0);
            created = true;
          }
        }
        if (!node) {
          throw new FS.ErrnoError(44);
        }
        // can't truncate a device
        if (FS.isChrdev(node.mode)) {
          flags &= ~512;
        }
        // if asked only for a directory, then this must be one
        if ((flags & 65536) && !FS.isDir(node.mode)) {
          throw new FS.ErrnoError(54);
        }
        // check permissions, if this is not a file we just created now (it is ok to
        // create and write to a file with read-only permissions; it is read-only
        // for later use)
        if (!created) {
          var errCode = FS.mayOpen(node, flags);
          if (errCode) {
            throw new FS.ErrnoError(errCode);
          }
        }
        // do truncation if necessary
        if ((flags & 512) && !created) {
          FS.truncate(node, 0);
        }
        // we've already handled these, don't pass down to the underlying vfs
        flags &= ~(128 | 512 | 131072);
  
        // register the stream with the filesystem
        var stream = FS.createStream({
          node,
          path: FS.getPath(node),  // we want the absolute path to the node
          flags,
          seekable: true,
          position: 0,
          stream_ops: node.stream_ops,
          // used by the file family libc calls (fopen, fwrite, ferror, etc.)
          ungotten: [],
          error: false
        });
        // call the new stream's open function
        if (stream.stream_ops.open) {
          stream.stream_ops.open(stream);
        }
        if (created) {
          FS.chmod(node, mode & 0o777);
        }
        return stream;
      },
  close(stream) {
        if (FS.isClosed(stream)) {
          throw new FS.ErrnoError(8);
        }
        if (stream.getdents) stream.getdents = null; // free readdir state
        try {
          if (stream.stream_ops.close) {
            stream.stream_ops.close(stream);
          }
        } catch (e) {
          throw e;
        } finally {
          FS.closeStream(stream.fd);
        }
        stream.fd = null;
      },
  isClosed(stream) {
        return stream.fd === null;
      },
  llseek(stream, offset, whence) {
        if (FS.isClosed(stream)) {
          throw new FS.ErrnoError(8);
        }
        if (!stream.seekable || !stream.stream_ops.llseek) {
          throw new FS.ErrnoError(70);
        }
        if (whence != 0 && whence != 1 && whence != 2) {
          throw new FS.ErrnoError(28);
        }
        stream.position = stream.stream_ops.llseek(stream, offset, whence);
        stream.ungotten = [];
        return stream.position;
      },
  read(stream, buffer, offset, length, position) {
        assert(offset >= 0);
        if (length < 0 || position < 0) {
          throw new FS.ErrnoError(28);
        }
        if (FS.isClosed(stream)) {
          throw new FS.ErrnoError(8);
        }
        if ((stream.flags & 2097155) === 1) {
          throw new FS.ErrnoError(8);
        }
        if (FS.isDir(stream.node.mode)) {
          throw new FS.ErrnoError(31);
        }
        if (!stream.stream_ops.read) {
          throw new FS.ErrnoError(28);
        }
        var seeking = typeof position != 'undefined';
        if (!seeking) {
          position = stream.position;
        } else if (!stream.seekable) {
          throw new FS.ErrnoError(70);
        }
        var bytesRead = stream.stream_ops.read(stream, buffer, offset, length, position);
        if (!seeking) stream.position += bytesRead;
        return bytesRead;
      },
  write(stream, buffer, offset, length, position, canOwn) {
        assert(offset >= 0);
        assert(buffer.subarray, 'FS.write expects a TypedArray');
        if (length < 0 || position < 0) {
          throw new FS.ErrnoError(28);
        }
        if (FS.isClosed(stream)) {
          throw new FS.ErrnoError(8);
        }
        if ((stream.flags & 2097155) === 0) {
          throw new FS.ErrnoError(8);
        }
        if (FS.isDir(stream.node.mode)) {
          throw new FS.ErrnoError(31);
        }
        if (!stream.stream_ops.write) {
          throw new FS.ErrnoError(28);
        }
        if (stream.seekable && stream.flags & 1024) {
          // seek to the end before writing in append mode
          FS.llseek(stream, 0, 2);
        }
        var seeking = typeof position != 'undefined';
        if (!seeking) {
          position = stream.position;
        } else if (!stream.seekable) {
          throw new FS.ErrnoError(70);
        }
        var bytesWritten = stream.stream_ops.write(stream, buffer, offset, length, position, canOwn);
        if (!seeking) stream.position += bytesWritten;
        return bytesWritten;
      },
  mmap(stream, length, position, prot, flags) {
        // User requests writing to file (prot & PROT_WRITE != 0).
        // Checking if we have permissions to write to the file unless
        // MAP_PRIVATE flag is set. According to POSIX spec it is possible
        // to write to file opened in read-only mode with MAP_PRIVATE flag,
        // as all modifications will be visible only in the memory of
        // the current process.
        if ((prot & 2) !== 0
            && (flags & 2) === 0
            && (stream.flags & 2097155) !== 2) {
          throw new FS.ErrnoError(2);
        }
        if ((stream.flags & 2097155) === 1) {
          throw new FS.ErrnoError(2);
        }
        if (!stream.stream_ops.mmap) {
          throw new FS.ErrnoError(43);
        }
        if (!length) {
          throw new FS.ErrnoError(28);
        }
        return stream.stream_ops.mmap(stream, length, position, prot, flags);
      },
  msync(stream, buffer, offset, length, mmapFlags) {
        assert(offset >= 0);
        if (!stream.stream_ops.msync) {
          return 0;
        }
        return stream.stream_ops.msync(stream, buffer, offset, length, mmapFlags);
      },
  ioctl(stream, cmd, arg) {
        if (!stream.stream_ops.ioctl) {
          throw new FS.ErrnoError(59);
        }
        return stream.stream_ops.ioctl(stream, cmd, arg);
      },
  readFile(path, opts = {}) {
        opts.flags = opts.flags ?? 0;
        opts.encoding = opts.encoding ?? 'binary';
        if (opts.encoding !== 'utf8' && opts.encoding !== 'binary') {
          abort(`Invalid encoding type "${opts.encoding}"`);
        }
        var stream = FS.open(path, opts.flags);
        var stat = FS.stat(path);
        var length = stat.size;
        var buf = new Uint8Array(length);
        FS.read(stream, buf, 0, length, 0);
        if (opts.encoding === 'utf8') {
          buf = UTF8ArrayToString(buf);
        }
        FS.close(stream);
        return buf;
      },
  writeFile(path, data, opts = {}) {
        opts.flags = opts.flags ?? 577;
        var stream = FS.open(path, opts.flags, opts.mode);
        data = FS_fileDataToTypedArray(data);
        FS.write(stream, data, 0, data.byteLength, undefined, opts.canOwn);
        FS.close(stream);
      },
  cwd:() => FS.currentPath,
  chdir(path) {
        var lookup = FS.lookupPath(path, { follow: true });
        if (lookup.node === null) {
          throw new FS.ErrnoError(44);
        }
        if (!FS.isDir(lookup.node.mode)) {
          throw new FS.ErrnoError(54);
        }
        var errCode = FS.nodePermissions(lookup.node, 'x');
        if (errCode) {
          throw new FS.ErrnoError(errCode);
        }
        FS.currentPath = lookup.path;
      },
  createDefaultDirectories() {
        FS.mkdir('/tmp');
        FS.mkdir('/home');
        FS.mkdir('/home/web_user');
      },
  createDefaultDevices() {
        // create /dev
        FS.mkdir('/dev');
        // setup /dev/null
        FS.registerDevice(FS.makedev(1, 3), {
          read: () => 0,
          write: (stream, buffer, offset, length, pos) => length,
          llseek: () => 0,
        });
        FS.mkdev('/dev/null', FS.makedev(1, 3));
        // setup /dev/tty and /dev/tty1
        // stderr needs to print output using err() rather than out()
        // so we register a second tty just for it.
        TTY.register(FS.makedev(5, 0), TTY.default_tty_ops);
        TTY.register(FS.makedev(6, 0), TTY.default_tty1_ops);
        FS.mkdev('/dev/tty', FS.makedev(5, 0));
        FS.mkdev('/dev/tty1', FS.makedev(6, 0));
        // setup /dev/[u]random
        // use a buffer to avoid overhead of individual crypto calls per byte
        var randomBuffer = new Uint8Array(1024), randomLeft = 0;
        var randomByte = () => {
          if (randomLeft === 0) {
            randomFill(randomBuffer);
            randomLeft = randomBuffer.byteLength;
          }
          return randomBuffer[--randomLeft];
        };
        FS.createDevice('/dev', 'random', randomByte);
        FS.createDevice('/dev', 'urandom', randomByte);
        // we're not going to emulate the actual shm device,
        // just create the tmp dirs that reside in it commonly
        FS.mkdir('/dev/shm');
        FS.mkdir('/dev/shm/tmp');
      },
  createSpecialDirectories() {
        // create /proc/self/fd which allows /proc/self/fd/6 => readlink gives the
        // name of the stream for fd 6 (see test_unistd_ttyname)
        FS.mkdir('/proc');
        var proc_self = FS.mkdir('/proc/self');
        FS.mkdir('/proc/self/fd');
        FS.mount({
          mount() {
            var node = FS.createNode(proc_self, 'fd', 16895, 73);
            node.stream_ops = {
              llseek: MEMFS.stream_ops.llseek,
            };
            node.node_ops = {
              lookup(parent, name) {
                var fd = +name;
                var stream = FS.getStreamChecked(fd);
                var ret = {
                  parent: null,
                  mount: { mountpoint: 'fake' },
                  node_ops: { readlink: () => stream.path },
                  id: fd + 1,
                };
                ret.parent = ret; // make it look like a simple root node
                return ret;
              },
              readdir() {
                return Array.from(FS.streams.entries())
                  .filter(([k, v]) => v)
                  .map(([k, v]) => k.toString());
              }
            };
            return node;
          }
        }, {}, '/proc/self/fd');
      },
  createStandardStreams(input, output, error) {
        // TODO deprecate the old functionality of a single
        // input / output callback and that utilizes FS.createDevice
        // and instead require a unique set of stream ops
  
        // by default, we symlink the standard streams to the
        // default tty devices. however, if the standard streams
        // have been overwritten we create a unique device for
        // them instead.
        if (input) {
          FS.createDevice('/dev', 'stdin', input);
        } else {
          FS.symlink('/dev/tty', '/dev/stdin');
        }
        if (output) {
          FS.createDevice('/dev', 'stdout', null, output);
        } else {
          FS.symlink('/dev/tty', '/dev/stdout');
        }
        if (error) {
          FS.createDevice('/dev', 'stderr', null, error);
        } else {
          FS.symlink('/dev/tty1', '/dev/stderr');
        }
  
        // open default streams for the stdin, stdout and stderr devices
        var stdin = FS.open('/dev/stdin', 0);
        var stdout = FS.open('/dev/stdout', 1);
        var stderr = FS.open('/dev/stderr', 1);
        assert(stdin.fd === 0, `invalid handle for stdin (${stdin.fd})`);
        assert(stdout.fd === 1, `invalid handle for stdout (${stdout.fd})`);
        assert(stderr.fd === 2, `invalid handle for stderr (${stderr.fd})`);
      },
  staticInit() {
        FS.nameTable = new Array(4096);
  
        FS.mount(MEMFS, {}, '/');
  
        FS.createDefaultDirectories();
        FS.createDefaultDevices();
        FS.createSpecialDirectories();
  
        FS.filesystems = {
          'MEMFS': MEMFS,
          'IDBFS': IDBFS,
        };
      },
  init(input, output, error) {
        assert(!FS.initialized, 'FS.init was previously called. If you want to initialize later with custom parameters, remove any earlier calls (note that one is automatically added to the generated code)');
        FS.initialized = true;
  
        // Allow Module.stdin etc. to provide defaults, if none explicitly passed to us here
        input ??= Module['stdin'];
        output ??= Module['stdout'];
        error ??= Module['stderr'];
  
        FS.createStandardStreams(input, output, error);
      },
  quit() {
        FS.initialized = false;
        // force-flush all streams, so we get musl std streams printed out
        _fflush(0);
        // close all of our streams
        for (var stream of FS.streams) {
          if (stream) {
            FS.close(stream);
          }
        }
      },
  findObject(path, dontResolveLastLink) {
        var ret = FS.analyzePath(path, dontResolveLastLink);
        if (!ret.exists) {
          return null;
        }
        return ret.object;
      },
  analyzePath(path, dontResolveLastLink) {
        // operate from within the context of the symlink's target
        try {
          var lookup = FS.lookupPath(path, { follow: !dontResolveLastLink });
          path = lookup.path;
        } catch (e) {
        }
        var ret = {
          isRoot: false, exists: false, error: 0, name: null, path: null, object: null,
          parentExists: false, parentPath: null, parentObject: null
        };
        try {
          var lookup = FS.lookupPath(path, { parent: true });
          ret.parentExists = true;
          ret.parentPath = lookup.path;
          ret.parentObject = lookup.node;
          ret.name = PATH.basename(path);
          lookup = FS.lookupPath(path, { follow: !dontResolveLastLink });
          ret.exists = true;
          ret.path = lookup.path;
          ret.object = lookup.node;
          ret.name = lookup.node.name;
          ret.isRoot = lookup.path === '/';
        } catch (e) {
          ret.error = e.errno;
        };
        return ret;
      },
  createPath(parent, path, canRead, canWrite) {
        parent = typeof parent == 'string' ? parent : FS.getPath(parent);
        var parts = path.split('/').reverse();
        while (parts.length) {
          var part = parts.pop();
          if (!part) continue;
          var current = PATH.join2(parent, part);
          try {
            FS.mkdir(current);
          } catch (e) {
            if (e.errno != 20) throw e;
          }
          parent = current;
        }
        return current;
      },
  createFile(parent, name, properties, canRead, canWrite) {
        var path = PATH.join2(typeof parent == 'string' ? parent : FS.getPath(parent), name);
        var mode = FS_getMode(canRead, canWrite);
        return FS.create(path, mode);
      },
  createDataFile(parent, name, data, canRead, canWrite, canOwn) {
        var path = name;
        if (parent) {
          parent = typeof parent == 'string' ? parent : FS.getPath(parent);
          path = name ? PATH.join2(parent, name) : parent;
        }
        var mode = FS_getMode(canRead, canWrite);
        var node = FS.create(path, mode);
        if (data) {
          data = FS_fileDataToTypedArray(data);
          // make sure we can write to the file
          FS.chmod(node, mode | 146);
          var stream = FS.open(node, 577);
          FS.write(stream, data, 0, data.length, 0, canOwn);
          FS.close(stream);
          FS.chmod(node, mode);
        }
      },
  createDevice(parent, name, input, output) {
        var path = PATH.join2(typeof parent == 'string' ? parent : FS.getPath(parent), name);
        var mode = FS_getMode(!!input, !!output);
        FS.createDevice.major ??= 64;
        var dev = FS.makedev(FS.createDevice.major++, 0);
        // Create a fake device that a set of stream ops to emulate
        // the old behavior.
        FS.registerDevice(dev, {
          open(stream) {
            stream.seekable = false;
          },
          close(stream) {
            // flush any pending line data
            if (output?.buffer?.length) {
              output(10);
            }
          },
          read(stream, buffer, offset, length, pos /* ignored */) {
            var bytesRead = 0;
            for (var i = 0; i < length; i++) {
              var result;
              try {
                result = input();
              } catch (e) {
                throw new FS.ErrnoError(29);
              }
              if (result === undefined && bytesRead === 0) {
                throw new FS.ErrnoError(6);
              }
              if (result === null || result === undefined) break;
              bytesRead++;
              buffer[offset+i] = result;
            }
            if (bytesRead) {
              stream.node.atime = Date.now();
            }
            return bytesRead;
          },
          write(stream, buffer, offset, length, pos) {
            for (var i = 0; i < length; i++) {
              try {
                output(buffer[offset+i]);
              } catch (e) {
                throw new FS.ErrnoError(29);
              }
            }
            if (length) {
              stream.node.mtime = stream.node.ctime = Date.now();
            }
            return i;
          }
        });
        return FS.mkdev(path, mode, dev);
      },
  forceLoadFile(obj) {
        if (obj.isDevice || obj.isFolder || obj.link || obj.contents) return true;
        if (globalThis.XMLHttpRequest) {
          abort("Lazy loading should have been performed (contents set) in createLazyFile, but it was not. Lazy loading only works in web workers. Use --embed-file or --preload-file in emcc on the main thread.");
        } else { // Command-line.
          try {
            obj.contents = readBinary(obj.url);
          } catch (e) {
            throw new FS.ErrnoError(29);
          }
        }
      },
  createLazyFile(parent, name, url, canRead, canWrite) {
        // Lazy chunked Uint8Array (implements get and length from Uint8Array).
        // Actual getting is abstracted away for eventual reuse.
        class LazyUint8Array {
          lengthKnown = false;
          chunks = []; // Loaded chunks. Index is the chunk number
          get(idx) {
            if (idx > this.length-1 || idx < 0) {
              return undefined;
            }
            var chunkOffset = idx % this.chunkSize;
            var chunkNum = (idx / this.chunkSize)|0;
            return this.getter(chunkNum)[chunkOffset];
          }
          setDataGetter(getter) {
            this.getter = getter;
          }
          cacheLength() {
            // Find length
            var xhr = new XMLHttpRequest();
            xhr.open('HEAD', url, false);
            xhr.send(null);
            if (!(xhr.status >= 200 && xhr.status < 300 || xhr.status === 304)) abort("Couldn't load " + url + ". Status: " + xhr.status);
            var datalength = Number(xhr.getResponseHeader("Content-length"));
            var header;
            var hasByteServing = (header = xhr.getResponseHeader("Accept-Ranges")) && header === "bytes";
            var usesGzip = (header = xhr.getResponseHeader("Content-Encoding")) && header === "gzip";
  
            var chunkSize = 1024*1024; // Chunk size in bytes
  
            if (!hasByteServing) chunkSize = datalength;
  
            // Function to get a range from the remote URL.
            var doXHR = (from, to) => {
              if (from > to) abort(`invalid range (${from}, ${to}) or no bytes requested!`);
              if (to > datalength-1) abort(`only ${datalength} bytes available! programmer error!`);
  
              // TODO: Use mozResponseArrayBuffer, responseStream, etc. if available.
              var xhr = new XMLHttpRequest();
              xhr.open('GET', url, false);
              if (datalength !== chunkSize) xhr.setRequestHeader("Range", "bytes=" + from + "-" + to);
  
              // Some hints to the browser that we want binary data.
              xhr.responseType = 'arraybuffer';
              if (xhr.overrideMimeType) {
                xhr.overrideMimeType('text/plain; charset=x-user-defined');
              }
  
              xhr.send(null);
              if (!(xhr.status >= 200 && xhr.status < 300 || xhr.status === 304)) abort("Couldn't load " + url + ". Status: " + xhr.status);
              if (xhr.response !== undefined) {
                return new Uint8Array(/** @type{Array<number>} */(xhr.response || []));
              }
              return intArrayFromString(xhr.responseText ?? '', true);
            };
            var lazyArray = this;
            lazyArray.setDataGetter((chunkNum) => {
              var start = chunkNum * chunkSize;
              var end = (chunkNum+1) * chunkSize - 1; // including this byte
              end = Math.min(end, datalength-1); // if datalength-1 is selected, this is the last block
              if (typeof lazyArray.chunks[chunkNum] == 'undefined') {
                lazyArray.chunks[chunkNum] = doXHR(start, end);
              }
              if (typeof lazyArray.chunks[chunkNum] == 'undefined') abort('doXHR failed!');
              return lazyArray.chunks[chunkNum];
            });
  
            if (usesGzip || !datalength) {
              // if the server uses gzip or doesn't supply the length, we have to download the whole file to get the (uncompressed) length
              chunkSize = datalength = 1; // this will force getter(0)/doXHR do download the whole file
              datalength = this.getter(0).length;
              chunkSize = datalength;
              out("LazyFiles on gzip forces download of the whole file when length is accessed");
            }
  
            this._length = datalength;
            this._chunkSize = chunkSize;
            this.lengthKnown = true;
          }
          get length() {
            if (!this.lengthKnown) {
              this.cacheLength();
            }
            return this._length;
          }
          get chunkSize() {
            if (!this.lengthKnown) {
              this.cacheLength();
            }
            return this._chunkSize;
          }
        }
  
        if (globalThis.XMLHttpRequest) {
          if (!ENVIRONMENT_IS_WORKER) abort('Cannot do synchronous binary XHRs outside webworkers in modern browsers. Use --embed-file or --preload-file in emcc');
          var lazyArray = new LazyUint8Array();
          var properties = { isDevice: false, contents: lazyArray };
        } else {
          var properties = { isDevice: false, url: url };
        }
  
        var node = FS.createFile(parent, name, properties, canRead, canWrite);
        // This is a total hack, but I want to get this lazy file code out of the
        // core of MEMFS. If we want to keep this lazy file concept I feel it should
        // be its own thin LAZYFS proxying calls to MEMFS.
        if (properties.contents) {
          node.contents = properties.contents;
        } else if (properties.url) {
          node.contents = null;
          node.url = properties.url;
        }
        // Add a function that defers querying the file size until it is asked the first time.
        Object.defineProperties(node, {
          usedBytes: {
            get: function() { return this.contents.length; }
          }
        });
        // override each stream op with one that tries to force load the lazy file first
        var stream_ops = {};
        for (const [key, fn] of Object.entries(node.stream_ops)) {
          stream_ops[key] = (...args) => {
            FS.forceLoadFile(node);
            return fn(...args);
          };
        }
        function writeChunks(stream, buffer, offset, length, position) {
          var contents = stream.node.contents;
          if (position >= contents.length)
            return 0;
          var size = Math.min(contents.length - position, length);
          assert(size >= 0);
          if (contents.slice) { // normal array
            for (var i = 0; i < size; i++) {
              buffer[offset + i] = contents[position + i];
            }
          } else {
            for (var i = 0; i < size; i++) { // LazyUint8Array from sync binary XHR
              buffer[offset + i] = contents.get(position + i);
            }
          }
          return size;
        }
        // use a custom read function
        stream_ops.read = (stream, buffer, offset, length, position) => {
          FS.forceLoadFile(node);
          return writeChunks(stream, buffer, offset, length, position)
        };
        // use a custom mmap function
        stream_ops.mmap = (stream, length, position, prot, flags) => {
          FS.forceLoadFile(node);
          var ptr = mmapAlloc(length);
          if (!ptr) {
            throw new FS.ErrnoError(48);
          }
          writeChunks(stream, HEAP8, ptr, length, position);
          return { ptr, allocated: true };
        };
        node.stream_ops = stream_ops;
        return node;
      },
  };
  var SOCKFS = {
  websocketArgs:{
  },
  callbacks:{
  },
  on(event, callback) {
        SOCKFS.callbacks[event] = callback;
      },
  emit(event, param) {
        SOCKFS.callbacks[event]?.(param);
      },
  mount(mount) {
        // The incoming Module['websocket'] can be used for configuring 
        // subprotocol/url, etc
        SOCKFS.websocketArgs = Module['websocket'] || {};
        // Add the Event registration mechanism to the exported websocket configuration
        // object so we can register network callbacks from native JavaScript too.
        // For more documentation see system/include/emscripten/emscripten.h
        (Module['websocket'] ??= {})['on'] = SOCKFS.on;
  
        return FS.createNode(null, '/', 16895, 0);
      },
  createSocket(family, type, protocol) {
        if (family != 2
           ) {
          throw new FS.ErrnoError(5);
        }
        type &= ~526336; // Some applications may pass it; it makes no sense for a single process.
        // Emscripten only supports SOCK_STREAM and SOCK_DGRAM
        if (type != 1 && type != 2) {
          throw new FS.ErrnoError(28);
        }
        var streaming = type == 1;
        if (streaming && protocol && protocol != 6) {
          throw new FS.ErrnoError(66); // if SOCK_STREAM, must be tcp or 0.
        }
  
        // create our internal socket structure
        var sock = {
          family,
          type,
          protocol,
          server: null,
          error: null, // Used in getsockopt for SOL_SOCKET/SO_ERROR test
          peers: {},
          pending: [],
          recv_queue: [],
          sock_ops: SOCKFS.websocket_sock_ops
        };
  
        // create the filesystem node to store the socket structure
        var name = SOCKFS.nextname();
        var node = FS.createNode(SOCKFS.root, name, 49152, 0);
        node.sock = sock;
  
        // and the wrapping stream that enables library functions such
        // as read and write to indirectly interact with the socket
        var stream = FS.createStream({
          path: name,
          node,
          flags: 2,
          seekable: false,
          stream_ops: SOCKFS.stream_ops
        });
  
        // map the new stream to the socket structure (sockets have a 1:1
        // relationship with a stream)
        sock.stream = stream;
  
        return sock;
      },
  getSocket(fd) {
        var stream = FS.getStream(fd);
        if (!stream || !FS.isSocket(stream.node.mode)) {
          return null;
        }
        return stream.node.sock;
      },
  stream_ops:{
  poll(stream) {
          var sock = stream.node.sock;
          return sock.sock_ops.poll(sock);
        },
  ioctl(stream, request, varargs) {
          var sock = stream.node.sock;
          return sock.sock_ops.ioctl(sock, request, varargs);
        },
  read(stream, buffer, offset, length, position /* ignored */) {
          var sock = stream.node.sock;
          var msg = sock.sock_ops.recvmsg(sock, length);
          if (!msg) {
            // socket is closed
            return 0;
          }
          buffer.set(msg.buffer, offset);
          return msg.buffer.length;
        },
  write(stream, buffer, offset, length, position /* ignored */) {
          var sock = stream.node.sock;
          return sock.sock_ops.sendmsg(sock, buffer, offset, length);
        },
  close(stream) {
          var sock = stream.node.sock;
          sock.sock_ops.close(sock);
        },
  },
  nextname() {
        if (!SOCKFS.nextname.current) {
          SOCKFS.nextname.current = 0;
        }
        return `socket[${SOCKFS.nextname.current++}]`;
      },
  websocket_sock_ops:{
  createPeer(sock, addr, port) {
          var ws;
  
          if (typeof addr == 'object') {
            ws = addr;
            addr = null;
            port = null;
          }
  
          if (ws) {
            // for sockets that've already connected (e.g. we're the server)
            // we can inspect the _socket property for the address
            if (ws._socket) {
              addr = ws._socket.remoteAddress;
              port = ws._socket.remotePort;
            }
            // if we're just now initializing a connection to the remote,
            // inspect the url property
            else {
              var result = /ws[s]?:\/\/([^:]+):(\d+)/.exec(ws.url);
              if (!result) {
                throw new Error('WebSocket URL must be in the format ws(s)://address:port');
              }
              addr = result[1];
              port = parseInt(result[2], 10);
            }
          } else {
            // create the actual websocket object and connect
            try {
              // The default value is 'ws://' the replace is needed because the compiler replaces '//' comments with '#'
              // comments without checking context, so we'd end up with ws:#, the replace swaps the '#' for '//' again.
              var url = 'ws://'.replace('#', '//');
              // Make the WebSocket subprotocol (Sec-WebSocket-Protocol) default to binary if no configuration is set.
              var subProtocols = 'binary'; // The default value is 'binary'
              // The default WebSocket options
              var opts = undefined;
  
              // Fetch runtime WebSocket URL config.
              if (SOCKFS.websocketArgs['url']) {
                url = SOCKFS.websocketArgs['url'];
              }
              // Fetch runtime WebSocket subprotocol config.
              if (SOCKFS.websocketArgs['subprotocol']) {
                subProtocols = SOCKFS.websocketArgs['subprotocol'];
              } else if (SOCKFS.websocketArgs['subprotocol'] === null) {
                subProtocols = 'null'
              }
  
              if (url === 'ws://' || url === 'wss://') { // Is the supplied URL config just a prefix, if so complete it.
                var parts = addr.split('/');
                url = url + parts[0] + ":" + port + "/" + parts.slice(1).join('/');
              }
  
              if (subProtocols !== 'null') {
                // The regex trims the string (removes spaces at the beginning and end), then splits the string by
                // <any space>,<any space> into an Array. Whitespace removal is important for Websockify and ws.
                subProtocols = subProtocols.replace(/^ +| +$/g,"").split(/ *, */);
  
                opts = subProtocols;
              }
  
              // If node we use the ws library.
              var WebSocketConstructor;
              if (ENVIRONMENT_IS_NODE) {
                WebSocketConstructor = /** @type{(typeof WebSocket)} */(require('ws'));
              } else
              {
                WebSocketConstructor = WebSocket;
              }
              ws = new WebSocketConstructor(url, opts);
              ws.binaryType = 'arraybuffer';
            } catch (e) {
              throw new FS.ErrnoError(23);
            }
          }
  
          var peer = {
            addr,
            port,
            socket: ws,
            msg_send_queue: []
          };
  
          SOCKFS.websocket_sock_ops.addPeer(sock, peer);
          SOCKFS.websocket_sock_ops.handlePeerEvents(sock, peer);
  
          // if this is a bound dgram socket, send the port number first to allow
          // us to override the ephemeral port reported to us by remotePort on the
          // remote end.
          if (sock.type === 2 && typeof sock.sport != 'undefined') {
            peer.msg_send_queue.push(new Uint8Array([
                255, 255, 255, 255,
                'p'.charCodeAt(0), 'o'.charCodeAt(0), 'r'.charCodeAt(0), 't'.charCodeAt(0),
                ((sock.sport & 0xff00) >> 8) , (sock.sport & 0xff)
            ]));
          }
  
          return peer;
        },
  getPeer(sock, addr, port) {
          return sock.peers[addr + ':' + port];
        },
  addPeer(sock, peer) {
          sock.peers[peer.addr + ':' + peer.port] = peer;
        },
  removePeer(sock, peer) {
          delete sock.peers[peer.addr + ':' + peer.port];
        },
  handlePeerEvents(sock, peer) {
          var first = true;
  
          function handleOpen() {
  
            sock.connecting = false;
            SOCKFS.emit('open', sock.stream.fd);
  
            try {
              var queued = peer.msg_send_queue.shift();
              while (queued) {
                peer.socket.send(queued);
                queued = peer.msg_send_queue.shift();
              }
            } catch (e) {
              // not much we can do here in the way of proper error handling as we've already
              // lied and said this data was sent. shut it down.
              peer.socket.close();
            }
          }
  
          function handleMessage(data) {
            if (typeof data == 'string') {
              var encoder = new TextEncoder(); // should be utf-8
              data = encoder.encode(data); // make a typed array from the string
            } else {
              assert(data.byteLength !== undefined); // must receive an ArrayBuffer
              if (data.byteLength == 0) {
                // An empty ArrayBuffer will emit a pseudo disconnect event
                // as recv/recvmsg will return zero which indicates that a socket
                // has performed a shutdown although the connection has not been disconnected yet.
                return;
              }
              data = new Uint8Array(data); // make a typed array view on the array buffer
            }
  
            // if this is the port message, override the peer's port with it
            var wasfirst = first;
            first = false;
            if (wasfirst &&
                data.length === 10 &&
                data[0] === 255 && data[1] === 255 && data[2] === 255 && data[3] === 255 &&
                data[4] === 'p'.charCodeAt(0) && data[5] === 'o'.charCodeAt(0) && data[6] === 'r'.charCodeAt(0) && data[7] === 't'.charCodeAt(0)) {
              // update the peer's port and its key in the peer map
              var newport = ((data[8] << 8) | data[9]);
              SOCKFS.websocket_sock_ops.removePeer(sock, peer);
              peer.port = newport;
              SOCKFS.websocket_sock_ops.addPeer(sock, peer);
              return;
            }
  
            sock.recv_queue.push({ addr: peer.addr, port: peer.port, data: data });
            SOCKFS.emit('message', sock.stream.fd);
          }
  
          if (ENVIRONMENT_IS_NODE) {
             // EventEmitter-style events use by ws library objects in Node.js).
            peer.socket.on('open', handleOpen);
            peer.socket.on('message', (data, isBinary) => {
              if (!isBinary) {
                return;
              }
              handleMessage((new Uint8Array(data)).buffer); // copy from node Buffer -> ArrayBuffer
            });
            peer.socket.on('close', () => SOCKFS.emit('close', sock.stream.fd));
            peer.socket.on('error', (error) =>{
              // Although the ws library may pass errors that may be more descriptive than
              // ECONNREFUSED they are not necessarily the expected error code e.g.
              // ENOTFOUND on getaddrinfo seems to be node.js specific, so using ECONNREFUSED
              // is still probably the most useful thing to do.
              sock.error = 14; // Used in getsockopt for SOL_SOCKET/SO_ERROR test.
              SOCKFS.emit('error', [sock.stream.fd, sock.error, 'ECONNREFUSED: Connection refused']);
            });
            return;
          }
          peer.socket.onopen = handleOpen;
          peer.socket.onclose = () => SOCKFS.emit('close', sock.stream.fd);
          peer.socket.onmessage = (event) => handleMessage(event.data);
          peer.socket.onerror = (error) => {
            // The WebSocket spec only allows a 'simple event' to be thrown on error,
            // so we only really know as much as ECONNREFUSED.
            sock.error = 14; // Used in getsockopt for SOL_SOCKET/SO_ERROR test.
            SOCKFS.emit('error', [sock.stream.fd, sock.error, 'ECONNREFUSED: Connection refused']);
          };
        },
  poll(sock) {
          if (sock.type === 1 && sock.server) {
            // listen sockets should only say they're available for reading
            // if there are pending clients.
            return sock.pending.length ? (64 | 1) : 0;
          }
  
          var mask = 0;
          var dest = sock.type === 1 ?  // we only care about the socket state for connection-based sockets
            SOCKFS.websocket_sock_ops.getPeer(sock, sock.daddr, sock.dport) :
            null;
  
          if (sock.recv_queue.length ||
              !dest ||  // connection-less sockets are always ready to read
              (dest && dest.socket.readyState === dest.socket.CLOSING) ||
              (dest && dest.socket.readyState === dest.socket.CLOSED)) {  // let recv return 0 once closed
            mask |= (64 | 1);
          }
  
          if (!dest ||  // connection-less sockets are always ready to write
              (dest && dest.socket.readyState === dest.socket.OPEN)) {
            mask |= 4;
          }
  
          if ((dest && dest.socket.readyState === dest.socket.CLOSING) ||
              (dest && dest.socket.readyState === dest.socket.CLOSED)) {
            // When an non-blocking connect fails mark the socket as writable.
            // Its up to the calling code to then use getsockopt with SO_ERROR to
            // retrieve the error.
            // See https://man7.org/linux/man-pages/man2/connect.2.html
            if (sock.connecting) {
              mask |= 4;
            } else  {
              mask |= 16;
            }
          }
  
          return mask;
        },
  ioctl(sock, request, arg) {
          switch (request) {
            case 21531:
              var bytes = 0;
              if (sock.recv_queue.length) {
                bytes = sock.recv_queue[0].data.length;
              }
              HEAP32[((arg)>>2)] = bytes;
              return 0;
            case 21537:
              var on = HEAP32[((arg)>>2)];
              if (on) {
                sock.stream.flags |= 2048;
              } else {
                sock.stream.flags &= ~2048;
              }
              return 0;
            default:
              return 28;
          }
        },
  close(sock) {
          // if we've spawned a listen server, close it
          if (sock.server) {
            try {
              sock.server.close();
            } catch (e) {
            }
            sock.server = null;
          }
          // close any peer connections
          for (var peer of Object.values(sock.peers)) {
            try {
              peer.socket.close();
            } catch (e) {
            }
            SOCKFS.websocket_sock_ops.removePeer(sock, peer);
          }
          return 0;
        },
  bind(sock, addr, port) {
          if (typeof sock.saddr != 'undefined' || typeof sock.sport != 'undefined') {
            throw new FS.ErrnoError(28);  // already bound
          }
          sock.saddr = addr;
          sock.sport = port;
          // in order to emulate dgram sockets, we need to launch a listen server when
          // binding on a connection-less socket
          // note: this is only required on the server side
          if (sock.type === 2) {
            // close the existing server if it exists
            if (sock.server) {
              sock.server.close();
              sock.server = null;
            }
            // swallow error operation not supported error that occurs when binding in the
            // browser where this isn't supported
            try {
              sock.sock_ops.listen(sock, 0);
            } catch (e) {
              if (!(e.name === 'ErrnoError')) throw e;
              if (e.errno !== 138) throw e;
            }
          }
        },
  connect(sock, addr, port) {
          if (sock.server) {
            throw new FS.ErrnoError(138);
          }
  
          // TODO autobind
          // if (!sock.addr && sock.type == 2) {
          // }
  
          // early out if we're already connected / in the middle of connecting
          if (typeof sock.daddr != 'undefined' && typeof sock.dport != 'undefined') {
            var dest = SOCKFS.websocket_sock_ops.getPeer(sock, sock.daddr, sock.dport);
            if (dest) {
              if (dest.socket.readyState === dest.socket.CONNECTING) {
                throw new FS.ErrnoError(7);
              } else {
                throw new FS.ErrnoError(30);
              }
            }
          }
  
          // add the socket to our peer list and set our
          // destination address / port to match
          var peer = SOCKFS.websocket_sock_ops.createPeer(sock, addr, port);
          sock.daddr = peer.addr;
          sock.dport = peer.port;
  
          // because we cannot synchronously block to wait for the WebSocket
          // connection to complete, we return here pretending that the connection
          // was a success.
          sock.connecting = true;
        },
  listen(sock, backlog) {
          if (!ENVIRONMENT_IS_NODE) {
            throw new FS.ErrnoError(138);
          }
          if (sock.server) {
             throw new FS.ErrnoError(28);  // already listening
          }
          var WebSocketServer = require('ws').Server;
          var host = sock.saddr;
          sock.server = new WebSocketServer({
            host,
            port: sock.sport
            // TODO support backlog
          });
          SOCKFS.emit('listen', sock.stream.fd); // Send Event with listen fd.
  
          sock.server.on('connection', (ws) => {
            if (sock.type === 1) {
              var newsock = SOCKFS.createSocket(sock.family, sock.type, sock.protocol);
  
              // create a peer on the new socket
              var peer = SOCKFS.websocket_sock_ops.createPeer(newsock, ws);
              newsock.daddr = peer.addr;
              newsock.dport = peer.port;
  
              // push to queue for accept to pick up
              sock.pending.push(newsock);
              SOCKFS.emit('connection', newsock.stream.fd);
            } else {
              // create a peer on the listen socket so calling sendto
              // with the listen socket and an address will resolve
              // to the correct client
              SOCKFS.websocket_sock_ops.createPeer(sock, ws);
              SOCKFS.emit('connection', sock.stream.fd);
            }
          });
          sock.server.on('close', () => {
            SOCKFS.emit('close', sock.stream.fd);
            sock.server = null;
          });
          sock.server.on('error', (error) => {
            // Although the ws library may pass errors that may be more descriptive than
            // ECONNREFUSED they are not necessarily the expected error code e.g.
            // ENOTFOUND on getaddrinfo seems to be node.js specific, so using EHOSTUNREACH
            // is still probably the most useful thing to do. This error shouldn't
            // occur in a well written app as errors should get trapped in the compiled
            // app's own getaddrinfo call.
            sock.error = 23; // Used in getsockopt for SOL_SOCKET/SO_ERROR test.
            SOCKFS.emit('error', [sock.stream.fd, sock.error, 'EHOSTUNREACH: Host is unreachable']);
            // don't throw
          });
        },
  accept(listensock) {
          if (!listensock.server || !listensock.pending.length) {
            throw new FS.ErrnoError(28);
          }
          var newsock = listensock.pending.shift();
          newsock.stream.flags = listensock.stream.flags;
          return newsock;
        },
  getname(sock, peer) {
          var addr, port;
          if (peer) {
            if (sock.daddr === undefined || sock.dport === undefined) {
              throw new FS.ErrnoError(53);
            }
            addr = sock.daddr;
            port = sock.dport;
          } else {
            // TODO saddr and sport will be set for bind()'d UDP sockets, but what
            // should we be returning for TCP sockets that've been connect()'d?
            addr = sock.saddr || 0;
            port = sock.sport || 0;
          }
          return { addr, port };
        },
  sendmsg(sock, buffer, offset, length, addr, port) {
          if (sock.type === 2) {
            // connection-less sockets will honor the message address,
            // and otherwise fall back to the bound destination address
            if (addr === undefined || port === undefined) {
              addr = sock.daddr;
              port = sock.dport;
            }
            // if there was no address to fall back to, error out
            if (addr === undefined || port === undefined) {
              throw new FS.ErrnoError(17);
            }
          } else {
            // connection-based sockets will only use the bound
            addr = sock.daddr;
            port = sock.dport;
          }
  
          // find the peer for the destination address
          var dest = SOCKFS.websocket_sock_ops.getPeer(sock, addr, port);
  
          // early out if not connected with a connection-based socket
          if (sock.type === 1) {
            if (!dest || dest.socket.readyState === dest.socket.CLOSING || dest.socket.readyState === dest.socket.CLOSED) {
              throw new FS.ErrnoError(53);
            }
          }
  
          // create a copy of the incoming data to send, as the WebSocket API
          // doesn't work entirely with an ArrayBufferView, it'll just send
          // the entire underlying buffer
          if (ArrayBuffer.isView(buffer)) {
            offset += buffer.byteOffset;
            buffer = buffer.buffer;
          }
  
          var data = buffer.slice(offset, offset + length);
          // WebSockets .send() does not allow passing a SharedArrayBuffer, so
          // clone the SharedArrayBuffer as regular ArrayBuffer before
          // sending.
          if (data instanceof SharedArrayBuffer) {
            data = new Uint8Array(new Uint8Array(data)).buffer;
          }
  
          // if we don't have a cached connectionless UDP datagram connection, or
          // the TCP socket is still connecting, queue the message to be sent upon
          // connect, and lie, saying the data was sent now.
          if (!dest || dest.socket.readyState !== dest.socket.OPEN) {
            // if we're not connected, open a new connection
            if (sock.type === 2) {
              if (!dest || dest.socket.readyState === dest.socket.CLOSING || dest.socket.readyState === dest.socket.CLOSED) {
                dest = SOCKFS.websocket_sock_ops.createPeer(sock, addr, port);
              }
            }
            dest.msg_send_queue.push(data);
            return length;
          }
  
          try {
            // send the actual data
            dest.socket.send(data);
            return length;
          } catch (e) {
            throw new FS.ErrnoError(28);
          }
        },
  recvmsg(sock, length) {
          // http://pubs.opengroup.org/onlinepubs/7908799/xns/recvmsg.html
          if (sock.type === 1 && sock.server) {
            // tcp servers should not be recv()'ing on the listen socket
            throw new FS.ErrnoError(53);
          }
  
          var queued = sock.recv_queue.shift();
          if (!queued) {
            if (sock.type === 1) {
              var dest = SOCKFS.websocket_sock_ops.getPeer(sock, sock.daddr, sock.dport);
  
              if (!dest) {
                // if we have a destination address but are not connected, error out
                throw new FS.ErrnoError(53);
              }
              if (dest.socket.readyState === dest.socket.CLOSING || dest.socket.readyState === dest.socket.CLOSED) {
                // return null if the socket has closed
                return null;
              }
              // else, our socket is in a valid state but truly has nothing available
              throw new FS.ErrnoError(6);
            }
            throw new FS.ErrnoError(6);
          }
  
          // queued.data will be an ArrayBuffer if it's unadulterated, but if it's
          // requeued TCP data it'll be an ArrayBufferView
          var queuedLength = queued.data.byteLength || queued.data.length;
          var queuedOffset = queued.data.byteOffset || 0;
          var queuedBuffer = queued.data.buffer || queued.data;
          var bytesRead = Math.min(length, queuedLength);
          var res = {
            buffer: new Uint8Array(queuedBuffer, queuedOffset, bytesRead),
            addr: queued.addr,
            port: queued.port
          };
  
          // push back any unread data for TCP connections
          if (sock.type === 1 && bytesRead < queuedLength) {
            var bytesRemaining = queuedLength - bytesRead;
            queued.data = new Uint8Array(queuedBuffer, queuedOffset + bytesRead, bytesRemaining);
            sock.recv_queue.unshift(queued);
          }
  
          return res;
        },
  },
  };
  
  var getSocketFromFD = (fd) => {
      var socket = SOCKFS.getSocket(fd);
      if (!socket) throw new FS.ErrnoError(8);
      return socket;
    };
  
  var inetPton4 = (str) => {
      var b = str.split('.');
      for (var i = 0; i < 4; i++) {
        var tmp = Number(b[i]);
        if (isNaN(tmp)) return null;
        b[i] = tmp;
      }
      return (b[0] | (b[1] << 8) | (b[2] << 16) | (b[3] << 24)) >>> 0;
    };
  
  var inetPton6 = (str) => {
      var words;
      var w, offset, z, i;
      /* http://home.deds.nl/~aeron/regex/ */
      var valid6regx = /^((?=.*::)(?!.*::.+::)(::)?([\dA-F]{1,4}:(:|\b)|){5}|([\dA-F]{1,4}:){6})((([\dA-F]{1,4}((?!\3)::|:\b|$))|(?!\2\3)){2}|(((2[0-4]|1\d|[1-9])?\d|25[0-5])\.?\b){4})$/i
      var parts = [];
      if (!valid6regx.test(str)) {
        return null;
      }
      if (str === "::") {
        return [0, 0, 0, 0, 0, 0, 0, 0];
      }
      // Z placeholder to keep track of zeros when splitting the string on ":"
      if (str.startsWith("::")) {
        str = str.replace("::", "Z:"); // leading zeros case
      } else {
        str = str.replace("::", ":Z:");
      }
  
      if (str.indexOf(".") > 0) {
        // parse IPv4 embedded address
        str = str.replace(new RegExp('[.]', 'g'), ":");
        words = str.split(":");
        words[words.length-4] = Number(words[words.length-4]) + Number(words[words.length-3])*256;
        words[words.length-3] = Number(words[words.length-2]) + Number(words[words.length-1])*256;
        words = words.slice(0, words.length-2);
      } else {
        words = str.split(":");
      }
  
      offset = 0; z = 0;
      for (w=0; w < words.length; w++) {
        if (typeof words[w] == 'string') {
          if (words[w] === 'Z') {
            // compressed zeros - write appropriate number of zero words
            for (z = 0; z < (8 - words.length+1); z++) {
              parts[w+z] = 0;
            }
            offset = z-1;
          } else {
            // parse hex field to 16-bit value and write it in network byte-order
            parts[w+offset] = _htons(parseInt(words[w],16));
          }
        } else {
          // parsed IPv4 words
          parts[w+offset] = words[w];
        }
      }
      return [
        (parts[1] << 16) | parts[0],
        (parts[3] << 16) | parts[2],
        (parts[5] << 16) | parts[4],
        (parts[7] << 16) | parts[6]
      ];
    };
  
  var zeroMemory = (ptr, size) => HEAPU8.fill(0, ptr, ptr + size);
  
  /** @param {number=} addrlen */
  var writeSockaddr = (sa, family, addr, port, addrlen) => {
      switch (family) {
        case 2:
          addr = inetPton4(addr);
          zeroMemory(sa, 16);
          if (addrlen) {
            HEAP32[((addrlen)>>2)] = 16;
          }
          HEAP16[((sa)>>1)] = family;
          HEAP32[(((sa)+(4))>>2)] = addr;
          HEAP16[(((sa)+(2))>>1)] = _htons(port);
          break;
        case 10:
          addr = inetPton6(addr);
          zeroMemory(sa, 28);
          if (addrlen) {
            HEAP32[((addrlen)>>2)] = 28;
          }
          HEAP32[((sa)>>2)] = family;
          HEAP32[(((sa)+(8))>>2)] = addr[0];
          HEAP32[(((sa)+(12))>>2)] = addr[1];
          HEAP32[(((sa)+(16))>>2)] = addr[2];
          HEAP32[(((sa)+(20))>>2)] = addr[3];
          HEAP16[(((sa)+(2))>>1)] = _htons(port);
          break;
        default:
          return 5;
      }
      return 0;
    };
  
  
  var DNS = {
  address_map:{
  id:1,
  addrs:{
  },
  names:{
  },
  },
  lookup_name(name) {
        // If the name is already a valid ipv4 / ipv6 address, don't generate a fake one.
        var res = inetPton4(name);
        if (res !== null) {
          return name;
        }
        res = inetPton6(name);
        if (res !== null) {
          return name;
        }
  
        // See if this name is already mapped.
        var addr;
  
        if (DNS.address_map.addrs[name]) {
          addr = DNS.address_map.addrs[name];
        } else {
          var id = DNS.address_map.id++;
          assert(id < 65535, 'exceeded max address mappings of 65535');
  
          addr = '172.29.' + (id & 0xff) + '.' + (id & 0xff00);
  
          DNS.address_map.names[addr] = name;
          DNS.address_map.addrs[name] = addr;
        }
  
        return addr;
      },
  lookup_addr(addr) {
        if (DNS.address_map.names[addr]) {
          return DNS.address_map.names[addr];
        }
  
        return null;
      },
  };
  
  
  function ___syscall_accept4(fd, addr, len, flags, u1, u2) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(3, 0, 1, fd, addr, len, flags, u1, u2);
  
  try {
  
      var sock = getSocketFromFD(fd);
      var newsock = sock.sock_ops.accept(sock);
      if (addr) {
        var errno = writeSockaddr(addr, newsock.family, DNS.lookup_name(newsock.daddr), newsock.dport, len);
        assert(!errno);
      }
      return newsock.stream.fd;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  var inetNtop4 = (addr) =>
      (addr & 0xff) + '.' + ((addr >> 8) & 0xff) + '.' + ((addr >> 16) & 0xff) + '.' + ((addr >> 24) & 0xff);
  
  
  var inetNtop6 = (ints) => {
      //  ref:  http://www.ietf.org/rfc/rfc2373.txt - section 2.5.4
      //  Format for IPv4 compatible and mapped  128-bit IPv6 Addresses
      //  128-bits are split into eight 16-bit words
      //  stored in network byte order (big-endian)
      //  |                80 bits               | 16 |      32 bits        |
      //  +-----------------------------------------------------------------+
      //  |               10 bytes               |  2 |      4 bytes        |
      //  +--------------------------------------+--------------------------+
      //  +               5 words                |  1 |      2 words        |
      //  +--------------------------------------+--------------------------+
      //  |0000..............................0000|0000|    IPv4 ADDRESS     | (compatible)
      //  +--------------------------------------+----+---------------------+
      //  |0000..............................0000|FFFF|    IPv4 ADDRESS     | (mapped)
      //  +--------------------------------------+----+---------------------+
      var str = "";
      var word = 0;
      var longest = 0;
      var lastzero = 0;
      var zstart = 0;
      var len = 0;
      var i = 0;
      var parts = [
        ints[0] & 0xffff,
        (ints[0] >> 16),
        ints[1] & 0xffff,
        (ints[1] >> 16),
        ints[2] & 0xffff,
        (ints[2] >> 16),
        ints[3] & 0xffff,
        (ints[3] >> 16)
      ];
  
      // Handle IPv4-compatible, IPv4-mapped, loopback and any/unspecified addresses
  
      var hasipv4 = true;
      var v4part = "";
      // check if the 10 high-order bytes are all zeros (first 5 words)
      for (i = 0; i < 5; i++) {
        if (parts[i] !== 0) { hasipv4 = false; break; }
      }
  
      if (hasipv4) {
        // low-order 32-bits store an IPv4 address (bytes 13 to 16) (last 2 words)
        v4part = inetNtop4(parts[6] | (parts[7] << 16));
        // IPv4-mapped IPv6 address if 16-bit value (bytes 11 and 12) == 0xFFFF (6th word)
        if (parts[5] === -1) {
          str = "::ffff:";
          str += v4part;
          return str;
        }
        // IPv4-compatible IPv6 address if 16-bit value (bytes 11 and 12) == 0x0000 (6th word)
        if (parts[5] === 0) {
          str = "::";
          // special case IPv6 addresses
          if (v4part === "0.0.0.0") v4part = ""; // any/unspecified address
          if (v4part === "0.0.0.1") v4part = "1";// loopback address
          str += v4part;
          return str;
        }
      }
  
      // Handle all other IPv6 addresses
  
      // first run to find the longest contiguous zero words
      for (word = 0; word < 8; word++) {
        if (parts[word] === 0) {
          if (word - lastzero > 1) {
            len = 0;
          }
          lastzero = word;
          len++;
        }
        if (len > longest) {
          longest = len;
          zstart = word - longest + 1;
        }
      }
  
      for (word = 0; word < 8; word++) {
        if (longest > 1) {
          // compress contiguous zeros - to produce "::"
          if (parts[word] === 0 && word >= zstart && word < (zstart + longest) ) {
            if (word === zstart) {
              str += ":";
              if (zstart === 0) str += ":"; //leading zeros case
            }
            continue;
          }
        }
        // converts 16-bit words from big-endian to little-endian before converting to hex string
        str += Number(_ntohs(parts[word] & 0xffff)).toString(16);
        str += word < 7 ? ":" : "";
      }
      return str;
    };
  
  var readSockaddr = (sa, salen) => {
      // family / port offsets are common to both sockaddr_in and sockaddr_in6
      var family = HEAP16[((sa)>>1)];
      var port = _ntohs(HEAPU16[(((sa)+(2))>>1)]);
      var addr;
  
      switch (family) {
        case 2:
          if (salen !== 16) {
            return { errno: 28 };
          }
          addr = HEAP32[(((sa)+(4))>>2)];
          addr = inetNtop4(addr);
          break;
        case 10:
          if (salen !== 28) {
            return { errno: 28 };
          }
          addr = [
            HEAP32[(((sa)+(8))>>2)],
            HEAP32[(((sa)+(12))>>2)],
            HEAP32[(((sa)+(16))>>2)],
            HEAP32[(((sa)+(20))>>2)]
          ];
          addr = inetNtop6(addr);
          break;
        default:
          return { errno: 5 };
      }
  
      return { family: family, addr: addr, port: port };
    };
  
  
  var getSocketAddress = (addrp, addrlen) => {
      var info = readSockaddr(addrp, addrlen);
      if (info.errno) throw new FS.ErrnoError(info.errno);
      info.addr = DNS.lookup_addr(info.addr) || info.addr;
      return info;
    };
  
  
  function ___syscall_bind(fd, addr, len, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(4, 0, 1, fd, addr, len, u1, u2, u3);
  
  try {
  
      var sock = getSocketFromFD(fd);
      var info = getSocketAddress(addr, len);
      sock.sock_ops.bind(sock, info.addr, info.port);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  var SYSCALLS = {
  currentUmask:18,
  calculateAt(dirfd, path, allowEmpty) {
        if (PATH.isAbs(path)) {
          return path;
        }
        // relative path
        var dir;
        if (dirfd === -100) {
          dir = FS.cwd();
        } else {
          var dirstream = SYSCALLS.getStreamFromFD(dirfd);
          dir = dirstream.path;
        }
        if (path.length == 0) {
          if (!allowEmpty) {
            throw new FS.ErrnoError(44);;
          }
          return dir;
        }
        return dir + '/' + path;
      },
  writeStat(buf, stat) {
        HEAPU32[((buf)>>2)] = stat.dev;
        HEAPU32[(((buf)+(4))>>2)] = stat.mode;
        HEAPU32[(((buf)+(8))>>2)] = stat.nlink;
        HEAPU32[(((buf)+(12))>>2)] = stat.uid;
        HEAPU32[(((buf)+(16))>>2)] = stat.gid;
        HEAPU32[(((buf)+(20))>>2)] = stat.rdev;
        HEAP64[(((buf)+(24))>>3)] = BigInt(stat.size);
        HEAP32[(((buf)+(32))>>2)] = 4096;
        HEAP32[(((buf)+(36))>>2)] = stat.blocks;
        var atime = stat.atime.getTime();
        var mtime = stat.mtime.getTime();
        var ctime = stat.ctime.getTime();
        HEAP64[(((buf)+(40))>>3)] = BigInt(Math.floor(atime / 1000));
        HEAPU32[(((buf)+(48))>>2)] = (atime % 1000) * 1000 * 1000;
        HEAP64[(((buf)+(56))>>3)] = BigInt(Math.floor(mtime / 1000));
        HEAPU32[(((buf)+(64))>>2)] = (mtime % 1000) * 1000 * 1000;
        HEAP64[(((buf)+(72))>>3)] = BigInt(Math.floor(ctime / 1000));
        HEAPU32[(((buf)+(80))>>2)] = (ctime % 1000) * 1000 * 1000;
        HEAP64[(((buf)+(88))>>3)] = BigInt(stat.ino);
        return 0;
      },
  writeStatFs(buf, stats) {
        HEAPU32[(((buf)+(4))>>2)] = stats.bsize;
        HEAPU32[(((buf)+(60))>>2)] = stats.bsize;
        HEAP64[(((buf)+(8))>>3)] = BigInt(stats.blocks);
        HEAP64[(((buf)+(16))>>3)] = BigInt(stats.bfree);
        HEAP64[(((buf)+(24))>>3)] = BigInt(stats.bavail);
        HEAP64[(((buf)+(32))>>3)] = BigInt(stats.files);
        HEAP64[(((buf)+(40))>>3)] = BigInt(stats.ffree);
        HEAPU32[(((buf)+(48))>>2)] = stats.fsid;
        HEAPU32[(((buf)+(64))>>2)] = stats.flags;  // ST_NOSUID
        HEAPU32[(((buf)+(56))>>2)] = stats.namelen;
      },
  doMsync(addr, stream, len, flags, offset) {
        if (!FS.isFile(stream.node.mode)) {
          throw new FS.ErrnoError(43);
        }
        if (flags & 2) {
          // MAP_PRIVATE calls need not to be synced back to underlying fs
          return 0;
        }
        var buffer = HEAPU8.subarray(addr, addr + len);
        FS.msync(stream, buffer, offset, len, flags);
      },
  getStreamFromFD(fd) {
        var stream = FS.getStreamChecked(fd);
        return stream;
      },
  varargs:undefined,
  getStr(ptr) {
        var ret = UTF8ToString(ptr);
        return ret;
      },
  };
  
  
  function ___syscall_chdir(path) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(5, 0, 1, path);
  
  try {
  
      path = SYSCALLS.getStr(path);
      FS.chdir(path);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_chmod(path, mode) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(6, 0, 1, path, mode);
  
  try {
  
      path = SYSCALLS.getStr(path);
      FS.chmod(path, mode);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  function ___syscall_connect(fd, addr, len, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(7, 0, 1, fd, addr, len, u1, u2, u3);
  
  try {
  
      var sock = getSocketFromFD(fd);
      var info = getSocketAddress(addr, len);
      sock.sock_ops.connect(sock, info.addr, info.port);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_dup(fd) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(8, 0, 1, fd);
  
  try {
  
      var old = SYSCALLS.getStreamFromFD(fd);
      return FS.dupStream(old).fd;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_dup3(fd, newfd, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(9, 0, 1, fd, newfd, flags);
  
  try {
  
      if (fd === newfd) return -28;
      if (flags & ~524288) return -28;
      var old = SYSCALLS.getStreamFromFD(fd);
      // Check newfd is within range of valid open file descriptors.
      if (newfd < 0 || newfd >= FS.MAX_OPEN_FDS) return -8;
      var existing = FS.getStream(newfd);
      if (existing) FS.close(existing);
      var stream = FS.dupStream(old, newfd);
      if (flags & 524288) {
        stream.flags |= 524288;
      }
      return stream.fd;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_faccessat(dirfd, path, amode, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(10, 0, 1, dirfd, path, amode, flags);
  
  try {
  
      path = SYSCALLS.getStr(path);
      assert(!flags || flags == 512);
      path = SYSCALLS.calculateAt(dirfd, path);
      if (amode & ~7) {
        // need a valid mode
        return -28;
      }
      var lookup = FS.lookupPath(path, { follow: true });
      var node = lookup.node;
      if (!node) {
        return -44;
      }
      var perms = '';
      if (amode & 4) perms += 'r';
      if (amode & 2) perms += 'w';
      if (amode & 1) perms += 'x';
      if (perms /* otherwise, they've just passed F_OK */ && FS.nodePermissions(node, perms)) {
        return -2;
      }
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  function ___syscall_fchmod(fd, mode) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(11, 0, 1, fd, mode);
  
  try {
  
      FS.fchmod(fd, mode);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  function ___syscall_fchown32(fd, owner, group) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(12, 0, 1, fd, owner, group);
  
  try {
  
      FS.fchown(fd, owner, group);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_fchownat(dirfd, path, owner, group, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(13, 0, 1, dirfd, path, owner, group, flags);
  
  try {
  
      path = SYSCALLS.getStr(path);
      var nofollow = flags & 256;
      flags = flags & (~256);
      assert(!flags);
      path = SYSCALLS.calculateAt(dirfd, path);
      (nofollow ? FS.lchown : FS.chown)(path, owner, group);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  var syscallGetVarargI = () => {
      assert(SYSCALLS.varargs != undefined);
      // the `+` prepended here is necessary to convince the JSCompiler that varargs is indeed a number.
      var ret = HEAP32[((+SYSCALLS.varargs)>>2)];
      SYSCALLS.varargs += 4;
      return ret;
    };
  var syscallGetVarargP = syscallGetVarargI;
  
  
  
  
  function ___syscall_fcntl64(fd, cmd, varargs) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(14, 0, 1, fd, cmd, varargs);
  
  SYSCALLS.varargs = varargs;
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd);
      switch (cmd) {
        case 0: {
          var arg = syscallGetVarargI();
          if (arg < 0) {
            return -28;
          }
          while (FS.streams[arg]) {
            arg++;
          }
          var newStream;
          newStream = FS.dupStream(stream, arg);
          return newStream.fd;
        }
        case 1:
        case 2:
          return 0;  // FD_CLOEXEC makes no sense for a single process.
        case 3:
          return stream.flags;
        case 4: {
          var arg = syscallGetVarargI();
          var mask = 289792;
          stream.flags = (stream.flags & ~mask) | (arg & mask);
          return 0;
        }
        case 12: {
          var arg = syscallGetVarargP();
          var offset = 0;
          // We're always unlocked.
          HEAP16[(((arg)+(offset))>>1)] = 2;
          return 0;
        }
        case 13:
        case 14:
          // Pretend that the locking is successful. These are process-level locks,
          // and Emscripten programs are a single process. If we supported linking a
          // filesystem between programs, we'd need to do more here.
          // See https://github.com/emscripten-core/emscripten/issues/23697
          return 0;
      }
      return -28;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_fstat64(fd, buf) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(15, 0, 1, fd, buf);
  
  try {
  
      return SYSCALLS.writeStat(buf, FS.fstat(fd));
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  var INT53_MAX = 9007199254740992;
  
  var INT53_MIN = -9007199254740992;
  var bigintToI53Checked = (num) => (num < INT53_MIN || num > INT53_MAX) ? NaN : Number(num);
  
  
  function ___syscall_ftruncate64(fd, length) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(16, 0, 1, fd, length);
  
    length = bigintToI53Checked(length);
  
  
  try {
  
      if (isNaN(length)) return -22;
      FS.ftruncate(fd, length);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  ;
  
  }
  

  
  var stringToUTF8 = (str, outPtr, maxBytesToWrite) => {
      assert(typeof maxBytesToWrite == 'number', 'stringToUTF8 requires a third parameter that specifies the length of the output buffer');
      return stringToUTF8Array(str, HEAPU8, outPtr, maxBytesToWrite);
    };
  
  
  function ___syscall_getcwd(buf, size) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(17, 0, 1, buf, size);
  
  try {
  
      if (size === 0) return -28;
      var cwd = FS.cwd();
      var cwdLengthInBytes = lengthBytesUTF8(cwd) + 1;
      if (size < cwdLengthInBytes) return -68;
      stringToUTF8(cwd, buf, size);
      return cwdLengthInBytes;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  function ___syscall_getdents64(fd, dirp, count) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(18, 0, 1, fd, dirp, count);
  
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd)
      stream.getdents ||= FS.readdir(stream.path);
  
      var struct_size = 280;
      var pos = 0;
      var off = FS.llseek(stream, 0, 1);
  
      var startIdx = Math.floor(off / struct_size);
      var endIdx = Math.min(stream.getdents.length, startIdx + Math.floor(count/struct_size))
      for (var idx = startIdx; idx < endIdx; idx++) {
        var id;
        var type;
        var name = stream.getdents[idx];
        if (name === '.') {
          id = stream.node.id;
          type = 4;
        }
        else if (name === '..') {
          var lookup = FS.lookupPath(stream.path, { parent: true });
          id = lookup.node.id;
          type = 4;
        }
        else {
          var child;
          try {
            child = FS.lookupNode(stream.node, name);
          } catch (e) {
            // If the entry is not a directory, file, or symlink, nodefs
            // lookupNode will raise EINVAL. Skip these and continue.
            if (e?.errno === 28) {
              continue;
            }
            throw e;
          }
          id = child.id;
          type = FS.isChrdev(child.mode) ? 2 : // character device.
                 FS.isDir(child.mode) ? 4 :    // directory
                 FS.isLink(child.mode) ? 10 :   // symbolic link.
                 8;                            // regular file.
        }
        assert(id);
        HEAP64[((dirp + pos)>>3)] = BigInt(id);
        HEAP64[(((dirp + pos)+(8))>>3)] = BigInt((idx + 1) * struct_size);
        HEAP16[(((dirp + pos)+(16))>>1)] = 280;
        HEAP8[(dirp + pos)+(18)] = type;
        stringToUTF8(name, dirp + pos + 19, 256);
        pos += struct_size;
      }
      FS.llseek(stream, idx * struct_size, 0);
      return pos;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  
  function ___syscall_getpeername(fd, addr, len, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(19, 0, 1, fd, addr, len, u1, u2, u3);
  
  try {
  
      var sock = getSocketFromFD(fd);
      if (!sock.daddr) {
        return -53; // The socket is not connected.
      }
      var errno = writeSockaddr(addr, sock.family, DNS.lookup_name(sock.daddr), sock.dport, len);
      assert(!errno);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  
  function ___syscall_getsockname(fd, addr, len, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(20, 0, 1, fd, addr, len, u1, u2, u3);
  
  try {
  
      var sock = getSocketFromFD(fd);
      // TODO: sock.saddr should never be undefined, see TODO in websocket_sock_ops.getname
      var errno = writeSockaddr(addr, sock.family, DNS.lookup_name(sock.saddr || '0.0.0.0'), sock.sport, len);
      assert(!errno);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_getsockopt(fd, level, optname, optval, optlen, unused) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(21, 0, 1, fd, level, optname, optval, optlen, unused);
  
  try {
  
      var sock = getSocketFromFD(fd);
      // Minimal getsockopt aimed at resolving https://github.com/emscripten-core/emscripten/issues/2211
      // so only supports SOL_SOCKET with SO_ERROR.
      if (level === 1) {
        if (optname === 4) {
          HEAP32[((optval)>>2)] = sock.error;
          HEAP32[((optlen)>>2)] = 4;
          sock.error = null; // Clear the error (The SO_ERROR option obtains and then clears this field).
          return 0;
        }
      }
      return -50; // The option is unknown at the level indicated.
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  function ___syscall_ioctl(fd, op, varargs) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(22, 0, 1, fd, op, varargs);
  
  SYSCALLS.varargs = varargs;
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd);
      switch (op) {
        case 21509: {
          if (!stream.tty) return -59;
          return 0;
        }
        case 21505: {
          if (!stream.tty) return -59;
          if (stream.tty.ops.ioctl_tcgets) {
            var termios = stream.tty.ops.ioctl_tcgets(stream);
            var argp = syscallGetVarargP();
            HEAP32[((argp)>>2)] = termios.c_iflag || 0;
            HEAP32[(((argp)+(4))>>2)] = termios.c_oflag || 0;
            HEAP32[(((argp)+(8))>>2)] = termios.c_cflag || 0;
            HEAP32[(((argp)+(12))>>2)] = termios.c_lflag || 0;
            for (var i = 0; i < 32; i++) {
              HEAP8[(argp + i)+(17)] = termios.c_cc[i] || 0;
            }
            return 0;
          }
          return 0;
        }
        case 21510:
        case 21511:
        case 21512: {
          if (!stream.tty) return -59;
          return 0; // no-op, not actually adjusting terminal settings
        }
        case 21506:
        case 21507:
        case 21508: {
          if (!stream.tty) return -59;
          if (stream.tty.ops.ioctl_tcsets) {
            var argp = syscallGetVarargP();
            var c_iflag = HEAP32[((argp)>>2)];
            var c_oflag = HEAP32[(((argp)+(4))>>2)];
            var c_cflag = HEAP32[(((argp)+(8))>>2)];
            var c_lflag = HEAP32[(((argp)+(12))>>2)];
            var c_cc = []
            for (var i = 0; i < 32; i++) {
              c_cc.push(HEAP8[(argp + i)+(17)]);
            }
            return stream.tty.ops.ioctl_tcsets(stream.tty, op, { c_iflag, c_oflag, c_cflag, c_lflag, c_cc });
          }
          return 0; // no-op, not actually adjusting terminal settings
        }
        case 21519: {
          if (!stream.tty) return -59;
          var argp = syscallGetVarargP();
          HEAP32[((argp)>>2)] = 0;
          return 0;
        }
        case 21520: {
          if (!stream.tty) return -59;
          return -28; // not supported
        }
        case 21537:
        case 21531: {
          var argp = syscallGetVarargP();
          return FS.ioctl(stream, op, argp);
        }
        case 21523: {
          // TODO: in theory we should write to the winsize struct that gets
          // passed in, but for now musl doesn't read anything on it
          if (!stream.tty) return -59;
          if (stream.tty.ops.ioctl_tiocgwinsz) {
            var winsize = stream.tty.ops.ioctl_tiocgwinsz(stream.tty);
            var argp = syscallGetVarargP();
            HEAP16[((argp)>>1)] = winsize[0];
            HEAP16[(((argp)+(2))>>1)] = winsize[1];
          }
          return 0;
        }
        case 21524: {
          // TODO: technically, this ioctl call should change the window size.
          // but, since emscripten doesn't have any concept of a terminal window
          // yet, we'll just silently throw it away as we do TIOCGWINSZ
          if (!stream.tty) return -59;
          return 0;
        }
        case 21515: {
          if (!stream.tty) return -59;
          return 0;
        }
        default: return -28; // not supported
      }
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_listen(fd, backlog, u1, u2, u3, u4) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(23, 0, 1, fd, backlog, u1, u2, u3, u4);
  
  try {
  
      var sock = getSocketFromFD(fd);
      sock.sock_ops.listen(sock, backlog);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_lstat64(path, buf) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(24, 0, 1, path, buf);
  
  try {
  
      path = SYSCALLS.getStr(path);
      return SYSCALLS.writeStat(buf, FS.lstat(path));
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_mkdirat(dirfd, path, mode) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(25, 0, 1, dirfd, path, mode);
  
  try {
  
      path = SYSCALLS.getStr(path);
      path = SYSCALLS.calculateAt(dirfd, path);
      mode &= ~SYSCALLS.currentUmask;
      FS.mkdir(path, mode, 0);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_newfstatat(dirfd, path, buf, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(26, 0, 1, dirfd, path, buf, flags);
  
  try {
  
      path = SYSCALLS.getStr(path);
      var nofollow = flags & 256;
      var allowEmpty = flags & 4096;
      flags = flags & (~6400);
      assert(!flags, `unknown flags in __syscall_newfstatat: ${flags}`);
      path = SYSCALLS.calculateAt(dirfd, path, allowEmpty);
      return SYSCALLS.writeStat(buf, nofollow ? FS.lstat(path) : FS.stat(path));
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  function ___syscall_openat(dirfd, path, flags, varargs) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(27, 0, 1, dirfd, path, flags, varargs);
  
  SYSCALLS.varargs = varargs;
  try {
  
      path = SYSCALLS.getStr(path);
      path = SYSCALLS.calculateAt(dirfd, path);
      var mode = varargs ? syscallGetVarargI() : 0;
      if (flags & 64) {
        mode &= ~SYSCALLS.currentUmask;
      }
      return FS.open(path, flags, mode).fd;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  var PIPEFS = {
  BUCKET_BUFFER_SIZE:8192,
  mount(mount) {
        // Do not pollute the real root directory or its child nodes with pipes
        // Looks like it is OK to create another pseudo-root node not linked to the FS.root hierarchy this way
        return FS.createNode(null, '/', 16384 | 0o777, 0);
      },
  createPipe() {
        var pipe = {
          buckets: [],
          // refcnt 2 because pipe has a read end and a write end. We need to be
          // able to read from the read end after write end is closed.
          refcnt : 2,
          timestamp: new Date(),
          readableHandlers: [],
          registerReadableHandler: (callback) => {
            callback.registerCleanupFunc(() => {
              const i = pipe.readableHandlers.indexOf(callback);
              if (i !== -1) pipe.readableHandlers.splice(i, 1);
            });
            pipe.readableHandlers.push(callback);
          },
          notifyReadableHandlers: () => {
            while (pipe.readableHandlers.length > 0) {
              const cb = pipe.readableHandlers.shift();
              if (cb) cb(64 | 1);
            }
            pipe.readableHandlers = [];
          }
        };
  
        pipe.buckets.push({
          buffer: new Uint8Array(PIPEFS.BUCKET_BUFFER_SIZE),
          offset: 0,
          roffset: 0
        });
  
        var rName = PIPEFS.nextname();
        var wName = PIPEFS.nextname();
        var rNode = FS.createNode(PIPEFS.root, rName, 4096, 0);
        var wNode = FS.createNode(PIPEFS.root, wName, 4096, 0);
  
        rNode.pipe = pipe;
        wNode.pipe = pipe;
  
        var readableStream = FS.createStream({
          path: rName,
          node: rNode,
          flags: 0,
          seekable: false,
          stream_ops: PIPEFS.stream_ops
        });
        rNode.stream = readableStream;
  
        var writableStream = FS.createStream({
          path: wName,
          node: wNode,
          flags: 1,
          seekable: false,
          stream_ops: PIPEFS.stream_ops
        });
        wNode.stream = writableStream;
  
        return {
          readable_fd: readableStream.fd,
          writable_fd: writableStream.fd
        };
      },
  stream_ops:{
  getattr(stream) {
          var node = stream.node;
          var timestamp = node.pipe.timestamp;
          return {
            dev: 14,
            ino: node.id,
            mode: 0o10600,
            nlink: 1,
            uid: 0,
            gid: 0,
            rdev: 0,
            size: 0,
            atime: timestamp,
            mtime: timestamp,
            ctime: timestamp,
            blksize: 4096,
            blocks: 0,
          };
        },
  poll(stream, timeout, notifyCallback) {
          var pipe = stream.node.pipe;
  
          if ((stream.flags & 2097155) === 1) {
            return (256 | 4);
          }
          for (var bucket of pipe.buckets) {
            if (bucket.offset - bucket.roffset > 0) {
              return (64 | 1);
            }
          }
  
          if (notifyCallback) pipe.registerReadableHandler(notifyCallback);
          return 0;
        },
  dup(stream) {
          stream.node.pipe.refcnt++;
        },
  ioctl(stream, request, argp) {
          if (request == 21531) {
            var pipe = stream.node.pipe;
            var currentLength = 0;
            for (var bucket of pipe.buckets) {
              currentLength += bucket.offset - bucket.roffset;
            }
            HEAP32[((argp)>>2)] = currentLength;
            return 0;
          }
          return 28;
        },
  fsync(stream) {
          return 28;
        },
  read(stream, buffer, offset, length, position /* ignored */) {
          var pipe = stream.node.pipe;
          var currentLength = 0;
  
          for (var bucket of pipe.buckets) {
            currentLength += bucket.offset - bucket.roffset;
          }
  
          assert(buffer instanceof ArrayBuffer || buffer instanceof SharedArrayBuffer || ArrayBuffer.isView(buffer));
          var data = buffer.subarray(offset, offset + length);
  
          if (length <= 0) {
            return 0;
          }
          if (currentLength == 0) {
            // Behave as if the read end is always non-blocking
            throw new FS.ErrnoError(6);
          }
          var toRead = Math.min(currentLength, length);
  
          var totalRead = toRead;
          var toRemove = 0;
  
          for (var bucket of pipe.buckets) {
            var bucketSize = bucket.offset - bucket.roffset;
  
            if (toRead <= bucketSize) {
              var tmpSlice = bucket.buffer.subarray(bucket.roffset, bucket.offset);
              if (toRead < bucketSize) {
                tmpSlice = tmpSlice.subarray(0, toRead);
                bucket.roffset += toRead;
              } else {
                toRemove++;
              }
              data.set(tmpSlice);
              break;
            } else {
              var tmpSlice = bucket.buffer.subarray(bucket.roffset, bucket.offset);
              data.set(tmpSlice);
              data = data.subarray(tmpSlice.byteLength);
              toRead -= tmpSlice.byteLength;
              toRemove++;
            }
          }
  
          if (toRemove && toRemove == pipe.buckets.length) {
            // Do not generate excessive garbage in use cases such as
            // write several bytes, read everything, write several bytes, read everything...
            toRemove--;
            pipe.buckets[toRemove].offset = 0;
            pipe.buckets[toRemove].roffset = 0;
          }
  
          pipe.buckets.splice(0, toRemove);
  
          return totalRead;
        },
  write(stream, buffer, offset, length, position /* ignored */) {
          var pipe = stream.node.pipe;
  
          assert(buffer instanceof ArrayBuffer || buffer instanceof SharedArrayBuffer || ArrayBuffer.isView(buffer));
          var data = buffer.subarray(offset, offset + length);
  
          var dataLen = data.byteLength;
          if (dataLen <= 0) {
            return 0;
          }
  
          var currBucket = null;
  
          if (pipe.buckets.length == 0) {
            currBucket = {
              buffer: new Uint8Array(PIPEFS.BUCKET_BUFFER_SIZE),
              offset: 0,
              roffset: 0
            };
            pipe.buckets.push(currBucket);
          } else {
            currBucket = pipe.buckets[pipe.buckets.length - 1];
          }
  
          assert(currBucket.offset <= PIPEFS.BUCKET_BUFFER_SIZE);
  
          var freeBytesInCurrBuffer = PIPEFS.BUCKET_BUFFER_SIZE - currBucket.offset;
          if (freeBytesInCurrBuffer >= dataLen) {
            currBucket.buffer.set(data, currBucket.offset);
            currBucket.offset += dataLen;
            pipe.notifyReadableHandlers();
            return dataLen;
          } else if (freeBytesInCurrBuffer > 0) {
            currBucket.buffer.set(data.subarray(0, freeBytesInCurrBuffer), currBucket.offset);
            currBucket.offset += freeBytesInCurrBuffer;
            data = data.subarray(freeBytesInCurrBuffer, data.byteLength);
          }
  
          var numBuckets = (data.byteLength / PIPEFS.BUCKET_BUFFER_SIZE) | 0;
          var remElements = data.byteLength % PIPEFS.BUCKET_BUFFER_SIZE;
  
          for (var i = 0; i < numBuckets; i++) {
            var newBucket = {
              buffer: new Uint8Array(PIPEFS.BUCKET_BUFFER_SIZE),
              offset: PIPEFS.BUCKET_BUFFER_SIZE,
              roffset: 0
            };
            pipe.buckets.push(newBucket);
            newBucket.buffer.set(data.subarray(0, PIPEFS.BUCKET_BUFFER_SIZE));
            data = data.subarray(PIPEFS.BUCKET_BUFFER_SIZE, data.byteLength);
          }
  
          if (remElements > 0) {
            var newBucket = {
              buffer: new Uint8Array(PIPEFS.BUCKET_BUFFER_SIZE),
              offset: data.byteLength,
              roffset: 0
            };
            pipe.buckets.push(newBucket);
            newBucket.buffer.set(data);
          }
  
          pipe.notifyReadableHandlers();
          return dataLen;
        },
  close(stream) {
          var pipe = stream.node.pipe;
          pipe.refcnt--;
          if (pipe.refcnt === 0) {
            pipe.buckets = null;
          }
        },
  },
  nextname() {
        if (!PIPEFS.nextname.current) {
          PIPEFS.nextname.current = 0;
        }
        return 'pipe[' + (PIPEFS.nextname.current++) + ']';
      },
  };
  
  
  function ___syscall_pipe2(fdPtr, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(28, 0, 1, fdPtr, flags);
  
  try {
  
      if (fdPtr == 0) {
        throw new FS.ErrnoError(21);
      }
      var validFlags = 524288 | 2048;
      if (flags & ~validFlags) {
        throw new FS.ErrnoError(138);
      }
  
      var res = PIPEFS.createPipe();
  
      if (flags & 2048) {
        FS.getStream(res.readable_fd).flags |= 2048;
        FS.getStream(res.writable_fd).flags |= 2048;
      }
  
      HEAP32[((fdPtr)>>2)] = res.readable_fd;
      HEAP32[(((fdPtr)+(4))>>2)] = res.writable_fd;
  
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  var doPoll = (fds, nfds, timeout, makeNotifyCallback) => {
      var count = 0;
      for (var i = 0; i < nfds; i++) {
        var pollfd = fds + 8 * i;
        var fd = HEAP32[((pollfd)>>2)];
        var events = HEAP16[(((pollfd)+(4))>>1)];
        var flags = 32;
        var stream = FS.getStream(fd);
        if (stream) {
          if (stream.stream_ops.poll) {
            flags = timeout
              ? stream.stream_ops.poll(stream, timeout, makeNotifyCallback(stream, pollfd))
              : stream.stream_ops.poll(stream, -1);
          } else {
            flags = 5;
          }
        }
        flags &= events | 8 | 16 | 32;
        if (flags) count++;
        HEAP16[(((pollfd)+(6))>>1)] = flags;
      }
      return count;
    };
  
  
  var doPollAsync = (fds, nfds, timeout) => {
  
      // Enable event handlers only when the poll call is proxied from a worker.
      // TODO: Could use `Promise.withResolvers` here if we know its available.
      var resolve;
      var promise = new Promise((resolve_) => { resolve = resolve_; });
      var cleanupFuncs = [];
      var notifyDone = false;
  
      function asyncPollComplete(count) {
        if (notifyDone) {
          return;
        }
        notifyDone = true;
        cleanupFuncs.forEach(cb => cb());
        resolve(count);
      }
  
      function makeNotifyCallback(stream, pollfd) {
        var cb = (flags) => {
          if (notifyDone) {
            return;
          }
          var events = HEAP16[(((pollfd)+(4))>>1)];
          flags &= events | 8 | 16 | 32;
          assert(flags)
          HEAP16[(((pollfd)+(6))>>1)] = flags;
          asyncPollComplete(1);
        }
        cb.registerCleanupFunc = (f) => {
          if (f) cleanupFuncs.push(f);
        }
        return cb;
      }
      if (timeout > 0) {
        var t = setTimeout(() => {
          asyncPollComplete(0);
        }, timeout);
        cleanupFuncs.push(() => clearTimeout(t));
      }
      // A zero timeout never registers notifications: the derivation alone
      // answers, matching the non-blocking probe.
      var count = doPoll(fds, nfds, timeout, makeNotifyCallback);
      if (count || !timeout) {
        asyncPollComplete(count);
      }
      return promise;
    };
  
  
  function ___syscall_poll(fds, nfds, timeout) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(29, 0, 2, fds, nfds, timeout);
  
  try {
  
      const isAsyncContext = PThread.currentProxiedOperationCallerThread;
      if (isAsyncContext) {
        return doPollAsync(fds, nfds, timeout);
      }
  
      var count = doPoll(fds, nfds, 0, undefined);
      if (!count && timeout != 0) warnOnce('non-zero poll() timeout not supported: ' + timeout)
      return count;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_poll_nonblocking(fds, nfds) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(30, 0, 1, fds, nfds);
  
  try {
  
      return doPoll(fds, nfds, 0, undefined);
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  
  function ___syscall_readlinkat(dirfd, path, buf, bufsize) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(31, 0, 1, dirfd, path, buf, bufsize);
  
  try {
  
      path = SYSCALLS.getStr(path);
      path = SYSCALLS.calculateAt(dirfd, path);
      if (bufsize <= 0) return -28;
      var ret = FS.readlink(path);
  
      var len = Math.min(bufsize, lengthBytesUTF8(ret));
      var endChar = HEAP8[buf+len];
      stringToUTF8(ret, buf, bufsize+1);
      // readlink is one of the rare functions that write out a C string, but does never append a null to the output buffer(!)
      // stringToUTF8() always appends a null byte, so restore the character under the null byte after the write.
      HEAP8[buf+len] = endChar;
      return len;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  
  function ___syscall_recvfrom(fd, buf, len, flags, addr, alen) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(32, 0, 1, fd, buf, len, flags, addr, alen);
  
  try {
  
      var sock = getSocketFromFD(fd);
      var msg = sock.sock_ops.recvmsg(sock, len);
      if (!msg) return 0; // socket is closed
      if (addr) {
        var errno = writeSockaddr(addr, sock.family, DNS.lookup_name(msg.addr), msg.port, alen);
        assert(!errno);
      }
      HEAPU8.set(msg.buffer, buf);
      return msg.buffer.byteLength;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  
  function ___syscall_recvmsg(fd, message, flags, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(33, 0, 1, fd, message, flags, u1, u2, u3);
  
  try {
  
      var sock = getSocketFromFD(fd);
      var iov = HEAPU32[(((message)+(8))>>2)];
      var num = HEAP32[(((message)+(12))>>2)];
      // get the total amount of data we can read across all arrays
      var total = 0;
      for (var i = 0; i < num; i++) {
        total += HEAP32[(((iov)+((8 * i) + 4))>>2)];
      }
      // try to read total data
      var msg = sock.sock_ops.recvmsg(sock, total);
      if (!msg) return 0; // socket is closed
  
      // TODO honor flags:
      // MSG_OOB
      // Requests out-of-band data. The significance and semantics of out-of-band data are protocol-specific.
      // MSG_PEEK
      // Peeks at the incoming message.
      // MSG_WAITALL
      // Requests that the function block until the full amount of data requested can be returned. The function may return a smaller amount of data if a signal is caught, if the connection is terminated, if MSG_PEEK was specified, or if an error is pending for the socket.
  
      // write the source address out
      var name = HEAPU32[((message)>>2)];
      if (name) {
        var errno = writeSockaddr(name, sock.family, DNS.lookup_name(msg.addr), msg.port);
        assert(!errno);
      }
      // write the buffer out to the scatter-gather arrays
      var bytesRead = 0;
      var bytesRemaining = msg.buffer.byteLength;
      for (var i = 0; bytesRemaining > 0 && i < num; i++) {
        var iovbase = HEAPU32[(((iov)+((8 * i) + 0))>>2)];
        var iovlen = HEAP32[(((iov)+((8 * i) + 4))>>2)];
        if (!iovlen) {
          continue;
        }
        var length = Math.min(iovlen, bytesRemaining);
        var buf = msg.buffer.subarray(bytesRead, bytesRead + length);
        HEAPU8.set(buf, iovbase + bytesRead);
        bytesRead += length;
        bytesRemaining -= length;
      }
  
      // TODO set msghdr.msg_flags
      // MSG_EOR
      // End of record was received (if supported by the protocol).
      // MSG_OOB
      // Out-of-band data was received.
      // MSG_TRUNC
      // Normal data was truncated.
      // MSG_CTRUNC
  
      return bytesRead;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_renameat(olddirfd, oldpath, newdirfd, newpath) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(34, 0, 1, olddirfd, oldpath, newdirfd, newpath);
  
  try {
  
      oldpath = SYSCALLS.getStr(oldpath);
      newpath = SYSCALLS.getStr(newpath);
      oldpath = SYSCALLS.calculateAt(olddirfd, oldpath);
      newpath = SYSCALLS.calculateAt(newdirfd, newpath);
      FS.rename(oldpath, newpath);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_rmdir(path) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(35, 0, 1, path);
  
  try {
  
      path = SYSCALLS.getStr(path);
      FS.rmdir(path);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  function ___syscall_sendmsg(fd, message, flags, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(36, 0, 1, fd, message, flags, u1, u2, u3);
  
  try {
  
      var sock = getSocketFromFD(fd);
      var iov = HEAPU32[(((message)+(8))>>2)];
      var num = HEAP32[(((message)+(12))>>2)];
      // read the address and port to send to
      var addr, port;
      var name = HEAPU32[((message)>>2)];
      var namelen = HEAP32[(((message)+(4))>>2)];
      if (name) {
        var info = getSocketAddress(name, namelen);
        port = info.port;
        addr = info.addr;
      }
      // concatenate scatter-gather arrays into one message buffer
      var total = 0;
      for (var i = 0; i < num; i++) {
        total += HEAP32[(((iov)+((8 * i) + 4))>>2)];
      }
      var view = new Uint8Array(total);
      var offset = 0;
      for (var i = 0; i < num; i++) {
        var iovbase = HEAPU32[(((iov)+((8 * i) + 0))>>2)];
        var iovlen = HEAP32[(((iov)+((8 * i) + 4))>>2)];
        for (var j = 0; j < iovlen; j++) {
          view[offset++] = HEAP8[(iovbase)+(j)];
        }
      }
      // write the buffer
      return sock.sock_ops.sendmsg(sock, view, 0, total, addr, port);
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  
  function ___syscall_sendto(fd, buf, len, flags, addr, alen) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(37, 0, 1, fd, buf, len, flags, addr, alen);
  
  try {
  
      var sock = getSocketFromFD(fd);
      if (!addr) {
        // send, no address provided
        return FS.write(sock.stream, HEAP8, buf, len);
      }
      var dest = getSocketAddress(addr, alen);
      // sendto an address
      return sock.sock_ops.sendmsg(sock, HEAP8, buf, len, dest.addr, dest.port);
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_setsockopt(fd, level, optname, optval, optlen, unused) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(38, 0, 1, fd, level, optname, optval, optlen, unused);
  
  try {
  
      getSocketFromFD(fd); // validate the fd (and keep this syscall's catch reachable)
      return -50; // The option is unknown at the level indicated.
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_shutdown(fd, how, u1, u2, u3, u4) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(39, 0, 1, fd, how, u1, u2, u3, u4);
  
  try {
  
      var sock = getSocketFromFD(fd);
      return -52; // unsupported feature
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_socket(domain, type, protocol, u1, u2, u3) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(40, 0, 1, domain, type, protocol, u1, u2, u3);
  
  try {
  
      var sock = SOCKFS.createSocket(domain, type, protocol);
      assert(sock.stream.fd < 64); // XXX ? select() assumes socket fd values are in 0..63
      return sock.stream.fd;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_stat64(path, buf) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(41, 0, 1, path, buf);
  
  try {
  
      path = SYSCALLS.getStr(path);
      return SYSCALLS.writeStat(buf, FS.stat(path));
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_statfs64(path, size, buf) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(42, 0, 1, path, size, buf);
  
  try {
  
      assert(size === 88);
      SYSCALLS.writeStatFs(buf, FS.statfs(SYSCALLS.getStr(path)));
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_symlinkat(target, dirfd, linkpath) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(43, 0, 1, target, dirfd, linkpath);
  
  try {
  
      target = SYSCALLS.getStr(target);
      linkpath = SYSCALLS.getStr(linkpath);
      linkpath = SYSCALLS.calculateAt(dirfd, linkpath);
      FS.symlink(target, linkpath);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_umask(mask) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(44, 0, 1, mask);
  
  try {
  
      var old = SYSCALLS.currentUmask;
      SYSCALLS.currentUmask = mask;
      return old;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  
  
  function ___syscall_unlinkat(dirfd, path, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(45, 0, 1, dirfd, path, flags);
  
  try {
  
      path = SYSCALLS.getStr(path);
      path = SYSCALLS.calculateAt(dirfd, path);
      if (!flags) {
        FS.unlink(path);
      } else if (flags === 512) {
        FS.rmdir(path);
      } else {
        return -28;
      }
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  var readI53FromI64 = (ptr) => {
      return HEAPU32[((ptr)>>2)] + HEAP32[(((ptr)+(4))>>2)] * 4294967296;
    };
  
  
  
  function ___syscall_utimensat(dirfd, path, times, flags) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(46, 0, 1, dirfd, path, times, flags);
  
  try {
  
      path = SYSCALLS.getStr(path);
      assert(!flags);
      path = SYSCALLS.calculateAt(dirfd, path, true);
      var now = Date.now(), atime, mtime;
      if (!times) {
        atime = now;
        mtime = now;
      } else {
        var seconds = readI53FromI64(times);
        var nanoseconds = HEAP32[(((times)+(8))>>2)];
        if (nanoseconds == 1073741823) {
          atime = now;
        } else if (nanoseconds == 1073741822) {
          atime = null;
        } else {
          atime = (seconds*1000) + (nanoseconds/(1000*1000));
        }
        times += 16;
        seconds = readI53FromI64(times);
        nanoseconds = HEAP32[(((times)+(8))>>2)];
        if (nanoseconds == 1073741823) {
          mtime = now;
        } else if (nanoseconds == 1073741822) {
          mtime = null;
        } else {
          mtime = (seconds*1000) + (nanoseconds/(1000*1000));
        }
      }
      // null here means UTIME_OMIT was passed. If both were set to UTIME_OMIT then
      // we can skip the call completely.
      if ((mtime ?? atime) !== null) {
        FS.utime(path, atime, mtime);
      }
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return -e.errno;
  }
  
  }
  

  var __abort_js = () =>
      abort('native code called abort()');

  var __emscripten_init_main_thread_js = (tb) => {
      var can_block = !ENVIRONMENT_IS_WEB;
      // Feature detect whether the main thread can block.
      try {
        Atomics.wait(HEAP32, 0, 0, 0)
        can_block = true;
      } catch (e) {}
      // Pass the thread address to the native code where they are stored in wasm
      // globals which act as a form of TLS. Global constructors trying
      // to access this value will read the wrong value, but that is UB anyway.
      __emscripten_thread_init(
        tb,
        /*is_main=*/!ENVIRONMENT_IS_WORKER,
        /*is_runtime=*/1,
        can_block,
        /*default_stacksize=*/8388608,
        /*start_profiling=*/false,
      );
      PThread.threadInitTLS();
    };

  var handleException = (e) => {
      // Certain exception types we do not treat as errors since they are used for
      // internal control flow.
      // 1. ExitStatus, which is thrown by exit()
      // 2. "unwind", which is thrown by emscripten_unwind_to_js_event_loop() and others
      //    that wish to return to JS event loop.
      if (e instanceof ExitStatus || e == 'unwind') {
        return EXITSTATUS;
      }
      checkStackCookie();
      if (e instanceof WebAssembly.RuntimeError) {
        if (_emscripten_stack_get_current() <= 0) {
          err('Stack overflow detected.  You can try increasing -sSTACK_SIZE (currently set to 8388608)');
        }
      }
      quit_(1, e);
    };
  
  
  
  
  var maybeExit = () => {
      if (!keepRuntimeAlive()) {
        try {
          if (ENVIRONMENT_IS_PTHREAD) {
            // exit the current thread, but only if there is one active.
            // TODO(https://github.com/emscripten-core/emscripten/issues/25076):
            // Unify this check with the runtimeExited check above
            if (_pthread_self()) __emscripten_thread_exit(EXITSTATUS);
            return;
          }
          _exit(EXITSTATUS);
        } catch (e) {
          handleException(e);
        }
      }
    };
  var callUserCallback = (func) => {
      if (ABORT) {
        err('user callback triggered after runtime exited or application aborted.  Ignoring.');
        return;
      }
      try {
        return func();
      } catch (e) {
        handleException(e);
      } finally {
        maybeExit();
      }
    };
  
  
  
  
  
  var __emscripten_thread_mailbox_await = (pthread_ptr) => {
      if (!waitAsyncPolyfilled) {
        // Wait on the pthread's initial self-pointer field because it is easy and
        // safe to access from sending threads that need to notify the waiting
        // thread.
        // Note: Under wasm64 only the low 32-bit of the pthread_ptr are
        // read/compared here, but we don't actually care about the exact values
        // here as long as they match.
        var wait = Atomics.waitAsync(HEAP32, ((pthread_ptr)>>2), pthread_ptr);
        assert(wait.async);
        wait.value.then(checkMailbox);
        var waitingAsync = pthread_ptr + 112;
        Atomics.store(HEAP32, ((waitingAsync)>>2), 1);
      }
      // If `Atomics.waitAsync` is not implemented, then we will always fall back
      // to postMessage and there is no need to do anything here.
    };
  
  var checkMailbox = () => {
      // checkMailbox can be called after the pthread has shut down. See
      // Pthread.terminateRuntime().
      // In this case we return silently without re-registering using waitAsync.
      // Perhaps there is a more universal way we can detect runtime has exited.
      // TODO(https://github.com/emscripten-core/emscripten/issues/25076)
      var pthread_ptr = _pthread_self();
      if (!pthread_ptr) return;
      callUserCallback(() => {
        // If we are using Atomics.waitAsync as our notification mechanism, wait
        // for a notification before processing the mailbox to avoid missing any
        // work that could otherwise arrive after we've finished processing the
        // mailbox and before we're ready for the next notification.
        __emscripten_thread_mailbox_await(pthread_ptr);
        __emscripten_check_mailbox();
      });
    };
  
  var __emscripten_notify_mailbox_postmessage = (targetThread, currThreadId) => {
      if (targetThread == currThreadId) {
        setTimeout(checkMailbox);
      } else if (ENVIRONMENT_IS_PTHREAD) {
        postMessage({targetThread, cmd: 4});
      } else {
        var worker = PThread.pthreads[targetThread];
        if (!worker) {
          err(`Cannot send message to thread with ID ${targetThread}, unknown thread ID!`);
          return;
        }
        worker.postMessage({cmd: 4});
      }
    };

  
  
  var proxiedJSCallArgs = [];
  
  var __emscripten_receive_on_main_thread_js = (funcIndex, emAsmAddr, callingThread, bufSize, args, ctx, ctxArgs) => {
      // Sometimes we need to backproxy events to the calling thread (e.g.
      // HTML5 DOM events handlers such as
      // emscripten_set_mousemove_callback()), so keep track in a globally
      // accessible variable about the thread that initiated the proxying.
      proxiedJSCallArgs.length = 0;
      var b = ((args)>>3);
      var end = ((args + bufSize)>>3);
      while (b < end) {
        var arg;
        if (HEAP64[b++]) {
          // It's a BigInt.
          arg = HEAP64[b++];
        } else {
          // It's a Number.
          arg = HEAPF64[b++];
        }
        proxiedJSCallArgs.push(arg);
      }
      // Proxied JS library funcs use funcIndex and EM_ASM functions use emAsmAddr
      assert(!emAsmAddr);
      var func = proxiedFunctionTable[funcIndex];
      assert(!(funcIndex && emAsmAddr));
      assert(func.length == proxiedJSCallArgs.length, 'Call args mismatch in _emscripten_receive_on_main_thread_js');
      PThread.currentProxiedOperationCallerThread = callingThread;
      var rtn = func(...proxiedJSCallArgs);
      PThread.currentProxiedOperationCallerThread = 0;
      if (ctx) {
        rtn.then((rtn) => __emscripten_run_js_on_main_thread_done(ctx, ctxArgs, rtn));
        return;
      }
  
      // Proxied functions can return any type except bigint.  All other types
      // coerce to f64/double (the return type of this function in C) but not
      // bigint.
      assert(typeof rtn != "bigint");
      return rtn;
    };

  var __emscripten_runtime_keepalive_clear = () => {
      noExitRuntime = false;
      runtimeKeepaliveCounter = 0;
    };

  var __emscripten_system = (command) => {
      if (ENVIRONMENT_IS_NODE) {
        if (!command) return 1; // shell is available
  
        var cmdstr = UTF8ToString(command);
        if (!cmdstr.length) return 0; // this is what glibc seems to do (shell works test?)
  
        var cp = require('node:child_process');
        var ret = cp.spawnSync(cmdstr, [], {shell:true, stdio:'inherit'});
  
        var _W_EXITCODE = (ret, sig) => ((ret) << 8 | (sig));
  
        // this really only can happen if process is killed by signal
        if (ret.status === null) {
          // sadly node doesn't expose such function
          var signalToNumber = (sig) => {
            // implement only the most common ones, and fallback to SIGINT
            switch (sig) {
              case 'SIGHUP': return 1;
              case 'SIGQUIT': return 3;
              case 'SIGFPE': return 8;
              case 'SIGKILL': return 9;
              case 'SIGALRM': return 14;
              case 'SIGTERM': return 15;
              default: return 2;
            }
          }
          return _W_EXITCODE(0, signalToNumber(ret.signal));
        }
  
        return _W_EXITCODE(ret.status, 0);
      }
      // int system(const char *command);
      // http://pubs.opengroup.org/onlinepubs/000095399/functions/system.html
      // Can't call external programs.
      if (!command) return 0; // no shell available
      return -52;
    };

  var __emscripten_thread_cleanup = (thread) => {
      // Called when a thread needs to be cleaned up so it can be reused.
      // A thread is considered reusable when it either returns from its
      // entry point, calls pthread_exit, or acts upon a cancellation.
      // Detached threads are responsible for calling this themselves,
      // otherwise pthread_join is responsible for calling this.
      if (!ENVIRONMENT_IS_PTHREAD) cleanupThread(thread);
      else postMessage({ cmd: 6, thread });
    };


  var __emscripten_thread_set_strongref = (thread) => {
      // Called when a thread needs to be strongly referenced.
      // Currently only used for:
      // - keeping the "main" thread alive in PROXY_TO_PTHREAD mode;
      // - crashed threads that need to propagate the uncaught exception
      //   back to the main thread.
      if (ENVIRONMENT_IS_NODE) {
        var worker = PThread.pthreads[thread];
        worker.ref();
        // Also, record that we called strongref, in case this function is called
        // bafore the 'loaded' callback from the thread (where we would normally
        // `unref` it.
        worker.strongref = 1;
      }
    };

  var __emscripten_throw_longjmp = () => {
      throw new EmscriptenSjLj;
    };

  function __gmtime_js(time, tmPtr) {
    time = bigintToI53Checked(time);
  
  
      var date = new Date(time * 1000);
      if (isNaN(date.getTime())) {
        return 1;
      }
      HEAP32[((tmPtr)>>2)] = date.getUTCSeconds();
      HEAP32[(((tmPtr)+(4))>>2)] = date.getUTCMinutes();
      HEAP32[(((tmPtr)+(8))>>2)] = date.getUTCHours();
      HEAP32[(((tmPtr)+(12))>>2)] = date.getUTCDate();
      HEAP32[(((tmPtr)+(16))>>2)] = date.getUTCMonth();
      HEAP32[(((tmPtr)+(20))>>2)] = date.getUTCFullYear()-1900;
      HEAP32[(((tmPtr)+(24))>>2)] = date.getUTCDay();
      var start = Date.UTC(date.getUTCFullYear(), 0, 1, 0, 0, 0, 0);
      var yday = ((date.getTime() - start) / (1000 * 60 * 60 * 24))|0;
      HEAP32[(((tmPtr)+(28))>>2)] = yday;
      return 0;
    ;
  }

  var isLeapYear = (year) => year%4 === 0 && (year%100 !== 0 || year%400 === 0);
  
  var MONTH_DAYS_LEAP_CUMULATIVE = [0,31,60,91,121,152,182,213,244,274,305,335];
  
  var MONTH_DAYS_REGULAR_CUMULATIVE = [0,31,59,90,120,151,181,212,243,273,304,334];
  var ydayFromDate = (date) => {
      var leap = isLeapYear(date.getFullYear());
      var monthDaysCumulative = (leap ? MONTH_DAYS_LEAP_CUMULATIVE : MONTH_DAYS_REGULAR_CUMULATIVE);
      var yday = monthDaysCumulative[date.getMonth()] + date.getDate() - 1; // -1 since it's days since Jan 1
  
      return yday;
    };
  
  function __localtime_js(time, tmPtr) {
    time = bigintToI53Checked(time);
  
  
      var date = new Date(time*1000);
      if (isNaN(date.getTime())) {
        return 1;
      }
      HEAP32[((tmPtr)>>2)] = date.getSeconds();
      HEAP32[(((tmPtr)+(4))>>2)] = date.getMinutes();
      HEAP32[(((tmPtr)+(8))>>2)] = date.getHours();
      HEAP32[(((tmPtr)+(12))>>2)] = date.getDate();
      HEAP32[(((tmPtr)+(16))>>2)] = date.getMonth();
      HEAP32[(((tmPtr)+(20))>>2)] = date.getFullYear()-1900;
      HEAP32[(((tmPtr)+(24))>>2)] = date.getDay();
  
      var yday = ydayFromDate(date)|0;
      HEAP32[(((tmPtr)+(28))>>2)] = yday;
      HEAP32[(((tmPtr)+(36))>>2)] = -(date.getTimezoneOffset() * 60);
  
      // Attention: DST is in December in South, and some regions don't have DST at all.
      var start = new Date(date.getFullYear(), 0, 1);
      var summerOffset = new Date(date.getFullYear(), 6, 1).getTimezoneOffset();
      var winterOffset = start.getTimezoneOffset();
      var dst = (summerOffset != winterOffset && date.getTimezoneOffset() == Math.min(winterOffset, summerOffset))|0;
      HEAP32[(((tmPtr)+(32))>>2)] = dst;
      return 0;
    ;
  }

  
  var __mktime_js = function(tmPtr) {
  
  var ret = (() => { 
      var date = new Date(HEAP32[(((tmPtr)+(20))>>2)] + 1900,
                          HEAP32[(((tmPtr)+(16))>>2)],
                          HEAP32[(((tmPtr)+(12))>>2)],
                          HEAP32[(((tmPtr)+(8))>>2)],
                          HEAP32[(((tmPtr)+(4))>>2)],
                          HEAP32[((tmPtr)>>2)],
                          0);
      if (isNaN(date.getTime())) {
        return -1;
      }
  
      // There's an ambiguous hour when the time goes back; the tm_isdst field is
      // used to disambiguate it.  Date() basically guesses, so we fix it up if it
      // guessed wrong, or fill in tm_isdst with the guess if it's -1.
      var dst = HEAP32[(((tmPtr)+(32))>>2)];
      var guessedOffset = date.getTimezoneOffset();
      var start = new Date(date.getFullYear(), 0, 1);
      var summerOffset = new Date(date.getFullYear(), 6, 1).getTimezoneOffset();
      var winterOffset = start.getTimezoneOffset();
      var dstOffset = Math.min(winterOffset, summerOffset); // DST is in December in South
      if (dst < 0) {
        // Attention: some regions don't have DST at all.
        dst = Number(summerOffset != winterOffset && dstOffset == guessedOffset);
      } else if ((dst > 0) != (dstOffset == guessedOffset)) {
        var nonDstOffset = Math.max(winterOffset, summerOffset);
        var trueOffset = dst > 0 ? dstOffset : nonDstOffset;
        // Don't try setMinutes(date.getMinutes() + ...) -- it's messed up.
        date.setTime(date.getTime() + (trueOffset - guessedOffset)*60000);
        if (isNaN(date.getTime())) {
          return -1;
        }
      }
  
      HEAP32[(((tmPtr)+(32))>>2)] = dst;
      HEAP32[(((tmPtr)+(24))>>2)] = date.getDay();
      var yday = ydayFromDate(date)|0;
      HEAP32[(((tmPtr)+(28))>>2)] = yday;
      // To match expected behavior, update fields from date
      HEAP32[((tmPtr)>>2)] = date.getSeconds();
      HEAP32[(((tmPtr)+(4))>>2)] = date.getMinutes();
      HEAP32[(((tmPtr)+(8))>>2)] = date.getHours();
      HEAP32[(((tmPtr)+(12))>>2)] = date.getDate();
      HEAP32[(((tmPtr)+(16))>>2)] = date.getMonth();
      HEAP32[(((tmPtr)+(20))>>2)] = date.getYear();
  
      // Return time in seconds
      return date.getTime() / 1000;
     })();
  return BigInt(ret);
  };

  var timers = {
  };
  
  var clearTimers = () => {
      for (var t of Object.values(timers)) {
        clearTimeout(t.id);
      }
    };
  
  
  
  var _emscripten_get_now = () => performance.timeOrigin + performance.now();
  
  
  function __setitimer_js(which, timeout_ms) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(47, 0, 1, which, timeout_ms);
  
      // First, clear any existing timer.
      if (timers[which]) {
        clearTimeout(timers[which].id);
        delete timers[which];
      }
  
      // A timeout of zero simply cancels the current timeout so we have nothing
      // more to do.
      if (!timeout_ms) return 0;
  
      var id = setTimeout(() => {
        assert(which in timers);
        delete timers[which];
        callUserCallback(() => __emscripten_timeout(which, _emscripten_get_now()));
      }, timeout_ms);
      timers[which] = { id, timeout_ms };
      return 0;
    
  }
  

  
  var __tzset_js = (timezone, daylight, std_name, dst_name) => {
      // TODO: Use (malleable) environment variables instead of system settings.
      var currentYear = new Date().getFullYear();
      var winter = new Date(currentYear, 0, 1);
      var summer = new Date(currentYear, 6, 1);
      var winterOffset = winter.getTimezoneOffset();
      var summerOffset = summer.getTimezoneOffset();
  
      // Local standard timezone offset. Local standard time is not adjusted for
      // daylight savings.  This code uses the fact that getTimezoneOffset returns
      // a greater value during Standard Time versus Daylight Saving Time (DST).
      // Thus it determines the expected output during Standard Time, and it
      // compares whether the output of the given date the same (Standard) or less
      // (DST).
      var stdTimezoneOffset = Math.max(winterOffset, summerOffset);
  
      // timezone is specified as seconds west of UTC ("The external variable
      // `timezone` shall be set to the difference, in seconds, between
      // Coordinated Universal Time (UTC) and local standard time."), the same
      // as returned by stdTimezoneOffset.
      // See http://pubs.opengroup.org/onlinepubs/009695399/functions/tzset.html
      HEAPU32[((timezone)>>2)] = stdTimezoneOffset * 60;
  
      HEAP32[((daylight)>>2)] = Number(winterOffset != summerOffset);
  
      var extractZone = (timezoneOffset) => {
        // Why inverse sign?
        // Read here https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTimezoneOffset
        var sign = timezoneOffset >= 0 ? "-" : "+";
  
        var absOffset = Math.abs(timezoneOffset)
        var hours = String(Math.floor(absOffset / 60)).padStart(2, "0");
        var minutes = String(absOffset % 60).padStart(2, "0");
  
        return `UTC${sign}${hours}${minutes}`;
      }
  
      var winterName = extractZone(winterOffset);
      var summerName = extractZone(summerOffset);
      assert(winterName);
      assert(summerName);
      assert(lengthBytesUTF8(winterName) <= 16, `timezone name truncated to fit in TZNAME_MAX (${winterName})`);
      assert(lengthBytesUTF8(summerName) <= 16, `timezone name truncated to fit in TZNAME_MAX (${summerName})`);
      if (summerOffset < winterOffset) {
        // Northern hemisphere
        stringToUTF8(winterName, std_name, 17);
        stringToUTF8(summerName, dst_name, 17);
      } else {
        stringToUTF8(winterName, dst_name, 17);
        stringToUTF8(summerName, std_name, 17);
      }
    };

  
  var _emscripten_date_now = () => Date.now();
  
  var nowIsMonotonic = 1;
  
  var checkWasiClock = (clock_id) => clock_id >= 0 && clock_id <= 3;
  
  function _clock_time_get(clk_id, ignored_precision, ptime) {
    ignored_precision = bigintToI53Checked(ignored_precision);
  
  
      if (!checkWasiClock(clk_id)) {
        return 28;
      }
      var now;
      // all wasi clocks but realtime are monotonic
      if (clk_id === 0) {
        now = _emscripten_date_now();
      } else if (nowIsMonotonic) {
        now = _emscripten_get_now();
      } else {
        return 52;
      }
      // "now" is in ms, and wasi times are in ns.
      var nsec = Math.round(now * 1000 * 1000);
      HEAP64[((ptime)>>3)] = BigInt(nsec);
      return 0;
    ;
  }

  
  var _emscripten_check_blocking_allowed = () => {
      if (ENVIRONMENT_IS_NODE) return;
  
      if (ENVIRONMENT_IS_WORKER) return; // Blocking in a worker/pthread is fine.
  
      warnOnce('Blocking on the main thread is very dangerous, see https://emscripten.org/docs/porting/pthreads.html#blocking-on-the-main-browser-thread');
  
    };


  var _emscripten_err = (str) => err(UTF8ToString(str));

  var runtimeKeepalivePush = () => {
      runtimeKeepaliveCounter += 1;
    };
  var _emscripten_exit_with_live_runtime = () => {
      runtimeKeepalivePush();
      throw 'unwind';
    };

  var getHeapMax = () =>
      HEAPU8.length;
  var _emscripten_get_heap_max = () => getHeapMax();


  var _emscripten_num_logical_cores = () =>
      ENVIRONMENT_IS_NODE ? require('node:os').cpus().length :
      navigator['hardwareConcurrency'];

  var abortOnCannotGrowMemory = (requestedSize) => {
      abort(`Cannot enlarge memory arrays to size ${requestedSize} bytes (OOM). Either (1) compile with -sINITIAL_MEMORY=X with X higher than the current value ${HEAP8.length}, (2) compile with -sALLOW_MEMORY_GROWTH which allows increasing the size at runtime, or (3) if you want malloc to return NULL (0) instead of this abort, compile with -sABORTING_MALLOC=0`);
    };
  var _emscripten_resize_heap = (requestedSize) => {
      var oldSize = HEAPU8.length;
      // With CAN_ADDRESS_2GB or MEMORY64, pointers are already unsigned.
      requestedSize >>>= 0;
      abortOnCannotGrowMemory(requestedSize);
    };

  var _emscripten_unwind_to_js_event_loop = () => {
      throw 'unwind';
    };

  var ENV = {
  };
  
  var getExecutableName = () => thisProgram;
  var getEnvStrings = () => {
      if (!getEnvStrings.strings) {
        // Default values.
        var lang = (globalThis.navigator?.language ?? 'C').replace('-', '_') + '.UTF-8';
        var env = {
          'USER': 'web_user',
          'LOGNAME': 'web_user',
          'PATH': '/',
          'PWD': '/',
          'HOME': '/home/web_user',
          'LANG': lang,
          '_': getExecutableName()
        };
        // Apply the user-provided values, if any.
        for (var x in ENV) {
          // x is a key in ENV; if ENV[x] is undefined, that means it was
          // explicitly set to be so. We allow user code to do that to
          // force variables with default values to remain unset.
          if (ENV[x] === undefined) delete env[x];
          else env[x] = ENV[x];
        }
        var strings = [];
        for (var x in env) {
          strings.push(`${x}=${env[x]}`);
        }
        getEnvStrings.strings = strings;
      }
      return getEnvStrings.strings;
    };
  
  
  
  function _environ_get(__environ, environ_buf) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(48, 0, 1, __environ, environ_buf);
  
      var bufSize = 0;
      var envp = 0;
      for (var string of getEnvStrings()) {
        var ptr = environ_buf + bufSize;
        HEAPU32[(((__environ)+(envp))>>2)] = ptr;
        bufSize += stringToUTF8(string, ptr, Infinity) + 1;
        envp += 4;
      }
      return 0;
    
  }
  

  
  
  
  function _environ_sizes_get(penviron_count, penviron_buf_size) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(49, 0, 1, penviron_count, penviron_buf_size);
  
      var strings = getEnvStrings();
      HEAPU32[((penviron_count)>>2)] = strings.length;
      var bufSize = 0;
      for (var string of strings) {
        bufSize += lengthBytesUTF8(string) + 1;
      }
      HEAPU32[((penviron_buf_size)>>2)] = bufSize;
      return 0;
    
  }
  


  
  
  function _fd_close(fd) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(50, 0, 1, fd);
  
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd);
      FS.close(stream);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  
  }
  

  
  
  function _fd_fdstat_get(fd, pbuf) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(51, 0, 1, fd, pbuf);
  
  try {
  
      var rightsBase = 0;
      var rightsInheriting = 0;
      var flags = 0;
      {
        var stream = SYSCALLS.getStreamFromFD(fd);
        // All character devices are terminals (other things a Linux system would
        // assume is a character device, like the mouse, we have special APIs for).
        var type = stream.tty ? 2 :
                   FS.isDir(stream.mode) ? 3 :
                   FS.isLink(stream.mode) ? 7 :
                   4;
      }
      HEAP8[pbuf] = type;
      HEAP16[(((pbuf)+(2))>>1)] = flags;
      HEAP64[(((pbuf)+(8))>>3)] = BigInt(rightsBase);
      HEAP64[(((pbuf)+(16))>>3)] = BigInt(rightsInheriting);
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  
  }
  

  /** @param {number=} offset */
  var doReadv = (stream, iov, iovcnt, offset) => {
      var ret = 0;
      for (var i = 0; i < iovcnt; i++) {
        var ptr = HEAPU32[((iov)>>2)];
        var len = HEAPU32[(((iov)+(4))>>2)];
        iov += 8;
        var curr = FS.read(stream, HEAP8, ptr, len, offset);
        if (curr < 0) return -1;
        ret += curr;
        if (curr < len) break; // nothing more to read
        if (typeof offset != 'undefined') {
          offset += curr;
        }
      }
      return ret;
    };
  
  
  
  
  function _fd_pread(fd, iov, iovcnt, offset, pnum) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(52, 0, 1, fd, iov, iovcnt, offset, pnum);
  
    offset = bigintToI53Checked(offset);
  
  
  try {
  
      if (isNaN(offset)) return 22;
      var stream = SYSCALLS.getStreamFromFD(fd)
      var num = doReadv(stream, iov, iovcnt, offset);
      HEAPU32[((pnum)>>2)] = num;
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  ;
  
  }
  

  /** @param {number=} offset */
  var doWritev = (stream, iov, iovcnt, offset) => {
      var ret = 0;
      for (var i = 0; i < iovcnt; i++) {
        var ptr = HEAPU32[((iov)>>2)];
        var len = HEAPU32[(((iov)+(4))>>2)];
        iov += 8;
        var curr = FS.write(stream, HEAP8, ptr, len, offset);
        if (curr < 0) return -1;
        ret += curr;
        if (curr < len) {
          // No more space to write.
          break;
        }
        if (typeof offset != 'undefined') {
          offset += curr;
        }
      }
      return ret;
    };
  
  
  
  
  function _fd_pwrite(fd, iov, iovcnt, offset, pnum) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(53, 0, 1, fd, iov, iovcnt, offset, pnum);
  
    offset = bigintToI53Checked(offset);
  
  
  try {
  
      if (isNaN(offset)) return 22;
      var stream = SYSCALLS.getStreamFromFD(fd)
      var num = doWritev(stream, iov, iovcnt, offset);
      HEAPU32[((pnum)>>2)] = num;
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  ;
  
  }
  

  
  
  
  function _fd_read(fd, iov, iovcnt, pnum) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(54, 0, 1, fd, iov, iovcnt, pnum);
  
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd);
      var num = doReadv(stream, iov, iovcnt);
      HEAPU32[((pnum)>>2)] = num;
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  
  }
  

  
  
  
  function _fd_seek(fd, offset, whence, newOffset) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(55, 0, 1, fd, offset, whence, newOffset);
  
    offset = bigintToI53Checked(offset);
  
  
  try {
  
      if (isNaN(offset)) return 22;
      var stream = SYSCALLS.getStreamFromFD(fd);
      FS.llseek(stream, offset, whence);
      HEAP64[((newOffset)>>3)] = BigInt(stream.position);
      if (stream.getdents && offset === 0 && whence === 0) stream.getdents = null; // reset readdir state
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  ;
  
  }
  

  
  var _fd_sync = 
  function(fd) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(56, 0, 2, fd);
  
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd);
      var rtn = stream.stream_ops?.fsync?.(stream);
      return new Promise((resolve) => {
        var mount = stream.node.mount;
        if (mount?.type.syncfs) {
          mount.type.syncfs(mount, false, (err) => resolve(err ? 29 : 0));
        } else {
          resolve(rtn);
        }
      });
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  
  }
  ;

  
  
  
  function _fd_write(fd, iov, iovcnt, pnum) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(57, 0, 1, fd, iov, iovcnt, pnum);
  
  try {
  
      var stream = SYSCALLS.getStreamFromFD(fd);
      var num = doWritev(stream, iov, iovcnt);
      HEAPU32[((pnum)>>2)] = num;
      return 0;
    } catch (e) {
    if (typeof FS == 'undefined' || !(e.name === 'ErrnoError')) throw e;
    return e.errno;
  }
  
  }
  

  
  
  
  
  
  
  
  
  
  
  function _getaddrinfo(node, service, hint, out) {
  if (ENVIRONMENT_IS_PTHREAD)
    return proxyToMainThread(58, 0, 1, node, service, hint, out);
  
      // Note getaddrinfo currently only returns a single addrinfo with ai_next defaulting to NULL. When NULL
      // hints are specified or ai_family set to AF_UNSPEC or ai_socktype or ai_protocol set to 0 then we
      // really should provide a linked list of suitable addrinfo values.
      var addrs = [];
      var canon = null;
      var addr = 0;
      var port = 0;
      var flags = 0;
      var family = 0;
      var type = 0;
      var proto = 0;
      var ai, last;
  
      function allocaddrinfo(family, type, proto, canon, addr, port) {
        var sa, salen, ai;
        var errno;
  
        salen = family === 10 ?
          28 :
          16;
        addr = family === 10 ?
          inetNtop6(addr) :
          inetNtop4(addr);
        sa = _malloc(salen);
        errno = writeSockaddr(sa, family, addr, port);
        assert(!errno);
  
        ai = _malloc(32);
        HEAP32[(((ai)+(4))>>2)] = family;
        HEAP32[(((ai)+(8))>>2)] = type;
        HEAP32[(((ai)+(12))>>2)] = proto;
        HEAPU32[(((ai)+(24))>>2)] = canon;
        HEAPU32[(((ai)+(20))>>2)] = sa;
        if (family === 10) {
          HEAP32[(((ai)+(16))>>2)] = 28;
        } else {
          HEAP32[(((ai)+(16))>>2)] = 16;
        }
        HEAP32[(((ai)+(28))>>2)] = 0;
  
        return ai;
      }
  
      if (hint) {
        flags = HEAP32[((hint)>>2)];
        family = HEAP32[(((hint)+(4))>>2)];
        type = HEAP32[(((hint)+(8))>>2)];
        proto = HEAP32[(((hint)+(12))>>2)];
      }
      if (type && !proto) {
        proto = type === 2 ? 17 : 6;
      }
      if (!type && proto) {
        type = proto === 17 ? 2 : 1;
      }
  
      // If type or proto are set to zero in hints we should really be returning multiple addrinfo values, but for
      // now default to a TCP STREAM socket so we can at least return a sensible addrinfo given NULL hints.
      if (proto === 0) {
        proto = 6;
      }
      if (type === 0) {
        type = 1;
      }
  
      if (!node && !service) {
        return -2;
      }
      if (flags & ~(1|2|4|
          1024|8|16|32)) {
        return -1;
      }
      if (hint !== 0 && (HEAP32[((hint)>>2)] & 2) && !node) {
        return -1;
      }
      if (flags & 32) {
        // TODO
        return -2;
      }
      if (type !== 0 && type !== 1 && type !== 2) {
        return -7;
      }
      if (family !== 0 && family !== 2 && family !== 10) {
        return -6;
      }
  
      if (service) {
        service = UTF8ToString(service);
        port = parseInt(service, 10);
  
        if (isNaN(port)) {
          if (flags & 1024) {
            return -2;
          }
          // TODO support resolving well-known service names from:
          // http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.txt
          return -8;
        }
      }
  
      if (!node) {
        if (family === 0) {
          family = 2;
        }
        if ((flags & 1) === 0) {
          if (family === 2) {
            addr = _htonl(2130706433);
          } else {
            addr = [0, 0, 0, _htonl(1)];
          }
        }
        ai = allocaddrinfo(family, type, proto, null, addr, port);
        HEAPU32[((out)>>2)] = ai;
        return 0;
      }
  
      //
      // try as a numeric address
      //
      node = UTF8ToString(node);
      addr = inetPton4(node);
      if (addr !== null) {
        // incoming node is a valid ipv4 address
        if (family === 0 || family === 2) {
          family = 2;
        }
        else if (family === 10 && (flags & 8)) {
          addr = [0, 0, _htonl(0xffff), addr];
          family = 10;
        } else {
          return -2;
        }
      } else {
        addr = inetPton6(node);
        if (addr !== null) {
          // incoming node is a valid ipv6 address
          if (family === 0 || family === 10) {
            family = 10;
          } else {
            return -2;
          }
        }
      }
      if (addr != null) {
        ai = allocaddrinfo(family, type, proto, node, addr, port);
        HEAPU32[((out)>>2)] = ai;
        return 0;
      }
      if (flags & 4) {
        return -2;
      }
  
      //
      // try as a hostname
      //
      // resolve the hostname to a temporary fake address
      node = DNS.lookup_name(node);
      addr = inetPton4(node);
      if (family === 0) {
        family = 2;
      } else if (family === 10) {
        addr = [0, 0, _htonl(0xffff), addr];
      }
      ai = allocaddrinfo(family, type, proto, null, addr, port);
      HEAPU32[((out)>>2)] = ai;
      return 0;
    
  }
  

  
  
  var _getnameinfo = (sa, salen, node, nodelen, serv, servlen, flags) => {
      var info = readSockaddr(sa, salen);
      if (info.errno) {
        return -6;
      }
      var port = info.port;
      var addr = info.addr;
  
      var overflowed = false;
  
      if (node && nodelen) {
        var lookup;
        if ((flags & 1) || !(lookup = DNS.lookup_addr(addr))) {
          if (flags & 8) {
            return -2;
          }
        } else {
          addr = lookup;
        }
        var numBytesWrittenExclNull = stringToUTF8(addr, node, nodelen);
  
        if (numBytesWrittenExclNull+1 >= nodelen) {
          overflowed = true;
        }
      }
  
      if (serv && servlen) {
        port = '' + port;
        var numBytesWrittenExclNull = stringToUTF8(port, serv, servlen);
  
        if (numBytesWrittenExclNull+1 >= servlen) {
          overflowed = true;
        }
      }
  
      if (overflowed) {
        // Note: even when we overflow, getnameinfo() is specced to write out the truncated results.
        return -12;
      }
  
      return 0;
    };

  var Protocols = {
  list:[],
  map:{
  },
  };
  
  var stringToAscii = (str, buffer) => {
      for (var i = 0; i < str.length; ++i) {
        assert(str.charCodeAt(i) === (str.charCodeAt(i) & 0xff));
        HEAP8[buffer++] = str.charCodeAt(i);
      }
      // Null-terminate the string
      HEAP8[buffer] = 0;
    };
  
  var _setprotoent = (stayopen) => {
      // void setprotoent(int stayopen);
  
      // Allocate and populate a protoent structure given a name, protocol number and array of aliases
      function allocprotoent(name, proto, aliases) {
        // write name into buffer
        var nameBuf = _malloc(name.length + 1);
        stringToAscii(name, nameBuf);
  
        // write aliases into buffer
        var j = 0;
        var length = aliases.length;
        var aliasListBuf = _malloc((length + 1) * 4); // Use length + 1 so we have space for the terminating NULL ptr.
  
        for (var i = 0; i < length; i++, j += 4) {
          var alias = aliases[i];
          var aliasBuf = _malloc(alias.length + 1);
          stringToAscii(alias, aliasBuf);
          HEAPU32[(((aliasListBuf)+(j))>>2)] = aliasBuf;
        }
        HEAPU32[(((aliasListBuf)+(j))>>2)] = 0; // Terminating NULL pointer.
  
        // generate protoent
        var pe = _malloc(12);
        HEAPU32[((pe)>>2)] = nameBuf;
        HEAPU32[(((pe)+(4))>>2)] = aliasListBuf;
        HEAP32[(((pe)+(8))>>2)] = proto;
        return pe;
      };
  
      // Populate the protocol 'database'. The entries are limited to tcp and udp, though it is fairly trivial
      // to add extra entries from /etc/protocols if desired - though not sure if that'd actually be useful.
      var list = Protocols.list;
      var map  = Protocols.map;
      if (list.length === 0) {
          var entry = allocprotoent('tcp', 6, ['TCP']);
          list.push(entry);
          map['tcp'] = map['6'] = entry;
          entry = allocprotoent('udp', 17, ['UDP']);
          list.push(entry);
          map['udp'] = map['17'] = entry;
      }
  
      _setprotoent.index = 0;
    };
  
  
  var _getprotobyname = (name) => {
      // struct protoent *getprotobyname(const char *);
      name = UTF8ToString(name);
      _setprotoent(true);
      var result = Protocols.map[name];
      return result;
    };

  
  var _getprotobynumber = (number) => {
      // struct protoent *getprotobynumber(int proto);
      _setprotoent(true);
      var result = Protocols.map[number];
      return result;
    };













  var getCFunc = (ident) => {
      var func = Module['_' + ident]; // closure exported function
      assert(func, `Cannot call unknown function ${ident}, make sure it is exported`);
      return func;
    };
  
  var writeArrayToMemory = (array, buffer) => {
      assert(array.length >= 0, 'writeArrayToMemory array must have a length (should be an array or typed array)')
      HEAP8.set(array, buffer);
    };
  
  
  
  var stringToUTF8OnStack = (str) => {
      var size = lengthBytesUTF8(str) + 1;
      var ret = stackAlloc(size);
      stringToUTF8(str, ret, size);
      return ret;
    };
  
  
  
  
  
    /**
   * @param {string|null=} returnType
   * @param {Array=} argTypes
   * @param {Array=} args
   * @param {Object=} opts
   */
  var ccall = (ident, returnType, argTypes, args, opts) => {
      // For fast lookup of conversion functions
      var toC = {
        'string': (str) => {
          var ret = 0;
          if (str !== null && str !== undefined && str !== 0) { // null string
            ret = stringToUTF8OnStack(str);
          }
          return ret;
        },
        'array': (arr) => {
          var ret = stackAlloc(arr.length);
          writeArrayToMemory(arr, ret);
          return ret;
        }
      };
  
      function convertReturnValue(ret) {
        if (returnType === 'string') {
          return UTF8ToString(ret);
        }
        if (returnType === 'boolean') return Boolean(ret);
        return ret;
      }
  
      var func = getCFunc(ident);
      var cArgs = [];
      var stack = 0;
      assert(returnType !== 'array', 'return type should not be "array"');
      if (args) {
        for (var i = 0; i < args.length; i++) {
          var converter = toC[argTypes[i]];
          if (converter) {
            if (stack === 0) stack = stackSave();
            cArgs[i] = converter(args[i]);
          } else {
            cArgs[i] = args[i];
          }
        }
      }
      var ret = func(...cArgs);
      function onDone(ret) {
        if (stack !== 0) stackRestore(stack);
        return convertReturnValue(ret);
      }
  
      ret = onDone(ret);
      return ret;
    };

  
    /**
   * @param {string=} returnType
   * @param {Array=} argTypes
   * @param {Object=} opts
   */
  var cwrap = (ident, returnType, argTypes, opts) => {
      return (...args) => ccall(ident, returnType, argTypes, args, opts);
    };

  var FS_createPath = (...args) => FS.createPath(...args);



  var FS_unlink = (...args) => FS.unlink(...args);

  var FS_createLazyFile = (...args) => FS.createLazyFile(...args);

  var FS_createDevice = (...args) => FS.createDevice(...args);


PThread.init();;

  FS.createPreloadedFile = FS_createPreloadedFile;
  FS.preloadFile = FS_preloadFile;
  FS.staticInit();;
// End JS library code

// include: postlibrary.js
// This file is included after the automatically-generated JS library code
// but before the wasm module is created.

{
  // With WASM_ESM_INTEGRATION this has to happen at the top level and not
  // delayed until processModuleArgs.
  initMemory();

  // Begin ATMODULES hooks
  if (Module['noExitRuntime']) noExitRuntime = Module['noExitRuntime'];

if (Module['print']) out = Module['print'];
if (Module['printErr']) err = Module['printErr'];
  // End ATMODULES hooks

  checkIncomingModuleAPI();

  if (Module['arguments']) programArgs = Module['arguments'];
  if (Module['thisProgram']) thisProgram = Module['thisProgram'];

  // Assertions on removed incoming Module JS APIs.
  assert(typeof Module['memoryInitializerPrefixURL'] == 'undefined', 'Module.memoryInitializerPrefixURL option was removed, use Module.locateFile instead');
  assert(typeof Module['pthreadMainPrefixURL'] == 'undefined', 'Module.pthreadMainPrefixURL option was removed, use Module.locateFile instead');
  assert(typeof Module['cdInitializerPrefixURL'] == 'undefined', 'Module.cdInitializerPrefixURL option was removed, use Module.locateFile instead');
  assert(typeof Module['filePackagePrefixURL'] == 'undefined', 'Module.filePackagePrefixURL option was removed, use Module.locateFile instead');
  assert(typeof Module['read'] == 'undefined', 'Module.read option was removed');
  assert(typeof Module['readAsync'] == 'undefined', 'Module.readAsync option was removed (modify readAsync in JS)');
  assert(typeof Module['readBinary'] == 'undefined', 'Module.readBinary option was removed (modify readBinary in JS)');
  assert(typeof Module['setWindowTitle'] == 'undefined', 'Module.setWindowTitle option was removed (modify emscripten_set_window_title in JS)');
  assert(typeof Module['TOTAL_MEMORY'] == 'undefined', 'Module.TOTAL_MEMORY has been renamed Module.INITIAL_MEMORY');
  assert(typeof Module['ENVIRONMENT'] == 'undefined', 'Module.ENVIRONMENT has been deprecated. To force the environment, use the ENVIRONMENT compile-time option (for example, -sENVIRONMENT=web or -sENVIRONMENT=node)');
  assert(typeof Module['STACK_SIZE'] == 'undefined', 'STACK_SIZE can no longer be set at runtime.  Use -sSTACK_SIZE at link time')

  var preInit = Module['preInit'];
  if (preInit) {
    if (typeof preInit == 'function') Module['preInit'] = preInit = [preInit];
    // Written as a loop so that preInit functions that themselves add more
    // preInit functions.  Is this actually needed?
    while (preInit.length > 0) {
      preInit.shift()();
    }
  }
  consumedModuleProp('preInit');
}

// Begin runtime exports
  Module['ENV'] = ENV;
  Module['addRunDependency'] = addRunDependency;
  Module['removeRunDependency'] = removeRunDependency;
  Module['ccall'] = ccall;
  Module['cwrap'] = cwrap;
  Module['setValue'] = setValue;
  Module['getValue'] = getValue;
  Module['UTF8ToString'] = UTF8ToString;
  Module['stringToUTF8'] = stringToUTF8;
  Module['lengthBytesUTF8'] = lengthBytesUTF8;
  Module['FS_preloadFile'] = FS_preloadFile;
  Module['FS_unlink'] = FS_unlink;
  Module['FS_createPath'] = FS_createPath;
  Module['FS_createDevice'] = FS_createDevice;
  Module['FS'] = FS;
  Module['FS_createDataFile'] = FS_createDataFile;
  Module['FS_createLazyFile'] = FS_createLazyFile;
  Module['IDBFS'] = IDBFS;
  var missingLibrarySymbols = [
  'writeI53ToI64',
  'writeI53ToI64Clamped',
  'writeI53ToI64Signaling',
  'writeI53ToU64Clamped',
  'writeI53ToU64Signaling',
  'readI53FromU64',
  'convertI32PairToI53',
  'convertI32PairToI53Checked',
  'convertU32PairToI53',
  'getTempRet0',
  'createNamedFunction',
  'growMemory',
  'withStackSave',
  'readEmAsmArgs',
  'jstoi_q',
  'autoResumeAudioContext',
  'getDynCaller',
  'dynCall',
  'runtimeKeepalivePop',
  'asmjsMangle',
  'alignMemory',
  'HandleAllocator',
  'addOnInit',
  'addOnPostCtor',
  'addOnPreMain',
  'addOnExit',
  'STACK_SIZE',
  'STACK_ALIGN',
  'POINTER_SIZE',
  'ASSERTIONS',
  'convertJsFunctionToWasm',
  'getEmptyTableSlot',
  'updateTableMap',
  'getFunctionAddress',
  'addFunction',
  'removeFunction',
  'intArrayToString',
  'AsciiToString',
  'UTF16ToString',
  'stringToUTF16',
  'lengthBytesUTF16',
  'UTF32ToString',
  'stringToUTF32',
  'lengthBytesUTF32',
  'stringToNewUTF8',
  'registerKeyEventCallback',
  'maybeCStringToJsString',
  'findEventTarget',
  'getBoundingClientRect',
  'fillMouseEventData',
  'registerMouseEventCallback',
  'registerWheelEventCallback',
  'registerUiEventCallback',
  'registerFocusEventCallback',
  'fillDeviceOrientationEventData',
  'registerDeviceOrientationEventCallback',
  'fillDeviceMotionEventData',
  'registerDeviceMotionEventCallback',
  'screenOrientation',
  'fillOrientationChangeEventData',
  'registerOrientationChangeEventCallback',
  'fillFullscreenChangeEventData',
  'registerFullscreenChangeEventCallback',
  'JSEvents_requestFullscreen',
  'JSEvents_resizeCanvasForFullscreen',
  'registerRestoreOldStyle',
  'hideEverythingExceptGivenElement',
  'restoreHiddenElements',
  'setLetterbox',
  'softFullscreenResizeWebGLRenderTarget',
  'doRequestFullscreen',
  'fillPointerlockChangeEventData',
  'registerPointerlockChangeEventCallback',
  'registerPointerlockErrorEventCallback',
  'requestPointerLock',
  'fillVisibilityChangeEventData',
  'registerVisibilityChangeEventCallback',
  'registerTouchEventCallback',
  'fillGamepadEventData',
  'registerGamepadEventCallback',
  'registerBeforeUnloadEventCallback',
  'fillBatteryEventData',
  'registerBatteryEventCallback',
  'setCanvasElementSizeCallingThread',
  'setCanvasElementSizeMainThread',
  'setCanvasElementSize',
  'getCanvasSizeCallingThread',
  'getCanvasSizeMainThread',
  'getCanvasElementSize',
  'jsStackTrace',
  'getCallstack',
  'convertPCtoSourceLocation',
  'wasiRightsToMuslOFlags',
  'wasiOFlagsToMuslOFlags',
  'safeSetTimeout',
  'setImmediateWrapped',
  'safeRequestAnimationFrame',
  'clearImmediateWrapped',
  'registerPostMainLoop',
  'registerPreMainLoop',
  'getPromise',
  'makePromise',
  'addPromise',
  'idsToPromises',
  'makePromiseCallback',
  'getExceptionMessageCommon',
  'incrementUncaughtExceptionCount',
  'decrementUncaughtExceptionCount',
  'incrementExceptionRefcount',
  'decrementExceptionRefcount',
  'getExceptionMessage',
  'Browser_asyncPrepareDataCounter',
  'arraySum',
  'addDays',
  'FS_mkdirTree',
  '_setNetworkCallback',
  'heapObjectForWebGLType',
  'toTypedArrayIndex',
  'webgl_enable_ANGLE_instanced_arrays',
  'webgl_enable_OES_vertex_array_object',
  'webgl_enable_WEBGL_draw_buffers',
  'webgl_enable_WEBGL_multi_draw',
  'webgl_enable_EXT_polygon_offset_clamp',
  'webgl_enable_EXT_clip_control',
  'webgl_enable_WEBGL_polygon_mode',
  'emscriptenWebGLGet',
  'computeUnpackAlignedImageSize',
  'colorChannelsInGlTextureFormat',
  'emscriptenWebGLGetTexPixelData',
  'emscriptenWebGLGetUniform',
  'webglGetProgramUniformLocation',
  'webglGetUniformLocation',
  'webglPrepareUniformLocationsBeforeFirstUse',
  'webglGetLeftBracePos',
  'emscriptenWebGLGetVertexAttrib',
  '__glGetActiveAttribOrUniform',
  'writeGLArray',
  'emscripten_webgl_destroy_context_before_on_calling_thread',
  'registerWebGlEventCallback',
  'runAndAbortIfError',
  'ALLOC_NORMAL',
  'ALLOC_STACK',
  'allocate',
  'writeStringToMemory',
  'writeAsciiToMemory',
  'allocateUTF8',
  'allocateUTF8OnStack',
  'demangle',
  'stackTrace',
  'getNativeTypeSize',
];
missingLibrarySymbols.forEach(missingLibrarySymbol)

  var unexportedSymbols = [
  'run',
  'out',
  'err',
  'callMain',
  'abort',
  'wasmExports',
  'writeStackCookie',
  'checkStackCookie',
  'readI53FromI64',
  'INT53_MAX',
  'INT53_MIN',
  'bigintToI53Checked',
  'HEAP8',
  'HEAP16',
  'HEAPU16',
  'HEAP32',
  'HEAPU32',
  'HEAPF32',
  'HEAPF64',
  'HEAP64',
  'HEAPU64',
  'stackSave',
  'stackRestore',
  'stackAlloc',
  'setTempRet0',
  'ptrToString',
  'zeroMemory',
  'exitJS',
  'getHeapMax',
  'abortOnCannotGrowMemory',
  'setStackLimits',
  'ERRNO_CODES',
  'strError',
  'inetPton4',
  'inetNtop4',
  'inetPton6',
  'inetNtop6',
  'readSockaddr',
  'writeSockaddr',
  'DNS',
  'Protocols',
  'Sockets',
  'timers',
  'warnOnce',
  'readEmAsmArgsArray',
  'getExecutableName',
  'handleException',
  'keepRuntimeAlive',
  'runtimeKeepalivePush',
  'callUserCallback',
  'maybeExit',
  'asyncLoad',
  'mmapAlloc',
  'wasmTable',
  'wasmMemory',
  'getUniqueRunDependency',
  'noExitRuntime',
  'addOnPreRun',
  'addOnPostRun',
  'freeTableIndexes',
  'functionsInTableMap',
  'PATH',
  'PATH_FS',
  'UTF8Decoder',
  'UTF8ArrayToString',
  'stringToUTF8Array',
  'intArrayFromString',
  'stringToAscii',
  'UTF16Decoder',
  'stringToUTF8OnStack',
  'writeArrayToMemory',
  'JSEvents',
  'specialHTMLTargets',
  'findCanvasEventTarget',
  'currentFullscreenStrategy',
  'restoreOldWindowedStyle',
  'UNWIND_CACHE',
  'ExitStatus',
  'getEnvStrings',
  'checkWasiClock',
  'doReadv',
  'doWritev',
  'initRandomFill',
  'randomFill',
  'emSetImmediate',
  'emClearImmediate_deps',
  'emClearImmediate',
  'promiseMap',
  'uncaughtExceptionCount',
  'exceptionLast',
  'exceptionCaught',
  'ExceptionInfo',
  'findMatchingCatch',
  'Browser',
  'requestFullscreen',
  'requestFullScreen',
  'setCanvasSize',
  'getUserMedia',
  'createContext',
  'getPreloadedImageData__data',
  'wget',
  'MONTH_DAYS_REGULAR',
  'MONTH_DAYS_LEAP',
  'MONTH_DAYS_REGULAR_CUMULATIVE',
  'MONTH_DAYS_LEAP_CUMULATIVE',
  'isLeapYear',
  'ydayFromDate',
  'SYSCALLS',
  'getSocketFromFD',
  'getSocketAddress',
  'preloadPlugins',
  'FS_createPreloadedFile',
  'FS_modeStringToFlags',
  'FS_getMode',
  'FS_fileDataToTypedArray',
  'FS_stdin_getChar_buffer',
  'FS_stdin_getChar',
  'FS_readFile',
  'FS_root',
  'FS_mounts',
  'FS_devices',
  'FS_streams',
  'FS_nextInode',
  'FS_nameTable',
  'FS_currentPath',
  'FS_initialized',
  'FS_ignorePermissions',
  'FS_filesystems',
  'FS_syncFSRequests',
  'FS_lookupPath',
  'FS_getPath',
  'FS_hashName',
  'FS_hashAddNode',
  'FS_hashRemoveNode',
  'FS_lookupNode',
  'FS_createNode',
  'FS_destroyNode',
  'FS_isRoot',
  'FS_isMountpoint',
  'FS_isFile',
  'FS_isDir',
  'FS_isLink',
  'FS_isChrdev',
  'FS_isBlkdev',
  'FS_isFIFO',
  'FS_isSocket',
  'FS_flagsToPermissionString',
  'FS_nodePermissions',
  'FS_mayLookup',
  'FS_mayCreate',
  'FS_mayDelete',
  'FS_mayOpen',
  'FS_checkOpExists',
  'FS_nextfd',
  'FS_getStreamChecked',
  'FS_getStream',
  'FS_createStream',
  'FS_closeStream',
  'FS_dupStream',
  'FS_doSetAttr',
  'FS_chrdev_stream_ops',
  'FS_major',
  'FS_minor',
  'FS_makedev',
  'FS_registerDevice',
  'FS_getDevice',
  'FS_getMounts',
  'FS_syncfs',
  'FS_mount',
  'FS_unmount',
  'FS_lookup',
  'FS_mknod',
  'FS_statfs',
  'FS_statfsStream',
  'FS_statfsNode',
  'FS_create',
  'FS_mkdir',
  'FS_mkdev',
  'FS_symlink',
  'FS_rename',
  'FS_rmdir',
  'FS_readdir',
  'FS_readlink',
  'FS_stat',
  'FS_fstat',
  'FS_lstat',
  'FS_doChmod',
  'FS_chmod',
  'FS_lchmod',
  'FS_fchmod',
  'FS_doChown',
  'FS_chown',
  'FS_lchown',
  'FS_fchown',
  'FS_doTruncate',
  'FS_truncate',
  'FS_ftruncate',
  'FS_utime',
  'FS_open',
  'FS_close',
  'FS_isClosed',
  'FS_llseek',
  'FS_read',
  'FS_write',
  'FS_mmap',
  'FS_msync',
  'FS_ioctl',
  'FS_writeFile',
  'FS_cwd',
  'FS_chdir',
  'FS_createDefaultDirectories',
  'FS_createDefaultDevices',
  'FS_createSpecialDirectories',
  'FS_createStandardStreams',
  'FS_staticInit',
  'FS_init',
  'FS_quit',
  'FS_findObject',
  'FS_analyzePath',
  'FS_createFile',
  'FS_forceLoadFile',
  'MEMFS',
  'TTY',
  'PIPEFS',
  'SOCKFS',
  'tempFixedLengthArray',
  'miniTempWebGLFloatBuffers',
  'miniTempWebGLIntBuffers',
  'GL',
  'AL',
  'GLUT',
  'EGL',
  'GLEW',
  'IDBStore',
  'SDL',
  'SDL_gfx',
  'waitAsyncPolyfilled',
  'print',
  'printErr',
  'jstoi_s',
  'PThread',
  'terminateWorker',
  'cleanupThread',
  'registerTLSInit',
  'spawnThread',
  'exitOnMainThread',
  'proxyToMainThread',
  'proxiedJSCallArgs',
  'invokeEntryPoint',
  'checkMailbox',
];
unexportedSymbols.forEach(unexportedRuntimeSymbol);

  // End runtime exports
  // Begin JS library exports
  // End JS library exports

// end include: postlibrary.js


// proxiedFunctionTable specifies the list of functions that can be called
// either synchronously or asynchronously from other threads in postMessage()d
// or internally queued events. This way a pthread in a Worker can synchronously
// access e.g. the DOM on the main thread.
var proxiedFunctionTable = [
  _proc_exit,
  exitOnMainThread,
  pthreadCreateProxied,
  ___syscall_accept4,
  ___syscall_bind,
  ___syscall_chdir,
  ___syscall_chmod,
  ___syscall_connect,
  ___syscall_dup,
  ___syscall_dup3,
  ___syscall_faccessat,
  ___syscall_fchmod,
  ___syscall_fchown32,
  ___syscall_fchownat,
  ___syscall_fcntl64,
  ___syscall_fstat64,
  ___syscall_ftruncate64,
  ___syscall_getcwd,
  ___syscall_getdents64,
  ___syscall_getpeername,
  ___syscall_getsockname,
  ___syscall_getsockopt,
  ___syscall_ioctl,
  ___syscall_listen,
  ___syscall_lstat64,
  ___syscall_mkdirat,
  ___syscall_newfstatat,
  ___syscall_openat,
  ___syscall_pipe2,
  ___syscall_poll,
  ___syscall_poll_nonblocking,
  ___syscall_readlinkat,
  ___syscall_recvfrom,
  ___syscall_recvmsg,
  ___syscall_renameat,
  ___syscall_rmdir,
  ___syscall_sendmsg,
  ___syscall_sendto,
  ___syscall_setsockopt,
  ___syscall_shutdown,
  ___syscall_socket,
  ___syscall_stat64,
  ___syscall_statfs64,
  ___syscall_symlinkat,
  ___syscall_umask,
  ___syscall_unlinkat,
  ___syscall_utimensat,
  __setitimer_js,
  _environ_get,
  _environ_sizes_get,
  _fd_close,
  _fd_fdstat_get,
  _fd_pread,
  _fd_pwrite,
  _fd_read,
  _fd_seek,
  _fd_sync,
  _fd_write,
  _getaddrinfo
];

function checkIncomingModuleAPI() {
  ignoredModuleProp('fetchSettings');
  ignoredModuleProp('logReadFiles');
  ignoredModuleProp('loadSplitModule');
  ignoredModuleProp('onMalloc');
  ignoredModuleProp('onRealloc');
  ignoredModuleProp('onFree');
  ignoredModuleProp('onSbrkGrow');
  ignoredModuleProp('onCOSCacheHit');
  ignoredModuleProp('onCOSCacheMiss');
  ignoredModuleProp('onCOSStore');
  ignoredModuleProp('GL_MAX_TEXTURE_IMAGE_UNITS');
  ignoredModuleProp('SDL_canPlayWithWebAudio');
  ignoredModuleProp('SDL_numSimultaneouslyQueuedBuffers');
  ignoredModuleProp('freePreloadedMediaOnUse');
  ignoredModuleProp('preinitializedWebGLContext');
  ignoredModuleProp('keyboardListeningElement');
  ignoredModuleProp('doNotCaptureKeyboard');
  ignoredModuleProp('extraStackTrace');
  ignoredModuleProp('preloadPlugins');
  ignoredModuleProp('preMainLoop');
  ignoredModuleProp('postMainLoop');
  ignoredModuleProp('forcedAspectRatio');
  ignoredModuleProp('mainScriptUrlOrBlob');
  ignoredModuleProp('onFullScreen');
  ignoredModuleProp('INITIAL_MEMORY');
  ignoredModuleProp('wasmMemory');
  ignoredModuleProp('wasmBinary');
}

// Imports from the Wasm binary.
var _free = Module['_free'] = makeInvalidEarlyAccess('_free');
var _malloc = Module['_malloc'] = makeInvalidEarlyAccess('_malloc');
var _strerror = makeInvalidEarlyAccess('_strerror');
var _fflush = makeInvalidEarlyAccess('_fflush');
var _nvim_main = Module['_nvim_main'] = makeInvalidEarlyAccess('_nvim_main');
var _ntohs = makeInvalidEarlyAccess('_ntohs');
var _uv_browser_get_shared_state_ptr = Module['_uv_browser_get_shared_state_ptr'] = makeInvalidEarlyAccess('_uv_browser_get_shared_state_ptr');
var _uv_browser_set_readable = Module['_uv_browser_set_readable'] = makeInvalidEarlyAccess('_uv_browser_set_readable');
var _uv_browser_set_writable = Module['_uv_browser_set_writable'] = makeInvalidEarlyAccess('_uv_browser_set_writable');
var _htons = makeInvalidEarlyAccess('_htons');
var _pthread_self = makeInvalidEarlyAccess('_pthread_self');
var _htonl = makeInvalidEarlyAccess('_htonl');
var __emscripten_tls_init = makeInvalidEarlyAccess('__emscripten_tls_init');
var __emscripten_thread_init = makeInvalidEarlyAccess('__emscripten_thread_init');
var ___set_thread_state = makeInvalidEarlyAccess('___set_thread_state');
var __emscripten_thread_crashed = makeInvalidEarlyAccess('__emscripten_thread_crashed');
var _emscripten_stack_get_end = makeInvalidEarlyAccess('_emscripten_stack_get_end');
var _emscripten_stack_get_base = makeInvalidEarlyAccess('_emscripten_stack_get_base');
var __emscripten_run_js_on_main_thread_done = makeInvalidEarlyAccess('__emscripten_run_js_on_main_thread_done');
var __emscripten_run_js_on_main_thread = makeInvalidEarlyAccess('__emscripten_run_js_on_main_thread');
var __emscripten_thread_free_data = makeInvalidEarlyAccess('__emscripten_thread_free_data');
var __emscripten_thread_exit = makeInvalidEarlyAccess('__emscripten_thread_exit');
var __emscripten_timeout = makeInvalidEarlyAccess('__emscripten_timeout');
var __emscripten_check_mailbox = makeInvalidEarlyAccess('__emscripten_check_mailbox');
var _setThrew = makeInvalidEarlyAccess('_setThrew');
var __emscripten_tempret_set = makeInvalidEarlyAccess('__emscripten_tempret_set');
var _emscripten_stack_init = makeInvalidEarlyAccess('_emscripten_stack_init');
var _emscripten_stack_set_limits = makeInvalidEarlyAccess('_emscripten_stack_set_limits');
var _emscripten_stack_get_free = makeInvalidEarlyAccess('_emscripten_stack_get_free');
var __emscripten_stack_restore = makeInvalidEarlyAccess('__emscripten_stack_restore');
var __emscripten_stack_alloc = makeInvalidEarlyAccess('__emscripten_stack_alloc');
var _emscripten_stack_get_current = makeInvalidEarlyAccess('_emscripten_stack_get_current');
var ___cxa_increment_exception_refcount = makeInvalidEarlyAccess('___cxa_increment_exception_refcount');
var ___cxa_decrement_exception_refcount = makeInvalidEarlyAccess('___cxa_decrement_exception_refcount');
var ___get_exception_message = makeInvalidEarlyAccess('___get_exception_message');
var ___cxa_can_catch = makeInvalidEarlyAccess('___cxa_can_catch');
var ___cxa_get_exception_ptr = makeInvalidEarlyAccess('___cxa_get_exception_ptr');
var ___set_stack_limits = Module['___set_stack_limits'] = makeInvalidEarlyAccess('___set_stack_limits');
var __indirect_function_table = makeInvalidEarlyAccess('__indirect_function_table');
var wasmTable = makeInvalidEarlyAccess('wasmTable');

function assignWasmExports(wasmExports) {
  assert(typeof wasmExports['free'] != 'undefined', 'missing Wasm export: free');
  assert(typeof wasmExports['malloc'] != 'undefined', 'missing Wasm export: malloc');
  assert(typeof wasmExports['strerror'] != 'undefined', 'missing Wasm export: strerror');
  assert(typeof wasmExports['fflush'] != 'undefined', 'missing Wasm export: fflush');
  assert(typeof wasmExports['nvim_main'] != 'undefined', 'missing Wasm export: nvim_main');
  assert(typeof wasmExports['ntohs'] != 'undefined', 'missing Wasm export: ntohs');
  assert(typeof wasmExports['uv_browser_get_shared_state_ptr'] != 'undefined', 'missing Wasm export: uv_browser_get_shared_state_ptr');
  assert(typeof wasmExports['uv_browser_set_readable'] != 'undefined', 'missing Wasm export: uv_browser_set_readable');
  assert(typeof wasmExports['uv_browser_set_writable'] != 'undefined', 'missing Wasm export: uv_browser_set_writable');
  assert(typeof wasmExports['htons'] != 'undefined', 'missing Wasm export: htons');
  assert(typeof wasmExports['pthread_self'] != 'undefined', 'missing Wasm export: pthread_self');
  assert(typeof wasmExports['htonl'] != 'undefined', 'missing Wasm export: htonl');
  assert(typeof wasmExports['_emscripten_tls_init'] != 'undefined', 'missing Wasm export: _emscripten_tls_init');
  assert(typeof wasmExports['_emscripten_thread_init'] != 'undefined', 'missing Wasm export: _emscripten_thread_init');
  assert(typeof wasmExports['__set_thread_state'] != 'undefined', 'missing Wasm export: __set_thread_state');
  assert(typeof wasmExports['_emscripten_thread_crashed'] != 'undefined', 'missing Wasm export: _emscripten_thread_crashed');
  assert(typeof wasmExports['emscripten_stack_get_end'] != 'undefined', 'missing Wasm export: emscripten_stack_get_end');
  assert(typeof wasmExports['emscripten_stack_get_base'] != 'undefined', 'missing Wasm export: emscripten_stack_get_base');
  assert(typeof wasmExports['_emscripten_run_js_on_main_thread_done'] != 'undefined', 'missing Wasm export: _emscripten_run_js_on_main_thread_done');
  assert(typeof wasmExports['_emscripten_run_js_on_main_thread'] != 'undefined', 'missing Wasm export: _emscripten_run_js_on_main_thread');
  assert(typeof wasmExports['_emscripten_thread_free_data'] != 'undefined', 'missing Wasm export: _emscripten_thread_free_data');
  assert(typeof wasmExports['_emscripten_thread_exit'] != 'undefined', 'missing Wasm export: _emscripten_thread_exit');
  assert(typeof wasmExports['_emscripten_timeout'] != 'undefined', 'missing Wasm export: _emscripten_timeout');
  assert(typeof wasmExports['_emscripten_check_mailbox'] != 'undefined', 'missing Wasm export: _emscripten_check_mailbox');
  assert(typeof wasmExports['setThrew'] != 'undefined', 'missing Wasm export: setThrew');
  assert(typeof wasmExports['_emscripten_tempret_set'] != 'undefined', 'missing Wasm export: _emscripten_tempret_set');
  assert(typeof wasmExports['emscripten_stack_init'] != 'undefined', 'missing Wasm export: emscripten_stack_init');
  assert(typeof wasmExports['emscripten_stack_set_limits'] != 'undefined', 'missing Wasm export: emscripten_stack_set_limits');
  assert(typeof wasmExports['emscripten_stack_get_free'] != 'undefined', 'missing Wasm export: emscripten_stack_get_free');
  assert(typeof wasmExports['_emscripten_stack_restore'] != 'undefined', 'missing Wasm export: _emscripten_stack_restore');
  assert(typeof wasmExports['_emscripten_stack_alloc'] != 'undefined', 'missing Wasm export: _emscripten_stack_alloc');
  assert(typeof wasmExports['emscripten_stack_get_current'] != 'undefined', 'missing Wasm export: emscripten_stack_get_current');
  assert(typeof wasmExports['__cxa_increment_exception_refcount'] != 'undefined', 'missing Wasm export: __cxa_increment_exception_refcount');
  assert(typeof wasmExports['__cxa_decrement_exception_refcount'] != 'undefined', 'missing Wasm export: __cxa_decrement_exception_refcount');
  assert(typeof wasmExports['__get_exception_message'] != 'undefined', 'missing Wasm export: __get_exception_message');
  assert(typeof wasmExports['__cxa_can_catch'] != 'undefined', 'missing Wasm export: __cxa_can_catch');
  assert(typeof wasmExports['__cxa_get_exception_ptr'] != 'undefined', 'missing Wasm export: __cxa_get_exception_ptr');
  assert(typeof wasmExports['__set_stack_limits'] != 'undefined', 'missing Wasm export: __set_stack_limits');
  assert(typeof wasmExports['__indirect_function_table'] != 'undefined', 'missing Wasm export: __indirect_function_table');
  _free = Module['_free'] = createExportWrapper('free', wasmExports['free'], 1);
  _malloc = Module['_malloc'] = createExportWrapper('malloc', wasmExports['malloc'], 1);
  _strerror = createExportWrapper('strerror', wasmExports['strerror'], 1);
  _fflush = createExportWrapper('fflush', wasmExports['fflush'], 1);
  _nvim_main = Module['_nvim_main'] = createExportWrapper('nvim_main', wasmExports['nvim_main'], 2);
  _ntohs = createExportWrapper('ntohs', wasmExports['ntohs'], 1);
  _uv_browser_get_shared_state_ptr = Module['_uv_browser_get_shared_state_ptr'] = createExportWrapper('uv_browser_get_shared_state_ptr', wasmExports['uv_browser_get_shared_state_ptr'], 0);
  _uv_browser_set_readable = Module['_uv_browser_set_readable'] = createExportWrapper('uv_browser_set_readable', wasmExports['uv_browser_set_readable'], 2);
  _uv_browser_set_writable = Module['_uv_browser_set_writable'] = createExportWrapper('uv_browser_set_writable', wasmExports['uv_browser_set_writable'], 2);
  _htons = createExportWrapper('htons', wasmExports['htons'], 1);
  _pthread_self = wasmExports['pthread_self'];
  _htonl = createExportWrapper('htonl', wasmExports['htonl'], 1);
  __emscripten_tls_init = createExportWrapper('_emscripten_tls_init', wasmExports['_emscripten_tls_init'], 0);
  __emscripten_thread_init = createExportWrapper('_emscripten_thread_init', wasmExports['_emscripten_thread_init'], 6);
  ___set_thread_state = createExportWrapper('__set_thread_state', wasmExports['__set_thread_state'], 4);
  __emscripten_thread_crashed = createExportWrapper('_emscripten_thread_crashed', wasmExports['_emscripten_thread_crashed'], 0);
  _emscripten_stack_get_end = wasmExports['emscripten_stack_get_end'];
  _emscripten_stack_get_base = wasmExports['emscripten_stack_get_base'];
  __emscripten_run_js_on_main_thread_done = createExportWrapper('_emscripten_run_js_on_main_thread_done', wasmExports['_emscripten_run_js_on_main_thread_done'], 3);
  __emscripten_run_js_on_main_thread = createExportWrapper('_emscripten_run_js_on_main_thread', wasmExports['_emscripten_run_js_on_main_thread'], 5);
  __emscripten_thread_free_data = createExportWrapper('_emscripten_thread_free_data', wasmExports['_emscripten_thread_free_data'], 1);
  __emscripten_thread_exit = createExportWrapper('_emscripten_thread_exit', wasmExports['_emscripten_thread_exit'], 1);
  __emscripten_timeout = createExportWrapper('_emscripten_timeout', wasmExports['_emscripten_timeout'], 2);
  __emscripten_check_mailbox = createExportWrapper('_emscripten_check_mailbox', wasmExports['_emscripten_check_mailbox'], 0);
  _setThrew = createExportWrapper('setThrew', wasmExports['setThrew'], 2);
  __emscripten_tempret_set = createExportWrapper('_emscripten_tempret_set', wasmExports['_emscripten_tempret_set'], 1);
  _emscripten_stack_init = wasmExports['emscripten_stack_init'];
  _emscripten_stack_set_limits = wasmExports['emscripten_stack_set_limits'];
  _emscripten_stack_get_free = wasmExports['emscripten_stack_get_free'];
  __emscripten_stack_restore = wasmExports['_emscripten_stack_restore'];
  __emscripten_stack_alloc = wasmExports['_emscripten_stack_alloc'];
  _emscripten_stack_get_current = wasmExports['emscripten_stack_get_current'];
  ___cxa_increment_exception_refcount = createExportWrapper('__cxa_increment_exception_refcount', wasmExports['__cxa_increment_exception_refcount'], 1);
  ___cxa_decrement_exception_refcount = createExportWrapper('__cxa_decrement_exception_refcount', wasmExports['__cxa_decrement_exception_refcount'], 1);
  ___get_exception_message = createExportWrapper('__get_exception_message', wasmExports['__get_exception_message'], 3);
  ___cxa_can_catch = createExportWrapper('__cxa_can_catch', wasmExports['__cxa_can_catch'], 3);
  ___cxa_get_exception_ptr = createExportWrapper('__cxa_get_exception_ptr', wasmExports['__cxa_get_exception_ptr'], 1);
  ___set_stack_limits = Module['___set_stack_limits'] = createExportWrapper('__set_stack_limits', wasmExports['__set_stack_limits'], 2);
  __indirect_function_table = wasmTable = wasmExports['__indirect_function_table'];
}

  var wasmImports;
  function assignWasmImports() {
    wasmImports = {
    /** @export */
    __assert_fail: ___assert_fail,
    /** @export */
    __call_sighandler: ___call_sighandler,
    /** @export */
    __cxa_begin_catch: ___cxa_begin_catch,
    /** @export */
    __cxa_find_matching_catch_2: ___cxa_find_matching_catch_2,
    /** @export */
    __cxa_find_matching_catch_3: ___cxa_find_matching_catch_3,
    /** @export */
    __handle_stack_overflow: ___handle_stack_overflow,
    /** @export */
    __pthread_create_js: ___pthread_create_js,
    /** @export */
    __resumeException: ___resumeException,
    /** @export */
    __syscall_accept4: ___syscall_accept4,
    /** @export */
    __syscall_bind: ___syscall_bind,
    /** @export */
    __syscall_chdir: ___syscall_chdir,
    /** @export */
    __syscall_chmod: ___syscall_chmod,
    /** @export */
    __syscall_connect: ___syscall_connect,
    /** @export */
    __syscall_dup: ___syscall_dup,
    /** @export */
    __syscall_dup3: ___syscall_dup3,
    /** @export */
    __syscall_faccessat: ___syscall_faccessat,
    /** @export */
    __syscall_fchmod: ___syscall_fchmod,
    /** @export */
    __syscall_fchown32: ___syscall_fchown32,
    /** @export */
    __syscall_fchownat: ___syscall_fchownat,
    /** @export */
    __syscall_fcntl64: ___syscall_fcntl64,
    /** @export */
    __syscall_fstat64: ___syscall_fstat64,
    /** @export */
    __syscall_ftruncate64: ___syscall_ftruncate64,
    /** @export */
    __syscall_getcwd: ___syscall_getcwd,
    /** @export */
    __syscall_getdents64: ___syscall_getdents64,
    /** @export */
    __syscall_getpeername: ___syscall_getpeername,
    /** @export */
    __syscall_getsockname: ___syscall_getsockname,
    /** @export */
    __syscall_getsockopt: ___syscall_getsockopt,
    /** @export */
    __syscall_ioctl: ___syscall_ioctl,
    /** @export */
    __syscall_listen: ___syscall_listen,
    /** @export */
    __syscall_lstat64: ___syscall_lstat64,
    /** @export */
    __syscall_mkdirat: ___syscall_mkdirat,
    /** @export */
    __syscall_newfstatat: ___syscall_newfstatat,
    /** @export */
    __syscall_openat: ___syscall_openat,
    /** @export */
    __syscall_pipe2: ___syscall_pipe2,
    /** @export */
    __syscall_poll: ___syscall_poll,
    /** @export */
    __syscall_poll_nonblocking: ___syscall_poll_nonblocking,
    /** @export */
    __syscall_readlinkat: ___syscall_readlinkat,
    /** @export */
    __syscall_recvfrom: ___syscall_recvfrom,
    /** @export */
    __syscall_recvmsg: ___syscall_recvmsg,
    /** @export */
    __syscall_renameat: ___syscall_renameat,
    /** @export */
    __syscall_rmdir: ___syscall_rmdir,
    /** @export */
    __syscall_sendmsg: ___syscall_sendmsg,
    /** @export */
    __syscall_sendto: ___syscall_sendto,
    /** @export */
    __syscall_setsockopt: ___syscall_setsockopt,
    /** @export */
    __syscall_shutdown: ___syscall_shutdown,
    /** @export */
    __syscall_socket: ___syscall_socket,
    /** @export */
    __syscall_stat64: ___syscall_stat64,
    /** @export */
    __syscall_statfs64: ___syscall_statfs64,
    /** @export */
    __syscall_symlinkat: ___syscall_symlinkat,
    /** @export */
    __syscall_umask: ___syscall_umask,
    /** @export */
    __syscall_unlinkat: ___syscall_unlinkat,
    /** @export */
    __syscall_utimensat: ___syscall_utimensat,
    /** @export */
    _abort_js: __abort_js,
    /** @export */
    _emscripten_init_main_thread_js: __emscripten_init_main_thread_js,
    /** @export */
    _emscripten_notify_mailbox_postmessage: __emscripten_notify_mailbox_postmessage,
    /** @export */
    _emscripten_receive_on_main_thread_js: __emscripten_receive_on_main_thread_js,
    /** @export */
    _emscripten_runtime_keepalive_clear: __emscripten_runtime_keepalive_clear,
    /** @export */
    _emscripten_system: __emscripten_system,
    /** @export */
    _emscripten_thread_cleanup: __emscripten_thread_cleanup,
    /** @export */
    _emscripten_thread_mailbox_await: __emscripten_thread_mailbox_await,
    /** @export */
    _emscripten_thread_set_strongref: __emscripten_thread_set_strongref,
    /** @export */
    _emscripten_throw_longjmp: __emscripten_throw_longjmp,
    /** @export */
    _gmtime_js: __gmtime_js,
    /** @export */
    _localtime_js: __localtime_js,
    /** @export */
    _mktime_js: __mktime_js,
    /** @export */
    _setitimer_js: __setitimer_js,
    /** @export */
    _tzset_js: __tzset_js,
    /** @export */
    clock_time_get: _clock_time_get,
    /** @export */
    emscripten_check_blocking_allowed: _emscripten_check_blocking_allowed,
    /** @export */
    emscripten_date_now: _emscripten_date_now,
    /** @export */
    emscripten_err: _emscripten_err,
    /** @export */
    emscripten_exit_with_live_runtime: _emscripten_exit_with_live_runtime,
    /** @export */
    emscripten_get_heap_max: _emscripten_get_heap_max,
    /** @export */
    emscripten_get_now: _emscripten_get_now,
    /** @export */
    emscripten_num_logical_cores: _emscripten_num_logical_cores,
    /** @export */
    emscripten_resize_heap: _emscripten_resize_heap,
    /** @export */
    emscripten_unwind_to_js_event_loop: _emscripten_unwind_to_js_event_loop,
    /** @export */
    environ_get: _environ_get,
    /** @export */
    environ_sizes_get: _environ_sizes_get,
    /** @export */
    exit: _exit,
    /** @export */
    fd_close: _fd_close,
    /** @export */
    fd_fdstat_get: _fd_fdstat_get,
    /** @export */
    fd_pread: _fd_pread,
    /** @export */
    fd_pwrite: _fd_pwrite,
    /** @export */
    fd_read: _fd_read,
    /** @export */
    fd_seek: _fd_seek,
    /** @export */
    fd_sync: _fd_sync,
    /** @export */
    fd_write: _fd_write,
    /** @export */
    getaddrinfo: _getaddrinfo,
    /** @export */
    getnameinfo: _getnameinfo,
    /** @export */
    getprotobyname: _getprotobyname,
    /** @export */
    getprotobynumber: _getprotobynumber,
    /** @export */
    invoke_ii,
    /** @export */
    invoke_iii,
    /** @export */
    invoke_vii,
    /** @export */
    invoke_viii,
    /** @export */
    invoke_viiii,
    /** @export */
    memory: wasmMemory,
    /** @export */
    proc_exit: _proc_exit
  };
  }

function invoke_vii(index,a1,a2) {
  var sp = stackSave();
  try {
    getWasmTableEntry(index)(a1,a2);
  } catch(e) {
    stackRestore(sp);
    if (!(e instanceof EmscriptenEH)) throw e;
    _setThrew(1, 0);
  }
}

function invoke_ii(index,a1) {
  var sp = stackSave();
  try {
    return getWasmTableEntry(index)(a1);
  } catch(e) {
    stackRestore(sp);
    if (!(e instanceof EmscriptenEH)) throw e;
    _setThrew(1, 0);
  }
}

function invoke_iii(index,a1,a2) {
  var sp = stackSave();
  try {
    return getWasmTableEntry(index)(a1,a2);
  } catch(e) {
    stackRestore(sp);
    if (!(e instanceof EmscriptenEH)) throw e;
    _setThrew(1, 0);
  }
}

function invoke_viiii(index,a1,a2,a3,a4) {
  var sp = stackSave();
  try {
    getWasmTableEntry(index)(a1,a2,a3,a4);
  } catch(e) {
    stackRestore(sp);
    if (!(e instanceof EmscriptenEH)) throw e;
    _setThrew(1, 0);
  }
}

function invoke_viii(index,a1,a2,a3) {
  var sp = stackSave();
  try {
    getWasmTableEntry(index)(a1,a2,a3);
  } catch(e) {
    stackRestore(sp);
    if (!(e instanceof EmscriptenEH)) throw e;
    _setThrew(1, 0);
  }
}


// include: postamble.js
// === Auto-generated postamble setup entry stuff ===

var calledRun;

function stackCheckInit() {
  // This is normally called automatically during __wasm_call_ctors but need to
  // get these values before even running any of the ctors so we call it redundantly
  // here.
  // See $establishStackSpace for the equivalent code that runs on a thread
  assert(!ENVIRONMENT_IS_PTHREAD);
  _emscripten_stack_init();
  // TODO(sbc): Move writeStackCookie to native to to avoid this.
  writeStackCookie();
}

async function run() {
  assert(!calledRun);
  calledRun = true;

  if ((ENVIRONMENT_IS_PTHREAD)) {
    initRuntime();
    return;
  }

  stackCheckInit();

  preRun();

  if (runDependencies) {
    await resolveRunDependencies();
  }

  var setStatus = Module['setStatus'];
  if (setStatus) {
    setStatus('Running...');
    // Yield to the event loop to allow the browser to paint "Running..."
    await new Promise((resolve) => setTimeout(resolve, 1));
    // Then we want to clear the status text, but only after the rest of this function runs.
    setTimeout(setStatus, 1, '');
  }

  if (ABORT) return;

  initRuntime();

  Module['onRuntimeInitialized']?.();
  consumedModuleProp('onRuntimeInitialized');

  assert(!Module['_main'], 'compiled without a main, but one is present. if you added it from JS, use Module["onRuntimeInitialized"]');

  postRun();
}

function checkUnflushedContent() {
  // Compiler settings do not allow exiting the runtime, so flushing
  // the streams is not possible. but in ASSERTIONS mode we check
  // if there was something to flush, and if so tell the user they
  // should request that the runtime be exitable.
  // Normally we would not even include flush() at all, but in ASSERTIONS
  // builds we do so just for this check, and here we see if there is any
  // content to flush, that is, we check if there would have been
  // something a non-ASSERTIONS build would have not seen.
  // How we flush the streams depends on whether we are in SYSCALLS_REQUIRE_FILESYSTEM=0
  // mode (which has its own special function for this; otherwise, all
  // the code is inside libc)
  var oldOut = out;
  var oldErr = err;
  var has = false;
  out = err = (x) => {
    has = true;
  }
  try { // it doesn't matter if it fails
    _fflush(0);
    // also flush in the JS FS layer
    for (var name of ['stdout', 'stderr']) {
      var info = FS.analyzePath('/dev/' + name);
      if (!info) return;
      var stream = info.object;
      var rdev = stream.rdev;
      var tty = TTY.ttys[rdev];
      if (tty?.output?.length) {
        has = true;
      }
    }
  } catch(e) {}
  out = oldOut;
  err = oldErr;
  if (has) {
    warnOnce('stdio streams had content in them that was not flushed. you should set EXIT_RUNTIME to 1 (see the Emscripten FAQ), or make sure to emit a newline when you printf etc.');
  }
}

var wasmExports;

if ((!(ENVIRONMENT_IS_PTHREAD))) {
// Call createWasm on startup if we are the main thread.
// Worker threads call this once they receive the module via postMessage

// In modularize mode the generated code is within a factory function so we
// can use await here (since it's not top-level-await).
wasmExports = await createWasm();
await run();

}

// end include: postamble.js

// include: postamble_modularize.js
// In MODULARIZE mode we wrap the generated code in a factory function
// and return either the Module itself, or a promise of the module.

// Assertion for attempting to access module properties on the incoming
// moduleArg.  In the past we used this object as the prototype of the module
// and assigned properties to it, but now we return a distinct object.  This
// keeps the instance private until it is ready (i.e the promise has been
// resolved).
for (const prop of Object.keys(Module)) {
  if (!(prop in moduleArg)) {
    Object.defineProperty(moduleArg, prop, {
      configurable: true,
      get() {
        abort(`Access to module property ('${prop}') is no longer possible via the module constructor argument; Instead, use the result of the module constructor.`)
      }
    });
  }
}
// end include: postamble_modularize.js



    return Module;
  };
})();

// Export using a UMD style export, or ES6 exports if selected
if (typeof exports === 'object' && typeof module === 'object') {
  module.exports = createNvim;
  // This default export looks redundant, but it allows TS to import this
  // commonjs style module.
  module.exports.default = createNvim;
} else if (typeof define === 'function' && define['amd'])
  define([], () => createNvim);

// Create code for detecting if we are running in a pthread.
// Normally this detection is done when the module is itself run but
// when running in MODULARIZE mode we need use this to know if we should
// run the module constructor on startup (true only for pthreads).
var isPthread = globalThis.name?.startsWith('em-pthread');
// In order to support both web and node we also need to detect node here.
var isNode = globalThis.process?.versions?.node && globalThis.process?.type != 'renderer';
if (isNode) isPthread = require('node:worker_threads').workerData === 'em-pthread'

isPthread && createNvim();

