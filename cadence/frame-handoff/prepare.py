from pathlib import Path

def swap(text, old, new):
    assert text.count(old) == 1, old
    return text.replace(old, new)

target = Path(__file__).parent
harness = Path("dev/preview")
for name in ['desktop-session.mjs', 'room-cadence.mjs', 'room-pass-analysis.mjs']:
    (target/name).write_bytes((harness/name).read_bytes())

analysis_file = target/'room-pass-analysis.mjs'
analysis = analysis_file.read_text()
analysis = swap(analysis, 'readyForFrameMs: t.readyForFrameMs,',
    'readyForFrameMs: t.readyForFrameMs, fencePolls: t.fencePolls, bitmapTransferMs: t.bitmapTransferMs,')
analysis_file.write_text(analysis)

worker = Path("js/preview/worker.js").read_text()
worker = swap(worker, '  let yields = 0;', '''  let yields = 0, polls = 0;
  const densePolling = self.roomPresentationMode() === 'eager-fence';''')
worker = swap(worker, '      const status = gl.clientWaitSync(fence, 0, 0);',
    '      polls++;\n      const status = gl.clientWaitSync(fence, 0, 0);')
worker = swap(worker, '      if (!engine.softwareRenderer && globalThis.scheduler?.yield && yields++ < 16 &&\n          performance.now() - started < Math.min(4, 500 / engine.targetHz))',
    '      if (!engine.softwareRenderer && globalThis.scheduler?.yield && yields++ < (densePolling ? 64 : 16) &&\n          performance.now() - started < (densePolling ? 850 / engine.targetHz : Math.min(4, 500 / engine.targetHz)))')
worker = swap(worker, '  } finally {gl.deleteSync(fence);}', '  } finally {engine.lastFencePolls = polls; gl.deleteSync(fence);}')
worker = swap(worker, '  rendering = true;\n  try {', '  rendering = true;\n  let bitmap, bitmapDuration;\n  try {')
worker = swap(worker, '    if (!await completeGpuFrame(engine.renderer.getContext())) return;', '''    if (self.roomPresentationMode() === 'early-bitmap') {
      const bitmapStarted = performance.now();
      bitmap = canvas.transferToImageBitmap();
      bitmapDuration = performance.now() - bitmapStarted;
    }
    const fenceStarted = performance.now();
    if (!await completeGpuFrame(engine.renderer.getContext())) return;''')
worker = swap(worker, '    engine.lastGPUWait = performance.now() - submitted;\n    const bitmapStarted = performance.now(), bitmap = canvas.transferToImageBitmap(); pendingFrame = ++frameId;\n    const completed = performance.now();\n    engine.lastBitmapTransfer = completed - bitmapStarted; engine.lastWorkerFrame = completed - started;',
    '''    engine.lastGPUWait = performance.now() - fenceStarted;
    if (!bitmap) {
      const bitmapStarted = performance.now();
      bitmap = canvas.transferToImageBitmap();
      bitmapDuration = performance.now() - bitmapStarted;
    }
    pendingFrame = ++frameId;
    const completed = performance.now();
    engine.lastBitmapTransfer = bitmapDuration; engine.lastWorkerFrame = completed - started;''')
worker = swap(worker, 'bitmapTransferMs: engine.lastBitmapTransfer}}, [bitmap]);',
    'bitmapTransferMs: engine.lastBitmapTransfer, fencePolls: engine.lastFencePolls}}, [bitmap]);\n    bitmap = null;')
worker = swap(worker, '  finally {\n    rendering = false;', '  finally {\n    bitmap?.close();\n    rendering = false;')
worker = worker.replace('    const submitted = performance.now();\n', '')
for name in ['display-clock.js', 'scene-renderer.js']:
    worker = worker.replace(f"'./{name}'", f"'../../js/preview/{name}'")
(target/'preview-worker.mjs').write_text(worker)

instrument = (harness/'room-pass-worker.mjs').read_text()
instrument = swap(instrument, "import '../../js/preview/worker.js';", "import './preview-worker.mjs';")
instrument = swap(instrument, "const modes = new Set(['full',", "const modes = new Set(['full', 'early-bitmap', 'eager-fence',")
instrument = swap(instrument, 'let mode = \'full\', probe;', "let mode = 'full', probe;\nself.roomPresentationMode = () => mode;")
(target/'room-pass-worker.mjs').write_text(instrument)

main = (harness/'measure-room.mjs').read_text()
main = swap(main, "const allowedPasses = ['full',", "const allowedPasses = ['full', 'early-bitmap', 'eager-fence',")
main = swap(main, 'const controlledComparison = loopComparison || timerComparison || compositeComparison || attributionComparison;', '''const presentationComparison = frozenAuto && passes.join(',') === 'full,early-bitmap,eager-fence,full';
const controlledComparison = loopComparison || timerComparison || compositeComparison || attributionComparison || presentationComparison;''')
main = swap(main, 'const requestTimerExtension = !compositeComparison && !attributionComparison;',
    'const requestTimerExtension = !compositeComparison && !attributionComparison && !presentationComparison;')
main = swap(main, '(frozenAuto && !attributionComparison && passes.some(', '(frozenAuto && !attributionComparison && !presentationComparison && passes.some(')
main = swap(main, "    : 'graphics pass attribution'};", "    : presentationComparison ? 'bitmap transfer order and cooperative GPU fence polling with all graphics stages retained'\n    : 'graphics pass attribution'};")
main = swap(main, 'const plumeScene = process.env.ROOM_PLUME_SCENE', '''if (presentationComparison && (scene !== 'dense' || minimumWarmupMs < 8000))
  throw new Error('The presentation comparison requires the moving scene and ≥8 s warmup.');
const plumeScene = process.env.ROOM_PLUME_SCENE''')
main = swap(main, "title;}, attributionComparison ? 'Tolglow graphics stage diagnostic'", "title;}, presentationComparison ? 'Tolglow frame handoff comparison' : attributionComparison ? 'Tolglow graphics stage diagnostic'")
(target/'measure-room.mjs').write_text(main)
