// Transferred frames carry show identity, not merely a render revision. An old
// show must never flash back onto the canvas after a restore/restart.
export const showIdentity = snapshot => JSON.stringify([snapshot?.['runtime-id'] ?? null, snapshot?.['show-id'] ?? null, snapshot?.epoch ?? null]);
export function presentFrame(message, identity, lastFrame, present) {
  try {
    if (message.frameId <= lastFrame || showIdentity({'runtime-id': message.runtimeId, 'show-id': message.showId, epoch: message.epoch}) !== identity) return false;
    present(message.bitmap); return true;
  } finally {message.bitmap.close();}
}
