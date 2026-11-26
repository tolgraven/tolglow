import {execFile} from 'node:child_process';
import {promisify} from 'node:util';

const execute = promisify(execFile);
const unknown = reason => ({status: 'unknown', reason});

// IORegistry's plain-text root properties contain private session fields.
// Extract only these booleans; never return or log the source dictionaries.
export function parseDesktopSession(text) {
  const root = [...text.matchAll(/^[\t |]*"IOConsoleLocked"\s*=\s*(\S+)\s*$/gm)];
  if (root.length !== 1 || !['Yes', 'No'].includes(root[0][1])) return unknown('root-lock-unavailable');
  if (root[0][1] === 'Yes') return {status: 'locked', reason: 'root-lock'};
  const lines = [...text.matchAll(/^[\t |]*"IOConsoleUsers"\s*=\s*(\(.*\))\s*$/gm)];
  if (lines.length !== 1) return unknown('console-records-unavailable');
  // Quoted values cannot create record boundaries or inject boolean keys.
  const tokens = lines[0][1].match(/"(?:\\.|[^"\\])*"|[{}()]|=|Yes\b|No\b|[^\s,{}()=]+/g) || [];
  const sessions = [];
  let current = null, depth = 0;
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    if (token === '{') {if (depth++ === 0) current = {};}
    else if (token === '}') {
      if (--depth < 0) return unknown('malformed-console-records');
      if (!depth) {sessions.push(current); current = null;}
    } else if (depth === 1 && ['"kCGSSessionOnConsoleKey"', '"CGSSessionScreenIsLocked"'].includes(token)) {
      if (Object.hasOwn(current, token) || tokens[i + 1] !== '=' || !['Yes', 'No'].includes(tokens[i + 2]))
        return unknown('ambiguous-session-flag');
      current[token] = tokens[i + 2];
    }
  }
  if (depth || !sessions.length || sessions.some(s => !Object.hasOwn(s, '"kCGSSessionOnConsoleKey"')))
    return unknown('malformed-console-records');
  if (sessions.some(s => s['"CGSSessionScreenIsLocked"'] === 'Yes')) return {status: 'locked', reason: 'session-lock'};
  if (sessions.filter(s => s['"kCGSSessionOnConsoleKey"'] === 'Yes').length !== 1)
    return unknown('ambiguous-console-ownership');
  // On an unlocked Mac the per-session lock key is normally absent. The
  // explicit root No, not that absence, supplies the negative evidence.
  return {status: 'unlocked', reason: 'root-unlocked-single-console'};
}

export async function readDesktopSession() {
  const started = performance.now(), checkedAt = new Date().toISOString();
  let state;
  if (process.platform !== 'darwin') state = unknown('unsupported-platform');
  else {
    try {
      const {stdout} = await execute('/usr/sbin/ioreg', ['-n', 'Root', '-d', '1', '-l'],
        {encoding: 'utf8', timeout: 750, maxBuffer: 1024 * 1024});
      state = parseDesktopSession(stdout);
    } catch {state = unknown('ioreg-read-failed');}
  }
  return {...state, checkedAt, readMs: performance.now() - started};
}

export function watchDesktopSession() {
  const checks = [];
  let signal;
  const failed = new Promise(resolve => {signal = resolve;});
  let timer, pending, stopped = false, failure;
  const inspect = async () => {
    if (checks.length >= 32) return failure;
    const result = checks.length === 31 ? {...unknown('monitor-check-limit'), checkedAt: new Date().toISOString(), readMs: 0}
      : await readDesktopSession();
    checks.push(result);
    if (result.status !== 'unlocked') {failure ||= result; signal(failure);}
    return result;
  };
  const tick = () => {
    pending = inspect().finally(() => {
      pending = null;
      if (!stopped && !failure) timer = setTimeout(tick, 1000);
    });
  };
  tick();
  return {checks, failed, get failure() {return failure;}, async stop() {
    stopped = true; clearTimeout(timer); await pending;
    return inspect();
  }};
}
