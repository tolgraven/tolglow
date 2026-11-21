#!/usr/bin/env python3
"""Owned PTY transport. argv comes only from the trusted JVM container service.

stdin/stdout are bounded JSON lines; child bytes are base64, never parsed as code.
The child is the Docker CLI; the JVM separately owns/removes its named container.
"""
import base64
import errno
import fcntl
import json
import os
import pty
import select
import signal
import struct
import sys
import termios


def emit(value):
    sys.stdout.write(json.dumps(value, separators=(",", ":")) + "\n")
    sys.stdout.flush()


def main():
    options = json.loads(sys.argv[1])
    argv = options["argv"]
    if not isinstance(argv, list) or not argv or not all(isinstance(x, str) for x in argv):
        raise ValueError("argv must contain strings")
    child, master = pty.fork()
    if child == 0:
        os.execvp(argv[0], argv)
    pending = b""
    closed = False
    try:
        fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", options["rows"], options["cols"], 0, 0))
        while not closed:
            readable, _, _ = select.select([master, sys.stdin.fileno()], [], [])
            if master in readable:
                try:
                    data = os.read(master, 8192)
                except OSError as error:
                    if error.errno != errno.EIO:
                        raise
                    data = b""
                if not data:
                    break
                emit({"kind": "output", "data": base64.b64encode(data).decode("ascii")})
            if sys.stdin.fileno() in readable:
                data = os.read(sys.stdin.fileno(), 8192)
                if not data:
                    break
                pending += data
                if len(pending) > 131072:
                    raise ValueError("PTY command exceeds the line limit")
                while b"\n" in pending:
                    line, pending = pending.split(b"\n", 1)
                    request = json.loads(line)
                    if request["op"] == "input":
                        value = base64.b64decode(request["data"], validate=True)
                        if len(value) > 65536:
                            raise ValueError("Input exceeds 64 KiB")
                        while value:
                            value = value[os.write(master, value):]
                    elif request["op"] == "resize":
                        rows, cols = request["rows"], request["cols"]
                        if not (2 <= rows <= 200 and 2 <= cols <= 500):
                            raise ValueError("Invalid PTY dimensions")
                        fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
                        os.kill(child, signal.SIGWINCH)
                    elif request["op"] == "close":
                        closed = True
                        break
                    else:
                        raise ValueError("Unknown PTY command")
    finally:
        os.close(master)
        if closed:
            try:
                os.kill(child, signal.SIGHUP)
            except ProcessLookupError:
                pass
        _, status = os.waitpid(child, 0)
        emit({"kind": "exit", "exit-code": os.waitstatus_to_exitcode(status)})


if __name__ == "__main__":
    main()
