# one-shot mcp stdio client: ghmcp BINARY TOOL [JSON_ARGS] [-- extra ghidra-mcp flags]
import json
import os
import queue
import subprocess
import sys
import threading
import time

TIMEOUT = 900

binary, tool = sys.argv[1], sys.argv[2]
args = json.loads(sys.argv[3]) if len(sys.argv) > 3 and sys.argv[3] != "--" else {}
extra = sys.argv[sys.argv.index("--") + 1 :] if "--" in sys.argv else []
cmd = [
    os.environ.get("GHMCP_EXE", "ghidra-mcp"),
    "--headless", "--binary", binary, "--ready-timeout", "600", *extra,
]

p = subprocess.Popen(
    cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
)
lines = queue.Queue()
errs = []


def pump_stdout():
    for line in p.stdout:
        lines.put(line.rstrip())
    lines.put(None)


def pump_stderr():
    for line in p.stderr:
        errs.append(line.rstrip())


threading.Thread(target=pump_stdout, daemon=True).start()
threading.Thread(target=pump_stderr, daemon=True).start()


def die(msg):
    p.kill()
    tail = "\n".join(errs[-20:])
    sys.exit(msg + (f"\n{tail}" if tail else ""))


def send(obj):
    try:
        p.stdin.write(json.dumps(obj) + "\n")
        p.stdin.flush()
    except BrokenPipeError:
        die("ghidra-mcp exited early")


def recv(i):
    t0 = time.time()
    while time.time() - t0 < TIMEOUT:
        try:
            line = lines.get(timeout=1)
        except queue.Empty:
            continue
        if line is None:
            die("ghidra-mcp exited before replying")
        try:
            msg = json.loads(line)
        except ValueError:
            continue
        if msg.get("id") == i:
            return msg
    die("timed out waiting for ghidra-mcp")


send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
    "protocolVersion": "2024-11-05", "capabilities": {},
    "clientInfo": {"name": "ghmcp", "version": "0"},
}})
recv(1)
send({"jsonrpc": "2.0", "method": "notifications/initialized"})
send({"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": tool, "arguments": args}})
r = recv(2)

p.terminate()
res = r.get("result")
if not res:
    sys.exit(json.dumps(r)[:2000])
print(res["content"][0]["text"] if res.get("content") else json.dumps(res))
