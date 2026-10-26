#!/usr/bin/env python3
"""Read a Playwright bridge token from a consistent temporary LevelDB copy.

Usage: read-token.py <leveldb-dir> <extension-id>
"""
import os
import re
import shutil
import sys
import tempfile

import plyvel

TOKEN_RE = re.compile(rb"[A-Za-z0-9_-]{43}\Z")


def copy_database(source, destination):
    """Copy LevelDB files except lock/log diagnostics; reject changing copies."""
    names = sorted(
        name for name in os.listdir(source)
        if name not in {"LOCK", "LOG", "LOG.old"}
    )
    if not names or "CURRENT" not in names:
        return False
    before = {name: os.stat(os.path.join(source, name)) for name in names}
    for name in names:
        path = os.path.join(source, name)
        target = os.path.join(destination, name)
        if os.path.isdir(path):
            shutil.copytree(path, target)
        else:
            shutil.copy2(path, target)
    after_names = sorted(
        name for name in os.listdir(source)
        if name not in {"LOCK", "LOG", "LOG.old"}
    )
    if names != after_names:
        return False
    for name in names:
        after = os.stat(os.path.join(source, name))
        if (before[name].st_size, before[name].st_mtime_ns) != (after.st_size, after.st_mtime_ns):
            return False
    return True


def read_token(dbdir, extension_id):
    key = b"_chrome-extension://" + extension_id + b"\x00\x01auth-token"
    for _ in range(3):
        if not os.path.isdir(dbdir):
            return None
        try:
            with tempfile.TemporaryDirectory(prefix="pw-token-") as tmp:
                if not copy_database(dbdir, tmp):
                    continue
                db = plyvel.DB(
                    tmp, create_if_missing=False, error_if_exists=False, paranoid_checks=True
                )
                try:
                    value = db.get(key, verify_checksums=True)
                finally:
                    db.close()
        except (OSError, plyvel.Error, ValueError):
            continue
        if value is None or len(value) < 2 or value[:1] not in (b"\x00", b"\x01"):
            return None
        token = value[1:]
        return token if TOKEN_RE.fullmatch(token) else None
    return None


def main():
    if len(sys.argv) != 3:
        return 1
    token = read_token(sys.argv[1], sys.argv[2].encode())
    if token is None:
        return 1
    sys.stdout.buffer.write(token + b"\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
