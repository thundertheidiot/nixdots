#!/usr/bin/env python3
"""Manage WireGuard hub-and-spoke keys; plaintext secrets stay in memory and pipes."""

import argparse
import base64
import binascii
import fcntl
import ipaddress
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import sys
import tempfile


class Error(Exception):
    pass


def run(args, data=None):
    result = subprocess.run(args, input=data, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode:
        # External diagnostics can contain plaintext, especially from decrypt/QR commands.
        raise Error(f"{args[0]} {args[1]} failed (exit {result.returncode}); diagnostics suppressed")
    return result.stdout


def key(data):
    try:
        value = data.decode("ascii").strip()
        decoded = base64.b64decode(value, validate=True)
        if len(decoded) == 32 and base64.b64encode(decoded).decode("ascii") == value:
            return value
    except (UnicodeError, binascii.Error, ValueError):
        pass
    raise Error("Invalid WireGuard key (expected a base64-encoded 32-byte value)")


def pubkey(private):
    return key(run(["wg", "pubkey"], (private + "\n").encode("ascii")))


def read_json(path):
    try:
        return json.loads(path.read_bytes())
    except (ValueError, UnicodeError):
        raise Error(f"Invalid JSON: {path}") from None


def inventory(root):
    network = read_json(root / "peers.json")
    if not isinstance(network, dict) or set(network) != {"hub", "endpoint", "port", "dns", "peers"}:
        raise Error("Inventory must contain exactly hub, endpoint, port, dns and peers")
    peers = network["peers"]
    if not isinstance(peers, dict) or not peers:
        raise Error("Inventory must contain peers")
    if any(not re.fullmatch(r"[a-zA-Z0-9_-]+", peer) for peer in peers):
        raise Error("Invalid peer name in inventory")
    if any(not isinstance(network[field], str) or network[field] not in peers for field in ("hub", "dns")):
        raise Error("Hub and DNS must name inventory peers")
    try:
        if any(not isinstance(address, str) for address in peers.values()):
            raise ValueError
        addresses = [str(ipaddress.IPv4Address(address)) for address in peers.values()]
        host, port = network["endpoint"].rsplit(":", 1)
        ipaddress.IPv4Address(host)
        if type(network["port"]) is not int or not 1 <= network["port"] <= 65535:
            raise ValueError
        if int(port) != network["port"] or len(set(addresses)) != len(addresses):
            raise ValueError
    except (TypeError, ValueError, AttributeError):
        raise Error("Invalid addresses, endpoint or port in inventory") from None
    return network


def public_keys(directory, network):
    public = read_json(directory / "public-keys.json")
    if not isinstance(public, dict) or set(public) != set(network["peers"]):
        raise Error("Public manifest must contain exactly the inventory peers")
    for value in public.values():
        if not isinstance(value, str) or key(value.encode("utf-8")) != value:
            raise Error("Invalid public manifest key")
    return public


def decrypt(path):
    return key(run(["sops", "--decrypt", "--input-type", "binary", "--output-type", "binary", str(path)]))


def encrypt(path, value):
    encrypted = run(
        ["sops", "--encrypt", "--input-type", "binary", "--output-type", "binary",
         "--filename-override", str(path), "/dev/stdin"],
        (value + "\n").encode("ascii"),
    )
    # Binary SOPS output is a JSON envelope. Do not require a decrypt identity here.
    try:
        envelope = json.loads(encrypted)
        if (isinstance(envelope, dict) and set(envelope) == {"data", "sops"}
                and isinstance(envelope["data"], str) and envelope["data"].startswith("ENC[")
                and isinstance(envelope["sops"], dict)
                and isinstance(envelope["sops"].get("mac"), str)
                and envelope["sops"]["mac"].startswith("ENC[")):
            return encrypted
    except (ValueError, UnicodeError):
        pass
    raise Error(f"Invalid encrypted SOPS envelope for {path}")


def commit(directory, secrets, public):
    directory.mkdir(exist_ok=True)
    temporary = Path(tempfile.mkdtemp(prefix=".keys-", dir=directory))
    cleanup = True
    replaced = []
    try:
        staged = temporary / "new"
        backup = temporary / "old"
        updates = {path: encrypt(directory / path, value) for path, value in secrets.items()}
        updates[Path("public-keys.json")] = (json.dumps(public, indent=2) + "\n").encode("ascii")
        for path, contents in updates.items():
            target = directory / path
            if target.is_symlink() or (target.exists() and not target.is_file()):
                raise Error(f"Refusing to replace non-regular file: {target}")
            (staged / path).parent.mkdir(parents=True, exist_ok=True)
            (staged / path).write_bytes(contents)
            if target.exists():
                (backup / path).parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(target, backup / path)
        try:
            for path in updates:
                target = directory / path
                target.parent.mkdir(parents=True, exist_ok=True)
                replaced.append(path)
                os.replace(staged / path, target)
        except BaseException:
            for path in reversed(replaced):
                try:
                    if (backup / path).exists():
                        os.replace(backup / path, directory / path)
                    else:
                        (directory / path).unlink(missing_ok=True)
                except OSError:
                    cleanup = False
            if not cleanup:
                raise Error(f"Rollback incomplete; encrypted backups retained in {temporary}") from None
            raise Error("Commit failed; previous outputs restored") from None
    finally:
        if cleanup:
            shutil.rmtree(temporary)


def generate(directory, network, name=None):
    peers = network["peers"]
    hub = network["hub"]
    leaves = [peer for peer in peers if peer != hub]
    if name is None:
        public = {}
        changed = peers
        changed_psks = leaves
    else:
        public = public_keys(directory, network)
        # Refuse to rotate any part of an inconsistent or incomplete inventory.
        for peer in peers:
            if pubkey(decrypt(directory / "private" / peer)) != public[peer]:
                raise Error(f"Existing keypair is inconsistent for {peer}")
        for leaf in leaves:
            decrypt(directory / "psk" / leaf)
        changed = [name]
        changed_psks = leaves if name == hub else [name]
    secrets = {}
    for peer in changed:
        private = key(run(["wg", "genkey"]))
        public[peer] = pubkey(private)
        secrets[Path("private") / peer] = private
    for leaf in changed_psks:
        secrets[Path("psk") / leaf] = key(run(["wg", "genpsk"]))
    for peer in changed:
        if pubkey(secrets[Path("private") / peer]) != public[peer]:
            raise Error(f"Generated keypair is inconsistent for {peer}")
    commit(directory, secrets, public)


def client_config(directory, network, name):
    hub = network["hub"]
    if name == hub:
        raise Error("Config export is only supported for leaf peers, not hubs")
    public = public_keys(directory, network)
    private = decrypt(directory / "private" / name)
    if pubkey(private) != public[name]:
        raise Error(f"Existing keypair is inconsistent for {name}")
    psk = decrypt(directory / "psk" / name)
    peers = network["peers"]
    lines = ["[Interface]", f"Address = {peers[name]}/32", f"DNS = {peers[network['dns']]}"]
    allowed_ips = [f"{address}/32" for peer, address in peers.items() if peer != name]
    lines += [f"PrivateKey = {private}", "MTU = 1420", "", "[Peer]",
              f"PublicKey = {public[hub]}", f"PresharedKey = {psk}",
              f"AllowedIPs = {', '.join(allowed_ips)}", f"Endpoint = {network['endpoint']}",
              "PersistentKeepalive = 25"]
    return ("\n".join(lines) + "\n").encode("ascii")


def main():
    os.umask(0o077)
    parser = argparse.ArgumentParser(description=__doc__)
    default = Path("sops/wireguard") if Path("sops/wireguard").is_dir() else Path.cwd()
    parser.add_argument("--directory", type=Path, default=default, help="inventory and key directory")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("list", help="list inventory peers without decrypting secrets")
    regeneration = commands.add_parser("regenerate", help="generate all inventory keys")
    regeneration.add_argument("--force", action="store_true", help="allow replacing existing outputs")
    rotation = commands.add_parser("rotate", help="rotate a peer's private key and associated PSKs")
    rotation.add_argument("name")
    config = commands.add_parser("config", help="print a standalone client config (contains secrets)")
    config.add_argument("name")
    config.add_argument("--qr", action="store_true", help="render config as a terminal QR code instead")
    args = parser.parse_args()
    root = args.directory.resolve()
    network = inventory(root)
    if args.command == "list":
        for peer in network["peers"]:
            print(peer)
        return
    if args.command in {"rotate", "config"} and args.name not in network["peers"]:
        raise Error(f"Unknown peer: {args.name}")
    directory = root
    # Keep the lock file: unlinking a flock file lets waiters lock different inodes.
    with (root / ".keys.lock").open("a") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise Error("Another key operation is running") from None
        if args.command == "regenerate":
            outputs = [directory / "public-keys.json", directory / "private", directory / "psk"]
            if not args.force and any(path.exists() for path in outputs):
                raise Error("Outputs already exist; use --force to replace them")
            generate(directory, network)
            print("regenerated all peers")
        elif args.command == "rotate":
            generate(directory, network, args.name)
            print(f"rotated {args.name}")
        else:
            contents = client_config(directory, network, args.name)
            if args.qr:
                sys.stdout.buffer.write(run(["qrencode", "-t", "ansiutf8"], contents))
            else:
                sys.stdout.buffer.write(contents)


def interrupted(signum, frame):
    raise KeyboardInterrupt


if __name__ == "__main__":
    signal.signal(signal.SIGTERM, interrupted)
    try:
        main()
    except (Error, OSError) as error:
        print(f"wireguard-keys: {error}", file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        print("wireguard-keys: interrupted", file=sys.stderr)
        sys.exit(130)
