#!/usr/bin/env python3
"""Format first-party sources, or verify them with --check. Never touches saves."""

import argparse
from pathlib import Path
import shutil
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--check", action="store_true")
args = parser.parse_args()
roots = [ROOT / name for name in ("src", "scripts", "shaderpacks", "assets/shaders", "examples")]
files = sorted(path for root in roots for path in root.rglob("*") if path.is_file())
tools = {name: shutil.which(name) for name in ("scala-cli", "clang-format", "black", "shfmt")}
missing = [name for name, path in tools.items() if path is None]
if missing:
    sys.exit("Install the formatters first: " + ", ".join(missing) + ". See FORMATTING.md.")


def run(command):
    subprocess.run(command, cwd=ROOT, check=True)


scala = [str(path.relative_to(ROOT)) for path in files if path.suffix == ".scala"]
run(
    [tools["scala-cli"], "fmt", *scala, "--scalafmt-conf", ".scalafmt.conf"]
    + (["--check"] if args.check else [])
)
native = {".java", ".vert", ".frag", ".vsh", ".fsh", ".gsh", ".glsl", ".comp"}
clang = [str(path.relative_to(ROOT)) for path in files if path.suffix in native]
run(
    [
        tools["clang-format"],
        "--style=file",
        *(["--dry-run", "--Werror"] if args.check else ["-i"]),
        *clang,
    ]
)
python = [str(path.relative_to(ROOT)) for path in files if path.suffix == ".py"]
run(
    [
        tools["black"],
        "--workers",
        "1",
        "--line-length",
        "100",
        *(["--check"] if args.check else []),
        *python,
    ]
)
shell = [str(path.relative_to(ROOT)) for path in files if path.suffix == ".sh"]
run([tools["shfmt"], "-i", "2", "-ci", "-sr", "-d" if args.check else "-w", *shell])
# These small launchers/examples retain their native indentation and line endings.
# Normalize whitespace without treating PowerShell/Groovy as another language.
for path in files:
    if path.suffix in {".groovy", ".ps1", ".bat"}:
        current = path.read_bytes()
        newline = "\r\n" if path.suffix in {".ps1", ".bat"} else "\n"
        expected = (
            newline.join(line.rstrip() for line in current.decode().splitlines()) + newline
        ).encode()
        if current != expected:
            if args.check:
                sys.exit(f"Whitespace differs: {path.relative_to(ROOT)}")
            path.write_bytes(expected)
print("All first-party source files are formatted.")
