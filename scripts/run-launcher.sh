#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
LAUNCHER_ROOT="${BLOCKBOX_LAUNCHER_ROOT:-$(cd -- "${PROJECT_ROOT}/.." && pwd)/blockbox-launcher}"

if [[ -n "${BLOCKBOX_LAUNCHER_JAVA_HOME:-}" ]]; then
  export JAVA_HOME="${BLOCKBOX_LAUNCHER_JAVA_HOME}"
  export PATH="${JAVA_HOME}/bin:${PATH}"
else
  for candidate in /opt/openjdk-bin-21 /usr/lib/jvm/openjdk-21 /usr/lib/jvm/openjdk-bin-21; do
    if [[ -x "${candidate}/bin/java" ]]; then
      export JAVA_HOME="${candidate}"
      export PATH="${JAVA_HOME}/bin:${PATH}"
      break
    fi
  done
fi

if [[ ! -d "${LAUNCHER_ROOT}" ]]; then
  printf 'Blockbox launcher folder not found: %s\n' "${LAUNCHER_ROOT}" >&2
  printf 'Set BLOCKBOX_LAUNCHER_ROOT=/path/to/blockbox-launcher if it lives somewhere else.\n' >&2
  exit 1
fi

export BLOCKBOX_GAME_ROOT="${BLOCKBOX_GAME_ROOT:-${PROJECT_ROOT}}"
cd "${LAUNCHER_ROOT}"
gradle run
