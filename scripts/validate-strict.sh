#!/usr/bin/env bash
set -Eeuo pipefail
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
export BLOCKBOX_REQUIRE_SCALA=1
exec "${SCRIPT_DIR}/validate-release.sh"
