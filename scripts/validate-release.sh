#!/usr/bin/env bash
set -Eeuo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"; cd "$ROOT"
say(){ printf '[validate] %s\n' "$*"; }; fail(){ printf '[validate] ERROR: %s\n' "$*" >&2; exit 1; }
say 'checking shell launchers'
while IFS= read -r -d '' f; do bash -n "$f"; done < <(find scripts -type f -name '*.sh' -print0)
say 'compiling and running Java 21 helper tests'
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
main_classes="$tmp/main-classes"; test_classes="$tmp/test-classes"; helper_jar="$tmp/blockbox-java-helpers.jar"
mkdir -p "$main_classes" "$test_classes"
mapfile -t java_main_sources < <(find src/main/java -type f -name '*.java' | sort)
mapfile -t java_test_sources < <(find src/test/java -type f -name '*.java' | sort)
if ((${#java_main_sources[@]})); then
  javac --release 21 -encoding UTF-8 -d "$main_classes" "${java_main_sources[@]}"
  jar --create --file "$helper_jar" -C "$main_classes" .
  [[ -s "$helper_jar" ]] || fail 'Java helper jar was not created'
  jar tf "$helper_jar" | grep -q '^blockbox/io/BlockboxSaveFiles.class$' || fail 'helper jar missing save classes'
  jar tf "$helper_jar" | grep -q '^blockbox/net/BlockboxNet.class$' || fail 'helper jar missing network classes'
fi
if ((${#java_test_sources[@]})); then
  javac --release 21 -encoding UTF-8 -cp "$helper_jar" -d "$test_classes" "${java_test_sources[@]}"
  java -cp "$helper_jar:$test_classes" blockbox.BlockboxJavaHelpersTest
fi
say 'checking Scala source structure and release invariants'
python3 scripts/validate-scala-structure.py
if [[ -z "${__EGL_VENDOR_LIBRARY_FILENAMES:-}" && -f /usr/share/glvnd/egl_vendor.d/10_nvidia.json ]]; then
  export __EGL_VENDOR_LIBRARY_FILENAMES=/usr/share/glvnd/egl_vendor.d/10_nvidia.json
fi
say 'driver-compiling every installed GLSL program'
EGL_PLATFORM=surfaceless python3 scripts/validate-glsl.py
if command -v glslangValidator >/dev/null 2>&1 && command -v spirv-val >/dev/null 2>&1; then
  say 'compiling and validating native Vulkan ray kernels and shipped SPIR-V'
  for kernel in assets/shaders/rt/*.comp; do
    [[ -f "$kernel" ]] || continue
    glslangValidator -V --target-env vulkan1.2 "$kernel" -o "$tmp/ray-kernel.spv"
    spirv-val --target-env vulkan1.2 "$tmp/ray-kernel.spv"
    spirv-val --target-env vulkan1.2 "${kernel%.comp}.spv"
  done
fi
say 'render-smoke testing terrain shaders for color-channel collapse'
EGL_PLATFORM=surfaceless python3 scripts/validate-shader-render.py
say 'checking shader-pack selection'
[[ -s shaderpacks/active.txt ]] || fail 'missing shaderpacks/active.txt'
active="$(tr -d '\r\n' < shaderpacks/active.txt)"; [[ -d "shaderpacks/$active/shaders" ]] || fail "active shader pack does not exist: $active"
if [[ -n "${BLOCKBOX_SCALA_TOOLCHAIN:-}" ]]; then
  [[ -x "$BLOCKBOX_SCALA_TOOLCHAIN/bin/scalac" ]] || fail 'BLOCKBOX_SCALA_TOOLCHAIN must contain bin/scalac'
  [[ -n "${BLOCKBOX_TEST_CLASSPATH:-}" ]] || fail 'offline validation requires BLOCKBOX_TEST_CLASSPATH with the Scala libraries and project dependencies'
  say 'compiling the complete Scala project and regression tests with the supplied offline toolchain'
  mkdir -p "$tmp/scala-classes"
  mapfile -t gpu_java_sources < <(find src/main/scala -type f -name '*.java' | sort)
  if ((${#gpu_java_sources[@]})); then
    javac --release 21 -encoding UTF-8 -cp "$helper_jar:$BLOCKBOX_TEST_CLASSPATH" -d "$tmp/scala-classes" "${gpu_java_sources[@]}"
  fi
  mapfile -t scala_sources < <(find src/main/scala src/test/scala -type f -name '*.scala' | sort)
  "$BLOCKBOX_SCALA_TOOLCHAIN/bin/scalac" -classpath "$tmp/scala-classes:$helper_jar:$BLOCKBOX_TEST_CLASSPATH" -d "$tmp/scala-classes" "${scala_sources[@]}"
  say 'running gameplay regression tests'
  java -cp "$tmp/scala-classes:$helper_jar:$BLOCKBOX_TEST_CLASSPATH" blockbox.runGameSystemsTests
  if [[ "${BLOCKBOX_RUN_ENGINE_RENDER_TESTS:-0}" == "1" ]]; then
    say 'rendering real engine frames and testing HDR, resolution scaling and reload recovery'
    EGL_PLATFORM=surfaceless java -Djava.awt.headless=true -Dorg.lwjgl.opengl.contextAPI=EGL \
      -Dorg.lwjgl.librarypath="${BLOCKBOX_TEST_NATIVE_PATH:-$ROOT/.lwjgl/3.4.1+2/x64}" \
      -cp "$tmp/scala-classes:$helper_jar:$BLOCKBOX_TEST_CLASSPATH" blockbox.runShaderPipelineTests "${BLOCKBOX_RENDER_ARTIFACTS:-$tmp/render-previews}"
  fi
elif command -v scala-cli >/dev/null 2>&1; then
  say 'compiling the complete Scala project and regression tests against the Java helper jar'
  scala-cli compile src/main/scala src/test/scala --server=false --extra-jar "$helper_jar"
  say 'running gameplay regression tests'
  scala-cli run src/main/scala src/test/scala --server=false --extra-jar "$helper_jar" --main-class blockbox.runGameSystemsTests
  if [[ "${BLOCKBOX_RUN_ENGINE_RENDER_TESTS:-0}" == "1" ]]; then
    say 'rendering real engine frames and testing HDR, resolution scaling and reload recovery'
    EGL_PLATFORM=surfaceless scala-cli run src/main/scala src/test/scala --server=false --extra-jar "$helper_jar" \
      --java-opt -Djava.awt.headless=true --java-opt -Dorg.lwjgl.opengl.contextAPI=EGL \
      --java-opt "-Dorg.lwjgl.librarypath=${BLOCKBOX_TEST_NATIVE_PATH:-$ROOT/.lwjgl/3.4.1+2/x64}" \
      --main-class blockbox.runShaderPipelineTests -- "${BLOCKBOX_RENDER_ARTIFACTS:-$tmp/render-previews}"
  fi
else
  if [[ "${BLOCKBOX_REQUIRE_SCALA:-0}" == "1" ]]; then
    fail 'scala-cli is required by BLOCKBOX_REQUIRE_SCALA=1 but was not found'
  fi
  say 'scala-cli not installed here; Scala typecheck gate skipped (set BLOCKBOX_REQUIRE_SCALA=1 to make this fatal)'
fi
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then say 'checking git diff whitespace'; git diff --check; fi
say 'all available validation gates passed'
