#!/usr/bin/env bash
set -euo pipefail

LWJGL_VERSION="3.4.1"
LWJGL_CACHE="${HOME}/.cache/coursier/v1/https/repo1.maven.org/maven2/org/lwjgl"

if [[ -z "${GLFW_PLATFORM:-}" ]]; then
  if [[ -n "${DISPLAY:-}" ]]; then
    export GLFW_PLATFORM=x11
  elif [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
    export GLFW_PLATFORM=wayland
    export GLFW_WAYLAND_LIBDECOR="${GLFW_WAYLAND_LIBDECOR:-0}"
  fi
fi

if [[ -n "${GLFW_PLATFORM:-}" ]]; then
  printf 'Blockbox: using GLFW_PLATFORM=%s\n' "${GLFW_PLATFORM}"
fi

native_jars=(
  "${LWJGL_CACHE}/lwjgl/${LWJGL_VERSION}/lwjgl-${LWJGL_VERSION}-natives-linux.jar"
  "${LWJGL_CACHE}/lwjgl-glfw/${LWJGL_VERSION}/lwjgl-glfw-${LWJGL_VERSION}-natives-linux.jar"
  "${LWJGL_CACHE}/lwjgl-opengl/${LWJGL_VERSION}/lwjgl-opengl-${LWJGL_VERSION}-natives-linux.jar"
  "${LWJGL_CACHE}/lwjgl-stb/${LWJGL_VERSION}/lwjgl-stb-${LWJGL_VERSION}-natives-linux.jar"
)

missing_native=false
for jar in "${native_jars[@]}"; do
  if [[ ! -f "${jar}" ]]; then
    missing_native=true
  fi
done

if [[ "${missing_native}" == "true" ]]; then
  scala-cli compile . \
    --dependency "org.lwjgl:lwjgl:${LWJGL_VERSION},classifier=natives-linux" \
    --dependency "org.lwjgl:lwjgl-glfw:${LWJGL_VERSION},classifier=natives-linux" \
    --dependency "org.lwjgl:lwjgl-opengl:${LWJGL_VERSION},classifier=natives-linux" \
    --dependency "org.lwjgl:lwjgl-stb:${LWJGL_VERSION},classifier=natives-linux"
fi

scala-cli run . \
  --java-opt "-Xmx4G" \
  --java-opt "--enable-native-access=ALL-UNNAMED" \
  --extra-jar "${native_jars[0]}" \
  --extra-jar "${native_jars[1]}" \
  --extra-jar "${native_jars[2]}" \
  --extra-jar "${native_jars[3]}"
