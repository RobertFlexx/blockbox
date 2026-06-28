#!/usr/bin/env bash
# Blockbox launcher script — cleaned up for Exherbo/Gentoo + NVIDIA + KDE Wayland/XWayland.
#
# Goals:
#   - Prefer a real JDK 21, especially Exherbo's /opt/openjdk-bin-21.* layout.
#   - Do not let Homebrew Java hijack the runtime.
#   - Default display backend to "auto", not forced X11.
#   - Do not force NVIDIA EGL vendor JSON or PRIME offload on a single-GPU desktop.
#   - Keep a software backend available, but never silently enable it.
set -Eeuo pipefail

LWJGL_VERSION="${LWJGL_VERSION:-3.4.1}"
LWJGL_CACHE="${HOME}/.cache/coursier/v1/https/repo1.maven.org/maven2/org/lwjgl"
JAVA_CLASSES=".blockbox-java-classes"

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
export BLOCKBOX_PROJECT_ROOT="${PROJECT_ROOT}"
SCALA_SOURCE="${PROJECT_ROOT}/src/main/scala"
JAVA_SOURCE="${PROJECT_ROOT}/src/main/java"

log()  { printf 'Blockbox: %s\n' "$*"; }
warn() { printf 'Blockbox warning: %s\n' "$*" >&2; }
die()  { printf 'Blockbox error: %s\n' "$*" >&2; exit 1; }

path_has_slash() {
  [[ "$1" == */* ]]
}

cmd_exists() {
  if path_has_slash "$1"; then
    [[ -x "$1" ]]
  else
    command -v "$1" >/dev/null 2>&1
  fi
}

append_path_if_dir() {
  local dir="$1"
  [[ -d "$dir" ]] || return 0
  case ":${PATH}:" in
    *":${dir}:"*) ;;
    *) PATH="${PATH}:${dir}" ;;
  esac
}

prepend_path_if_dir() {
  local dir="$1"
  [[ -d "$dir" ]] || return 0
  case ":${PATH}:" in
    *":${dir}:"*) ;;
    *) PATH="${dir}:${PATH}" ;;
  esac
}

find_best_jdk_home() {
  local candidates=()

  # Explicit user override wins.
  if [[ -n "${BLOCKBOX_JAVA_HOME:-}" ]]; then
    candidates+=("${BLOCKBOX_JAVA_HOME}")
  fi

  # Real Exherbo layout seen on your machine, plus common distro layouts.
  candidates+=(
    "/opt/openjdk-bin-21.0.11_p10"
    "/opt/openjdk-bin-21"
    "/usr/lib/jvm/openjdk-bin-21"
    "/usr/lib/jvm/openjdk-21"
    "/usr/lib/jvm/java-21-openjdk"
    "/usr/lib/jvm/temurin-21"
    "/opt/openjdk-bin-25"
    "/usr/lib/jvm/openjdk-bin-25"
  )

  # Globbed fallbacks. nullglob avoids literally returning unmatched globs.
  shopt -s nullglob
  local g
  for g in /opt/openjdk-bin-21* /usr/lib/jvm/*21* /usr/x86_64-pc-linux-gnu/lib/jvm/*21*; do
    candidates+=("$g")
  done
  shopt -u nullglob

  local c
  for c in "${candidates[@]}"; do
    if [[ -x "${c}/bin/java" && -x "${c}/bin/javac" ]]; then
      printf '%s\n' "$c"
      return 0
    fi
  done

  return 1
}

resolve_java_commands() {
  JAVA_HOME_RESOLVED=""

  if [[ -n "${BLOCKBOX_JAVA_HOME:-}" ]]; then
    [[ -x "${BLOCKBOX_JAVA_HOME}/bin/java" ]] || die "BLOCKBOX_JAVA_HOME has no executable bin/java: ${BLOCKBOX_JAVA_HOME}"
    [[ -x "${BLOCKBOX_JAVA_HOME}/bin/javac" ]] || die "BLOCKBOX_JAVA_HOME has no executable bin/javac: ${BLOCKBOX_JAVA_HOME}"
    JAVA_HOME_RESOLVED="${BLOCKBOX_JAVA_HOME}"
    JAVA_CMD="${JAVA_HOME_RESOLVED}/bin/java"
    JAVAC_CMD="${JAVA_HOME_RESOLVED}/bin/javac"
    return 0
  fi

  if [[ -n "${BLOCKBOX_JAVA:-}" ]]; then
    JAVA_CMD="${BLOCKBOX_JAVA}"
    if [[ -n "${BLOCKBOX_JAVAC:-}" ]]; then
      JAVAC_CMD="${BLOCKBOX_JAVAC}"
    else
      # If BLOCKBOX_JAVA is /path/to/bin/java, infer /path/to/bin/javac.
      if path_has_slash "${BLOCKBOX_JAVA}"; then
        local inferred
        inferred="$(dirname -- "${BLOCKBOX_JAVA}")/javac"
        [[ -x "$inferred" ]] && JAVAC_CMD="$inferred" || JAVAC_CMD="javac"
      else
        JAVAC_CMD="javac"
      fi
    fi

    if path_has_slash "$JAVA_CMD"; then
      JAVA_HOME_RESOLVED="$(dirname -- "$(dirname -- "$(readlink -f "$JAVA_CMD")")")"
    fi
    return 0
  fi

  if JAVA_HOME_RESOLVED="$(find_best_jdk_home)"; then
    JAVA_CMD="${JAVA_HOME_RESOLVED}/bin/java"
    JAVAC_CMD="${JAVA_HOME_RESOLVED}/bin/javac"
    return 0
  fi

  # Last resort: whatever PATH says. This may be Homebrew; warn loudly.
  JAVA_CMD="java"
  JAVAC_CMD="javac"
}

resolve_java_commands

if [[ -n "${JAVA_HOME_RESOLVED:-}" ]]; then
  export JAVA_HOME="${JAVA_HOME_RESOLVED}"
  prepend_path_if_dir "${JAVA_HOME}/bin"
fi

# Homebrew is allowed for scala-cli, but appended after system/JDK paths so it cannot hijack Java.
BREW_PREFIXES=("${HOME}/.linuxbrew" "/home/linuxbrew/.linuxbrew" "/opt/homebrew" "/usr/local")
for prefix in "${BREW_PREFIXES[@]}"; do
  append_path_if_dir "${prefix}/bin"
  append_path_if_dir "${prefix}/sbin"
done
hash -r 2>/dev/null || true

if ! cmd_exists "${JAVA_CMD}"; then
  die "java command not found: ${JAVA_CMD}"
fi
if ! cmd_exists "${JAVAC_CMD}"; then
  die "javac was not found. Install a JDK 21+, not only a JRE."
fi

if ! command -v scala-cli >/dev/null 2>&1; then
  die "scala-cli was not found. Install it or make it reachable in PATH. Current PATH=${PATH}"
fi

# If Java was provided as a bare command, resolve JAVA_HOME now for scala-cli --java-home.
if [[ -z "${JAVA_HOME_RESOLVED:-}" ]]; then
  if command -v "${JAVA_CMD}" >/dev/null 2>&1; then
    JAVA_HOME_RESOLVED="$(dirname -- "$(dirname -- "$(readlink -f "$(command -v "${JAVA_CMD}")")")")"
    export JAVA_HOME="${JAVA_HOME_RESOLVED}"
  fi
fi

# Basic sanity logging for the Java runtime.
JAVA_VERSION_TEXT="$("${JAVA_CMD}" -version 2>&1 | head -n 1 || true)"
JAVAC_VERSION_TEXT="$("${JAVAC_CMD}" -version 2>&1 | head -n 1 || true)"
case "${JAVA_VERSION_TEXT}" in
  *\"21.*|*\"22.*|*\"23.*|*\"24.*|*\"25.*|*\"26.*) ;;
  *) warn "Java version looks unexpected: ${JAVA_VERSION_TEXT}. Blockbox is built with --release 21." ;;
esac

read_args_array() {
  local var_name="$1"
  local file_var="$2"
  local inline_var="$3"
  local default_value="$4"

  if [[ -n "${!file_var:-}" && -f "${!file_var}" ]]; then
    mapfile -t "$var_name" < "${!file_var}"
  else
    # shellcheck disable=SC2178
    read -r -a "$var_name" <<< "${!inline_var:-$default_value}"
  fi
}

read_args_array BLOCKBOX_JVM_ARGS_ARRAY  BLOCKBOX_JVM_ARGS_FILE  BLOCKBOX_JVM_ARGS  "-Xmx4G"
read_args_array BLOCKBOX_GAME_ARGS_ARRAY BLOCKBOX_GAME_ARGS_FILE BLOCKBOX_GAME_ARGS ""

for i in "${!BLOCKBOX_JVM_ARGS_ARRAY[@]}"; do
  [[ -n "${BLOCKBOX_JVM_ARGS_ARRAY[$i]}" ]] || unset 'BLOCKBOX_JVM_ARGS_ARRAY[$i]'
done
for i in "${!BLOCKBOX_GAME_ARGS_ARRAY[@]}"; do
  [[ -n "${BLOCKBOX_GAME_ARGS_ARRAY[$i]}" ]] || unset 'BLOCKBOX_GAME_ARGS_ARRAY[$i]'
done

export _JAVA_AWT_WM_NONREPARENTING="${_JAVA_AWT_WM_NONREPARENTING:-1}"

x11_socket_available() {
  if [[ -n "${DISPLAY:-}" ]]; then
    local num="${DISPLAY#*:}"
    num="${num%%.*}"
    if [[ -S "/tmp/.X11-unix/X${num}" ]]; then
      return 0
    fi
  fi

  local sock
  for sock in /tmp/.X11-unix/X*; do
    if [[ -S "$sock" ]]; then
      export DISPLAY=":${sock##/tmp/.X11-unix/X}"
      return 0
    fi
  done
  return 1
}

wayland_available() {
  [[ -n "${WAYLAND_DISPLAY:-}" && -n "${XDG_RUNTIME_DIR:-}" ]] && [[ -S "${XDG_RUNTIME_DIR}/${WAYLAND_DISPLAY}" || -e "${XDG_RUNTIME_DIR}/${WAYLAND_DISPLAY}" ]]
}

libdecor_installed() {
  ldconfig -p 2>/dev/null | grep -qF libdecor-0.so && return 0
  local d
  for d in \
    /usr/lib64 \
    /usr/lib \
    /usr/lib/x86_64-linux-gnu \
    /usr/x86_64-pc-linux-gnu/lib \
    /usr/local/lib
  do
    [[ -f "${d}/libdecor-0.so" ]] && return 0
  done
  return 1
}

nvidia_drm_available() {
  grep -qF nvidia_drm /proc/modules 2>/dev/null
}

clean_graphics_env() {
  # These variables are common causes of "why am I on llvmpipe?" pain.
  # Keep them only when the user explicitly asks.
  if [[ "${BLOCKBOX_KEEP_SOFTWARE_GL_ENV:-0}" != "1" ]]; then
    unset LIBGL_ALWAYS_SOFTWARE
    unset MESA_LOADER_DRIVER_OVERRIDE
    unset GALLIUM_DRIVER
  fi

  if [[ "${BLOCKBOX_KEEP_EGL_OVERRIDES:-0}" != "1" ]]; then
    unset __EGL_VENDOR_LIBRARY_FILENAMES
    unset __EGL_EXTERNAL_PLATFORM_CONFIG_DIRS
  fi

  # PRIME offload is for hybrid laptops. On a single-GPU NVIDIA desktop it can
  # make GLFW/EGL choose strange paths. Only set it when explicitly requested.
  if [[ "${BLOCKBOX_NVIDIA_PRIME_OFFLOAD:-0}" != "1" ]]; then
    unset __NV_PRIME_RENDER_OFFLOAD
  else
    export __NV_PRIME_RENDER_OFFLOAD=1
  fi
}

setup_nvidia_glx() {
  export __GLX_VENDOR_LIBRARY_NAME="${__GLX_VENDOR_LIBRARY_NAME:-nvidia}"
  export __GL_SYNC_TO_VBLANK="${__GL_SYNC_TO_VBLANK:-1}"
}

setup_wayland() {
  export GLFW_WAYLAND_LIBDECOR="${GLFW_WAYLAND_LIBDECOR:-0}"
  if ! libdecor_installed; then
    warn "libdecor was not detected. Wayland may still work, but window decorations can be weird."
  fi
}

force_nvidia_egl_if_requested() {
  [[ "${BLOCKBOX_FORCE_NVIDIA_EGL:-0}" == "1" ]] || return 0

  local system_vendor="/usr/share/glvnd/egl_vendor.d/10_nvidia.json"
  local user_vendor="${HOME}/.local/share/egl-vendor-override/10_nvidia.json"

  if [[ -f "${system_vendor}" ]]; then
    export __EGL_VENDOR_LIBRARY_FILENAMES="${system_vendor}"
  elif [[ -f "${user_vendor}" ]]; then
    export __EGL_VENDOR_LIBRARY_FILENAMES="${user_vendor}"
  else
    warn "BLOCKBOX_FORCE_NVIDIA_EGL=1 was set, but no NVIDIA EGL vendor JSON was found."
  fi

  if [[ -d "/usr/share/egl/egl_external_platform.d" ]]; then
    export __EGL_EXTERNAL_PLATFORM_CONFIG_DIRS="/usr/share/egl/egl_external_platform.d"
  fi
}

clean_graphics_env

BACKEND="${BLOCKBOX_DISPLAY_BACKEND:-auto}"
BACKEND="$(printf '%s' "$BACKEND" | tr '[:upper:]' '[:lower:]')"
[[ -n "$BACKEND" ]] || BACKEND="auto"
export BLOCKBOX_DISPLAY_BACKEND="$BACKEND"

case "$BACKEND" in
  auto)
    # Let GLFW decide. On modern GLFW/LWJGL this is usually the least cursed.
    unset GLFW_PLATFORM
    if [[ "${XDG_SESSION_TYPE:-}" == "wayland" || -n "${WAYLAND_DISPLAY:-}" ]]; then
      setup_wayland
    fi
    if nvidia_drm_available; then
      setup_nvidia_glx
    fi
    ;;

  x11|x11-nvidia)
    if x11_socket_available; then
      export GLFW_PLATFORM=x11
      setup_nvidia_glx
    elif wayland_available; then
      warn "No X11 socket found. Falling back to Wayland."
      export GLFW_PLATFORM=wayland
      setup_wayland
      nvidia_drm_available && setup_nvidia_glx
    else
      die "No usable X11 or Wayland display found. DISPLAY=${DISPLAY:-unset} WAYLAND_DISPLAY=${WAYLAND_DISPLAY:-unset}"
    fi
    ;;

  wayland)
    if wayland_available || [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
      export GLFW_PLATFORM=wayland
      setup_wayland
      nvidia_drm_available && setup_nvidia_glx
    else
      warn "Wayland requested but no Wayland socket found. Trying X11."
      if x11_socket_available; then
        export GLFW_PLATFORM=x11
        setup_nvidia_glx
      else
        die "No usable Wayland or X11 display found."
      fi
    fi
    ;;

  software)
    unset GLFW_PLATFORM
    export LIBGL_ALWAYS_SOFTWARE=1
    export MESA_LOADER_DRIVER_OVERRIDE=llvmpipe
    export GALLIUM_DRIVER=llvmpipe
    ;;

  null|headless)
    export GLFW_PLATFORM=null
    ;;

  legacy)
    # Original-ish behavior, but still cleaned. Prefer native Wayland on Wayland.
    if [[ -z "${GLFW_PLATFORM:-}" ]]; then
      if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
        export GLFW_PLATFORM=wayland
        setup_wayland
      elif x11_socket_available; then
        export GLFW_PLATFORM=x11
      fi
    fi
    nvidia_drm_available && setup_nvidia_glx
    ;;

  *)
    warn "unknown BLOCKBOX_DISPLAY_BACKEND=${BACKEND}; using GLFW auto"
    unset GLFW_PLATFORM
    BACKEND="auto"
    export BLOCKBOX_DISPLAY_BACKEND="$BACKEND"
    ;;
esac

force_nvidia_egl_if_requested

if [[ -n "${GLFW_PLATFORM:-}" ]]; then
  log "using GLFW_PLATFORM=${GLFW_PLATFORM}"
else
  log "using GLFW platform auto"
fi

log "backend=${BLOCKBOX_DISPLAY_BACKEND} DISPLAY=${DISPLAY:-} WAYLAND_DISPLAY=${WAYLAND_DISPLAY:-} XDG_SESSION_TYPE=${XDG_SESSION_TYPE:-}"
log "java=${JAVA_CMD} (${JAVA_VERSION_TEXT}) javac=${JAVAC_CMD} (${JAVAC_VERSION_TEXT}) JAVA_HOME=${JAVA_HOME:-}"
log "graphics env GLFW_WAYLAND_LIBDECOR=${GLFW_WAYLAND_LIBDECOR:-} __GLX_VENDOR_LIBRARY_NAME=${__GLX_VENDOR_LIBRARY_NAME:-} __NV_PRIME_RENDER_OFFLOAD=${__NV_PRIME_RENDER_OFFLOAD:-}"
log "EGL overrides __EGL_VENDOR_LIBRARY_FILENAMES=${__EGL_VENDOR_LIBRARY_FILENAMES:-} __EGL_EXTERNAL_PLATFORM_CONFIG_DIRS=${__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS:-}"
if [[ -n "${LIBGL_ALWAYS_SOFTWARE:-}" || -n "${MESA_LOADER_DRIVER_OVERRIDE:-}" || -n "${GALLIUM_DRIVER:-}" ]]; then
  log "software GL env LIBGL_ALWAYS_SOFTWARE=${LIBGL_ALWAYS_SOFTWARE:-} MESA_LOADER_DRIVER_OVERRIDE=${MESA_LOADER_DRIVER_OVERRIDE:-} GALLIUM_DRIVER=${GALLIUM_DRIVER:-}"
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

mkdir -p "${JAVA_CLASSES}"

if [[ -d "${JAVA_SOURCE}" ]]; then
  mapfile -t java_sources < <(find "${JAVA_SOURCE}" -name '*.java' -type f | sort)
  if [[ "${#java_sources[@]}" -gt 0 ]]; then
    "${JAVAC_CMD}" --release 21 -encoding UTF-8 -d "${JAVA_CLASSES}" "${java_sources[@]}"
  fi
fi

if [[ "${missing_native}" == "true" ]]; then
  log "Downloading LWJGL native dependencies"
  scala-cli compile "${SCALA_SOURCE}" \
    --server=false \
    --extra-jar "${JAVA_CLASSES}" \
    --dependency "org.lwjgl:lwjgl:${LWJGL_VERSION},classifier=natives-linux" \
    --dependency "org.lwjgl:lwjgl-glfw:${LWJGL_VERSION},classifier=natives-linux" \
    --dependency "org.lwjgl:lwjgl-opengl:${LWJGL_VERSION},classifier=natives-linux" \
    --dependency "org.lwjgl:lwjgl-stb:${LWJGL_VERSION},classifier=natives-linux"
fi

scala-cli run "${SCALA_SOURCE}" \
  --server=false \
  --java-home "${JAVA_HOME_RESOLVED}" \
  "${BLOCKBOX_JVM_ARGS_ARRAY[@]/#/--java-opt=}" \
  --java-opt "--enable-native-access=ALL-UNNAMED" \
  --extra-jar "${JAVA_CLASSES}" \
  --extra-jar "${native_jars[0]}" \
  --extra-jar "${native_jars[1]}" \
  --extra-jar "${native_jars[2]}" \
  --extra-jar "${native_jars[3]}" \
  -- "${BLOCKBOX_GAME_ARGS_ARRAY[@]}"
