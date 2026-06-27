#!/usr/bin/env bash
set -euo pipefail

LWJGL_VERSION="3.4.1"
LWJGL_CACHE="${HOME}/.cache/coursier/v1/https/repo1.maven.org/maven2/org/lwjgl"
JAVA_CLASSES=".blockbox-java-classes"
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
SCALA_SOURCE="${PROJECT_ROOT}/src/main/scala"
JAVA_SOURCE="${PROJECT_ROOT}/src/main/java"
SYSTEM_JDK_CANDIDATES=(
  "/opt/openjdk-bin-21"
  "/usr/lib/jvm/openjdk-bin-21"
  "/usr/lib/jvm/openjdk-21"
  "/opt/openjdk-bin-25"
  "/usr/lib/jvm/openjdk-bin-25"
)
if [[ -n "${BLOCKBOX_JAVA_HOME:-}" ]]; then
  JAVA_CMD="${BLOCKBOX_JAVA_HOME}/bin/java"
  JAVAC_CMD="${BLOCKBOX_JAVA_HOME}/bin/javac"
elif [[ -n "${BLOCKBOX_JAVA:-}" ]]; then
  JAVA_CMD="${BLOCKBOX_JAVA}"
  JAVAC_CMD="${BLOCKBOX_JAVAC:-javac}"
else
  JAVA_CMD="java"
  JAVAC_CMD="javac"
  for candidate in "${SYSTEM_JDK_CANDIDATES[@]}"; do
    if [[ -x "${candidate}/bin/java" && -x "${candidate}/bin/javac" ]]; then
      JAVA_CMD="${candidate}/bin/java"
      JAVAC_CMD="${candidate}/bin/javac"
      break
    fi
  done
fi
BREW_PREFIXES=("${HOME}/.linuxbrew" "/home/linuxbrew/.linuxbrew" "/opt/homebrew" "/usr/local")
for prefix in "${BREW_PREFIXES[@]}"; do
  if [[ -d "${prefix}/bin" ]]; then
    export PATH="${prefix}/bin:${prefix}/sbin:${PATH}"
  fi
done

if ! command -v scala-cli >/dev/null 2>&1; then
  printf 'Blockbox: scala-cli was not found in PATH.\n' >&2
  printf 'Blockbox: current PATH=%s\n' "${PATH}" >&2
  printf 'Blockbox: if scala-cli is installed with Homebrew, make sure ~/.linuxbrew/bin, /home/linuxbrew/.linuxbrew/bin, /opt/homebrew/bin, or /usr/local/bin is reachable.\n' >&2
  exit 127
fi

if ! command -v "${JAVA_CMD}" >/dev/null 2>&1; then
  printf 'Blockbox: java command not found: %s\n' "${JAVA_CMD}" >&2
  exit 127
fi

if ! command -v "${JAVAC_CMD}" >/dev/null 2>&1; then
  printf 'Blockbox: javac was not found in PATH. Install JDK 21 or newer, not just a JRE.\n' >&2
  exit 127
fi
if [[ -n "${BLOCKBOX_JVM_ARGS_FILE:-}" && -f "${BLOCKBOX_JVM_ARGS_FILE}" ]]; then
  mapfile -t BLOCKBOX_JVM_ARGS_ARRAY < "${BLOCKBOX_JVM_ARGS_FILE}"
else
  read -r -a BLOCKBOX_JVM_ARGS_ARRAY <<< "${BLOCKBOX_JVM_ARGS:--Xmx4G}"
fi
if [[ -n "${BLOCKBOX_GAME_ARGS_FILE:-}" && -f "${BLOCKBOX_GAME_ARGS_FILE}" ]]; then
  mapfile -t BLOCKBOX_GAME_ARGS_ARRAY < "${BLOCKBOX_GAME_ARGS_FILE}"
else
  read -r -a BLOCKBOX_GAME_ARGS_ARRAY <<< "${BLOCKBOX_GAME_ARGS:-}"
fi
# Java AWT hint — prevents reparenting issues on Wayland compositors, harmless on X11.
export _JAVA_AWT_WM_NONREPARENTING="${_JAVA_AWT_WM_NONREPARENTING:-1}"

for i in "${!BLOCKBOX_JVM_ARGS_ARRAY[@]}"; do
  [[ -n "${BLOCKBOX_JVM_ARGS_ARRAY[$i]}" ]] || unset 'BLOCKBOX_JVM_ARGS_ARRAY[$i]'
done
for i in "${!BLOCKBOX_GAME_ARGS_ARRAY[@]}"; do
  [[ -n "${BLOCKBOX_GAME_ARGS_ARRAY[$i]}" ]] || unset 'BLOCKBOX_GAME_ARGS_ARRAY[$i]'
done

# Check if an X11 display socket actually exists (native X11 or XWayland).
x11_socket_available() {
  if [[ -n "${DISPLAY:-}" ]]; then
    local num="${DISPLAY#*:}"
    num="${num%%.*}"
    [[ -S "/tmp/.X11-unix/X${num}" ]] && return 0
  fi
  for sock in /tmp/.X11-unix/X*; do
    [[ -S "$sock" ]] && { export DISPLAY=":${sock##/tmp/.X11-unix/X}"; return 0; }
  done
  return 1
}

# Check if libdecor is installed (needed for ghost-free Wayland rendering).
libdecor_installed() {
  ldconfig -p 2>/dev/null | grep -qF libdecor-0.so && return 0
  # Fallback: check common library paths directly
  for d in /usr/lib64 /usr/lib /usr/lib/x86_64-linux-gnu /usr/local/lib; do
    [[ -f "${d}/libdecor-0.so" ]] && return 0
  done
  return 1
}

# Check if the NVIDIA DRM GBM backend is available (needed for hardware-accelerated
# OpenGL/EGL on Wayland with the proprietary NVIDIA driver).
# Uses /proc/modules directly (no pipe) to avoid SIGPIPE with set -o pipefail.
nvidia_gbm_available() {
  grep -qF nvidia_drm /proc/modules 2>/dev/null
}

# Set up environment for NVIDIA EGL on Wayland.
setup_nvidia_egl() {
  export __NV_PRIME_RENDER_OFFLOAD="${__NV_PRIME_RENDER_OFFLOAD:-1}"
  export __GLX_VENDOR_LIBRARY_NAME="${__GLX_VENDOR_LIBRARY_NAME:-nvidia}"
  export __EGL_EXTERNAL_PLATFORM_CONFIG_DIRS="${__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS:-/usr/share/egl/egl_external_platform.d}"
  if [[ "${BLOCKBOX_PRELOAD_SYSTEM_EGL:-0}" == "1" ]]; then
    export LD_PRELOAD="${LD_PRELOAD:+${LD_PRELOAD}:}/usr/lib64/libEGL.so.1"
  fi
  local system_vendor="/usr/share/glvnd/egl_vendor.d/10_nvidia.json"
  local user_vendor="${HOME}/.local/share/egl-vendor-override/10_nvidia.json"
  if [[ -f "${system_vendor}" ]]; then
    export __EGL_VENDOR_LIBRARY_FILENAMES="${system_vendor}"
  elif [[ -f "${user_vendor}" ]]; then
    export __EGL_VENDOR_LIBRARY_FILENAMES="${user_vendor}"
  fi
}

case "${BLOCKBOX_DISPLAY_BACKEND:-x11}" in
  auto)
    unset GLFW_PLATFORM
    if nvidia_gbm_available && [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
      setup_nvidia_egl
    fi
    ;;
  x11|x11-nvidia)
    if ! x11_socket_available; then
      printf 'Blockbox: no X11 display socket found. Falling back to Wayland (this is your native display). Install XWayland if you must use X11.\n' >&2
      export GLFW_PLATFORM=wayland
      if nvidia_gbm_available; then
        setup_nvidia_egl
      fi
    else
      export GLFW_PLATFORM=x11
    fi
    if [[ "${BLOCKBOX_DISPLAY_BACKEND:-}" == "x11-nvidia" ]]; then
      export __GLX_VENDOR_LIBRARY_NAME="${__GLX_VENDOR_LIBRARY_NAME:-nvidia}"
      export __GL_SYNC_TO_VBLANK="${__GL_SYNC_TO_VBLANK:-1}"
    fi
    ;;
  wayland)
    export GLFW_PLATFORM=wayland
    export GLFW_WAYLAND_LIBDECOR="${GLFW_WAYLAND_LIBDECOR:-0}"
    # Gentoo/NVIDIA can fail inside libdecor-gtk and create an invisible window.
    # Default to GLFW's non-libdecor Wayland path; users can override this env var.
    if ! libdecor_installed; then
      printf 'Blockbox: libdecor is not installed. Wayland rendering may show frame ghosting. Install it with your package manager (libdecor on Debian/Ubuntu/Fedora/Arch). Or use the Software backend as a fallback.\n'
    fi
    # Detect NVIDIA GPU and set the GBM backend for hardware-accelerated EGL on Wayland.
    # Without this, Mesa's EGL will fail to find a DRI driver for the NVIDIA GPU and
    # fall back to software llvmpipe rendering, causing severe lag.
    if nvidia_gbm_available; then
      setup_nvidia_egl
      if ! ldconfig -p 2>/dev/null | grep -qF libEGL_nvidia; then
        printf 'Blockbox: NVIDIA GPU detected but NVIDIA EGL library (libEGL_nvidia) is missing. GPU acceleration may not work. Update your NVIDIA driver or use the Software backend.\n'
      fi
    fi
    ;;
  software)
    unset GLFW_PLATFORM
    export LIBGL_ALWAYS_SOFTWARE=1
    export MESA_LOADER_DRIVER_OVERRIDE=llvmpipe
    export GALLIUM_DRIVER=llvmpipe
    ;;
  legacy|"")
    if [[ -z "${GLFW_PLATFORM:-}" ]]; then
      if [[ -n "${WAYLAND_DISPLAY:-}" ]]; then
        export GLFW_PLATFORM=wayland
        export GLFW_WAYLAND_LIBDECOR="${GLFW_WAYLAND_LIBDECOR:-0}"
        if nvidia_gbm_available; then
          setup_nvidia_egl
        fi
      elif x11_socket_available; then
        export GLFW_PLATFORM=x11
      fi
    fi
    ;;
  *)
    printf 'Blockbox: unknown BLOCKBOX_DISPLAY_BACKEND=%s, using GLFW auto\n' "${BLOCKBOX_DISPLAY_BACKEND}" >&2
    unset GLFW_PLATFORM
    ;;
esac

if [[ -n "${GLFW_PLATFORM:-}" ]]; then
  printf 'Blockbox: using GLFW_PLATFORM=%s\n' "${GLFW_PLATFORM}"
else
  printf 'Blockbox: using GLFW platform auto\n'
fi
printf 'Blockbox: backend=%s display env DISPLAY=%s WAYLAND_DISPLAY=%s XDG_SESSION_TYPE=%s LD_PRELOAD=%s __EGL_VENDOR_LIBRARY_FILENAMES=%s __EGL_EXTERNAL_PLATFORM_CONFIG_DIRS=%s\n' "${BLOCKBOX_DISPLAY_BACKEND:-x11}" "${DISPLAY:-}" "${WAYLAND_DISPLAY:-}" "${XDG_SESSION_TYPE:-}" "${LD_PRELOAD:-}" "${__EGL_VENDOR_LIBRARY_FILENAMES:-}" "${__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS:-}"
printf 'Blockbox: java=%s javac=%s\n' "${JAVA_CMD}" "${JAVAC_CMD}"
printf 'Blockbox: env _JAVA_AWT_WM_NONREPARENTING=%s LD_PRELOAD=%s __EGL_VENDOR_LIBRARY_FILENAMES=%s\n' "${_JAVA_AWT_WM_NONREPARENTING:-}" "${LD_PRELOAD:-}" "${__EGL_VENDOR_LIBRARY_FILENAMES:-}"
if [[ -n "${__GLX_VENDOR_LIBRARY_NAME:-}" || -n "${LIBGL_ALWAYS_SOFTWARE:-}" || -n "${MESA_LOADER_DRIVER_OVERRIDE:-}" || -n "${GALLIUM_DRIVER:-}" || -n "${__GL_SYNC_TO_VBLANK:-}" ]]; then
  printf 'Blockbox: GL env __GLX_VENDOR_LIBRARY_NAME=%s LIBGL_ALWAYS_SOFTWARE=%s MESA_LOADER_DRIVER_OVERRIDE=%s GALLIUM_DRIVER=%s __GL_SYNC_TO_VBLANK=%s\n' "${__GLX_VENDOR_LIBRARY_NAME:-}" "${LIBGL_ALWAYS_SOFTWARE:-}" "${MESA_LOADER_DRIVER_OVERRIDE:-}" "${GALLIUM_DRIVER:-}" "${__GL_SYNC_TO_VBLANK:-}"
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

if [[ -d "${JAVA_SOURCE}" ]]; then
  mapfile -t java_sources < <(find "${JAVA_SOURCE}" -name '*.java' -type f | sort)
  if [[ "${#java_sources[@]}" -gt 0 ]]; then
    mkdir -p "${JAVA_CLASSES}"
    "${JAVAC_CMD}" --release 21 -encoding UTF-8 -d "${JAVA_CLASSES}" "${java_sources[@]}"
  fi
fi

if [[ "${missing_native}" == "true" ]]; then
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
  --java-home "$(dirname -- "$(dirname -- "$(readlink -f "$(command -v "${JAVA_CMD}")")")")" \
  "${BLOCKBOX_JVM_ARGS_ARRAY[@]/#/--java-opt=}" \
  --java-opt "--enable-native-access=ALL-UNNAMED" \
  --extra-jar "${JAVA_CLASSES}" \
  --extra-jar "${native_jars[0]}" \
  --extra-jar "${native_jars[1]}" \
  --extra-jar "${native_jars[2]}" \
  --extra-jar "${native_jars[3]}" \
  -- "${BLOCKBOX_GAME_ARGS_ARRAY[@]}"
