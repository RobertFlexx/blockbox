#!/usr/bin/env python3
from pathlib import Path
import ctypes as C, ctypes.util, os, sys

ROOT = Path(__file__).resolve().parents[1]


def expand(path: Path, root: Path, stack=()):
    p = path.resolve()
    root = root.resolve()
    if root not in p.parents and p != root:
        raise RuntimeError(f"include escapes shader directory: {path}")
    if p in stack:
        raise RuntimeError(f"recursive include: {path}")
    out = []
    for line in p.read_text().splitlines():
        t = line.strip()
        if t.startswith("#include"):
            if '"' in t:
                rel = t.split('"', 2)[1]
            elif "<" in t and ">" in t:
                rel = t.split("<", 1)[1].split(">", 1)[0]
            else:
                raise RuntimeError(f"bad include: {line}")
            out.append(expand((p.parent / rel).resolve(), root, stack + (p,)))
        else:
            out.append(line)
    return "\n".join(out) + "\n"


E = C.CDLL(ctypes.util.find_library("EGL") or "libEGL.so.1")
G = C.CDLL(ctypes.util.find_library("GL") or "libGL.so.1")
I = C.c_int
U = C.c_uint
P = C.c_void_p
E.eglGetDisplay.argtypes = [P]
E.eglGetDisplay.restype = P
E.eglInitialize.argtypes = [P, C.POINTER(I), C.POINTER(I)]
E.eglInitialize.restype = U
E.eglBindAPI.argtypes = [U]
E.eglBindAPI.restype = U
E.eglChooseConfig.argtypes = [P, C.POINTER(I), C.POINTER(P), I, C.POINTER(I)]
E.eglChooseConfig.restype = U
E.eglCreatePbufferSurface.argtypes = [P, P, C.POINTER(I)]
E.eglCreatePbufferSurface.restype = P
E.eglCreateContext.argtypes = [P, P, P, C.POINTER(I)]
E.eglCreateContext.restype = P
E.eglMakeCurrent.argtypes = [P, P, P, P]
E.eglMakeCurrent.restype = U
E.eglDestroyContext.argtypes = [P, P]
E.eglDestroySurface.argtypes = [P, P]
E.eglTerminate.argtypes = [P]

d = E.eglGetDisplay(P(0))
ma = I()
mi = I()
if not d or not E.eglInitialize(d, C.byref(ma), C.byref(mi)):
    raise RuntimeError("EGL initialize failed")
if not E.eglBindAPI(0x30A2):
    raise RuntimeError("EGL OpenGL bind failed")
attrs = (I * 13)(0x3033, 0x0001, 0x3040, 0x0008, 0x3024, 8, 0x3023, 8, 0x3022, 8, 0x3021, 8, 0x3038)
cfg = P()
n = I()
if not E.eglChooseConfig(d, attrs, C.byref(cfg), 1, C.byref(n)) or n.value < 1:
    raise RuntimeError("no EGL OpenGL config")
pattrs = (I * 5)(0x3057, 1, 0x3056, 1, 0x3038)
surf = E.eglCreatePbufferSurface(d, cfg, pattrs)
ctx = E.eglCreateContext(d, cfg, P(0), (I * 1)(0x3038))
if not surf or not ctx or not E.eglMakeCurrent(d, surf, surf, ctx):
    raise RuntimeError("could not make OpenGL context current")

G.glCreateShader.argtypes = [U]
G.glCreateShader.restype = U
G.glShaderSource.argtypes = [U, I, C.POINTER(C.c_char_p), C.POINTER(I)]
G.glCompileShader.argtypes = [U]
G.glGetShaderiv.argtypes = [U, U, C.POINTER(I)]
G.glGetShaderInfoLog.argtypes = [U, I, C.POINTER(I), C.c_char_p]
G.glDeleteShader.argtypes = [U]
G.glCreateProgram.restype = U
G.glAttachShader.argtypes = [U, U]
G.glLinkProgram.argtypes = [U]
G.glGetProgramiv.argtypes = [U, U, C.POINTER(I)]
G.glGetProgramInfoLog.argtypes = [U, I, C.POINTER(I), C.c_char_p]
G.glDeleteProgram.argtypes = [U]
COMPILE = 0x8B81
LINK = 0x8B82
VS = 0x8B31
FS = 0x8B30


def shader(kind, src, label):
    sid = G.glCreateShader(kind)
    data = src.encode()
    csrc = C.c_char_p(data)
    ln = I(len(data))
    G.glShaderSource(sid, 1, C.byref(csrc), C.byref(ln))
    G.glCompileShader(sid)
    ok = I()
    G.glGetShaderiv(sid, COMPILE, C.byref(ok))
    if not ok.value:
        buf = C.create_string_buffer(32768)
        l = I()
        G.glGetShaderInfoLog(sid, len(buf), C.byref(l), buf)
        raise RuntimeError(f"{label}: {buf.value.decode(errors='replace')}")
    return sid


def program(vs, fs, label):
    a = shader(VS, vs, label + " vertex")
    b = shader(FS, fs, label + " fragment")
    pid = G.glCreateProgram()
    try:
        G.glAttachShader(pid, a)
        G.glAttachShader(pid, b)
        G.glLinkProgram(pid)
        ok = I()
        G.glGetProgramiv(pid, LINK, C.byref(ok))
        if not ok.value:
            buf = C.create_string_buffer(32768)
            l = I()
            G.glGetProgramInfoLog(pid, len(buf), C.byref(l), buf)
            raise RuntimeError(f"{label} link: {buf.value.decode(errors='replace')}")
    finally:
        G.glDeleteShader(a)
        G.glDeleteShader(b)
        G.glDeleteProgram(pid)


count = 0
for pack in sorted((ROOT / "shaderpacks").iterdir()):
    sd = pack / "shaders"
    if not sd.is_dir():
        continue
    names = set()
    for f in sd.iterdir():
        if f.suffix in (".vsh", ".vert", ".fsh", ".frag"):
            names.add(f.stem)
    for name in sorted(names):
        vf = next((sd / (name + e) for e in (".vsh", ".vert") if (sd / (name + e)).is_file()), None)
        ff = next((sd / (name + e) for e in (".fsh", ".frag") if (sd / (name + e)).is_file()), None)
        if vf is None or ff is None:
            raise RuntimeError(f"{pack.name}/{name}: incomplete vertex/fragment pair")
        program(expand(vf, sd), expand(ff, sd), f"{pack.name}/{name}")
        count += 1
print(f"[glsl] {count}/{count} shader programs compiled and linked (EGL OpenGL compatibility)")
E.eglMakeCurrent(d, P(0), P(0), P(0))
E.eglDestroyContext(d, ctx)
E.eglDestroySurface(d, surf)
E.eglTerminate(d)
