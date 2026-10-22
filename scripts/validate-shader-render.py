#!/usr/bin/env python3
# Semantic smoke test: render a colored textured quad through every terrain pack and
# verify the final G-buffer color attachment contains a real multi-channel image.
from pathlib import Path
import ctypes as C, ctypes.util, os
ROOT=Path(__file__).resolve().parents[1]

def expand(path,root,stack=()):
    path=Path(path).resolve(); root=Path(root).resolve()
    if root not in path.parents and path!=root: raise RuntimeError('include escapes shader directory')
    if path in stack: raise RuntimeError('recursive include')
    out=[]
    for line in path.read_text().splitlines():
        t=line.strip()
        if t.startswith('#include'):
            rel=t.split('"',2)[1] if '"' in t else t.split('<',1)[1].split('>',1)[0]
            out.append(expand(path.parent/rel,root,stack+(path,)))
        else: out.append(line)
    return '\n'.join(out)+'\n'

E=C.CDLL(ctypes.util.find_library('EGL') or 'libEGL.so.1'); G=C.CDLL(ctypes.util.find_library('GL') or 'libGL.so.1')
I=C.c_int; U=C.c_uint; P=C.c_void_p; F=C.c_float
# EGL
for fn,args,res in [
 ('eglGetDisplay',[P],P),('eglInitialize',[P,C.POINTER(I),C.POINTER(I)],U),('eglBindAPI',[U],U),
 ('eglChooseConfig',[P,C.POINTER(I),C.POINTER(P),I,C.POINTER(I)],U),('eglCreatePbufferSurface',[P,P,C.POINTER(I)],P),
 ('eglCreateContext',[P,P,P,C.POINTER(I)],P),('eglMakeCurrent',[P,P,P,P],U)]:
    f=getattr(E,fn); f.argtypes=args; f.restype=res
D=E.eglGetDisplay(P(0)); ma=I(); mi=I(); assert E.eglInitialize(D,C.byref(ma),C.byref(mi)); assert E.eglBindAPI(0x30A2)
attrs=(I*13)(0x3033,1,0x3040,8,0x3024,8,0x3023,8,0x3022,8,0x3021,8,0x3038); cfg=P(); n=I(); assert E.eglChooseConfig(D,attrs,C.byref(cfg),1,C.byref(n)) and n.value
surf=E.eglCreatePbufferSurface(D,cfg,(I*5)(0x3057,32,0x3056,32,0x3038)); ctx=E.eglCreateContext(D,cfg,P(0),(I*1)(0x3038)); assert E.eglMakeCurrent(D,surf,surf,ctx)
# Function signatures
G.glCreateShader.argtypes=[U];G.glCreateShader.restype=U;G.glShaderSource.argtypes=[U,I,C.POINTER(C.c_char_p),C.POINTER(I)];G.glCompileShader.argtypes=[U];G.glGetShaderiv.argtypes=[U,U,C.POINTER(I)];G.glGetShaderInfoLog.argtypes=[U,I,C.POINTER(I),C.c_char_p]
G.glCreateProgram.restype=U;G.glAttachShader.argtypes=[U,U];G.glLinkProgram.argtypes=[U];G.glGetProgramiv.argtypes=[U,U,C.POINTER(I)];G.glUseProgram.argtypes=[U];G.glGetUniformLocation.argtypes=[U,C.c_char_p];G.glGetUniformLocation.restype=I;G.glUniform1i.argtypes=[I,I];G.glUniform1f.argtypes=[I,F];G.glUniform3f.argtypes=[I,F,F,F]
G.glGenTextures.argtypes=[I,C.POINTER(U)];G.glBindTexture.argtypes=[U,U];G.glTexParameteri.argtypes=[U,U,I];G.glTexImage2D.argtypes=[U,I,I,I,I,I,U,U,P]
G.glGenFramebuffers.argtypes=[I,C.POINTER(U)];G.glBindFramebuffer.argtypes=[U,U];G.glFramebufferTexture2D.argtypes=[U,U,U,U,I];G.glDrawBuffers.argtypes=[I,C.POINTER(U)];G.glCheckFramebufferStatus.argtypes=[U];G.glCheckFramebufferStatus.restype=U
G.glViewport.argtypes=[I,I,I,I];G.glClearColor.argtypes=[F,F,F,F];G.glClear.argtypes=[U];G.glMatrixMode.argtypes=[U];G.glLoadIdentity.argtypes=[];G.glBegin.argtypes=[U];G.glEnd.argtypes=[];G.glColor4f.argtypes=[F,F,F,F];G.glNormal3f.argtypes=[F,F,F];G.glTexCoord2f.argtypes=[F,F];G.glVertex3f.argtypes=[F,F,F];G.glReadBuffer.argtypes=[U];G.glReadPixels.argtypes=[I,I,I,I,U,U,P]
G.glActiveTexture.argtypes=[U];G.glMultiTexCoord2f.argtypes=[U,F,F];G.glTranslatef.argtypes=[F,F,F];G.glScalef.argtypes=[F,F,F]
VS=0x8B31;FS=0x8B30;OK=0x8B81;LINK=0x8B82

def sh(k,src):
    x=G.glCreateShader(k); b=src.encode(); p=C.c_char_p(b); l=I(len(b)); G.glShaderSource(x,1,C.byref(p),C.byref(l));G.glCompileShader(x); ok=I();G.glGetShaderiv(x,OK,C.byref(ok));
    if not ok.value:
        z=C.create_string_buffer(8192);G.glGetShaderInfoLog(x,len(z),None,z);raise RuntimeError(z.value.decode())
    return x

def prog(v,f):
    a=sh(VS,v);b=sh(FS,f);p=G.glCreateProgram();G.glAttachShader(p,a);G.glAttachShader(p,b);G.glLinkProgram(p);ok=I();G.glGetProgramiv(p,LINK,C.byref(ok));
    if not ok.value: raise RuntimeError('link failed')
    return p
# 2x2 colorful texture
tex=U();G.glGenTextures(1,C.byref(tex));G.glBindTexture(0x0DE1,tex);G.glTexParameteri(0x0DE1,0x2801,0x2600);G.glTexParameteri(0x0DE1,0x2800,0x2600)
pixels=(C.c_ubyte*16)(220,80,35,255, 60,190,70,255, 45,90,220,255, 220,190,60,255)
G.glTexImage2D(0x0DE1,0,0x1908,2,2,0,0x1908,0x1401,C.cast(pixels,P))
# 3-color MRT FBO
fbo=U();G.glGenFramebuffers(1,C.byref(fbo));G.glBindFramebuffer(0x8D40,fbo); colors=[]
for i in range(3):
    t=U();G.glGenTextures(1,C.byref(t));G.glBindTexture(0x0DE1,t);G.glTexParameteri(0x0DE1,0x2801,0x2600);G.glTexParameteri(0x0DE1,0x2800,0x2600);G.glTexImage2D(0x0DE1,0,0x1908,32,32,0,0x1908,0x1401,P(0));G.glFramebufferTexture2D(0x8D40,0x8CE0+i,0x0DE1,t,0);colors.append(t)
bufs=(U*3)(0x8CE0,0x8CE1,0x8CE2);G.glDrawBuffers(3,bufs);assert G.glCheckFramebufferStatus(0x8D40)==0x8CD5
G.glViewport(0,0,32,32);G.glMatrixMode(0x1701);G.glLoadIdentity();G.glMatrixMode(0x1700);G.glLoadIdentity()
# 16x16 block/sky lightmap on unit 1 with the engine's 0..240 -> texel-centre matrix.
lightmap=U();G.glActiveTexture(0x84C1);G.glGenTextures(1,C.byref(lightmap));G.glBindTexture(0x0DE1,lightmap)
G.glTexParameteri(0x0DE1,0x2801,0x2601);G.glTexParameteri(0x0DE1,0x2800,0x2601)
G.glTexParameteri(0x0DE1,0x2802,0x812F);G.glTexParameteri(0x0DE1,0x2803,0x812F)
lm=(C.c_ubyte*(16*16*4))()
for sky in range(16):
    for block in range(16):
        level=max(sky,block)/15.0; v=0.03+0.97*level**1.6; i=(sky*16+block)*4
        lm[i:i+4]=[int(255*min(v*(1.0 if sky>=block else 1.08),1.0)),int(255*v*0.97),int(255*v*(0.94 if sky>=block else 0.70)),255]
G.glTexImage2D(0x0DE1,0,0x1908,16,16,0,0x1908,0x1401,C.cast(lm,P))
G.glMatrixMode(0x1702);G.glLoadIdentity();G.glTranslatef(0.03125,0.03125,0.03125);G.glScalef(1/256,1/256,1/256);G.glMatrixMode(0x1700)
G.glActiveTexture(0x84C0)

def uniform(p,name,kind,*vals):
    loc=G.glGetUniformLocation(p,name.encode())
    if loc<0:return
    if kind=='i':G.glUniform1i(loc,int(vals[0]))
    elif kind=='f':G.glUniform1f(loc,F(vals[0]))
    else:G.glUniform3f(loc,F(vals[0]),F(vals[1]),F(vals[2]))

results=[]
for pack in sorted((ROOT/'shaderpacks').iterdir()):
    sd=pack/'shaders'; vf=sd/'gbuffers_terrain.vsh'; ff=sd/'gbuffers_terrain.fsh'
    if not vf.is_file() or not ff.is_file(): continue
    p=prog(expand(vf,sd),expand(ff,sd));G.glUseProgram(p);G.glBindTexture(0x0DE1,tex)
    uniform(p,'uAtlas','i',0);uniform(p,'texture','i',0);uniform(p,'gtexture','i',0);uniform(p,'lightmap','i',1);uniform(p,'uFogColor','v',0.55,0.72,0.95);uniform(p,'uFogStart','f',80);uniform(p,'uFogEnd','f',200);uniform(p,'uDaylight','f',1);uniform(p,'sunBrightness','f',1);uniform(p,'uTime','f',12);uniform(p,'frameTimeCounter','f',12);uniform(p,'renderStage','i',0);uniform(p,'uPass','i',0);uniform(p,'uSunDir','v',0.3,0.85,-0.25);uniform(p,'uMoonDir','v',-0.3,-0.85,0.25)
    def draw(sky):
        G.glClearColor(0,0,0,1);G.glClear(0x00004000)
        G.glBegin(0x0007);G.glColor4f(1,1,1,1);G.glNormal3f(0,1,0);G.glMultiTexCoord2f(0x84C1,0.0,sky*16.0)
        for x,y,u,v in [(-.8,-.8,0,0),(.8,-.8,1,0),(.8,.8,1,1),(-.8,.8,0,1)]:G.glTexCoord2f(u,v);G.glVertex3f(x,y,0)
        G.glEnd();G.glReadBuffer(0x8CE0);pix=(C.c_ubyte*4)();G.glReadPixels(16,16,1,1,0x1908,0x1401,C.cast(pix,P));return tuple(pix[:3])
    rgb=draw(15); dark=draw(0); G.glUseProgram(0); results.append((pack.name,rgb,dark))
    active=sum(c>8 for c in rgb)
    if active<2 or max(rgb)<24: raise RuntimeError(f'{pack.name}: terrain smoke produced suspicious RGB {rgb}')
    if sum(dark)*3>sum(rgb): raise RuntimeError(f'{pack.name}: terrain ignores the lightmap (sky 15 {rgb}, sky 0 {dark})')
for name,rgb,dark in results: print(f'[shader-render] {name}: RGB={rgb} unlit={dark}')
print(f'[shader-render] {len(results)}/{len(results)} terrain packs produced healthy multi-channel output and honour the lightmap')
