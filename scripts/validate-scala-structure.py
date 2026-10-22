#!/usr/bin/env python3
from pathlib import Path
import re, sys
ROOT=Path(__file__).resolve().parents[1]
files=list((ROOT/'src/main/scala').rglob('*.scala'))

def stripped(text):
    # Preserve newlines while removing strings/comments enough for delimiter checks.
    out=[]; i=0; n=len(text); block=0
    while i<n:
        if block:
            if text.startswith('/*',i): block+=1; out.extend('  '); i+=2
            elif text.startswith('*/',i): block-=1; out.extend('  '); i+=2
            else: out.append('\n' if text[i]=='\n' else ' '); i+=1
        elif text.startswith('//',i):
            j=text.find('\n',i); j=n if j<0 else j
            out.extend(' '*(j-i)); i=j
        elif text.startswith('/*',i): block=1; out.extend('  '); i+=2
        elif text.startswith('"""',i):
            j=text.find('"""',i+3); j=n-3 if j<0 else j
            seg=text[i:j+3]; out.extend('\n' if c=='\n' else ' ' for c in seg); i=j+3
        elif text[i]=='"':
            out.append(' '); i+=1
            while i<n:
                c=text[i]; out.append('\n' if c=='\n' else ' ')
                if c=='\\' and i+1<n: i+=1; out.append(' ')
                elif c=='"': i+=1; break
                i+=1
        elif text[i]=="'":
            # char literals only; Scala symbol syntax is not used in this project.
            out.append(' '); i+=1
            while i<n:
                c=text[i]; out.append(' ')
                if c=='\\' and i+1<n: i+=1; out.append(' ')
                elif c=="'": i+=1; break
                i+=1
        else: out.append(text[i]); i+=1
    return ''.join(out)

pairs={')':'(',']':'[','}':'{'}
for f in files:
    t=stripped(f.read_text()); st=[]
    for ln,line in enumerate(t.splitlines(),1):
        for c in line:
            if c in '([{': st.append((c,ln))
            elif c in pairs:
                if not st or st[-1][0]!=pairs[c]: raise SystemExit(f"[scala-structure] {f}:{ln}: mismatched {c}")
                st.pop()
    if st: raise SystemExit(f"[scala-structure] {f}:{st[-1][1]}: unclosed {st[-1][0]}")
main=(ROOT/'src/main/scala/blockbox/Main.scala').read_text()
inv=(ROOT/'src/main/scala/blockbox/BlockboxInventory.scala').read_text()
shader=(ROOT/'src/main/scala/blockbox/ShaderPipeline.scala').read_text()
checks=[
 ('9-slot hotbar','Array.fill(9)(Block.Air)' in main and 'Array.fill(9)(0)' in main),
 ('27-slot physical backpack','backpackBlocks: Array[Block] = Array.fill(27)(Block.Air)' in main),
 ('save format v3','worldExtraVersion = 3' in main),
 ('counted crafting','craftGridCounts: Array[Int] = Array.fill(9)(0)' in main),
 ('right-click inventory','handleInventoryClick(mx, my, rightClick = true)' in main),
 ('safe G-buffer clear','glClearBufferfv(GL_COLOR, 1' in shader and 'enableColorOnlyWrites' in shader),
 ('post texture-state cleanup','resetCompatibilityTextureUnits' in shader and 'glActiveTexture(GL_TEXTURE0 + unit)' in shader),
 ('isolated 3d viewmodel','drawHeldBlockCube' in main and 'glClear(GL_DEPTH_BUFFER_BIT)' in main and 'glFrustum(' in main),
 ('physical inventory model','handleBackpackSlotClick' in inv and 'inventoryRoomFor' in inv),
]
for label,ok in checks:
    if not ok: raise SystemExit('[scala-structure] missing invariant: '+label)
print(f"[scala-structure] {len(files)} Scala files passed delimiter/invariant checks")
