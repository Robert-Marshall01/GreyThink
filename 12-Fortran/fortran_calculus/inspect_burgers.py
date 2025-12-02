import re

f='burgers_snapshots.csv'
with open(f,'rb') as fh:
    raw=fh.read()
try:
    text=raw.decode('utf-8')
except Exception:
    text=raw.decode('latin-1')

for n,l in enumerate(text.splitlines(),1):
    if not l.strip():
        continue
    toks=re.findall(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?", l)
    if len(toks)!=3:
        print(n, len(toks), repr(l))
