import re

inf='burgers_snapshots.csv'
outf='burgers_snapshots_clean.csv'
num_re=re.compile(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?")

with open(inf,'rb') as fh:
    raw=fh.read()
try:
    text=raw.decode('utf-8')
except Exception:
    text=raw.decode('latin-1')
lines=text.splitlines()
header=None
out_lines=[]
for ln in lines:
    if not ln.strip():
        continue
    if header is None:
        header=ln.strip()
        out_lines.append(header)
        continue
    toks=num_re.findall(ln)
    if len(toks)==3:
        # normalize spacing: t,i,u
        t,i,v=toks
        out_lines.append(f"{t},{i},{v}")

with open(outf,'w',encoding='utf-8') as fh:
    fh.write('\n'.join(out_lines))

print('Wrote', outf)
