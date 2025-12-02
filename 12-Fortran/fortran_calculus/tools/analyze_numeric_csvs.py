import re
import sys
from pathlib import Path

FILES = [
    'delta_convergence.csv',
    'delta_convergence_extended.csv',
    'functorial_semantics.csv',
    'hyperreals_calculus.csv',
    'nonlinear_functional_calculus.csv',
    'numerical_curvature.csv',
    'pseudodiff_symbol.csv',
    'rough_distributions.csv',
    'stiff_integrator_comparison.csv',
    'symplectic_higher_order.csv',
    'variational_bicomplex.csv',
    'white_noise_calculus.csv'
]
NUM_RE = re.compile(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?")
BROKEN_EXP_RE = re.compile(r"(\d)([-+])(\d{1,3})$")


def fix_malformed_exp(tok):
    # If token looks like 1.23-306 (missing E), insert E
    if re.search(r'[eE]', tok):
        return tok
    m = re.search(r'([0-9])(\-\d{1,3})$', tok)
    if m:
        return tok[:-len(m.group(2))] + 'E' + m.group(2)
    return tok


def analyze_file(p):
    stats = {
        'lines': 0,
        'numeric_tokens': 0,
        'parsed': 0,
        'nan': 0,
        'inf': 0,
        'finite_count': 0,
        'min': None,
        'max': None,
        'bad_tokens': {},
    }
    sample_rows = []
    with open(p, 'r', encoding='utf-8', errors='ignore') as f:
        for i, line in enumerate(f, start=1):
            stats['lines'] += 1
            toks = re.findall(r"[^,\s]+", line.strip())
            nums = []
            for t in toks:
                if '***' in t:
                    stats['bad_tokens'].setdefault('asterisk', 0)
                    stats['bad_tokens']['asterisk'] += 1
                    continue
                # Try numeric regex first
                m = NUM_RE.search(t)
                if not m:
                    # maybe malformed exponent like 2.156696-306
                    if re.search(r"\d[-+]\d{1,3}$", t) and not re.search(r'[eE]', t):
                        fixed = fix_malformed_exp(t)
                        try:
                            v = float(fixed)
                            stats['parsed'] += 1
                            nums.append(v)
                        except Exception:
                            stats['bad_tokens'].setdefault('malformed_exp', 0)
                            stats['bad_tokens']['malformed_exp'] += 1
                    else:
                        stats['bad_tokens'].setdefault('non_numeric', 0)
                        stats['bad_tokens']['non_numeric'] += 1
                    continue
                else:
                    tok = m.group(0)
                    tok = fix_malformed_exp(tok)
                    stats['numeric_tokens'] += 1
                    try:
                        v = float(tok)
                        stats['parsed'] += 1
                        if v != v:
                            stats['nan'] += 1
                        elif v == float('inf') or v == float('-inf'):
                            stats['inf'] += 1
                        else:
                            stats['finite_count'] += 1
                            if stats['min'] is None or v < stats['min']:
                                stats['min'] = v
                            if stats['max'] is None or v > stats['max']:
                                stats['max'] = v
                            nums.append(v)
                    except Exception:
                        stats['bad_tokens'].setdefault('parse_fail', 0)
                        stats['bad_tokens']['parse_fail'] += 1
            if i <= 5:
                sample_rows.append((i, line.strip(), nums[:10]))
    return stats, sample_rows


def main():
    base = Path.cwd()
    for fn in FILES:
        p = base / fn
        if not p.exists():
            print(f"Missing: {fn}")
            continue
        s, sample = analyze_file(p)
        print('\n----', fn, '----')
        print('lines:', s['lines'])
        print('numeric token candidates:', s['numeric_tokens'])
        print('parsed tokens:', s['parsed'])
        print('finite count:', s['finite_count'])
        print('NaN count:', s['nan'], 'Inf count:', s['inf'])
        if s['min'] is not None:
            print('min:', s['min'], 'max:', s['max'])
        if s['bad_tokens']:
            print('bad token categories:', s['bad_tokens'])
        print('sample rows:')
        for rno, raw, nums in sample:
            print(f" {rno}: {raw} -> {nums}")

if __name__ == '__main__':
    main()
