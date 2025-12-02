import re, os, sys, math

# Files flagged by the validator (from last run)
files = [
    'delta_convergence.csv',
    'delta_convergence_extended.csv',
    'fourier_integral_operator.csv',
    'functorial_semantics.csv',
    'hormander_calculus.csv',
    'hyperreals_calculus.csv',
    'nonlinear_functional_calculus.csv',
    'numerical_curvature.csv',
    'pseudodiff_symbol.csv',
    'rough_distributions.csv',
    'stiff_integrator_comparison.csv',
    'symplectic_higher_order.csv',
    'variational_bicomplex.csv',
    'white_noise_calculus.csv',
]

regex = re.compile(r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?")
alpha_re = re.compile(r"[A-Za-z]")

cwd = os.getcwd()

def try_read(path):
    try:
        with open(path, 'r', encoding='utf-8') as f:
            return f.readlines()
    except Exception:
        with open(path, 'r', encoding='latin-1') as f:
            return f.readlines()

print('Scanner working directory:', cwd)
print('\nScanning files:')

for fn in files:
    path = os.path.join(cwd, fn)
    print('\n----', fn, '----')
    if not os.path.isfile(path):
        print(' MISSING FILE:', path)
        continue
    lines = try_read(path)
    nlines = len(lines)
    print(' lines:', nlines)
    # extract tokens per line for entire file
    token_counts = []
    parse_fail_lines = []
    parse_nan_inf = False
    varied_counts = False
    counts_seen = {}
    max_show = 10
    show_lines = []
    for i, L in enumerate(lines):
        s = L.rstrip('\n')
        toks = regex.findall(s)
        token_counts.append(len(toks))
        counts_seen[len(toks)] = counts_seen.get(len(toks), 0) + 1
        if i < max_show:
            show_lines.append((i+1, s, toks))
        # try parse tokens to floats to detect NaN/Inf or invalid tokens
        for t in toks:
            try:
                v = float(t)
                if math.isnan(v) or math.isinf(v):
                    parse_nan_inf = True
            except Exception:
                parse_fail_lines.append((i+1, t))
    if len(set(token_counts)) > 1:
        varied_counts = True
    # classify
    if parse_nan_inf:
        classification = 'B (true numeric NaN/Inf)'
    elif parse_fail_lines:
        classification = 'A (formatting/parse failures)'
    elif varied_counts:
        classification = 'A (inconsistent numeric token counts per row)'
    else:
        classification = 'C (consistent numeric rows / likely acceptable)'
    print(' classification:', classification)
    print(' token-counts seen (count:occurrences):', counts_seen)
    if parse_fail_lines:
        print(' parse failures (first 8):', parse_fail_lines[:8])
    print('\n Example first %d lines and token lists:' % max_show)
    for lineno, raw, toks in show_lines:
        # mark if raw contains letters (e.g., 'fd2' or method names)
        has_alpha = bool(alpha_re.search(raw))
        print(f' {lineno:4d}: tokens={len(toks):2d} alpha={has_alpha} ->', toks)
    # also print a small sample of lines that have uncommon token counts (if any)
    if len(counts_seen) > 1:
        uncommon = [c for c in counts_seen if counts_seen[c] < max(counts_seen.values())]
        if uncommon:
            print('\n Lines with uncommon token-counts (sample):')
            for i, L in enumerate(lines[:200]):
                toks = regex.findall(L)
                if len(toks) in uncommon:
                    print(' ', i+1, 'count=', len(toks), 'raw=', L.strip()[:200])
                    # limit output
                    if i > 50:
                        break
    # quick heuristic: if file contains literal 'nan' or 'NaN' strings
    literal_nan = any('nan' in (L.lower()) for L in lines[:200])
    if literal_nan:
        print('\n Note: file contains literal "nan" text in the first 200 lines')
    # end

print('\nScan complete.')
