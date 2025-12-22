#!/usr/bin/env python3
"""Compare per-app bar metadata JSON files: expected vs actual.

Usage: compare_visual_meta.py expected.json actual.json [--frac-tol 0.05] [--color-tol 0.1]
Returns 0 if similar, non-zero otherwise.
"""
import sys
import json

def color_dist(a, b):
    return sum(abs(x-y) for x,y in zip(a,b))


def main():
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('expected')
    p.add_argument('actual')
    p.add_argument('--frac-tol', type=float, default=0.05)
    p.add_argument('--color-tol', type=float, default=0.12)
    args = p.parse_args()

    e = json.load(open(args.expected))
    a = json.load(open(args.actual))
    e_bars = e.get('per_app_bars', [])
    a_bars = a.get('per_app_bars', [])

    # Require same set of apps (order may vary)
    e_map = {b['app']: b for b in e_bars}
    a_map = {b['app']: b for b in a_bars}

    missing = set(e_map) - set(a_map)
    extra = set(a_map) - set(e_map)
    if missing or extra:
        print('App set mismatch. Missing:', missing, 'Extra:', extra)
        return 2

    # Compare per-app fractions and color similarity
    for app, eb in e_map.items():
        ab = a_map[app]
        ef = float(eb.get('fraction', 0.0))
        af = float(ab.get('fraction', 0.0))
        if abs(ef - af) > args.frac_tol:
            print(f'Fraction mismatch for {app}: expected={ef:.3f} actual={af:.3f} tol={args.frac_tol}')
            return 3
        ec = tuple(eb.get('color', (0.0,0.0,0.0)))
        ac = tuple(ab.get('color', (0.0,0.0,0.0)))
        if color_dist(ec, ac) > args.color_tol:
            print(f'Color mismatch for {app}: expected={ec} actual={ac} dist={color_dist(ec,ac):.3f} tol={args.color_tol}')
            return 4
        # optional KWH/CO2 checks
        ek = eb.get('kwh')
        ak = ab.get('kwh')
        if ek is not None and ak is not None:
            try:
                ekf = float(ek)
                akf = float(ak)
                if abs(ekf - akf) > max(1e-6, args.frac_tol * max(ekf, akf, 1.0)):
                    print(f'KWh mismatch for {app}: expected={ekf:.4f} actual={akf:.4f}')
                    return 5
            except Exception:
                pass
        eco = eb.get('co2_kg')
        aco = ab.get('co2_kg')
        if eco is not None and aco is not None:
            try:
                eco_f = float(eco)
                aco_f = float(aco)
                if abs(eco_f - aco_f) > max(1e-6, args.frac_tol * max(abs(eco_f), abs(aco_f), 1.0)):
                    print(f'CO2 mismatch for {app}: expected={eco_f:.4f} actual={aco_f:.4f}')
                    return 6
            except Exception:
                pass
        # optional centroid comparisons (pixel->fraction ratio independent)
        ecf = eb.get('centroid_frac')
        acf = ab.get('centroid_frac')
        if ecf is not None and acf is not None:
            try:
                if abs(float(ecf) - float(acf)) > 0.05:
                    print(f'Centroid fraction mismatch for {app}: expected={ecf} actual={acf}')
                    return 7
            except Exception:
                pass

    print('Metadata compare: OK')
    return 0

if __name__ == '__main__':
    sys.exit(main())
