#!/usr/bin/env python3
"""Compare two PNG files using a simple pixel-difference threshold.

Usage: compare_pngs.py expected.png actual.png [--threshold N] [--out-diff diff.png]
Exits with 0 if images are similar (diff < threshold), non-zero otherwise.
"""
import sys
from PIL import Image, ImageChops

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('expected')
    parser.add_argument('actual')
    parser.add_argument('--threshold', type=int, default=5000)
    parser.add_argument('--out-diff', default=None)
    args = parser.parse_args()

    exp = Image.open(args.expected).convert('RGBA')
    act = Image.open(args.actual).convert('RGBA')
    if exp.size != act.size:
        print(f"Size mismatch: expected {exp.size}, actual {act.size}")
        if args.out_diff:
            act.save(args.out_diff)
        return 2
    diff = ImageChops.difference(exp, act)
    bw = diff.convert('L')
    stat = sum(bw.getdata())
    print(f"Pixel diff sum: {stat} (threshold {args.threshold})")
    if stat > args.threshold:
        if args.out_diff:
            diff.save(args.out_diff)
        return 1
    return 0

if __name__ == '__main__':
    sys.exit(main())
