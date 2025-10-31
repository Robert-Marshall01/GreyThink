#!/usr/bin/env python3
# SPDX-License-Identifier: MIT
"""
tools/similarity_scan.py

Token-based k-gram similarity scanner using winnowing (Rabin-style hashing + window minima).

Usage: run from the repository root (script handles defaults):
  python tools/similarity_scan.py --repo-root . --upstream upstream_bash --out similarity_report.json

It finds files with extensions .c, .cpp, .h, .sh, .py, .txt, .md in the repo (excluding the upstream dir),
builds fingerprints for the upstream corpus, then compares each repo file and reports matches.

Output: JSON report with per-file matches and basic metrics.

This is intended as an automated aid; for legal assurance, consult counsel.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
from collections import defaultdict
from typing import Dict, Iterable, List, Sequence, Tuple


def list_source_files(root: str, exts=None, exclude_dirs=None) -> List[str]:
	if exts is None:
		exts = {'.c', '.cpp', '.h', '.sh', '.py', '.txt', '.md'}
	if exclude_dirs is None:
		exclude_dirs = {'upstream_bash', '.git', '__pycache__'}
	files = []
	for dirpath, dirnames, filenames in os.walk(root):
		# skip excluded dirs
		dirnames[:] = [d for d in dirnames if d not in exclude_dirs]
		for fn in filenames:
			_, e = os.path.splitext(fn)
			if e.lower() in exts:
				files.append(os.path.join(dirpath, fn))
	return sorted(files)


def read_text(path: str) -> str:
	try:
		with open(path, 'r', encoding='utf-8', errors='ignore') as f:
			return f.read()
	except Exception:
		return ''


def strip_comments_and_tokenize(text: str) -> List[str]:
	# Remove C-style /* */ and C++ // comments and shell-style # comments.
	# This is a heuristic, good enough for similarity scanning.
	# Remove multiline C comments
	text = re.sub(r'/\*.*?\*/', ' ', text, flags=re.S)
	# Remove // comments
	text = re.sub(r'//.*?$', ' ', text, flags=re.M)
	# Remove # comments at line start or after whitespace (but keep shebang)
	text = re.sub(r'(^|\n)\s*#(?!\!).*?(?=\n|$)', '\n', text)
	# Normalize whitespace
	text = re.sub(r'\s+', ' ', text)
	# Tokenize on non-alphanumeric (keep _)
	tokens = re.findall(r"[A-Za-z0-9_]+", text)
	return tokens


def kgrams(tokens: Sequence[str], k: int) -> List[Tuple[int, Tuple[str, ...]]]:
	out = []
	if len(tokens) < k:
		return out
	for i in range(len(tokens) - k + 1):
		kg = tuple(tokens[i:i + k])
		# Use md5 for deterministic stable hash across runs
		h = hashlib.md5((' '.join(kg)).encode('utf-8')).hexdigest()
		out.append((i, h))
	return out


def winnow(hashes: List[Tuple[int, str]], window: int) -> Dict[str, List[int]]:
	"""Return mapping hash->list of chosen positions using winnowing (choose min hash in each window)."""
	selected: Dict[str, List[int]] = defaultdict(list)
	if not hashes:
		return selected
	n = len(hashes)
	w = max(1, window)
	for i in range(n - w + 1):
		window_slice = hashes[i:i + w]
		# choose minimum hex string; tie-breaker: rightmost
		min_h = min(h for _, h in window_slice)
		# find positions within window with that min (could be multiple); record their absolute pos
		for pos, h in window_slice:
			if h == min_h:
				selected[h].append(pos)
	return selected


def build_upstream_index(upstream_files: List[str], k: int, w: int) -> Dict[str, List[Tuple[str, int]]]:
	index: Dict[str, List[Tuple[str, int]]] = defaultdict(list)
	for path in upstream_files:
		text = read_text(path)
		tokens = strip_comments_and_tokenize(text)
		kg = kgrams(tokens, k)
		fp = winnow(kg, w)
		for h, positions in fp.items():
			for p in positions:
				index[h].append((path, p))
	return index


def compare_file(path: str, k: int, w: int, upstream_index: Dict[str, List[Tuple[str, int]]]) -> Dict:
	text = read_text(path)
	tokens = strip_comments_and_tokenize(text)
	kg = kgrams(tokens, k)
	fp = winnow(kg, w)
	total_kgrams = max(1, len(kg))
	hits: Dict[str, Dict] = {}
	for h, positions in fp.items():
		if h in upstream_index:
			for up_path, up_pos in upstream_index[h]:
				key = up_path
				if key not in hits:
					hits[key] = {'match_kgram_positions_repo': [], 'match_kgram_positions_upstream': [], 'count': 0}
				# record repo positions and upstream positions (we'll only append first upstream pos per occurrence)
				hits[key]['match_kgram_positions_repo'].extend(positions)
				hits[key]['match_kgram_positions_upstream'].append(up_pos)
				hits[key]['count'] += len(positions)

	# produce sorted matches with basic metrics
	matches = []
	for up_path, data in hits.items():
		unique_repo_positions = sorted(set(data['match_kgram_positions_repo']))
		match_count = len(unique_repo_positions)
		pct = match_count / total_kgrams * 100.0
		matches.append({'upstream_file': os.path.relpath(up_path),
						'match_kgrams': match_count,
						'percent_of_file_kgrams': round(pct, 3),
						'repo_kgram_positions_sample': unique_repo_positions[:10],
						'upstream_positions_sample': data['match_kgram_positions_upstream'][:10]})

	matches.sort(key=lambda x: -x['match_kgrams'])
	return {'path': os.path.relpath(path), 'total_kgrams': total_kgrams, 'matches': matches}


def main(argv: List[str] | None = None) -> int:
	p = argparse.ArgumentParser(description='Similarity scan (k-gram + winnowing)')
	p.add_argument('--repo-root', default=os.path.abspath(os.path.join(os.path.dirname(__file__), '..')),
				   help='Repository root path')
	p.add_argument('--upstream', default='upstream_bash', help='Upstream corpus directory (relative to repo-root)')
	p.add_argument('--k', type=int, default=10, help='k-gram size (tokens)')
	p.add_argument('--w', type=int, default=4, help='winnowing window size')
	p.add_argument('--out', default=None, help='Output JSON file path')
	args = p.parse_args(argv)

	repo_root = os.path.abspath(args.repo_root)
	upstream_dir = os.path.join(repo_root, args.upstream)
	out_path = args.out or os.path.join(repo_root, 'similarity_report.json')

	print(f'Repo root: {repo_root}')
	print(f'Upstream dir: {upstream_dir}')
	print(f'k={args.k} w={args.w} -> output {out_path}')

	source_files = list_source_files(repo_root)
	# exclude files inside upstream dir
	source_files = [f for f in source_files if os.path.commonpath([repo_root, f]) == repo_root and args.upstream not in f]

	upstream_files = []
	if os.path.isdir(upstream_dir):
		upstream_files = list_source_files(upstream_dir, exts={'.c', '.cpp', '.h', '.sh', '.py', '.txt', '.md'})

	print(f'Found {len(source_files)} repo files, {len(upstream_files)} upstream files')

	# Build upstream index
	print('Building upstream fingerprint index...')
	upstream_index = build_upstream_index(upstream_files, args.k, args.w)
	print(f'Index contains {len(upstream_index)} unique fingerprint hashes')

	report = {'repo_root': repo_root, 'k': args.k, 'w': args.w, 'files': []}
	for i, path in enumerate(source_files):
		print(f'[{i+1}/{len(source_files)}] Scanning {os.path.relpath(path, repo_root)}')
		try:
			r = compare_file(path, args.k, args.w, upstream_index)
			# keep only matches above a tiny threshold (e.g., >0.5% of kgrams)
			r['matches'] = [m for m in r['matches'] if m['percent_of_file_kgrams'] >= 0.5]
			if r['matches']:
				report['files'].append(r)
		except Exception as e:
			print(f'Error scanning {path}: {e}')

	# write report
	with open(out_path, 'w', encoding='utf-8') as f:
		json.dump(report, f, indent=2)

	print(f'Done. Report written to {out_path}')
	return 0


if __name__ == '__main__':
	raise SystemExit(main())
