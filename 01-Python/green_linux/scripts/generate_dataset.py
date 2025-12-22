"""Generate a dataset NPZ file from simulated series for local training/evaluation.

Usage: python scripts/generate_dataset.py --out datasets/windows.npz --n-series 200 --length 48 --window-hours 2
"""
import argparse
from pathlib import Path

from powerapp.ml.dataset import generate_simulated_series, extract_window_examples, save_dataset_npz


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--out', default='datasets/windows.npz')
    p.add_argument('--n-series', default=200, type=int)
    p.add_argument('--length', default=48, type=int)
    p.add_argument('--window-hours', default=2, type=int)
    p.add_argument('--seed', default=None, type=int, help='Optional seed for deterministic generation')
    p.add_argument('--test-size', default=0.2, type=float, help='Fraction of examples to reserve for test split')
    p.add_argument('--train-out', default=None, help='Optional path to write train .npz')
    p.add_argument('--test-out', default=None, help='Optional path to write test .npz')
    p.add_argument('--calibration-out', default=None, help='Optional path to write calibration .npz (features only)')
    p.add_argument('--calibration-samples', default=200, type=int, help='Number of calibration samples to produce')
    args = p.parse_args()

    print('Generating simulated series...')
    series = generate_simulated_series(n_series=args.n_series, length=args.length, seed=args.seed)
    print('Extracting window examples...')
    X, y, app_keys = extract_window_examples(series, window_hours=args.window_hours)
    print(f'Examples: {len(X)}, features: {len(X[0]) if X else 0}')

    # Optionally produce a calibration dataset (sampled window features) and save it
    if args.calibration_out:
        try:
            import numpy as _np
            import json as _json
        except Exception:
            raise SystemExit('numpy is required to produce calibration dataset')
        n = len(X)
        k = min(int(args.calibration_samples), n)
        # deterministic sampling using provided seed
        import random as _random
        idx = list(range(n))
        if args.seed is not None:
            _random.Random(args.seed).shuffle(idx)
        sampled = [X[i] for i in idx[:k]]
        arrX = _np.array(sampled, dtype=_np.float32)
        p = Path(args.calibration_out)
        p.parent.mkdir(parents=True, exist_ok=True)
        _np.savez_compressed(str(p), X=arrX, meta=_json.dumps({'app_keys': app_keys, 'seed': args.seed, 'window_hours': args.window_hours}))
        print('Saved calibration dataset to', p)

    # Optionally split into train/test and save both
    if args.train_out or args.test_out:
        try:
            from sklearn.model_selection import train_test_split
            use_sklearn = True
        except Exception:
            use_sklearn = False

        test_size = float(args.test_size)
        if use_sklearn:
            X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=args.seed)
        else:
            # deterministic simple split
            n = len(X)
            idx = list(range(n))
            if args.seed is not None:
                import random as _random
                _random.Random(args.seed).shuffle(idx)
            split_at = int(n * (1 - test_size))
            train_idx = idx[:split_at]
            test_idx = idx[split_at:]
            X_train = [X[i] for i in train_idx]
            y_train = [y[i] for i in train_idx]
            X_test = [X[i] for i in test_idx]
            y_test = [y[i] for i in test_idx]

        if args.train_out:
            p = Path(args.train_out)
            p.parent.mkdir(parents=True, exist_ok=True)
            save_dataset_npz(str(p), X_train, y_train, app_keys=app_keys)
            print('Saved train dataset to', p)
        if args.test_out:
            p = Path(args.test_out)
            p.parent.mkdir(parents=True, exist_ok=True)
            save_dataset_npz(str(p), X_test, y_test, app_keys=app_keys)
            print('Saved test dataset to', p)
    else:
        out = save_dataset_npz(args.out, X, y, app_keys=app_keys)
        print('Saved dataset to', out)


if __name__ == '__main__':
    main()
