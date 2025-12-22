"""Train a small model on simulated window examples and evaluate ranking vs heuristic baseline.

Outputs a short report of NDCG@k and top-1 accuracy for model vs heuristic.

Usage:
  python scripts/evaluate_model.py --n-series 200 --length 48 --window-hours 2 --top-k 3 --model-out models/window_eval.joblib
"""
import argparse
import json
from pathlib import Path
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
import joblib

from powerapp.ml.dataset import generate_simulated_series, extract_window_examples

try:
    from sklearn.metrics import ndcg_score
    _HAS_NDCG = True
except Exception:
    _HAS_NDCG = False


def ndcg_at_k(true_vals, pred_scores, k=3):
    """Compute NDCG@k where lower true_vals are better (e.g., intensity)."""
    # Convert true_vals to relevance where higher is better
    # relevance = max - val
    true_vals = np.array(true_vals)
    rel = np.max(true_vals) - true_vals
    # predicted scores: lower predicted intensity is better, so invert
    pred_scores = np.array(pred_scores)
    if _HAS_NDCG:
        return ndcg_score([rel], [-pred_scores], k=k)
    # fallback implementation
    order_pred = np.argsort(pred_scores)[:k]
    order_ideal = np.argsort(true_vals)[:k]
    def dcg(rels):
        return sum((2**r - 1) / np.log2(i+2) for i, r in enumerate(rels))
    pred_rels = rel[order_pred]
    ideal_rels = rel[order_ideal]
    idcg = dcg(ideal_rels)
    if idcg == 0:
        return 1.0 if dcg(pred_rels) == 0 else 0.0
    return dcg(pred_rels) / idcg


def evaluate(n_series=200, length=48, window_hours=2, top_k=3, n_estimators=50, random_state=42):
    # generate series
    series = generate_simulated_series(n_series=n_series, length=length)
    # extract windows but keep grouping per series
    series_windows = []  # list of tuples (X_list, y_list)
    for s in series:
        X, y, app_keys = extract_window_examples([s], window_hours=window_hours)
        series_windows.append((X, y))
    # split series into train/test
    idx = np.arange(len(series_windows))
    train_idx, test_idx = train_test_split(idx, test_size=0.2, random_state=random_state)

    # flatten train windows
    X_train = []
    y_train = []
    for i in train_idx:
        X_train.extend(series_windows[i][0])
        y_train.extend(series_windows[i][1])
    X_train = np.array(X_train)
    y_train = np.array(y_train)

    # train model
    model = RandomForestRegressor(n_estimators=n_estimators, random_state=random_state)
    model.fit(X_train, y_train)

    # evaluate on test series
    ndcgs_model = []
    ndcgs_heur = []
    top1_model = 0
    top1_heur = 0
    n_total = 0

    for i in test_idx:
        X_list, y_list = series_windows[i]
        if not X_list:
            continue
        X_arr = np.array(X_list)
        y_arr = np.array(y_list)
        preds = model.predict(X_arr)
        # heuristic uses avg forecast (y_arr) as predicted intensity (lower better)
        heur_scores = y_arr.copy()
        # compute ndcg @ top_k
        ndm = ndcg_at_k(y_arr, preds, k=top_k)
        ndh = ndcg_at_k(y_arr, heur_scores, k=top_k)
        ndcgs_model.append(ndm)
        ndcgs_heur.append(ndh)
        # top1 accuracy
        best_true = int(np.argmin(y_arr))
        best_model = int(np.argmin(preds))
        best_heur = int(np.argmin(heur_scores))
        top1_model += 1 if best_model == best_true else 0
        top1_heur += 1 if best_heur == best_true else 0
        n_total += 1

    avg_ndcg_model = float(np.mean(ndcgs_model)) if ndcgs_model else 0.0
    avg_ndcg_heur = float(np.mean(ndcgs_heur)) if ndcgs_heur else 0.0
    top1_model_pct = float(top1_model) / n_total if n_total else 0.0
    top1_heur_pct = float(top1_heur) / n_total if n_total else 0.0

    metrics = {
        'ndcg_model': avg_ndcg_model,
        'ndcg_heuristic': avg_ndcg_heur,
        'top1_model': top1_model_pct,
        'top1_heuristic': top1_heur_pct,
        'n_test_series': int(n_total)
    }
    return model, metrics, app_keys


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--n-series', default=200, type=int)
    p.add_argument('--length', default=48, type=int)
    p.add_argument('--window-hours', default=2, type=int)
    p.add_argument('--top-k', default=3, type=int)
    p.add_argument('--n-estimators', default=50, type=int)
    p.add_argument('--model-out', default='models/window_eval.joblib')
    p.add_argument('--report-out', default=None)
    args = p.parse_args()

    model, metrics, app_keys = evaluate(n_series=args.n_series, length=args.length, window_hours=args.window_hours, top_k=args.top_k, n_estimators=args.n_estimators)
    print('Evaluation metrics:')
    for k, v in metrics.items():
        print(f'  {k}: {v}')

    outp = Path(args.model_out)
    outp.parent.mkdir(parents=True, exist_ok=True)
    joblib.dump({'model': model, 'app_keys': app_keys}, str(outp))
    print('Saved model to', outp)

    if args.report_out:
        with open(args.report_out, 'w', encoding='utf-8') as fh:
            json.dump({'metrics': metrics}, fh, indent=2)
        print('Saved report to', args.report_out)


if __name__ == '__main__':
    import argparse
    main()
