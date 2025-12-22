#!/usr/bin/env python3
"""Train a simple scikit-learn baseline model for window intensity prediction.

This script uses the simulated dataset generator to produce training examples of
candidate windows and trains a small regressor to predict the average intensity
of a candidate window (lower is better). The trained model is saved via joblib.
"""
import argparse
from pathlib import Path
import joblib
import numpy as np

from powerapp.ml.dataset import generate_simulated_series

try:
    from sklearn.ensemble import RandomForestRegressor
    from sklearn.model_selection import train_test_split
    from sklearn.metrics import mean_squared_error
except Exception:
    raise SystemExit("scikit-learn is required to run this script. Install scikit-learn and try again.")


def extract_window_examples(series_list, window_hours=2):
    X = []
    y = []
    for s in series_list:
        series = s['series']
        n = len(series)
        for i in range(0, max(0, n - window_hours + 1)):
            window = series[i:i+window_hours]
            # features: window start hour, avg forecast intensity, mean total power, per-app mean CPU
            start_hour = int(window[0]['timestamp'].split('T')[1].split(':')[0])
            avg_forecast = float(sum(p['forecast_intensity'] for p in window) / len(window))
            mean_power = float(sum(p['total_power_w'] for p in window) / len(window))
            # flatten per-app means in sorted key order to keep deterministic shape
            app_keys = sorted(window[0]['per_app_cpu'].keys())
            per_app_means = [float(sum(p['per_app_cpu'].get(k, 0.0) for p in window) / len(window)) for k in app_keys]
            feat = [start_hour, avg_forecast, mean_power] + per_app_means
            X.append(feat)
            y.append(avg_forecast)
    return np.array(X), np.array(y)


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--out', default='models/window_predictor.joblib', help='Output path for trained model (joblib)')
    p.add_argument('--n-series', default=200, type=int, help='Number of simulated series to generate')
    p.add_argument('--length', default=48, type=int, help='Hours per simulated series')
    p.add_argument('--window-hours', default=2, type=int, help='Window size in hours')
    p.add_argument('--n-estimators', default=50, type=int, help='RandomForest n_estimators')
    args = p.parse_args()

    print('Generating simulated dataset...')
    series = generate_simulated_series(n_series=args.n_series, length=args.length)
    print('Extracting window examples...')
    X, y = extract_window_examples(series, window_hours=args.window_hours)
    if X.shape[0] == 0:
        raise SystemExit('No training examples generated.')

    print(f'Examples: {X.shape[0]}, features: {X.shape[1]}')

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.15, random_state=42)
    print('Training RandomForestRegressor...')
    model = RandomForestRegressor(n_estimators=args.n_estimators, random_state=42)
    model.fit(X_train, y_train)

    y_pred = model.predict(X_test)
    mse = mean_squared_error(y_test, y_pred)
    print(f'Test MSE: {mse:.4f}')

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    joblib.dump({'model': model, 'app_keys': sorted(series[0]['series'][0]['per_app_cpu'].keys())}, str(out_path))
    print(f'Model saved to {out_path}')


if __name__ == '__main__':
    main()
