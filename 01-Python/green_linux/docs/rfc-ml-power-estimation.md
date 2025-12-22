RFC: On-device ML for best-window prediction & per-app power estimation

Status: Draft
Date: 2025-12-16

Overview
--------
Add a privacy-preserving on-device ML feature to PowerApp that: (1) predicts personalized best postponement windows using local history/forecasts and (2) provides per-app power estimates so suggestions can include targeted actions.

Goals
-----
- Improve suggestion personalization and impact by using local usage signals.
- Keep all inference local (no network calls by default).
- Keep model tiny and dependency-light (scikit-learn for training, lightweight inference API).
- Provide a robust heuristic baseline (no ML) and a path to safely enable learned models.
- Support optional, opt-in anonymized telemetry for aggregated improvement (explicit opt-in and privacy documentation required).

Constraints & Decisions
-----------------------
- Libraries: use scikit-learn for training / prototyping (decision tree or small gradient booster). Avoid heavy runtime deps for inference; prefer serializing models via joblib for local use.
- Runtime: inference must be cheap (< 50ms on typical laptop), low memory footprint.
- Privacy: default OFF for any telemetry; all model artifacts and inference data stay on device. Telemetry (if opted-in) uses aggregated, anonymized pings.

Data schema (simulated & collection)
-----------------------------------
- Time series resolution: hourly samples (configurable) for forecasting work.
- Per-sample fields:
  - timestamp (ISO)
  - location_zone (string)
  - forecast_intensity (gCO2/kWh)
  - total_power_w (float)
  - per_app_cpu_pct: {app_name: cpu_percent_float}
  - env_features: battery_level, on_ac_power, screen_brightness_pct
- Label/target for best-window prediction:
  - target_window_start (ISO) or score for each candidate window

Prototype plan
--------------
1. Implement a simulated dataset generator for training/evaluation (module: `powerapp.ml.dataset`).
2. Implement a heuristic per-app estimator (`powerapp.ml.heuristics`) as a lightweight baseline. This will be used by the UI immediately.
3. Implement training/experiment scripts that fit a small scikit-learn model (offline) and evaluate it on simulated data.
4. Add `predict_best_windows()` API in `powerapp.emissions` that can optionally call the local model, and falls back to heuristic scoring.
5. Add UI & simulator visuals to show model-suggested windows and per-app estimated impacts (opt-in feature flags in Settings).

Evaluation & Metrics
--------------------
- Accuracy: recall/precision for windows that yield lower predicted CO2 vs baseline.
- Savings estimate: mean predicted vs actual CO2 savings in simulation.
- Performance: inference latency and memory usage.

Telemetry & Privacy (opt-in)
----------------------------
- Telemetry collects only aggregated usage stats (e.g., counts of accepted suggestions and anonymized histogram of estimated savings). No raw timestamps, no identifiers.
- Provide explicit opt-in in Settings and a short privacy blurb describing aggregation and retention.

Next steps / immediate work
--------------------------
- Add `docs/rfc-ml-power-estimation.md` (this file). ✅
- Add prototype modules: `powerapp/ml/dataset.py`, `powerapp/ml/heuristics.py` and simple unit tests. (I will add these now.)
- Evaluate model choices with a small experiment and document results in this RFC.


Notes
-----
This RFC targets an MVP that can be iterated on based on evaluation and user feedback. The plan intentionally separates a deterministic heuristic baseline from learned models to allow safe rollout and easy A/B comparisons.