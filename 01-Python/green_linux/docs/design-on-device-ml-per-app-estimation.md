Title: On-Device ML for Best-Window Prediction & Per-App Power Estimation
Authors: Robert Marshall
Date: 2025-12-16
Status: Draft

## Summary

This document specifies the design for adding an **on-device machine learning (ML) feature** to the app that: (1) predicts the best postponement windows for user tasks (``best-window prediction``), and (2) provides **per-application power estimates** to help attribute potential savings across apps. The design emphasizes privacy (on-device inference, opt-in telemetry), fallback heuristics, small models, and UX controls.

---

## Goals

- Provide better postponement suggestions by using an on-device model that predicts low-carbon windows with higher fidelity than the simple intensity-only heuristics.
- Offer per-app power estimates to show users which application(s) consume more energy during the task and how postponement impacts them.
- Keep user privacy as first priority: models run locally; no raw telemetry leaves device unless user explicitly opts in.
- Ensure graceful fallback to deterministic heuristics when model unavailable or disabled.
- Keep compute, memory, and storage footprint small (target: < 5MB model; inference <100ms on typical laptop).
- Provide instrumentation and opt-in telemetry for aggregated, privacy-conscious evaluation (opt-in only).

## Non-Goals

- Replace the heuristic estimator entirely at launch — initial rollout will be opt-in and feature-flagged with fallback.
- Provide aggressive on-device training; initial design focuses on inference with a small, pre-trained model.

## Components

1. Dataset & Feature Schema
   - Input time window (hours): a forecasted intensity series for window candidates (e.g., 2–6 hour windows).
   - Candidate features per-window (examples):
     - avg_intensity, min_intensity, variance, percentile(10/90)
     - hour_of_day distribution, day_of_week
     - forecast trend (slope) and seasonality indicators
     - per-app CPU shares/time series summary (mean, max, std) for the task's runtime
     - estimated_total_power_w (heuristic) — optional contextual feature
     - historical baseline and recent anomalies
   - Label: expected carbon delta of postponing into the window (kgCO2), or relative score to rank windows.

2. Training & Evaluation
   - Synthetic dataset generator (existing scripts/dataset) to bootstrap training (simulate many app mixes and power draws).
   - Produce a compact training set of window examples; train models (RandomForest or small lightGBM/xgboost) offline.
   - Evaluation metrics: NDCG / ranking accuracy for top-k windows, RMSE on predicted delta CO2 where applicable.
   - Model size & latency profiling — target thresholds for inclusion in release.

3. Model Format & Storage
   - Persist models as joblib or ONNX for compact portability.
   - Default model path: XDG_CACHE_HOME/powerapp/models/window_predictor.joblib
   - Users may specify `ml_model_path` in Settings (already present); app checks this first, then default cache path.

4. Runtime Inference API
   - New function: predict_best_windows(forecast, candidate_windows, per_app_series=None, model_path=None, **opts)
     - Loads model lazily (cache model object in-memory per-launch session).
     - Accepts a list of window candidates (start,end,features) and returns top_k ranked windows with predicted scores.
     - If model_path absent or model fails to load, fall back to find_low_carbon_windows heuristic.
   - Integration: augment_suggestions_with_best_window(..., use_model=False, model_path=None)
     - If `use_model=True` and model available, call predict_best_windows and annotate suggestions with model picks and predicted impact.

5. Per-App Power Estimation
   - Deterministic baseline algorithm (already implemented in heuristics): estimate_total_power(per_app_cpu, base_power, app_coeffs) and split_app_power(estimated_total, per_app_cpu)
   - Use per-app series to compute per-app contributions over candidate windows and present aggregate numbers in the UI (task-level energy_kwh and per-app shares).
   - Optionally include per-app features in ML model (per-app aggregated features) to improve predicted savings attribution.

   Per-app series contract (runtime schema):
   - `per_app_series` is an optional list of samples, each sample is a dict:
     - `timestamp`: ISO-8601 string aligned to hourly forecast timestamps (e.g. `2025-12-17T02:00:00+00:00`).
     - `per_app_cpu`: mapping `app_name -> cpu_percent` (float, 0.0-100.0). Example: `{'chrome': 12.5, 'editor': 4.2}`.
   - The runtime predictor will compute per-window aggregates (mean, optionally max/std) for each `app_name` appearing in the model's `app_keys` and will default to 0.0 when data missing for a window.
   - Timestamps should match or be mappable (same timezone/format) as forecast timestamps; best results when aligned to the same hourly bins used in `forecast`.
   - Privacy note: `per_app_series` data is used locally for on-device inference and is never uploaded unless the user explicitly opts-in to telemetry; summary aggregates (model version, latency, hit rates) can be reported only with consent.

6. UI / UX
   - Settings: opt-in check (``Enable ML best-window suggestions``) and optional `Model path` field.
   - Privacy blurb in Settings (docs/PRIVACY.md link) describing on-device inference and opt-in telemetry.
   - Suggestion dialogs: show which window is model-recommended (badge/label), present estimated carbon savings (kgCO2), and optionally break down per-app contributions.
   - Simulator: allow toggling between heuristic and ML mode and visualize ML-selected windows and per-app impacts.

### Explainability & Counterfactuals
- The Simulator now includes a **Why this window?** panel that shows a concise list of top contributing features (via `powerapp.ml.explain.explain_window_choice`) with normalized importance bars and per-feature descriptions. For quick exploration, the panel presents a short list of alternative candidate windows (counterfactuals) with **Preview** buttons that update the simulator view to show expected savings for that alternative. This enhances transparency and supports user trust and higher suggestion acceptance rates.
   - Local logs may be retained for debugging; clearly disclosed in privacy doc.

8. Performance & Safety
   - Model size limit: aim for < 5MB compressed.
   - Inference latency limit: < 100ms on typical modern laptop; if not met, fall back or use lighter model.
   - Fallbacks for model load failure or missing dependencies: continue using heuristics; UI not blocked.

9. Testing Strategy
   - Unit tests: feature extraction correctness, predict_best_windows results with deterministic stub model, and per-app split correctness.
   - Integration tests: headful tests flipping the Settings checkbox and asserting augment_suggestions_with_best_window uses `use_model` and `model_path` correctly (already in test suite).
   - Model acceptance tests: run small evaluation script against synthetic validation set and assert ranking metrics exceed baseline thresholds before promoting model into default release.

10. Release & Rollout Plan
    - Phase 1 (experimental): feature behind Settings opt-in, default OFF. Ship with heuristics enabled by default.
    - Phase 2 (opt-in trials): collect opt-in telemetry (if user consents) to evaluate model accuracy and latency across devices.
    - Phase 3 (GA): if results show benefit and resource constraints acceptable, enable model by default with a clearly documented privacy guarantee and an easy opt-out.

---

## Implementation Notes & Open Questions
- Model choice: RandomForestRegressor suffices for initial prototype; if more compactness needed, consider LightGBM with small leaf counts, or convert to ONNX and apply quantization.
- Feature engineering: need to standardize per-app series aggregation (mean, peak, percentiles) and a feature pipeline consistent with offline training and runtime code.
- Model update path: how will we ship model updates? Options: include in releases, allow user-specified path, or provide secure optional model download (with consent).
- Telemetry design: what exact metrics do we want to collect under opt-in? Propose: model_version, inference_latency_ms (p50,p95), top1_hit (whether model top pick matched the offline top pick per held-out ground truth), and sample rate.
- Security: verify models are loaded from trusted paths only; avoid executing arbitrary code (joblib loading is generally safe for scikit-learn models but specify safe model provenance).

## Implementation Plan (high-level tasks)
1. Finalize feature schema and extend dataset generator to emit window-level examples with per-app aggregated features. (owner: dev)
2. Train prototype models offline; store one or more models in `models/` and run evaluation. (owner: dev)
3. Implement `predict_best_windows()` and integrate into `augment_suggestions_with_best_window()` behind opt-in flag. Add caching & lazy-load. (owner: dev)
4. Add Settings UI controls, privacy blurb, and tests (unit + headful). (owner: dev)
5. Add model acceptance tests and resource checks (size, latency) to the CI or a gating script. (owner: dev)
6. Prepare release notes, privacy text, and changelog entry. (owner: docs/dev)

---

## Appendix: Minimal API Example

# pseudo-code
# model = load_model(model_path)
# features = extract_features_for_window(window_candidate, task_metadata, per_app_series)
# score = model.predict(features)

predict_best_windows(forecast, candidate_windows, per_app_series=None, model_path=None, top_k=3)

Return: list of windows annotated with predicted_score and predicted_savings_kg_co2
