Design: Opt-in remote bug-report upload (privacy-first)

Goal
- Allow users to optionally upload anonymized diagnostics and logs to an operator server for triage and faster fixes.
- Preserve privacy: opt-in only, provide preview, allow selective file inclusion, anonymize PII, and provide retention/erase information.

User flow
1. User encounters a failure and is offered to generate a bug report bundle.
2. Dialog offers:
   - Preview of anonymized diagnostics JSON (expandable)
   - Checkbox: "Include logs"
   - Checkbox: "Upload anonymized bug report to support server (opt-in)" — off by default
   - Link to "What is included" and "How data is anonymized"
   - Consent button: "Upload" or Cancel
3. If upload chosen, show progress and result (success/fail). Offer to copy or save the bundle locally.

Payload & API
- POST /v1/bugreport
- Headers: Authorization: Bearer <token> (optional config), Content-Type: application/json
- Body: { "diagnostics": { ...anonymized... }, "meta": { "app_version": "x.y.z", "os": "..." }, "logs_included": true }
- Server returns: {"id": "<server id>", "url": "https://.../report/<id>"}

Anonymization rules (must be conservative)
- Replace email addresses with <REDACTED_EMAIL>
- Replace IPs (IPv4/IPv6) with <REDACTED_IP>
- Replace absolute user paths: /home/<user>/... -> /home/<REDACTED>/...
- Replace full usernames in known fields (username, user) with <REDACTED_USER>
- Replace UUIDs, MAC addresses with <REDACTED>
- Truncate long tokens (e.g., tokens with many characters) and replace with <REDACTED_TOKEN>
- Remove or redact content keys that are known sensitive: tokens, secrets, passwords
- For logs: strip stack traces down to exception class and message; remove lines that match email/IP patterns

Testing strategy
- Unit tests for anonymizer: ensure emails/IPs/home paths are replaced and other information preserved.
- Tests ensure no PII posted when enabled (mock HTTP post and inspect payload).
- Tests for opt-in behavior: upload is not attempted when disabled.
- Headful UI tests for preview dialog and consent flow (later).

Retention & transparency
- Include details in the privacy dashboard about what's collected and retention policy.
- Allow users to delete uploaded reports (server-side API; provide an ID to user).

Security
- Use HTTPS and server auth when configured.
- Do not attempt upload if no upload URL is configured.
- Provide retry/backoff and clear UI for failures.

Notes
- Implementation is incremental: build anonymizer + tests first, then UI integration, then server integration and telemetry/ID tracking.
