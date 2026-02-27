import React, { useState } from "react";
import { api } from "../api";

export function QueryPage() {
    const [sql, setSql] = useState("SELECT * FROM pg_tables WHERE schemaname = 'public' LIMIT 20;");
    const [tenantId, setTenantId] = useState("");
    const [result, setResult] = useState<any>(null);
    const [explainResult, setExplainResult] = useState<any>(null);
    const [tab, setTab] = useState<"results" | "explain">("results");
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");

    const execute = async () => {
        setLoading(true);
        setError("");
        setExplainResult(null);
        try {
            const data = await api.executeQuery(sql, [], tenantId || undefined);
            setResult(data);
            setTab("results");
        } catch (err: any) {
            setError(err.message);
            setResult(null);
        } finally {
            setLoading(false);
        }
    };

    const explain = async () => {
        setLoading(true);
        setError("");
        setResult(null);
        try {
            const data = await api.explainQuery(sql);
            setExplainResult(data);
            setTab("explain");
        } catch (err: any) {
            setError(err.message);
            setExplainResult(null);
        } finally {
            setLoading(false);
        }
    };

    return (
        <div>
            <div className="page-header">
                <h2>Query Console</h2>
                <p>Execute SQL, analyze plans, and explore data</p>
            </div>

            <div className="card">
                <div className="card-header"><h3>SQL Editor</h3></div>
                <div className="input-group">
                    <label>Tenant Context (optional)</label>
                    <input className="input" value={tenantId} onChange={(e) => setTenantId(e.target.value)} placeholder="tenant-uuid (leave empty for no tenant context)" style={{ maxWidth: 400 }} />
                </div>
                <textarea className="sql-editor" value={sql} onChange={(e) => setSql(e.target.value)} rows={8} spellCheck={false} />
                <div style={{ display: "flex", gap: 8, marginTop: 8 }}>
                    <button className="btn btn-primary" onClick={execute} disabled={loading || !sql.trim()}>
                        {loading ? "Running..." : "▶ Execute"}
                    </button>
                    <button className="btn btn-secondary" onClick={explain} disabled={loading || !sql.trim()}>
                        📊 Explain
                    </button>
                    <button className="btn" onClick={() => { setSql(""); setResult(null); setExplainResult(null); setError(""); }}>Clear</button>
                </div>
            </div>

            {error && (
                <div className="card" style={{ borderColor: "var(--danger)" }}>
                    <div className="card-header"><h3 style={{ color: "var(--danger)" }}>Error</h3></div>
                    <pre style={{ color: "var(--danger)", fontSize: 13, whiteSpace: "pre-wrap" }}>{error}</pre>
                </div>
            )}

            {(result || explainResult) && (
                <div className="card">
                    <div className="tabs">
                        <button className={`tab ${tab === "results" ? "tab-active" : ""}`} onClick={() => setTab("results")} disabled={!result}>Results</button>
                        <button className={`tab ${tab === "explain" ? "tab-active" : ""}`} onClick={() => setTab("explain")} disabled={!explainResult}>Explain Plan</button>
                    </div>

                    {tab === "results" && result && (
                        <div>
                            <div style={{ fontSize: 13, color: "var(--text-dim)", marginBottom: 8 }}>
                                {result.rowCount} row{result.rowCount !== 1 ? "s" : ""} returned
                                {result.duration != null && <span> · {result.duration}ms</span>}
                            </div>
                            {result.rows && result.rows.length > 0 ? (
                                <div style={{ overflowX: "auto" }}>
                                    <table className="data-table">
                                        <thead>
                                            <tr>{Object.keys(result.rows[0]).map((col) => <th key={col}>{col}</th>)}</tr>
                                        </thead>
                                        <tbody>
                                            {result.rows.map((row: any, i: number) => (
                                                <tr key={i}>
                                                    {Object.values(row).map((val: any, j: number) => (
                                                        <td key={j} className="mono" style={{ fontSize: 12 }}>
                                                            {val === null ? <span style={{ color: "var(--text-dim)" }}>NULL</span> : typeof val === "object" ? JSON.stringify(val) : String(val)}
                                                        </td>
                                                    ))}
                                                </tr>
                                            ))}
                                        </tbody>
                                    </table>
                                </div>
                            ) : (
                                <div className="empty-state"><p>Query executed. No rows returned.</p></div>
                            )}
                        </div>
                    )}

                    {tab === "explain" && explainResult && (
                        <div>
                            {explainResult.warnings && explainResult.warnings.length > 0 && (
                                <div style={{ marginBottom: 12 }}>
                                    <h4 style={{ marginBottom: 4 }}>Warnings</h4>
                                    {explainResult.warnings.map((w: any, i: number) => (
                                        <div key={i} className={`badge ${w.severity === "high" ? "badge-danger" : w.severity === "medium" ? "badge-warning" : "badge-info"}`} style={{ display: "block", marginBottom: 4, textAlign: "left" }}>
                                            [{w.severity}] {w.message}
                                        </div>
                                    ))}
                                </div>
                            )}
                            {explainResult.summary && (
                                <div className="stat-grid" style={{ marginBottom: 12 }}>
                                    <div className="stat-card"><div className="stat-label">Total Cost</div><div className="stat-value">{explainResult.summary.totalCost?.toFixed(1)}</div></div>
                                    <div className="stat-card"><div className="stat-label">Est. Rows</div><div className="stat-value">{explainResult.summary.estimatedRows}</div></div>
                                    {explainResult.summary.actualTime != null && <div className="stat-card"><div className="stat-label">Actual Time</div><div className="stat-value">{explainResult.summary.actualTime}ms</div></div>}
                                </div>
                            )}
                            <h4>Plan</h4>
                            <pre style={{ fontSize: 12, whiteSpace: "pre-wrap", background: "var(--bg-secondary)", padding: 12, borderRadius: 6, maxHeight: 500, overflow: "auto" }}>
                                {explainResult.formatted || JSON.stringify(explainResult.plan, null, 2)}
                            </pre>
                        </div>
                    )}
                </div>
            )}
        </div>
    );
}
