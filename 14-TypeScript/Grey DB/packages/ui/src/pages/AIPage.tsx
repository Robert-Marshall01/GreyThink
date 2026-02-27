import React, { useState } from "react";
import { api } from "../api";

export function AIPage() {
    const [nlQuery, setNlQuery] = useState("");
    const [sqlResult, setSqlResult] = useState<any>(null);
    const [docs, setDocs] = useState<any>(null);
    const [erDiagram, setErDiagram] = useState("");
    const [tab, setTab] = useState<"nl2sql" | "docs" | "erd">("nl2sql");
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");
    const [safetyCheck, setSafetyCheck] = useState<any>(null);

    const translateQuery = async () => {
        if (!nlQuery.trim()) return;
        setLoading(true);
        setError("");
        setSafetyCheck(null);
        try {
            const data = await api.nlToSql(nlQuery);
            setSqlResult(data);
        } catch (err: any) {
            setError(err.message);
        } finally {
            setLoading(false);
        }
    };

    const checkSafety = async (sql: string) => {
        try {
            const data = await api.checkSqlSafety(sql);
            setSafetyCheck(data);
        } catch (err: any) {
            setError(err.message);
        }
    };

    const loadDocs = async () => {
        setLoading(true);
        setError("");
        try {
            const data = await api.getAllDocs();
            setDocs(data);
        } catch (err: any) {
            setError(err.message);
        } finally {
            setLoading(false);
        }
    };

    const loadERD = async () => {
        setLoading(true);
        setError("");
        try {
            const data = await api.getERDiagram();
            setErDiagram(data.diagram);
        } catch (err: any) {
            setError(err.message);
        } finally {
            setLoading(false);
        }
    };

    return (
        <div>
            <div className="page-header">
                <h2>AI Assistant</h2>
                <p>Natural language queries, auto-documentation, and schema visualization</p>
            </div>

            <div className="tabs" style={{ marginBottom: 16 }}>
                <button className={`tab ${tab === "nl2sql" ? "tab-active" : ""}`} onClick={() => setTab("nl2sql")}>NL → SQL</button>
                <button className={`tab ${tab === "docs" ? "tab-active" : ""}`} onClick={() => { setTab("docs"); if (!docs) loadDocs(); }}>Auto-Docs</button>
                <button className={`tab ${tab === "erd" ? "tab-active" : ""}`} onClick={() => { setTab("erd"); if (!erDiagram) loadERD(); }}>ER Diagram</button>
            </div>

            {error && <div className="card" style={{ borderColor: "var(--danger)", color: "var(--danger)", fontSize: 13 }}>{error}</div>}

            {tab === "nl2sql" && (
                <div>
                    <div className="card">
                        <div className="card-header"><h3>Natural Language → SQL</h3></div>
                        <p style={{ fontSize: 13, color: "var(--text-dim)", marginBottom: 8 }}>
                            Describe what you want to query in plain English. Grey DB generates a schema-aware prompt you can send to any LLM.
                        </p>
                        <div className="input-group">
                            <label>Your Question</label>
                            <textarea
                                className="sql-editor"
                                value={nlQuery}
                                onChange={(e) => setNlQuery(e.target.value)}
                                rows={3}
                                placeholder="e.g. Show me the top 10 users who signed up this month, ordered by their total order amount"
                            />
                        </div>
                        <button className="btn btn-primary" onClick={translateQuery} disabled={loading || !nlQuery.trim()}>
                            {loading ? "Preparing..." : "🤖 Generate SQL Prompt"}
                        </button>
                    </div>

                    {sqlResult && (
                        <div className="card">
                            <div className="card-header"><h3>Generated Prompt</h3></div>
                            <div style={{ marginBottom: 12 }}>
                                <h4 style={{ fontSize: 13, marginBottom: 4 }}>System Prompt</h4>
                                <pre style={{ fontSize: 11, background: "var(--bg-secondary)", padding: 12, borderRadius: 6, maxHeight: 300, overflow: "auto", whiteSpace: "pre-wrap" }}>
                                    {sqlResult.systemPrompt}
                                </pre>
                            </div>
                            <div style={{ marginBottom: 12 }}>
                                <h4 style={{ fontSize: 13, marginBottom: 4 }}>User Prompt</h4>
                                <pre style={{ fontSize: 11, background: "var(--bg-secondary)", padding: 12, borderRadius: 6, whiteSpace: "pre-wrap" }}>
                                    {sqlResult.userPrompt}
                                </pre>
                            </div>
                            <p style={{ fontSize: 12, color: "var(--text-dim)" }}>
                                Copy these prompts to your preferred LLM (GPT-4, Claude, etc.) to get the SQL query.
                            </p>
                            {sqlResult.generatedSQL && (
                                <div style={{ marginTop: 12 }}>
                                    <h4 style={{ fontSize: 13, marginBottom: 4 }}>Generated SQL</h4>
                                    <pre className="sql-editor" style={{ minHeight: 0 }}>{sqlResult.generatedSQL}</pre>
                                    <button className="btn btn-secondary" onClick={() => checkSafety(sqlResult.generatedSQL)} style={{ marginTop: 8 }}>
                                        🛡️ Check Safety
                                    </button>
                                    {safetyCheck && (
                                        <div style={{ marginTop: 8 }}>
                                            {safetyCheck.safe ? (
                                                <span className="badge badge-success">✓ Safe to execute</span>
                                            ) : (
                                                <div>
                                                    <span className="badge badge-danger">⚠ Safety issues detected</span>
                                                    <ul style={{ fontSize: 13, marginTop: 8 }}>
                                                        {safetyCheck.issues?.map((issue: string, i: number) => <li key={i}>{issue}</li>)}
                                                    </ul>
                                                </div>
                                            )}
                                        </div>
                                    )}
                                </div>
                            )}
                        </div>
                    )}
                </div>
            )}

            {tab === "docs" && (
                <div>
                    {loading ? (
                        <div className="loading-state"><div className="spinner" /> Generating documentation...</div>
                    ) : docs ? (
                        <div>
                            {docs.tables?.map((table: any) => (
                                <div className="card" key={table.name}>
                                    <div className="card-header">
                                        <h3>{table.name}</h3>
                                        {table.description && <p style={{ fontSize: 13, marginTop: 4 }}>{table.description}</p>}
                                    </div>
                                    {table.columns && (
                                        <table className="data-table">
                                            <thead>
                                                <tr><th>Column</th><th>Type</th><th>Nullable</th><th>Description</th></tr>
                                            </thead>
                                            <tbody>
                                                {table.columns.map((col: any) => (
                                                    <tr key={col.name}>
                                                        <td className="mono">{col.name}</td>
                                                        <td className="mono">{col.type}</td>
                                                        <td>{col.nullable ? "Yes" : "No"}</td>
                                                        <td style={{ fontSize: 13, color: "var(--text-dim)" }}>{col.description || "—"}</td>
                                                    </tr>
                                                ))}
                                            </tbody>
                                        </table>
                                    )}
                                    {table.relationships && table.relationships.length > 0 && (
                                        <div style={{ marginTop: 8 }}>
                                            <h4 style={{ fontSize: 13 }}>Relationships</h4>
                                            {table.relationships.map((rel: any, i: number) => (
                                                <span className="badge badge-info" key={i} style={{ marginRight: 4 }}>
                                                    {rel.column} → {rel.referencedTable}.{rel.referencedColumn}
                                                </span>
                                            ))}
                                        </div>
                                    )}
                                </div>
                            ))}
                        </div>
                    ) : (
                        <div className="empty-state"><p>Click the Auto-Docs tab to generate documentation</p></div>
                    )}
                </div>
            )}

            {tab === "erd" && (
                <div className="card">
                    <div className="card-header">
                        <h3>Entity-Relationship Diagram</h3>
                        <p style={{ fontSize: 13 }}>Mermaid ER diagram generated from your database schema</p>
                    </div>
                    {loading ? (
                        <div className="loading-state"><div className="spinner" /> Generating diagram...</div>
                    ) : erDiagram ? (
                        <div>
                            <pre style={{ fontSize: 11, background: "var(--bg-secondary)", padding: 12, borderRadius: 6, maxHeight: 600, overflow: "auto", whiteSpace: "pre-wrap" }}>
                                {erDiagram}
                            </pre>
                            <p style={{ fontSize: 12, color: "var(--text-dim)", marginTop: 8 }}>
                                Copy this Mermaid markup to render it at mermaid.live or in any Mermaid-compatible viewer.
                            </p>
                        </div>
                    ) : (
                        <div className="empty-state"><p>Click the ER Diagram tab to generate</p></div>
                    )}
                </div>
            )}
        </div>
    );
}
