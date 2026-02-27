import React, { useEffect, useState } from "react";
import { api } from "../api";

export function IndexesPage() {
    const [indexes, setIndexes] = useState<any[]>([]);
    const [unused, setUnused] = useState<any[]>([]);
    const [duplicates, setDuplicates] = useState<any[]>([]);
    const [suggestions, setSuggestions] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [tab, setTab] = useState<"all" | "unused" | "duplicates" | "suggestions">("all");
    const [message, setMessage] = useState("");

    const load = async () => {
        setLoading(true);
        try {
            const [idx, un, dup, sug] = await Promise.all([
                api.getIndexes().catch(() => ({ indexes: [] })),
                api.getUnusedIndexes().catch(() => ({ indexes: [] })),
                api.getDuplicateIndexes().catch(() => ({ duplicates: [] })),
                api.getIndexSuggestions().catch(() => ({ suggestions: [] })),
            ]);
            setIndexes(idx.indexes || []);
            setUnused(un.indexes || []);
            setDuplicates(dup.duplicates || []);
            setSuggestions(sug.suggestions || []);
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => { load(); }, []);

    const dropIndex = async (name: string) => {
        if (!confirm(`Drop index "${name}"?`)) return;
        try {
            await api.dropIndex(name);
            setMessage(`Index ${name} dropped`);
            load();
        } catch (err: any) {
            setMessage(`Error: ${err.message}`);
        }
    };

    const createIndex = async (sug: any) => {
        try {
            await api.createIndex({ table: sug.table, columns: sug.columns, unique: false });
            setMessage(`Index created on ${sug.table}(${sug.columns.join(", ")})`);
            load();
        } catch (err: any) {
            setMessage(`Error: ${err.message}`);
        }
    };

    if (loading) return <div className="loading-state"><div className="spinner" /> Analyzing indexes...</div>;

    return (
        <div>
            <div className="page-header">
                <h2>Index Manager</h2>
                <p>Analyze, optimize, and manage database indexes</p>
            </div>

            <div className="stat-grid">
                <div className="stat-card"><div className="stat-label">Total Indexes</div><div className="stat-value">{indexes.length}</div></div>
                <div className="stat-card"><div className="stat-label">Unused</div><div className="stat-value" style={{ color: unused.length > 0 ? "var(--warning)" : "var(--success)" }}>{unused.length}</div></div>
                <div className="stat-card"><div className="stat-label">Duplicate Groups</div><div className="stat-value" style={{ color: duplicates.length > 0 ? "var(--warning)" : "var(--success)" }}>{duplicates.length}</div></div>
                <div className="stat-card"><div className="stat-label">Suggestions</div><div className="stat-value" style={{ color: suggestions.length > 0 ? "var(--accent)" : "var(--text-dim)" }}>{suggestions.length}</div></div>
            </div>

            {message && <div className="card" style={{ padding: 8, fontSize: 13, color: message.startsWith("Error") ? "var(--danger)" : "var(--success)" }}>{message}</div>}

            <div className="tabs" style={{ marginBottom: 16 }}>
                <button className={`tab ${tab === "all" ? "tab-active" : ""}`} onClick={() => setTab("all")}>All ({indexes.length})</button>
                <button className={`tab ${tab === "unused" ? "tab-active" : ""}`} onClick={() => setTab("unused")}>Unused ({unused.length})</button>
                <button className={`tab ${tab === "duplicates" ? "tab-active" : ""}`} onClick={() => setTab("duplicates")}>Duplicates ({duplicates.length})</button>
                <button className={`tab ${tab === "suggestions" ? "tab-active" : ""}`} onClick={() => setTab("suggestions")}>Suggestions ({suggestions.length})</button>
            </div>

            {tab === "all" && (
                <div className="card">
                    <div className="card-header"><h3>All Indexes</h3></div>
                    {indexes.length === 0 ? (
                        <div className="empty-state"><p>No indexes found</p></div>
                    ) : (
                        <table className="data-table">
                            <thead>
                                <tr><th>Name</th><th>Table</th><th>Columns</th><th>Size</th><th>Scans</th><th>Rows Read</th></tr>
                            </thead>
                            <tbody>
                                {indexes.map((idx) => (
                                    <tr key={idx.name}>
                                        <td className="mono">{idx.name}</td>
                                        <td className="mono">{idx.tableName}</td>
                                        <td className="mono">{idx.columns || "—"}</td>
                                        <td>{idx.size || "—"}</td>
                                        <td>{idx.scans?.toLocaleString() ?? "—"}</td>
                                        <td>{idx.tuplesRead?.toLocaleString() ?? "—"}</td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </div>
            )}

            {tab === "unused" && (
                <div className="card">
                    <div className="card-header"><h3>Unused Indexes</h3></div>
                    <p style={{ fontSize: 13, color: "var(--text-dim)", marginBottom: 8 }}>
                        These indexes have zero or very few scans and may be safe to drop.
                    </p>
                    {unused.length === 0 ? (
                        <div className="empty-state"><p>No unused indexes — looking good!</p></div>
                    ) : (
                        <table className="data-table">
                            <thead>
                                <tr><th>Name</th><th>Table</th><th>Size</th><th>Scans</th><th></th></tr>
                            </thead>
                            <tbody>
                                {unused.map((idx) => (
                                    <tr key={idx.name}>
                                        <td className="mono">{idx.name}</td>
                                        <td className="mono">{idx.tableName}</td>
                                        <td>{idx.size || "—"}</td>
                                        <td>{idx.scans ?? 0}</td>
                                        <td><button className="btn btn-danger btn-sm" onClick={() => dropIndex(idx.name)}>Drop</button></td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </div>
            )}

            {tab === "duplicates" && (
                <div className="card">
                    <div className="card-header"><h3>Duplicate Index Groups</h3></div>
                    <p style={{ fontSize: 13, color: "var(--text-dim)", marginBottom: 8 }}>
                        Groups of indexes that cover the same columns. Consider keeping only one per group.
                    </p>
                    {duplicates.length === 0 ? (
                        <div className="empty-state"><p>No duplicate indexes found</p></div>
                    ) : (
                        duplicates.map((group, gi) => (
                            <div key={gi} style={{ marginBottom: 12, padding: 8, background: "var(--bg-secondary)", borderRadius: 6 }}>
                                <h4 style={{ fontSize: 13, marginBottom: 4 }}>Group {gi + 1}: {group.group}</h4>
                                {group.indexes?.map((idx: any) => (
                                    <div key={idx.name} style={{ display: "flex", justifyContent: "space-between", alignItems: "center", padding: "4px 0" }}>
                                        <span className="mono" style={{ fontSize: 12 }}>{idx.name} — {idx.scans ?? 0} scans, size: {idx.size || "?"}</span>
                                        <button className="btn btn-danger btn-sm" onClick={() => dropIndex(idx.name)}>Drop</button>
                                    </div>
                                ))}
                            </div>
                        ))
                    )}
                </div>
            )}

            {tab === "suggestions" && (
                <div className="card">
                    <div className="card-header"><h3>Index Suggestions</h3></div>
                    <p style={{ fontSize: 13, color: "var(--text-dim)", marginBottom: 8 }}>
                        Tables with high sequential scan ratios that could benefit from new indexes.
                    </p>
                    {suggestions.length === 0 ? (
                        <div className="empty-state"><p>No suggestions — your indexes look optimal</p></div>
                    ) : (
                        <table className="data-table">
                            <thead>
                                <tr><th>Table</th><th>Columns</th><th>Reason</th><th>Seq Scan Ratio</th><th></th></tr>
                            </thead>
                            <tbody>
                                {suggestions.map((sug, i) => (
                                    <tr key={i}>
                                        <td className="mono">{sug.table}</td>
                                        <td className="mono">{sug.columns?.join(", ") || "—"}</td>
                                        <td style={{ fontSize: 13 }}>{sug.reason || "High sequential scan ratio"}</td>
                                        <td>{sug.seqScanRatio != null ? `${(sug.seqScanRatio * 100).toFixed(0)}%` : "—"}</td>
                                        <td>
                                            {sug.columns && <button className="btn btn-primary btn-sm" onClick={() => createIndex(sug)}>Create</button>}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </div>
            )}
        </div>
    );
}
