import React, { useEffect, useState } from "react";
import { api } from "../api";

export function SchemaPage() {
    const [tables, setTables] = useState<any[]>([]);
    const [selectedTable, setSelectedTable] = useState<any>(null);
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        api.getTables().then((d) => { setTables(d.tables); setLoading(false); }).catch(() => setLoading(false));
    }, []);

    const loadTable = async (name: string) => {
        try {
            const detail = await api.getTable(name);
            setSelectedTable(detail);
        } catch (err: any) {
            console.error(err);
        }
    };

    if (loading) return <div className="loading-state"><div className="spinner" /> Loading schema...</div>;

    return (
        <div>
            <div className="page-header">
                <h2>Schema Browser</h2>
                <p>Explore database tables, columns, indexes, and constraints</p>
            </div>

            <div style={{ display: "flex", gap: 24 }}>
                {/* Table list */}
                <div style={{ width: 280, flexShrink: 0 }}>
                    <div className="card" style={{ maxHeight: "70vh", overflowY: "auto" }}>
                        <div className="card-header"><h3>Tables ({tables.length})</h3></div>
                        {tables.length === 0 ? (
                            <div className="empty-state"><p>No tables found</p></div>
                        ) : (
                            tables.map((t) => (
                                <button
                                    key={t.table_name}
                                    className={`nav-item ${selectedTable?.name === t.table_name ? "active" : ""}`}
                                    onClick={() => loadTable(t.table_name)}
                                    style={{ marginBottom: 2 }}
                                >
                                    <span>📋</span>
                                    <div>
                                        <div>{t.table_name}</div>
                                        <div style={{ fontSize: 11, color: "var(--text-dim)" }}>
                                            {t.column_count} cols · ~{parseInt(t.row_estimate || 0).toLocaleString()} rows
                                        </div>
                                    </div>
                                </button>
                            ))
                        )}
                    </div>
                </div>

                {/* Table detail */}
                <div style={{ flex: 1 }}>
                    {!selectedTable ? (
                        <div className="card">
                            <div className="empty-state">
                                <h3>Select a table</h3>
                                <p>Click a table on the left to view its structure</p>
                            </div>
                        </div>
                    ) : (
                        <>
                            <div className="card">
                                <div className="card-header">
                                    <h3>📋 {selectedTable.name}</h3>
                                    <span className="badge badge-info">{selectedTable.rowCount?.toLocaleString()} rows</span>
                                </div>

                                <h4 style={{ marginBottom: 8, fontSize: 14 }}>Columns</h4>
                                <table className="data-table">
                                    <thead>
                                        <tr><th>Name</th><th>Type</th><th>Nullable</th><th>Default</th><th>Description</th></tr>
                                    </thead>
                                    <tbody>
                                        {selectedTable.columns?.map((c: any) => (
                                            <tr key={c.column_name}>
                                                <td className="mono">{c.column_name}</td>
                                                <td><span className="badge badge-primary">{c.data_type}</span></td>
                                                <td>{c.is_nullable === "YES" ? <span className="badge badge-warning">NULL</span> : <span className="badge badge-success">NOT NULL</span>}</td>
                                                <td className="mono" style={{ fontSize: 12 }}>{c.column_default || "—"}</td>
                                                <td style={{ color: "var(--text-dim)", fontSize: 13 }}>{c.description || ""}</td>
                                            </tr>
                                        ))}
                                    </tbody>
                                </table>
                            </div>

                            {selectedTable.indexes?.length > 0 && (
                                <div className="card">
                                    <div className="card-header"><h3>Indexes</h3></div>
                                    <table className="data-table">
                                        <thead><tr><th>Name</th><th>Definition</th></tr></thead>
                                        <tbody>
                                            {selectedTable.indexes.map((idx: any) => (
                                                <tr key={idx.indexname}>
                                                    <td className="mono">{idx.indexname}</td>
                                                    <td className="mono" style={{ fontSize: 12 }}>{idx.indexdef}</td>
                                                </tr>
                                            ))}
                                        </tbody>
                                    </table>
                                </div>
                            )}

                            {selectedTable.constraints?.length > 0 && (
                                <div className="card">
                                    <div className="card-header"><h3>Constraints</h3></div>
                                    <table className="data-table">
                                        <thead><tr><th>Name</th><th>Type</th><th>Column</th><th>References</th></tr></thead>
                                        <tbody>
                                            {selectedTable.constraints.map((c: any, i: number) => (
                                                <tr key={i}>
                                                    <td className="mono">{c.constraint_name}</td>
                                                    <td><span className="badge badge-info">{c.constraint_type}</span></td>
                                                    <td className="mono">{c.column_name}</td>
                                                    <td className="mono">{c.foreign_table ? `${c.foreign_table}.${c.foreign_column}` : "—"}</td>
                                                </tr>
                                            ))}
                                        </tbody>
                                    </table>
                                </div>
                            )}
                        </>
                    )}
                </div>
            </div>
        </div>
    );
}
