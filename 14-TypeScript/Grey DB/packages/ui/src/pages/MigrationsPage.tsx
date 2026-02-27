import React, { useEffect, useState } from "react";
import { api } from "../api";

export function MigrationsPage() {
    const [migrations, setMigrations] = useState<any>(null);
    const [loading, setLoading] = useState(true);
    const [sqlInput, setSqlInput] = useState("");
    const [nameInput, setNameInput] = useState("");
    const [applying, setApplying] = useState(false);
    const [message, setMessage] = useState("");

    const load = () => {
        api.getMigrations().then(setMigrations).catch(() => { }).finally(() => setLoading(false));
    };

    useEffect(load, []);

    const applyMigration = async () => {
        if (!sqlInput.trim()) return;
        setApplying(true);
        setMessage("");
        try {
            await api.applyMigration({ upSQL: sqlInput, name: nameInput || "manual_migration" });
            setMessage("Migration applied successfully!");
            setSqlInput("");
            setNameInput("");
            load();
        } catch (err: any) {
            setMessage(`Error: ${err.message}`);
        } finally {
            setApplying(false);
        }
    };

    const rollback = async () => {
        if (!confirm("Are you sure you want to rollback the last migration?")) return;
        try {
            await api.rollbackMigration();
            setMessage("Rollback successful");
            load();
        } catch (err: any) {
            setMessage(`Rollback error: ${err.message}`);
        }
    };

    if (loading) return <div className="loading-state"><div className="spinner" /> Loading migrations...</div>;

    return (
        <div>
            <div className="page-header">
                <h2>Migrations</h2>
                <p>Track and manage database schema changes</p>
            </div>

            <div className="stat-grid">
                <div className="stat-card">
                    <div className="label">Current Version</div>
                    <div className="value info">{migrations?.currentVersion ?? 0}</div>
                </div>
                <div className="stat-card">
                    <div className="label">Total Migrations</div>
                    <div className="value">{migrations?.migrations?.length ?? 0}</div>
                </div>
            </div>

            {/* Apply new migration */}
            <div className="card">
                <div className="card-header">
                    <h3>Apply Migration</h3>
                    <button className="btn btn-danger btn-sm" onClick={rollback}>Rollback Last</button>
                </div>

                <div className="input-group">
                    <label>Migration Name</label>
                    <input className="input" value={nameInput} onChange={(e) => setNameInput(e.target.value)} placeholder="e.g. add_user_email_column" />
                </div>

                <div className="input-group">
                    <label>SQL</label>
                    <textarea className="sql-editor" value={sqlInput} onChange={(e) => setSqlInput(e.target.value)} placeholder="Enter migration SQL..." />
                </div>

                <div style={{ display: "flex", gap: 8, alignItems: "center" }}>
                    <button className="btn btn-primary" onClick={applyMigration} disabled={applying}>
                        {applying ? "Applying..." : "Apply Migration"}
                    </button>
                    {message && <span style={{ fontSize: 13, color: message.startsWith("Error") ? "var(--danger)" : "var(--success)" }}>{message}</span>}
                </div>
            </div>

            {/* Migration history */}
            <div className="card">
                <div className="card-header"><h3>Migration History</h3></div>
                {(migrations?.migrations?.length ?? 0) === 0 ? (
                    <div className="empty-state"><p>No migrations yet</p></div>
                ) : (
                    <table className="data-table">
                        <thead>
                            <tr><th>Version</th><th>Name</th><th>Safe</th><th>Applied At</th><th>Warnings</th></tr>
                        </thead>
                        <tbody>
                            {[...migrations.migrations].reverse().map((m: any) => (
                                <tr key={m.version}>
                                    <td className="mono">v{m.version}</td>
                                    <td>{m.name}</td>
                                    <td>{m.safe ? <span className="badge badge-success">Safe</span> : <span className="badge badge-danger">Unsafe</span>}</td>
                                    <td style={{ fontSize: 13, color: "var(--text-dim)" }}>{m.appliedAt ? new Date(m.appliedAt).toLocaleString() : "—"}</td>
                                    <td>{(m.warnings?.length ?? 0) > 0 ? <span className="badge badge-warning">{m.warnings.length}</span> : "—"}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
        </div>
    );
}
