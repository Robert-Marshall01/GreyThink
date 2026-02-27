import React, { useEffect, useState } from "react";
import { api } from "../api";

export function TenantsPage() {
    const [tenants, setTenants] = useState<any[]>([]);
    const [loading, setLoading] = useState(true);
    const [showCreate, setShowCreate] = useState(false);
    const [form, setForm] = useState({ name: "", slug: "", isolationStrategy: "shared", storageLimitMB: 1024 });
    const [message, setMessage] = useState("");

    const load = () => {
        api.getTenants().then((d) => setTenants(d.tenants)).catch(() => { }).finally(() => setLoading(false));
    };

    useEffect(load, []);

    const createTenant = async () => {
        if (!form.name || !form.slug) return;
        try {
            await api.createTenant(form);
            setMessage("Tenant created!");
            setShowCreate(false);
            setForm({ name: "", slug: "", isolationStrategy: "shared", storageLimitMB: 1024 });
            load();
        } catch (err: any) {
            setMessage(`Error: ${err.message}`);
        }
    };

    const deleteTenant = async (id: string) => {
        if (!confirm("Delete this tenant?")) return;
        try {
            await api.deleteTenant(id);
            load();
        } catch (err: any) {
            setMessage(`Error: ${err.message}`);
        }
    };

    if (loading) return <div className="loading-state"><div className="spinner" /> Loading tenants...</div>;

    return (
        <div>
            <div className="page-header">
                <h2>Tenant Management</h2>
                <p>Multi-tenant isolation, configuration, and monitoring</p>
            </div>

            <div style={{ marginBottom: 16, display: "flex", gap: 8 }}>
                <button className="btn btn-primary" onClick={() => setShowCreate(!showCreate)}>
                    {showCreate ? "Cancel" : "+ New Tenant"}
                </button>
                {message && <span style={{ fontSize: 13, alignSelf: "center", color: message.startsWith("Error") ? "var(--danger)" : "var(--success)" }}>{message}</span>}
            </div>

            {showCreate && (
                <div className="card">
                    <div className="card-header"><h3>Create Tenant</h3></div>
                    <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 12 }}>
                        <div className="input-group">
                            <label>Name</label>
                            <input className="input" value={form.name} onChange={(e) => setForm({ ...form, name: e.target.value })} placeholder="Acme Corp" />
                        </div>
                        <div className="input-group">
                            <label>Slug</label>
                            <input className="input" value={form.slug} onChange={(e) => setForm({ ...form, slug: e.target.value })} placeholder="acme-corp" />
                        </div>
                        <div className="input-group">
                            <label>Isolation Strategy</label>
                            <select className="select" value={form.isolationStrategy} onChange={(e) => setForm({ ...form, isolationStrategy: e.target.value })}>
                                <option value="shared">Shared (tenant_id column)</option>
                                <option value="schema">Schema per tenant</option>
                                <option value="hybrid">Hybrid</option>
                            </select>
                        </div>
                        <div className="input-group">
                            <label>Storage Limit (MB)</label>
                            <input className="input" type="number" value={form.storageLimitMB} onChange={(e) => setForm({ ...form, storageLimitMB: parseInt(e.target.value) })} />
                        </div>
                    </div>
                    <button className="btn btn-primary" onClick={createTenant} style={{ marginTop: 8 }}>Create Tenant</button>
                </div>
            )}

            <div className="card">
                <div className="card-header"><h3>Tenants ({tenants.length})</h3></div>
                {tenants.length === 0 ? (
                    <div className="empty-state"><p>No tenants created yet</p></div>
                ) : (
                    <table className="data-table">
                        <thead>
                            <tr><th>Name</th><th>Slug</th><th>Isolation</th><th>Storage Limit</th><th>Status</th><th>Created</th><th></th></tr>
                        </thead>
                        <tbody>
                            {tenants.map((t) => (
                                <tr key={t.id}>
                                    <td><strong>{t.name}</strong></td>
                                    <td className="mono">{t.slug}</td>
                                    <td><span className={`badge ${t.isolationStrategy === "shared" ? "badge-info" : t.isolationStrategy === "schema" ? "badge-primary" : "badge-warning"}`}>{t.isolationStrategy}</span></td>
                                    <td>{t.storageLimitMB} MB</td>
                                    <td>{t.active ? <span className="badge badge-success">Active</span> : <span className="badge badge-danger">Inactive</span>}</td>
                                    <td style={{ fontSize: 13, color: "var(--text-dim)" }}>{new Date(t.createdAt).toLocaleDateString()}</td>
                                    <td><button className="btn btn-danger btn-sm" onClick={() => deleteTenant(t.id)}>Delete</button></td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
        </div>
    );
}
