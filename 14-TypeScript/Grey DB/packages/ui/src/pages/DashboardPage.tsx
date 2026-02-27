import React, { useEffect, useState } from "react";
import { api } from "../api";

export function DashboardPage() {
    const [data, setData] = useState<any>(null);
    const [health, setHealth] = useState<any>(null);
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        Promise.all([
            api.getDashboard().catch(() => null),
            api.health().catch(() => null),
        ]).then(([d, h]) => {
            setData(d);
            setHealth(h);
            setLoading(false);
        });
    }, []);

    if (loading) return <div className="loading-state"><div className="spinner" /> Loading dashboard...</div>;

    return (
        <div>
            <div className="page-header">
                <h2>Dashboard</h2>
                <p>System overview and query metrics</p>
            </div>

            {/* Health indicator */}
            {health && (
                <div className={`warning-box ${health.status === "healthy" ? "info" : health.status === "degraded" ? "warning" : "danger"}`}>
                    <span>{health.status === "healthy" ? "✅" : health.status === "degraded" ? "⚠️" : "❌"}</span>
                    <div>
                        <strong>Database: {health.status}</strong>
                        <div style={{ fontSize: 12, marginTop: 2 }}>
                            Size: {health.databaseSize} · Latency: {health.latencyMs}ms · Connections: {health.connectionPool?.total ?? 0}
                        </div>
                    </div>
                </div>
            )}

            {/* Stats grid */}
            <div className="stat-grid">
                <div className="stat-card">
                    <div className="label">Total Queries (24h)</div>
                    <div className="value info">{data?.totalQueries?.toLocaleString() ?? "—"}</div>
                </div>
                <div className="stat-card">
                    <div className="label">Avg Duration</div>
                    <div className="value">{data?.avgDurationMs ?? "—"}ms</div>
                </div>
                <div className="stat-card">
                    <div className="label">P95 Duration</div>
                    <div className="value warning">{data?.p95DurationMs ?? "—"}ms</div>
                </div>
                <div className="stat-card">
                    <div className="label">Queries/min</div>
                    <div className="value success">{data?.queriesPerMinute ?? "—"}</div>
                </div>
                <div className="stat-card">
                    <div className="label">Slow Queries</div>
                    <div className="value danger">{data?.slowQueries ?? "—"}</div>
                </div>
                <div className="stat-card">
                    <div className="label">Errors (24h)</div>
                    <div className="value danger">{data?.errorCount ?? "—"}</div>
                </div>
                <div className="stat-card">
                    <div className="label">Active Connections</div>
                    <div className="value">{data?.activeConnections ?? "—"}</div>
                </div>
                <div className="stat-card">
                    <div className="label">Database Size</div>
                    <div className="value info">{data?.databaseSize ?? "—"}</div>
                </div>
            </div>

            {/* Top tenants */}
            {data?.topTenants?.length > 0 && (
                <div className="card">
                    <div className="card-header"><h3>Top Tenants by Query Count</h3></div>
                    <table className="data-table">
                        <thead>
                            <tr><th>Tenant ID</th><th>Queries</th></tr>
                        </thead>
                        <tbody>
                            {data.topTenants.map((t: any) => (
                                <tr key={t.tenantId}>
                                    <td className="mono">{t.tenantId}</td>
                                    <td>{t.queryCount.toLocaleString()}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}

            {/* Query trend */}
            {data?.queryTrend?.length > 0 && (
                <div className="card">
                    <div className="card-header"><h3>Query Trend (24h)</h3></div>
                    <table className="data-table">
                        <thead>
                            <tr><th>Time</th><th>Queries</th><th>Avg Duration</th></tr>
                        </thead>
                        <tbody>
                            {data.queryTrend.map((t: any, i: number) => (
                                <tr key={i}>
                                    <td>{new Date(t.timestamp).toLocaleTimeString()}</td>
                                    <td>{t.count}</td>
                                    <td>{t.avgMs}ms</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    );
}
