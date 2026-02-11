import { PageHeader, StatCard, Badge, DataTable } from '../../components/ui';
import { Shield, Key, Smartphone, FileSearch, Users, AlertTriangle } from 'lucide-react';

export function IdentitySecurity() {
    return (
        <div>
            <PageHeader
                title="Identity & Security"
                subtitle="Authentication, MFA, device security & audit logs"
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Active Users" value={342} change="All authenticated" changeType="positive" icon={<Users size={18} />} />
                <StatCard label="MFA Enabled" value="98.2%" change="+1.4%" changeType="positive" icon={<Smartphone size={18} />} />
                <StatCard label="Security Score" value="94/100" change="A+ Rating" changeType="positive" icon={<Shield size={18} />} />
                <StatCard label="Threats Blocked" value={12} change="Last 24h" changeType="neutral" icon={<AlertTriangle size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6 mb-6">
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Security Posture</h3>
                    <div className="space-y-4">
                        {[
                            { check: 'SSO Configuration', status: 'Pass', detail: 'SAML 2.0 + OIDC' },
                            { check: 'MFA Enforcement', status: 'Pass', detail: '98.2% coverage' },
                            { check: 'Password Policy', status: 'Pass', detail: 'Min 12 chars + complexity' },
                            { check: 'Session Timeout', status: 'Pass', detail: '30m idle timeout' },
                            { check: 'IP Allowlisting', status: 'Warning', detail: '3 broad ranges detected' },
                            { check: 'Certificate Rotation', status: 'Pass', detail: 'Auto-rotated Q3' },
                            { check: 'Audit Log Retention', status: 'Pass', detail: '365 days encrypted' },
                        ].map(c => (
                            <div key={c.check} className="flex items-center justify-between">
                                <div className="flex items-center gap-3">
                                    <div className={`w-2 h-2 rounded-full ${c.status === 'Pass' ? 'bg-accent-emerald' : 'bg-accent-amber'}`} />
                                    <div>
                                        <p className="text-sm text-grey-300">{c.check}</p>
                                        <p className="text-xs text-grey-500">{c.detail}</p>
                                    </div>
                                </div>
                                <Badge variant={c.status === 'Pass' ? 'success' : 'warning'}>{c.status}</Badge>
                            </div>
                        ))}
                    </div>
                </div>

                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4">Active Sessions</h3>
                    <div className="space-y-3">
                        {[
                            { user: 'Admin User', device: 'macOS — Chrome', ip: '192.168.1.42', location: 'New York, US', active: '2m ago' },
                            { user: 'Sarah Chen', device: 'Windows — Edge', ip: '10.0.0.128', location: 'San Francisco, US', active: '5m ago' },
                            { user: 'Marcus Johnson', device: 'macOS — Safari', ip: '172.16.0.84', location: 'Chicago, US', active: '12m ago' },
                            { user: 'Emily Rodriguez', device: 'iOS — App', ip: '10.0.1.200', location: 'Austin, US', active: '18m ago' },
                            { user: 'David Kim', device: 'Linux — Firefox', ip: '10.0.2.64', location: 'Seattle, US', active: '24m ago' },
                        ].map(s => (
                            <div key={s.user} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 rounded-full bg-grey-700 flex items-center justify-center text-xs font-bold text-grey-300">
                                        {s.user.split(' ').map(n => n[0]).join('')}
                                    </div>
                                    <div>
                                        <p className="text-sm text-grey-200">{s.user}</p>
                                        <p className="text-xs text-grey-500">{s.device} · {s.ip}</p>
                                    </div>
                                </div>
                                <div className="text-right">
                                    <p className="text-xs text-grey-400">{s.location}</p>
                                    <p className="text-xs text-grey-500">{s.active}</p>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            </div>

            <div className="gs-card p-5">
                <div className="flex items-center justify-between mb-4">
                    <h3 className="gs-section-title flex items-center gap-2"><FileSearch size={18} /> Audit Log</h3>
                    <button className="gs-btn-ghost text-sm">Export</button>
                </div>
                <DataTable
                    columns={['Timestamp', 'User', 'Action', 'Resource', 'IP Address', 'Status']}
                    rows={[
                        ['10:02:14 AM', 'Admin User', 'Login', 'Auth Service', '192.168.1.42', <Badge variant="success">Success</Badge>],
                        ['10:01:48 AM', 'Bot — CI/CD', 'Deploy', 'Production', '10.0.0.1', <Badge variant="success">Success</Badge>],
                        ['9:58:22 AM', 'Sarah Chen', 'Update Role', 'User Management', '10.0.0.128', <Badge variant="success">Success</Badge>],
                        ['9:54:01 AM', 'Unknown', 'Login Attempt', 'Auth Service', '45.33.128.92', <Badge variant="danger">Blocked</Badge>],
                        ['9:48:36 AM', 'Marcus J.', 'API Key Rotate', 'Settings', '172.16.0.84', <Badge variant="success">Success</Badge>],
                    ]}
                />
            </div>
        </div>
    );
}
