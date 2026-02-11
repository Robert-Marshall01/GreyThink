import { useState } from 'react';
import { PageHeader, StatCard, Badge, DataTable, Tabs, ProgressBar } from '../../components/ui';
import {
    Shield, Users, Key, FileText, Lock, UserCheck,
    GitBranch, AlertTriangle, CheckCircle, Settings,
    Eye, Clock, Fingerprint, Network
} from 'lucide-react';

const ssoProviders = [
    { name: 'Okta', protocol: 'SAML 2.0', tenants: 4, status: 'Active', lastSync: '2 min ago' },
    { name: 'Azure AD', protocol: 'OIDC', tenants: 3, status: 'Active', lastSync: '5 min ago' },
    { name: 'Google Workspace', protocol: 'OIDC', tenants: 2, status: 'Active', lastSync: '8 min ago' },
    { name: 'OneLogin', protocol: 'SAML 2.0', tenants: 1, status: 'Active', lastSync: '12 min ago' },
    { name: 'PingIdentity', protocol: 'SAML 2.0', tenants: 1, status: 'Pending', lastSync: '—' },
];

const roles = [
    { role: 'Super Admin', users: 4, permissions: 248, scope: 'Global', type: 'RBAC' },
    { role: 'Tenant Admin', users: 18, permissions: 186, scope: 'Tenant', type: 'RBAC' },
    { role: 'HR Manager', users: 12, permissions: 64, scope: 'Module', type: 'RBAC+ABAC' },
    { role: 'Finance Analyst', users: 24, permissions: 42, scope: 'Module', type: 'RBAC+ABAC' },
    { role: 'Sales Rep', users: 86, permissions: 38, scope: 'Module', type: 'RBAC' },
    { role: 'Developer', users: 142, permissions: 92, scope: 'Tenant', type: 'RBAC+ABAC' },
    { role: 'Viewer', users: 420, permissions: 12, scope: 'Module', type: 'RBAC' },
];

const auditEvents = [
    { actor: 'admin@acme.com', action: 'role.assign', resource: 'user:sarah@acme.com → HR Manager', time: '2 min ago', risk: 'Low' },
    { actor: 'system', action: 'token.revoke', resource: 'session:expired_batch (14 tokens)', time: '5 min ago', risk: 'None' },
    { actor: 'bob@globex.com', action: 'permission.escalate', resource: 'role:Finance Analyst → Admin', time: '12 min ago', risk: 'High' },
    { actor: 'scim@okta.com', action: 'user.provision', resource: 'user:new_hire_batch (8 users)', time: '18 min ago', risk: 'None' },
    { actor: 'admin@initech.com', action: 'policy.update', resource: 'policy:mfa_enforcement', time: '25 min ago', risk: 'Medium' },
    { actor: 'system', action: 'key.rotate', resource: 'jwt_signing_key_v42', time: '1h ago', risk: 'None' },
    { actor: 'alice@umbrella.com', action: 'login.failed', resource: '3 attempts from 185.42.x.x', time: '1.5h ago', risk: 'High' },
];

const scimEndpoints = [
    { method: 'GET', path: '/scim/v2/Users', description: 'List/search users', status: 'Active' },
    { method: 'POST', path: '/scim/v2/Users', description: 'Create user', status: 'Active' },
    { method: 'PUT', path: '/scim/v2/Users/:id', description: 'Replace user', status: 'Active' },
    { method: 'PATCH', path: '/scim/v2/Users/:id', description: 'Update user attributes', status: 'Active' },
    { method: 'DELETE', path: '/scim/v2/Users/:id', description: 'Deactivate user', status: 'Active' },
    { method: 'GET', path: '/scim/v2/Groups', description: 'List/search groups', status: 'Active' },
    { method: 'POST', path: '/scim/v2/Groups', description: 'Create group', status: 'Active' },
    { method: 'PATCH', path: '/scim/v2/Groups/:id', description: 'Update group membership', status: 'Active' },
];

export function UnifiedIdentity() {
    const [activeTab, setActiveTab] = useState('SSO');

    return (
        <div>
            <PageHeader
                title="Unified Identity & Access Platform"
                subtitle="Central identity service — SSO, SCIM, RBAC/ABAC, and audit"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary flex items-center gap-2"><Settings size={16} /> Policies</button>
                        <button className="gs-btn-primary flex items-center gap-2"><UserCheck size={16} /> Add Provider</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Identity Providers" value="5" change="SAML + OIDC" changeType="neutral" icon={<Fingerprint size={18} />} />
                <StatCard label="Managed Users" value="6,722" change="Across 6 tenants" changeType="neutral" icon={<Users size={18} />} />
                <StatCard label="Active Sessions" value="2,841" change="99.2% MFA" changeType="positive" icon={<Key size={18} />} />
                <StatCard label="Audit Events (24h)" value="14,286" change="2 high-risk" changeType="negative" icon={<FileText size={18} />} />
            </div>

            <Tabs tabs={['SSO', 'SCIM', 'RBAC / ABAC', 'Permission Graph', 'Audit Log', 'JWT Config']} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'SSO' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Lock size={16} /> SSO Providers (SAML + OIDC)</h3>
                    <DataTable
                        columns={['Provider', 'Protocol', 'Tenants', 'Last Sync', 'Status', 'Actions']}
                        rows={ssoProviders.map(p => [
                            <span className="font-medium text-grey-200">{p.name}</span>,
                            <Badge variant={p.protocol === 'SAML 2.0' ? 'purple' : 'info'}>{p.protocol}</Badge>,
                            `${p.tenants} tenant${p.tenants > 1 ? 's' : ''}`,
                            <span className="text-xs text-grey-500">{p.lastSync}</span>,
                            <Badge variant={p.status === 'Active' ? 'success' : 'warning'}>{p.status}</Badge>,
                            <button className="text-xs text-brand-400 hover:text-brand-300">Configure</button>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'SCIM' && (
                <div className="space-y-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><UserCheck size={16} /> SCIM 2.0 User Provisioning</h3>
                        <div className="grid grid-cols-3 gap-4 mb-5">
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-xl font-bold text-grey-100">842</p>
                                <p className="text-xs text-grey-500">Auto-provisioned (30d)</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-xl font-bold text-grey-100">24</p>
                                <p className="text-xs text-grey-500">Auto-deprovisioned</p>
                            </div>
                            <div className="p-3 bg-grey-800/30 rounded-lg text-center">
                                <p className="text-xl font-bold text-accent-emerald">99.8%</p>
                                <p className="text-xs text-grey-500">Sync Success Rate</p>
                            </div>
                        </div>
                        <DataTable
                            columns={['Method', 'Endpoint', 'Description', 'Status']}
                            rows={scimEndpoints.map(e => [
                                <Badge variant={e.method === 'GET' ? 'info' : e.method === 'POST' ? 'success' : e.method === 'DELETE' ? 'danger' : 'warning'}>{e.method}</Badge>,
                                <span className="font-mono text-xs text-accent-cyan">{e.path}</span>,
                                <span className="text-grey-300">{e.description}</span>,
                                <Badge variant="success">{e.status}</Badge>,
                            ])}
                        />
                    </div>
                </div>
            )}

            {activeTab === 'RBAC / ABAC' && (
                <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                    <div className="lg:col-span-2 gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Shield size={16} /> Roles & Permissions</h3>
                        <DataTable
                            columns={['Role', 'Users', 'Permissions', 'Scope', 'Model', 'Actions']}
                            rows={roles.map(r => [
                                <span className="font-medium text-grey-200">{r.role}</span>,
                                r.users,
                                r.permissions,
                                <Badge variant={r.scope === 'Global' ? 'danger' : r.scope === 'Tenant' ? 'purple' : 'info'}>{r.scope}</Badge>,
                                <Badge variant={r.type.includes('ABAC') ? 'warning' : 'default'}>{r.type}</Badge>,
                                <button className="text-xs text-brand-400 hover:text-brand-300">Edit</button>,
                            ])}
                        />
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">ABAC Policy Engine</h3>
                        <div className="bg-grey-900/50 rounded-lg p-3 font-mono text-xs space-y-1 mb-4">
                            <div className="text-grey-500">// Example ABAC policy (JSON)</div>
                            <div className="text-accent-cyan">{'{'}</div>
                            <div className="pl-3 text-grey-300">"effect": <span className="text-accent-emerald">"allow"</span>,</div>
                            <div className="pl-3 text-grey-300">"action": <span className="text-accent-emerald">"read"</span>,</div>
                            <div className="pl-3 text-grey-300">"resource": <span className="text-accent-emerald">"hrm:payroll:*"</span>,</div>
                            <div className="pl-3 text-grey-300">"condition": {'{'}</div>
                            <div className="pl-6 text-grey-300">"department": <span className="text-accent-amber">{"${user.department}"}</span>,</div>
                            <div className="pl-6 text-grey-300">"level": <span className="text-accent-amber">{">= manager"}</span></div>
                            <div className="pl-3 text-grey-300">{'}'}</div>
                            <div className="text-accent-cyan">{'}'}</div>
                        </div>
                        <h4 className="text-xs font-semibold text-grey-500 uppercase mb-2">Permission Inheritance</h4>
                        <div className="space-y-2">
                            {['Role → Group → User', 'Deny overrides Allow', 'Tenant scope limits Global', 'ABAC evaluated after RBAC'].map(r => (
                                <div key={r} className="flex items-center gap-2 text-sm text-grey-400">
                                    <GitBranch size={12} className="text-brand-400" />
                                    {r}
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Permission Graph' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Network size={16} /> Permission Graph Visualization</h3>
                    <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
                        <div className="space-y-3">
                            <h4 className="text-xs font-semibold text-grey-500 uppercase">Roles</h4>
                            {roles.slice(0, 5).map(r => (
                                <div key={r.role} className="p-3 rounded-lg border border-grey-800 hover:border-brand-500/50 cursor-pointer transition-colors">
                                    <div className="flex items-center justify-between">
                                        <span className="text-sm font-medium text-grey-200">{r.role}</span>
                                        <span className="text-xs text-grey-500">{r.permissions} perms</span>
                                    </div>
                                    <ProgressBar value={r.permissions} max={248} color="bg-brand-500" size="sm" />
                                </div>
                            ))}
                        </div>
                        <div className="space-y-3">
                            <h4 className="text-xs font-semibold text-grey-500 uppercase">Groups</h4>
                            {['Engineering', 'Finance', 'HR', 'Sales', 'Executive'].map((g, i) => (
                                <div key={g} className="p-3 rounded-lg border border-grey-800 hover:border-accent-violet/50 cursor-pointer transition-colors">
                                    <div className="flex items-center justify-between">
                                        <span className="text-sm font-medium text-grey-200">{g}</span>
                                        <span className="text-xs text-grey-500">{[142, 24, 12, 86, 8][i]} users</span>
                                    </div>
                                </div>
                            ))}
                        </div>
                        <div className="space-y-3">
                            <h4 className="text-xs font-semibold text-grey-500 uppercase">Resources</h4>
                            {['crm:*', 'erp:finance:*', 'hcm:payroll:read', 'data:bi:*', 'ai:agents:execute'].map(r => (
                                <div key={r} className="p-3 rounded-lg border border-grey-800">
                                    <span className="font-mono text-xs text-accent-cyan">{r}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Audit Log' && (
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Eye size={16} /> Identity Audit Log</h3>
                    <div className="text-xs text-grey-500 mb-4">Schema: actor, action, resource, timestamp, metadata — Immutable append-only log</div>
                    <DataTable
                        columns={['Actor', 'Action', 'Resource', 'Time', 'Risk']}
                        rows={auditEvents.map(e => [
                            <span className="font-mono text-xs text-grey-300">{e.actor}</span>,
                            <Badge variant={e.action.includes('failed') || e.action.includes('escalate') ? 'danger' : e.action.includes('revoke') || e.action.includes('rotate') ? 'warning' : 'info'}>
                                {e.action}
                            </Badge>,
                            <span className="text-xs text-grey-400 max-w-xs truncate block">{e.resource}</span>,
                            <span className="text-xs text-grey-500 flex items-center gap-1"><Clock size={10} />{e.time}</span>,
                            <Badge variant={e.risk === 'High' ? 'danger' : e.risk === 'Medium' ? 'warning' : e.risk === 'Low' ? 'info' : 'default'}>{e.risk}</Badge>,
                        ])}
                    />
                </div>
            )}

            {activeTab === 'JWT Config' && (
                <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4 flex items-center gap-2"><Key size={16} /> JWT Configuration</h3>
                        <div className="space-y-3">
                            {[
                                { param: 'Algorithm', value: 'RS256' },
                                { param: 'Access Token TTL', value: '15 minutes' },
                                { param: 'Refresh Token TTL', value: '7 days' },
                                { param: 'Issuer', value: 'https://id.greysuite.io' },
                                { param: 'Audience', value: 'greysuite-api' },
                                { param: 'Signing Key', value: 'jwt_signing_key_v42' },
                                { param: 'Key Rotation', value: 'Every 30 days' },
                            ].map(c => (
                                <div key={c.param} className="flex items-center justify-between p-3 rounded-lg bg-grey-800/30">
                                    <span className="text-sm text-grey-300">{c.param}</span>
                                    <span className="font-mono text-xs text-accent-cyan">{c.value}</span>
                                </div>
                            ))}
                        </div>
                    </div>
                    <div className="gs-card p-5">
                        <h3 className="gs-section-title mb-4">JWT Claims Structure</h3>
                        <div className="bg-grey-900/50 rounded-lg p-4 font-mono text-xs space-y-1">
                            <div className="text-accent-cyan">{'{'}</div>
                            <div className="pl-3 text-grey-400">"sub": <span className="text-accent-emerald">"usr_a1b2c3d4"</span>,</div>
                            <div className="pl-3 text-grey-400">"tenant_id": <span className="text-accent-emerald">"TNT-001"</span>,</div>
                            <div className="pl-3 text-grey-400">"email": <span className="text-accent-emerald">"admin@acme.com"</span>,</div>
                            <div className="pl-3 text-grey-400">"roles": [<span className="text-accent-emerald">"tenant_admin"</span>],</div>
                            <div className="pl-3 text-grey-400">"groups": [<span className="text-accent-emerald">"engineering"</span>],</div>
                            <div className="pl-3 text-grey-400">"permissions": [<span className="text-accent-emerald">"crm:*"</span>, <span className="text-accent-emerald">"erp:read"</span>],</div>
                            <div className="pl-3 text-grey-400">"iss": <span className="text-accent-emerald">"https://id.greysuite.io"</span>,</div>
                            <div className="pl-3 text-grey-400">"aud": <span className="text-accent-emerald">"greysuite-api"</span>,</div>
                            <div className="pl-3 text-grey-400">"iat": <span className="text-accent-amber">1739180400</span>,</div>
                            <div className="pl-3 text-grey-400">"exp": <span className="text-accent-amber">1739181300</span></div>
                            <div className="text-accent-cyan">{'}'}</div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
