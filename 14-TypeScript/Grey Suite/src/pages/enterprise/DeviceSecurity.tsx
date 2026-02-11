import { useState } from 'react';
import {
    Smartphone, Shield, Lock, Eye, AlertTriangle, CheckCircle2, Users,
    Monitor, Fingerprint, Globe, Key, RefreshCw, Search,
    Wifi, HardDrive, Zap, Settings, FileText, Clock, Activity
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar, MiniSparkline } from '../../components/ui';

export function DeviceSecurity() {
    const [activeTab, setActiveTab] = useState('Devices');

    const tabs = ['Devices', 'Compliance', 'Conditional Access', 'Threat Detection', 'Identity Protection'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Device, Identity & Security Management"
                subtitle="Device compliance, conditional access, endpoint management, threat detection, and identity protection workflows"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <FileText size={14} /> Reports
                        </button>
                        <button className="gs-btn-primary">
                            <Shield size={14} /> Security Dashboard
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Managed Devices" value="18,421" change="+842 this month" changeType="positive" icon={<Smartphone size={18} />} />
                <StatCard label="Compliance Rate" value="96.8%" change="+1.2%" changeType="positive" icon={<CheckCircle2 size={18} />} />
                <StatCard label="Threats Blocked (24h)" value="1,284" change="-14%" changeType="positive" icon={<Shield size={18} />} />
                <StatCard label="Identity Risk Score" value="Low" change="12 at-risk users" changeType="neutral" icon={<Fingerprint size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Devices' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Endpoint Management (UEM)</h3>
                        <div className="grid grid-cols-5 gap-4 mb-6">
                            {[
                                { platform: 'Windows', count: 8421, managed: 8210, icon: <Monitor size={20} /> },
                                { platform: 'macOS', count: 4284, managed: 4120, icon: <Monitor size={20} /> },
                                { platform: 'iOS', count: 3842, managed: 3684, icon: <Smartphone size={20} /> },
                                { platform: 'Android', count: 1284, managed: 1042, icon: <Smartphone size={20} /> },
                                { platform: 'Linux', count: 590, managed: 482, icon: <Monitor size={20} /> },
                            ].map(p => (
                                <div key={p.platform} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50 text-center">
                                    <div className="text-brand-400 mx-auto mb-2 flex justify-center">{p.icon}</div>
                                    <p className="text-sm font-semibold text-grey-200">{p.platform}</p>
                                    <p className="text-2xl font-bold text-grey-100 mt-1">{p.count.toLocaleString()}</p>
                                    <div className="mt-2">
                                        <ProgressBar value={(p.managed / p.count) * 100} color="bg-accent-emerald" />
                                        <p className="text-[10px] text-grey-500 mt-1">{p.managed.toLocaleString()} managed ({Math.round((p.managed / p.count) * 100)}%)</p>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>

                    <DataTable
                        columns={['Device', 'User', 'Platform', 'OS Version', 'Encryption', 'Last Check-in', 'Compliance', 'Status']}
                        rows={[
                            ['DESKTOP-SC001', 'Sarah Chen', 'Windows 11', '23H2 (22631)', <Badge variant="success">BitLocker</Badge>, '2m ago', <Badge variant="success">Compliant</Badge>, <Badge variant="success">Active</Badge>],
                            ['MBP-JW-2024', 'James Wilson', 'macOS 15', 'Sequoia 15.3', <Badge variant="success">FileVault</Badge>, '5m ago', <Badge variant="success">Compliant</Badge>, <Badge variant="success">Active</Badge>],
                            ['iPhone-ML-14P', 'Maria Lopez', 'iOS 18', '18.3.1', <Badge variant="success">Hardware</Badge>, '1h ago', <Badge variant="success">Compliant</Badge>, <Badge variant="success">Active</Badge>],
                            ['DESKTOP-AK042', 'Alex Kumar', 'Windows 11', '22H2 (22621)', <Badge variant="success">BitLocker</Badge>, '3h ago', <Badge variant="warning">Non-Compliant</Badge>, <Badge variant="warning">Out of date</Badge>],
                            ['Pixel-RG-8', 'Rachel Green', 'Android 15', 'QPR1', <Badge variant="success">Encrypted</Badge>, '2d ago', <Badge variant="danger">Non-Compliant</Badge>, <Badge variant="danger">Stale</Badge>],
                        ]}
                    />

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Device Actions</h3>
                        <div className="grid grid-cols-4 gap-3">
                            {[
                                { action: 'Remote Lock', desc: 'Lock device immediately', risk: 'Low' },
                                { action: 'Remote Wipe', desc: 'Factory reset — erase all data', risk: 'Critical' },
                                { action: 'Selective Wipe', desc: 'Remove only corporate data', risk: 'Medium' },
                                { action: 'Locate Device', desc: 'GPS location (mobile only)', risk: 'Low' },
                                { action: 'Force Sync', desc: 'Push latest policies now', risk: 'Low' },
                                { action: 'Retire', desc: 'Remove from management', risk: 'Medium' },
                                { action: 'Restart', desc: 'Force device restart', risk: 'Low' },
                                { action: 'Collect Diagnostics', desc: 'Pull logs for analysis', risk: 'Low' },
                            ].map(a => (
                                <div key={a.action} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50 hover:border-brand-500/50 cursor-pointer transition-colors">
                                    <p className="text-sm font-semibold text-grey-200 mb-1">{a.action}</p>
                                    <p className="text-xs text-grey-500 mb-2">{a.desc}</p>
                                    <Badge variant={a.risk === 'Critical' ? 'danger' : a.risk === 'Medium' ? 'warning' : 'default'}>{a.risk} Risk</Badge>
                                </div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Compliance' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Device Compliance Policies</h3>
                        <DataTable
                            columns={['Policy', 'Platform', 'Requirements', 'Devices', 'Compliant', 'Non-Compliant', 'Grace Period']}
                            rows={[
                                ['Corporate Windows', 'Windows', 'BitLocker, TPM 2.0, Win 11 22H2+, Defender active', '8,421', <Badge variant="success">8,210 (97.5%)</Badge>, <Badge variant="danger">211</Badge>, '72h'],
                                ['Corporate macOS', 'macOS', 'FileVault, macOS 14+, Gatekeeper, SIP enabled', '4,284', <Badge variant="success">4,120 (96.2%)</Badge>, <Badge variant="danger">164</Badge>, '72h'],
                                ['Mobile iOS', 'iOS', 'Passcode 6+, iOS 17+, not jailbroken, managed', '3,842', <Badge variant="success">3,684 (95.9%)</Badge>, <Badge variant="danger">158</Badge>, '24h'],
                                ['Mobile Android', 'Android', 'Screen lock, Android 13+, encrypted, Play Protect', '1,284', <Badge variant="success">1,042 (81.2%)</Badge>, <Badge variant="danger">242</Badge>, '24h'],
                                ['BYOD Baseline', 'All', 'Screen lock, encryption, OS not EOL, no root/jailbreak', '2,840', <Badge variant="success">2,684 (94.5%)</Badge>, <Badge variant="danger">156</Badge>, '48h'],
                            ]}
                        />
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Compliance Policy Engine</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Compliance Evaluation Pipeline
Device Check-in → Collect Inventory
  → OS Version Check ────────→ ✅/❌
  → Encryption Status ───────→ ✅/❌
  → Antivirus / EDR Active ──→ ✅/❌
  → Jailbreak / Root Detect ──→ ✅/❌
  → Password Policy ─────────→ ✅/❌
  → Required Apps Installed ──→ ✅/❌
  → Patch Level ─────────────→ ✅/❌

If ANY check fails:
  1. Mark device non-compliant
  2. Start grace period timer
  3. Notify user (email + push)
  4. If grace period expires:
     → Block corporate resource access
     → Conditional Access enforces this
     → Remediation instructions sent
     → Escalate to IT after 7 days`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Conditional Access' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Conditional Access Policies</h3>
                        <DataTable
                            columns={['Policy', 'Signals', 'Grant Controls', 'Session Controls', 'Users Affected', 'Status']}
                            rows={[
                                [
                                    'Require MFA for all users',
                                    'All cloud apps',
                                    'Require MFA',
                                    'Sign-in frequency: 24h',
                                    '12,400',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                                [
                                    'Block legacy authentication',
                                    'Exchange ActiveSync, IMAP, POP3',
                                    'Block access',
                                    '—',
                                    '12,400',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                                [
                                    'Require compliant device',
                                    'All apps + non-compliant device',
                                    'Require compliant device',
                                    'App-enforced restrictions',
                                    '12,400',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                                [
                                    'Block high-risk sign-ins',
                                    'Sign-in risk = High',
                                    'Block access',
                                    '—',
                                    '12,400',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                                [
                                    'Require MFA for risky users',
                                    'User risk = Medium+',
                                    'Require MFA + password change',
                                    'Sign-in frequency: 1h',
                                    '12,400',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                                [
                                    'Restricted access from untrusted IPs',
                                    'Outside corporate + VPN IPs',
                                    'Limited access (web only)',
                                    'No download, no print',
                                    '12,400',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                                [
                                    'Executive protection',
                                    'Executive group + any sign-in',
                                    'Require phishing-resistant MFA',
                                    'Continuous access evaluation',
                                    '42',
                                    <Badge variant="success">Enforced</Badge>,
                                ],
                            ]}
                        />
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Access Decision Engine</h3>
                        <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                            <pre>{`// Conditional Access Evaluation Flow
Sign-in Request
├── Collect Signals:
│   ├── User identity + group membership
│   ├── IP address + geolocation
│   ├── Device state (compliant? managed?)
│   ├── Application being accessed
│   ├── Sign-in risk (Identity Protection)
│   ├── User risk (compromised probability)
│   └── Client app (browser, mobile, desktop)
│
├── Evaluate Policies (AND logic within, OR between):
│   ├── Policy 1: conditions met? → Apply controls
│   ├── Policy 2: conditions met? → Apply controls
│   └── Policy N: conditions met? → Apply controls
│
├── Grant Decision:
│   ├── ALLOW (all controls satisfied)
│   ├── BLOCK (any block policy matched)
│   └── REQUIRE (MFA, compliant device, etc.)
│
└── Session Controls:
    ├── Sign-in frequency
    ├── Persistent browser session
    ├── App-enforced restrictions
    └── Continuous Access Evaluation (CAE)
        └── Near-real-time revocation on:
            - User disabled
            - Password changed
            - MFA revoked
            - IP moved to blocked range
            - Admin-initiated revoke`}</pre>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Threat Detection' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-4 gap-4">
                        <StatCard label="Alerts (24h)" value="284" change="-12%" changeType="positive" icon={<AlertTriangle size={18} />} />
                        <StatCard label="Critical Threats" value="3" change="Requires attention" changeType="negative" icon={<Shield size={18} />} />
                        <StatCard label="MTTD" value="4.2m" change="-1.8m" changeType="positive" icon={<Clock size={18} />} />
                        <StatCard label="MTTR" value="18m" change="-6m" changeType="positive" icon={<RefreshCw size={18} />} />
                    </div>

                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Threat Intelligence Dashboard</h3>
                        <DataTable
                            columns={['Threat', 'Severity', 'Category', 'Source', 'Affected', 'Detection', 'Status', 'Timestamp']}
                            rows={[
                                ['Brute force attack on admin account', <Badge variant="danger">Critical</Badge>, 'Identity', 'Sign-in logs', '1 user', 'ML anomaly', <Badge variant="warning">Investigating</Badge>, '09:42 UTC'],
                                ['Malware detected on endpoint', <Badge variant="danger">Critical</Badge>, 'Endpoint', 'EDR agent', '1 device', 'Signature + behavioral', <Badge variant="warning">Contained</Badge>, '09:38 UTC'],
                                ['Suspicious email forwarding rule', <Badge variant="danger">Critical</Badge>, 'Email', 'Exchange audit', '1 mailbox', 'Rule engine', <Badge variant="info">In progress</Badge>, '09:15 UTC'],
                                ['Impossible travel (2 countries in 10m)', <Badge variant="warning">High</Badge>, 'Identity', 'Sign-in logs', '1 user', 'Geo-velocity', <Badge variant="success">Auto-remediated</Badge>, '08:52 UTC'],
                                ['Mass file download detected', <Badge variant="warning">High</Badge>, 'Data', 'DLP engine', '1 user', 'Threshold rule', <Badge variant="info">In progress</Badge>, '08:30 UTC'],
                                ['Anomalous API call pattern', <Badge variant="info">Medium</Badge>, 'App', 'API gateway', '1 service', 'ML baseline', <Badge variant="success">Resolved</Badge>, '07:45 UTC'],
                            ]}
                        />
                    </div>

                    <div className="grid grid-cols-3 gap-4">
                        {[
                            { label: 'Threats by Category', data: [42, 28, 14, 8, 6], categories: ['Identity', 'Endpoint', 'Email', 'Data', 'App'], color: '#f43f5e' },
                            { label: 'Alert Volume (7d)', data: [312, 284, 296, 342, 318, 284, 284], categories: [], color: '#1a8fe0' },
                            { label: 'Auto-Remediation Rate', data: [62, 64, 68, 72, 74, 78, 82], categories: [], color: '#10b981' },
                        ].map(m => (
                            <div key={m.label} className="gs-card p-4">
                                <p className="text-xs text-grey-500 mb-2">{m.label}</p>
                                <MiniSparkline data={m.data} color={m.color} height={40} />
                                <p className="text-lg font-bold text-grey-200 mt-1">{m.data[m.data.length - 1]}{m.label.includes('Rate') ? '%' : ''}</p>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Identity Protection' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Identity Protection Workflows</h3>

                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { risk: 'High Risk Users', count: 3, desc: 'Confirmed compromised credential', action: 'Forced password reset + MFA re-enrollment' },
                                { risk: 'Medium Risk Users', count: 12, desc: 'Anomalous sign-in patterns', action: 'Required MFA challenge + monitoring' },
                                { risk: 'Low Risk Users', count: 42, desc: 'Minor anomalies detected', action: 'Enhanced monitoring + risk-based MFA' },
                            ].map(r => (
                                <div key={r.risk} className={`bg-grey-800/50 rounded-lg p-4 border ${r.risk.includes('High') ? 'border-rose-500/50' :
                                        r.risk.includes('Medium') ? 'border-amber-500/50' : 'border-grey-700/50'
                                    }`}>
                                    <div className="flex items-center justify-between mb-3">
                                        <span className="text-sm font-semibold text-grey-200">{r.risk}</span>
                                        <span className="text-2xl font-bold text-grey-100">{r.count}</span>
                                    </div>
                                    <p className="text-xs text-grey-500 mb-2">{r.desc}</p>
                                    <p className="text-xs text-brand-400">{r.action}</p>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Risk Detection Methods</h3>
                            <div className="space-y-3">
                                {[
                                    { detection: 'Leaked Credentials', type: 'User risk', desc: 'Credentials found in dark web dumps', method: 'Feed from MSTIC + HIBP' },
                                    { detection: 'Impossible Travel', type: 'Sign-in risk', desc: 'Sign-ins from geographically impossible locations', method: 'Geo-velocity analysis' },
                                    { detection: 'Anonymous IP', type: 'Sign-in risk', desc: 'Sign-in from Tor/VPN exit nodes', method: 'IP reputation database' },
                                    { detection: 'Password Spray', type: 'Sign-in risk', desc: 'Similar failed attempts across many accounts', method: 'Distributed pattern detection' },
                                    { detection: 'Token Anomaly', type: 'Sign-in risk', desc: 'Suspicious token usage patterns', method: 'ML behavioral analysis' },
                                    { detection: 'Unfamiliar Sign-in', type: 'Sign-in risk', desc: 'New device/location/IP combination', method: 'User behavior baseline' },
                                    { detection: 'MFA Fatigue', type: 'Sign-in risk', desc: 'Repeated MFA prompts (push bombing)', method: 'Rate + pattern analysis' },
                                ].map(d => (
                                    <div key={d.detection} className="bg-grey-800/50 rounded-lg p-3 border border-grey-700/50">
                                        <div className="flex items-center justify-between mb-1">
                                            <span className="text-sm text-grey-200">{d.detection}</span>
                                            <Badge variant={d.type === 'User risk' ? 'danger' : 'warning'}>{d.type}</Badge>
                                        </div>
                                        <p className="text-xs text-grey-500">{d.desc}</p>
                                        <p className="text-[10px] text-grey-600 mt-1">Method: {d.method}</p>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Auto-Remediation Workflows</h3>
                            <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                                <pre>{`// Identity Protection Auto-Remediation
Risk Event Detected
├── Sign-in Risk = HIGH
│   ├── Block sign-in
│   ├── Revoke existing sessions (CAE)
│   ├── Alert SOC team
│   └── Create incident ticket
│
├── Sign-in Risk = MEDIUM
│   ├── Require MFA step-up
│   ├── Log enhanced audit data
│   └── Add to watchlist (7 days)
│
├── User Risk = HIGH
│   ├── Force password change at next sign-in
│   ├── Require MFA re-registration
│   ├── Revoke all refresh tokens
│   ├── Disable account (if leaked creds)
│   ├── Alert user's manager
│   └── Create P1 incident
│
└── User Risk = MEDIUM
    ├── Require secure password change
    ├── Require MFA verification
    ├── Enable enhanced session controls
    └── Weekly review by security team

// All actions logged to immutable audit trail
// SOC dashboard shows real-time risk posture`}</pre>
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
