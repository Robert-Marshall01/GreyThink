import { useState } from 'react';
import {
    CheckSquare, Calendar, List, ClipboardList, BarChart3, Users, Clock,
    Plus, Filter, ArrowRight, Star, Flag, CircleDot, Grip,
    MessageSquare, ChevronRight, Zap, Target
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar } from '../../components/ui';

export function TaskPlanning() {
    const [activeTab, setActiveTab] = useState('Kanban');

    const tabs = ['Kanban', 'Personal Tasks', 'Scheduling', 'Lists', 'Forms & Surveys'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Organizational Task & Planning Tools"
                subtitle="Kanban boards, personal task management, appointment scheduling, custom lists, and survey builder"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Filter size={14} /> Filter
                        </button>
                        <button className="gs-btn-primary">
                            <Plus size={14} /> New Board
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Active Tasks" value="8,421" change="+342 this week" changeType="positive" icon={<CheckSquare size={18} />} />
                <StatCard label="Completion Rate" value="87.4%" change="+3.2%" changeType="positive" icon={<Target size={18} />} />
                <StatCard label="Overdue Tasks" value="124" change="-18" changeType="positive" icon={<Clock size={18} />} />
                <StatCard label="Active Boards" value="284" change="+12" changeType="positive" icon={<ClipboardList size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Kanban' && (
                <div className="space-y-6">
                    <div className="flex items-center justify-between">
                        <div className="flex items-center gap-3">
                            <h3 className="text-sm font-semibold text-grey-300">Product Launch — Q1 2026</h3>
                            <Badge variant="info">Sprint 14</Badge>
                        </div>
                        <div className="flex gap-2">
                            <button className="gs-btn-ghost text-xs"><Users size={12} /> 12 members</button>
                            <button className="gs-btn-ghost text-xs"><Zap size={12} /> Automations (4)</button>
                        </div>
                    </div>

                    <div className="grid grid-cols-5 gap-4">
                        {[
                            {
                                column: 'Backlog', color: 'border-grey-600', count: 8,
                                cards: [
                                    { title: 'Implement SSO for partner portal', priority: 'Medium', assignee: 'SC', labels: ['Backend', 'Security'], points: 5 },
                                    { title: 'Design mobile onboarding flow', priority: 'Low', assignee: 'ML', labels: ['Design', 'Mobile'], points: 3 },
                                    { title: 'API rate limiting per tenant', priority: 'High', assignee: 'JW', labels: ['Backend', 'Platform'], points: 8 },
                                ],
                            },
                            {
                                column: 'To Do', color: 'border-brand-500', count: 5,
                                cards: [
                                    { title: 'Migrate billing to Stripe v3', priority: 'High', assignee: 'AK', labels: ['Backend', 'Payments'], points: 8 },
                                    { title: 'Add export to CSV for reports', priority: 'Medium', assignee: 'RG', labels: ['Frontend', 'Data'], points: 3 },
                                ],
                            },
                            {
                                column: 'In Progress', color: 'border-amber-500', count: 4,
                                cards: [
                                    { title: 'Real-time notification system', priority: 'High', assignee: 'SC', labels: ['Backend', 'WebSocket'], points: 13 },
                                    { title: 'Dashboard performance audit', priority: 'Critical', assignee: 'JW', labels: ['Frontend', 'Perf'], points: 5 },
                                ],
                            },
                            {
                                column: 'In Review', color: 'border-violet-500', count: 3,
                                cards: [
                                    { title: 'Multi-language support (i18n)', priority: 'Medium', assignee: 'ML', labels: ['Frontend', 'i18n'], points: 8 },
                                    { title: 'Audit log compliance report', priority: 'High', assignee: 'AK', labels: ['Security', 'Compliance'], points: 5 },
                                ],
                            },
                            {
                                column: 'Done', color: 'border-emerald-500', count: 12,
                                cards: [
                                    { title: 'OAuth 2.1 migration complete', priority: 'High', assignee: 'SC', labels: ['Backend', 'Auth'], points: 8 },
                                    { title: 'Design system v2 rollout', priority: 'Medium', assignee: 'ML', labels: ['Design', 'UI'], points: 5 },
                                ],
                            },
                        ].map(col => (
                            <div key={col.column} className={`bg-grey-900/50 rounded-lg border-t-2 ${col.color}`}>
                                <div className="p-3 flex items-center justify-between">
                                    <div className="flex items-center gap-2">
                                        <span className="text-sm font-semibold text-grey-300">{col.column}</span>
                                        <span className="text-xs text-grey-500 bg-grey-800 px-1.5 py-0.5 rounded">{col.count}</span>
                                    </div>
                                    <button className="text-grey-500 hover:text-grey-300"><Plus size={14} /></button>
                                </div>
                                <div className="p-2 space-y-2">
                                    {col.cards.map(card => (
                                        <div key={card.title} className="bg-grey-800/80 rounded-lg p-3 border border-grey-700/50 hover:border-grey-600 cursor-pointer transition-colors">
                                            <div className="flex items-start justify-between mb-2">
                                                <p className="text-xs text-grey-200 font-medium leading-tight">{card.title}</p>
                                                <Grip size={12} className="text-grey-600 flex-shrink-0 mt-0.5" />
                                            </div>
                                            <div className="flex flex-wrap gap-1 mb-2">
                                                {card.labels.map(l => (
                                                    <span key={l} className="text-[10px] px-1.5 py-0.5 rounded bg-grey-700/50 text-grey-400">{l}</span>
                                                ))}
                                            </div>
                                            <div className="flex items-center justify-between">
                                                <div className="flex items-center gap-1">
                                                    <Flag size={10} className={
                                                        card.priority === 'Critical' ? 'text-rose-400' :
                                                            card.priority === 'High' ? 'text-amber-400' :
                                                                card.priority === 'Medium' ? 'text-brand-400' : 'text-grey-500'
                                                    } />
                                                    <span className="text-[10px] text-grey-500">{card.priority}</span>
                                                </div>
                                                <div className="flex items-center gap-2">
                                                    <span className="text-[10px] text-grey-500">{card.points}pt</span>
                                                    <div className="w-5 h-5 rounded-full bg-brand-500/20 flex items-center justify-center text-[9px] text-brand-400 font-semibold">{card.assignee}</div>
                                                </div>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Personal Tasks' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-3 gap-6">
                        <div className="col-span-2 gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">My Tasks</h3>
                            <div className="space-y-1">
                                {[
                                    { task: 'Review Q1 budget proposal', due: 'Today', priority: 'High', list: 'Work', completed: false, starred: true },
                                    { task: 'Prepare board presentation slides', due: 'Today', priority: 'Critical', list: 'Work', completed: false, starred: true },
                                    { task: 'Send partner onboarding documents', due: 'Tomorrow', priority: 'Medium', list: 'Work', completed: false, starred: false },
                                    { task: 'Schedule 1:1 with team leads', due: 'This week', priority: 'Medium', list: 'Work', completed: false, starred: false },
                                    { task: 'Complete security training', due: 'Feb 15', priority: 'Low', list: 'Personal Dev', completed: false, starred: false },
                                    { task: 'Update project roadmap in wiki', due: 'Feb 12', priority: 'Medium', list: 'Work', completed: false, starred: false },
                                    { task: 'Review and merge PR #1842', due: 'Today', priority: 'High', list: 'Engineering', completed: true, starred: false },
                                    { task: 'Submit expense reports', due: 'Yesterday', priority: 'Medium', list: 'Admin', completed: true, starred: false },
                                ].map(t => (
                                    <div key={t.task} className={`flex items-center gap-3 p-3 rounded-lg hover:bg-grey-800/50 transition-colors group ${t.completed ? 'opacity-50' : ''}`}>
                                        <button className={`w-5 h-5 rounded border flex-shrink-0 flex items-center justify-center ${t.completed ? 'bg-accent-emerald border-accent-emerald' : 'border-grey-600 hover:border-brand-400'}`}>
                                            {t.completed && <CheckSquare size={12} className="text-white" />}
                                        </button>
                                        <div className="flex-1 min-w-0">
                                            <p className={`text-sm ${t.completed ? 'line-through text-grey-500' : 'text-grey-200'}`}>{t.task}</p>
                                            <div className="flex items-center gap-2 mt-1">
                                                <span className={`text-[10px] ${t.due === 'Today' || t.due === 'Yesterday' ? 'text-amber-400' : 'text-grey-500'}`}>{t.due}</span>
                                                <span className="text-[10px] text-grey-600">•</span>
                                                <span className="text-[10px] text-grey-500">{t.list}</span>
                                            </div>
                                        </div>
                                        <Flag size={12} className={
                                            t.priority === 'Critical' ? 'text-rose-400' :
                                                t.priority === 'High' ? 'text-amber-400' :
                                                    t.priority === 'Medium' ? 'text-brand-400' : 'text-grey-600'
                                        } />
                                        <Star size={12} className={t.starred ? 'text-amber-400 fill-amber-400' : 'text-grey-600 opacity-0 group-hover:opacity-100'} />
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="space-y-4">
                            <div className="gs-card p-4">
                                <h3 className="text-sm font-semibold text-grey-300 mb-3">My Lists</h3>
                                <div className="space-y-2">
                                    {[
                                        { name: 'Work', count: 24, color: 'bg-brand-500' },
                                        { name: 'Engineering', count: 8, color: 'bg-violet-500' },
                                        { name: 'Personal Dev', count: 6, color: 'bg-emerald-500' },
                                        { name: 'Admin', count: 3, color: 'bg-amber-500' },
                                        { name: 'Someday/Maybe', count: 14, color: 'bg-grey-500' },
                                    ].map(l => (
                                        <div key={l.name} className="flex items-center gap-3 px-2 py-1.5 rounded hover:bg-grey-800/50 cursor-pointer">
                                            <div className={`w-2 h-2 rounded-full ${l.color}`} />
                                            <span className="text-sm text-grey-300 flex-1">{l.name}</span>
                                            <span className="text-xs text-grey-500">{l.count}</span>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            <div className="gs-card p-4">
                                <h3 className="text-sm font-semibold text-grey-300 mb-3">Smart Lists</h3>
                                <div className="space-y-2">
                                    {[
                                        { name: 'My Day', count: 4, icon: <CircleDot size={14} /> },
                                        { name: 'Important', count: 6, icon: <Star size={14} /> },
                                        { name: 'Planned', count: 18, icon: <Calendar size={14} /> },
                                        { name: 'Assigned to Me', count: 12, icon: <Users size={14} /> },
                                        { name: 'Overdue', count: 2, icon: <Clock size={14} /> },
                                    ].map(l => (
                                        <div key={l.name} className="flex items-center gap-3 px-2 py-1.5 rounded hover:bg-grey-800/50 cursor-pointer">
                                            <span className="text-brand-400">{l.icon}</span>
                                            <span className="text-sm text-grey-300 flex-1">{l.name}</span>
                                            <span className="text-xs text-grey-500">{l.count}</span>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Scheduling' && (
                <div className="space-y-6">
                    <div className="grid grid-cols-3 gap-6">
                        <div className="col-span-2 gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Booking Pages</h3>
                            <DataTable
                                columns={['Booking Type', 'Duration', 'Availability', 'Buffer', 'Bookings (30d)', 'Status']}
                                rows={[
                                    ['1:1 Meeting', '30 min', 'Mon–Fri 9am–5pm', '15 min', '142', <Badge variant="success">Active</Badge>],
                                    ['Team Standup', '15 min', 'Mon–Fri 9:00am', '5 min', '84', <Badge variant="success">Active</Badge>],
                                    ['Customer Demo', '60 min', 'Mon–Thu 10am–4pm', '30 min', '28', <Badge variant="success">Active</Badge>],
                                    ['Office Hours', '20 min', 'Wed 2pm–5pm', '10 min', '64', <Badge variant="success">Active</Badge>],
                                    ['Interview - Engineering', '45 min', 'Mon–Fri 10am–4pm', '15 min', '36', <Badge variant="success">Active</Badge>],
                                    ['Board Meeting', '120 min', 'First Monday, 2pm', '60 min', '4', <Badge variant="info">Restricted</Badge>],
                                ]}
                            />
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Scheduling Features</h3>
                            <div className="space-y-3">
                                {[
                                    { feature: 'Calendar Sync', desc: 'Google, Outlook, iCal' },
                                    { feature: 'Timezone Detection', desc: 'Auto-detect + override' },
                                    { feature: 'Round-Robin', desc: 'Distribute across team' },
                                    { feature: 'Collective Scheduling', desc: 'Find mutual availability' },
                                    { feature: 'Custom Questions', desc: 'Pre-meeting intake form' },
                                    { feature: 'Reminders', desc: 'Email + SMS + in-app' },
                                    { feature: 'Waitlist', desc: 'Auto-fill cancellations' },
                                    { feature: 'No-Show Tracking', desc: 'Auto-follow-up workflow' },
                                ].map(f => (
                                    <div key={f.feature} className="flex items-center justify-between py-1.5 border-b border-grey-800/50 last:border-0">
                                        <span className="text-sm text-grey-300">{f.feature}</span>
                                        <span className="text-xs text-grey-500">{f.desc}</span>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Lists' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Custom List Builder</h3>
                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { name: 'Issue Tracker', items: 1842, columns: 12, views: 5, template: 'Bug Tracking' },
                                { name: 'Asset Inventory', items: 4210, columns: 18, views: 4, template: 'Inventory' },
                                { name: 'Vendor Registry', items: 284, columns: 14, views: 3, template: 'CRM Lite' },
                            ].map(l => (
                                <div key={l.name} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <p className="text-sm font-semibold text-grey-200 mb-2">{l.name}</p>
                                    <div className="space-y-1 text-xs text-grey-400">
                                        <div className="flex justify-between"><span>Items</span><span className="text-grey-300">{l.items.toLocaleString()}</span></div>
                                        <div className="flex justify-between"><span>Columns</span><span className="text-grey-300">{l.columns}</span></div>
                                        <div className="flex justify-between"><span>Views</span><span className="text-grey-300">{l.views}</span></div>
                                        <div className="flex justify-between"><span>Template</span><span className="text-brand-400">{l.template}</span></div>
                                    </div>
                                </div>
                            ))}
                        </div>

                        <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Supported Column Types</h4>
                        <div className="grid grid-cols-4 gap-2">
                            {[
                                'Single Line Text', 'Multi-Line Text', 'Number', 'Currency',
                                'Date/Time', 'Choice (Single)', 'Choice (Multi)', 'Person/Group',
                                'Yes/No', 'Lookup', 'Calculated', 'Hyperlink',
                                'Image', 'Rating', 'Location', 'JSON',
                            ].map(t => (
                                <div key={t} className="text-xs px-3 py-2 bg-grey-800/50 rounded text-grey-400 border border-grey-700/50">{t}</div>
                            ))}
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Forms & Surveys' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Form & Survey Builder</h3>
                        <DataTable
                            columns={['Form', 'Type', 'Questions', 'Responses', 'Completion Rate', 'Branching', 'Status']}
                            rows={[
                                ['Employee Satisfaction Q1', <Badge variant="info">Survey</Badge>, '24', '1,842', <><ProgressBar value={78} color="bg-accent-emerald" /><span className="text-xs text-grey-400 mt-1">78%</span></>, <Badge variant="success">Yes</Badge>, <Badge variant="success">Active</Badge>],
                                ['IT Equipment Request', <Badge variant="purple">Form</Badge>, '12', '421', <><ProgressBar value={94} color="bg-accent-emerald" /><span className="text-xs text-grey-400 mt-1">94%</span></>, <Badge variant="default">No</Badge>, <Badge variant="success">Active</Badge>],
                                ['Customer NPS Survey', <Badge variant="info">Survey</Badge>, '8', '4,284', <><ProgressBar value={62} color="bg-amber-500" /><span className="text-xs text-grey-400 mt-1">62%</span></>, <Badge variant="success">Yes</Badge>, <Badge variant="success">Active</Badge>],
                                ['New Hire Onboarding Quiz', <Badge variant="warning">Quiz</Badge>, '20', '284', <><ProgressBar value={100} color="bg-accent-emerald" /><span className="text-xs text-grey-400 mt-1">100%</span></>, <Badge variant="success">Yes</Badge>, <Badge variant="success">Active</Badge>],
                                ['Event Registration', <Badge variant="purple">Form</Badge>, '10', '892', <><ProgressBar value={88} color="bg-accent-emerald" /><span className="text-xs text-grey-400 mt-1">88%</span></>, <Badge variant="default">No</Badge>, <Badge variant="success">Active</Badge>],
                                ['360° Performance Review', <Badge variant="info">Survey</Badge>, '32', '642', <><ProgressBar value={56} color="bg-amber-500" /><span className="text-xs text-grey-400 mt-1">56%</span></>, <Badge variant="success">Yes</Badge>, <Badge variant="warning">Closing Soon</Badge>],
                            ]}
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Question Types</h3>
                            <div className="grid grid-cols-2 gap-2">
                                {[
                                    'Short Answer', 'Long Answer', 'Multiple Choice', 'Checkbox',
                                    'Dropdown', 'Rating (1-5/1-10)', 'Likert Scale', 'Date Picker',
                                    'Net Promoter Score', 'Matrix / Grid', 'File Upload', 'Ranking',
                                    'Slider', 'Section Break', 'Conditional Logic', 'Signature',
                                ].map(q => (
                                    <div key={q} className="text-xs px-3 py-2 bg-grey-800/50 rounded text-grey-400 border border-grey-700/50">{q}</div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Survey Capabilities</h3>
                            <div className="space-y-3">
                                {[
                                    { feature: 'Conditional Branching', desc: 'Show/hide based on answers' },
                                    { feature: 'Response Piping', desc: 'Insert previous answers into text' },
                                    { feature: 'Anonymous Mode', desc: 'Hide respondent identity' },
                                    { feature: 'Email Invitations', desc: 'Track invited vs. responded' },
                                    { feature: 'Real-time Results', desc: 'Live dashboard with charts' },
                                    { feature: 'Export', desc: 'CSV, Excel, PDF, Power BI' },
                                    { feature: 'AI Summarization', desc: 'Auto-generate insights from responses' },
                                    { feature: 'Multi-Language', desc: 'Support 40+ languages' },
                                ].map(f => (
                                    <div key={f.feature} className="flex items-center justify-between py-1.5 border-b border-grey-800/50 last:border-0">
                                        <span className="text-sm text-grey-300">{f.feature}</span>
                                        <span className="text-xs text-grey-500">{f.desc}</span>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
