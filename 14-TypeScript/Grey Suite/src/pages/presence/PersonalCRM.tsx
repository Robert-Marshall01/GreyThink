import { PageHeader, StatCard, Badge } from '../../components/ui';
import { Contact, Bell, MessageSquare, Users, Plus, Calendar } from 'lucide-react';

export function PersonalCRM() {
    return (
        <div>
            <PageHeader
                title="Personal CRM"
                subtitle="Contact management, reminders & relationship tracking"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> Add Contact</button>}
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Total Contacts" value={486} icon={<Contact size={18} />} />
                <StatCard label="Due Today" value={4} change="Follow-ups" changeType="neutral" icon={<Bell size={18} />} />
                <StatCard label="Interactions (7d)" value={28} change="+6 vs last week" changeType="positive" icon={<MessageSquare size={18} />} />
                <StatCard label="Groups" value={12} icon={<Users size={18} />} />
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                {/* Reminders */}
                <div className="gs-card p-5">
                    <h3 className="gs-section-title mb-4 flex items-center gap-2"><Bell size={16} /> Today's Follow-ups</h3>
                    <div className="space-y-3">
                        {[
                            { contact: 'Alex Morgan', action: 'Send proposal follow-up', time: '10:00 AM', priority: 'High' },
                            { contact: 'Priya Sharma', action: 'Schedule coffee chat', time: '2:00 PM', priority: 'Medium' },
                            { contact: 'Tom Baker', action: 'Share article on AI trends', time: '4:00 PM', priority: 'Low' },
                            { contact: 'Lisa Wang', action: 'Birthday message', time: 'Today', priority: 'Medium' },
                        ].map(r => (
                            <div key={r.contact} className="flex items-center justify-between p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors">
                                <div className="flex items-center gap-3">
                                    <div className="w-8 h-8 rounded-full bg-grey-700 flex items-center justify-center text-xs font-bold text-grey-300">
                                        {r.contact.split(' ').map(n => n[0]).join('')}
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{r.contact}</p>
                                        <p className="text-xs text-grey-500">{r.action}</p>
                                    </div>
                                </div>
                                <div className="flex items-center gap-2">
                                    <span className="text-xs text-grey-500">{r.time}</span>
                                    <button className="text-accent-emerald text-xs hover:underline">Done</button>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>

                {/* Contacts */}
                <div className="lg:col-span-2 gs-card p-5">
                    <h3 className="gs-section-title mb-4">Key Contacts</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                        {[
                            { name: 'Alex Morgan', title: 'CTO at DataPrime', lastContact: '3 days ago', strength: 'Strong', tags: ['Tech', 'Decision Maker'] },
                            { name: 'Priya Sharma', title: 'VP Product at CloudScale', lastContact: '1 week ago', strength: 'Strong', tags: ['Product', 'Partner'] },
                            { name: 'Tom Baker', title: 'CEO at NexGen', lastContact: '2 weeks ago', strength: 'Medium', tags: ['Executive', 'Investor'] },
                            { name: 'Lisa Wang', title: 'Head of AI at Vertex', lastContact: '5 days ago', strength: 'Strong', tags: ['AI/ML', 'Mentor'] },
                            { name: 'James Park', title: 'Director Eng at Apex', lastContact: '3 weeks ago', strength: 'Weak', tags: ['Engineering'] },
                            { name: 'Rachel Torres', title: 'VP Sales at Pinnacle', lastContact: '1 day ago', strength: 'Strong', tags: ['Sales', 'Client'] },
                        ].map(c => (
                            <div key={c.name} className="p-3 rounded-lg border border-grey-800 hover:border-grey-700 transition-colors cursor-pointer">
                                <div className="flex items-center gap-3 mb-2">
                                    <div className="w-10 h-10 rounded-full bg-gradient-to-br from-grey-600 to-grey-700 flex items-center justify-center text-xs font-bold text-grey-200">
                                        {c.name.split(' ').map(n => n[0]).join('')}
                                    </div>
                                    <div>
                                        <p className="text-sm font-medium text-grey-200">{c.name}</p>
                                        <p className="text-xs text-grey-500">{c.title}</p>
                                    </div>
                                </div>
                                <div className="flex items-center justify-between text-xs">
                                    <div className="flex gap-1">{c.tags.map(t => <Badge key={t}>{t}</Badge>)}</div>
                                    <span className={`${c.strength === 'Strong' ? 'text-accent-emerald' : c.strength === 'Medium' ? 'text-accent-amber' : 'text-accent-rose'}`}>
                                        {c.strength}
                                    </span>
                                </div>
                                <p className="text-xs text-grey-500 mt-2 flex items-center gap-1"><Calendar size={10} /> Last contact: {c.lastContact}</p>
                            </div>
                        ))}
                    </div>
                </div>
            </div>
        </div>
    );
}
