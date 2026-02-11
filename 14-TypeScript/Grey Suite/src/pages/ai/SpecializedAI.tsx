import { PageHeader, StatCard, Badge, MiniSparkline } from '../../components/ui';
import { Brain, Megaphone, Mic, FileText, TrendingUp, Zap } from 'lucide-react';

export function SpecializedAI() {
    return (
        <div>
            <PageHeader
                title="Specialized AI"
                subtitle="Marketing AI, Voice AI & Summarization AI"
            />

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
                {/* Marketing AI */}
                <div className="gs-card p-5">
                    <div className="flex items-center gap-3 mb-4">
                        <div className="w-10 h-10 rounded-xl bg-accent-rose/15 text-accent-rose flex items-center justify-center">
                            <Megaphone size={20} />
                        </div>
                        <div>
                            <h3 className="text-sm font-semibold text-grey-200">Marketing AI</h3>
                            <p className="text-xs text-grey-500">Content, campaigns & audience</p>
                        </div>
                    </div>
                    <div className="space-y-3">
                        {[
                            { feat: 'Content Generator', usage: 1240, status: 'Active' },
                            { feat: 'Ad Copy Optimizer', usage: 842, status: 'Active' },
                            { feat: 'Audience Segmenter', usage: 356, status: 'Active' },
                            { feat: 'A/B Test Analyzer', usage: 198, status: 'Beta' },
                        ].map(f => (
                            <div key={f.feat} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <span className="text-sm text-grey-300">{f.feat}</span>
                                <div className="flex items-center gap-2">
                                    <span className="text-xs text-grey-500">{f.usage} uses</span>
                                    <Badge variant={f.status === 'Beta' ? 'purple' : 'success'}>{f.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                    <MiniSparkline data={[120, 150, 180, 210, 190, 240, 280, 320, 310, 350]} height={40} color="#f43f5e" />
                </div>

                {/* Voice AI */}
                <div className="gs-card p-5">
                    <div className="flex items-center gap-3 mb-4">
                        <div className="w-10 h-10 rounded-xl bg-accent-cyan/15 text-accent-cyan flex items-center justify-center">
                            <Mic size={20} />
                        </div>
                        <div>
                            <h3 className="text-sm font-semibold text-grey-200">Voice AI</h3>
                            <p className="text-xs text-grey-500">Speech, synthesis & commands</p>
                        </div>
                    </div>
                    <div className="space-y-3">
                        {[
                            { feat: 'Voice Commands', usage: 8420, status: 'Active' },
                            { feat: 'Text-to-Speech', usage: 3210, status: 'Active' },
                            { feat: 'Voice Cloning', usage: 142, status: 'Beta' },
                            { feat: 'Real-time Translation', usage: 567, status: 'Active' },
                        ].map(f => (
                            <div key={f.feat} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <span className="text-sm text-grey-300">{f.feat}</span>
                                <div className="flex items-center gap-2">
                                    <span className="text-xs text-grey-500">{f.usage.toLocaleString()} uses</span>
                                    <Badge variant={f.status === 'Beta' ? 'purple' : 'success'}>{f.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                    <MiniSparkline data={[800, 920, 1040, 1100, 980, 1200, 1340, 1280, 1400, 1520]} height={40} color="#06b6d4" />
                </div>

                {/* Summarization AI */}
                <div className="gs-card p-5">
                    <div className="flex items-center gap-3 mb-4">
                        <div className="w-10 h-10 rounded-xl bg-accent-amber/15 text-accent-amber flex items-center justify-center">
                            <FileText size={20} />
                        </div>
                        <div>
                            <h3 className="text-sm font-semibold text-grey-200">Summarization AI</h3>
                            <p className="text-xs text-grey-500">Docs, meetings & research</p>
                        </div>
                    </div>
                    <div className="space-y-3">
                        {[
                            { feat: 'Document Summarizer', usage: 4280, status: 'Active' },
                            { feat: 'Meeting Notes', usage: 2140, status: 'Active' },
                            { feat: 'Research Digest', usage: 892, status: 'Active' },
                            { feat: 'Slack/Chat Recap', usage: 1560, status: 'Beta' },
                        ].map(f => (
                            <div key={f.feat} className="flex items-center justify-between p-2 rounded-lg hover:bg-grey-800/40 transition-colors">
                                <span className="text-sm text-grey-300">{f.feat}</span>
                                <div className="flex items-center gap-2">
                                    <span className="text-xs text-grey-500">{f.usage.toLocaleString()} uses</span>
                                    <Badge variant={f.status === 'Beta' ? 'purple' : 'success'}>{f.status}</Badge>
                                </div>
                            </div>
                        ))}
                    </div>
                    <MiniSparkline data={[420, 480, 520, 580, 610, 680, 720, 780, 820, 890]} height={40} color="#f59e0b" />
                </div>
            </div>

            {/* AI Usage Overview */}
            <div className="gs-card p-5">
                <h3 className="gs-section-title mb-4">AI Usage Overview</h3>
                <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
                    <StatCard label="Total AI Requests" value="48.2K" change="+32% MoM" changeType="positive" icon={<Brain size={18} />} />
                    <StatCard label="Avg Response Time" value="1.2s" change="-0.3s" changeType="positive" icon={<Zap size={18} />} />
                    <StatCard label="User Satisfaction" value="4.6/5" change="+0.2" changeType="positive" icon={<TrendingUp size={18} />} />
                    <StatCard label="Cost Savings" value="$124K" change="This quarter" changeType="neutral" icon={<TrendingUp size={18} />} />
                </div>
            </div>
        </div>
    );
}
