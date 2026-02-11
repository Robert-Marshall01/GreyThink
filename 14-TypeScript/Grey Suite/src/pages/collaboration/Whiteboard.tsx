import { PageHeader, Badge } from '../../components/ui';
import { PenTool, Users, Clock, Plus, Square, Circle, Type, ArrowUpRight, Minus } from 'lucide-react';

export function Whiteboard() {
    return (
        <div>
            <PageHeader
                title="Whiteboard"
                subtitle="Visual collaboration, diagramming & brainstorming"
                actions={<button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Board</button>}
            />

            {/* Canvas Preview */}
            <div className="gs-card mb-6 overflow-hidden">
                <div className="flex items-center justify-between px-4 py-2 border-b border-grey-800">
                    <div className="flex items-center gap-2">
                        <span className="text-sm font-medium text-grey-200">Architecture Diagram — v3</span>
                        <Badge variant="info">Shared</Badge>
                    </div>
                    <div className="flex items-center gap-1">
                        <div className="flex -space-x-2 mr-3">
                            {['SC', 'MJ', 'ER'].map(a => (
                                <div key={a} className="w-6 h-6 rounded-full bg-grey-700 border-2 border-grey-900 flex items-center justify-center text-[9px] font-bold text-grey-300">{a}</div>
                            ))}
                        </div>
                        <span className="text-xs text-grey-500">3 editing</span>
                    </div>
                </div>
                {/* Toolbar */}
                <div className="flex items-center gap-1 px-4 py-2 bg-grey-900/50 border-b border-grey-800">
                    {[
                        { icon: <ArrowUpRight size={16} />, label: 'Select' },
                        { icon: <Square size={16} />, label: 'Rectangle' },
                        { icon: <Circle size={16} />, label: 'Circle' },
                        { icon: <Minus size={16} />, label: 'Line' },
                        { icon: <PenTool size={16} />, label: 'Draw' },
                        { icon: <Type size={16} />, label: 'Text' },
                    ].map(tool => (
                        <button key={tool.label} className="p-2 rounded-lg text-grey-400 hover:bg-grey-800 hover:text-grey-200 transition-colors" title={tool.label}>
                            {tool.icon}
                        </button>
                    ))}
                    <div className="w-px h-6 bg-grey-800 mx-2" />
                    <div className="flex gap-1">
                        {['#1a8fe0', '#10b981', '#f59e0b', '#f43f5e', '#8b5cf6', '#ffffff'].map(color => (
                            <button key={color} className="w-5 h-5 rounded-full border border-grey-700" style={{ backgroundColor: color }} />
                        ))}
                    </div>
                </div>
                {/* Canvas */}
                <div className="h-64 bg-grey-950 relative flex items-center justify-center" style={{ backgroundImage: 'radial-gradient(circle, #343a40 1px, transparent 1px)', backgroundSize: '24px 24px' }}>
                    {/* Sample shapes */}
                    <div className="absolute top-8 left-16 w-32 h-16 border-2 border-brand-500 rounded-lg flex items-center justify-center text-xs text-brand-400">API Gateway</div>
                    <div className="absolute top-8 right-16 w-32 h-16 border-2 border-accent-emerald rounded-lg flex items-center justify-center text-xs text-accent-emerald">Auth Service</div>
                    <div className="absolute bottom-8 left-1/4 w-32 h-16 border-2 border-accent-violet rounded-lg flex items-center justify-center text-xs text-accent-violet">Database</div>
                    <div className="absolute bottom-8 right-1/4 w-32 h-16 border-2 border-accent-amber rounded-lg flex items-center justify-center text-xs text-accent-amber">Cache Layer</div>
                    {/* Connection lines */}
                    <svg className="absolute inset-0 w-full h-full pointer-events-none">
                        <line x1="200" y1="56" x2="500" y2="56" stroke="#495057" strokeWidth="1.5" strokeDasharray="6,4" />
                    </svg>
                </div>
            </div>

            {/* Recent Boards */}
            <h3 className="gs-section-title mb-4">Recent Boards</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                {[
                    { name: 'Architecture Diagram — v3', owner: 'Sarah Chen', updated: '1h ago', collaborators: 3 },
                    { name: 'User Flow — Onboarding', owner: 'Emily Rodriguez', updated: '3h ago', collaborators: 2 },
                    { name: 'Sprint Retrospective Board', owner: 'Marcus Johnson', updated: '1d ago', collaborators: 8 },
                    { name: 'Data Model Brainstorm', owner: 'David Kim', updated: '2d ago', collaborators: 4 },
                    { name: 'Marketing Campaign Flow', owner: 'Amanda Foster', updated: '3d ago', collaborators: 3 },
                    { name: 'Q2 Strategy Map', owner: 'James Wright', updated: '4d ago', collaborators: 6 },
                ].map(board => (
                    <div key={board.name} className="gs-card p-4 cursor-pointer hover:border-brand-500/30 transition-colors">
                        <div className="h-24 bg-grey-800/50 rounded-lg mb-3 flex items-center justify-center" style={{ backgroundImage: 'radial-gradient(circle, #343a40 1px, transparent 1px)', backgroundSize: '16px 16px' }}>
                            <PenTool size={24} className="text-grey-600" />
                        </div>
                        <h4 className="text-sm font-medium text-grey-200">{board.name}</h4>
                        <div className="flex items-center justify-between mt-2 text-xs text-grey-500">
                            <span>{board.owner} · {board.updated}</span>
                            <span className="flex items-center gap-1"><Users size={12} /> {board.collaborators}</span>
                        </div>
                    </div>
                ))}
            </div>
        </div>
    );
}
