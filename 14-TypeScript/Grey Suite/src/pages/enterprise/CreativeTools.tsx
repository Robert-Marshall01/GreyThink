import { useState } from 'react';
import {
    Film, Presentation, GitBranch, Palette, Image, Type, Layers,
    Play, Scissors, Music, Wand2, Download, Upload, Monitor,
    MousePointer, Sparkles, PenTool, ArrowRight, Square, Circle,
    Move, ZoomIn, Undo, Redo
} from 'lucide-react';
import { PageHeader, StatCard, DataTable, Badge, Tabs, ProgressBar } from '../../components/ui';

export function CreativeTools() {
    const [activeTab, setActiveTab] = useState('Video Editor');

    const tabs = ['Video Editor', 'Report Builder', 'Diagramming', 'AI Design'];

    return (
        <div className="p-6 space-y-6">
            <PageHeader
                title="Creative & Content Production Tools"
                subtitle="Video editor, interactive report builder, diagramming tool, and AI graphic design assistant"
                actions={
                    <div className="flex gap-2">
                        <button className="gs-btn-secondary">
                            <Upload size={14} /> Import Media
                        </button>
                        <button className="gs-btn-primary">
                            <Sparkles size={14} /> AI Create
                        </button>
                    </div>
                }
            />

            <div className="grid grid-cols-4 gap-4">
                <StatCard label="Projects Created" value="2,841" change="+184 this month" changeType="positive" icon={<Film size={18} />} />
                <StatCard label="Assets Library" value="48.2K" change="+2,140" changeType="positive" icon={<Image size={18} />} />
                <StatCard label="AI Generations" value="12,420" change="+42%" changeType="positive" icon={<Wand2 size={18} />} />
                <StatCard label="Exports (30d)" value="4,891" change="+18%" changeType="positive" icon={<Download size={18} />} />
            </div>

            <Tabs tabs={tabs} active={activeTab} onChange={setActiveTab} />

            {activeTab === 'Video Editor' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Video Editor</h3>

                        {/* Simulated Video Editor UI */}
                        <div className="bg-grey-900 rounded-lg border border-grey-700/50 overflow-hidden">
                            {/* Toolbar */}
                            <div className="flex items-center gap-1 px-3 py-2 bg-grey-800/80 border-b border-grey-700/50">
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Undo size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Redo size={14} /></button>
                                <div className="w-px h-4 bg-grey-700 mx-1" />
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Scissors size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Type size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Music size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Sparkles size={14} /></button>
                                <div className="flex-1" />
                                <span className="text-xs text-grey-500 font-mono">00:02:34 / 00:05:12</span>
                                <div className="w-px h-4 bg-grey-700 mx-1" />
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Download size={14} /></button>
                            </div>

                            {/* Preview Area */}
                            <div className="flex">
                                <div className="flex-1 p-4">
                                    <div className="aspect-video bg-grey-950 rounded-lg flex items-center justify-center border border-grey-800">
                                        <div className="text-center">
                                            <Play size={48} className="text-grey-600 mx-auto mb-2" />
                                            <p className="text-sm text-grey-500">Preview Canvas</p>
                                            <p className="text-xs text-grey-600">1920×1080 • 30fps • H.264</p>
                                        </div>
                                    </div>
                                </div>

                                {/* Properties Panel */}
                                <div className="w-64 border-l border-grey-700/50 p-3 space-y-3">
                                    <p className="text-xs font-semibold text-grey-400 uppercase">Properties</p>
                                    {[
                                        { label: 'Resolution', value: '1920×1080' },
                                        { label: 'Frame Rate', value: '30 fps' },
                                        { label: 'Codec', value: 'H.264 / AAC' },
                                        { label: 'Bitrate', value: '8 Mbps' },
                                        { label: 'Duration', value: '05:12' },
                                    ].map(p => (
                                        <div key={p.label} className="flex justify-between text-xs">
                                            <span className="text-grey-500">{p.label}</span>
                                            <span className="text-grey-300">{p.value}</span>
                                        </div>
                                    ))}
                                </div>
                            </div>

                            {/* Timeline */}
                            <div className="border-t border-grey-700/50 p-3 bg-grey-800/30">
                                <div className="flex items-center gap-2 mb-2">
                                    <button className="p-1 rounded hover:bg-grey-700 text-grey-400"><Play size={12} /></button>
                                    <div className="flex-1 h-1 bg-grey-700 rounded overflow-hidden">
                                        <div className="w-[49%] h-full bg-brand-500 rounded" />
                                    </div>
                                </div>
                                <div className="space-y-1">
                                    {[
                                        { track: 'Video', color: 'bg-brand-500', segments: [{ w: '30%', label: 'Intro' }, { w: '50%', label: 'Main Content' }, { w: '18%', label: 'Outro' }] },
                                        { track: 'Audio', color: 'bg-violet-500', segments: [{ w: '100%', label: 'Background Music' }] },
                                        { track: 'Text', color: 'bg-amber-500', segments: [{ w: '15%', label: 'Title' }, { w: '0%', label: '' }, { w: '10%', label: 'CTA' }] },
                                        { track: 'Effects', color: 'bg-emerald-500', segments: [{ w: '20%', label: 'Fade In' }, { w: '0%', label: '' }, { w: '15%', label: 'Transition' }] },
                                    ].map(t => (
                                        <div key={t.track} className="flex items-center gap-2">
                                            <span className="text-[10px] text-grey-500 w-12">{t.track}</span>
                                            <div className="flex-1 flex gap-0.5 h-6">
                                                {t.segments.filter(s => s.w !== '0%').map((s, i) => (
                                                    <div key={i} className={`${t.color}/30 rounded px-1 flex items-center border border-${t.color}/50`} style={{ width: s.w }}>
                                                        <span className="text-[9px] text-grey-300 truncate">{s.label}</span>
                                                    </div>
                                                ))}
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="grid grid-cols-3 gap-4">
                        {[
                            { feature: 'AI Auto-Cut', desc: 'Remove silences and filler words automatically', icon: <Scissors size={20} /> },
                            { feature: 'AI Captions', desc: 'Real-time transcription with 98.5% accuracy in 40+ languages', icon: <Type size={20} /> },
                            { feature: 'Brand Kit', desc: 'Enforce brand colors, fonts, logos, and intros across all videos', icon: <Palette size={20} /> },
                        ].map(f => (
                            <div key={f.feature} className="gs-card p-4">
                                <div className="flex items-center gap-3 mb-3">
                                    <div className="p-2 bg-brand-500/10 rounded-lg text-brand-400">{f.icon}</div>
                                    <div>
                                        <p className="text-sm font-semibold text-grey-200">{f.feature}</p>
                                        <p className="text-xs text-grey-500">{f.desc}</p>
                                    </div>
                                </div>
                            </div>
                        ))}
                    </div>
                </div>
            )}

            {activeTab === 'Report Builder' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Interactive Report Builder</h3>
                        <p className="text-xs text-grey-500 mb-4">Create immersive, scroll-based presentations with embedded data, media, and interactivity</p>

                        <DataTable
                            columns={['Report', 'Type', 'Sections', 'Data Sources', 'Views', 'Last Edited', 'Status']}
                            rows={[
                                ['Q1 2026 Company Performance', <Badge variant="info">Executive Brief</Badge>, '8', '4 (BI, CRM, HCM, Finance)', '2,841', '2h ago', <Badge variant="success">Published</Badge>],
                                ['Product Launch Retrospective', <Badge variant="purple">Presentation</Badge>, '12', '2 (Analytics, PM)', '892', '1d ago', <Badge variant="success">Published</Badge>],
                                ['Engineering Quarterly Review', <Badge variant="info">Report</Badge>, '14', '5 (DORA, SRE, CI/CD, Git, Tickets)', '421', '3d ago', <Badge variant="success">Published</Badge>],
                                ['Customer Success Dashboard', <Badge variant="warning">Interactive</Badge>, '6', '3 (CRM, Support, NPS)', '1,204', '6h ago', <Badge variant="success">Published</Badge>],
                                ['Board Deck — February 2026', <Badge variant="danger">Confidential</Badge>, '18', '8 (All systems)', '12', 'Just now', <Badge variant="warning">Draft</Badge>],
                            ]}
                        />
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Content Blocks</h3>
                            <div className="grid grid-cols-2 gap-2">
                                {[
                                    'Hero Section', 'Text + Image', 'Data Chart', 'KPI Cards',
                                    'Comparison Table', 'Timeline', 'Quote Block', 'Video Embed',
                                    'Image Gallery', 'Code Snippet', 'Map View', 'Form Embed',
                                    'AI Summary', 'Call to Action', 'Divider', 'Navigation',
                                ].map(b => (
                                    <div key={b} className="text-xs px-3 py-2 bg-grey-800/50 rounded text-grey-400 border border-grey-700/50 hover:border-brand-500/50 cursor-pointer transition-colors">{b}</div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Rendering Engine</h3>
                            <div className="bg-grey-900 rounded-lg p-4 font-mono text-xs text-grey-400">
                                <pre>{`// Report Rendering Pipeline
1. Content Layer (React Components)
   ├── Markdown → MDX compilation
   ├── Data bindings → Live query
   └── Interactive widgets

2. Layout Engine
   ├── CSS Grid + Flexbox
   ├── Scroll-snap sections
   ├── Responsive breakpoints
   └── Print-optimized @media

3. Data Layer
   ├── GraphQL data sources
   ├── Real-time subscriptions
   ├── Cached snapshots
   └── Parameterized filters

4. Export Pipeline
   ├── PDF (Puppeteer headless)
   ├── PowerPoint (pptxgenjs)
   ├── Static HTML (SSG)
   └── Image Raster (Sharp)`}</pre>
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'Diagramming' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">Diagramming Tool</h3>

                        {/* Simulated Canvas */}
                        <div className="bg-grey-900 rounded-lg border border-grey-700/50 overflow-hidden">
                            {/* Toolbar */}
                            <div className="flex items-center gap-1 px-3 py-2 bg-grey-800/80 border-b border-grey-700/50">
                                <button className="p-1.5 rounded bg-brand-500/20 text-brand-400"><MousePointer size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Square size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Circle size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><ArrowRight size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Type size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><PenTool size={14} /></button>
                                <div className="w-px h-4 bg-grey-700 mx-1" />
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Layers size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><Move size={14} /></button>
                                <button className="p-1.5 rounded hover:bg-grey-700 text-grey-400"><ZoomIn size={14} /></button>
                                <div className="flex-1" />
                                <span className="text-xs text-grey-500">100%</span>
                            </div>

                            {/* Canvas Area */}
                            <div className="p-8 min-h-[300px]" style={{ backgroundImage: 'radial-gradient(circle, #374151 1px, transparent 1px)', backgroundSize: '24px 24px' }}>
                                {/* Simulated diagram nodes */}
                                <div className="flex items-center gap-8 justify-center">
                                    {[
                                        { label: 'Client App', type: 'rect', color: 'border-brand-500 bg-brand-500/10' },
                                        { label: 'API Gateway', type: 'rect', color: 'border-violet-500 bg-violet-500/10' },
                                        { label: 'Auth Service', type: 'rect', color: 'border-amber-500 bg-amber-500/10' },
                                        { label: 'Database', type: 'rect', color: 'border-emerald-500 bg-emerald-500/10' },
                                    ].map((node, i) => (
                                        <div key={node.label} className="flex items-center gap-4">
                                            <div className={`px-6 py-3 rounded-lg border-2 ${node.color} text-xs font-semibold text-grey-200`}>
                                                {node.label}
                                            </div>
                                            {i < 3 && <ArrowRight size={16} className="text-grey-500" />}
                                        </div>
                                    ))}
                                </div>
                            </div>
                        </div>
                    </div>

                    <div className="grid grid-cols-3 gap-4">
                        <div className="gs-card p-4">
                            <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Diagram Types</h4>
                            <div className="space-y-2">
                                {[
                                    'Flowchart', 'Network Topology', 'UML Class Diagram',
                                    'Sequence Diagram', 'ER Diagram', 'Architecture Diagram',
                                    'BPMN Process', 'Org Chart', 'Mind Map',
                                    'Wireframe / Mockup', 'State Machine', 'Data Flow',
                                ].map(d => (
                                    <div key={d} className="text-xs px-2 py-1.5 bg-grey-800/50 rounded text-grey-400">{d}</div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-4">
                            <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Shape Libraries</h4>
                            <div className="space-y-2">
                                {[
                                    { lib: 'AWS Architecture', shapes: 142 },
                                    { lib: 'Azure Services', shapes: 128 },
                                    { lib: 'GCP Icons', shapes: 96 },
                                    { lib: 'Kubernetes', shapes: 48 },
                                    { lib: 'UML Notation', shapes: 64 },
                                    { lib: 'Network (Cisco)', shapes: 84 },
                                    { lib: 'BPMN 2.0', shapes: 42 },
                                    { lib: 'Basic Shapes', shapes: 32 },
                                ].map(l => (
                                    <div key={l.lib} className="flex justify-between text-xs px-2 py-1.5 bg-grey-800/50 rounded">
                                        <span className="text-grey-400">{l.lib}</span>
                                        <span className="text-grey-500">{l.shapes} shapes</span>
                                    </div>
                                ))}
                            </div>
                        </div>

                        <div className="gs-card p-4">
                            <h4 className="text-xs font-semibold text-grey-400 uppercase mb-3">Export Formats</h4>
                            <div className="space-y-2">
                                {[
                                    { format: 'SVG', desc: 'Vector, scalable' },
                                    { format: 'PNG', desc: 'Raster, high-res' },
                                    { format: 'PDF', desc: 'Print-ready' },
                                    { format: 'Visio (VSDX)', desc: 'Microsoft Visio compatible' },
                                    { format: 'Draw.io XML', desc: 'Open format' },
                                    { format: 'Mermaid', desc: 'Code-based diagrams' },
                                    { format: 'PlantUML', desc: 'Text-based UML' },
                                    { format: 'Embed Code', desc: 'iframe / React component' },
                                ].map(f => (
                                    <div key={f.format} className="flex justify-between text-xs px-2 py-1.5 bg-grey-800/50 rounded">
                                        <span className="text-grey-300 font-medium">{f.format}</span>
                                        <span className="text-grey-500">{f.desc}</span>
                                    </div>
                                ))}
                            </div>
                        </div>
                    </div>
                </div>
            )}

            {activeTab === 'AI Design' && (
                <div className="space-y-6">
                    <div className="gs-card p-6">
                        <h3 className="text-sm font-semibold text-grey-300 mb-4">AI Graphic Design Assistant</h3>

                        <div className="grid grid-cols-3 gap-4 mb-6">
                            {[
                                { capability: 'Text to Image', model: 'DALL·E 3 / Stable Diffusion XL', usage: '4,284/mo', icon: <Image size={20} /> },
                                { capability: 'Brand Adaptation', model: 'Custom LoRA fine-tuned', usage: '1,842/mo', icon: <Palette size={20} /> },
                                { capability: 'Layout Generation', model: 'LayoutGPT + CSS Grid', usage: '2,104/mo', icon: <Layers size={20} /> },
                            ].map(c => (
                                <div key={c.capability} className="bg-grey-800/50 rounded-lg p-4 border border-grey-700/50">
                                    <div className="flex items-center gap-3 mb-3">
                                        <div className="p-2 bg-brand-500/10 rounded-lg text-brand-400">{c.icon}</div>
                                        <div>
                                            <p className="text-sm font-semibold text-grey-200">{c.capability}</p>
                                            <p className="text-xs text-grey-500">{c.model}</p>
                                        </div>
                                    </div>
                                    <Badge variant="info">{c.usage}</Badge>
                                </div>
                            ))}
                        </div>
                    </div>

                    <div className="grid grid-cols-2 gap-6">
                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Design Templates (AI-Powered)</h3>
                            <DataTable
                                columns={['Template', 'Category', 'AI Features', 'Uses (30d)']}
                                rows={[
                                    ['Social Media Post', 'Marketing', 'Copy + image + layout', '2,841'],
                                    ['Email Header', 'Communications', 'Brand-aware generation', '1,284'],
                                    ['Presentation Slide', 'Business', 'Content-to-slide AI', '892'],
                                    ['Infographic', 'Data Viz', 'Data-to-visual pipeline', '421'],
                                    ['Banner Ad', 'Marketing', 'A/B variant generation', '642'],
                                    ['Product Mockup', 'Design', 'AI scene composition', '284'],
                                    ['Icon Set', 'UI/UX', 'Style-consistent generation', '184'],
                                    ['Chart + Graph', 'Data Viz', 'Auto-style from data', '1,042'],
                                ]}
                            />
                        </div>

                        <div className="gs-card p-6">
                            <h3 className="text-sm font-semibold text-grey-300 mb-4">Brand Kit Enforcement</h3>
                            <div className="space-y-4">
                                <div>
                                    <p className="text-xs text-grey-400 mb-2">Primary Colors</p>
                                    <div className="flex gap-2">
                                        {['#1a8fe0', '#0f172a', '#f8fafc', '#10b981', '#f59e0b'].map(c => (
                                            <div key={c} className="flex items-center gap-2">
                                                <div className="w-6 h-6 rounded" style={{ backgroundColor: c }} />
                                                <span className="text-[10px] font-mono text-grey-500">{c}</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                                <div>
                                    <p className="text-xs text-grey-400 mb-2">Typography</p>
                                    <div className="space-y-1 text-xs text-grey-400">
                                        <div className="flex justify-between"><span>Headings</span><span className="text-grey-300 font-semibold">Inter Bold</span></div>
                                        <div className="flex justify-between"><span>Body</span><span className="text-grey-300">Inter Regular</span></div>
                                        <div className="flex justify-between"><span>Code</span><span className="text-grey-300 font-mono">JetBrains Mono</span></div>
                                    </div>
                                </div>
                                <div>
                                    <p className="text-xs text-grey-400 mb-2">AI Guardrails</p>
                                    <div className="space-y-2">
                                        {[
                                            { rule: 'Color compliance', desc: 'All generated assets use brand palette' },
                                            { rule: 'Logo placement', desc: 'Auto-detect and enforce safe zones' },
                                            { rule: 'Accessibility', desc: 'WCAG 2.1 AA contrast ratio enforcement' },
                                            { rule: 'Content policy', desc: 'Block inappropriate imagery generation' },
                                        ].map(r => (
                                            <div key={r.rule} className="flex justify-between text-xs">
                                                <span className="text-grey-300">{r.rule}</span>
                                                <span className="text-grey-500">{r.desc}</span>
                                            </div>
                                        ))}
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            )}
        </div>
    );
}
