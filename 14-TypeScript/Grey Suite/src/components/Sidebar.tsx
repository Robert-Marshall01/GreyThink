import { NavLink, useLocation } from 'react-router-dom';
import { useState } from 'react';
import {
    LayoutDashboard, Building2, Users, Briefcase, DollarSign, ShoppingCart, Truck,
    MessageSquare, Video, BookOpen, PenTool, FileAudio, Bot, Sparkles, Brain,
    BarChart3, Database, Activity, FolderKanban, Shield, CreditCard,
    Globe, Star, Contact, Search, GraduationCap, Server,
    ChevronDown, ChevronRight, PanelLeftClose, PanelLeft, Hexagon,
    Layers, Fingerprint, Cpu, Plug, GitBranch, FlaskConical, Share2, Lock, Key,
    Radio, FileText, CheckSquare, Film, Wand2, Workflow, Blocks, Table, Smartphone,
    Network, Rss, UserPlus, Eye, ShieldAlert
} from 'lucide-react';

interface SidebarProps {
    collapsed: boolean;
    onToggle: () => void;
}

interface NavSection {
    label: string;
    icon: React.ReactNode;
    children: { label: string; path: string; icon: React.ReactNode }[];
}

const navSections: NavSection[] = [
    {
        label: 'ERP',
        icon: <Building2 size={18} />,
        children: [
            { label: 'Finance', path: '/erp/finance', icon: <DollarSign size={16} /> },
            { label: 'Procurement', path: '/erp/procurement', icon: <ShoppingCart size={16} /> },
            { label: 'Supply Chain', path: '/erp/supply-chain', icon: <Truck size={16} /> },
        ],
    },
    {
        label: 'CRM',
        icon: <Users size={18} />,
        children: [
            { label: 'Leads', path: '/crm/leads', icon: <Users size={16} /> },
            { label: 'Accounts', path: '/crm/accounts', icon: <Building2 size={16} /> },
            { label: 'Opportunities', path: '/crm/opportunities', icon: <Briefcase size={16} /> },
        ],
    },
    {
        label: 'HCM',
        icon: <Briefcase size={18} />,
        children: [
            { label: 'Recruiting', path: '/hcm/recruiting', icon: <Users size={16} /> },
            { label: 'Payroll', path: '/hcm/payroll', icon: <DollarSign size={16} /> },
            { label: 'Performance', path: '/hcm/performance', icon: <Activity size={16} /> },
        ],
    },
    {
        label: 'Collaboration',
        icon: <MessageSquare size={18} />,
        children: [
            { label: 'Chat', path: '/collaboration/chat', icon: <MessageSquare size={16} /> },
            { label: 'Video', path: '/collaboration/video', icon: <Video size={16} /> },
            { label: 'Knowledge Base', path: '/collaboration/knowledge-base', icon: <BookOpen size={16} /> },
            { label: 'Whiteboard', path: '/collaboration/whiteboard', icon: <PenTool size={16} /> },
            { label: 'Transcription', path: '/collaboration/transcription', icon: <FileAudio size={16} /> },
        ],
    },
    {
        label: 'AI & Automation',
        icon: <Bot size={18} />,
        children: [
            { label: 'Agent Framework', path: '/ai/agent-framework', icon: <Bot size={16} /> },
            { label: 'Generative Suite', path: '/ai/generative-suite', icon: <Sparkles size={16} /> },
            { label: 'Specialized AI', path: '/ai/specialized', icon: <Brain size={16} /> },
        ],
    },
    {
        label: 'Data & Intelligence',
        icon: <BarChart3 size={18} />,
        children: [
            { label: 'BI Dashboards', path: '/data/bi', icon: <BarChart3 size={16} /> },
            { label: 'Data Warehouse', path: '/data/warehouse', icon: <Database size={16} /> },
            { label: 'Product Analytics', path: '/data/analytics', icon: <Activity size={16} /> },
        ],
    },
    {
        label: 'Operations',
        icon: <FolderKanban size={18} />,
        children: [
            { label: 'Project Mgmt', path: '/ops/projects', icon: <FolderKanban size={16} /> },
            { label: 'Identity & Security', path: '/ops/security', icon: <Shield size={16} /> },
            { label: 'Finance & Payments', path: '/ops/finance', icon: <CreditCard size={16} /> },
        ],
    },
    {
        label: 'Presence',
        icon: <Globe size={18} />,
        children: [
            { label: 'Networking', path: '/presence/networking', icon: <Globe size={16} /> },
            { label: 'Reputation', path: '/presence/reputation', icon: <Star size={16} /> },
            { label: 'Personal CRM', path: '/presence/personal-crm', icon: <Contact size={16} /> },
        ],
    },
    {
        label: 'Career',
        icon: <Search size={18} />,
        children: [
            { label: 'Job Search', path: '/career/job-search', icon: <Search size={16} /> },
            { label: 'Skill Development', path: '/career/skills', icon: <GraduationCap size={16} /> },
        ],
    },
    {
        label: 'Infrastructure',
        icon: <Server size={18} />,
        children: [
            { label: 'All Services', path: '/infra', icon: <Hexagon size={16} /> },
        ],
    },
    {
        label: 'Platform Engineering',
        icon: <Layers size={18} />,
        children: [
            { label: 'Multi-Tenant', path: '/platform/multi-tenant', icon: <Building2 size={16} /> },
            { label: 'Identity & Access', path: '/platform/identity', icon: <Fingerprint size={16} /> },
            { label: 'AI Orchestration', path: '/platform/ai-orchestration', icon: <Cpu size={16} /> },
            { label: 'Data Warehouse', path: '/platform/data-warehouse', icon: <Database size={16} /> },
            { label: 'Security & Compliance', path: '/platform/security', icon: <Shield size={16} /> },
            { label: 'Integration Hub', path: '/platform/integrations', icon: <Plug size={16} /> },
            { label: 'Workflow Engine', path: '/platform/workflows', icon: <GitBranch size={16} /> },
            { label: 'Simulation Engine', path: '/platform/simulations', icon: <FlaskConical size={16} /> },
            { label: 'Knowledge Graph', path: '/platform/knowledge-graph', icon: <Share2 size={16} /> },
        ],
    },
    {
        label: 'Enterprise Modules',
        icon: <Blocks size={18} />,
        children: [
            { label: 'Real-Time Comms', path: '/enterprise/realtime-comms', icon: <Radio size={16} /> },
            { label: 'Document Mgmt', path: '/enterprise/document-management', icon: <FileText size={16} /> },
            { label: 'Task & Planning', path: '/enterprise/task-planning', icon: <CheckSquare size={16} /> },
            { label: 'Creative Tools', path: '/enterprise/creative-tools', icon: <Film size={16} /> },
            { label: 'Deep AI', path: '/enterprise/deep-ai', icon: <Wand2 size={16} /> },
            { label: 'Workflow Automation', path: '/enterprise/workflow-automation', icon: <Workflow size={16} /> },
            { label: 'Low-Code Builder', path: '/enterprise/low-code', icon: <Blocks size={16} /> },
            { label: 'BI Platform', path: '/enterprise/bi-platform', icon: <Table size={16} /> },
            { label: 'Device & Security', path: '/enterprise/device-security', icon: <Smartphone size={16} /> },
        ],
    },
    {
        label: 'Professional Networking',
        icon: <Network size={18} />,
        children: [
            { label: 'Profiles', path: '/network/profiles', icon: <Users size={16} /> },
            { label: 'Graph', path: '/network/graph', icon: <Share2 size={16} /> },
            { label: 'Activity Feed', path: '/network/feed', icon: <Rss size={16} /> },
            { label: 'Jobs & Recruiting', path: '/network/jobs', icon: <Briefcase size={16} /> },
            { label: 'Search & Recs', path: '/network/search', icon: <Search size={16} /> },
            { label: 'Trust & Safety', path: '/network/trust', icon: <ShieldAlert size={16} /> },
        ],
    },
];

export function Sidebar({ collapsed, onToggle }: SidebarProps) {
    const location = useLocation();
    const [expandedSections, setExpandedSections] = useState<Set<string>>(() => {
        const active = navSections.find(s => s.children.some(c => location.pathname.startsWith(c.path)));
        return new Set(active ? [active.label] : ['ERP']);
    });

    const toggleSection = (label: string) => {
        setExpandedSections(prev => {
            const next = new Set(prev);
            if (next.has(label)) next.delete(label);
            else next.add(label);
            return next;
        });
    };

    return (
        <aside className={`flex flex-col bg-grey-950 border-r border-grey-800 transition-all duration-300 ${collapsed ? 'w-16' : 'w-64'} h-screen`}>
            {/* Logo */}
            <div className="flex items-center gap-3 px-4 h-14 border-b border-grey-800 flex-shrink-0">
                <div className="w-8 h-8 rounded-lg bg-brand-500 flex items-center justify-center flex-shrink-0">
                    <Hexagon size={18} className="text-white" />
                </div>
                {!collapsed && (
                    <div className="animate-slide-in">
                        <span className="text-base font-bold text-grey-100 tracking-tight">Grey Suite</span>
                    </div>
                )}
            </div>

            {/* Dashboard Link */}
            <div className="px-3 pt-3 pb-1">
                <NavLink
                    to="/"
                    end
                    className={({ isActive }) =>
                        `gs-sidebar-item ${isActive ? 'active' : ''} ${collapsed ? 'justify-center px-0' : ''}`
                    }
                >
                    <LayoutDashboard size={18} />
                    {!collapsed && <span>Dashboard</span>}
                </NavLink>
            </div>

            {/* Navigation Sections */}
            <nav className="flex-1 overflow-y-auto px-3 py-2 space-y-0.5">
                {navSections.map(section => {
                    const isExpanded = expandedSections.has(section.label);
                    const isActive = section.children.some(c => location.pathname.startsWith(c.path));

                    return (
                        <div key={section.label}>
                            <button
                                onClick={() => collapsed ? undefined : toggleSection(section.label)}
                                className={`w-full gs-sidebar-item ${isActive && collapsed ? 'active' : ''} ${collapsed ? 'justify-center px-0' : ''}`}
                                title={collapsed ? section.label : undefined}
                            >
                                <span className={isActive ? 'text-brand-400' : ''}>{section.icon}</span>
                                {!collapsed && (
                                    <>
                                        <span className="flex-1 text-left">{section.label}</span>
                                        {isExpanded ? <ChevronDown size={14} /> : <ChevronRight size={14} />}
                                    </>
                                )}
                            </button>
                            {!collapsed && isExpanded && (
                                <div className="ml-4 pl-3 border-l border-grey-800 space-y-0.5 pb-1 animate-fade-in">
                                    {section.children.map(child => (
                                        <NavLink
                                            key={child.path}
                                            to={child.path}
                                            className={({ isActive }) =>
                                                `gs-sidebar-item text-xs ${isActive ? 'active' : ''}`
                                            }
                                        >
                                            {child.icon}
                                            <span>{child.label}</span>
                                        </NavLink>
                                    ))}
                                </div>
                            )}
                        </div>
                    );
                })}
            </nav>

            {/* Collapse Toggle */}
            <div className="p-3 border-t border-grey-800 flex-shrink-0">
                <button onClick={onToggle} className="gs-sidebar-item w-full justify-center">
                    {collapsed ? <PanelLeft size={18} /> : <PanelLeftClose size={18} />}
                </button>
            </div>
        </aside>
    );
}
