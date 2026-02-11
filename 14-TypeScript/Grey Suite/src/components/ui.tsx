import { ReactNode } from 'react';

interface StatCardProps {
    label: string;
    value: string | number;
    change?: string;
    changeType?: 'positive' | 'negative' | 'neutral';
    icon?: ReactNode;
}

export function StatCard({ label, value, change, changeType = 'neutral', icon }: StatCardProps) {
    const changeColor = {
        positive: 'text-accent-emerald',
        negative: 'text-accent-rose',
        neutral: 'text-grey-500',
    }[changeType];

    return (
        <div className="gs-stat-card">
            <div className="flex items-center justify-between">
                <span className="text-xs font-semibold text-grey-500 uppercase tracking-wider">{label}</span>
                {icon && <span className="text-grey-600">{icon}</span>}
            </div>
            <div className="flex items-end gap-2">
                <span className="text-2xl font-bold text-grey-100">{value}</span>
                {change && <span className={`text-xs font-medium ${changeColor} pb-1`}>{change}</span>}
            </div>
        </div>
    );
}

interface PageHeaderProps {
    title: string;
    subtitle?: string;
    actions?: ReactNode;
}

export function PageHeader({ title, subtitle, actions }: PageHeaderProps) {
    return (
        <div className="gs-page-header">
            <div>
                <h1 className="text-2xl font-bold text-grey-100">{title}</h1>
                {subtitle && <p className="text-sm text-grey-500 mt-1">{subtitle}</p>}
            </div>
            {actions && <div className="flex items-center gap-2">{actions}</div>}
        </div>
    );
}

interface DataTableProps {
    columns: string[];
    rows: (string | ReactNode)[][];
}

export function DataTable({ columns, rows }: DataTableProps) {
    return (
        <div className="overflow-x-auto">
            <table className="gs-table w-full">
                <thead>
                    <tr>
                        {columns.map(col => <th key={col}>{col}</th>)}
                    </tr>
                </thead>
                <tbody>
                    {rows.map((row, i) => (
                        <tr key={i}>
                            {row.map((cell, j) => <td key={j}>{cell}</td>)}
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

interface BadgeProps {
    children: ReactNode;
    variant?: 'default' | 'success' | 'warning' | 'danger' | 'info' | 'purple';
}

export function Badge({ children, variant = 'default' }: BadgeProps) {
    const styles = {
        default: 'bg-grey-800 text-grey-300',
        success: 'bg-emerald-500/15 text-emerald-400',
        warning: 'bg-amber-500/15 text-amber-400',
        danger: 'bg-rose-500/15 text-rose-400',
        info: 'bg-brand-500/15 text-brand-400',
        purple: 'bg-violet-500/15 text-violet-400',
    }[variant];

    return <span className={`gs-badge ${styles}`}>{children}</span>;
}

interface ProgressBarProps {
    value: number;
    max?: number;
    color?: string;
    size?: 'sm' | 'md';
}

export function ProgressBar({ value, max = 100, color = 'bg-brand-500', size = 'sm' }: ProgressBarProps) {
    const pct = Math.min(100, (value / max) * 100);
    const h = size === 'sm' ? 'h-1.5' : 'h-2.5';
    return (
        <div className={`w-full bg-grey-800 rounded-full ${h}`}>
            <div className={`${color} ${h} rounded-full transition-all duration-500`} style={{ width: `${pct}%` }} />
        </div>
    );
}

interface TabsProps {
    tabs: string[];
    active: string;
    onChange: (tab: string) => void;
}

export function Tabs({ tabs, active, onChange }: TabsProps) {
    return (
        <div className="flex gap-1 border-b border-grey-800 mb-6">
            {tabs.map(tab => (
                <button
                    key={tab}
                    onClick={() => onChange(tab)}
                    className={`px-4 py-2.5 text-sm font-medium border-b-2 transition-colors ${active === tab
                            ? 'border-brand-400 text-brand-400'
                            : 'border-transparent text-grey-500 hover:text-grey-300'
                        }`}
                >
                    {tab}
                </button>
            ))}
        </div>
    );
}

export function EmptyState({ icon, title, description }: { icon: ReactNode; title: string; description: string }) {
    return (
        <div className="flex flex-col items-center justify-center py-16 text-center">
            <div className="text-grey-600 mb-4">{icon}</div>
            <h3 className="text-lg font-semibold text-grey-300 mb-2">{title}</h3>
            <p className="text-sm text-grey-500 max-w-md">{description}</p>
        </div>
    );
}

interface MiniChartProps {
    data: number[];
    color?: string;
    height?: number;
}

export function MiniSparkline({ data, color = '#1a8fe0', height = 32 }: MiniChartProps) {
    const max = Math.max(...data);
    const min = Math.min(...data);
    const range = max - min || 1;
    const w = 100;
    const points = data.map((d, i) => `${(i / (data.length - 1)) * w},${height - ((d - min) / range) * (height - 4) - 2}`).join(' ');

    return (
        <svg viewBox={`0 0 ${w} ${height}`} className="w-full" style={{ height }}>
            <polyline fill="none" stroke={color} strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" points={points} />
        </svg>
    );
}
