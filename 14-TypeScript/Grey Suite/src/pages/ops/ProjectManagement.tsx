import { PageHeader, StatCard, Badge } from '../../components/ui';
import { FolderKanban, CheckSquare, Clock, AlertCircle, Plus } from 'lucide-react';
import { useState } from 'react';

const tasks = [
    { id: 'ENG-4521', title: 'Auth token refresh implementation', assignee: 'Marcus J.', priority: 'High', status: 'In Progress', sprint: 'Sprint 24', points: 5 },
    { id: 'ENG-4520', title: 'Dashboard redesign — new widgets', assignee: 'David K.', priority: 'High', status: 'In Progress', sprint: 'Sprint 24', points: 8 },
    { id: 'ENG-4519', title: 'Performance: API latency optimization', assignee: 'Sarah C.', priority: 'Critical', status: 'In Review', sprint: 'Sprint 24', points: 5 },
    { id: 'ENG-4518', title: 'Fix payment processing edge case', assignee: 'Emily R.', priority: 'Medium', status: 'To Do', sprint: 'Sprint 24', points: 3 },
    { id: 'ENG-4517', title: 'Add export to CSV for reports', assignee: 'Tom B.', priority: 'Low', status: 'To Do', sprint: 'Sprint 24', points: 2 },
    { id: 'ENG-4516', title: 'Upgrade React to v19', assignee: 'Sarah C.', priority: 'Medium', status: 'Done', sprint: 'Sprint 24', points: 3 },
    { id: 'ENG-4515', title: 'SSO integration — Okta', assignee: 'Marcus J.', priority: 'High', status: 'Done', sprint: 'Sprint 24', points: 8 },
    { id: 'ENG-4514', title: 'Mobile responsive navigation', assignee: 'Emily R.', priority: 'Medium', status: 'Done', sprint: 'Sprint 24', points: 5 },
];

const columns = ['To Do', 'In Progress', 'In Review', 'Done'];

export function ProjectManagement() {
    const [view, setView] = useState<'board' | 'list'>('board');

    return (
        <div>
            <PageHeader
                title="Project Management"
                subtitle="Boards, sprints, tasks & workflows"
                actions={
                    <div className="flex gap-2">
                        <div className="flex bg-grey-900 border border-grey-800 rounded-lg overflow-hidden">
                            <button onClick={() => setView('board')} className={`px-3 py-1.5 text-sm ${view === 'board' ? 'bg-grey-700 text-grey-100' : 'text-grey-500'}`}>Board</button>
                            <button onClick={() => setView('list')} className={`px-3 py-1.5 text-sm ${view === 'list' ? 'bg-grey-700 text-grey-100' : 'text-grey-500'}`}>List</button>
                        </div>
                        <button className="gs-btn-primary text-sm flex items-center gap-2"><Plus size={14} /> New Task</button>
                    </div>
                }
            />

            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
                <StatCard label="Sprint 24" value="12 days left" icon={<Clock size={18} />} />
                <StatCard label="Total Tasks" value={tasks.length} change="39 story points" changeType="neutral" icon={<FolderKanban size={18} />} />
                <StatCard label="Completed" value={tasks.filter(t => t.status === 'Done').length} change={`${Math.round(tasks.filter(t => t.status === 'Done').length / tasks.length * 100)}%`} changeType="positive" icon={<CheckSquare size={18} />} />
                <StatCard label="Blockers" value={1} change="Action needed" changeType="negative" icon={<AlertCircle size={18} />} />
            </div>

            {view === 'board' ? (
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
                    {columns.map(col => {
                        const colTasks = tasks.filter(t => t.status === col);
                        return (
                            <div key={col}>
                                <div className="flex items-center justify-between mb-3 px-1">
                                    <div className="flex items-center gap-2">
                                        <span className={`w-2 h-2 rounded-full ${col === 'Done' ? 'bg-accent-emerald' :
                                                col === 'In Progress' ? 'bg-brand-500' :
                                                    col === 'In Review' ? 'bg-accent-amber' : 'bg-grey-600'
                                            }`} />
                                        <span className="text-sm font-semibold text-grey-300">{col}</span>
                                    </div>
                                    <span className="text-xs text-grey-500">{colTasks.length}</span>
                                </div>
                                <div className="space-y-2">
                                    {colTasks.map(task => (
                                        <div key={task.id} className="gs-card p-3 cursor-pointer hover:border-grey-600 transition-colors">
                                            <p className="text-xs text-grey-500 mb-1">{task.id}</p>
                                            <p className="text-sm font-medium text-grey-200 mb-2">{task.title}</p>
                                            <div className="flex items-center justify-between text-xs">
                                                <Badge variant={
                                                    task.priority === 'Critical' ? 'danger' :
                                                        task.priority === 'High' ? 'warning' :
                                                            task.priority === 'Medium' ? 'info' : 'default'
                                                }>{task.priority}</Badge>
                                                <div className="flex items-center gap-2">
                                                    <span className="text-grey-500">{task.points}pt</span>
                                                    <div className="w-5 h-5 rounded-full bg-grey-700 flex items-center justify-center text-[9px] font-bold text-grey-300">
                                                        {task.assignee.split(' ').map(n => n[0]).join('')}
                                                    </div>
                                                </div>
                                            </div>
                                        </div>
                                    ))}
                                </div>
                            </div>
                        );
                    })}
                </div>
            ) : (
                <div className="gs-card">
                    <table className="gs-table w-full">
                        <thead>
                            <tr><th>ID</th><th>Task</th><th>Assignee</th><th>Priority</th><th>Points</th><th>Status</th></tr>
                        </thead>
                        <tbody>
                            {tasks.map(t => (
                                <tr key={t.id}>
                                    <td className="font-mono text-brand-400">{t.id}</td>
                                    <td className="font-medium text-grey-200">{t.title}</td>
                                    <td>{t.assignee}</td>
                                    <td><Badge variant={t.priority === 'Critical' ? 'danger' : t.priority === 'High' ? 'warning' : t.priority === 'Medium' ? 'info' : 'default'}>{t.priority}</Badge></td>
                                    <td>{t.points}</td>
                                    <td><Badge variant={t.status === 'Done' ? 'success' : t.status === 'In Review' ? 'warning' : t.status === 'In Progress' ? 'info' : 'default'}>{t.status}</Badge></td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </div>
    );
}
