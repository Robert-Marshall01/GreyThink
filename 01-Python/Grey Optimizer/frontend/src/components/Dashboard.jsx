/**
 * Dashboard Component
 * 
 * Main dashboard layout that displays CPU, RAM, and Disk metrics
 * with gauges, charts, and configuration panel.
 */
import Gauge from './Gauge'
import MetricCard from './MetricCard'
import Chart from './Chart'
import ConfigPanel from './ConfigPanel'
import RecentActions from './RecentActions'

function Dashboard({ metrics, metricsHistory, baseline, config, onToggle }) {
  // Calculate reductions
  const cpuReduction = baseline.cpu > 0 
    ? ((baseline.cpu - metrics.cpu) / baseline.cpu) * 100 
    : 0
  const ramReduction = baseline.ram > 0 
    ? ((baseline.ram - metrics.ram) / baseline.ram) * 100 
    : 0
  const diskReduction = baseline.disk > 0 
    ? ((baseline.disk - metrics.disk) / baseline.disk) * 100 
    : 0
  
  const avgReduction = (cpuReduction + ramReduction + diskReduction) / 3

  // Format for history chart
  const chartData = metricsHistory.map(m => ({
    timestamp: m.timestamp,
    cpu: m.cpu,
    ram: m.ram,
    disk: m.disk
  }))

  // Mock recent actions (would come from API in real implementation)
  const recentActions = metrics.actions || []

  return (
    <div className="space-y-6">
      {/* Summary gauges */}
      <div className="bg-grey-800 rounded-xl border border-grey-700 p-6">
        <h2 className="text-lg font-semibold text-white mb-6 flex items-center">
          <svg className="w-5 h-5 mr-2 text-grey-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z" />
          </svg>
          Resource Usage Overview
        </h2>

        <div className="grid grid-cols-4 gap-6">
          <Gauge 
            value={metrics.cpu} 
            label="CPU Usage"
            color="rgb(59, 130, 246)"
            size={140}
          />
          <Gauge 
            value={metrics.ram} 
            label="RAM Usage"
            color="rgb(34, 197, 94)"
            size={140}
          />
          <Gauge 
            value={metrics.disk} 
            label="Disk I/O"
            color="rgb(168, 85, 247)"
            size={140}
          />
          <Gauge 
            value={avgReduction}
            label="Avg Reduction"
            color={avgReduction >= 50 ? 'rgb(34, 197, 94)' : 'rgb(251, 191, 36)'}
            size={140}
          />
        </div>
      </div>

      {/* Metric cards with charts */}
      <div className="grid grid-cols-3 gap-6">
        <MetricCard
          title="CPU"
          current={metrics.cpu}
          baseline={baseline.cpu}
          unit="%"
          color="rgb(59, 130, 246)"
          icon={
            <svg className="w-5 h-5 text-blue-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 3v2m6-2v2M9 19v2m6-2v2M5 9H3m2 6H3m18-6h-2m2 6h-2M7 19h10a2 2 0 002-2V7a2 2 0 00-2-2H7a2 2 0 00-2 2v10a2 2 0 002 2zM9 9h6v6H9V9z" />
            </svg>
          }
        >
          <Chart 
            data={chartData}
            dataKeys={['cpu']}
            colors={['rgb(59, 130, 246)']}
            height={100}
            showLegend={false}
            tooltipFormatter={(v) => `${v.toFixed(1)}%`}
          />
        </MetricCard>

        <MetricCard
          title="RAM"
          current={metrics.ram}
          baseline={baseline.ram}
          unit="%"
          color="rgb(34, 197, 94)"
          icon={
            <svg className="w-5 h-5 text-green-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10" />
            </svg>
          }
        >
          <Chart 
            data={chartData}
            dataKeys={['ram']}
            colors={['rgb(34, 197, 94)']}
            height={100}
            showLegend={false}
            tooltipFormatter={(v) => `${v.toFixed(1)}%`}
          />
        </MetricCard>

        <MetricCard
          title="Disk I/O"
          current={metrics.disk}
          baseline={baseline.disk}
          unit="%"
          color="rgb(168, 85, 247)"
          icon={
            <svg className="w-5 h-5 text-purple-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 7v10c0 2.21 3.582 4 8 4s8-1.79 8-4V7M4 7c0 2.21 3.582 4 8 4s8-1.79 8-4M4 7c0-2.21 3.582-4 8-4s8 1.79 8 4m0 5c0 2.21-3.582 4-8 4s-8-1.79-8-4" />
            </svg>
          }
        >
          <Chart 
            data={chartData}
            dataKeys={['disk']}
            colors={['rgb(168, 85, 247)']}
            height={100}
            showLegend={false}
            tooltipFormatter={(v) => `${v.toFixed(1)}%`}
          />
        </MetricCard>
      </div>

      {/* Full chart */}
      <div className="bg-grey-800 rounded-xl border border-grey-700 p-6">
        <h2 className="text-lg font-semibold text-white mb-4 flex items-center">
          <svg className="w-5 h-5 mr-2 text-grey-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M7 12l3-3 3 3 4-4M8 21l4-4 4 4M3 4h18M4 4h16v12a1 1 0 01-1 1H5a1 1 0 01-1-1V4z" />
          </svg>
          Resource History
        </h2>
        <Chart 
          data={chartData}
          dataKeys={['cpu', 'ram', 'disk']}
          colors={['rgb(59, 130, 246)', 'rgb(34, 197, 94)', 'rgb(168, 85, 247)']}
          height={250}
          showLegend={true}
          tooltipFormatter={(v) => `${v.toFixed(1)}%`}
        />
      </div>

      {/* Bottom row: config and actions */}
      <div className="grid grid-cols-2 gap-6">
        <ConfigPanel config={config} onToggle={onToggle} />
        <RecentActions actions={recentActions} />
      </div>

      {/* Target indicator */}
      <div className={`p-4 rounded-xl border ${
        avgReduction >= 90 
          ? 'bg-green-900/30 border-green-700' 
          : avgReduction >= 50 
            ? 'bg-yellow-900/30 border-yellow-700'
            : 'bg-grey-800 border-grey-700'
      }`}>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-3">
            {avgReduction >= 90 ? (
              <span className="text-3xl">🎯</span>
            ) : (
              <span className="text-3xl">📊</span>
            )}
            <div>
              <p className="text-lg font-semibold text-white">
                {avgReduction >= 90 ? '90% Reduction Target Met!' : 'Progress Toward 90% Target'}
              </p>
              <p className="text-sm text-grey-400">
                Current average reduction: {avgReduction.toFixed(1)}%
              </p>
            </div>
          </div>
          <div className="w-48 h-3 bg-grey-700 rounded-full overflow-hidden">
            <div 
              className={`h-full rounded-full transition-all duration-500 ${
                avgReduction >= 90 ? 'bg-green-500' : avgReduction >= 50 ? 'bg-yellow-500' : 'bg-blue-500'
              }`}
              style={{ width: `${Math.min(avgReduction / 90 * 100, 100)}%` }}
            />
          </div>
        </div>
      </div>
    </div>
  )
}

export default Dashboard
