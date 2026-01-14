/**
 * MetricCard Component
 * 
 * Displays a single metric with current value, baseline comparison,
 * and reduction percentage indicator.
 */
function MetricCard({ title, current, baseline, unit, color, icon, children }) {
  const reduction = baseline > 0 ? ((baseline - current) / baseline) * 100 : 0
  const isImproved = reduction > 0

  return (
    <div className="metric-card rounded-xl p-6 bg-grey-800 border border-grey-700">
      {/* Header */}
      <div className="flex items-center justify-between mb-4">
        <div className="flex items-center space-x-3">
          <div 
            className="w-10 h-10 rounded-lg flex items-center justify-center"
            style={{ backgroundColor: `${color}20` }}
          >
            {icon}
          </div>
          <h3 className="text-lg font-semibold text-white">{title}</h3>
        </div>
        <div className={`flex items-center space-x-1 px-2 py-1 rounded-full text-sm font-medium ${
          isImproved 
            ? 'bg-green-900/50 text-green-300' 
            : 'bg-red-900/50 text-red-300'
        }`}>
          {isImproved ? (
            <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 10l7-7m0 0l7 7m-7-7v18" />
            </svg>
          ) : (
            <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 14l-7 7m0 0l-7-7m7 7V3" />
            </svg>
          )}
          <span>{Math.abs(reduction).toFixed(1)}%</span>
        </div>
      </div>

      {/* Values */}
      <div className="grid grid-cols-2 gap-4 mb-4">
        <div>
          <p className="text-xs text-grey-400 uppercase tracking-wide mb-1">Current</p>
          <p className="text-2xl font-bold text-white">
            {typeof current === 'number' ? current.toFixed(2) : current}
            <span className="text-sm font-normal text-grey-400 ml-1">{unit}</span>
          </p>
        </div>
        <div>
          <p className="text-xs text-grey-400 uppercase tracking-wide mb-1">Baseline</p>
          <p className="text-2xl font-bold text-grey-500">
            {typeof baseline === 'number' ? baseline.toFixed(2) : baseline}
            <span className="text-sm font-normal text-grey-600 ml-1">{unit}</span>
          </p>
        </div>
      </div>

      {/* Optional children (e.g., chart) */}
      {children && (
        <div className="mt-4 pt-4 border-t border-grey-700">
          {children}
        </div>
      )}
    </div>
  )
}

export default MetricCard
