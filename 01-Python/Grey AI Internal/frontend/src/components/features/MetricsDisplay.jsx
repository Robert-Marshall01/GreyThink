/**
 * Metrics Display Component
 * =========================
 * Displays parsed data metrics including row counts, column info,
 * statistics, and a data preview table.
 */

import { useState } from 'react'
import PropTypes from 'prop-types'
import Card from '../ui/Card'
import Button from '../ui/Button'
import LoadingSpinner from '../ui/LoadingSpinner'

function MetricsDisplay({ 
  metrics, 
  loading, 
  onAnalyze, 
  analyzing, 
  hasInsights,
  uploadInfo 
}) {
  // State for custom prompt input
  const [showPromptInput, setShowPromptInput] = useState(false)
  const [customPrompt, setCustomPrompt] = useState('')

  // ---------------------------------------------------------------------------
  // Loading State
  // ---------------------------------------------------------------------------
  
  if (loading) {
    return (
      <Card>
        <div className="flex items-center justify-center py-12">
          <LoadingSpinner size="lg" />
          <span className="ml-3 text-gray-600">Loading metrics...</span>
        </div>
      </Card>
    )
  }

  // Don't render if no data
  if (!metrics && !uploadInfo) {
    return null
  }

  // ---------------------------------------------------------------------------
  // Handlers
  // ---------------------------------------------------------------------------

  const handleAnalyze = () => {
    onAnalyze(customPrompt || null)
  }

  // ---------------------------------------------------------------------------
  // Helper Functions
  // ---------------------------------------------------------------------------

  const formatBytes = (bytes) => {
    if (!bytes) return '0 B'
    if (bytes < 1024) return bytes + ' B'
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB'
    return (bytes / (1024 * 1024)).toFixed(2) + ' MB'
  }

  // ---------------------------------------------------------------------------
  // Render
  // ---------------------------------------------------------------------------

  return (
    <Card>
      {/* Header */}
      <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4 mb-6">
        <div>
          <h2 className="text-lg font-semibold text-gray-900">
            Data Metrics
          </h2>
          {uploadInfo && (
            <p className="text-sm text-gray-500 mt-1">
              {uploadInfo.filename} • {formatBytes(uploadInfo.file_size)}
            </p>
          )}
        </div>
        
        {/* Action Buttons */}
        <div className="flex items-center gap-2">
          <button
            onClick={() => setShowPromptInput(!showPromptInput)}
            className="text-sm text-gray-600 hover:text-gray-900 px-3 py-1.5 rounded border border-gray-200 hover:border-gray-300 transition-colors"
          >
            {showPromptInput ? 'Hide Options' : 'Custom Prompt'}
          </button>
          <Button
            onClick={handleAnalyze}
            disabled={analyzing}
            loading={analyzing}
            variant="primary"
          >
            {analyzing ? 'Analyzing...' : hasInsights ? 'Re-Analyze' : 'Analyze with AI'}
          </Button>
        </div>
      </div>

      {/* Custom Prompt Input */}
      {showPromptInput && (
        <div className="mb-6 p-4 bg-gray-50 rounded-lg">
          <label className="block text-sm font-medium text-gray-700 mb-2">
            Custom Analysis Instructions (optional)
          </label>
          <textarea
            value={customPrompt}
            onChange={(e) => setCustomPrompt(e.target.value)}
            placeholder="E.g., Focus on sales trends, identify top performers, find cost reduction opportunities..."
            className="w-full px-3 py-2 border border-gray-300 rounded-lg text-sm focus:ring-2 focus:ring-blue-500 focus:border-blue-500 resize-none"
            rows={2}
          />
        </div>
      )}

      {/* Summary Stats Grid */}
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-6">
        <StatCard 
          label="Rows" 
          value={metrics?.row_count?.toLocaleString() || '—'} 
          icon="rows"
        />
        <StatCard 
          label="Columns" 
          value={metrics?.column_count || '—'} 
          icon="columns"
        />
        <StatCard 
          label="Status" 
          value={uploadInfo?.status || 'Ready'} 
          icon="status"
          valueClassName="capitalize text-green-600"
        />
        <StatCard 
          label="AI Insights" 
          value={hasInsights ? 'Ready' : 'Pending'} 
          icon="ai"
          valueClassName={hasInsights ? 'text-green-600' : 'text-gray-400'}
        />
      </div>

      {/* Column Information */}
      {metrics?.columns && metrics.columns.length > 0 && (
        <div className="mb-6">
          <h3 className="text-sm font-medium text-gray-700 mb-3">Columns</h3>
          <div className="flex flex-wrap gap-2">
            {metrics.columns.map((col, idx) => (
              <span
                key={idx}
                className="inline-flex items-center px-3 py-1.5 rounded-full text-sm bg-blue-50 text-blue-700 border border-blue-100"
              >
                {col.name}
                <span className="ml-1.5 text-blue-400 text-xs">
                  ({col.dtype})
                </span>
              </span>
            ))}
          </div>
        </div>
      )}

      {/* Statistics Table */}
      {metrics?.stats && Object.keys(metrics.stats).length > 0 && (
        <div className="mb-6">
          <h3 className="text-sm font-medium text-gray-700 mb-3">Statistics</h3>
          <div className="overflow-x-auto rounded-lg border border-gray-200">
            <table className="min-w-full text-sm">
              <thead>
                <tr className="bg-gray-50">
                  <th className="px-4 py-3 text-left font-medium text-gray-600">Column</th>
                  <th className="px-4 py-3 text-right font-medium text-gray-600">Min</th>
                  <th className="px-4 py-3 text-right font-medium text-gray-600">Max</th>
                  <th className="px-4 py-3 text-right font-medium text-gray-600">Mean</th>
                  <th className="px-4 py-3 text-right font-medium text-gray-600">Median</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-gray-100">
                {Object.entries(metrics.stats).map(([colName, stats]) => (
                  <tr key={colName} className="hover:bg-gray-50">
                    <td className="px-4 py-3 font-medium text-gray-900">{colName}</td>
                    <td className="px-4 py-3 text-right text-gray-600">
                      {formatNumber(stats.min)}
                    </td>
                    <td className="px-4 py-3 text-right text-gray-600">
                      {formatNumber(stats.max)}
                    </td>
                    <td className="px-4 py-3 text-right text-gray-600">
                      {formatNumber(stats.mean)}
                    </td>
                    <td className="px-4 py-3 text-right text-gray-600">
                      {formatNumber(stats.median)}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* Data Preview */}
      {metrics?.preview && metrics.preview.length > 0 && (
        <div>
          <h3 className="text-sm font-medium text-gray-700 mb-3">
            Data Preview ({metrics.preview.length} rows)
          </h3>
          <div className="overflow-x-auto rounded-lg border border-gray-200">
            <table className="min-w-full text-sm">
              <thead>
                <tr className="bg-gray-50">
                  {Object.keys(metrics.preview[0]).map((key) => (
                    <th 
                      key={key} 
                      className="px-4 py-3 text-left font-medium text-gray-600 whitespace-nowrap"
                    >
                      {key}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody className="divide-y divide-gray-100">
                {metrics.preview.map((row, idx) => (
                  <tr key={idx} className="hover:bg-gray-50">
                    {Object.values(row).map((val, vIdx) => (
                      <td 
                        key={vIdx} 
                        className="px-4 py-3 text-gray-600 whitespace-nowrap max-w-xs truncate"
                      >
                        {String(val ?? '')}
                      </td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}
    </Card>
  )
}

// ---------------------------------------------------------------------------
// Helper Components
// ---------------------------------------------------------------------------

function StatCard({ label, value, icon, valueClassName = '' }) {
  const icons = {
    rows: (
      <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 6h16M4 10h16M4 14h16M4 18h16" />
      </svg>
    ),
    columns: (
      <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 17V7m0 10a2 2 0 01-2 2H5a2 2 0 01-2-2V7a2 2 0 012-2h2a2 2 0 012 2m0 10a2 2 0 002 2h2a2 2 0 002-2M9 7a2 2 0 012-2h2a2 2 0 012 2m0 10V7m0 10a2 2 0 002 2h2a2 2 0 002-2V7a2 2 0 00-2-2h-2a2 2 0 00-2 2" />
      </svg>
    ),
    status: (
      <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
      </svg>
    ),
    ai: (
      <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z" />
      </svg>
    ),
  }

  return (
    <div className="bg-gray-50 rounded-lg p-4">
      <div className="flex items-center text-gray-400 mb-2">
        {icons[icon]}
        <span className="ml-2 text-xs font-medium uppercase tracking-wide">{label}</span>
      </div>
      <p className={`text-2xl font-bold text-gray-900 ${valueClassName}`}>
        {value}
      </p>
    </div>
  )
}

function formatNumber(val) {
  if (val === null || val === undefined) return '—'
  if (typeof val === 'number') {
    return val.toLocaleString(undefined, { maximumFractionDigits: 2 })
  }
  return String(val)
}

MetricsDisplay.propTypes = {
  metrics: PropTypes.shape({
    row_count: PropTypes.number,
    column_count: PropTypes.number,
    columns: PropTypes.arrayOf(PropTypes.shape({
      name: PropTypes.string,
      dtype: PropTypes.string,
    })),
    stats: PropTypes.object,
    preview: PropTypes.array,
  }),
  loading: PropTypes.bool,
  onAnalyze: PropTypes.func.isRequired,
  analyzing: PropTypes.bool,
  hasInsights: PropTypes.bool,
  uploadInfo: PropTypes.object,
}

StatCard.propTypes = {
  label: PropTypes.string.isRequired,
  value: PropTypes.oneOfType([PropTypes.string, PropTypes.number]).isRequired,
  icon: PropTypes.string,
  valueClassName: PropTypes.string,
}

export default MetricsDisplay
