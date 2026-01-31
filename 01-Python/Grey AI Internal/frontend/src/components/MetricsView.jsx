import { useState } from 'react'

export default function MetricsView({ upload, onAnalyze, analyzing, hasInsights }) {
  const [customPrompt, setCustomPrompt] = useState('')
  const [showPromptInput, setShowPromptInput] = useState(false)

  if (!upload) {
    return null
  }

  const handleAnalyze = () => {
    onAnalyze(customPrompt || null)
  }

  const formatBytes = (bytes) => {
    if (bytes < 1024) return bytes + ' B'
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB'
    return (bytes / (1024 * 1024)).toFixed(2) + ' MB'
  }

  return (
    <div className="bg-white rounded-lg shadow">
      {/* Header */}
      <div className="px-6 py-4 border-b border-gray-100">
        <div className="flex items-center justify-between">
          <div>
            <h2 className="text-lg font-semibold text-gray-900">{upload.filename}</h2>
            <p className="text-sm text-gray-500">
              {formatBytes(upload.file_size)} • {upload.file_type.toUpperCase()}
            </p>
          </div>
          <div className="flex items-center gap-2">
            <button
              onClick={() => setShowPromptInput(!showPromptInput)}
              className="text-sm text-gray-600 hover:text-gray-900 px-3 py-1.5 rounded border border-gray-200 hover:border-gray-300"
            >
              {showPromptInput ? 'Hide Options' : 'Custom Prompt'}
            </button>
            <button
              onClick={handleAnalyze}
              disabled={analyzing}
              className={`
                px-4 py-2 rounded-lg text-sm font-medium
                transition-colors duration-200
                ${analyzing 
                  ? 'bg-gray-100 text-gray-400 cursor-not-allowed' 
                  : 'bg-primary-600 text-white hover:bg-primary-700'
                }
              `}
            >
              {analyzing ? (
                <span className="flex items-center">
                  <svg className="animate-spin -ml-1 mr-2 h-4 w-4" fill="none" viewBox="0 0 24 24">
                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
                  </svg>
                  Analyzing...
                </span>
              ) : hasInsights ? 'Re-Analyze' : 'Analyze with AI'}
            </button>
          </div>
        </div>
        
        {/* Custom Prompt Input */}
        {showPromptInput && (
          <div className="mt-4">
            <label className="block text-sm font-medium text-gray-700 mb-1">
              Custom Analysis Instructions (optional)
            </label>
            <textarea
              value={customPrompt}
              onChange={(e) => setCustomPrompt(e.target.value)}
              placeholder="E.g., Focus on sales trends, identify top performers, find cost reduction opportunities..."
              className="w-full px-3 py-2 border border-gray-300 rounded-lg text-sm focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
              rows={2}
            />
          </div>
        )}
      </div>

      {/* Metrics Grid */}
      {upload.metrics && (
        <div className="p-6">
          {/* Summary Stats */}
          <div className="grid grid-cols-2 sm:grid-cols-4 gap-4 mb-6">
            <div className="bg-gray-50 rounded-lg p-4">
              <p className="text-sm text-gray-500">Rows</p>
              <p className="text-2xl font-bold text-gray-900">
                {upload.metrics.row_count?.toLocaleString() || 0}
              </p>
            </div>
            <div className="bg-gray-50 rounded-lg p-4">
              <p className="text-sm text-gray-500">Columns</p>
              <p className="text-2xl font-bold text-gray-900">
                {upload.metrics.column_count || 0}
              </p>
            </div>
            <div className="bg-gray-50 rounded-lg p-4">
              <p className="text-sm text-gray-500">Status</p>
              <p className="text-lg font-semibold text-green-600 capitalize">
                {upload.status}
              </p>
            </div>
            <div className="bg-gray-50 rounded-lg p-4">
              <p className="text-sm text-gray-500">AI Insights</p>
              <p className={`text-lg font-semibold ${hasInsights ? 'text-green-600' : 'text-gray-400'}`}>
                {hasInsights ? 'Available' : 'Pending'}
              </p>
            </div>
          </div>

          {/* Columns */}
          {upload.metrics.columns && upload.metrics.columns.length > 0 && (
            <div className="mb-6">
              <h3 className="text-sm font-semibold text-gray-700 mb-3">Columns</h3>
              <div className="flex flex-wrap gap-2">
                {upload.metrics.columns.map((col, idx) => (
                  <span
                    key={idx}
                    className="inline-flex items-center px-3 py-1 rounded-full text-sm bg-blue-50 text-blue-700"
                  >
                    {col.name}
                    <span className="ml-1 text-blue-400 text-xs">({col.dtype})</span>
                  </span>
                ))}
              </div>
            </div>
          )}

          {/* Statistics */}
          {upload.metrics.stats && Object.keys(upload.metrics.stats).length > 0 && (
            <div className="mb-6">
              <h3 className="text-sm font-semibold text-gray-700 mb-3">Statistics</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full text-sm">
                  <thead>
                    <tr className="bg-gray-50">
                      <th className="px-4 py-2 text-left font-medium text-gray-600">Column</th>
                      <th className="px-4 py-2 text-right font-medium text-gray-600">Min</th>
                      <th className="px-4 py-2 text-right font-medium text-gray-600">Max</th>
                      <th className="px-4 py-2 text-right font-medium text-gray-600">Mean</th>
                      <th className="px-4 py-2 text-right font-medium text-gray-600">Median</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-100">
                    {Object.entries(upload.metrics.stats).map(([colName, stats]) => (
                      <tr key={colName}>
                        <td className="px-4 py-2 font-medium text-gray-900">{colName}</td>
                        <td className="px-4 py-2 text-right text-gray-600">
                          {stats.min?.toLocaleString(undefined, { maximumFractionDigits: 2 }) ?? '-'}
                        </td>
                        <td className="px-4 py-2 text-right text-gray-600">
                          {stats.max?.toLocaleString(undefined, { maximumFractionDigits: 2 }) ?? '-'}
                        </td>
                        <td className="px-4 py-2 text-right text-gray-600">
                          {stats.mean?.toLocaleString(undefined, { maximumFractionDigits: 2 }) ?? '-'}
                        </td>
                        <td className="px-4 py-2 text-right text-gray-600">
                          {stats.median?.toLocaleString(undefined, { maximumFractionDigits: 2 }) ?? '-'}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {/* Data Preview */}
          {upload.metrics.preview && upload.metrics.preview.length > 0 && (
            <div>
              <h3 className="text-sm font-semibold text-gray-700 mb-3">
                Data Preview (first {upload.metrics.preview.length} rows)
              </h3>
              <div className="overflow-x-auto border rounded-lg">
                <table className="min-w-full text-sm">
                  <thead>
                    <tr className="bg-gray-50">
                      {Object.keys(upload.metrics.preview[0]).map((key) => (
                        <th key={key} className="px-4 py-2 text-left font-medium text-gray-600 whitespace-nowrap">
                          {key}
                        </th>
                      ))}
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-100">
                    {upload.metrics.preview.map((row, idx) => (
                      <tr key={idx} className="hover:bg-gray-50">
                        {Object.values(row).map((val, vIdx) => (
                          <td key={vIdx} className="px-4 py-2 text-gray-600 whitespace-nowrap max-w-xs truncate">
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
        </div>
      )}
    </div>
  )
}
