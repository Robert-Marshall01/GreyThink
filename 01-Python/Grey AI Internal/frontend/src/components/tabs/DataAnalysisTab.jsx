/**
 * Data Analysis Tab Component
 * ===========================
 * CSV/TXT parsing with executive summaries.
 * Wraps existing upload and analysis functionality.
 */

import { useState, useCallback } from 'react'
import UploadComponent from '../features/UploadComponent'
import MetricsDisplay from '../features/MetricsDisplay'
import InsightsDisplay from '../features/InsightsDisplay'
import { fetchMetrics, triggerAnalysis } from '../../services/api'

export default function DataAnalysisTab() {
  const [currentUpload, setCurrentUpload] = useState(null)
  const [metrics, setMetrics] = useState(null)
  const [insights, setInsights] = useState(null)
  const [loading, setLoading] = useState({ metrics: false, analysis: false })
  const [error, setError] = useState(null)

  const handleUploadComplete = useCallback(async (uploadData) => {
    setCurrentUpload(uploadData)
    setInsights(null)
    setError(null)
    
    if (uploadData.row_count !== undefined) {
      setMetrics({
        row_count: uploadData.row_count,
        column_count: uploadData.column_count,
      })
    }
    
    setLoading(prev => ({ ...prev, metrics: true }))
    try {
      const fullMetrics = await fetchMetrics(uploadData.id)
      setMetrics(fullMetrics)
    } catch (err) {
      console.log('Full metrics not available:', err.message)
    } finally {
      setLoading(prev => ({ ...prev, metrics: false }))
    }
  }, [])

  const handleAnalyze = useCallback(async (customPrompt = null) => {
    if (!currentUpload) return
    
    setError(null)
    setLoading(prev => ({ ...prev, analysis: true }))
    
    try {
      const result = await triggerAnalysis(currentUpload.id, customPrompt)
      setInsights(result)
    } catch (err) {
      setError(err.message || 'Analysis failed. Ensure Ollama is running.')
    } finally {
      setLoading(prev => ({ ...prev, analysis: false }))
    }
  }, [currentUpload])

  const handleReset = useCallback(() => {
    setCurrentUpload(null)
    setMetrics(null)
    setInsights(null)
    setError(null)
  }, [])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex justify-between items-start">
        <div>
          <h2 className="text-2xl font-bold text-gray-900">Data Analysis</h2>
          <p className="text-gray-600 mt-1">
            Upload CSV/TXT files for AI-powered executive summaries and insights
          </p>
        </div>
        {currentUpload && (
          <button
            onClick={handleReset}
            className="text-sm text-gray-600 hover:text-gray-900 flex items-center gap-1"
          >
            <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
            </svg>
            New Analysis
          </button>
        )}
      </div>

      {/* Error Banner */}
      {error && (
        <div className="bg-red-50 border-l-4 border-red-500 p-4 rounded-r-lg">
          <div className="flex items-center justify-between">
            <div className="flex items-center">
              <svg className="w-5 h-5 text-red-500 mr-3" fill="currentColor" viewBox="0 0 20 20">
                <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
              </svg>
              <p className="text-red-700 font-medium">{error}</p>
            </div>
            <button 
              onClick={() => setError(null)}
              className="text-red-500 hover:text-red-700"
            >
              <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
                <path fillRule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clipRule="evenodd" />
              </svg>
            </button>
          </div>
        </div>
      )}

      {/* Content Grid */}
      <div className="grid grid-cols-1 lg:grid-cols-12 gap-6">
        {/* Left Column: Upload */}
        <div className="lg:col-span-4">
          <UploadComponent 
            onUploadComplete={handleUploadComplete}
            currentUpload={currentUpload}
          />
        </div>

        {/* Right Column: Metrics & Insights */}
        <div className="lg:col-span-8 space-y-6">
          {/* Metrics Display */}
          <MetricsDisplay 
            metrics={metrics}
            loading={loading.metrics}
            onAnalyze={handleAnalyze}
            analyzing={loading.analysis}
            hasInsights={!!insights}
            uploadInfo={currentUpload}
          />

          {/* Insights Display */}
          <InsightsDisplay 
            insights={insights}
            loading={loading.analysis}
          />

          {/* Empty State */}
          {!currentUpload && !loading.metrics && (
            <div className="bg-white rounded-xl shadow-sm border border-gray-100 p-12 text-center">
              <div className="w-16 h-16 bg-blue-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg className="w-8 h-8 text-blue-500" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 17v-2m3 2v-4m3 4v-6m2 10H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
              </div>
              <h3 className="text-lg font-semibold text-gray-900 mb-2">
                Upload Data to Get Started
              </h3>
              <p className="text-gray-500 max-w-sm mx-auto">
                Upload a CSV or text file to receive AI-powered executive summaries 
                with insights, implications, and recommendations.
              </p>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
