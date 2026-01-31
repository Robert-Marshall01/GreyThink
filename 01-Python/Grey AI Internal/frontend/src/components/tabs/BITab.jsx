/**
 * Business Intelligence Tab Component
 * ====================================
 * Dashboards, analytics, and industry-specific insights.
 * Demonstrates data visualization and strategic analysis.
 */

import { useState, useEffect, useCallback } from 'react'
import { Card, Button, LoadingSpinner } from '../ui'
import { fetchUploads, rawInference } from '../../services/api'

const ANALYSIS_TYPES = [
  { id: 'industry', label: 'Industry Analysis', description: 'Sector trends and benchmarks' },
  { id: 'competitive', label: 'Competitive Intel', description: 'Market positioning and gaps' },
  { id: 'workforce', label: 'Workforce Analysis', description: 'Talent trends and scaling' },
  { id: 'modernization', label: 'Modernization Roadmap', description: 'Digital transformation priorities' },
]

export default function BITab() {
  const [analysisType, setAnalysisType] = useState('industry')
  const [context, setContext] = useState('')
  const [uploads, setUploads] = useState([])
  const [selectedUpload, setSelectedUpload] = useState(null)
  const [result, setResult] = useState(null)
  const [loading, setLoading] = useState(false)
  const [loadingUploads, setLoadingUploads] = useState(true)
  const [error, setError] = useState(null)

  // Load recent uploads for context
  useEffect(() => {
    async function loadUploads() {
      try {
        const data = await fetchUploads({ pageSize: 5 })
        setUploads(data.items || [])
      } catch (err) {
        console.log('Could not load uploads:', err.message)
      } finally {
        setLoadingUploads(false)
      }
    }
    loadUploads()
  }, [])

  const handleAnalyze = useCallback(async () => {
    if (!context.trim() && !selectedUpload) {
      setError('Please enter context or select an upload')
      return
    }

    setLoading(true)
    setError(null)

    try {
      const analysisInfo = ANALYSIS_TYPES.find(a => a.id === analysisType)
      
      const systemPrompt = `You are a senior business strategist and industry analyst. Provide executive-level insights with clear actionable recommendations. Use data to support conclusions.`
      
      let prompt = `Perform a ${analysisInfo?.label || 'business'} analysis.

Context:
${context || `Analysis requested for upload: ${selectedUpload?.filename}`}

Provide:
1. Executive Summary (2-3 sentences)
2. Key Findings (3-5 bullet points)
3. Strategic Implications
4. Recommended Actions (prioritized High/Medium/Low)
5. KPIs to Track

Be specific, tie insights to business outcomes like revenue, efficiency, or competitive advantage.`

      const response = await rawInference(prompt, systemPrompt)
      
      setResult({
        analysis: response.response,
        type: analysisInfo?.label,
        model: response.model_used || 'AI',
        timestamp: new Date().toISOString(),
      })
    } catch (err) {
      setError(err.message || 'Analysis failed')
    } finally {
      setLoading(false)
    }
  }, [context, selectedUpload, analysisType])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold text-gray-900">Business Intelligence</h2>
        <p className="text-gray-600 mt-1">
          Strategic analysis, industry insights, and modernization roadmaps
        </p>
      </div>

      {/* Quick Stats (Demo) */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <Card>
          <div className="p-4 text-center">
            <div className="text-2xl font-bold text-blue-600">{uploads.length}</div>
            <div className="text-sm text-gray-600">Datasets Available</div>
          </div>
        </Card>
        <Card>
          <div className="p-4 text-center">
            <div className="text-2xl font-bold text-green-600">4</div>
            <div className="text-sm text-gray-600">Analysis Types</div>
          </div>
        </Card>
        <Card>
          <div className="p-4 text-center">
            <div className="text-2xl font-bold text-purple-600">12</div>
            <div className="text-sm text-gray-600">AI Domains</div>
          </div>
        </Card>
        <Card>
          <div className="p-4 text-center">
            <div className="text-2xl font-bold text-orange-600">100%</div>
            <div className="text-sm text-gray-600">Local Processing</div>
          </div>
        </Card>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Panel */}
        <Card>
          <div className="p-6 space-y-5">
            <h3 className="font-semibold text-gray-900">Configure Analysis</h3>

            {/* Analysis Type */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Analysis Type
              </label>
              <div className="grid grid-cols-2 gap-2">
                {ANALYSIS_TYPES.map((type) => (
                  <button
                    key={type.id}
                    onClick={() => setAnalysisType(type.id)}
                    className={`p-3 rounded-lg border text-left transition-all ${
                      analysisType === type.id
                        ? 'border-indigo-500 bg-indigo-50 text-indigo-700'
                        : 'border-gray-200 hover:border-gray-300'
                    }`}
                  >
                    <div className="font-medium text-sm">{type.label}</div>
                    <div className="text-xs text-gray-500 mt-0.5">{type.description}</div>
                  </button>
                ))}
              </div>
            </div>

            {/* Recent Uploads */}
            {!loadingUploads && uploads.length > 0 && (
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Use Existing Data (optional)
                </label>
                <div className="flex flex-wrap gap-2">
                  {uploads.map((upload) => (
                    <button
                      key={upload.id}
                      onClick={() => setSelectedUpload(
                        selectedUpload?.id === upload.id ? null : upload
                      )}
                      className={`px-3 py-1.5 rounded-full text-sm transition-all ${
                        selectedUpload?.id === upload.id
                          ? 'bg-indigo-600 text-white'
                          : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                      }`}
                    >
                      {upload.filename}
                    </button>
                  ))}
                </div>
              </div>
            )}

            {/* Context Input */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Business Context
              </label>
              <textarea
                value={context}
                onChange={(e) => setContext(e.target.value)}
                placeholder="Describe the business context, industry, or specific questions you want to analyze..."
                rows={5}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
              />
            </div>

            {/* Error */}
            {error && (
              <div className="p-3 bg-red-50 border border-red-200 rounded-lg text-red-700 text-sm">
                {error}
              </div>
            )}

            {/* Analyze Button */}
            <Button
              onClick={handleAnalyze}
              disabled={loading || (!context.trim() && !selectedUpload)}
              className="w-full bg-indigo-600 hover:bg-indigo-700"
            >
              {loading ? (
                <span className="flex items-center justify-center gap-2">
                  <LoadingSpinner size="sm" />
                  Analyzing...
                </span>
              ) : (
                'Generate Intelligence Report'
              )}
            </Button>
          </div>
        </Card>

        {/* Output Panel */}
        <Card>
          <div className="p-6">
            <h3 className="font-semibold text-gray-900 mb-4">Intelligence Report</h3>
            
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <LoadingSpinner />
              </div>
            ) : result ? (
              <div className="space-y-4">
                {/* Metadata */}
                <div className="flex flex-wrap gap-2 text-xs">
                  <span className="px-2 py-1 bg-indigo-100 text-indigo-700 rounded-full">
                    {result.type}
                  </span>
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {result.model}
                  </span>
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {new Date(result.timestamp).toLocaleString()}
                  </span>
                </div>

                {/* Report Content */}
                <div className="bg-gray-50 rounded-lg p-4 max-h-80 overflow-y-auto">
                  <pre className="whitespace-pre-wrap text-gray-800 text-sm font-sans leading-relaxed">
                    {result.analysis}
                  </pre>
                </div>

                {/* JSON Output */}
                <details>
                  <summary className="cursor-pointer text-sm text-gray-600 hover:text-gray-900">
                    View JSON Report
                  </summary>
                  <pre className="mt-2 p-3 bg-gray-900 text-gray-100 rounded-lg text-xs overflow-x-auto">
                    {JSON.stringify({
                      insight: `${result.type} report generated`,
                      implication: 'Data-driven strategic recommendations available',
                      recommendation: 'Review high-priority actions and implement KPI tracking',
                      priority: 'High',
                      domain: 'BI',
                      analysis_type: result.type,
                      timestamp: result.timestamp,
                    }, null, 2)}
                  </pre>
                </details>
              </div>
            ) : (
              <div className="text-center py-12 text-gray-500">
                <svg className="w-12 h-12 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M16 8v8m-4-5v5m-4-2v2m-2 4h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z" />
                </svg>
                <p>Configure analysis to generate intelligence</p>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}
