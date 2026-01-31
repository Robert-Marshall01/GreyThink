/**
 * Insights Display Component
 * ==========================
 * Displays AI-generated insights including summary, key findings,
 * and actionable recommendations.
 * 
 * Receives data from:
 *   POST /api/ai/analyze/{id} → triggers analysis
 *   GET /api/ai/insights/{id} → fetches cached insights
 * 
 * Expected data shape:
 * {
 *   id: number,
 *   upload_id: number,
 *   summary: string,           // or ai_summary
 *   insights: string[],        // or insights_json
 *   recommendations: object,   // or ai_recommendations
 *   model_used: string,
 *   created_at: string
 * }
 */

import PropTypes from 'prop-types'
import Card from '../ui/Card'
import LoadingSpinner from '../ui/LoadingSpinner'

// Helper to normalize insight field names from backend
function normalizeInsights(data) {
  if (!data) return null
  return {
    id: data.id,
    upload_id: data.upload_id,
    summary: data.summary || data.ai_summary || '',
    insights: data.insights || data.insights_json || [],
    recommendations: data.recommendations || data.ai_recommendations || null,
    model_used: data.model_used,
    created_at: data.created_at,
  }
}

function InsightsDisplay({ insights: rawInsights, loading }) {
  // Normalize the insights data to handle field name variations
  const insights = normalizeInsights(rawInsights)
  // ---------------------------------------------------------------------------
  // Loading State
  // ---------------------------------------------------------------------------
  
  if (loading) {
    return (
      <Card className="bg-gradient-to-br from-blue-50 to-indigo-50 border-blue-100">
        <div className="flex items-center justify-center py-12">
          <div className="text-center">
            <div className="w-16 h-16 bg-white rounded-full flex items-center justify-center mx-auto mb-4 shadow-sm">
              <LoadingSpinner size="lg" />
            </div>
            <h3 className="text-lg font-medium text-gray-900 mb-1">
              AI is analyzing your data...
            </h3>
            <p className="text-sm text-gray-500">
              This may take a moment depending on data size
            </p>
          </div>
        </div>
      </Card>
    )
  }

  // Don't render if no insights
  if (!insights) {
    return null
  }

  // ---------------------------------------------------------------------------
  // Helper Functions
  // ---------------------------------------------------------------------------

  const formatDate = (dateString) => {
    if (!dateString) return ''
    const date = new Date(dateString)
    return date.toLocaleDateString('en-US', {
      month: 'long',
      day: 'numeric',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    })
  }

  // ---------------------------------------------------------------------------
  // Render
  // ---------------------------------------------------------------------------

  return (
    <Card className="overflow-hidden">
      {/* Header with gradient background */}
      <div className="bg-gradient-to-r from-blue-600 to-indigo-600 -m-6 mb-6 p-6">
        <div className="flex items-center space-x-3">
          <div className="w-10 h-10 bg-white/20 rounded-full flex items-center justify-center">
            <svg className="w-5 h-5 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z" />
            </svg>
          </div>
          <div>
            <h2 className="text-lg font-semibold text-white">
              AI-Generated Insights
            </h2>
            <p className="text-sm text-blue-100">
              Model: {insights.model_used} • {formatDate(insights.created_at)}
            </p>
          </div>
        </div>
      </div>

      {/* Content */}
      <div className="space-y-6">
        
        {/* Executive Summary */}
        <Section 
          title="Executive Summary" 
          icon="document"
        >
          <div className="bg-gray-50 rounded-lg p-4">
            <p className="text-gray-700 leading-relaxed">
              {insights.summary}
            </p>
          </div>
        </Section>

        {/* Key Insights */}
        {insights.insights && insights.insights.length > 0 && (
          <Section 
            title="Key Insights" 
            icon="lightbulb"
          >
            <ul className="space-y-3">
              {insights.insights.map((insight, idx) => (
                <li key={idx} className="flex items-start group">
                  <span className="flex-shrink-0 w-7 h-7 bg-blue-100 text-blue-600 rounded-full flex items-center justify-center text-sm font-medium mr-3 mt-0.5 group-hover:bg-blue-200 transition-colors">
                    {idx + 1}
                  </span>
                  <p className="text-gray-700 leading-relaxed pt-0.5">
                    {insight}
                  </p>
                </li>
              ))}
            </ul>
          </Section>
        )}

        {/* Recommendations */}
        {insights.recommendations && (
          <Section 
            title="Recommendations" 
            icon="clipboard"
          >
            <div className="bg-green-50 border border-green-100 rounded-lg p-4">
              {/* Handle object recommendations (from backend) or string */}
              {typeof insights.recommendations === 'object' && insights.recommendations.actions ? (
                <ul className="space-y-2">
                  {insights.recommendations.actions.map((action, idx) => (
                    <li key={idx} className="flex items-start">
                      <svg className="w-5 h-5 text-green-500 mr-2 mt-0.5 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                      </svg>
                      <span className="text-gray-700">{action}</span>
                    </li>
                  ))}
                </ul>
              ) : (
                <p className="text-gray-700 leading-relaxed">
                  {typeof insights.recommendations === 'string' 
                    ? insights.recommendations 
                    : JSON.stringify(insights.recommendations)}
                </p>
              )}
            </div>
          </Section>
        )}
      </div>

      {/* Footer */}
      <div className="mt-6 pt-4 border-t border-gray-100 flex items-center justify-between text-xs text-gray-400">
        <span>Generated by local LLM</span>
        <span>ID: {insights.id}</span>
      </div>
    </Card>
  )
}

// ---------------------------------------------------------------------------
// Helper Components
// ---------------------------------------------------------------------------

function Section({ title, icon, children }) {
  const icons = {
    document: (
      <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
      </svg>
    ),
    lightbulb: (
      <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 10V3L4 14h7v7l9-11h-7z" />
      </svg>
    ),
    clipboard: (
      <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4" />
      </svg>
    ),
  }

  return (
    <div>
      <h3 className="text-sm font-semibold text-gray-700 uppercase tracking-wider mb-3 flex items-center">
        <span className="text-gray-400 mr-2">{icons[icon]}</span>
        {title}
      </h3>
      {children}
    </div>
  )
}

Section.propTypes = {
  title: PropTypes.string.isRequired,
  icon: PropTypes.string,
  children: PropTypes.node.isRequired,
}

InsightsDisplay.propTypes = {
  insights: PropTypes.shape({
    id: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
    upload_id: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
    summary: PropTypes.string,
    ai_summary: PropTypes.string,  // Alias from backend
    insights: PropTypes.arrayOf(PropTypes.string),
    insights_json: PropTypes.arrayOf(PropTypes.string),  // Alias from backend
    recommendations: PropTypes.oneOfType([PropTypes.string, PropTypes.object]),
    ai_recommendations: PropTypes.object,  // Alias from backend
    model_used: PropTypes.string,
    created_at: PropTypes.string,
  }),
  loading: PropTypes.bool,
}

export default InsightsDisplay
