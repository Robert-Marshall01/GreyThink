/**
 * AI Writing Tab Component
 * ========================
 * Generate blog posts, documentation, and executive briefings.
 * Demonstrates content generation with professional tone.
 */

import { useState, useCallback } from 'react'
import { Card, Button, LoadingSpinner } from '../ui'
import { rawInference } from '../../services/api'

const CONTENT_TYPES = [
  { id: 'blog', label: 'Blog Post', description: 'SEO-friendly article with headers and CTAs' },
  { id: 'docs', label: 'Documentation', description: 'Technical docs with examples' },
  { id: 'briefing', label: 'Executive Briefing', description: 'Strategic summary for leadership' },
  { id: 'email', label: 'Professional Email', description: 'Business communication' },
]

const TONES = [
  { id: 'professional', label: 'Professional' },
  { id: 'consultative', label: 'Consultative' },
  { id: 'technical', label: 'Technical' },
  { id: 'friendly', label: 'Friendly' },
]

export default function AIWritingTab() {
  const [contentType, setContentType] = useState('blog')
  const [tone, setTone] = useState('professional')
  const [topic, setTopic] = useState('')
  const [context, setContext] = useState('')
  const [result, setResult] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)

  const handleGenerate = useCallback(async () => {
    if (!topic.trim()) {
      setError('Please enter a topic')
      return
    }

    setLoading(true)
    setError(null)
    setResult(null)

    try {
      const typeLabel = CONTENT_TYPES.find(t => t.id === contentType)?.label || 'content'
      
      const systemPrompt = `You are a professional content writer. Write in a ${tone} tone. Be clear, actionable, and engaging.`
      
      const prompt = `Write a ${typeLabel} about: ${topic}
${context ? `\nAdditional context: ${context}` : ''}

Format the output with clear sections. Be specific and provide actionable insights.`

      const response = await rawInference(prompt, systemPrompt)
      
      setResult({
        content: response.response,
        type: typeLabel,
        tone: tone,
        model: response.model_used || 'AI',
        generatedAt: new Date().toISOString(),
      })
    } catch (err) {
      setError(err.message || 'Generation failed. Ensure Ollama is running.')
    } finally {
      setLoading(false)
    }
  }, [topic, context, contentType, tone])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold text-gray-900">AI Writing</h2>
        <p className="text-gray-600 mt-1">
          Generate professional content with configurable tone and format
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Panel */}
        <Card>
          <div className="p-6 space-y-5">
            <h3 className="font-semibold text-gray-900">Configure Content</h3>

            {/* Content Type */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Content Type
              </label>
              <div className="grid grid-cols-2 gap-2">
                {CONTENT_TYPES.map((type) => (
                  <button
                    key={type.id}
                    onClick={() => setContentType(type.id)}
                    className={`p-3 rounded-lg border text-left transition-all ${
                      contentType === type.id
                        ? 'border-blue-500 bg-blue-50 text-blue-700'
                        : 'border-gray-200 hover:border-gray-300'
                    }`}
                  >
                    <div className="font-medium text-sm">{type.label}</div>
                    <div className="text-xs text-gray-500 mt-0.5">{type.description}</div>
                  </button>
                ))}
              </div>
            </div>

            {/* Tone */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Tone
              </label>
              <div className="flex flex-wrap gap-2">
                {TONES.map((t) => (
                  <button
                    key={t.id}
                    onClick={() => setTone(t.id)}
                    className={`px-4 py-2 rounded-full text-sm font-medium transition-all ${
                      tone === t.id
                        ? 'bg-blue-600 text-white'
                        : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                    }`}
                  >
                    {t.label}
                  </button>
                ))}
              </div>
            </div>

            {/* Topic */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Topic *
              </label>
              <input
                type="text"
                value={topic}
                onChange={(e) => setTopic(e.target.value)}
                placeholder="e.g., Benefits of MLOps for Enterprise AI"
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
              />
            </div>

            {/* Context */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Additional Context (optional)
              </label>
              <textarea
                value={context}
                onChange={(e) => setContext(e.target.value)}
                placeholder="Any specific points to cover, audience details, or constraints..."
                rows={3}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
              />
            </div>

            {/* Error */}
            {error && (
              <div className="p-3 bg-red-50 border border-red-200 rounded-lg text-red-700 text-sm">
                {error}
              </div>
            )}

            {/* Generate Button */}
            <Button
              onClick={handleGenerate}
              disabled={loading || !topic.trim()}
              className="w-full"
            >
              {loading ? (
                <span className="flex items-center justify-center gap-2">
                  <LoadingSpinner size="sm" />
                  Generating...
                </span>
              ) : (
                'Generate Content'
              )}
            </Button>
          </div>
        </Card>

        {/* Output Panel */}
        <Card>
          <div className="p-6">
            <h3 className="font-semibold text-gray-900 mb-4">Generated Content</h3>
            
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <LoadingSpinner />
              </div>
            ) : result ? (
              <div className="space-y-4">
                {/* Metadata */}
                <div className="flex flex-wrap gap-2 text-xs">
                  <span className="px-2 py-1 bg-blue-100 text-blue-700 rounded-full">
                    {result.type}
                  </span>
                  <span className="px-2 py-1 bg-purple-100 text-purple-700 rounded-full">
                    {result.tone} tone
                  </span>
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {result.model}
                  </span>
                </div>

                {/* Content */}
                <div className="prose prose-sm max-w-none bg-gray-50 rounded-lg p-4 max-h-96 overflow-y-auto">
                  <pre className="whitespace-pre-wrap text-gray-800 font-sans text-sm leading-relaxed">
                    {result.content}
                  </pre>
                </div>

                {/* JSON Output */}
                <details className="mt-4">
                  <summary className="cursor-pointer text-sm text-gray-600 hover:text-gray-900">
                    View JSON Output
                  </summary>
                  <pre className="mt-2 p-3 bg-gray-900 text-gray-100 rounded-lg text-xs overflow-x-auto">
                    {JSON.stringify({
                      insight: topic,
                      implication: `Generated ${result.type} content for strategic use`,
                      recommendation: result.content.slice(0, 200) + '...',
                      priority: 'Medium',
                      domain: 'Writing',
                    }, null, 2)}
                  </pre>
                </details>
              </div>
            ) : (
              <div className="text-center py-12 text-gray-500">
                <svg className="w-12 h-12 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" />
                </svg>
                <p>Configure and generate content to see results</p>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}
