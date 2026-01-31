/**
 * AI Code Tab Component
 * =====================
 * Generate, refactor, and test code with AI assistance.
 * Demonstrates AI-assisted development workflows.
 */

import { useState, useCallback } from 'react'
import { Card, Button, LoadingSpinner } from '../ui'
import { rawInference } from '../../services/api'

const CODE_TASKS = [
  { id: 'generate', label: 'Generate Code', description: 'Create new code from description' },
  { id: 'refactor', label: 'Refactor', description: 'Improve existing code' },
  { id: 'test', label: 'Write Tests', description: 'Generate unit tests' },
  { id: 'debug', label: 'Debug', description: 'Find and fix issues' },
  { id: 'document', label: 'Document', description: 'Add comments and docs' },
  { id: 'optimize', label: 'Optimize', description: 'Improve performance' },
]

const LANGUAGES = [
  { id: 'python', label: 'Python' },
  { id: 'javascript', label: 'JavaScript' },
  { id: 'typescript', label: 'TypeScript' },
  { id: 'sql', label: 'SQL' },
  { id: 'bash', label: 'Bash' },
]

export default function AICodeTab() {
  const [task, setTask] = useState('generate')
  const [language, setLanguage] = useState('python')
  const [description, setDescription] = useState('')
  const [code, setCode] = useState('')
  const [result, setResult] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)

  const handleGenerate = useCallback(async () => {
    if (!description.trim() && !code.trim()) {
      setError('Please enter a description or paste code')
      return
    }

    setLoading(true)
    setError(null)
    setResult(null)

    try {
      const taskLabel = CODE_TASKS.find(t => t.id === task)?.label || task
      
      const systemPrompt = `You are an expert ${language} developer. Provide clean, well-documented, production-ready code. Include comments explaining key logic.`
      
      let prompt = ''
      if (task === 'generate') {
        prompt = `Generate ${language} code for: ${description}`
      } else if (task === 'refactor') {
        prompt = `Refactor this ${language} code to be cleaner, more efficient, and follow best practices:\n\n\`\`\`${language}\n${code}\n\`\`\`\n\nProvide the improved code with explanations.`
      } else if (task === 'test') {
        prompt = `Write comprehensive unit tests for this ${language} code:\n\n\`\`\`${language}\n${code}\n\`\`\`\n\nUse appropriate testing frameworks (pytest for Python, Jest for JS/TS).`
      } else if (task === 'debug') {
        prompt = `Debug this ${language} code. Find issues and provide the fixed version:\n\n\`\`\`${language}\n${code}\n\`\`\`\n\nExplain what was wrong and how you fixed it.`
      } else if (task === 'document') {
        prompt = `Add comprehensive documentation to this ${language} code:\n\n\`\`\`${language}\n${code}\n\`\`\`\n\nInclude docstrings, type hints, and inline comments.`
      } else if (task === 'optimize') {
        prompt = `Optimize this ${language} code for better performance:\n\n\`\`\`${language}\n${code}\n\`\`\`\n\nExplain the optimizations made and their impact.`
      }

      const response = await rawInference(prompt, systemPrompt)
      
      setResult({
        output: response.response,
        task: taskLabel,
        language: language,
        model: response.model_used || 'AI',
        generatedAt: new Date().toISOString(),
      })
    } catch (err) {
      setError(err.message || 'Generation failed. Ensure Ollama is running.')
    } finally {
      setLoading(false)
    }
  }, [description, code, task, language])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold text-gray-900">AI Code Assistant</h2>
        <p className="text-gray-600 mt-1">
          Generate, refactor, test, and optimize code with AI assistance
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Panel */}
        <Card>
          <div className="p-6 space-y-5">
            <h3 className="font-semibold text-gray-900">Configure Task</h3>

            {/* Task Selection */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Task
              </label>
              <div className="grid grid-cols-3 gap-2">
                {CODE_TASKS.map((t) => (
                  <button
                    key={t.id}
                    onClick={() => setTask(t.id)}
                    className={`p-2 rounded-lg border text-center transition-all ${
                      task === t.id
                        ? 'border-green-500 bg-green-50 text-green-700'
                        : 'border-gray-200 hover:border-gray-300'
                    }`}
                  >
                    <div className="font-medium text-sm">{t.label}</div>
                  </button>
                ))}
              </div>
            </div>

            {/* Language */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Language
              </label>
              <div className="flex flex-wrap gap-2">
                {LANGUAGES.map((l) => (
                  <button
                    key={l.id}
                    onClick={() => setLanguage(l.id)}
                    className={`px-4 py-2 rounded-full text-sm font-medium transition-all ${
                      language === l.id
                        ? 'bg-green-600 text-white'
                        : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                    }`}
                  >
                    {l.label}
                  </button>
                ))}
              </div>
            </div>

            {/* Description (for generate) */}
            {task === 'generate' && (
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Description
                </label>
                <textarea
                  value={description}
                  onChange={(e) => setDescription(e.target.value)}
                  placeholder="Describe what you want to build, e.g., 'A FastAPI endpoint that validates email addresses'"
                  rows={4}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-green-500 focus:border-green-500 font-mono text-sm"
                />
              </div>
            )}

            {/* Code Input (for other tasks) */}
            {task !== 'generate' && (
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Paste Your Code
                </label>
                <textarea
                  value={code}
                  onChange={(e) => setCode(e.target.value)}
                  placeholder={`Paste your ${language} code here...`}
                  rows={8}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-green-500 focus:border-green-500 font-mono text-sm bg-gray-900 text-gray-100"
                />
              </div>
            )}

            {/* Error */}
            {error && (
              <div className="p-3 bg-red-50 border border-red-200 rounded-lg text-red-700 text-sm">
                {error}
              </div>
            )}

            {/* Generate Button */}
            <Button
              onClick={handleGenerate}
              disabled={loading || (!description.trim() && !code.trim())}
              className="w-full bg-green-600 hover:bg-green-700"
            >
              {loading ? (
                <span className="flex items-center justify-center gap-2">
                  <LoadingSpinner size="sm" />
                  Processing...
                </span>
              ) : (
                `${CODE_TASKS.find(t => t.id === task)?.label || 'Generate'}`
              )}
            </Button>
          </div>
        </Card>

        {/* Output Panel */}
        <Card>
          <div className="p-6">
            <h3 className="font-semibold text-gray-900 mb-4">Output</h3>
            
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <LoadingSpinner />
              </div>
            ) : result ? (
              <div className="space-y-4">
                {/* Metadata */}
                <div className="flex flex-wrap gap-2 text-xs">
                  <span className="px-2 py-1 bg-green-100 text-green-700 rounded-full">
                    {result.task}
                  </span>
                  <span className="px-2 py-1 bg-blue-100 text-blue-700 rounded-full">
                    {result.language}
                  </span>
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {result.model}
                  </span>
                </div>

                {/* Code Output */}
                <div className="bg-gray-900 rounded-lg p-4 max-h-96 overflow-y-auto">
                  <pre className="text-gray-100 font-mono text-sm whitespace-pre-wrap">
                    {result.output}
                  </pre>
                </div>

                {/* Copy Button */}
                <button
                  onClick={() => navigator.clipboard.writeText(result.output)}
                  className="text-sm text-gray-600 hover:text-gray-900 flex items-center gap-1"
                >
                  <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M8 16H6a2 2 0 01-2-2V6a2 2 0 012-2h8a2 2 0 012 2v2m-6 12h8a2 2 0 002-2v-8a2 2 0 00-2-2h-8a2 2 0 00-2 2v8a2 2 0 002 2z" />
                  </svg>
                  Copy to clipboard
                </button>
              </div>
            ) : (
              <div className="text-center py-12 text-gray-500">
                <svg className="w-12 h-12 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4" />
                </svg>
                <p>Configure a task to see generated code</p>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}
