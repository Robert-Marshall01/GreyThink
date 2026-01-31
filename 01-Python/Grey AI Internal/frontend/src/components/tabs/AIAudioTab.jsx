/**
 * AI Audio Tab Component
 * ======================
 * Text-to-speech and voice synthesis demonstration.
 * Showcases audio AI integration concepts.
 */

import { useState, useCallback } from 'react'
import { Card, Button, LoadingSpinner } from '../ui'
import { rawInference } from '../../services/api'

const VOICES = [
  { id: 'professional', label: 'Professional', description: 'Clear, authoritative business voice' },
  { id: 'friendly', label: 'Friendly', description: 'Warm, approachable tone' },
  { id: 'narrator', label: 'Narrator', description: 'Documentary-style narration' },
  { id: 'podcast', label: 'Podcast Host', description: 'Conversational, engaging' },
]

const CONTENT_TYPES = [
  { id: 'summary', label: 'Executive Summary' },
  { id: 'podcast', label: 'Podcast Script' },
  { id: 'narration', label: 'Narration' },
  { id: 'transcript', label: 'Meeting Transcript' },
]

export default function AIAudioTab() {
  const [text, setText] = useState('')
  const [voice, setVoice] = useState('professional')
  const [contentType, setContentType] = useState('summary')
  const [result, setResult] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)

  const handleGenerate = useCallback(async () => {
    if (!text.trim()) {
      setError('Please enter text to convert')
      return
    }

    setLoading(true)
    setError(null)

    try {
      const voiceInfo = VOICES.find(v => v.id === voice)
      const typeInfo = CONTENT_TYPES.find(t => t.id === contentType)
      
      const systemPrompt = `You are a professional audio script writer. Format text for ${voiceInfo?.label || voice} voice narration. Add speaking cues like [PAUSE], [EMPHASIS], [SLOWER].`
      
      const prompt = `Convert this text into a ${typeInfo?.label || 'script'} ready for text-to-speech:

${text}

Add appropriate pacing markers and emphasis cues. Format for natural speech.`

      const response = await rawInference(prompt, systemPrompt)
      
      // Estimate audio duration (rough: 150 words per minute)
      const wordCount = response.response.split(/\s+/).length
      const estimatedMinutes = Math.ceil(wordCount / 150)
      
      setResult({
        script: response.response,
        voice: voiceInfo?.label,
        type: typeInfo?.label,
        wordCount,
        estimatedDuration: `~${estimatedMinutes} min`,
        model: response.model_used || 'AI',
        generatedAt: new Date().toISOString(),
      })
    } catch (err) {
      setError(err.message || 'Generation failed')
    } finally {
      setLoading(false)
    }
  }, [text, voice, contentType])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold text-gray-900">AI Audio</h2>
        <p className="text-gray-600 mt-1">
          Text-to-speech script preparation and voice synthesis integration
        </p>
      </div>

      {/* Notice Banner */}
      <div className="bg-amber-50 border border-amber-200 rounded-lg p-4">
        <div className="flex gap-3">
          <svg className="w-5 h-5 text-amber-600 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
          <div>
            <p className="text-amber-800 font-medium">TTS Integration Demo</p>
            <p className="text-amber-700 text-sm mt-1">
              This tab generates TTS-optimized scripts with speaking cues. In production, these would feed into 
              ElevenLabs, Amazon Polly, or Google TTS APIs to generate actual audio files.
            </p>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Panel */}
        <Card>
          <div className="p-6 space-y-5">
            <h3 className="font-semibold text-gray-900">Prepare Audio Script</h3>

            {/* Content Type */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Content Type
              </label>
              <div className="flex flex-wrap gap-2">
                {CONTENT_TYPES.map((type) => (
                  <button
                    key={type.id}
                    onClick={() => setContentType(type.id)}
                    className={`px-4 py-2 rounded-full text-sm font-medium transition-all ${
                      contentType === type.id
                        ? 'bg-yellow-500 text-white'
                        : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
                    }`}
                  >
                    {type.label}
                  </button>
                ))}
              </div>
            </div>

            {/* Voice Selection */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Voice Style
              </label>
              <div className="grid grid-cols-2 gap-2">
                {VOICES.map((v) => (
                  <button
                    key={v.id}
                    onClick={() => setVoice(v.id)}
                    className={`p-3 rounded-lg border text-left transition-all ${
                      voice === v.id
                        ? 'border-yellow-500 bg-yellow-50 text-yellow-700'
                        : 'border-gray-200 hover:border-gray-300'
                    }`}
                  >
                    <div className="font-medium text-sm">{v.label}</div>
                    <div className="text-xs text-gray-500 mt-0.5">{v.description}</div>
                  </button>
                ))}
              </div>
            </div>

            {/* Text Input */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Text to Convert
              </label>
              <textarea
                value={text}
                onChange={(e) => setText(e.target.value)}
                placeholder="Enter the text you want to convert to speech-ready format..."
                rows={6}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-yellow-500 focus:border-yellow-500"
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
              disabled={loading || !text.trim()}
              className="w-full bg-yellow-500 hover:bg-yellow-600 text-gray-900"
            >
              {loading ? (
                <span className="flex items-center justify-center gap-2">
                  <LoadingSpinner size="sm" />
                  Processing...
                </span>
              ) : (
                'Generate TTS Script'
              )}
            </Button>
          </div>
        </Card>

        {/* Output Panel */}
        <Card>
          <div className="p-6">
            <h3 className="font-semibold text-gray-900 mb-4">TTS-Ready Script</h3>
            
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <LoadingSpinner />
              </div>
            ) : result ? (
              <div className="space-y-4">
                {/* Audio Player Placeholder */}
                <div className="bg-gray-100 rounded-lg p-4 flex items-center gap-4">
                  <button className="w-12 h-12 bg-yellow-500 rounded-full flex items-center justify-center text-white hover:bg-yellow-600 transition-colors">
                    <svg className="w-6 h-6 ml-1" fill="currentColor" viewBox="0 0 24 24">
                      <path d="M8 5v14l11-7z" />
                    </svg>
                  </button>
                  <div className="flex-1">
                    <div className="h-2 bg-gray-300 rounded-full">
                      <div className="h-2 bg-yellow-500 rounded-full w-0"></div>
                    </div>
                    <div className="flex justify-between text-xs text-gray-500 mt-1">
                      <span>0:00</span>
                      <span>{result.estimatedDuration}</span>
                    </div>
                  </div>
                </div>

                {/* Metadata */}
                <div className="flex flex-wrap gap-2 text-xs">
                  <span className="px-2 py-1 bg-yellow-100 text-yellow-700 rounded-full">
                    {result.voice}
                  </span>
                  <span className="px-2 py-1 bg-blue-100 text-blue-700 rounded-full">
                    {result.type}
                  </span>
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {result.wordCount} words
                  </span>
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {result.estimatedDuration}
                  </span>
                </div>

                {/* Script */}
                <div>
                  <label className="text-sm font-medium text-gray-700 mb-2 block">
                    TTS-Optimized Script
                  </label>
                  <div className="bg-gray-50 rounded-lg p-4 max-h-64 overflow-y-auto">
                    <pre className="whitespace-pre-wrap text-gray-800 text-sm font-sans leading-relaxed">
                      {result.script}
                    </pre>
                  </div>
                </div>

                {/* JSON Output */}
                <details>
                  <summary className="cursor-pointer text-sm text-gray-600 hover:text-gray-900">
                    View JSON Metadata
                  </summary>
                  <pre className="mt-2 p-3 bg-gray-900 text-gray-100 rounded-lg text-xs overflow-x-auto">
                    {JSON.stringify({
                      insight: 'Audio content prepared for synthesis',
                      implication: 'Ready for TTS API integration',
                      recommendation: `Use ${result.voice} voice for ${result.type}`,
                      priority: 'Medium',
                      domain: 'Audio',
                      word_count: result.wordCount,
                      estimated_duration: result.estimatedDuration,
                      compatible_apis: ['ElevenLabs', 'Amazon Polly', 'Google TTS', 'Azure TTS'],
                    }, null, 2)}
                  </pre>
                </details>
              </div>
            ) : (
              <div className="text-center py-12 text-gray-500">
                <svg className="w-12 h-12 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M19 11a7 7 0 01-7 7m0 0a7 7 0 01-7-7m7 7v4m0 0H8m4 0h4m-4-8a3 3 0 01-3-3V5a3 3 0 116 0v6a3 3 0 01-3 3z" />
                </svg>
                <p>Enter text to prepare for audio synthesis</p>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}
