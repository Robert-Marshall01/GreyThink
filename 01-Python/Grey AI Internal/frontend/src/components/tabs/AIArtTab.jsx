/**
 * AI Art Tab Component
 * ====================
 * Open-source image generation using Stable Diffusion XL.
 * Supports text-to-image and image-to-image transformations.
 */

import { useState, useEffect, useCallback, useRef } from 'react'
import { Card, Button } from '../ui'
import { generateImage, transformImage, checkSDStatus } from '../../services/api'

// Style presets matching backend ImageStyle enum
const STYLES = [
  { id: 'photorealistic', label: 'Photorealistic', description: '8K, detailed, sharp focus' },
  { id: 'artistic', label: 'Artistic', description: 'Creative, expressive brushstrokes' },
  { id: 'abstract', label: 'Abstract', description: 'Geometric, bold colors' },
  { id: 'anime', label: 'Anime', description: 'Vibrant colors, manga style' },
  { id: 'digital_art', label: 'Digital Art', description: 'Concept art, trending' },
  { id: 'oil_painting', label: 'Oil Painting', description: 'Classical, visible brushstrokes' },
  { id: 'watercolor', label: 'Watercolor', description: 'Soft edges, flowing' },
  { id: 'pencil_sketch', label: 'Pencil Sketch', description: 'Black & white linework' },
]

// Quality presets
const QUALITY_PRESETS = [
  { id: 'fast', label: 'Fast', description: '~10 steps, quick preview', steps: 10 },
  { id: 'balanced', label: 'Balanced', description: '~25 steps, recommended', steps: 25 },
  { id: 'quality', label: 'Quality', description: '~50 steps, highest detail', steps: 50 },
]

// Demo prompts
const DEMO_PROMPTS = [
  { label: 'Mountain Sunset', prompt: 'A serene mountain landscape at golden hour with snow-capped peaks reflecting in a crystal clear lake' },
  { label: 'Cyberpunk City', prompt: 'Neon-lit cyberpunk cityscape at night with flying cars and holographic advertisements' },
  { label: 'Fantasy Portal', prompt: 'A magical glowing portal in an ancient forest with mystical creatures and floating particles' },
  { label: 'Space Station', prompt: 'Futuristic space station orbiting Earth with detailed mechanical structures and solar panels' },
]

// Image dimensions
const DIMENSIONS = [
  { id: '1024x1024', label: 'Square (1:1)', width: 1024, height: 1024 },
  { id: '1152x896', label: 'Landscape (4:3)', width: 1152, height: 896 },
  { id: '896x1152', label: 'Portrait (3:4)', width: 896, height: 1152 },
  { id: '1344x768', label: 'Wide (16:9)', width: 1344, height: 768 },
  { id: '768x1344', label: 'Tall (9:16)', width: 768, height: 1344 },
]

export default function AIArtTab() {
  // Generation mode: 'text2img' or 'img2img'
  const [mode, setMode] = useState('text2img')
  
  // Form state
  const [prompt, setPrompt] = useState('')
  const [negativePrompt, setNegativePrompt] = useState('')
  const [style, setStyle] = useState('photorealistic')
  const [quality, setQuality] = useState('balanced')
  const [dimensions, setDimensions] = useState('1024x1024')
  const [denoisingStrength, setDenoisingStrength] = useState(0.75)
  
  // Image state
  const [sourceImage, setSourceImage] = useState(null)
  const [sourceImagePreview, setSourceImagePreview] = useState(null)
  const [generatedImage, setGeneratedImage] = useState(null)
  const [metadata, setMetadata] = useState(null)
  
  // UI state
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)
  const [sdStatus, setSdStatus] = useState({ available: false, checking: true })
  
  const fileInputRef = useRef(null)

  // Check SD status on mount
  useEffect(() => {
    const checkStatus = async () => {
      try {
        const status = await checkSDStatus()
        setSdStatus({ ...status, checking: false })
      } catch (err) {
        setSdStatus({ available: false, checking: false, error: err.message })
      }
    }
    checkStatus()
  }, [])

  // Handle image upload for img2img
  const handleImageUpload = useCallback((e) => {
    const file = e.target.files?.[0]
    if (!file) return
    
    // Validate file type
    if (!file.type.startsWith('image/')) {
      setError('Please upload an image file (PNG, JPG, etc.)')
      return
    }
    
    // Create preview
    const reader = new FileReader()
    reader.onload = (event) => {
      setSourceImagePreview(event.target.result)
      // Remove data:image/...;base64, prefix for API
      const base64 = event.target.result.split(',')[1]
      setSourceImage(base64)
      setError(null)
    }
    reader.readAsDataURL(file)
  }, [])

  // Clear source image
  const clearSourceImage = useCallback(() => {
    setSourceImage(null)
    setSourceImagePreview(null)
    if (fileInputRef.current) {
      fileInputRef.current.value = ''
    }
  }, [])

  // Generate image
  const handleGenerate = useCallback(async () => {
    if (!prompt.trim()) {
      setError('Please enter a prompt')
      return
    }
    
    if (mode === 'img2img' && !sourceImage) {
      setError('Please upload a source image for transformation')
      return
    }
    
    setLoading(true)
    setError(null)
    
    const dim = DIMENSIONS.find(d => d.id === dimensions) || DIMENSIONS[0]
    
    try {
      let result
      
      if (mode === 'text2img') {
        result = await generateImage({
          prompt,
          negative_prompt: negativePrompt || null,
          style,
          quality,
          width: dim.width,
          height: dim.height,
        })
      } else {
        result = await transformImage({
          image_base64: sourceImage,
          prompt,
          negative_prompt: negativePrompt || null,
          style,
          quality,
          denoising_strength: denoisingStrength,
          width: dim.width,
          height: dim.height,
        })
      }
      
      setGeneratedImage(`data:image/png;base64,${result.image_base64}`)
      setMetadata(result.metadata)
      
    } catch (err) {
      const errorMessage = err.data?.detail?.message || err.message || 'Generation failed'
      setError(errorMessage)
    } finally {
      setLoading(false)
    }
  }, [prompt, negativePrompt, style, quality, dimensions, mode, sourceImage, denoisingStrength])

  // Download generated image
  const handleDownload = useCallback(() => {
    if (!generatedImage || !metadata) return
    
    const link = document.createElement('a')
    link.href = generatedImage
    link.download = `generated_${metadata.id}.png`
    document.body.appendChild(link)
    link.click()
    document.body.removeChild(link)
  }, [generatedImage, metadata])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold text-gray-900">AI Art Generation</h2>
        <p className="text-gray-600 mt-1">
          Open-source image generation using Stable Diffusion XL
        </p>
      </div>

      {/* SD Status Banner */}
      {sdStatus.checking ? (
        <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 border-2 border-gray-400 border-t-transparent rounded-full animate-spin" />
            <span className="text-gray-600">Checking Stable Diffusion service...</span>
          </div>
        </div>
      ) : sdStatus.available ? (
        <div className="bg-green-50 border border-green-200 rounded-lg p-4">
          <div className="flex items-center gap-2">
            <div className="w-3 h-3 bg-green-500 rounded-full animate-pulse" />
            <span className="text-green-800 font-medium">Stable Diffusion Online</span>
            <span className="text-green-600 text-sm">— Model: {sdStatus.current_model || 'SDXL'}</span>
          </div>
        </div>
      ) : (
        <div className="bg-amber-50 border border-amber-200 rounded-lg p-4">
          <div className="flex gap-3">
            <svg className="w-5 h-5 text-amber-600 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
            </svg>
            <div>
              <p className="text-amber-800 font-medium">Stable Diffusion Not Running</p>
              <p className="text-amber-700 text-sm mt-1">
                Start the SD container: <code className="bg-amber-100 px-1 rounded">docker-compose up -d stable-diffusion</code>
              </p>
              {sdStatus.error && (
                <p className="text-amber-600 text-xs mt-1">{sdStatus.error}</p>
              )}
            </div>
          </div>
        </div>
      )}

      {/* Error Banner */}
      {error && (
        <div className="bg-red-50 border-l-4 border-red-500 p-4 rounded-r-lg">
          <div className="flex items-center justify-between">
            <p className="text-red-700">{error}</p>
            <button onClick={() => setError(null)} className="text-red-500 hover:text-red-700">
              <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
                <path fillRule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clipRule="evenodd" />
              </svg>
            </button>
          </div>
        </div>
      )}

      {/* Mode Toggle */}
      <div className="flex gap-2">
        <button
          onClick={() => setMode('text2img')}
          className={`px-4 py-2 rounded-lg font-medium transition-colors ${
            mode === 'text2img'
              ? 'bg-pink-600 text-white'
              : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
          }`}
        >
          Text to Image
        </button>
        <button
          onClick={() => setMode('img2img')}
          className={`px-4 py-2 rounded-lg font-medium transition-colors ${
            mode === 'img2img'
              ? 'bg-pink-600 text-white'
              : 'bg-gray-100 text-gray-700 hover:bg-gray-200'
          }`}
        >
          Image to Image
        </button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Panel */}
        <Card>
          <div className="p-6 space-y-5">
            <h3 className="font-semibold text-gray-900">
              {mode === 'text2img' ? 'Generate New Image' : 'Transform Image'}
            </h3>

            {/* Source Image Upload (img2img mode) */}
            {mode === 'img2img' && (
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Source Image
                </label>
                {sourceImagePreview ? (
                  <div className="relative">
                    <img
                      src={sourceImagePreview}
                      alt="Source"
                      className="w-full h-48 object-cover rounded-lg"
                    />
                    <button
                      onClick={clearSourceImage}
                      className="absolute top-2 right-2 p-1 bg-red-500 text-white rounded-full hover:bg-red-600"
                    >
                      <svg className="w-4 h-4" fill="currentColor" viewBox="0 0 20 20">
                        <path fillRule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clipRule="evenodd" />
                      </svg>
                    </button>
                  </div>
                ) : (
                  <label className="flex flex-col items-center justify-center h-32 border-2 border-dashed border-gray-300 rounded-lg cursor-pointer hover:border-pink-400 transition-colors">
                    <svg className="w-8 h-8 text-gray-400 mb-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z" />
                    </svg>
                    <span className="text-sm text-gray-500">Click to upload image</span>
                    <input
                      ref={fileInputRef}
                      type="file"
                      accept="image/*"
                      onChange={handleImageUpload}
                      className="hidden"
                    />
                  </label>
                )}
              </div>
            )}

            {/* Quick Prompts */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Quick Start
              </label>
              <div className="flex flex-wrap gap-2">
                {DEMO_PROMPTS.map((demo) => (
                  <button
                    key={demo.label}
                    onClick={() => setPrompt(demo.prompt)}
                    className="px-3 py-1.5 bg-pink-50 text-pink-700 rounded-full text-sm hover:bg-pink-100 transition-colors"
                  >
                    {demo.label}
                  </button>
                ))}
              </div>
            </div>

            {/* Prompt Input */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                {mode === 'text2img' ? 'Image Description' : 'Transformation Prompt'}
              </label>
              <textarea
                value={prompt}
                onChange={(e) => setPrompt(e.target.value)}
                placeholder={mode === 'text2img' 
                  ? "Describe the image you want to generate..."
                  : "Describe how to transform the image..."
                }
                rows={3}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-pink-500 focus:border-pink-500"
              />
            </div>

            {/* Negative Prompt */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Negative Prompt <span className="text-gray-400 font-normal">(optional)</span>
              </label>
              <input
                type="text"
                value={negativePrompt}
                onChange={(e) => setNegativePrompt(e.target.value)}
                placeholder="Things to avoid (e.g., blurry, distorted)"
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-pink-500 focus:border-pink-500"
              />
            </div>

            {/* Style Selection */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">Style</label>
              <select
                value={style}
                onChange={(e) => setStyle(e.target.value)}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-pink-500 focus:border-pink-500"
              >
                {STYLES.map((s) => (
                  <option key={s.id} value={s.id}>
                    {s.label} — {s.description}
                  </option>
                ))}
              </select>
            </div>

            {/* Quality & Dimensions Row */}
            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">Quality</label>
                <select
                  value={quality}
                  onChange={(e) => setQuality(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-pink-500 focus:border-pink-500"
                >
                  {QUALITY_PRESETS.map((q) => (
                    <option key={q.id} value={q.id}>
                      {q.label} ({q.steps} steps)
                    </option>
                  ))}
                </select>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">Dimensions</label>
                <select
                  value={dimensions}
                  onChange={(e) => setDimensions(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-pink-500 focus:border-pink-500"
                >
                  {DIMENSIONS.map((d) => (
                    <option key={d.id} value={d.id}>
                      {d.label}
                    </option>
                  ))}
                </select>
              </div>
            </div>

            {/* Denoising Strength (img2img only) */}
            {mode === 'img2img' && (
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Transformation Strength: {Math.round(denoisingStrength * 100)}%
                </label>
                <input
                  type="range"
                  min="0.1"
                  max="1"
                  step="0.05"
                  value={denoisingStrength}
                  onChange={(e) => setDenoisingStrength(parseFloat(e.target.value))}
                  className="w-full"
                />
                <div className="flex justify-between text-xs text-gray-500 mt-1">
                  <span>Subtle (keep original)</span>
                  <span>Major (reimagine)</span>
                </div>
              </div>
            )}

            {/* Generate Button */}
            <Button
              onClick={handleGenerate}
              disabled={!prompt.trim() || loading || !sdStatus.available}
              className="w-full bg-pink-600 hover:bg-pink-700 disabled:opacity-50"
            >
              {loading ? (
                <span className="flex items-center justify-center gap-2">
                  <svg className="w-5 h-5 animate-spin" fill="none" viewBox="0 0 24 24">
                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                  </svg>
                  Generating...
                </span>
              ) : (
                `Generate ${mode === 'text2img' ? 'Image' : 'Transformation'}`
              )}
            </Button>
          </div>
        </Card>

        {/* Output Panel */}
        <Card>
          <div className="p-6">
            <div className="flex items-center justify-between mb-4">
              <h3 className="font-semibold text-gray-900">Generated Image</h3>
              {generatedImage && (
                <button
                  onClick={handleDownload}
                  className="flex items-center gap-1 px-3 py-1.5 text-sm bg-pink-100 text-pink-700 rounded-lg hover:bg-pink-200 transition-colors"
                >
                  <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
                  </svg>
                  Download
                </button>
              )}
            </div>
            
            {loading ? (
              <div className="aspect-square bg-gradient-to-br from-pink-100 to-purple-100 rounded-lg flex items-center justify-center">
                <div className="text-center">
                  <svg className="w-16 h-16 mx-auto text-pink-400 animate-pulse mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z" />
                  </svg>
                  <p className="text-pink-600 font-medium">Generating image...</p>
                  <p className="text-pink-500 text-sm mt-1">This may take 30-120 seconds</p>
                </div>
              </div>
            ) : generatedImage ? (
              <div className="space-y-4">
                {/* Generated Image */}
                <img
                  src={generatedImage}
                  alt="Generated"
                  className="w-full rounded-lg shadow-lg"
                />

                {/* Metadata Panel */}
                {metadata && (
                  <div className="bg-gray-50 rounded-lg p-4 space-y-3">
                    <h4 className="font-medium text-gray-900">Generation Details</h4>
                    <div className="grid grid-cols-2 gap-2 text-sm">
                      <div>
                        <span className="text-gray-500">Model:</span>
                        <span className="ml-2 text-gray-900">{metadata.model}</span>
                      </div>
                      <div>
                        <span className="text-gray-500">Style:</span>
                        <span className="ml-2 text-gray-900">{metadata.style}</span>
                      </div>
                      <div>
                        <span className="text-gray-500">Size:</span>
                        <span className="ml-2 text-gray-900">{metadata.width}×{metadata.height}</span>
                      </div>
                      <div>
                        <span className="text-gray-500">Steps:</span>
                        <span className="ml-2 text-gray-900">{metadata.steps}</span>
                      </div>
                      <div>
                        <span className="text-gray-500">CFG:</span>
                        <span className="ml-2 text-gray-900">{metadata.cfg_scale}</span>
                      </div>
                      <div>
                        <span className="text-gray-500">Seed:</span>
                        <span className="ml-2 text-gray-900">{metadata.seed}</span>
                      </div>
                      <div className="col-span-2">
                        <span className="text-gray-500">Time:</span>
                        <span className="ml-2 text-gray-900">
                          {(metadata.generation_time_ms / 1000).toFixed(1)}s
                        </span>
                      </div>
                    </div>
                    
                    {/* Tags */}
                    <div className="flex flex-wrap gap-2 pt-2 border-t border-gray-200">
                      <span className="px-2 py-1 bg-pink-100 text-pink-700 rounded-full text-xs">
                        Domain: Art
                      </span>
                      <span className="px-2 py-1 bg-purple-100 text-purple-700 rounded-full text-xs">
                        Stable Diffusion XL
                      </span>
                      <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full text-xs">
                        Open Source
                      </span>
                    </div>
                  </div>
                )}

                {/* JSON Metadata */}
                <details>
                  <summary className="cursor-pointer text-sm text-gray-600 hover:text-gray-900">
                    View Full Metadata
                  </summary>
                  <pre className="mt-2 p-3 bg-gray-900 text-gray-100 rounded-lg text-xs overflow-x-auto">
                    {JSON.stringify(metadata, null, 2)}
                  </pre>
                </details>
              </div>
            ) : (
              <div className="aspect-square bg-gradient-to-br from-gray-50 to-gray-100 rounded-lg flex items-center justify-center">
                <div className="text-center p-6">
                  <svg className="w-16 h-16 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z" />
                  </svg>
                  <p className="text-gray-500">Enter a prompt and click Generate</p>
                  <p className="text-gray-400 text-sm mt-1">
                    Images will appear here
                  </p>
                </div>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}
