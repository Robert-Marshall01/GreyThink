/**
 * Upload Component
 * ================
 * Handles file upload with drag-and-drop support.
 * Validates file type and size before sending to backend.
 */

import { useState, useRef, useCallback } from 'react'
import PropTypes from 'prop-types'
import { uploadFile } from '../../services/api'
import LoadingSpinner from '../ui/LoadingSpinner'
import Card from '../ui/Card'

// Configuration
const ALLOWED_TYPES = ['text/csv', 'text/plain']
const ALLOWED_EXTENSIONS = ['.csv', '.txt']
const MAX_SIZE_MB = 10

function UploadComponent({ onUploadComplete, currentUpload }) {
  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------
  
  const [isDragging, setIsDragging] = useState(false)
  const [isUploading, setIsUploading] = useState(false)
  const [uploadError, setUploadError] = useState(null)
  const [uploadProgress, setUploadProgress] = useState(0)
  
  const fileInputRef = useRef(null)

  // ---------------------------------------------------------------------------
  // Validation
  // ---------------------------------------------------------------------------

  /**
   * Validate file type and size
   * @returns {string|null} Error message or null if valid
   */
  const validateFile = (file) => {
    // Check extension
    const ext = '.' + file.name.split('.').pop().toLowerCase()
    if (!ALLOWED_EXTENSIONS.includes(ext)) {
      return `Invalid file type. Allowed: ${ALLOWED_EXTENSIONS.join(', ')}`
    }
    
    // Check size
    const sizeMB = file.size / (1024 * 1024)
    if (sizeMB > MAX_SIZE_MB) {
      return `File too large (${sizeMB.toFixed(1)}MB). Maximum: ${MAX_SIZE_MB}MB`
    }
    
    return null
  }

  // ---------------------------------------------------------------------------
  // Upload Handler
  // ---------------------------------------------------------------------------

  /**
   * Process and upload the selected file
   */
  const handleFile = useCallback(async (file) => {
    // Validate
    const validationError = validateFile(file)
    if (validationError) {
      setUploadError(validationError)
      return
    }
    
    setUploadError(null)
    setIsUploading(true)
    setUploadProgress(0)
    
    try {
      // Simulate progress for better UX
      const progressInterval = setInterval(() => {
        setUploadProgress(prev => Math.min(prev + 10, 90))
      }, 100)
      
      // Upload to backend
      const result = await uploadFile(file)
      
      clearInterval(progressInterval)
      setUploadProgress(100)
      
      // Notify parent component
      onUploadComplete(result)
      
    } catch (err) {
      setUploadError(err.message || 'Upload failed. Please try again.')
    } finally {
      setIsUploading(false)
      setUploadProgress(0)
      // Reset file input
      if (fileInputRef.current) {
        fileInputRef.current.value = ''
      }
    }
  }, [onUploadComplete])

  // ---------------------------------------------------------------------------
  // Drag & Drop Handlers
  // ---------------------------------------------------------------------------

  const handleDragOver = useCallback((e) => {
    e.preventDefault()
    e.stopPropagation()
    setIsDragging(true)
  }, [])

  const handleDragLeave = useCallback((e) => {
    e.preventDefault()
    e.stopPropagation()
    setIsDragging(false)
  }, [])

  const handleDrop = useCallback((e) => {
    e.preventDefault()
    e.stopPropagation()
    setIsDragging(false)
    
    const files = e.dataTransfer.files
    if (files.length > 0) {
      handleFile(files[0])
    }
  }, [handleFile])

  const handleFileSelect = useCallback((e) => {
    const files = e.target.files
    if (files.length > 0) {
      handleFile(files[0])
    }
  }, [handleFile])

  const handleClick = useCallback(() => {
    fileInputRef.current?.click()
  }, [])

  // ---------------------------------------------------------------------------
  // Render
  // ---------------------------------------------------------------------------

  return (
    <Card>
      <h2 className="text-lg font-semibold text-gray-900 mb-4">
        Upload Data
      </h2>
      
      {/* Dropzone */}
      <div
        onDragOver={handleDragOver}
        onDragLeave={handleDragLeave}
        onDrop={handleDrop}
        onClick={handleClick}
        className={`
          relative border-2 border-dashed rounded-xl p-8
          transition-all duration-200 cursor-pointer
          ${isDragging 
            ? 'border-blue-500 bg-blue-50' 
            : 'border-gray-200 hover:border-gray-300 hover:bg-gray-50'
          }
          ${isUploading ? 'pointer-events-none opacity-70' : ''}
        `}
      >
        {/* Hidden File Input */}
        <input
          ref={fileInputRef}
          type="file"
          accept=".csv,.txt"
          onChange={handleFileSelect}
          className="hidden"
          disabled={isUploading}
        />
        
        {/* Content */}
        <div className="text-center">
          {isUploading ? (
            <>
              <LoadingSpinner size="lg" className="mx-auto mb-3" />
              <p className="text-sm font-medium text-gray-700">
                Uploading... {uploadProgress}%
              </p>
              {/* Progress Bar */}
              <div className="mt-3 w-full bg-gray-200 rounded-full h-2 overflow-hidden">
                <div 
                  className="bg-blue-600 h-full transition-all duration-300"
                  style={{ width: `${uploadProgress}%` }}
                />
              </div>
            </>
          ) : (
            <>
              {/* Upload Icon */}
              <div className="w-12 h-12 bg-gray-100 rounded-full flex items-center justify-center mx-auto mb-4">
                <svg 
                  className={`w-6 h-6 ${isDragging ? 'text-blue-500' : 'text-gray-400'}`}
                  fill="none" 
                  stroke="currentColor" 
                  viewBox="0 0 24 24"
                >
                  <path 
                    strokeLinecap="round" 
                    strokeLinejoin="round" 
                    strokeWidth={1.5} 
                    d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12" 
                  />
                </svg>
              </div>
              
              {/* Instructions */}
              <p className="text-sm text-gray-600 mb-1">
                <span className="text-blue-600 font-medium">Click to upload</span>
                {' '}or drag and drop
              </p>
              <p className="text-xs text-gray-400">
                CSV or TXT files up to {MAX_SIZE_MB}MB
              </p>
            </>
          )}
        </div>
      </div>

      {/* Error Message */}
      {uploadError && (
        <div className="mt-4 p-3 bg-red-50 border border-red-100 rounded-lg">
          <p className="text-sm text-red-600 flex items-center">
            <svg className="w-4 h-4 mr-2 flex-shrink-0" fill="currentColor" viewBox="0 0 20 20">
              <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
            </svg>
            {uploadError}
          </p>
        </div>
      )}

      {/* Current Upload Info */}
      {currentUpload && !isUploading && (
        <div className="mt-4 p-4 bg-green-50 border border-green-100 rounded-lg">
          <div className="flex items-start">
            <svg className="w-5 h-5 text-green-500 mr-3 mt-0.5" fill="currentColor" viewBox="0 0 20 20">
              <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd" />
            </svg>
            <div className="flex-1 min-w-0">
              <p className="text-sm font-medium text-green-800 truncate">
                {currentUpload.filename}
              </p>
              <p className="text-xs text-green-600 mt-1">
                {currentUpload.row_count?.toLocaleString()} rows 
                {currentUpload.column_count && ` • ${currentUpload.column_count} columns`}
              </p>
            </div>
          </div>
        </div>
      )}
    </Card>
  )
}

UploadComponent.propTypes = {
  onUploadComplete: PropTypes.func.isRequired,
  currentUpload: PropTypes.shape({
    id: PropTypes.oneOfType([PropTypes.string, PropTypes.number]),
    filename: PropTypes.string,
    row_count: PropTypes.number,
    column_count: PropTypes.number,
    file_size: PropTypes.number,
    status: PropTypes.string,
  }),
}

export default UploadComponent
