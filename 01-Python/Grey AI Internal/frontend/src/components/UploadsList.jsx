export default function UploadsList({ uploads, selectedId, onSelect }) {
  if (uploads.length === 0) {
    return (
      <div className="bg-white rounded-lg shadow p-4">
        <h2 className="text-sm font-semibold text-gray-700 mb-3">Recent Uploads</h2>
        <p className="text-sm text-gray-500 text-center py-4">No files uploaded yet</p>
      </div>
    )
  }

  const formatDate = (dateString) => {
    if (!dateString) return ''
    const date = new Date(dateString)
    return date.toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    })
  }

  const getStatusBadge = (status) => {
    const styles = {
      uploaded: 'bg-gray-100 text-gray-700',
      parsed: 'bg-blue-100 text-blue-700',
      analyzed: 'bg-green-100 text-green-700',
      failed: 'bg-red-100 text-red-700',
    }
    return styles[status] || styles.uploaded
  }

  return (
    <div className="bg-white rounded-lg shadow">
      <div className="px-4 py-3 border-b border-gray-100">
        <h2 className="text-sm font-semibold text-gray-700">Recent Uploads</h2>
      </div>
      
      <div className="divide-y divide-gray-100 max-h-96 overflow-y-auto">
        {uploads.map((upload) => (
          <button
            key={upload.id}
            onClick={() => onSelect(upload)}
            className={`
              w-full px-4 py-3 text-left hover:bg-gray-50 transition-colors
              ${selectedId === upload.id ? 'bg-primary-50 border-l-2 border-primary-500' : ''}
            `}
          >
            <div className="flex items-start justify-between">
              <div className="flex-1 min-w-0">
                <p className="text-sm font-medium text-gray-900 truncate">
                  {upload.filename}
                </p>
                <div className="flex items-center gap-2 mt-1">
                  <span className={`text-xs px-2 py-0.5 rounded-full ${getStatusBadge(upload.status)}`}>
                    {upload.status}
                  </span>
                  {upload.row_count && (
                    <span className="text-xs text-gray-500">
                      {upload.row_count.toLocaleString()} rows
                    </span>
                  )}
                </div>
              </div>
              <div className="ml-2 flex-shrink-0">
                <p className="text-xs text-gray-400">
                  {formatDate(upload.created_at)}
                </p>
              </div>
            </div>
          </button>
        ))}
      </div>
    </div>
  )
}
