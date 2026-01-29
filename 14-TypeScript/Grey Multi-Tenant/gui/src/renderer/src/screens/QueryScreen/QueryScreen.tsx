/**
 * Query Console Screen
 */
import React, { useState } from 'react'
import { Card, Button, CodeEditor, LogPanel } from '../../components'
import { queryApi } from '../../services/api'
import { useLogsStore } from '../../stores'
import './QueryScreen.css'

const SAMPLE_QUERY = `{
  "query": "SELECT * FROM projects WHERE tenant_id = $1",
  "params": ["tenant-123"]
}`

export function QueryScreen() {
  const [query, setQuery] = useState(SAMPLE_QUERY)
  const [result, setResult] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const { addLog } = useLogsStore()

  const handleExecute = async () => {
    setIsLoading(true)
    setError(null)
    setResult('')

    try {
      const parsed = JSON.parse(query)
      await addLog('info', `Executing query: ${parsed.query || query}`)
      
      const response = await queryApi.query(parsed.query || query, parsed.params || parsed.variables)
      
      if (response.ok) {
        setResult(JSON.stringify(response.data, null, 2))
        await addLog('info', 'Query executed successfully')
      } else {
        setError(response.error || response.statusText)
        await addLog('error', `Query failed: ${response.error || response.statusText}`)
      }
    } catch (err) {
      // If not valid JSON, send as raw query string
      const response = await queryApi.query(query)
      
      if (response.ok) {
        setResult(JSON.stringify(response.data, null, 2))
        await addLog('info', 'Query executed successfully')
      } else {
        setError(response.error || String(err))
        await addLog('error', `Query failed: ${response.error || err}`)
      }
    }

    setIsLoading(false)
  }

  const handleClear = () => {
    setQuery('')
    setResult('')
    setError(null)
  }

  const handleLoadSample = () => {
    setQuery(SAMPLE_QUERY)
  }

  return (
    <div className="query-screen">
      <header className="query-header">
        <div>
          <h1>Query Console</h1>
          <p>Execute read-only queries against the adapter core</p>
        </div>
      </header>

      <div className="query-workspace">
        <Card title="Query" className="query-input-card" actions={
          <div className="query-actions">
            <Button size="sm" variant="ghost" onClick={handleLoadSample}>
              Load Sample
            </Button>
            <Button size="sm" variant="ghost" onClick={handleClear}>
              Clear
            </Button>
          </div>
        }>
          <CodeEditor
            value={query}
            onChange={setQuery}
            placeholder="Enter your query here..."
            language="json"
            height="250px"
          />
          <div className="query-execute">
            <Button
              onClick={handleExecute}
              loading={isLoading}
              disabled={!query.trim()}
            >
              Execute Query
            </Button>
          </div>
        </Card>

        <Card title="Result" className="query-result-card">
          {error ? (
            <div className="query-error">
              <strong>Error:</strong> {error}
            </div>
          ) : result ? (
            <CodeEditor
              value={result}
              onChange={() => {}}
              readOnly
              language="json"
              height="250px"
            />
          ) : (
            <div className="query-placeholder">
              Execute a query to see results here.
            </div>
          )}
        </Card>
      </div>

      <section className="query-logs">
        <h2>Query Log</h2>
        <LogPanel />
      </section>
    </div>
  )
}
