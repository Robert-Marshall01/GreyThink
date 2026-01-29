/**
 * Mutation Console Screen
 */
import React, { useState } from 'react'
import { Card, Button, CodeEditor, LogPanel } from '../../components'
import { mutationApi } from '../../services/api'
import { useLogsStore } from '../../stores'
import './MutationScreen.css'

const SAMPLE_MUTATION = `{
  "mutation": "INSERT INTO projects (name, tenant_id) VALUES ($1, $2)",
  "params": ["New Project", "tenant-123"]
}`

export function MutationScreen() {
  const [mutation, setMutation] = useState(SAMPLE_MUTATION)
  const [result, setResult] = useState('')
  const [isLoading, setIsLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [showConfirm, setShowConfirm] = useState(false)
  const { addLog } = useLogsStore()

  const handleExecute = async () => {
    setShowConfirm(false)
    setIsLoading(true)
    setError(null)
    setResult('')

    try {
      const parsed = JSON.parse(mutation)
      await addLog('info', `Executing mutation: ${parsed.mutation || mutation}`)
      
      const response = await mutationApi.mutate(parsed.mutation || mutation, parsed.params || parsed.variables)
      
      if (response.ok) {
        setResult(JSON.stringify(response.data, null, 2))
        await addLog('info', 'Mutation executed successfully')
      } else {
        setError(response.error || response.statusText)
        await addLog('error', `Mutation failed: ${response.error || response.statusText}`)
      }
    } catch (err) {
      const response = await mutationApi.mutate(mutation)
      
      if (response.ok) {
        setResult(JSON.stringify(response.data, null, 2))
        await addLog('info', 'Mutation executed successfully')
      } else {
        setError(response.error || String(err))
        await addLog('error', `Mutation failed: ${response.error || err}`)
      }
    }

    setIsLoading(false)
  }

  const handleClear = () => {
    setMutation('')
    setResult('')
    setError(null)
  }

  const handleLoadSample = () => {
    setMutation(SAMPLE_MUTATION)
  }

  return (
    <div className="mutation-screen">
      <header className="mutation-header">
        <div>
          <h1>Mutation Console</h1>
          <p>Execute write operations against the adapter core</p>
        </div>
        <div className="mutation-warning">
          ⚠️ Mutations can modify data. Use with caution.
        </div>
      </header>

      <div className="mutation-workspace">
        <Card title="Mutation" className="mutation-input-card" actions={
          <div className="mutation-actions">
            <Button size="sm" variant="ghost" onClick={handleLoadSample}>
              Load Sample
            </Button>
            <Button size="sm" variant="ghost" onClick={handleClear}>
              Clear
            </Button>
          </div>
        }>
          <CodeEditor
            value={mutation}
            onChange={setMutation}
            placeholder="Enter your mutation here..."
            language="json"
            height="250px"
          />
          <div className="mutation-execute">
            <Button
              onClick={() => setShowConfirm(true)}
              variant="danger"
              loading={isLoading}
              disabled={!mutation.trim()}
            >
              Execute Mutation
            </Button>
          </div>
        </Card>

        {/* Confirmation Dialog */}
        {showConfirm && (
          <div className="modal-overlay">
            <Card title="Confirm Mutation" className="confirm-modal">
              <p>Are you sure you want to execute this mutation? This action may modify data.</p>
              <div className="confirm-actions">
                <Button variant="secondary" onClick={() => setShowConfirm(false)}>
                  Cancel
                </Button>
                <Button variant="danger" onClick={handleExecute}>
                  Confirm & Execute
                </Button>
              </div>
            </Card>
          </div>
        )}

        <Card title="Result" className="mutation-result-card">
          {error ? (
            <div className="mutation-error">
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
            <div className="mutation-placeholder">
              Execute a mutation to see results here.
            </div>
          )}
        </Card>
      </div>

      <section className="mutation-logs">
        <h2>Mutation Log</h2>
        <LogPanel />
      </section>
    </div>
  )
}
