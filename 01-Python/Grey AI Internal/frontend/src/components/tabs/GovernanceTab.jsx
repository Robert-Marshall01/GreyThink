/**
 * Governance & Security Tab Component
 * ====================================
 * Bias detection, adversarial testing, and compliance recommendations.
 * Demonstrates AI governance and security best practices.
 */

import { useState, useCallback } from 'react'
import { Card, Button, LoadingSpinner } from '../ui'
import { rawInference } from '../../services/api'

const AUDIT_TYPES = [
  { id: 'bias', label: 'Bias Detection', description: 'Check for fairness issues in data or outputs' },
  { id: 'security', label: 'Security Audit', description: 'Identify vulnerabilities and risks' },
  { id: 'compliance', label: 'Compliance Check', description: 'GDPR, SOC2, HIPAA readiness' },
  { id: 'adversarial', label: 'Adversarial Testing', description: 'Prompt injection and jailbreak tests' },
]

const COMPLIANCE_FRAMEWORKS = [
  { id: 'gdpr', label: 'GDPR', region: 'EU' },
  { id: 'soc2', label: 'SOC 2', region: 'Global' },
  { id: 'hipaa', label: 'HIPAA', region: 'US Healthcare' },
  { id: 'iso27001', label: 'ISO 27001', region: 'Global' },
]

export default function GovernanceTab() {
  const [auditType, setAuditType] = useState('bias')
  const [inputText, setInputText] = useState('')
  const [frameworks, setFrameworks] = useState(['gdpr'])
  const [result, setResult] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)

  const toggleFramework = (id) => {
    setFrameworks(prev => 
      prev.includes(id) 
        ? prev.filter(f => f !== id)
        : [...prev, id]
    )
  }

  const handleAudit = useCallback(async () => {
    if (!inputText.trim()) {
      setError('Please enter content to audit')
      return
    }

    setLoading(true)
    setError(null)

    try {
      const auditInfo = AUDIT_TYPES.find(a => a.id === auditType)
      const frameworkLabels = frameworks.map(f => 
        COMPLIANCE_FRAMEWORKS.find(cf => cf.id === f)?.label
      ).filter(Boolean)
      
      let systemPrompt = 'You are an AI governance and security expert. Provide actionable recommendations with severity levels.'
      let prompt = ''
      
      if (auditType === 'bias') {
        prompt = `Analyze this content for potential bias issues (gender, age, racial, socioeconomic). Identify specific concerns and provide mitigation recommendations:

${inputText}

Format as: 
- Finding (with severity: Low/Medium/High)
- Why it's a concern
- Recommendation to fix`
      } else if (auditType === 'security') {
        prompt = `Perform a security assessment of this system/content. Identify vulnerabilities, data exposure risks, and attack vectors:

${inputText}

Provide:
- Security risks identified (with severity)
- Potential attack vectors
- Mitigation recommendations`
      } else if (auditType === 'compliance') {
        prompt = `Assess compliance readiness for: ${frameworkLabels.join(', ')}

Content/System to evaluate:
${inputText}

For each framework, provide:
- Compliance gaps identified
- Required actions
- Priority level`
      } else if (auditType === 'adversarial') {
        prompt = `Evaluate this AI system/prompt for adversarial vulnerabilities:

${inputText}

Test for:
- Prompt injection susceptibility
- Jailbreak vulnerabilities
- Data leakage risks
- Hallucination concerns

Provide specific test cases and mitigation strategies.`
      }

      const response = await rawInference(prompt, systemPrompt)
      
      setResult({
        analysis: response.response,
        auditType: auditInfo?.label,
        frameworks: frameworkLabels,
        model: response.model_used || 'AI',
        timestamp: new Date().toISOString(),
      })
    } catch (err) {
      setError(err.message || 'Audit failed')
    } finally {
      setLoading(false)
    }
  }, [inputText, auditType, frameworks])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-bold text-gray-900">Governance & Security</h2>
        <p className="text-gray-600 mt-1">
          Bias detection, security audits, compliance checks, and adversarial testing
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Panel */}
        <Card>
          <div className="p-6 space-y-5">
            <h3 className="font-semibold text-gray-900">Configure Audit</h3>

            {/* Audit Type */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Audit Type
              </label>
              <div className="grid grid-cols-2 gap-2">
                {AUDIT_TYPES.map((type) => (
                  <button
                    key={type.id}
                    onClick={() => setAuditType(type.id)}
                    className={`p-3 rounded-lg border text-left transition-all ${
                      auditType === type.id
                        ? 'border-red-500 bg-red-50 text-red-700'
                        : 'border-gray-200 hover:border-gray-300'
                    }`}
                  >
                    <div className="font-medium text-sm">{type.label}</div>
                    <div className="text-xs text-gray-500 mt-0.5">{type.description}</div>
                  </button>
                ))}
              </div>
            </div>

            {/* Compliance Frameworks (show for compliance audit) */}
            {auditType === 'compliance' && (
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Compliance Frameworks
                </label>
                <div className="flex flex-wrap gap-2">
                  {COMPLIANCE_FRAMEWORKS.map((f) => (
                    <button
                      key={f.id}
                      onClick={() => toggleFramework(f.id)}
                      className={`px-3 py-2 rounded-lg border text-sm transition-all ${
                        frameworks.includes(f.id)
                          ? 'border-red-500 bg-red-50 text-red-700'
                          : 'border-gray-200 hover:border-gray-300'
                      }`}
                    >
                      <span className="font-medium">{f.label}</span>
                      <span className="text-xs text-gray-500 ml-1">({f.region})</span>
                    </button>
                  ))}
                </div>
              </div>
            )}

            {/* Content Input */}
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Content to Audit
              </label>
              <textarea
                value={inputText}
                onChange={(e) => setInputText(e.target.value)}
                placeholder={auditType === 'adversarial' 
                  ? "Paste an AI prompt or system description to test for vulnerabilities..."
                  : "Paste content, data descriptions, or system specs to audit..."
                }
                rows={6}
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-red-500 focus:border-red-500"
              />
            </div>

            {/* Error */}
            {error && (
              <div className="p-3 bg-red-50 border border-red-200 rounded-lg text-red-700 text-sm">
                {error}
              </div>
            )}

            {/* Audit Button */}
            <Button
              onClick={handleAudit}
              disabled={loading || !inputText.trim()}
              className="w-full bg-red-600 hover:bg-red-700"
            >
              {loading ? (
                <span className="flex items-center justify-center gap-2">
                  <LoadingSpinner size="sm" />
                  Running Audit...
                </span>
              ) : (
                `Run ${AUDIT_TYPES.find(a => a.id === auditType)?.label || 'Audit'}`
              )}
            </Button>
          </div>
        </Card>

        {/* Output Panel */}
        <Card>
          <div className="p-6">
            <h3 className="font-semibold text-gray-900 mb-4">Audit Results</h3>
            
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <LoadingSpinner />
              </div>
            ) : result ? (
              <div className="space-y-4">
                {/* Metadata */}
                <div className="flex flex-wrap gap-2 text-xs">
                  <span className="px-2 py-1 bg-red-100 text-red-700 rounded-full">
                    {result.auditType}
                  </span>
                  {result.frameworks?.map(f => (
                    <span key={f} className="px-2 py-1 bg-blue-100 text-blue-700 rounded-full">
                      {f}
                    </span>
                  ))}
                  <span className="px-2 py-1 bg-gray-100 text-gray-600 rounded-full">
                    {result.model}
                  </span>
                </div>

                {/* Results */}
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
                      insight: `${result.auditType} completed`,
                      implication: 'Review findings for compliance and security posture',
                      recommendation: 'Address high-severity items immediately',
                      priority: 'High',
                      domain: 'Security',
                      audit_type: result.auditType,
                      frameworks_checked: result.frameworks,
                      timestamp: result.timestamp,
                    }, null, 2)}
                  </pre>
                </details>
              </div>
            ) : (
              <div className="text-center py-12 text-gray-500">
                <svg className="w-12 h-12 mx-auto text-gray-300 mb-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={1.5} d="M9 12l2 2 4-4m5.618-4.016A11.955 11.955 0 0112 2.944a11.955 11.955 0 01-8.618 3.04A12.02 12.02 0 003 9c0 5.591 3.824 10.29 9 11.622 5.176-1.332 9-6.03 9-11.622 0-1.042-.133-2.052-.382-3.016z" />
                </svg>
                <p>Configure an audit to see results</p>
              </div>
            )}
          </div>
        </Card>
      </div>
    </div>
  )
}
