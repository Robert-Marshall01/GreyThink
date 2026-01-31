/**
 * Home Tab Component
 * ==================
 * Executive summary and navigation hub for Grey AI Internal.
 * Showcases recruiter-facing skills and AI domain overview.
 */

import { Card } from '../ui'

const DOMAINS = [
  {
    id: 'data',
    title: 'Data Analysis',
    description: 'Parse CSVs, generate executive summaries with insights, implications, and recommendations.',
    skills: ['Pandas', 'NumPy', 'Statistical Analysis', 'Data Cleaning'],
    color: 'blue',
  },
  {
    id: 'writing',
    title: 'AI Writing',
    description: 'Generate blog posts, documentation, and executive briefings with professional tone.',
    skills: ['Prompt Engineering', 'Content Strategy', 'Technical Writing'],
    color: 'purple',
  },
  {
    id: 'code',
    title: 'AI Code',
    description: 'Generate, refactor, and test code. Demonstrate AI-assisted development workflows.',
    skills: ['Python', 'JavaScript', 'FastAPI', 'React', 'Testing'],
    color: 'green',
  },
  {
    id: 'art',
    title: 'AI Art',
    description: 'Conceptual and illustrative image generation with multimodal integration.',
    skills: ['Stable Diffusion', 'DALL-E', 'Prompt Design', 'Visual AI'],
    color: 'pink',
  },
  {
    id: 'audio',
    title: 'AI Audio',
    description: 'Text-to-speech narration, podcast summaries, and voice synthesis.',
    skills: ['TTS APIs', 'Audio Processing', 'Transcription'],
    color: 'yellow',
  },
  {
    id: 'governance',
    title: 'Governance & Security',
    description: 'Bias detection, anomaly handling, adversarial testing, and compliance.',
    skills: ['MLOps', 'Security Audits', 'GDPR', 'Model Governance'],
    color: 'red',
  },
  {
    id: 'bi',
    title: 'Business Intelligence',
    description: 'Dashboards, analytics, and industry-specific insights tied to business outcomes.',
    skills: ['SQL', 'Visualization', 'KPIs', 'Strategic Analysis'],
    color: 'indigo',
  },
]

const COLOR_CLASSES = {
  blue: 'bg-blue-100 text-blue-700 border-blue-200',
  purple: 'bg-purple-100 text-purple-700 border-purple-200',
  green: 'bg-green-100 text-green-700 border-green-200',
  pink: 'bg-pink-100 text-pink-700 border-pink-200',
  yellow: 'bg-yellow-100 text-yellow-700 border-yellow-200',
  red: 'bg-red-100 text-red-700 border-red-200',
  indigo: 'bg-indigo-100 text-indigo-700 border-indigo-200',
}

export default function HomeTab({ onTabChange }) {
  return (
    <div className="space-y-8">
      {/* Hero Section */}
      <div className="bg-gradient-to-br from-blue-600 to-indigo-700 rounded-2xl p-8 text-white">
        <h1 className="text-3xl font-bold mb-4">Grey AI Internal</h1>
        <p className="text-xl text-blue-100 mb-6 max-w-2xl">
          A comprehensive AI platform demonstrating mastery across data analysis, 
          content generation, code assistance, creative AI, and business intelligence.
        </p>
        <div className="flex flex-wrap gap-3">
          <span className="px-3 py-1 bg-white/20 rounded-full text-sm">Full-Stack Development</span>
          <span className="px-3 py-1 bg-white/20 rounded-full text-sm">Machine Learning</span>
          <span className="px-3 py-1 bg-white/20 rounded-full text-sm">MLOps</span>
          <span className="px-3 py-1 bg-white/20 rounded-full text-sm">LLM Integration</span>
          <span className="px-3 py-1 bg-white/20 rounded-full text-sm">Data Engineering</span>
        </div>
      </div>

      {/* Executive Summary */}
      <Card>
        <div className="p-6">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">Executive Summary</h2>
          <div className="prose prose-gray max-w-none">
            <p className="text-gray-600 leading-relaxed">
              This platform showcases end-to-end AI capabilities across multiple domains. 
              Each tab demonstrates a distinct AI competency with production-ready implementations:
            </p>
            <ul className="mt-4 space-y-2 text-gray-600">
              <li><strong>Data Analysis:</strong> Upload CSVs, get structured insights with business implications</li>
              <li><strong>AI Writing:</strong> Generate professional content with configurable tone and format</li>
              <li><strong>AI Code:</strong> Scaffold, refactor, and test code with AI assistance</li>
              <li><strong>Multimodal AI:</strong> Image generation, audio synthesis, and cross-modal integration</li>
              <li><strong>Governance:</strong> Security, bias detection, and compliance recommendations</li>
            </ul>
          </div>
        </div>
      </Card>

      {/* Domain Cards */}
      <div>
        <h2 className="text-xl font-semibold text-gray-900 mb-4">AI Domains</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {DOMAINS.map((domain) => (
            <button
              key={domain.id}
              onClick={() => onTabChange(domain.id)}
              className="text-left bg-white rounded-xl border border-gray-200 p-5 hover:shadow-lg hover:border-gray-300 transition-all duration-200"
            >
              <h3 className="font-semibold text-gray-900 mb-2">{domain.title}</h3>
              <p className="text-sm text-gray-600 mb-4">{domain.description}</p>
              <div className="flex flex-wrap gap-1.5">
                {domain.skills.map((skill) => (
                  <span
                    key={skill}
                    className={`text-xs px-2 py-0.5 rounded-full border ${COLOR_CLASSES[domain.color]}`}
                  >
                    {skill}
                  </span>
                ))}
              </div>
            </button>
          ))}
        </div>
      </div>

      {/* Tech Stack */}
      <Card>
        <div className="p-6">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">Technology Stack</h2>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            <div>
              <h3 className="font-medium text-gray-700 mb-2">Backend</h3>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>Python 3.11</li>
                <li>FastAPI</li>
                <li>SQLAlchemy (async)</li>
                <li>PostgreSQL</li>
              </ul>
            </div>
            <div>
              <h3 className="font-medium text-gray-700 mb-2">Frontend</h3>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>React 18</li>
                <li>Tailwind CSS</li>
                <li>Vite</li>
                <li>Chart.js</li>
              </ul>
            </div>
            <div>
              <h3 className="font-medium text-gray-700 mb-2">AI/ML</h3>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>Ollama (Local LLM)</li>
                <li>LLaMA 3 / TinyLlama</li>
                <li>Pandas / NumPy</li>
                <li>Rule-based Fallbacks</li>
              </ul>
            </div>
            <div>
              <h3 className="font-medium text-gray-700 mb-2">DevOps</h3>
              <ul className="text-sm text-gray-600 space-y-1">
                <li>Docker Compose</li>
                <li>GitHub Actions</li>
                <li>Nginx</li>
                <li>Multi-stage Builds</li>
              </ul>
            </div>
          </div>
        </div>
      </Card>
    </div>
  )
}
