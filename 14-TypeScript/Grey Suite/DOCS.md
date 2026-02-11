# Grey Suite — Documentation

> Comprehensive technical documentation for the Grey Suite corporate operating system.

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Module Reference](#module-reference)
   - [Dashboard](#dashboard)
   - [Core Systems](#core-systems)
   - [Collaboration](#collaboration)
   - [AI & Automation](#ai--automation)
   - [Data & Intelligence](#data--intelligence)
   - [Operations & Security](#operations--security)
   - [Professional Presence](#professional-presence)
   - [Career & Development](#career--development)
   - [Infrastructure](#infrastructure)
3. [Component Library](#component-library)
4. [Routing](#routing)
5. [Theming & Design System](#theming--design-system)
6. [Configuration](#configuration)
7. [Development Guide](#development-guide)

---

## Architecture Overview

Grey Suite follows a **modular single-page application** architecture built with React 18 and TypeScript.

```
┌─────────────────────────────────────────────────────┐
│                      App Shell                       │
│  ┌──────────┐  ┌──────────────────────────────────┐ │
│  │          │  │            Topbar                 │ │
│  │          │  ├──────────────────────────────────┤ │
│  │ Sidebar  │  │                                  │ │
│  │          │  │          Page Content             │ │
│  │ (Nav)    │  │       (React Router Outlet)      │ │
│  │          │  │                                  │ │
│  │          │  │                                  │ │
│  └──────────┘  └──────────────────────────────────┘ │
│                   Command Palette (Ctrl+K)           │
└─────────────────────────────────────────────────────┘
```

### Key Principles

- **Module isolation** — Each business domain (CRM, ERP, HCM, etc.) is self-contained in its own directory under `src/pages/`
- **Shared component library** — Reusable UI primitives in `src/components/ui.tsx` ensure visual consistency
- **Client-side routing** — React Router v6 handles all navigation without page reloads
- **Dark-first design** — The entire UI is built on a dark grey palette optimized for extended use

### Data Flow

Currently, all module data is defined as local state / inline mock data within each page component. This architecture is designed so that each module can be connected to its own API service or shared data layer when backend integration is implemented.

---

## Module Reference

### Dashboard

**Route:** `/`  
**File:** `src/pages/Dashboard.tsx`

The executive home screen providing a unified view across all modules.

| Section | Description |
| ------- | ----------- |
| Welcome Banner | Personalized greeting with date and system status |
| Executive KPIs | Revenue MTD, active customers, open deals, employee count |
| Revenue Overview | 12-month sparkline with MRR, ARR, NRR, and churn metrics |
| Quick Actions | Direct links to the 6 most-used modules |
| Recent Activity | Real-time feed of actions across all modules |
| AI Activity | Agent run counts, time saved, and top agents |
| System Health | Service status with latency, uptime, and deploy stats |
| Upcoming | Calendar view of today's meetings |
| My Tasks | Personal task list with priority and due dates |

---

### Core Systems

#### ERP — Finance

**Route:** `/erp/finance`  
**File:** `src/pages/core/ERPFinance.tsx`

Full financial management including:
- Revenue, net income, expenses, and cash flow stat cards
- Revenue trend sparkline (12 months)
- Budget allocation by department with progress bars
- Recent transactions table with category, date, and status

#### ERP — Procurement

**Route:** `/erp/procurement`  
**File:** `src/pages/core/ERPProcurement.tsx`

Purchase order and vendor management:
- Active POs, pending approvals, spend tracking
- Top vendors ranked by spend and order volume
- Approval queue with approve/reject actions
- Purchase order history table

#### ERP — Supply Chain

**Route:** `/erp/supply-chain`  
**File:** `src/pages/core/ERPSupplyChain.tsx`

Supply chain visibility:
- Active shipments, in-transit, delivered, and delayed counts
- Fulfillment rate sparkline
- Warehouse capacity by location with progress bars
- Shipment tracking table with origin, destination, status

#### CRM — Leads

**Route:** `/crm/leads`  
**File:** `src/pages/core/CRMLeads.tsx`

Lead management with dual views:
- **List View** — Sortable lead table with scores, source, and status
- **Kanban View** — Drag-style columns for Hot, Warm, and Cold leads
- Lead statistics with conversion rates
- Lead scoring with color-coded badges

#### CRM — Accounts

**Route:** `/crm/accounts`  
**File:** `src/pages/core/CRMAccounts.tsx`

Account intelligence:
- Total accounts, enterprise tier count, ARR, NPS score
- Top accounts table ranked by revenue
- Industry breakdown with visual bars
- Account health indicators

#### CRM — Opportunities

**Route:** `/crm/opportunities`  
**File:** `src/pages/core/CRMOpportunities.tsx`

Deal pipeline management:
- 5-stage pipeline cards (Discovery → Closed Won) with deal counts and values
- Forecast sparkline for projected revenue
- Opportunities table with owner, value, close date, and probability

#### HCM — Recruiting

**Route:** `/hcm/recruiting`  
**File:** `src/pages/core/HCMRecruiting.tsx`

Hiring pipeline:
- Open positions, applications, interviews scheduled, offers pending
- 4-stage recruiting pipeline (Applied → Offer) with visual cards
- Open positions table with department, level, and applicant count
- Candidate profile cards with skills and status

#### HCM — Payroll

**Route:** `/hcm/payroll`  
**File:** `src/pages/core/HCMPayroll.tsx`

Compensation management:
- Monthly payroll total, average salary, benefits cost, tax withholdings
- Department salary breakdown
- Benefits package grid (health, dental, vision, 401k, PTO, etc.)
- Payroll history table by period

#### HCM — Performance

**Route:** `/hcm/performance`  
**File:** `src/pages/core/HCMPerformance.tsx`

Performance management:
- Quarterly OKR tracking with progress bars and status indicators
- Rating distribution across the organization
- Top performers grid with scores and departments

---

### Collaboration

#### Chat

**Route:** `/collaboration/chat`  
**File:** `src/pages/collaboration/Chat.tsx`

Full-featured messaging:
- Channel sidebar with unread count badges
- Direct message list with online/offline/away status dots
- Message thread display with timestamps and avatars
- Rich text input with attachment, emoji, and mention controls

#### Video Conferencing

**Route:** `/collaboration/video`  
**File:** `src/pages/collaboration/VideoConference.tsx`

Meeting management:
- Quick join with meeting code input
- Today's scheduled meetings with time and attendee count
- Meeting recordings library
- Virtual meeting rooms with capacity and join buttons

#### Knowledge Base

**Route:** `/collaboration/knowledge`  
**File:** `src/pages/collaboration/KnowledgeBase.tsx`

Document management:
- Full-text search across all spaces
- 6 knowledge spaces (Engineering, Product, Design, Marketing, HR, Finance) with emoji icons and article counts
- Recent documents list with author and update timestamps

#### Whiteboard

**Route:** `/collaboration/whiteboard`  
**File:** `src/pages/collaboration/Whiteboard.tsx`

Collaborative canvas:
- Toolbar with select, rectangle, circle, line, pen draw, and text tools
- Color picker (8 colors)
- Dot-grid canvas with sample shapes
- Board gallery for managing multiple whiteboards

#### Transcription

**Route:** `/collaboration/transcription`  
**File:** `src/pages/collaboration/Transcription.tsx`

AI meeting transcription:
- Real-time transcript viewer with speaker identification and timestamps
- AI-generated summary sidebar with key decisions, action items, and sentiment analysis
- Transcription history table with duration, speakers, and topic count

---

### AI & Automation

#### Agent Framework

**Route:** `/ai/agent-framework`  
**File:** `src/pages/ai/AgentFramework.tsx`

Autonomous AI agents:
- Agent cards showing status (active/paused), total runs, and success rate
- Tabbed view for Agents vs Workflows
- Workflow visualizer with sequential step cards and status indicators

#### Generative Suite

**Route:** `/ai/generative`  
**File:** `src/pages/ai/GenerativeSuite.tsx`

AI content creation:
- 4 app selectors: Docs AI, Sheets AI, Slides AI, Email AI
- AI composer with prompt textarea and tone/length controls
- Recent AI-generated creations list with type indicators

#### Specialized AI

**Route:** `/ai/specialized`  
**File:** `src/pages/ai/SpecializedAI.tsx`

Domain-specific AI tools:
- **Marketing AI** — Campaign optimization, A/B testing, audience targeting
- **Voice AI** — Speech-to-text, sentiment, call analysis
- **Summarization AI** — Document, meeting, and email summarization
- Usage overview with request counts, accuracy, and processing time

---

### Data & Intelligence

#### BI Dashboards

**Route:** `/data/bi`  
**File:** `src/pages/data/BIDashboards.tsx`

Business intelligence:
- Dashboard tab selector (Overview, Sales, Marketing, Finance)
- KPI stat cards with change indicators
- Revenue trend sparkline
- Revenue by segment breakdown bars
- Top products, geographic performance, and saved reports

#### Data Warehouse

**Route:** `/data/warehouse`  
**File:** `src/pages/data/DataWarehouse.tsx`

Data infrastructure:
- Interactive schema browser with table/column exploration
- ETL pipeline status list with health indicators
- Data catalog table with row counts, freshness, and ownership

#### Product Analytics

**Route:** `/data/analytics`  
**File:** `src/pages/data/ProductAnalytics.tsx`

Product metrics:
- DAU, total events, session duration, error rate
- Active users sparkline over time
- Conversion funnel with step-by-step drop-off percentages
- Weekly retention cohort heatmap
- Top events ranked by volume

---

### Operations & Security

#### Project Management

**Route:** `/ops/projects`  
**File:** `src/pages/ops/ProjectManagement.tsx`

Agile project management:
- Toggle between Board and List views
- Kanban board with 4 columns: To Do, In Progress, In Review, Done
- Task cards with assignee, priority badges (Critical/High/Medium/Low), and story points
- Sprint progress tracking

#### Identity & Security

**Route:** `/ops/security`  
**File:** `src/pages/ops/IdentitySecurity.tsx`

Security operations:
- Security posture checklist (SSO, MFA, encryption, key rotation, etc.)
- Active sessions list with device, IP, and location
- Audit log table with user actions, timestamps, and blocked threats

#### Finance & Payments

**Route:** `/ops/finance`  
**File:** `src/pages/ops/FinancePayments.tsx`

Financial operations:
- MRR, outstanding balance, processing volume, payout tracking
- Invoice list with client names, amounts, due dates, and status
- Expense approval queue with requester, amount, and category
- Payout history with settlement dates and method

---

### Professional Presence

#### Networking

**Route:** `/presence/networking`  
**File:** `src/pages/presence/Networking.tsx`

Professional networking:
- User profile card with headline, connections, and endorsements
- Social feed with posts, likes, and comments
- Suggested connections with mutual contact counts

#### Reputation

**Route:** `/presence/reputation`  
**File:** `src/pages/presence/Reputation.tsx`

Company reputation management:
- Overall company rating trend sparkline
- Category ratings (Work-Life Balance, Compensation, Culture, Management, Growth)
- Employee review cards with star ratings
- Workplace discussion threads

#### Personal CRM

**Route:** `/presence/personal-crm`  
**File:** `src/pages/presence/PersonalCRM.tsx`

Relationship management:
- Follow-up reminder cards with due dates and priority
- Contact grid with relationship strength indicators (Strong/Medium/Weak)
- Contact tags and last interaction dates

---

### Career & Development

#### Job Search

**Route:** `/career/jobs`  
**File:** `src/pages/career/JobSearch.tsx`

Job search toolkit with 4 tabs:
- **Tracker** — Application pipeline with status stages
- **ATS Optimizer** — Resume analysis textarea with keyword matching
- **Outreach** — Campaign management with response tracking
- **Automation** — Auto-search agents with saved criteria and alerts

#### Skill Development

**Route:** `/career/skills`  
**File:** `src/pages/career/SkillDevelopment.tsx`

Professional growth:
- Active courses with progress bars and completion percentage
- Certifications list with expiration tracking
- 12-dimension skill matrix grid (leadership, technical, communication, etc.)
- Portfolio / proof-of-work showcase cards

---

### Infrastructure

**Route:** `/infra`  
**File:** `src/pages/infra/Infrastructure.tsx`

Platform operations:
- **Service Health** — 8 core services with latency (ms), RPS, CPU usage, and status
- **Kubernetes** — 4 cluster overview with node counts, pod status, and regions
- **CI/CD** — Recent deployments with branch, commit, author, duration, and status
- **Infrastructure as Code** — Terraform stacks with resource counts, drift detection, and last apply timestamp
- **Monitoring** — Real-time metrics grid (error rate, P95 latency, throughput, active connections)

---

## Component Library

All shared UI components are exported from `src/components/ui.tsx`.

### StatCard

Displays a KPI metric with label, value, change indicator, and optional icon.

```tsx
<StatCard
  label="Revenue"
  value="$14.8M"
  change="+12.3%"
  changeType="positive"   // "positive" | "negative" | "neutral"
  icon={<DollarSign size={18} />}
/>
```

### PageHeader

Page title with optional subtitle and action buttons.

```tsx
<PageHeader
  title="Finance"
  subtitle="ERP Module"
  actions={<button className="gs-btn-primary">Export</button>}
/>
```

### DataTable

Renders a sortable data table from column definitions and row data.

```tsx
<DataTable
  columns={['Name', 'Status', 'Amount']}
  rows={[
    ['Acme Corp', <Badge variant="success">Active</Badge>, '$50,000'],
    ['Globex', <Badge variant="warning">Pending</Badge>, '$28,000'],
  ]}
/>
```

### Badge

Status indicator with 6 variants.

```tsx
<Badge variant="success">Active</Badge>
<Badge variant="warning">Pending</Badge>
<Badge variant="danger">Critical</Badge>
<Badge variant="info">Info</Badge>
<Badge variant="purple">Premium</Badge>
<Badge variant="default">Default</Badge>
```

### ProgressBar

Visual progress indicator with customizable color and size.

```tsx
<ProgressBar value={72} max={100} color="brand" size="md" />
```

### Tabs

Tab switcher component.

```tsx
<Tabs
  tabs={['Overview', 'Details', 'History']}
  active="Overview"
  onChange={(tab) => setActiveTab(tab)}
/>
```

### EmptyState

Placeholder for empty data states with icon, title, and description.

```tsx
<EmptyState
  icon={<Inbox size={48} />}
  title="No items yet"
  description="Create your first item to get started."
/>
```

### MiniSparkline

Inline SVG sparkline chart for trend visualization.

```tsx
<MiniSparkline data={[10, 25, 18, 32, 28, 45]} height={60} />
```

---

## Routing

All routes are defined in `src/App.tsx` using React Router v6 nested routes.

| Path | Component | Module |
| ---- | --------- | ------ |
| `/` | Dashboard | Home |
| `/erp/finance` | ERPFinance | ERP |
| `/erp/procurement` | ERPProcurement | ERP |
| `/erp/supply-chain` | ERPSupplyChain | ERP |
| `/crm/leads` | CRMLeads | CRM |
| `/crm/accounts` | CRMAccounts | CRM |
| `/crm/opportunities` | CRMOpportunities | CRM |
| `/hcm/recruiting` | HCMRecruiting | HCM |
| `/hcm/payroll` | HCMPayroll | HCM |
| `/hcm/performance` | HCMPerformance | HCM |
| `/collaboration/chat` | Chat | Collaboration |
| `/collaboration/video` | VideoConference | Collaboration |
| `/collaboration/knowledge` | KnowledgeBase | Collaboration |
| `/collaboration/whiteboard` | Whiteboard | Collaboration |
| `/collaboration/transcription` | Transcription | Collaboration |
| `/ai/agent-framework` | AgentFramework | AI |
| `/ai/generative` | GenerativeSuite | AI |
| `/ai/specialized` | SpecializedAI | AI |
| `/data/bi` | BIDashboards | Data |
| `/data/warehouse` | DataWarehouse | Data |
| `/data/analytics` | ProductAnalytics | Data |
| `/ops/projects` | ProjectManagement | Ops |
| `/ops/security` | IdentitySecurity | Ops |
| `/ops/finance` | FinancePayments | Ops |
| `/presence/networking` | Networking | Presence |
| `/presence/reputation` | Reputation | Presence |
| `/presence/personal-crm` | PersonalCRM | Presence |
| `/career/jobs` | JobSearch | Career |
| `/career/skills` | SkillDevelopment | Career |
| `/infra` | Infrastructure | Infra |

---

## Theming & Design System

### Color Palette

Grey Suite uses a custom dark-first color system defined in `tailwind.config.js`.

| Token | Usage | Hex Range |
| ----- | ----- | --------- |
| `grey-50` to `grey-950` | Backgrounds, text, borders | `#f8f9fa` → `#0a0a0f` |
| `brand-50` to `brand-900` | Primary actions, links, accents | `#eff6ff` → `#1e3a5f` |
| `accent-emerald` | Success, positive changes | `#34d399` |
| `accent-amber` | Warnings, medium priority | `#fbbf24` |
| `accent-rose` | Errors, critical, danger | `#fb7185` |
| `accent-violet` | AI features, premium badges | `#a78bfa` |
| `accent-cyan` | Information, analytics | `#22d3ee` |

### Typography

- **Primary:** Inter (sans-serif) — UI text, headings, labels
- **Monospace:** JetBrains Mono — code snippets, table IDs, technical data

### CSS Component Classes

Custom utility classes defined in `src/index.css`:

| Class | Description |
| ----- | ----------- |
| `.gs-card` | Card container with dark background, border, and hover shadow |
| `.gs-btn-primary` | Primary action button (brand color) |
| `.gs-btn-secondary` | Secondary action button (grey) |
| `.gs-btn-ghost` | Ghost/text button |
| `.gs-input` | Text input with dark styling |
| `.gs-badge` | Small status badge |
| `.gs-table` | Styled data table |
| `.gs-sidebar-item` | Sidebar navigation link |
| `.gs-stat-card` | KPI stat card |
| `.gs-section-title` | Section heading |
| `.gs-page-header` | Page title area |

### Animations

| Animation | Usage |
| --------- | ----- |
| `fadeIn` | Page transitions, modal entrances |
| `slideIn` | Sidebar items, dropdown menus |
| `pulse-dot` | Status indicators (e.g., system health dots) |

---

## Configuration

### Vite (`vite.config.ts`)

- React plugin enabled
- Path alias: `@/` → `./src/*`
- Dev server: port 5173, auto-opens browser

### TypeScript (`tsconfig.json`)

- Target: ES2020
- Module resolution: Bundler
- JSX: react-jsx
- Strict mode enabled
- Path alias: `@/*` → `./src/*`

### Tailwind (`tailwind.config.js`)

- Content paths: `./index.html`, `./src/**/*.{js,ts,jsx,tsx}`
- Extended theme with custom colors, fonts, and shadows
- No plugins required

---

## Development Guide

### Adding a New Module Page

1. Create a new `.tsx` file in the appropriate `src/pages/` subdirectory
2. Export a named function component
3. Use shared components from `@/components/ui`
4. Add the route in `src/App.tsx`
5. Add the sidebar link in `src/components/Sidebar.tsx`

Example:

```tsx
// src/pages/newmodule/MyPage.tsx
import { PageHeader, StatCard } from '../../components/ui';

export function MyPage() {
  return (
    <div>
      <PageHeader title="My Module" subtitle="Description" />
      <div className="grid grid-cols-4 gap-4">
        <StatCard label="Metric" value="100" />
      </div>
    </div>
  );
}
```

### Adding a Sidebar Section

In `src/components/Sidebar.tsx`, add an entry to the navigation sections array with:
- Section label
- Section icon
- Array of child links with `label` and `path`

### Customizing the Theme

Edit `tailwind.config.js` to modify:
- Color tokens under `theme.extend.colors`
- Font families under `theme.extend.fontFamily`
- Shadows under `theme.extend.boxShadow`

---

## Browser Support

- Chrome 90+
- Firefox 90+
- Safari 15+
- Edge 90+

---

## Author

**Robert-Marshall01**

## License

MIT — see [LICENSE](LICENSE)
