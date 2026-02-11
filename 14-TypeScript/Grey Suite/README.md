# Grey Suite

**The Unified Corporate Operating System**

Grey Suite is an enterprise-grade platform that consolidates your entire business stack into a single, cohesive application. It replaces fragmented tools across ERP, CRM, HCM, collaboration, AI automation, data intelligence, security, and more — all under one roof.

Warning: may be unstable. Verify stability before implementing in a high-stakes environment.

![TypeScript](https://img.shields.io/badge/TypeScript-5.6-blue)
![React](https://img.shields.io/badge/React-18.3-61dafb)
![Vite](https://img.shields.io/badge/Vite-6.0-646cff)
![License](https://img.shields.io/badge/License-MIT-green)

---

## Features

### Core Systems
- **ERP** — Finance, Procurement, Supply Chain management with real-time dashboards, budget tracking, purchase orders, and shipment monitoring
- **CRM** — Lead management (list & kanban views), account intelligence, deal pipeline with stage-based tracking and forecasting
- **HCM** — Recruiting pipeline, payroll & benefits administration, OKR-based performance management

### Collaboration
- **Chat** — Channel-based messaging with DMs, presence indicators, and rich text input
- **Video Conferencing** — Meeting scheduling, virtual rooms, and recording management
- **Knowledge Base** — Organized document spaces with full-text search
- **Whiteboard** — Real-time collaborative canvas with shapes, drawing tools, and color picker
- **Transcription** — AI-powered meeting transcription with speaker identification, summaries, and action items

### AI & Automation
- **Agent Framework** — Deploy and monitor autonomous AI agents with workflow orchestration
- **Generative Suite** — AI-assisted document, spreadsheet, presentation, and email creation
- **Specialized AI** — Marketing AI, voice AI, and intelligent summarization engines

### Data & Intelligence
- **BI Dashboards** — KPI tracking, revenue analytics, segment analysis, and executive reporting
- **Data Warehouse** — Schema browser, ETL pipeline monitoring, and data catalog
- **Product Analytics** — DAU/MAU metrics, conversion funnels, retention cohorts, and event tracking

### Operations & Security
- **Project Management** — Sprint boards (kanban & list), task cards with priority and story points
- **Identity & Security** — SSO/MFA posture, active sessions, audit logs, and threat monitoring
- **Finance & Payments** — MRR tracking, invoicing, expense approvals, and payout history

### Professional Presence
- **Networking** — Professional profiles, social feed, and connection suggestions
- **Reputation** — Company ratings, employee reviews, and workplace discussions
- **Personal CRM** — Contact management with relationship strength tracking and follow-up reminders

### Career & Development
- **Job Search** — Application tracker, ATS resume optimizer, outreach campaigns, and automated search agents
- **Skill Development** — Course progress, certifications, 12-dimension skill matrix, and portfolio showcase

### Infrastructure
- **Service Health** — Real-time monitoring of 48+ microservices with latency, RPS, and CPU metrics
- **Kubernetes** — Cluster management across multiple regions
- **CI/CD** — Deployment tracking with rollback history
- **Infrastructure as Code** — Terraform stack management
- **Monitoring** — Live metrics dashboard with error rates, P95 latency, and throughput

---

## Tech Stack

| Layer        | Technology                     |
| ------------ | ------------------------------ |
| Framework    | React 18 + TypeScript 5.6      |
| Build        | Vite 6                         |
| Styling      | Tailwind CSS 3.4               |
| Routing      | React Router v6                |
| Icons        | Lucide React                   |
| Charts       | Recharts                       |

---

## Getting Started

### Prerequisites

- [Node.js](https://nodejs.org/) 18+ 
- npm 9+

### Installation

```bash
git clone https://github.com/Robert-Marshall01/grey-suite.git
cd grey-suite
npm install
```

### Development

```bash
npm run dev
```

The app starts at [http://localhost:5173](http://localhost:5173).

### Production Build

```bash
npm run build
npm run preview
```

---

## Project Structure

```
grey-suite/
├── public/
│   └── favicon.svg
├── src/
│   ├── components/
│   │   ├── Layout.tsx          # App shell (sidebar + content + command palette)
│   │   ├── Sidebar.tsx         # Collapsible sidebar with module navigation
│   │   ├── Topbar.tsx          # Search, notifications, settings, user avatar
│   │   └── ui.tsx              # Shared UI component library
│   ├── pages/
│   │   ├── Dashboard.tsx       # Executive overview & home
│   │   ├── core/               # ERP, CRM, HCM modules
│   │   ├── collaboration/      # Chat, Video, KB, Whiteboard, Transcription
│   │   ├── ai/                 # Agent Framework, Generative Suite, Specialized AI
│   │   ├── data/               # BI, Data Warehouse, Product Analytics
│   │   ├── ops/                # Projects, Security, Finance
│   │   ├── presence/           # Networking, Reputation, Personal CRM
│   │   ├── career/             # Job Search, Skill Development
│   │   └── infra/              # Infrastructure monitoring
│   ├── App.tsx                 # Route definitions
│   ├── main.tsx                # Entry point
│   └── index.css               # Tailwind + custom component styles
├── index.html
├── package.json
├── vite.config.ts
├── tsconfig.json
├── tailwind.config.js
└── postcss.config.js
```

---

## Keyboard Shortcuts

| Shortcut   | Action               |
| ---------- | -------------------- |
| `Ctrl + K` | Open Command Palette |
| `Ctrl + /` | Collapse Sidebar     |

---

## Author

**Robert-Marshall01**

---

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
