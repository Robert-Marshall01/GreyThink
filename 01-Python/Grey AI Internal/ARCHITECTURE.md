# AI Data Insights Hub - Architecture Plan

## System Overview

A lightweight, production-lean internal tool that enables users to upload data files (CSV/text), receive AI-generated insights, summaries, and recommendations powered by a local LLM via Ollama.

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                        FRONTEND (React)                         │
│  ┌─────────────┐  ┌──────────────┐  ┌────────────────────────┐  │
│  │ File Upload │  │ Metrics View │  │ AI Insights Display    │  │
│  └─────────────┘  └──────────────┘  └────────────────────────┘  │
└───────────────────────────┬─────────────────────────────────────┘
                            │ HTTP/REST
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                     BACKEND (FastAPI)                            │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                      API LAYER                            │   │
│  │  /api/upload  │  /api/analyze  │  /api/insights/{id}     │   │
│  └──────────────────────────────────────────────────────────┘   │
│                            │                                     │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                    SERVICE LAYER                          │   │
│  │  FileService  │  ParserService  │  AnalysisService       │   │
│  └──────────────────────────────────────────────────────────┘   │
│                            │                                     │
│  ┌─────────────────────┐  ┌───────────────────────────────┐     │
│  │   DATA LAYER        │  │      AI INTEGRATION           │     │
│  │   (SQLite/PG)       │  │      (Ollama Client)          │     │
│  └─────────────────────┘  └───────────────────────────────┘     │
└─────────────────────────────────────────────────────────────────┘
                                        │
                                        ▼
                            ┌───────────────────┐
                            │  OLLAMA (Local)   │
                            │  Llama 3/Mistral  │
                            └───────────────────┘
```

---

## Design Principles

1. **Clean Separation of Concerns**: API → Service → Data layers
2. **Dependency Injection**: Services injected via FastAPI's DI system
3. **Type Safety**: Pydantic models for all data contracts
4. **Async-First**: Non-blocking I/O for file ops and AI calls
5. **Minimal Dependencies**: Only essential packages

---

## Data Flow

1. **Upload**: User uploads CSV/text → stored in filesystem + metadata in DB
2. **Parse**: File content extracted → metrics computed → stored in DB
3. **Analyze**: Parsed data sent to Ollama → AI insights generated → stored in DB
4. **Display**: Frontend fetches insights via REST API

---

## Technology Stack

| Layer      | Technology                          |
|------------|-------------------------------------|
| Frontend   | React + Tailwind CSS                |
| Backend    | FastAPI + Python 3.11+              |
| Database   | SQLite (dev) / PostgreSQL (prod)    |
| AI Engine  | Ollama (local LLM)                  |
| File Store | Local filesystem (./uploads)        |

---

## Folder Structure

```
grey-ai-internal/
├── backend/
│   ├── app/
│   │   ├── __init__.py
│   │   ├── main.py              # FastAPI app entry
│   │   ├── config.py            # Settings & env vars
│   │   ├── api/
│   │   │   ├── __init__.py
│   │   │   ├── routes.py        # All API endpoints
│   │   │   └── deps.py          # Dependency injection
│   │   ├── services/
│   │   │   ├── __init__.py
│   │   │   ├── file_service.py  # File upload/management
│   │   │   ├── parser_service.py # Data parsing & metrics
│   │   │   └── analysis_service.py # AI-powered analysis
│   │   ├── ai/
│   │   │   ├── __init__.py
│   │   │   ├── ollama_client.py # Ollama API wrapper
│   │   │   └── prompts.py       # Prompt templates
│   │   ├── models/
│   │   │   ├── __init__.py
│   │   │   ├── database.py      # DB connection & session
│   │   │   ├── schemas.py       # Pydantic models (API)
│   │   │   └── orm.py           # SQLAlchemy ORM models
│   │   └── utils/
│   │       ├── __init__.py
│   │       └── helpers.py       # Utility functions
│   ├── uploads/                 # Uploaded files storage
│   ├── requirements.txt
│   ├── .env.example
│   └── alembic/                 # DB migrations (optional)
├── frontend/
│   ├── src/
│   │   ├── App.jsx
│   │   ├── main.jsx
│   │   ├── components/
│   │   │   ├── FileUpload.jsx
│   │   │   ├── MetricsView.jsx
│   │   │   └── InsightsPanel.jsx
│   │   ├── services/
│   │   │   └── api.js
│   │   └── index.css
│   ├── index.html
│   ├── package.json
│   ├── vite.config.js
│   └── tailwind.config.js
└── README.md
```

---

## Database Schema

### Tables

1. **uploads** - File metadata
2. **parsed_metrics** - Extracted data & computed metrics
3. **insights** - AI-generated summaries & recommendations
4. **sessions** (optional) - User session tracking

### ERD

```
┌──────────────┐       ┌─────────────────┐       ┌──────────────┐
│   uploads    │──1:1──│  parsed_metrics │──1:N──│   insights   │
├──────────────┤       ├─────────────────┤       ├──────────────┤
│ id (PK)      │       │ id (PK)         │       │ id (PK)      │
│ filename     │       │ upload_id (FK)  │       │ metrics_id   │
│ file_path    │       │ row_count       │       │ summary      │
│ file_type    │       │ column_count    │       │ insights_json│
│ file_size    │       │ columns_json    │       │ recommendations│
│ created_at   │       │ stats_json      │       │ model_used   │
│ status       │       │ preview_json    │       │ created_at   │
└──────────────┘       │ created_at      │       └──────────────┘
                       └─────────────────┘
```

---

## API Endpoints

| Method | Endpoint              | Description                     |
|--------|----------------------|---------------------------------|
| POST   | /api/upload          | Upload CSV/text file            |
| GET    | /api/uploads         | List all uploads                |
| GET    | /api/uploads/{id}    | Get upload details              |
| POST   | /api/analyze/{id}    | Trigger AI analysis             |
| GET    | /api/insights/{id}   | Get insights for an upload      |
| GET    | /health              | Health check                    |

---

## Security Considerations

- File type validation (whitelist: .csv, .txt)
- File size limits (configurable, default 10MB)
- Sanitized filenames (prevent path traversal)
- CORS configured for frontend origin only
- Rate limiting on AI endpoints (prevent abuse)
