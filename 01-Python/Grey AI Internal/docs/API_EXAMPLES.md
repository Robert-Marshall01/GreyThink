# Grey AI Internal - API Examples

This document shows example API requests and responses for the complete end-to-end flow.

## Complete Flow Overview

```
User uploads CSV → Backend parses → Metrics saved → User triggers AI → Ollama analyzes → Insights saved → Dashboard displays
```

---

## 1. Upload File

### Request

```http
POST /api/upload/
Content-Type: multipart/form-data

file: sales_data_2024.csv
```

### Response (201 Created)

```json
{
    "id": 1,
    "filename": "sales_data_2024.csv",
    "file_type": "csv",
    "file_size": 102400,
    "status": "parsed",
    "row_count": 1500,
    "column_count": 8,
    "created_at": "2026-01-30T10:30:00Z",
    "has_metrics": true,
    "has_insights": false,
    "message": "File uploaded and parsed successfully"
}
```

---

## 2. Get Upload Details

### Request

```http
GET /api/upload/1
```

### Response (200 OK)

```json
{
    "id": 1,
    "filename": "sales_data_2024.csv",
    "file_type": "csv",
    "file_size": 102400,
    "status": "parsed",
    "row_count": 1500,
    "column_count": 8,
    "created_at": "2026-01-30T10:30:00Z",
    "has_metrics": true,
    "has_insights": false,
    "metrics": {
        "id": 1,
        "upload_id": 1,
        "row_count": 1500,
        "column_count": 8,
        "columns": [
            {"name": "date", "dtype": "object", "non_null_count": 1500, "null_count": 0},
            {"name": "product_id", "dtype": "int64", "non_null_count": 1500, "null_count": 0},
            {"name": "product_name", "dtype": "object", "non_null_count": 1498, "null_count": 2},
            {"name": "category", "dtype": "object", "non_null_count": 1500, "null_count": 0},
            {"name": "region", "dtype": "object", "non_null_count": 1500, "null_count": 0},
            {"name": "units_sold", "dtype": "int64", "non_null_count": 1500, "null_count": 0},
            {"name": "unit_price", "dtype": "float64", "non_null_count": 1500, "null_count": 0},
            {"name": "revenue", "dtype": "float64", "non_null_count": 1500, "null_count": 0}
        ],
        "stats": {
            "units_sold": {
                "min": 12,
                "max": 450,
                "mean": 156.4,
                "median": 142.0,
                "std": 87.2
            },
            "unit_price": {
                "min": 9.99,
                "max": 299.99,
                "mean": 67.50,
                "median": 49.99,
                "std": 45.8
            },
            "revenue": {
                "min": 119.88,
                "max": 134995.50,
                "mean": 10546.80,
                "median": 7103.58,
                "std": 12450.30
            }
        },
        "preview": [
            {
                "date": "2024-01-15",
                "product_id": 1001,
                "product_name": "Premium Widget",
                "category": "Electronics",
                "region": "North",
                "units_sold": 156,
                "unit_price": 49.99,
                "revenue": 7798.44
            },
            {
                "date": "2024-01-15",
                "product_id": 1002,
                "product_name": "Basic Widget",
                "category": "Electronics",
                "region": "South",
                "units_sold": 89,
                "unit_price": 29.99,
                "revenue": 2669.11
            }
        ],
        "created_at": "2026-01-30T10:30:01Z"
    },
    "insights": null,
    "error_message": null
}
```

---

## 3. Trigger AI Analysis

### Request

```http
POST /api/ai/analyze/1
Content-Type: application/json

{
    "custom_prompt": "Focus on regional performance and identify top-selling products"
}
```

### Response (200 OK)

```json
{
    "id": 1,
    "upload_id": 1,
    "summary": "The sales data reveals strong Q4 2024 performance with $15.8M total revenue across 1,500 transactions. The North region leads with 34% of sales, while Electronics category dominates at 45% market share. Notable growth patterns indicate seasonal peaks in November-December.",
    "insights": [
        "North region outperformed all others with $5.4M revenue, a 23% increase from Q3",
        "Premium Widget is the top seller with 12,450 units sold, generating $621K revenue",
        "Electronics category shows 45% market share but declining margin (from 32% to 28%)",
        "December showed 156% spike in sales compared to monthly average",
        "South region has highest average order value ($89.50) despite lower volume"
    ],
    "recommendations": {
        "actions": [
            "Increase inventory for Premium Widget ahead of Q1 seasonal demand",
            "Investigate Electronics margin decline - potential supplier renegotiation needed",
            "Expand North region sales team to capitalize on strong performance",
            "Launch targeted campaigns in South region to increase volume while maintaining AOV",
            "Prepare for seasonal demand patterns with 60% additional stock in Q4"
        ],
        "priority": "high"
    },
    "model_used": "llama3",
    "created_at": "2026-01-30T10:35:00Z"
}
```

---

## 4. Get Cached Insights

### Request

```http
GET /api/ai/insights/1
```

### Response (200 OK)

Same as the analysis response above (returns cached insights).

### Response (404 Not Found) - When no insights exist

```json
{
    "detail": "No insights found for upload id=1. Run POST /api/ai/analyze/1 first."
}
```

---

## 5. List Uploads

### Request

```http
GET /api/upload/?page=1&page_size=10
```

### Response (200 OK)

```json
{
    "items": [
        {
            "id": 1,
            "filename": "sales_data_2024.csv",
            "file_type": "csv",
            "status": "analyzed",
            "row_count": 1500,
            "has_insights": true,
            "created_at": "2026-01-30T10:30:00Z"
        },
        {
            "id": 2,
            "filename": "inventory_report.csv",
            "file_type": "csv",
            "status": "parsed",
            "row_count": 850,
            "has_insights": false,
            "created_at": "2026-01-29T15:45:00Z"
        }
    ],
    "total": 42,
    "page": 1,
    "page_size": 10,
    "has_more": true
}
```

---

## 6. Check AI Status

### Request

```http
GET /api/ai/status
```

### Response (200 OK)

```json
{
    "available": true,
    "configured_model": "llama3",
    "ollama_url": "http://localhost:11434",
    "available_models": ["llama3", "mistral", "phi3", "codellama"],
    "status": "healthy"
}
```

### Response When Ollama is Unavailable

```json
{
    "available": false,
    "configured_model": "llama3",
    "ollama_url": "http://localhost:11434",
    "available_models": [],
    "status": "unavailable"
}
```

---

## 7. Raw AI Inference

### Request

```http
POST /api/ai/infer
Content-Type: application/json

{
    "prompt": "Explain the concept of data normalization in 3 sentences",
    "system_prompt": "You are a helpful data science tutor"
}
```

### Response (200 OK)

```json
{
    "response": "Data normalization is the process of organizing data in a database to reduce redundancy and improve data integrity. It involves dividing large tables into smaller, related tables and defining relationships between them. The main goals are to eliminate duplicate data, ensure dependencies make sense, and simplify queries.",
    "model": "llama3",
    "prompt_tokens": null,
    "completion_tokens": null
}
```

---

## Error Responses

### 400 Bad Request - Invalid File Type

```json
{
    "detail": "File type '.xlsx' not allowed. Allowed: ['.csv', '.txt']"
}
```

### 413 Request Entity Too Large

```json
{
    "detail": "File size (15.50MB) exceeds limit (10MB)"
}
```

### 404 Not Found

```json
{
    "detail": "Upload with id=999 not found"
}
```

### 503 Service Unavailable - Ollama Down

```json
{
    "detail": "Cannot connect to Ollama at http://localhost:11434. Ensure Ollama is running: ollama serve"
}
```

### 500 Internal Server Error

```json
{
    "detail": "AI analysis failed: Model 'llama3' not found. Run: ollama pull llama3"
}
```

---

## Example Ollama Prompts

### 1. Summarize Dataset Trends

```
Analyze the dataset and identify key trends:
1. What are the main patterns over time?
2. Are there any seasonal variations?
3. What is the overall direction (growth/decline)?
4. Which categories/segments show the strongest trends?
```

### 2. Highlight Anomalies

```
Examine the data for anomalies and outliers:
1. Identify any values that deviate significantly from the norm
2. Look for unexpected patterns or breaks in continuity
3. Highlight potential data quality issues
4. Suggest which anomalies warrant further investigation
```

### 3. Suggest Optimizations

```
Based on the data, suggest optimizations:
1. What inefficiencies can you identify?
2. Where are the biggest opportunities for improvement?
3. What actions would have the highest impact?
4. Prioritize recommendations by effort vs. impact
```

### 4. Executive Summary

```
Provide an executive summary suitable for stakeholders:
1. Key metrics and their current state
2. Most important findings (positive and negative)
3. Critical action items
4. Recommended next steps
```
