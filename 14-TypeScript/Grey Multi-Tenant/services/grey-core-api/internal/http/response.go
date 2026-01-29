// Package http provides the HTTP server and routing for the API.
package http

import (
	"encoding/json"
	"net/http"
)

// ErrorResponse represents an error response matching OpenAPI ErrorResponse schema.
type ErrorResponse struct {
	Error   string `json:"error"`
	Message string `json:"message"`
}

// Pagination represents pagination metadata matching OpenAPI Pagination schema.
type Pagination struct {
	Page     int `json:"page"`
	PageSize int `json:"page_size"`
	Total    int `json:"total"`
}

// PaginatedResponse creates a paginated response.
type PaginatedResponse struct {
	Data       interface{} `json:"data"`
	Pagination Pagination  `json:"pagination"`
}

// WriteJSON writes a JSON response with the given status code.
// For single resources, writes the data directly without a wrapper.
func WriteJSON(w http.ResponseWriter, status int, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if data != nil {
		if err := json.NewEncoder(w).Encode(data); err != nil {
			// Log error but can't do much at this point
			_ = err
		}
	}
}

// WriteError writes an error response matching OpenAPI ErrorResponse schema.
// error: short error code (e.g., "not_found", "validation_error")
// message: human-readable error message
func WriteError(w http.ResponseWriter, status int, errorCode, message string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)

	response := ErrorResponse{
		Error:   errorCode,
		Message: message,
	}
	_ = json.NewEncoder(w).Encode(response)
}

// WritePaginated writes a paginated JSON response matching OpenAPI format.
func WritePaginated(w http.ResponseWriter, status int, data interface{}, page, pageSize, total int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)

	response := PaginatedResponse{
		Data: data,
		Pagination: Pagination{
			Page:     page,
			PageSize: pageSize,
			Total:    total,
		},
	}
	_ = json.NewEncoder(w).Encode(response)
}

// ParseJSON parses the request body as JSON into the given struct.
func ParseJSON(r *http.Request, v interface{}) error {
	return json.NewDecoder(r.Body).Decode(v)
}
