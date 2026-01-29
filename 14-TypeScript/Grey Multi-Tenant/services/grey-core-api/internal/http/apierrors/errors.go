// Package apierrors provides standardized error handling for the API.
package apierrors

import (
	"errors"
	"fmt"
	"net/http"
)

// Error codes used throughout the API.
const (
	CodeBadRequest     = "BadRequest"
	CodeUnauthorized   = "Unauthorized"
	CodeForbidden      = "Forbidden"
	CodeNotFound       = "NotFound"
	CodeConflict       = "Conflict"
	CodeInternalError  = "InternalServerError"
	CodeValidation     = "ValidationError"
)

// APIError represents a structured API error.
type APIError struct {
	Code       string `json:"code"`
	Message    string `json:"message"`
	Details    string `json:"details,omitempty"`
	StatusCode int    `json:"-"`
	Err        error  `json:"-"`
}

// Error implements the error interface.
func (e *APIError) Error() string {
	if e.Details != "" {
		return fmt.Sprintf("%s: %s (%s)", e.Code, e.Message, e.Details)
	}
	return fmt.Sprintf("%s: %s", e.Code, e.Message)
}

// Unwrap returns the wrapped error.
func (e *APIError) Unwrap() error {
	return e.Err
}

// Is checks if the target error matches this error's code.
func (e *APIError) Is(target error) bool {
	var apiErr *APIError
	if errors.As(target, &apiErr) {
		return e.Code == apiErr.Code
	}
	return false
}

// StatusCode returns the HTTP status code for this error.
func (e *APIError) HTTPStatusCode() int {
	if e.StatusCode != 0 {
		return e.StatusCode
	}
	// Default status codes based on error code
	switch e.Code {
	case CodeBadRequest, CodeValidation:
		return http.StatusBadRequest
	case CodeUnauthorized:
		return http.StatusUnauthorized
	case CodeForbidden:
		return http.StatusForbidden
	case CodeNotFound:
		return http.StatusNotFound
	case CodeConflict:
		return http.StatusConflict
	default:
		return http.StatusInternalServerError
	}
}

// NewBadRequest creates a bad request error.
func NewBadRequest(message string, err error) *APIError {
	return &APIError{
		Code:       CodeBadRequest,
		Message:    message,
		StatusCode: http.StatusBadRequest,
		Err:        err,
	}
}

// NewUnauthorized creates an unauthorized error.
func NewUnauthorized(message string) *APIError {
	return &APIError{
		Code:       CodeUnauthorized,
		Message:    message,
		StatusCode: http.StatusUnauthorized,
	}
}

// NewForbidden creates a forbidden error.
func NewForbidden(message string) *APIError {
	return &APIError{
		Code:       CodeForbidden,
		Message:    message,
		StatusCode: http.StatusForbidden,
	}
}

// NewNotFound creates a not found error.
func NewNotFound(resource string) *APIError {
	return &APIError{
		Code:       CodeNotFound,
		Message:    fmt.Sprintf("%s not found", resource),
		StatusCode: http.StatusNotFound,
	}
}

// NewConflict creates a conflict error.
func NewConflict(message string) *APIError {
	return &APIError{
		Code:       CodeConflict,
		Message:    message,
		StatusCode: http.StatusConflict,
	}
}

// NewInternalError creates an internal server error.
func NewInternalError(message string, err error) *APIError {
	return &APIError{
		Code:       CodeInternalError,
		Message:    message,
		StatusCode: http.StatusInternalServerError,
		Err:        err,
	}
}

// NewValidationError creates a validation error with details.
func NewValidationError(message, details string) *APIError {
	return &APIError{
		Code:       CodeValidation,
		Message:    message,
		Details:    details,
		StatusCode: http.StatusBadRequest,
	}
}

// Wrap wraps an error in an APIError.
func Wrap(code string, message string, err error) *APIError {
	return &APIError{
		Code:    code,
		Message: message,
		Err:     err,
	}
}

// IsAPIError checks if an error is an APIError.
func IsAPIError(err error) bool {
	var apiErr *APIError
	return errors.As(err, &apiErr)
}

// GetAPIError extracts an APIError from an error.
func GetAPIError(err error) (*APIError, bool) {
	var apiErr *APIError
	if errors.As(err, &apiErr) {
		return apiErr, true
	}
	return nil, false
}
