// Package auth provides JWT-based authentication and authorization.
package auth

// LoginRequest represents the API request for logging in.
// Matches OpenAPI LoginRequest schema.
type LoginRequest struct {
	Email    string `json:"email" validate:"required,email"`
	Password string `json:"password" validate:"required,min=8"`
}

// RefreshTokenRequest represents the API request for refreshing tokens.
// Matches OpenAPI RefreshTokenRequest schema.
type RefreshTokenRequest struct {
	RefreshToken string `json:"refresh_token" validate:"required"`
}

// RegisterRequest represents the API request for user registration.
type RegisterRequest struct {
	Email    string `json:"email" validate:"required,email"`
	Password string `json:"password" validate:"required,min=6"`
	Name     string `json:"name" validate:"required"`
}

// AuthSessionResponse represents the API response for authentication.
// Matches OpenAPI AuthSession schema.
type AuthSessionResponse struct {
	AccessToken  string `json:"access_token"`
	RefreshToken string `json:"refresh_token"`
	ExpiresIn    int    `json:"expires_in"` // Seconds until access token expires
}
