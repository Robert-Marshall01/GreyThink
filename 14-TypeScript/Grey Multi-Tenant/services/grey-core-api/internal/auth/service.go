// Package auth provides JWT-based authentication and authorization.
package auth

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"time"

	"github.com/golang-jwt/jwt/v5"
	"github.com/google/uuid"
	"github.com/grey-platform/grey-core-api/internal/config"
	"golang.org/x/crypto/bcrypt"
)

// Common errors for authentication.
var (
	ErrInvalidCredentials = errors.New("invalid credentials")
	ErrInvalidToken       = errors.New("invalid token")
	ErrTokenExpired       = errors.New("token expired")
	ErrUserNotActive      = errors.New("user account is not active")
)

// Claims represents the JWT claims for access tokens.
type Claims struct {
	UserID         uuid.UUID `json:"user_id"`
	OrganizationID uuid.UUID `json:"organization_id"`
	Email          string    `json:"email"`
	Role           string    `json:"role"`
	jwt.RegisteredClaims
}

// TokenPair represents an access and refresh token pair.
type TokenPair struct {
	AccessToken  string    `json:"access_token"`
	RefreshToken string    `json:"refresh_token"`
	ExpiresAt    time.Time `json:"expires_at"`
}

// Service provides authentication operations.
type Service struct {
	cfg        config.JWTConfig
	repository Repository
}

// Repository defines the interface for auth data persistence.
type Repository interface {
	StoreRefreshToken(ctx context.Context, userID uuid.UUID, tokenHash string, expiresAt time.Time) error
	ValidateRefreshToken(ctx context.Context, tokenHash string) (*RefreshTokenInfo, error)
	RevokeRefreshToken(ctx context.Context, tokenHash string) error
	RevokeAllUserTokens(ctx context.Context, userID uuid.UUID) error
}

// RefreshTokenInfo contains user information associated with a refresh token.
type RefreshTokenInfo struct {
	UserID         uuid.UUID
	OrganizationID uuid.UUID
	Email          string
}

// NewService creates a new authentication service.
func NewService(cfg config.JWTConfig, repo Repository) *Service {
	return &Service{
		cfg:        cfg,
		repository: repo,
	}
}

// GenerateTokenPair creates a new access and refresh token pair.
func (s *Service) GenerateTokenPair(ctx context.Context, userID, orgID uuid.UUID, email, role string) (*TokenPair, error) {
	// Generate access token
	accessToken, expiresAt, err := s.generateAccessToken(userID, orgID, email, role)
	if err != nil {
		return nil, fmt.Errorf("generating access token: %w", err)
	}

	// Generate refresh token
	refreshToken := uuid.New().String()
	refreshTokenHash := hashToken(refreshToken)
	refreshExpiresAt := time.Now().Add(s.cfg.RefreshTokenExpiry)

	// Store refresh token
	if err := s.repository.StoreRefreshToken(ctx, userID, refreshTokenHash, refreshExpiresAt); err != nil {
		return nil, fmt.Errorf("storing refresh token: %w", err)
	}

	return &TokenPair{
		AccessToken:  accessToken,
		RefreshToken: refreshToken,
		ExpiresAt:    expiresAt,
	}, nil
}

// generateAccessToken creates a new JWT access token.
func (s *Service) generateAccessToken(userID, orgID uuid.UUID, email, role string) (string, time.Time, error) {
	expiresAt := time.Now().Add(s.cfg.AccessTokenExpiry)

	claims := &Claims{
		UserID:         userID,
		OrganizationID: orgID,
		Email:          email,
		Role:           role,
		RegisteredClaims: jwt.RegisteredClaims{
			ExpiresAt: jwt.NewNumericDate(expiresAt),
			IssuedAt:  jwt.NewNumericDate(time.Now()),
			Issuer:    s.cfg.Issuer,
			Subject:   userID.String(),
		},
	}

	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	tokenString, err := token.SignedString([]byte(s.cfg.Secret))
	if err != nil {
		return "", time.Time{}, err
	}

	return tokenString, expiresAt, nil
}

// ValidateAccessToken validates an access token and returns its claims.
func (s *Service) ValidateAccessToken(tokenString string) (*Claims, error) {
	token, err := jwt.ParseWithClaims(tokenString, &Claims{}, func(token *jwt.Token) (interface{}, error) {
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		return []byte(s.cfg.Secret), nil
	})

	if err != nil {
		if errors.Is(err, jwt.ErrTokenExpired) {
			return nil, ErrTokenExpired
		}
		return nil, ErrInvalidToken
	}

	claims, ok := token.Claims.(*Claims)
	if !ok || !token.Valid {
		return nil, ErrInvalidToken
	}

	return claims, nil
}

// RefreshTokens validates a refresh token and generates a new token pair.
func (s *Service) RefreshTokens(ctx context.Context, refreshToken string) (*TokenPair, error) {
	tokenHash := hashToken(refreshToken)

	// Validate and get user info from refresh token (joins with users table)
	userInfo, err := s.repository.ValidateRefreshToken(ctx, tokenHash)
	if err != nil {
		return nil, ErrInvalidToken
	}

	// Revoke the old refresh token
	if err := s.repository.RevokeRefreshToken(ctx, tokenHash); err != nil {
		return nil, fmt.Errorf("revoking old token: %w", err)
	}

	// Generate new token pair using user info from refresh token validation
	return s.GenerateTokenPair(ctx, userInfo.UserID, userInfo.OrganizationID, userInfo.Email, "member")
}

// RevokeRefreshToken revokes a specific refresh token.
func (s *Service) RevokeRefreshToken(ctx context.Context, refreshToken string) error {
	tokenHash := hashToken(refreshToken)
	return s.repository.RevokeRefreshToken(ctx, tokenHash)
}

// RevokeAllUserTokens revokes all refresh tokens for a user.
func (s *Service) RevokeAllUserTokens(ctx context.Context, userID uuid.UUID) error {
	return s.repository.RevokeAllUserTokens(ctx, userID)
}

// HashPassword hashes a password using bcrypt.
func HashPassword(password string) (string, error) {
	hash, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	if err != nil {
		return "", err
	}
	return string(hash), nil
}

// CheckPassword compares a password with a hash.
func CheckPassword(password, hash string) bool {
	return bcrypt.CompareHashAndPassword([]byte(hash), []byte(password)) == nil
}

// hashToken creates a SHA-256 hash of a token.
func hashToken(token string) string {
	hash := sha256.Sum256([]byte(token))
	return hex.EncodeToString(hash[:])
}
