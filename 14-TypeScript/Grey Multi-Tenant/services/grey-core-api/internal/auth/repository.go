// Package auth provides JWT-based authentication and authorization.
package auth

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

// PostgresRepository implements the Repository interface using PostgreSQL.
type PostgresRepository struct {
	pool *pgxpool.Pool
}

// NewPostgresRepository creates a new PostgreSQL auth repository.
func NewPostgresRepository(pool *pgxpool.Pool) *PostgresRepository {
	return &PostgresRepository{pool: pool}
}

// StoreRefreshToken stores a new refresh token in the database.
func (r *PostgresRepository) StoreRefreshToken(ctx context.Context, userID uuid.UUID, tokenHash string, expiresAt time.Time) error {
	query := `
		INSERT INTO refresh_tokens (user_id, token_hash, expires_at)
		VALUES ($1, $2, $3)
	`
	_, err := r.pool.Exec(ctx, query, userID, tokenHash, expiresAt)
	return err
}

// ValidateRefreshToken checks if a refresh token is valid and returns the associated user info.
func (r *PostgresRepository) ValidateRefreshToken(ctx context.Context, tokenHash string) (*RefreshTokenInfo, error) {
	query := `
		SELECT rt.user_id, u.organization_id, u.email
		FROM refresh_tokens rt
		JOIN users u ON u.id = rt.user_id
		WHERE rt.token_hash = $1 
			AND rt.expires_at > NOW() 
			AND rt.revoked_at IS NULL
	`

	var info RefreshTokenInfo
	err := r.pool.QueryRow(ctx, query, tokenHash).Scan(&info.UserID, &info.OrganizationID, &info.Email)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrInvalidToken
		}
		return nil, err
	}

	return &info, nil
}

// RevokeRefreshToken marks a refresh token as revoked.
func (r *PostgresRepository) RevokeRefreshToken(ctx context.Context, tokenHash string) error {
	query := `
		UPDATE refresh_tokens 
		SET revoked_at = NOW() 
		WHERE token_hash = $1 AND revoked_at IS NULL
	`
	result, err := r.pool.Exec(ctx, query, tokenHash)
	if err != nil {
		return err
	}
	if result.RowsAffected() == 0 {
		return sql.ErrNoRows
	}
	return nil
}

// RevokeAllUserTokens revokes all refresh tokens for a specific user.
func (r *PostgresRepository) RevokeAllUserTokens(ctx context.Context, userID uuid.UUID) error {
	query := `
		UPDATE refresh_tokens 
		SET revoked_at = NOW() 
		WHERE user_id = $1 AND revoked_at IS NULL
	`
	_, err := r.pool.Exec(ctx, query, userID)
	return err
}

// CleanupExpiredTokens removes expired refresh tokens from the database.
// This should be called periodically (e.g., via a cron job).
func (r *PostgresRepository) CleanupExpiredTokens(ctx context.Context) (int64, error) {
	query := `
		DELETE FROM refresh_tokens 
		WHERE expires_at < NOW() OR revoked_at IS NOT NULL
	`
	result, err := r.pool.Exec(ctx, query)
	if err != nil {
		return 0, err
	}
	return result.RowsAffected(), nil
}
