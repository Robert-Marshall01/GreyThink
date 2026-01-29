// Package users provides user management functionality.
package users

import (
	"context"
	"errors"
	"strings"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

// Repository defines the interface for user data persistence.
type Repository interface {
	Create(ctx context.Context, user *User) error
	GetByID(ctx context.Context, id uuid.UUID) (*User, error)
	GetByEmail(ctx context.Context, orgID uuid.UUID, email string) (*User, error)
	GetByEmailAnyOrg(ctx context.Context, email string) (*User, error)
	Update(ctx context.Context, user *User) error
	Delete(ctx context.Context, id uuid.UUID) error
	ListByOrganization(ctx context.Context, orgID uuid.UUID, limit, offset int) ([]*User, int, error)
}

// PostgresRepository implements the Repository interface using PostgreSQL.
type PostgresRepository struct {
	pool *pgxpool.Pool
}

// NewPostgresRepository creates a new PostgreSQL user repository.
func NewPostgresRepository(pool *pgxpool.Pool) *PostgresRepository {
	return &PostgresRepository{pool: pool}
}

// Create inserts a new user into the database.
func (r *PostgresRepository) Create(ctx context.Context, user *User) error {
	query := `
		INSERT INTO users (id, organization_id, email, name, password_hash)
		VALUES ($1, $2, $3, $4, $5)
		RETURNING created_at, updated_at
	`
	user.ID = uuid.New()
	err := r.pool.QueryRow(
		ctx, query,
		user.ID, user.OrganizationID, user.Email, user.Name, user.PasswordHash,
	).Scan(&user.CreatedAt, &user.UpdatedAt)

	if err != nil {
		// Check for unique constraint violation
		if isUniqueViolation(err) {
			return ErrUserEmailExists
		}
		return err
	}
	return nil
}

// GetByID retrieves a user by their ID.
func (r *PostgresRepository) GetByID(ctx context.Context, id uuid.UUID) (*User, error) {
	query := `
		SELECT id, organization_id, email, name, password_hash, created_at, updated_at
		FROM users
		WHERE id = $1
	`
	user := &User{}
	err := r.pool.QueryRow(ctx, query, id).Scan(
		&user.ID, &user.OrganizationID, &user.Email, &user.Name, &user.PasswordHash,
		&user.CreatedAt, &user.UpdatedAt,
	)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrUserNotFound
		}
		return nil, err
	}
	return user, nil
}

// GetByEmail retrieves a user by their email within a specific organization.
func (r *PostgresRepository) GetByEmail(ctx context.Context, orgID uuid.UUID, email string) (*User, error) {
	query := `
		SELECT id, organization_id, email, name, password_hash, created_at, updated_at
		FROM users
		WHERE organization_id = $1 AND email = $2
	`
	user := &User{}
	err := r.pool.QueryRow(ctx, query, orgID, email).Scan(
		&user.ID, &user.OrganizationID, &user.Email, &user.Name, &user.PasswordHash,
		&user.CreatedAt, &user.UpdatedAt,
	)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrUserNotFound
		}
		return nil, err
	}
	return user, nil
}

// GetByEmailAnyOrg retrieves a user by email across all organizations.
// Used for login when organization is not yet known.
func (r *PostgresRepository) GetByEmailAnyOrg(ctx context.Context, email string) (*User, error) {
	query := `
		SELECT id, organization_id, email, name, password_hash, created_at, updated_at
		FROM users
		WHERE email = $1
		LIMIT 1
	`
	user := &User{}
	err := r.pool.QueryRow(ctx, query, email).Scan(
		&user.ID, &user.OrganizationID, &user.Email, &user.Name, &user.PasswordHash,
		&user.CreatedAt, &user.UpdatedAt,
	)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrUserNotFound
		}
		return nil, err
	}
	return user, nil
}

// Update updates an existing user.
func (r *PostgresRepository) Update(ctx context.Context, user *User) error {
	query := `
		UPDATE users
		SET name = $2, updated_at = NOW()
		WHERE id = $1
		RETURNING updated_at
	`
	err := r.pool.QueryRow(ctx, query, user.ID, user.Name).Scan(&user.UpdatedAt)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return ErrUserNotFound
		}
		return err
	}
	return nil
}

// Delete removes a user from the database.
func (r *PostgresRepository) Delete(ctx context.Context, id uuid.UUID) error {
	query := `DELETE FROM users WHERE id = $1`
	result, err := r.pool.Exec(ctx, query, id)
	if err != nil {
		return err
	}
	if result.RowsAffected() == 0 {
		return ErrUserNotFound
	}
	return nil
}

// ListByOrganization retrieves users for a specific organization with pagination.
func (r *PostgresRepository) ListByOrganization(ctx context.Context, orgID uuid.UUID, limit, offset int) ([]*User, int, error) {
	// Get total count
	countQuery := `SELECT COUNT(*) FROM users WHERE organization_id = $1`
	var total int
	if err := r.pool.QueryRow(ctx, countQuery, orgID).Scan(&total); err != nil {
		return nil, 0, err
	}

	// Get paginated results
	query := `
		SELECT id, organization_id, email, name, password_hash, created_at, updated_at
		FROM users
		WHERE organization_id = $1
		ORDER BY created_at DESC
		LIMIT $2 OFFSET $3
	`
	rows, err := r.pool.Query(ctx, query, orgID, limit, offset)
	if err != nil {
		return nil, 0, err
	}
	defer rows.Close()

	var users []*User
	for rows.Next() {
		user := &User{}
		if err := rows.Scan(
			&user.ID, &user.OrganizationID, &user.Email, &user.Name, &user.PasswordHash,
			&user.CreatedAt, &user.UpdatedAt,
		); err != nil {
			return nil, 0, err
		}
		users = append(users, user)
	}

	return users, total, rows.Err()
}

// isUniqueViolation checks if the error is a PostgreSQL unique constraint violation.
func isUniqueViolation(err error) bool {
	if err == nil {
		return false
	}
	errStr := err.Error()
	// PostgreSQL error code 23505 is unique_violation
	return strings.Contains(errStr, "23505") || strings.Contains(errStr, "unique constraint") || strings.Contains(errStr, "duplicate key")
}
