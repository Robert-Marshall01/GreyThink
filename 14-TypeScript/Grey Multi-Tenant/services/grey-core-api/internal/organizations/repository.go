// Package organizations provides organization (tenant) management functionality.
package organizations

import (
	"context"
	"errors"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

// Repository defines the interface for organization data persistence.
type Repository interface {
	Create(ctx context.Context, org *Organization) error
	GetByID(ctx context.Context, id uuid.UUID) (*Organization, error)
	Update(ctx context.Context, org *Organization) error
	Delete(ctx context.Context, id uuid.UUID) error
	List(ctx context.Context, limit, offset int) ([]*Organization, int, error)
}

// PostgresRepository implements the Repository interface using PostgreSQL.
type PostgresRepository struct {
	pool *pgxpool.Pool
}

// NewPostgresRepository creates a new PostgreSQL organization repository.
func NewPostgresRepository(pool *pgxpool.Pool) *PostgresRepository {
	return &PostgresRepository{pool: pool}
}

// Create inserts a new organization into the database.
func (r *PostgresRepository) Create(ctx context.Context, org *Organization) error {
	query := `
		INSERT INTO organizations (id, name)
		VALUES ($1, $2)
		RETURNING created_at, updated_at
	`
	org.ID = uuid.New()
	err := r.pool.QueryRow(ctx, query, org.ID, org.Name).
		Scan(&org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return err
	}
	return nil
}

// GetByID retrieves an organization by its ID.
func (r *PostgresRepository) GetByID(ctx context.Context, id uuid.UUID) (*Organization, error) {
	query := `
		SELECT id, name, created_at, updated_at
		FROM organizations
		WHERE id = $1
	`
	org := &Organization{}
	err := r.pool.QueryRow(ctx, query, id).Scan(
		&org.ID, &org.Name, &org.CreatedAt, &org.UpdatedAt,
	)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrOrganizationNotFound
		}
		return nil, err
	}
	return org, nil
}

// Update updates an existing organization.
func (r *PostgresRepository) Update(ctx context.Context, org *Organization) error {
	query := `
		UPDATE organizations
		SET name = $2, updated_at = NOW()
		WHERE id = $1
		RETURNING updated_at
	`
	err := r.pool.QueryRow(ctx, query, org.ID, org.Name).Scan(&org.UpdatedAt)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return ErrOrganizationNotFound
		}
		return err
	}
	return nil
}

// Delete removes an organization from the database.
func (r *PostgresRepository) Delete(ctx context.Context, id uuid.UUID) error {
	query := `DELETE FROM organizations WHERE id = $1`
	result, err := r.pool.Exec(ctx, query, id)
	if err != nil {
		return err
	}
	if result.RowsAffected() == 0 {
		return ErrOrganizationNotFound
	}
	return nil
}

// List retrieves organizations with pagination.
func (r *PostgresRepository) List(ctx context.Context, limit, offset int) ([]*Organization, int, error) {
	// Get total count
	countQuery := `SELECT COUNT(*) FROM organizations`
	var total int
	if err := r.pool.QueryRow(ctx, countQuery).Scan(&total); err != nil {
		return nil, 0, err
	}

	// Get paginated results
	query := `
		SELECT id, name, created_at, updated_at
		FROM organizations
		ORDER BY created_at DESC
		LIMIT $1 OFFSET $2
	`
	rows, err := r.pool.Query(ctx, query, limit, offset)
	if err != nil {
		return nil, 0, err
	}
	defer rows.Close()

	var orgs []*Organization
	for rows.Next() {
		org := &Organization{}
		if err := rows.Scan(
			&org.ID, &org.Name, &org.CreatedAt, &org.UpdatedAt,
		); err != nil {
			return nil, 0, err
		}
		orgs = append(orgs, org)
	}

	return orgs, total, rows.Err()
}
