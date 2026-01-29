// Package projects provides project management functionality.
package projects

import (
	"context"
	"errors"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jackc/pgx/v5/pgxpool"
)

// Repository defines the interface for project data persistence.
type Repository interface {
	Create(ctx context.Context, project *Project) error
	GetByID(ctx context.Context, id uuid.UUID) (*Project, error)
	GetByIDAndOrg(ctx context.Context, id, orgID uuid.UUID) (*Project, error)
	Update(ctx context.Context, project *Project) error
	Delete(ctx context.Context, id uuid.UUID) error
	ListByOrganization(ctx context.Context, orgID uuid.UUID, limit, offset int) ([]*Project, int, error)
}

// PostgresRepository implements the Repository interface using PostgreSQL.
type PostgresRepository struct {
	pool *pgxpool.Pool
}

// NewPostgresRepository creates a new PostgreSQL project repository.
func NewPostgresRepository(pool *pgxpool.Pool) *PostgresRepository {
	return &PostgresRepository{pool: pool}
}

// Create inserts a new project into the database.
func (r *PostgresRepository) Create(ctx context.Context, project *Project) error {
	query := `
		INSERT INTO projects (id, organization_id, name, description)
		VALUES ($1, $2, $3, $4)
		RETURNING created_at, updated_at
	`
	project.ID = uuid.New()
	err := r.pool.QueryRow(
		ctx, query,
		project.ID, project.OrganizationID, project.Name, project.Description,
	).Scan(&project.CreatedAt, &project.UpdatedAt)
	if err != nil {
		return err
	}
	return nil
}

// GetByID retrieves a project by its ID.
func (r *PostgresRepository) GetByID(ctx context.Context, id uuid.UUID) (*Project, error) {
	query := `
		SELECT id, organization_id, name, description, created_at, updated_at
		FROM projects
		WHERE id = $1
	`
	project := &Project{}
	err := r.pool.QueryRow(ctx, query, id).Scan(
		&project.ID, &project.OrganizationID, &project.Name,
		&project.Description, &project.CreatedAt, &project.UpdatedAt,
	)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrProjectNotFound
		}
		return nil, err
	}
	return project, nil
}

// GetByIDAndOrg retrieves a project by its ID and organization ID.
// This enforces multi-tenant isolation.
func (r *PostgresRepository) GetByIDAndOrg(ctx context.Context, id, orgID uuid.UUID) (*Project, error) {
	query := `
		SELECT id, organization_id, name, description, created_at, updated_at
		FROM projects
		WHERE id = $1 AND organization_id = $2
	`
	project := &Project{}
	err := r.pool.QueryRow(ctx, query, id, orgID).Scan(
		&project.ID, &project.OrganizationID, &project.Name,
		&project.Description, &project.CreatedAt, &project.UpdatedAt,
	)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return nil, ErrProjectNotFound
		}
		return nil, err
	}
	return project, nil
}

// Update updates an existing project.
func (r *PostgresRepository) Update(ctx context.Context, project *Project) error {
	query := `
		UPDATE projects
		SET name = $2, description = $3, updated_at = NOW()
		WHERE id = $1
		RETURNING updated_at
	`
	err := r.pool.QueryRow(
		ctx, query,
		project.ID, project.Name, project.Description,
	).Scan(&project.UpdatedAt)
	if err != nil {
		if errors.Is(err, pgx.ErrNoRows) {
			return ErrProjectNotFound
		}
		return err
	}
	return nil
}

// Delete removes a project from the database.
func (r *PostgresRepository) Delete(ctx context.Context, id uuid.UUID) error {
	query := `DELETE FROM projects WHERE id = $1`
	result, err := r.pool.Exec(ctx, query, id)
	if err != nil {
		return err
	}
	if result.RowsAffected() == 0 {
		return ErrProjectNotFound
	}
	return nil
}

// ListByOrganization retrieves projects for a specific organization with pagination.
func (r *PostgresRepository) ListByOrganization(ctx context.Context, orgID uuid.UUID, limit, offset int) ([]*Project, int, error) {
	// Get total count
	countQuery := `SELECT COUNT(*) FROM projects WHERE organization_id = $1`
	var total int
	if err := r.pool.QueryRow(ctx, countQuery, orgID).Scan(&total); err != nil {
		return nil, 0, err
	}

	// Get paginated results
	query := `
		SELECT id, organization_id, name, description, created_at, updated_at
		FROM projects
		WHERE organization_id = $1
		ORDER BY created_at DESC
		LIMIT $2 OFFSET $3
	`
	rows, err := r.pool.Query(ctx, query, orgID, limit, offset)
	if err != nil {
		return nil, 0, err
	}
	defer rows.Close()

	var projects []*Project
	for rows.Next() {
		project := &Project{}
		if err := rows.Scan(
			&project.ID, &project.OrganizationID, &project.Name,
			&project.Description, &project.CreatedAt, &project.UpdatedAt,
		); err != nil {
			return nil, 0, err
		}
		projects = append(projects, project)
	}

	return projects, total, rows.Err()
}
