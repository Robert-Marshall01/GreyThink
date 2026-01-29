// Package db provides database connection and migration utilities.
package db

import (
	"context"
	"fmt"

	"github.com/grey-platform/grey-core-api/internal/config"
	"github.com/jackc/pgx/v5/pgxpool"
)

// Database wraps the connection pool and provides database operations.
type Database struct {
	Pool *pgxpool.Pool
}

// New creates a new database connection pool.
func New(ctx context.Context, cfg config.DatabaseConfig) (*Database, error) {
	poolConfig, err := pgxpool.ParseConfig(cfg.URL)
	if err != nil {
		return nil, fmt.Errorf("parsing database URL: %w", err)
	}

	poolConfig.MaxConns = cfg.MaxConnections
	poolConfig.MinConns = cfg.MinConnections

	pool, err := pgxpool.NewWithConfig(ctx, poolConfig)
	if err != nil {
		return nil, fmt.Errorf("creating connection pool: %w", err)
	}

	// Verify connection
	if err := pool.Ping(ctx); err != nil {
		pool.Close()
		return nil, fmt.Errorf("pinging database: %w", err)
	}

	return &Database{Pool: pool}, nil
}

// Close closes the database connection pool.
func (db *Database) Close() {
	db.Pool.Close()
}

// HealthCheck verifies the database connection is alive.
func (db *Database) HealthCheck(ctx context.Context) error {
	return db.Pool.Ping(ctx)
}
