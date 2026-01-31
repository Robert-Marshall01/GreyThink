"""
Alembic Environment Configuration
=================================
Configures Alembic to work with async SQLAlchemy.

This file handles:
- Loading database models for autogenerate
- Configuring async database connection
- Running migrations in online/offline modes
"""

import asyncio
import os
import sys
from logging.config import fileConfig

from alembic import context
from sqlalchemy import pool
from sqlalchemy.engine import Connection
from sqlalchemy.ext.asyncio import async_engine_from_config

# Add parent directory to path for model imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import our models - this registers them with Base.metadata
from models.database import Base, User, Upload, Metrics, AIOutput

# Alembic Config object - provides access to .ini file values
config = context.config

# Override sqlalchemy.url from environment variable if set
database_url = os.getenv("DATABASE_URL")
if database_url:
    # Alembic requires sync driver for some operations, but we use async
    # Convert async URL to sync for Alembic if needed
    if "asyncpg" in database_url:
        sync_url = database_url.replace("postgresql+asyncpg", "postgresql")
    elif "aiosqlite" in database_url:
        sync_url = database_url.replace("sqlite+aiosqlite", "sqlite")
    else:
        sync_url = database_url
    config.set_main_option("sqlalchemy.url", database_url)

# Configure Python logging from alembic.ini
if config.config_file_name is not None:
    fileConfig(config.config_file_name)

# Target metadata for autogenerate support
# This is our SQLAlchemy Base.metadata containing all model definitions
target_metadata = Base.metadata


def run_migrations_offline() -> None:
    """
    Run migrations in 'offline' mode.

    This configures the context with just a URL and not an Engine,
    which is useful for generating SQL scripts without database access.
    
    Called when running: alembic upgrade head --sql
    """
    url = config.get_main_option("sqlalchemy.url")
    context.configure(
        url=url,
        target_metadata=target_metadata,
        literal_binds=True,
        dialect_opts={"paramstyle": "named"},
        # Enable comparison of types for autogenerate
        compare_type=True,
        # Enable comparison of server defaults
        compare_server_default=True,
    )

    with context.begin_transaction():
        context.run_migrations()


def do_run_migrations(connection: Connection) -> None:
    """
    Run migrations within a connection context.
    
    This is called by both async and sync migration runners.
    """
    context.configure(
        connection=connection,
        target_metadata=target_metadata,
        # Enable comparison of types for autogenerate
        compare_type=True,
        # Enable comparison of server defaults
        compare_server_default=True,
        # Render item comparisons for autogenerate
        render_as_batch=True,  # Required for SQLite ALTER TABLE support
    )

    with context.begin_transaction():
        context.run_migrations()


async def run_async_migrations() -> None:
    """
    Run migrations using async engine.
    
    This is the standard path for PostgreSQL with asyncpg.
    """
    connectable = async_engine_from_config(
        config.get_section(config.config_ini_section, {}),
        prefix="sqlalchemy.",
        poolclass=pool.NullPool,
    )

    async with connectable.connect() as connection:
        await connection.run_sync(do_run_migrations)

    await connectable.dispose()


def run_migrations_online() -> None:
    """Run migrations in 'online' mode with async support."""
    asyncio.run(run_async_migrations())


# Determine which mode to run
if context.is_offline_mode():
    run_migrations_offline()
else:
    run_migrations_online()
