"""Initial schema - users, uploads, metrics, ai_outputs

Revision ID: 001_initial
Revises: 
Create Date: 2026-01-30

This migration creates the initial database schema for Grey AI Internal.

Tables:
    - users: Optional user tracking for sessions/audit
    - uploads: File upload metadata
    - metrics: Parsed data metrics from uploads
    - ai_outputs: AI-generated insights and recommendations

Relationships:
    User (1) → (n) Upload (1) → (1) Metrics
                              → (1) AIOutput
"""
from typing import Sequence, Union

from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision: str = '001_initial'
down_revision: Union[str, None] = None
branch_labels: Union[str, Sequence[str], None] = None
depends_on: Union[str, Sequence[str], None] = None


def upgrade() -> None:
    """Create initial database schema."""
    
    # =========================================================================
    # Users Table (Optional - for session tracking)
    # =========================================================================
    op.create_table(
        'users',
        sa.Column('id', sa.Integer(), autoincrement=True, nullable=False),
        sa.Column('username', sa.String(50), nullable=False, comment='Unique username for login'),
        sa.Column('email', sa.String(255), nullable=False, comment='User email address'),
        sa.Column('display_name', sa.String(100), nullable=True, comment='Display name for UI'),
        sa.Column('is_active', sa.Boolean(), default=True, comment='Whether user account is active'),
        sa.Column('created_at', sa.DateTime(timezone=True), nullable=False, comment='Account creation timestamp'),
        sa.Column('last_login_at', sa.DateTime(timezone=True), nullable=True, comment='Last login timestamp'),
        sa.PrimaryKeyConstraint('id'),
    )
    op.create_index('ix_users_username', 'users', ['username'], unique=True)
    op.create_index('ix_users_email', 'users', ['email'], unique=True)
    
    # =========================================================================
    # Uploads Table
    # =========================================================================
    op.create_table(
        'uploads',
        sa.Column('id', sa.Integer(), autoincrement=True, nullable=False),
        sa.Column('user_id', sa.Integer(), nullable=True, comment='User who uploaded the file (optional)'),
        sa.Column('filename', sa.String(255), nullable=False, comment='Original filename as uploaded'),
        sa.Column('file_path', sa.String(500), nullable=False, comment='Storage path on disk/object storage'),
        sa.Column('file_type', sa.String(10), nullable=False, comment='File type: csv, txt, json'),
        sa.Column('file_size', sa.Integer(), nullable=False, comment='File size in bytes'),
        sa.Column('mime_type', sa.String(100), nullable=True, comment='MIME type from upload'),
        sa.Column('status', sa.String(20), default='uploaded', comment='Status: uploaded, parsing, parsed, analyzing, analyzed, failed'),
        sa.Column('error_message', sa.Text(), nullable=True, comment='Error details if status is failed'),
        sa.Column('created_at', sa.DateTime(timezone=True), nullable=False, comment='Upload timestamp'),
        sa.ForeignKeyConstraint(['user_id'], ['users.id'], ondelete='SET NULL'),
        sa.PrimaryKeyConstraint('id'),
    )
    op.create_index('ix_uploads_user_id', 'uploads', ['user_id'])
    op.create_index('ix_uploads_status', 'uploads', ['status'])
    op.create_index('ix_uploads_created_at', 'uploads', ['created_at'])
    op.create_index('ix_uploads_status_created', 'uploads', ['status', 'created_at'])
    
    # =========================================================================
    # Metrics Table
    # =========================================================================
    op.create_table(
        'metrics',
        sa.Column('id', sa.Integer(), autoincrement=True, nullable=False),
        sa.Column('upload_id', sa.Integer(), nullable=False, comment='Associated upload'),
        sa.Column('row_count', sa.Integer(), nullable=False, comment='Total number of data rows'),
        sa.Column('column_count', sa.Integer(), default=0, comment='Number of columns (0 for text files)'),
        sa.Column('parsed_data', sa.JSON(), nullable=True, comment='Full parsed data structure'),
        sa.Column('columns_json', sa.JSON(), nullable=True, comment='Column metadata: name, dtype, null_count, sample_values'),
        sa.Column('stats_json', sa.JSON(), nullable=True, comment='Statistical summaries per column'),
        sa.Column('preview_json', sa.JSON(), nullable=True, comment='First N rows as list of dicts'),
        sa.Column('created_at', sa.DateTime(timezone=True), nullable=False, comment='Parse completion timestamp'),
        sa.ForeignKeyConstraint(['upload_id'], ['uploads.id'], ondelete='CASCADE'),
        sa.PrimaryKeyConstraint('id'),
        sa.UniqueConstraint('upload_id', name='uq_metrics_upload_id'),
    )
    op.create_index('ix_metrics_upload_id', 'metrics', ['upload_id'])
    
    # =========================================================================
    # AI Outputs Table
    # =========================================================================
    op.create_table(
        'ai_outputs',
        sa.Column('id', sa.Integer(), autoincrement=True, nullable=False),
        sa.Column('upload_id', sa.Integer(), nullable=False, comment='Associated upload'),
        sa.Column('ai_summary', sa.Text(), nullable=False, comment='Executive summary of the analysis'),
        sa.Column('ai_recommendations', sa.JSON(), nullable=True, comment='List of actionable recommendations'),
        sa.Column('insights_json', sa.JSON(), nullable=True, comment='Detailed insights as structured list'),
        sa.Column('model_used', sa.String(50), nullable=False, comment='LLM model name (e.g., llama3, mistral)'),
        sa.Column('prompt_tokens', sa.Integer(), nullable=True, comment='Input token count'),
        sa.Column('completion_tokens', sa.Integer(), nullable=True, comment='Output token count'),
        sa.Column('created_at', sa.DateTime(timezone=True), nullable=False, comment='Analysis completion timestamp'),
        sa.ForeignKeyConstraint(['upload_id'], ['uploads.id'], ondelete='CASCADE'),
        sa.PrimaryKeyConstraint('id'),
        sa.UniqueConstraint('upload_id', name='uq_ai_outputs_upload_id'),
    )
    op.create_index('ix_ai_outputs_upload_id', 'ai_outputs', ['upload_id'])


def downgrade() -> None:
    """Drop all tables in reverse order."""
    
    # Drop tables in reverse order (respecting foreign keys)
    op.drop_index('ix_ai_outputs_upload_id', table_name='ai_outputs')
    op.drop_table('ai_outputs')
    
    op.drop_index('ix_metrics_upload_id', table_name='metrics')
    op.drop_table('metrics')
    
    op.drop_index('ix_uploads_status_created', table_name='uploads')
    op.drop_index('ix_uploads_created_at', table_name='uploads')
    op.drop_index('ix_uploads_status', table_name='uploads')
    op.drop_index('ix_uploads_user_id', table_name='uploads')
    op.drop_table('uploads')
    
    op.drop_index('ix_users_email', table_name='users')
    op.drop_index('ix_users_username', table_name='users')
    op.drop_table('users')
