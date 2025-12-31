# Dockerfile for MySQL Business Intelligence Database
# Uses official MySQL image with initialization scripts

FROM mysql:8.0-debian

# Set labels for better maintainability
LABEL maintainer="biz_intelli"
LABEL description="MySQL database for Business Intelligence project"
LABEL version="1.0"

# Environment variables (can be overridden at runtime)
ENV MYSQL_DATABASE=biz_intelli_db
ENV MYSQL_USER=bi_user
ENV MYSQL_PASSWORD=bi_password
ENV MYSQL_ROOT_PASSWORD=root_password

# Copy SQL initialization scripts
# MySQL will auto-execute .sql files in /docker-entrypoint-initdb.d/ on first startup
COPY biz_intelli.sql /docker-entrypoint-initdb.d/01-init.sql

# Expose MySQL default port
EXPOSE 3306

# Health check for container orchestration
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD mysqladmin ping -h localhost -u root -p${MYSQL_ROOT_PASSWORD} || exit 1

# Use default MySQL entrypoint and command
# The base image handles running mysqld properly
