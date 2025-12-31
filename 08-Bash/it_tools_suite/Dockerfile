# Dockerfile for IT Tools Suite
# A menu-driven bash script for common IT administrative tasks

FROM ubuntu:22.04

LABEL maintainer="IT Tools Suite"
LABEL description="Interactive IT Tools Suite for system administration tasks"
LABEL version="1.0"

# Avoid prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install required system utilities, create user and directories
RUN apt-get update && apt-get install -y --no-install-recommends \
    bash \
    coreutils \
    procps \
    iproute2 \
    iputils-ping \
    net-tools \
    tar \
    gzip \
    findutils \
    util-linux \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
    && groupadd -r ittools && useradd -r -g ittools -d /home/ittools -s /bin/bash -m ittools \
    && mkdir -p /data/backups /data/logs \
    && chown -R ittools:ittools /data

# Set working directory
WORKDIR /app

# Copy the script
COPY --chown=ittools:ittools it_tools_suite.sh /app/it_tools_suite.sh

# Make the script executable
RUN chmod +x /app/it_tools_suite.sh

# Switch to non-root user
# Note: Some operations in the script require root privileges
# For full functionality, run with --user root or remove this line
USER ittools

# Set environment variables
ENV TERM=xterm-256color
ENV BACKUP_DIR=/data/backups

# Health check (basic bash availability)
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD bash --version || exit 1

# Default command - run the interactive script
CMD ["/bin/bash", "/app/it_tools_suite.sh"]
