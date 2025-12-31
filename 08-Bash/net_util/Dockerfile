# Dockerfile for net_util - Network Utility Script
# Production-grade container for running network diagnostic commands

FROM alpine:3.19

LABEL maintainer="Robert Marshall"
LABEL description="Network utility script for educational purposes"
LABEL version="1.0"

# Install required network utilities and create non-root user
# Using alpine for minimal image size
RUN apk add --no-cache \
    bash \
    iproute2 \
    iputils-ping \
    bind-tools \
    traceroute \
    && rm -rf /var/cache/apk/* \
    && addgroup -S netutil && adduser -S netutil -G netutil

# Set working directory
WORKDIR /app

# Copy the script and make executable
COPY --chown=netutil:netutil --chmod=755 net_util.sh /app/net_util.sh

# Switch to non-root user
USER netutil

# Set default shell to bash
SHELL ["/bin/bash", "-c"]

# Default command - run the network utility script
ENTRYPOINT ["/bin/bash", "/app/net_util.sh"]
