#--------------------------------------------------------------
# Dockerfile for Digital Organism: Compliance-as-Code
# compliance-as-code | digital organism metaphor
#
# This container packages Terraform with PostgreSQL client
# to run compliance infrastructure as code.
#
# Build: docker build -t compliance-terraform .
# Run:   docker run --rm -e TF_VAR_postgres_password=xxx compliance-terraform plan
#--------------------------------------------------------------

FROM hashicorp/terraform:1.5.7 AS terraform

#--------------------------------------------------------------
# Production Image
# digital organism metaphor: The organism's nervous system
#--------------------------------------------------------------
FROM alpine:3.19

LABEL maintainer="DevSecOps Team"
LABEL description="Terraform runner for Compliance-as-Code digital organism"
LABEL org.opencontainers.image.source="https://github.com/your-org/compliance-as-code"

# Install PostgreSQL client, create user, and copy Terraform in single layer
RUN apk add --no-cache \
    postgresql16-client \
    bash \
    curl \
    jq \
    ca-certificates \
    && rm -rf /var/cache/apk/* \
    && addgroup -g 1000 terraform \
    && adduser -u 1000 -G terraform -s /bin/bash -D terraform

# Copy Terraform binary from official image
COPY --from=terraform /bin/terraform /usr/local/bin/terraform

# Verify installations
RUN terraform version && psql --version

# Set working directory
WORKDIR /workspace

# Copy Terraform configuration files
COPY --chown=terraform:terraform *.tf ./
COPY --chown=terraform:terraform modules/ ./modules/
COPY --chown=terraform:terraform sql/ ./sql/

# Switch to non-root user
USER terraform

# Initialize Terraform (download providers)
RUN terraform init -backend=false

# Default entrypoint
ENTRYPOINT ["terraform"]

# Default command (can be overridden)
CMD ["--help"]
