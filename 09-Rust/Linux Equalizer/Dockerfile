# =============================================================================
# Linux Equalizer - Multi-stage Dockerfile
# A system-wide audio equalizer for Linux written in Rust
# =============================================================================
# NOTE: This is a desktop audio application requiring host audio system access.
# Run with appropriate audio device mappings (see docker-compose.yml).
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Environment
# -----------------------------------------------------------------------------
FROM rust:1.83-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgtk-4-dev \
    pkg-config \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy manifests first for dependency caching
COPY Cargo.toml Cargo.lock* ./

# Create dummy src for dependency compilation
RUN mkdir src && \
    echo "fn main() {}" > src/main.rs

# Build dependencies only (for layer caching)
RUN cargo build --release && \
    rm -rf src target/release/deps/linux_equalizer*

# Copy actual source code
COPY src/ ./src/

# Build the application
RUN cargo build --release && \
    strip target/release/linux-equalizer

# -----------------------------------------------------------------------------
# Stage 2: Runtime Environment
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Labels for container metadata
LABEL org.opencontainers.image.title="Linux Equalizer" \
      org.opencontainers.image.description="System-wide audio equalizer for Linux with GTK4 GUI" \
      org.opencontainers.image.version="1.0.0" \
      org.opencontainers.image.authors="Robert Marshall" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/yourusername/linux-equalizer"

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    # GTK4 runtime
    libgtk-4-1 \
    # PulseAudio client libraries and tools
    pulseaudio-utils \
    libpulse0 \
    # LADSPA plugins for audio processing
    swh-plugins \
    ladspa-sdk \
    # D-Bus for GTK
    dbus \
    # Fonts and icons for GUI
    fonts-dejavu-core \
    adwaita-icon-theme \
    # Cleanup
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for security
RUN groupadd --gid 1000 equalizer && \
    useradd --uid 1000 --gid equalizer --shell /bin/bash --create-home equalizer

# Create necessary directories
RUN mkdir -p /home/equalizer/.config/linux-equalizer && \
    chown -R equalizer:equalizer /home/equalizer

# Copy the built binary from builder stage
COPY --from=builder /app/target/release/linux-equalizer /usr/local/bin/linux-equalizer

# Ensure binary is executable
RUN chmod +x /usr/local/bin/linux-equalizer

# Switch to non-root user
USER equalizer
WORKDIR /home/equalizer

# Set environment variables
ENV HOME=/home/equalizer \
    XDG_CONFIG_HOME=/home/equalizer/.config \
    XDG_RUNTIME_DIR=/run/user/1000 \
    GTK_THEME=Adwaita \
    # PulseAudio connection (host's PulseAudio server)
    PULSE_SERVER=unix:/run/user/1000/pulse/native

# Volume for persistent configuration
VOLUME ["/home/equalizer/.config/linux-equalizer"]

# Default command: headless mode for containerized usage
# Use --headless for daemon mode, or remove for GUI mode
ENTRYPOINT ["linux-equalizer"]
CMD ["--headless"]

# -----------------------------------------------------------------------------
# Stage 3: Development Environment (optional)
# -----------------------------------------------------------------------------
FROM rust:1.83-bookworm AS development

# Install all dependencies for development
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgtk-4-dev \
    pkg-config \
    build-essential \
    swh-plugins \
    ladspa-sdk \
    pulseaudio-utils \
    libpulse-dev \
    dbus \
    fonts-dejavu-core \
    adwaita-icon-theme \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd --gid 1000 developer && \
    useradd --uid 1000 --gid developer --shell /bin/bash --create-home developer

# Set up Rust tooling for user
USER developer
WORKDIR /home/developer/app

# Install additional Rust tools
RUN rustup component add rustfmt clippy

# Environment for development
ENV CARGO_HOME=/home/developer/.cargo \
    RUSTUP_HOME=/home/developer/.rustup \
    PATH="/home/developer/.cargo/bin:${PATH}"

# Default command for development
CMD ["cargo", "build", "--release"]
