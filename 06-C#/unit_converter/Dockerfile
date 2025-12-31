# Build stage
FROM mcr.microsoft.com/dotnet/sdk:8.0-alpine AS build
WORKDIR /src

# Copy source files
COPY unit_converter.cs .

# Create a minimal project file for compilation
RUN echo '<Project Sdk="Microsoft.NET.Sdk">\n\
  <PropertyGroup>\n\
    <OutputType>Exe</OutputType>\n\
    <TargetFramework>net8.0</TargetFramework>\n\
    <ImplicitUsings>disable</ImplicitUsings>\n\
    <Nullable>disable</Nullable>\n\
    <PublishSingleFile>true</PublishSingleFile>\n\
    <SelfContained>true</SelfContained>\n\
    <RuntimeIdentifier>linux-musl-x64</RuntimeIdentifier>\n\
    <PublishTrimmed>true</PublishTrimmed>\n\
  </PropertyGroup>\n\
</Project>' > unit_converter.csproj

# Restore and publish
RUN dotnet restore
RUN dotnet publish -c Release -o /app --no-restore

# Runtime stage - use minimal base image
FROM mcr.microsoft.com/dotnet/runtime-deps:8.0-alpine AS runtime
WORKDIR /app

# Create non-root user for security
RUN addgroup -g 1000 appgroup && \
    adduser -u 1000 -G appgroup -D appuser

# Copy published application
COPY --from=build /app .

# Set ownership to non-root user
RUN chown -R appuser:appgroup /app

# Switch to non-root user
USER appuser

# Set entrypoint
ENTRYPOINT ["./unit_converter"]
