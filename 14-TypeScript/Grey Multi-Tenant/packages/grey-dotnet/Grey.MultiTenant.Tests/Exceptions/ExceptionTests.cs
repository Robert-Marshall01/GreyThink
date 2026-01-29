using FluentAssertions;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Xunit;

namespace Grey.MultiTenant.Tests.Exceptions;

public class ExceptionTests
{
    [Fact]
    public void ApiException_HasCorrectProperties()
    {
        // Arrange & Act
        var exception = new ApiException("Test error", System.Net.HttpStatusCode.BadRequest, "ERR_001", "raw response", "trace-123");

        // Assert
        exception.Message.Should().Be("Test error");
        exception.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);
        exception.ErrorCode.Should().Be("ERR_001");
        exception.RawResponse.Should().Be("raw response");
        exception.TraceId.Should().Be("trace-123");
        exception.IsClientError.Should().BeTrue();
        exception.IsServerError.Should().BeFalse();
    }

    [Fact]
    public void AuthException_HasCorrectErrorType()
    {
        // Arrange & Act
        var exception = new AuthException("Token expired", AuthErrorType.TokenExpired);

        // Assert
        exception.Message.Should().Be("Token expired");
        exception.ErrorType.Should().Be(AuthErrorType.TokenExpired);
        exception.IsTokenExpired.Should().BeTrue();
        exception.NeedsReauthentication.Should().BeTrue();
        exception.StatusCode.Should().Be(System.Net.HttpStatusCode.Unauthorized);
    }

    [Fact]
    public void ValidationException_ContainsValidationErrors()
    {
        // Arrange
        var errors = new List<ValidationError>
        {
            new() { Field = "email", Message = "Email is required" },
            new() { Field = "password", Message = "Password is too short" }
        };

        // Act
        var exception = new ValidationException("Validation failed", errors);

        // Assert
        exception.Message.Should().Be("Validation failed");
        exception.ValidationErrors.Should().HaveCount(2);
        exception.HasErrorForField("email").Should().BeTrue();
        exception.GetErrorsForField("email").Should().HaveCount(1);
    }

    [Fact]
    public void NetworkException_HasCorrectTimeoutState()
    {
        // Arrange & Act
        var exception = new NetworkException("Request timed out", isTimeout: true, isOffline: false);

        // Assert
        exception.IsTimeout.Should().BeTrue();
        exception.IsOffline.Should().BeFalse();
        exception.IsRetryable.Should().BeTrue();
    }

    [Fact]
    public void NetworkException_HasCorrectOfflineState()
    {
        // Arrange & Act
        var exception = new NetworkException("No network connection", isTimeout: false, isOffline: true);

        // Assert
        exception.IsTimeout.Should().BeFalse();
        exception.IsOffline.Should().BeTrue();
        exception.IsRetryable.Should().BeFalse();
    }
}
