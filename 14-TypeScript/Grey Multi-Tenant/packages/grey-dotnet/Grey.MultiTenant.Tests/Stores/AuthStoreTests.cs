using FluentAssertions;
using Grey.MultiTenant.Blazor.Stores;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Auth;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Moq;
using Xunit;

namespace Grey.MultiTenant.Tests.Stores;

public class AuthStoreTests
{
    private readonly Mock<IAuthService> _authServiceMock;
    private readonly AuthStore _store;

    public AuthStoreTests()
    {
        _authServiceMock = new Mock<IAuthService>();
        _store = new AuthStore(_authServiceMock.Object);
    }

    [Fact]
    public async Task LoginAsync_WithValidCredentials_SetsAuthenticatedState()
    {
        // Arrange
        var loginResult = new LoginResult
        {
            User = new UserModel { Id = "user-1", Email = "test@example.com" },
            Tokens = new TokenModel { AccessToken = "token" }
        };

        _authServiceMock
            .Setup(x => x.LoginAsync(It.IsAny<LoginRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(loginResult);

        // Act
        var result = await _store.LoginAsync("test@example.com", "password");

        // Assert
        result.Should().BeTrue();
        _store.IsAuthenticated.Should().BeTrue();
        _store.User.Should().NotBeNull();
        _store.User!.Email.Should().Be("test@example.com");
        _store.Error.Should().BeNull();
    }

    [Fact]
    public async Task LoginAsync_WithInvalidCredentials_SetsErrorState()
    {
        // Arrange
        _authServiceMock
            .Setup(x => x.LoginAsync(It.IsAny<LoginRequest>(), It.IsAny<CancellationToken>()))
            .ThrowsAsync(new AuthException("Invalid credentials", AuthErrorType.InvalidCredentials));

        // Act
        var result = await _store.LoginAsync("test@example.com", "wrong-password");

        // Assert
        result.Should().BeFalse();
        _store.IsAuthenticated.Should().BeFalse();
        _store.Error.Should().Be("Invalid credentials");
    }

    [Fact]
    public async Task LogoutAsync_ClearsAuthenticatedState()
    {
        // Arrange - First login
        var loginResult = new LoginResult
        {
            User = new UserModel { Id = "user-1", Email = "test@example.com" },
            Tokens = new TokenModel { AccessToken = "token" }
        };

        _authServiceMock
            .Setup(x => x.LoginAsync(It.IsAny<LoginRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(loginResult);

        await _store.LoginAsync("test@example.com", "password");

        // Act
        await _store.LogoutAsync();

        // Assert
        _store.IsAuthenticated.Should().BeFalse();
        _store.User.Should().BeNull();
        _store.Tokens.Should().BeNull();
    }

    [Fact]
    public async Task LoginAsync_RaisesAuthStateChangedEvent()
    {
        // Arrange
        var loginResult = new LoginResult
        {
            User = new UserModel { Id = "user-1", Email = "test@example.com" },
            Tokens = new TokenModel { AccessToken = "token" }
        };

        _authServiceMock
            .Setup(x => x.LoginAsync(It.IsAny<LoginRequest>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(loginResult);

        AuthStateChangedEventArgs? eventArgs = null;
        _store.AuthStateChanged += (sender, args) => eventArgs = args;

        // Act
        await _store.LoginAsync("test@example.com", "password");

        // Assert
        eventArgs.Should().NotBeNull();
        eventArgs!.IsAuthenticated.Should().BeTrue();
        eventArgs.User.Should().NotBeNull();
    }

    [Fact]
    public void ClearError_ClearsErrorState()
    {
        // Arrange - Simulate an error
        _authServiceMock
            .Setup(x => x.LoginAsync(It.IsAny<LoginRequest>(), It.IsAny<CancellationToken>()))
            .ThrowsAsync(new AuthException("Test error", AuthErrorType.Unknown));

        _ = _store.LoginAsync("test@example.com", "password");

        // Act
        _store.ClearError();

        // Assert
        _store.Error.Should().BeNull();
    }

    [Fact]
    public async Task InitializeAsync_ChecksAuthenticationState()
    {
        // Arrange
        _authServiceMock
            .Setup(x => x.IsAuthenticatedAsync(It.IsAny<CancellationToken>()))
            .ReturnsAsync(true);

        // Act
        await _store.InitializeAsync();

        // Assert
        _store.IsAuthenticated.Should().BeTrue();
        _store.IsLoading.Should().BeFalse();
    }
}
