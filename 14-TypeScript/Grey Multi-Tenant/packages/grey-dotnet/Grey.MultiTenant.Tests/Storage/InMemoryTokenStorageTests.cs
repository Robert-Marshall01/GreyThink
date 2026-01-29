using FluentAssertions;
using Grey.MultiTenant.Http;
using Xunit;

namespace Grey.MultiTenant.Tests.Storage;

public class InMemoryTokenStorageTests
{
    [Fact]
    public async Task SetAndGetAccessToken_WorksCorrectly()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();

        // Act
        await storage.SetAccessTokenAsync("test-token");
        var result = await storage.GetAccessTokenAsync();

        // Assert
        result.Should().Be("test-token");
    }

    [Fact]
    public async Task SetAndGetRefreshToken_WorksCorrectly()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();

        // Act
        await storage.SetRefreshTokenAsync("refresh-token");
        var result = await storage.GetRefreshTokenAsync();

        // Assert
        result.Should().Be("refresh-token");
    }

    [Fact]
    public async Task ClearAccessToken_RemovesToken()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();
        await storage.SetAccessTokenAsync("test-token");

        // Act
        await storage.ClearAccessTokenAsync();
        var result = await storage.GetAccessTokenAsync();

        // Assert
        result.Should().BeNull();
    }

    [Fact]
    public async Task ClearAllAsync_RemovesBothTokens()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();
        await storage.SetAccessTokenAsync("access-token");
        await storage.SetRefreshTokenAsync("refresh-token");

        // Act
        await storage.ClearAllAsync();

        // Assert
        (await storage.GetAccessTokenAsync()).Should().BeNull();
        (await storage.GetRefreshTokenAsync()).Should().BeNull();
    }

    [Fact]
    public async Task HasTokensAsync_ReturnsTrueWhenAccessTokenExists()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();
        await storage.SetAccessTokenAsync("test-token");

        // Act
        var result = await storage.HasTokensAsync();

        // Assert
        result.Should().BeTrue();
    }

    [Fact]
    public async Task HasTokensAsync_ReturnsFalseWhenNoTokens()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();

        // Act
        var result = await storage.HasTokensAsync();

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public async Task ConcurrentAccess_IsThreadSafe()
    {
        // Arrange
        var storage = new InMemoryTokenStorage();
        var tasks = new List<Task>();

        // Act - Multiple concurrent writes
        for (int i = 0; i < 100; i++)
        {
            var token = $"token-{i}";
            tasks.Add(Task.Run(async () =>
            {
                await storage.SetAccessTokenAsync(token);
                await storage.GetAccessTokenAsync();
            }));
        }

        // Assert - No exceptions should be thrown
        await Task.WhenAll(tasks);
    }
}
