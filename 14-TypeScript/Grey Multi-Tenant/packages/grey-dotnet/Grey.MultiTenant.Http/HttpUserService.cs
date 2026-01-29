using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.User;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// HTTP-based implementation of the user service.
/// </summary>
public sealed class HttpUserService : IUserService
{
    private readonly HttpClient _httpClient;
    private readonly GreyMultiTenantOptions _options;
    private readonly JsonSerializerOptions _jsonOptions;

    public HttpUserService(
        HttpClient httpClient,
        IOptions<GreyMultiTenantOptions> options)
    {
        _httpClient = httpClient;
        _options = options.Value;
        _jsonOptions = new JsonSerializerOptions
        {
            PropertyNameCaseInsensitive = true,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
    }

    public async Task<UserModel> GetUserAsync(CancellationToken cancellationToken = default)
    {
        try
        {
            var response = await _httpClient.GetAsync("/users/me", cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<UserModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid user response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<UserModel> GetUserByIdAsync(string userId, CancellationToken cancellationToken = default)
    {
        try
        {
            var response = await _httpClient.GetAsync($"/users/{Uri.EscapeDataString(userId)}", cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<UserModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid user response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<UserModel> UpdateUserAsync(UpdateUserRequest request, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PatchAsync("/users/me", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<UserModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid user response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }

    public async Task<UserModel> UpdateUserAsync(string userId, UpdateUserRequest request, CancellationToken cancellationToken = default)
    {
        try
        {
            var content = new StringContent(
                JsonSerializer.Serialize(request, _jsonOptions),
                Encoding.UTF8,
                "application/json");

            var response = await _httpClient.PatchAsync($"/users/{Uri.EscapeDataString(userId)}", content, cancellationToken);
            await ErrorHandler.EnsureSuccessAsync(response, cancellationToken);

            return await response.Content.ReadFromJsonAsync<UserModel>(_jsonOptions, cancellationToken)
                ?? throw new ApiException("Invalid user response");
        }
        catch (Exception ex) when (ex is not ApiException and not AuthException and not ValidationException)
        {
            throw ErrorHandler.WrapException(ex);
        }
    }
}
