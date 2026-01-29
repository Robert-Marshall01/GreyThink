using Grey.MultiTenant.Core.Models;
using Grey.MultiTenant.Core.User;

namespace Grey.MultiTenant.Core.Abstractions;

/// <summary>
/// Service interface for user operations.
/// </summary>
public interface IUserService
{
    /// <summary>
    /// Gets the current authenticated user.
    /// </summary>
    Task<UserModel> GetUserAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets a user by their ID.
    /// </summary>
    Task<UserModel> GetUserByIdAsync(string userId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates the current user's profile.
    /// </summary>
    Task<UserModel> UpdateUserAsync(UpdateUserRequest request, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates a specific user's profile.
    /// </summary>
    Task<UserModel> UpdateUserAsync(string userId, UpdateUserRequest request, CancellationToken cancellationToken = default);
}
