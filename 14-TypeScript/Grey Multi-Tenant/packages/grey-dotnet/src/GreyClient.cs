// =============================================================================
// Grey Multi-Tenant SDK - .NET
// GreyClient.cs
//
// Façade over all Grey services.
// Single entry point for consuming applications.
// =============================================================================

using Grey.Sdk.Services;

namespace Grey.Sdk;

/// <summary>
/// Main client façade for the Grey Multi-Tenant SDK.
/// Provides access to all domain services.
/// </summary>
public sealed class GreyClient
{
    /// <summary>
    /// Authentication service for login, logout, and token management.
    /// </summary>
    public AuthService Auth { get; }

    /// <summary>
    /// User service for fetching user data.
    /// </summary>
    public UserService User { get; }

    /// <summary>
    /// Projects service for listing and creating projects.
    /// </summary>
    public ProjectsService Projects { get; }

    /// <summary>
    /// Generic query service for custom endpoints.
    /// </summary>
    public QueryService Query { get; }

    /// <summary>
    /// Generic mutation service for custom endpoints.
    /// </summary>
    public MutationService Mutation { get; }

    /// <summary>
    /// Creates a new GreyClient.
    /// </summary>
    public GreyClient(
        AuthService auth,
        UserService user,
        ProjectsService projects,
        QueryService query,
        MutationService mutation)
    {
        Auth = auth ?? throw new ArgumentNullException(nameof(auth));
        User = user ?? throw new ArgumentNullException(nameof(user));
        Projects = projects ?? throw new ArgumentNullException(nameof(projects));
        Query = query ?? throw new ArgumentNullException(nameof(query));
        Mutation = mutation ?? throw new ArgumentNullException(nameof(mutation));
    }
}
