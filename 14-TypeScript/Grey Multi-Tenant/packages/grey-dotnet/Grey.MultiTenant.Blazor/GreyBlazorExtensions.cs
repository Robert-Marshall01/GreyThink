using Grey.MultiTenant.Blazor.Stores;
using Grey.MultiTenant.Http;
using Microsoft.Extensions.DependencyInjection;

namespace Grey.MultiTenant.Blazor;

/// <summary>
/// Extension methods for registering Grey Multi-Tenant Blazor services.
/// </summary>
public static class GreyBlazorExtensions
{
    /// <summary>
    /// Adds Grey Multi-Tenant Blazor stores to the service collection.
    /// This should be called after AddGreyMultiTenant().
    /// </summary>
    public static IServiceCollection AddGreyMultiTenantBlazor(this IServiceCollection services)
    {
        // Register stores as scoped (per-circuit in Blazor Server, per-component in Blazor WebAssembly)
        services.AddScoped<AuthStore>();
        services.AddScoped<UserStore>();
        services.AddScoped<ProjectsStore>();
        services.AddScoped<QueryStore>();
        services.AddScoped<MutationStore>();

        return services;
    }

    /// <summary>
    /// Adds Grey Multi-Tenant HTTP services and Blazor stores in one call.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenantWithBlazor(
        this IServiceCollection services,
        Action<GreyMultiTenantOptions> configureOptions)
    {
        services.AddGreyMultiTenant(configureOptions);
        services.AddGreyMultiTenantBlazor();
        return services;
    }
}
