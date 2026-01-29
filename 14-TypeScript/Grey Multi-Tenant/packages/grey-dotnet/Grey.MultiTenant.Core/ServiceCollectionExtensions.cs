using Microsoft.Extensions.DependencyInjection;

namespace Grey.MultiTenant.Core;

/// <summary>
/// Extension methods for registering Grey Multi-Tenant Core services.
/// </summary>
public static class ServiceCollectionExtensions
{
    /// <summary>
    /// Adds Grey Multi-Tenant Core abstractions to the service collection.
    /// This only registers the interfaces; implementations must be added separately.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenantCore(this IServiceCollection services)
    {
        // Core only contains abstractions and models
        // Implementations are provided by Grey.MultiTenant.Http
        return services;
    }
}
