using Grey.MultiTenant.Blazor;
using Grey.MultiTenant.Core.Abstractions;
using Grey.MultiTenant.Http;
using Grey.MultiTenant.Maui.Services;
using Microsoft.Extensions.DependencyInjection;

namespace Grey.MultiTenant.Maui;

/// <summary>
/// Extension methods for registering Grey Multi-Tenant MAUI services.
/// </summary>
public static class GreyMauiExtensions
{
    /// <summary>
    /// Adds Grey Multi-Tenant services with MAUI SecureStorage for token persistence.
    /// This is the recommended method for MAUI apps.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenantMaui(
        this IServiceCollection services,
        Action<GreyMultiTenantOptions> configureOptions)
    {
        // Register SecureTokenStorage as the token storage implementation
        services.AddSingleton<ITokenStorage, SecureTokenStorage>();

        // Add HTTP services (will use the registered ITokenStorage)
        services.AddGreyMultiTenant(configureOptions);

        // Add Blazor stores for MAUI Blazor Hybrid apps
        services.AddGreyMultiTenantBlazor();

        return services;
    }

    /// <summary>
    /// Adds only the MAUI-specific services without Blazor stores.
    /// Use this if you're building a native MAUI app without Blazor.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenantMauiNative(
        this IServiceCollection services,
        Action<GreyMultiTenantOptions> configureOptions)
    {
        // Register SecureTokenStorage as the token storage implementation
        services.AddSingleton<ITokenStorage, SecureTokenStorage>();

        // Add HTTP services only
        services.AddGreyMultiTenant(configureOptions);

        return services;
    }
}
