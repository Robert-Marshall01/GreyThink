using Grey.MultiTenant.Core.Abstractions;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Microsoft.Extensions.Options;

namespace Grey.MultiTenant.Http;

/// <summary>
/// Extension methods for registering Grey Multi-Tenant HTTP services.
/// </summary>
public static class GreyHttpClientExtensions
{
    private const string HttpClientName = "GreyMultiTenant";

    /// <summary>
    /// Adds Grey Multi-Tenant HTTP services to the service collection.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenant(
        this IServiceCollection services,
        Action<GreyMultiTenantOptions> configureOptions)
    {
        // Configure options
        services.Configure(configureOptions);

        // Register token storage (can be replaced by caller)
        services.TryAddSingleton<ITokenStorage, InMemoryTokenStorage>();

        // Register the delegating handler
        services.AddTransient<AuthenticatedHttpMessageHandler>();

        // Configure HttpClient
        services.AddHttpClient(HttpClientName, (sp, client) =>
        {
            var options = sp.GetRequiredService<IOptions<GreyMultiTenantOptions>>().Value;
            client.BaseAddress = new Uri(options.BaseUrl.TrimEnd('/') + "/");
            client.Timeout = options.Timeout;
        })
        .AddHttpMessageHandler<AuthenticatedHttpMessageHandler>();

        // Register services
        services.AddScoped<IAuthService>(sp => new HttpAuthService(
            sp.GetRequiredService<IHttpClientFactory>().CreateClient(HttpClientName),
            sp.GetRequiredService<ITokenStorage>(),
            sp.GetRequiredService<IOptions<GreyMultiTenantOptions>>()));

        services.AddScoped<IUserService>(sp => new HttpUserService(
            sp.GetRequiredService<IHttpClientFactory>().CreateClient(HttpClientName),
            sp.GetRequiredService<IOptions<GreyMultiTenantOptions>>()));

        services.AddScoped<IProjectsService>(sp => new HttpProjectsService(
            sp.GetRequiredService<IHttpClientFactory>().CreateClient(HttpClientName),
            sp.GetRequiredService<IOptions<GreyMultiTenantOptions>>()));

        services.AddScoped<IQueryService>(sp => new HttpQueryService(
            sp.GetRequiredService<IHttpClientFactory>().CreateClient(HttpClientName),
            sp.GetRequiredService<IOptions<GreyMultiTenantOptions>>()));

        services.AddScoped<IMutationService>(sp => new HttpMutationService(
            sp.GetRequiredService<IHttpClientFactory>().CreateClient(HttpClientName),
            sp.GetRequiredService<IOptions<GreyMultiTenantOptions>>()));

        return services;
    }

    /// <summary>
    /// Adds Grey Multi-Tenant HTTP services with a custom token storage implementation.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenant<TTokenStorage>(
        this IServiceCollection services,
        Action<GreyMultiTenantOptions> configureOptions)
        where TTokenStorage : class, ITokenStorage
    {
        services.AddSingleton<ITokenStorage, TTokenStorage>();
        return services.AddGreyMultiTenant(configureOptions);
    }

    /// <summary>
    /// Adds Grey Multi-Tenant HTTP services with a token provider for advanced token management.
    /// </summary>
    public static IServiceCollection AddGreyMultiTenant<TTokenProvider>(
        this IServiceCollection services,
        Action<GreyMultiTenantOptions> configureOptions,
        ServiceLifetime tokenProviderLifetime = ServiceLifetime.Scoped)
        where TTokenProvider : class, ITokenProvider
    {
        services.Add(new ServiceDescriptor(typeof(ITokenProvider), typeof(TTokenProvider), tokenProviderLifetime));
        return services.AddGreyMultiTenant(configureOptions);
    }
}
