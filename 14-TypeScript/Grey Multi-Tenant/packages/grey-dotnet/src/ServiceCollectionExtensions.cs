// =============================================================================
// Grey Multi-Tenant SDK - .NET
// ServiceCollectionExtensions.cs
//
// DI registration extensions for IServiceCollection.
// =============================================================================

using Grey.Sdk.Services;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;

namespace Grey.Sdk;

/// <summary>
/// Extension methods for configuring Grey services in DI.
/// </summary>
public static class ServiceCollectionExtensions
{
    /// <summary>
    /// Adds Grey Multi-Tenant SDK services to the service collection.
    /// </summary>
    /// <param name="services">The service collection.</param>
    /// <param name="configureOptions">Action to configure GreyOptions.</param>
    /// <returns>The service collection for chaining.</returns>
    public static IServiceCollection AddGrey(
        this IServiceCollection services,
        Action<GreyOptions> configureOptions)
    {
        ArgumentNullException.ThrowIfNull(services);
        ArgumentNullException.ThrowIfNull(configureOptions);

        // Configure options
        services.Configure(configureOptions);

        // Register HttpClient with configured timeout
        services.AddHttpClient<AuthService>((sp, client) =>
        {
            var options = sp.GetRequiredService<IOptions<GreyOptions>>().Value;
            client.Timeout = options.Timeout;
            if (!string.IsNullOrEmpty(options.ApiKey))
            {
                client.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
            }
        });

        services.AddHttpClient<UserService>((sp, client) =>
        {
            var options = sp.GetRequiredService<IOptions<GreyOptions>>().Value;
            client.Timeout = options.Timeout;
            if (!string.IsNullOrEmpty(options.ApiKey))
            {
                client.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
            }
        });

        services.AddHttpClient<ProjectsService>((sp, client) =>
        {
            var options = sp.GetRequiredService<IOptions<GreyOptions>>().Value;
            client.Timeout = options.Timeout;
            if (!string.IsNullOrEmpty(options.ApiKey))
            {
                client.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
            }
        });

        services.AddHttpClient<QueryService>((sp, client) =>
        {
            var options = sp.GetRequiredService<IOptions<GreyOptions>>().Value;
            client.Timeout = options.Timeout;
            if (!string.IsNullOrEmpty(options.ApiKey))
            {
                client.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
            }
        });

        services.AddHttpClient<MutationService>((sp, client) =>
        {
            var options = sp.GetRequiredService<IOptions<GreyOptions>>().Value;
            client.Timeout = options.Timeout;
            if (!string.IsNullOrEmpty(options.ApiKey))
            {
                client.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
            }
        });

        // Register GreyClient façade
        services.AddScoped<GreyClient>();

        return services;
    }

    /// <summary>
    /// Adds Grey Multi-Tenant SDK services to the service collection.
    /// Binds options from configuration section.
    /// </summary>
    /// <param name="services">The service collection.</param>
    /// <param name="options">The options instance to use.</param>
    /// <returns>The service collection for chaining.</returns>
    public static IServiceCollection AddGrey(
        this IServiceCollection services,
        GreyOptions options)
    {
        ArgumentNullException.ThrowIfNull(options);
        return services.AddGrey(o =>
        {
            o.ApiUrl = options.ApiUrl;
            o.ApiKey = options.ApiKey;
            o.Timeout = options.Timeout;
        });
    }
}
