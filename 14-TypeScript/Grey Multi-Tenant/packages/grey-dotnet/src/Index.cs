// =============================================================================
// Grey Multi-Tenant SDK - .NET
// Index.cs (GlobalUsings / Barrel)
//
// Central exports for the Grey Multi-Tenant SDK.
// Consumers can import Grey.Sdk to access everything.
// =============================================================================

// Re-export all services via namespace
global using Grey.Sdk;
global using Grey.Sdk.Services;

// Note: This file provides convenient global usings.
// The main entry point is GreyClient, configured via ServiceCollectionExtensions.
//
// Usage:
// services.AddGrey(options => {
//     options.ApiUrl = "https://api.grey.example.com";
//     options.ApiKey = "your-api-key";
// });
//
// Then inject GreyClient:
// public class MyService(GreyClient grey) {
//     public async Task DoWork() {
//         var user = await grey.User.FetchUserAsync();
//     }
// }
