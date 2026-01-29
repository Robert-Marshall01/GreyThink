# Grey Multi-Tenant SDK for Objective-C

Official Objective-C SDK for the Grey Multi-Tenant platform using gRPC transport.

## Features

- **Authentication**: Login, logout, and token refresh
- **User Management**: Get current user and user by ID
- **Project Operations**: List, create, and get projects
- **Query/Mutation**: Generic query and mutation execution
- **Error Handling**: Normalized error types with code, message, and details
- **Result Type**: Type-safe result handling with functional combinators

## Installation

### CocoaPods

Add to your `Podfile`:

```ruby
pod 'GreySDK', '~> 1.0'
```

Then run:

```bash
pod install
```

### Manual Installation

Copy the `GreySDK` folder into your project and add all `.h` and `.m` files to your target.

## Quick Start

### Import the SDK

```objc
#import <GreySDK/GreySDK.h>
```

### Create a Client

```objc
// Local development
GREYClient *client = [GREYClient localClient];

// Production
GREYClient *client = [GREYClient productionClientWithHost:@"api.grey.io"];

// Custom configuration
GREYOptions *options = [[GREYOptions productionOptionsWithHost:@"api.grey.io"]
                        withTimeout:60.0];
GREYClient *client = [[GREYClient alloc] initWithOptions:options];
```

### Authentication

```objc
// Login
[client.auth loginWithEmail:@"user@example.com"
                   password:@"secure-password"
                   tenantId:@"tenant-123"
                 completion:^(GREYResult<GREYAuthTokens *> *result) {
    [result onSuccess:^(GREYAuthTokens *tokens) {
        NSLog(@"Logged in! Token: %@", tokens.accessToken);
        [client setAuthToken:tokens.accessToken];
    }];
    
    [result onFailure:^(GREYError *error) {
        NSLog(@"Login failed: %@", error.message);
    }];
}];

// Logout
[client.auth logoutWithCompletion:^(GREYResult<NSNumber *> *result) {
    if (result.isSuccess) {
        [client clearAuthToken];
    }
}];

// Refresh token
[client.auth refreshWithToken:refreshToken
                   completion:^(GREYResult<GREYAuthTokens *> *result) {
    // Handle new tokens
}];
```

### User Operations

```objc
// Get current user
[client.user getCurrentUserWithCompletion:^(GREYResult<GREYUser *> *result) {
    [result matchSuccess:^id(GREYUser *user) {
        NSLog(@"User: %@ (%@)", user.displayName, user.email);
        return nil;
    } failure:^id(GREYError *error) {
        NSLog(@"Error: %@", error.message);
        return nil;
    }];
}];

// Get user by ID
[client.user getUserById:@"user-456"
              completion:^(GREYResult<GREYUser *> *result) {
    // Handle result
}];
```

### Project Operations

```objc
// List projects
[client.projects listProjectsWithLimit:10
                                offset:0
                              tenantId:nil
                            completion:^(GREYResult<GREYProjectList *> *result) {
    if (result.isSuccess) {
        for (GREYProject *project in result.value.projects) {
            NSLog(@"Project: %@", project.name);
        }
    }
}];

// Create project
[client.projects createProjectWithName:@"New Project"
                           description:@"Project description"
                              tenantId:nil
                              metadata:@{@"key": @"value"}
                            completion:^(GREYResult<GREYProject *> *result) {
    if (result.isSuccess) {
        NSLog(@"Created project: %@", result.value.projectId);
    }
}];
```

### Query and Mutation

```objc
// Execute a query
[client.query executeQuery:@"getProjectStats"
                parameters:@{@"projectId": @"proj-123"}
                  tenantId:nil
                completion:^(GREYResult<GREYQueryResult *> *result) {
    if (result.isSuccess) {
        id data = result.value.data;
        // Process query result
    }
}];

// Execute a mutation
[client.mutation executeMutation:@"updateProjectStatus"
                      parameters:@{@"projectId": @"proj-123", @"status": @"active"}
                        tenantId:nil
                      completion:^(GREYResult<GREYMutationResult *> *result) {
    if (result.isSuccess && result.value.success) {
        NSLog(@"Mutation succeeded: %@", result.value.message);
    }
}];
```

## Error Handling

### Error Codes

| Code | Description |
|------|-------------|
| `GREYErrorCodeUnauthorized` | Authentication required or token invalid |
| `GREYErrorCodeForbidden` | Access denied |
| `GREYErrorCodeNotFound` | Resource not found |
| `GREYErrorCodeValidationError` | Request validation failed |
| `GREYErrorCodeNetworkError` | Network connectivity issue |
| `GREYErrorCodeTimeout` | Request timed out |
| `GREYErrorCodeServerError` | Server error |
| `GREYErrorCodeUnknown` | Unknown error |

### Working with Errors

```objc
GREYError *error = result.error;

// Check error type
if (error.code == GREYErrorCodeUnauthorized) {
    // Handle auth error
}

// Check if retryable
if (error.isRetryable) {
    // Retry the request
}

// Get error details
NSLog(@"Code: %@", error.codeString);
NSLog(@"Message: %@", error.message);
NSLog(@"Details: %@", error.details);

// Convert to NSError if needed
NSError *nsError = [error toNSError];
```

## Result Type

The SDK uses `GREYResult<T>` for type-safe error handling:

```objc
// Pattern matching
id value = [result matchSuccess:^id(GREYUser *user) {
    return user.displayName;
} failure:^id(GREYError *error) {
    return @"Unknown";
}];

// Mapping
GREYResult *mapped = [result map:^id(GREYUser *user) {
    return user.email;
}];

// Chaining
GREYResult *chained = [result flatMap:^GREYResult *(GREYUser *user) {
    return [self processUser:user];
}];

// Default values
NSString *name = [result getOr:@"Anonymous"];
```

## Configuration Options

```objc
GREYOptions *options = [[GREYOptions alloc] init];
options.host = @"api.grey.io";
options.port = 443;
options.useTLS = YES;
options.timeoutSeconds = 30.0;
options.authToken = @"your-token";

// Using builder pattern (immutable copies)
GREYOptions *configured = [[[GREYOptions productionOptionsWithHost:@"api.grey.io"]
                            withTimeout:60.0]
                           withHeader:@"X-Custom" value:@"value"];
```

## Requirements

- iOS 12.0+ / macOS 10.14+ / tvOS 12.0+ / watchOS 5.0+
- Xcode 12.0+

## License

MIT License - see [LICENSE](LICENSE) for details.
