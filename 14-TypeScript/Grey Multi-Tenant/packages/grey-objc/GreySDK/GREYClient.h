//
//  GREYClient.h
//  GreySDK
//
//  Main client facade for the Grey Multi-Tenant SDK.
//

#import <Foundation/Foundation.h>
#import "GREYOptions.h"
#import "GREYAuthClient.h"
#import "GREYUserClient.h"
#import "GREYProjectsClient.h"
#import "GREYQueryClient.h"
#import "GREYMutationClient.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Main client for the Grey Multi-Tenant SDK.
 * Provides access to all domain clients.
 */
@interface GREYClient : NSObject

/// SDK configuration options (readonly)
@property (nonatomic, readonly) GREYOptions *options;

/// Authentication client
@property (nonatomic, readonly) GREYAuthClient *auth;

/// User client
@property (nonatomic, readonly) GREYUserClient *user;

/// Projects client
@property (nonatomic, readonly) GREYProjectsClient *projects;

/// Query client
@property (nonatomic, readonly) GREYQueryClient *query;

/// Mutation client
@property (nonatomic, readonly) GREYMutationClient *mutation;

/**
 * Create a client with the given options.
 */
- (instancetype)initWithOptions:(GREYOptions *)options;

/**
 * Create a client for local development.
 */
+ (instancetype)localClient;

/**
 * Create a client for local development with custom port.
 */
+ (instancetype)localClientWithPort:(NSInteger)port;

/**
 * Create a client for production.
 */
+ (instancetype)productionClientWithHost:(NSString *)host;

/**
 * Create a client for production with custom port.
 */
+ (instancetype)productionClientWithHost:(NSString *)host port:(NSInteger)port;

/**
 * Set the authentication token for all requests.
 */
- (void)setAuthToken:(NSString *)token;

/**
 * Clear the authentication token.
 */
- (void)clearAuthToken;

@end

NS_ASSUME_NONNULL_END
