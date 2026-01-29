//
//  GREYOptions.h
//  GreySDK
//
//  Configuration options for the Grey SDK client.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/**
 * Configuration options for the Grey SDK client.
 */
@interface GREYOptions : NSObject <NSCopying>

/// API host
@property (nonatomic, copy) NSString *host;

/// API port
@property (nonatomic, assign) NSInteger port;

/// Whether to use TLS
@property (nonatomic, assign) BOOL useTLS;

/// Request timeout in seconds
@property (nonatomic, assign) NSTimeInterval timeoutSeconds;

/// Authentication token
@property (nonatomic, copy, nullable) NSString *authToken;

/// Custom headers
@property (nonatomic, copy) NSDictionary<NSString *, NSString *> *customHeaders;

/**
 * Create default options for local development.
 */
+ (instancetype)localOptions;

/**
 * Create options for local development with custom port.
 */
+ (instancetype)localOptionsWithPort:(NSInteger)port;

/**
 * Create options for production.
 */
+ (instancetype)productionOptionsWithHost:(NSString *)host;

/**
 * Create options for production with custom port.
 */
+ (instancetype)productionOptionsWithHost:(NSString *)host port:(NSInteger)port;

/**
 * Get the gRPC endpoint string.
 */
- (NSString *)endpoint;

/**
 * Return a new options with auth token set.
 */
- (instancetype)withAuthToken:(NSString *)token;

/**
 * Return a new options with timeout set.
 */
- (instancetype)withTimeout:(NSTimeInterval)seconds;

/**
 * Return a new options with an additional header.
 */
- (instancetype)withHeader:(NSString *)name value:(NSString *)value;

@end

NS_ASSUME_NONNULL_END
