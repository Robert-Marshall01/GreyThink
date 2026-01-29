//
//  GREYGRPCClient.h
//  GreySDK
//
//  Base gRPC client for the Grey SDK.
//

#import <Foundation/Foundation.h>
#import "GREYOptions.h"
#import "GREYResult.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Completion handler for gRPC calls.
 */
typedef void (^GREYGRPCCompletion)(GREYResult *result);

/**
 * Base gRPC client providing shared functionality.
 */
@interface GREYGRPCClient : NSObject

/// Client configuration options
@property (nonatomic, readonly) GREYOptions *options;

/**
 * Initialize with options.
 */
- (instancetype)initWithOptions:(GREYOptions *)options;

/**
 * Update the authentication token.
 */
- (void)setAuthToken:(NSString *)token;

/**
 * Clear the authentication token.
 */
- (void)clearAuthToken;

/**
 * Get the current metadata including auth token.
 */
- (NSDictionary<NSString *, NSString *> *)metadata;

@end

NS_ASSUME_NONNULL_END
