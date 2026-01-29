//
//  GREYAuthClient.h
//  GreySDK
//
//  Domain client for authentication operations.
//

#import <Foundation/Foundation.h>
#import "GREYAuthService.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Authentication result containing tokens.
 */
@interface GREYAuthTokens : NSObject

@property (nonatomic, copy) NSString *accessToken;
@property (nonatomic, copy) NSString *refreshToken;
@property (nonatomic, assign) NSTimeInterval expiresIn;

@end

/**
 * Completion handler for auth operations.
 */
typedef void (^GREYAuthCompletion)(GREYResult<GREYAuthTokens *> *result);
typedef void (^GREYLogoutCompletion)(GREYResult<NSNumber *> *result);

/**
 * Domain client for authentication operations with validation.
 */
@interface GREYAuthClient : NSObject

/**
 * Initialize with auth service.
 */
- (instancetype)initWithService:(GREYAuthService *)service;

/**
 * Login with email and password.
 * @param email User's email address.
 * @param password User's password.
 * @param tenantId Optional tenant identifier.
 * @param completion Completion handler with auth tokens or error.
 */
- (void)loginWithEmail:(NSString *)email
              password:(NSString *)password
              tenantId:(nullable NSString *)tenantId
            completion:(GREYAuthCompletion)completion;

/**
 * Logout the current session.
 * @param completion Completion handler with success or error.
 */
- (void)logoutWithCompletion:(GREYLogoutCompletion)completion;

/**
 * Refresh the authentication token.
 * @param refreshToken The refresh token.
 * @param completion Completion handler with new auth tokens or error.
 */
- (void)refreshWithToken:(NSString *)refreshToken
              completion:(GREYAuthCompletion)completion;

@end

NS_ASSUME_NONNULL_END
