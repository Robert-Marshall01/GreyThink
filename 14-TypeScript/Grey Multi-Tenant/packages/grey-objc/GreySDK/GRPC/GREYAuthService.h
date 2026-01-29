//
//  GREYAuthService.h
//  GreySDK
//
//  gRPC service stub for authentication.
//

#import <Foundation/Foundation.h>
#import "GREYGRPCClient.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Login request parameters.
 */
@interface GREYLoginRequest : NSObject

@property (nonatomic, copy) NSString *email;
@property (nonatomic, copy) NSString *password;
@property (nonatomic, copy, nullable) NSString *tenantId;

+ (instancetype)requestWithEmail:(NSString *)email
                        password:(NSString *)password
                        tenantId:(nullable NSString *)tenantId;

@end

/**
 * Login response.
 */
@interface GREYLoginResponse : NSObject

@property (nonatomic, copy) NSString *accessToken;
@property (nonatomic, copy) NSString *refreshToken;
@property (nonatomic, assign) NSTimeInterval expiresIn;

@end

/**
 * Token refresh request.
 */
@interface GREYRefreshRequest : NSObject

@property (nonatomic, copy) NSString *refreshToken;

+ (instancetype)requestWithRefreshToken:(NSString *)refreshToken;

@end

/**
 * Auth service gRPC stub.
 */
@interface GREYAuthService : GREYGRPCClient

/**
 * Login with credentials.
 */
- (void)login:(GREYLoginRequest *)request
   completion:(GREYGRPCCompletion)completion;

/**
 * Logout the current session.
 */
- (void)logoutWithCompletion:(GREYGRPCCompletion)completion;

/**
 * Refresh the authentication token.
 */
- (void)refresh:(GREYRefreshRequest *)request
     completion:(GREYGRPCCompletion)completion;

@end

NS_ASSUME_NONNULL_END
