//
//  GREYUserService.h
//  GreySDK
//
//  gRPC service stub for user operations.
//

#import <Foundation/Foundation.h>
#import "GREYGRPCClient.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Get user request.
 */
@interface GREYGetUserRequest : NSObject

@property (nonatomic, copy) NSString *userId;

+ (instancetype)requestWithUserId:(NSString *)userId;

@end

/**
 * User response.
 */
@interface GREYUserResponse : NSObject

@property (nonatomic, copy) NSString *userId;
@property (nonatomic, copy) NSString *email;
@property (nonatomic, copy, nullable) NSString *displayName;
@property (nonatomic, copy, nullable) NSString *tenantId;
@property (nonatomic, copy) NSDictionary<NSString *, id> *metadata;

@end

/**
 * User service gRPC stub.
 */
@interface GREYUserService : GREYGRPCClient

/**
 * Get current user.
 */
- (void)getCurrentUserWithCompletion:(GREYGRPCCompletion)completion;

/**
 * Get user by ID.
 */
- (void)getUser:(GREYGetUserRequest *)request
     completion:(GREYGRPCCompletion)completion;

@end

NS_ASSUME_NONNULL_END
