//
//  GREYUserClient.h
//  GreySDK
//
//  Domain client for user operations.
//

#import <Foundation/Foundation.h>
#import "GREYUserService.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * User model.
 */
@interface GREYUser : NSObject

@property (nonatomic, copy) NSString *userId;
@property (nonatomic, copy) NSString *email;
@property (nonatomic, copy, nullable) NSString *displayName;
@property (nonatomic, copy, nullable) NSString *tenantId;
@property (nonatomic, copy) NSDictionary<NSString *, id> *metadata;

@end

/**
 * Completion handler for user operations.
 */
typedef void (^GREYUserCompletion)(GREYResult<GREYUser *> *result);

/**
 * Domain client for user operations with validation.
 */
@interface GREYUserClient : NSObject

/**
 * Initialize with user service.
 */
- (instancetype)initWithService:(GREYUserService *)service;

/**
 * Get the current authenticated user.
 * @param completion Completion handler with user or error.
 */
- (void)getCurrentUserWithCompletion:(GREYUserCompletion)completion;

/**
 * Get a user by ID.
 * @param userId The user ID.
 * @param completion Completion handler with user or error.
 */
- (void)getUserById:(NSString *)userId
         completion:(GREYUserCompletion)completion;

@end

NS_ASSUME_NONNULL_END
