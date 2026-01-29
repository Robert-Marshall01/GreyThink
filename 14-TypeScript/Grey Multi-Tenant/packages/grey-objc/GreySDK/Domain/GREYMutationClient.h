//
//  GREYMutationClient.h
//  GreySDK
//
//  Domain client for mutation operations.
//

#import <Foundation/Foundation.h>
#import "GREYMutationService.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Mutation result.
 */
@interface GREYMutationResult : NSObject

@property (nonatomic, copy, nullable) id data;
@property (nonatomic, assign) BOOL success;
@property (nonatomic, copy, nullable) NSString *message;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *metadata;

@end

/**
 * Completion handler for mutation operations.
 */
typedef void (^GREYMutationCompletion)(GREYResult<GREYMutationResult *> *result);

/**
 * Domain client for mutation operations with validation.
 */
@interface GREYMutationClient : NSObject

/**
 * Initialize with mutation service.
 */
- (instancetype)initWithService:(GREYMutationService *)service;

/**
 * Execute a mutation.
 * @param mutationName Name of the mutation to execute.
 * @param parameters Mutation parameters.
 * @param tenantId Optional tenant identifier.
 * @param completion Completion handler with mutation result or error.
 */
- (void)executeMutation:(NSString *)mutationName
             parameters:(NSDictionary<NSString *, id> *)parameters
               tenantId:(nullable NSString *)tenantId
             completion:(GREYMutationCompletion)completion;

/**
 * Execute a mutation with just a name.
 * @param mutationName Name of the mutation to execute.
 * @param completion Completion handler with mutation result or error.
 */
- (void)executeMutation:(NSString *)mutationName
             completion:(GREYMutationCompletion)completion;

@end

NS_ASSUME_NONNULL_END
