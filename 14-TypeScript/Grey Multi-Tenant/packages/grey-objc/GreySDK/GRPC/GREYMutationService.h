//
//  GREYMutationService.h
//  GreySDK
//
//  gRPC service stub for mutation operations.
//

#import <Foundation/Foundation.h>
#import "GREYGRPCClient.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Mutation request.
 */
@interface GREYMutationRequest : NSObject

@property (nonatomic, copy) NSString *mutationName;
@property (nonatomic, copy) NSDictionary<NSString *, id> *parameters;
@property (nonatomic, copy, nullable) NSString *tenantId;

+ (instancetype)requestWithName:(NSString *)mutationName
                     parameters:(NSDictionary<NSString *, id> *)parameters;

@end

/**
 * Mutation response.
 */
@interface GREYMutationResponse : NSObject

@property (nonatomic, copy, nullable) id data;
@property (nonatomic, assign) BOOL success;
@property (nonatomic, copy, nullable) NSString *message;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *metadata;

@end

/**
 * Mutation service gRPC stub.
 */
@interface GREYMutationService : GREYGRPCClient

/**
 * Execute a mutation.
 */
- (void)mutate:(GREYMutationRequest *)request
    completion:(GREYGRPCCompletion)completion;

@end

NS_ASSUME_NONNULL_END
