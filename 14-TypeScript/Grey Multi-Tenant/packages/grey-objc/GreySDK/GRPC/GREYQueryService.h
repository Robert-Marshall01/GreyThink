//
//  GREYQueryService.h
//  GreySDK
//
//  gRPC service stub for query operations.
//

#import <Foundation/Foundation.h>
#import "GREYGRPCClient.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Query request.
 */
@interface GREYQueryRequest : NSObject

@property (nonatomic, copy) NSString *queryName;
@property (nonatomic, copy) NSDictionary<NSString *, id> *parameters;
@property (nonatomic, copy, nullable) NSString *tenantId;

+ (instancetype)requestWithName:(NSString *)queryName
                     parameters:(NSDictionary<NSString *, id> *)parameters;

@end

/**
 * Query response.
 */
@interface GREYQueryResponse : NSObject

@property (nonatomic, copy) id data;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *metadata;

@end

/**
 * Query service gRPC stub.
 */
@interface GREYQueryService : GREYGRPCClient

/**
 * Execute a query.
 */
- (void)query:(GREYQueryRequest *)request
   completion:(GREYGRPCCompletion)completion;

@end

NS_ASSUME_NONNULL_END
