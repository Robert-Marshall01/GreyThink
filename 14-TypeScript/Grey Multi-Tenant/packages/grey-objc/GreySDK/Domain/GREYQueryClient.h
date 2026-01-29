//
//  GREYQueryClient.h
//  GreySDK
//
//  Domain client for query operations.
//

#import <Foundation/Foundation.h>
#import "GREYQueryService.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Query result.
 */
@interface GREYQueryResult : NSObject

@property (nonatomic, copy) id data;
@property (nonatomic, copy, nullable) NSDictionary<NSString *, id> *metadata;

@end

/**
 * Completion handler for query operations.
 */
typedef void (^GREYQueryCompletion)(GREYResult<GREYQueryResult *> *result);

/**
 * Domain client for query operations with validation.
 */
@interface GREYQueryClient : NSObject

/**
 * Initialize with query service.
 */
- (instancetype)initWithService:(GREYQueryService *)service;

/**
 * Execute a query.
 * @param queryName Name of the query to execute.
 * @param parameters Query parameters.
 * @param tenantId Optional tenant identifier.
 * @param completion Completion handler with query result or error.
 */
- (void)executeQuery:(NSString *)queryName
          parameters:(NSDictionary<NSString *, id> *)parameters
            tenantId:(nullable NSString *)tenantId
          completion:(GREYQueryCompletion)completion;

/**
 * Execute a query with just a name.
 * @param queryName Name of the query to execute.
 * @param completion Completion handler with query result or error.
 */
- (void)executeQuery:(NSString *)queryName
          completion:(GREYQueryCompletion)completion;

@end

NS_ASSUME_NONNULL_END
