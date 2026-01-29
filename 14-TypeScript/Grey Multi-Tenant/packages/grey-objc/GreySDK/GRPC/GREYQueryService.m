//
//  GREYQueryService.m
//  GreySDK
//
//  gRPC service stub for query operations.
//

#import "GREYQueryService.h"

#pragma mark - Request/Response Implementations

@implementation GREYQueryRequest

- (instancetype)init {
    self = [super init];
    if (self) {
        _parameters = @{};
    }
    return self;
}

+ (instancetype)requestWithName:(NSString *)queryName
                     parameters:(NSDictionary<NSString *, id> *)parameters {
    GREYQueryRequest *request = [[GREYQueryRequest alloc] init];
    request.queryName = queryName;
    request.parameters = parameters ?: @{};
    return request;
}

@end

@implementation GREYQueryResponse
@end

#pragma mark - Service Implementation

@implementation GREYQueryService

- (void)query:(GREYQueryRequest *)request
   completion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

@end
