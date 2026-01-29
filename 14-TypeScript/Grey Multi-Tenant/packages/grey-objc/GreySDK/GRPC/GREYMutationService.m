//
//  GREYMutationService.m
//  GreySDK
//
//  gRPC service stub for mutation operations.
//

#import "GREYMutationService.h"

#pragma mark - Request/Response Implementations

@implementation GREYMutationRequest

- (instancetype)init {
    self = [super init];
    if (self) {
        _parameters = @{};
    }
    return self;
}

+ (instancetype)requestWithName:(NSString *)mutationName
                     parameters:(NSDictionary<NSString *, id> *)parameters {
    GREYMutationRequest *request = [[GREYMutationRequest alloc] init];
    request.mutationName = mutationName;
    request.parameters = parameters ?: @{};
    return request;
}

@end

@implementation GREYMutationResponse
@end

#pragma mark - Service Implementation

@implementation GREYMutationService

- (void)mutate:(GREYMutationRequest *)request
    completion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

@end
