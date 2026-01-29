//
//  GREYUserService.m
//  GreySDK
//
//  gRPC service stub for user operations.
//

#import "GREYUserService.h"

#pragma mark - Request/Response Implementations

@implementation GREYGetUserRequest

+ (instancetype)requestWithUserId:(NSString *)userId {
    GREYGetUserRequest *request = [[GREYGetUserRequest alloc] init];
    request.userId = userId;
    return request;
}

@end

@implementation GREYUserResponse

- (instancetype)init {
    self = [super init];
    if (self) {
        _metadata = @{};
    }
    return self;
}

@end

#pragma mark - Service Implementation

@implementation GREYUserService

- (void)getCurrentUserWithCompletion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

- (void)getUser:(GREYGetUserRequest *)request
     completion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

@end
