//
//  GREYProjectsService.m
//  GreySDK
//
//  gRPC service stub for project operations.
//

#import "GREYProjectsService.h"

#pragma mark - Model Implementations

@implementation GREYProject

- (instancetype)init {
    self = [super init];
    if (self) {
        _metadata = @{};
    }
    return self;
}

@end

@implementation GREYListProjectsRequest

+ (instancetype)requestWithLimit:(NSInteger)limit offset:(NSInteger)offset {
    GREYListProjectsRequest *request = [[GREYListProjectsRequest alloc] init];
    request.limit = limit;
    request.offset = offset;
    return request;
}

@end

@implementation GREYListProjectsResponse

- (instancetype)init {
    self = [super init];
    if (self) {
        _projects = @[];
    }
    return self;
}

@end

@implementation GREYCreateProjectRequest

- (instancetype)init {
    self = [super init];
    if (self) {
        _metadata = @{};
    }
    return self;
}

+ (instancetype)requestWithName:(NSString *)name
                    description:(nullable NSString *)description {
    GREYCreateProjectRequest *request = [[GREYCreateProjectRequest alloc] init];
    request.name = name;
    request.projectDescription = description;
    return request;
}

@end

#pragma mark - Service Implementation

@implementation GREYProjectsService

- (void)listProjects:(GREYListProjectsRequest *)request
          completion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

- (void)createProject:(GREYCreateProjectRequest *)request
           completion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

- (void)getProject:(NSString *)projectId
        completion:(GREYGRPCCompletion)completion {
    // TODO: Implement actual gRPC call using gRPC-ObjC
    // Stub implementation
    GREYError *error = [GREYError serverErrorWithDetails:@"gRPC not implemented - stub only"];
    completion([GREYResult fail:error]);
}

@end
