//
//  GREYProjectsClient.m
//  GreySDK
//
//  Domain client for project operations.
//

#import "GREYProjectsClient.h"

@implementation GREYProjectList

- (instancetype)init {
    self = [super init];
    if (self) {
        _projects = @[];
    }
    return self;
}

@end

@interface GREYProjectsClient ()
@property (nonatomic, strong) GREYProjectsService *service;
@end

@implementation GREYProjectsClient

- (instancetype)initWithService:(GREYProjectsService *)service {
    self = [super init];
    if (self) {
        _service = service;
    }
    return self;
}

#pragma mark - Validation

- (GREYError *)validateProjectName:(NSString *)name {
    if (!name || name.length == 0) {
        return [GREYError validationErrorWithMessage:@"Project name is required"];
    }
    
    if (name.length < 3) {
        return [GREYError validationErrorWithMessage:@"Project name must be at least 3 characters"];
    }
    
    if (name.length > 100) {
        return [GREYError validationErrorWithMessage:@"Project name must be at most 100 characters"];
    }
    
    return nil;
}

- (GREYError *)validateProjectId:(NSString *)projectId {
    if (!projectId || projectId.length == 0) {
        return [GREYError validationErrorWithMessage:@"Project ID is required"];
    }
    
    return nil;
}

- (GREYError *)validatePagination:(NSInteger)limit offset:(NSInteger)offset {
    if (limit < 1 || limit > 100) {
        return [GREYError validationErrorWithMessage:@"Limit must be between 1 and 100"];
    }
    
    if (offset < 0) {
        return [GREYError validationErrorWithMessage:@"Offset must be non-negative"];
    }
    
    return nil;
}

#pragma mark - Public Methods

- (void)listProjectsWithLimit:(NSInteger)limit
                       offset:(NSInteger)offset
                     tenantId:(nullable NSString *)tenantId
                   completion:(GREYProjectListCompletion)completion {
    // Validate pagination
    GREYError *paginationError = [self validatePagination:limit offset:offset];
    if (paginationError) {
        completion([GREYResult fail:paginationError]);
        return;
    }
    
    // Create request
    GREYListProjectsRequest *request = [GREYListProjectsRequest requestWithLimit:limit offset:offset];
    request.tenantId = tenantId;
    
    // Execute
    [_service listProjects:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYListProjectsResponse *response = result.value;
            GREYProjectList *list = [[GREYProjectList alloc] init];
            list.projects = response.projects;
            list.total = response.total;
            completion([GREYResult ok:list]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)createProjectWithName:(NSString *)name
                  description:(nullable NSString *)description
                     tenantId:(nullable NSString *)tenantId
                     metadata:(nullable NSDictionary<NSString *, id> *)metadata
                   completion:(GREYProjectCompletion)completion {
    // Validate name
    GREYError *nameError = [self validateProjectName:name];
    if (nameError) {
        completion([GREYResult fail:nameError]);
        return;
    }
    
    // Create request
    GREYCreateProjectRequest *request = [GREYCreateProjectRequest requestWithName:name
                                                                      description:description];
    request.tenantId = tenantId;
    if (metadata) {
        request.metadata = metadata;
    }
    
    // Execute
    [_service createProject:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYProject *project = result.value;
            completion([GREYResult ok:project]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)getProjectById:(NSString *)projectId
            completion:(GREYProjectCompletion)completion {
    // Validate project ID
    GREYError *idError = [self validateProjectId:projectId];
    if (idError) {
        completion([GREYResult fail:idError]);
        return;
    }
    
    // Execute
    [_service getProject:projectId completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYProject *project = result.value;
            completion([GREYResult ok:project]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

@end
