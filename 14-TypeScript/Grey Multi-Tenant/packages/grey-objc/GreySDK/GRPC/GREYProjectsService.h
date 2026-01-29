//
//  GREYProjectsService.h
//  GreySDK
//
//  gRPC service stub for project operations.
//

#import <Foundation/Foundation.h>
#import "GREYGRPCClient.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Single project.
 */
@interface GREYProject : NSObject

@property (nonatomic, copy) NSString *projectId;
@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy, nullable) NSString *projectDescription;
@property (nonatomic, copy, nullable) NSString *tenantId;
@property (nonatomic, strong, nullable) NSDate *createdAt;
@property (nonatomic, strong, nullable) NSDate *updatedAt;
@property (nonatomic, copy) NSDictionary<NSString *, id> *metadata;

@end

/**
 * List projects request.
 */
@interface GREYListProjectsRequest : NSObject

@property (nonatomic, assign) NSInteger limit;
@property (nonatomic, assign) NSInteger offset;
@property (nonatomic, copy, nullable) NSString *tenantId;

+ (instancetype)requestWithLimit:(NSInteger)limit offset:(NSInteger)offset;

@end

/**
 * List projects response.
 */
@interface GREYListProjectsResponse : NSObject

@property (nonatomic, copy) NSArray<GREYProject *> *projects;
@property (nonatomic, assign) NSInteger total;

@end

/**
 * Create project request.
 */
@interface GREYCreateProjectRequest : NSObject

@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy, nullable) NSString *projectDescription;
@property (nonatomic, copy, nullable) NSString *tenantId;
@property (nonatomic, copy) NSDictionary<NSString *, id> *metadata;

+ (instancetype)requestWithName:(NSString *)name
                    description:(nullable NSString *)description;

@end

/**
 * Projects service gRPC stub.
 */
@interface GREYProjectsService : GREYGRPCClient

/**
 * List projects.
 */
- (void)listProjects:(GREYListProjectsRequest *)request
          completion:(GREYGRPCCompletion)completion;

/**
 * Create a new project.
 */
- (void)createProject:(GREYCreateProjectRequest *)request
           completion:(GREYGRPCCompletion)completion;

/**
 * Get a project by ID.
 */
- (void)getProject:(NSString *)projectId
        completion:(GREYGRPCCompletion)completion;

@end

NS_ASSUME_NONNULL_END
