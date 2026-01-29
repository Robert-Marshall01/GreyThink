//
//  GREYProjectsClient.h
//  GreySDK
//
//  Domain client for project operations.
//

#import <Foundation/Foundation.h>
#import "GREYProjectsService.h"

NS_ASSUME_NONNULL_BEGIN

/**
 * Project list result.
 */
@interface GREYProjectList : NSObject

@property (nonatomic, copy) NSArray<GREYProject *> *projects;
@property (nonatomic, assign) NSInteger total;

@end

/**
 * Completion handler for project operations.
 */
typedef void (^GREYProjectCompletion)(GREYResult<GREYProject *> *result);
typedef void (^GREYProjectListCompletion)(GREYResult<GREYProjectList *> *result);

/**
 * Domain client for project operations with validation.
 */
@interface GREYProjectsClient : NSObject

/**
 * Initialize with projects service.
 */
- (instancetype)initWithService:(GREYProjectsService *)service;

/**
 * List projects with pagination.
 * @param limit Maximum number of projects to return.
 * @param offset Number of projects to skip.
 * @param tenantId Optional tenant filter.
 * @param completion Completion handler with project list or error.
 */
- (void)listProjectsWithLimit:(NSInteger)limit
                       offset:(NSInteger)offset
                     tenantId:(nullable NSString *)tenantId
                   completion:(GREYProjectListCompletion)completion;

/**
 * Create a new project.
 * @param name Project name.
 * @param description Optional project description.
 * @param tenantId Optional tenant identifier.
 * @param metadata Optional metadata dictionary.
 * @param completion Completion handler with created project or error.
 */
- (void)createProjectWithName:(NSString *)name
                  description:(nullable NSString *)description
                     tenantId:(nullable NSString *)tenantId
                     metadata:(nullable NSDictionary<NSString *, id> *)metadata
                   completion:(GREYProjectCompletion)completion;

/**
 * Get a project by ID.
 * @param projectId The project ID.
 * @param completion Completion handler with project or error.
 */
- (void)getProjectById:(NSString *)projectId
            completion:(GREYProjectCompletion)completion;

@end

NS_ASSUME_NONNULL_END
