//
//  GREYValidationTests.m
//  GreySDKTests
//
//  Unit tests for domain client validation logic.
//

#import <XCTest/XCTest.h>
#import "GREYAuthClient.h"
#import "GREYProjectsClient.h"
#import "GREYQueryClient.h"
#import "GREYMutationClient.h"

@interface GREYValidationTests : XCTestCase

@property (nonatomic, strong) GREYOptions *options;
@property (nonatomic, strong) GREYAuthClient *authClient;
@property (nonatomic, strong) GREYProjectsClient *projectsClient;
@property (nonatomic, strong) GREYQueryClient *queryClient;
@property (nonatomic, strong) GREYMutationClient *mutationClient;

@end

@implementation GREYValidationTests

- (void)setUp {
    [super setUp];
    
    _options = [GREYOptions localOptions];
    
    // Create services and clients for testing
    GREYAuthService *authService = [[GREYAuthService alloc] initWithOptions:_options];
    _authClient = [[GREYAuthClient alloc] initWithService:authService];
    
    GREYProjectsService *projectsService = [[GREYProjectsService alloc] initWithOptions:_options];
    _projectsClient = [[GREYProjectsClient alloc] initWithService:projectsService];
    
    GREYQueryService *queryService = [[GREYQueryService alloc] initWithOptions:_options];
    _queryClient = [[GREYQueryClient alloc] initWithService:queryService];
    
    GREYMutationService *mutationService = [[GREYMutationService alloc] initWithOptions:_options];
    _mutationClient = [[GREYMutationClient alloc] initWithService:mutationService];
}

#pragma mark - Auth Validation Tests

- (void)testLoginWithEmptyEmailFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Login validation"];
    
    [_authClient loginWithEmail:@""
                       password:@"password123"
                       tenantId:nil
                     completion:^(GREYResult<GREYAuthTokens *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"Email"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testLoginWithInvalidEmailFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Login validation"];
    
    [_authClient loginWithEmail:@"not-an-email"
                       password:@"password123"
                       tenantId:nil
                     completion:^(GREYResult<GREYAuthTokens *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"email"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testLoginWithShortPasswordFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Login validation"];
    
    [_authClient loginWithEmail:@"user@example.com"
                       password:@"short"
                       tenantId:nil
                     completion:^(GREYResult<GREYAuthTokens *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"Password"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testRefreshWithEmptyTokenFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Refresh validation"];
    
    [_authClient refreshWithToken:@""
                       completion:^(GREYResult<GREYAuthTokens *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"token"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

#pragma mark - Project Validation Tests

- (void)testCreateProjectWithEmptyNameFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Create project validation"];
    
    [_projectsClient createProjectWithName:@""
                               description:nil
                                  tenantId:nil
                                  metadata:nil
                                completion:^(GREYResult<GREYProject *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"name"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testCreateProjectWithShortNameFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Create project validation"];
    
    [_projectsClient createProjectWithName:@"ab"
                               description:nil
                                  tenantId:nil
                                  metadata:nil
                                completion:^(GREYResult<GREYProject *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"3 characters"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testListProjectsWithInvalidLimitFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"List projects validation"];
    
    [_projectsClient listProjectsWithLimit:0
                                    offset:0
                                  tenantId:nil
                                completion:^(GREYResult<GREYProjectList *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"Limit"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testListProjectsWithNegativeOffsetFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"List projects validation"];
    
    [_projectsClient listProjectsWithLimit:10
                                    offset:-1
                                  tenantId:nil
                                completion:^(GREYResult<GREYProjectList *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        XCTAssertTrue([result.error.message containsString:@"Offset"]);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

#pragma mark - Query Validation Tests

- (void)testQueryWithEmptyNameFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Query validation"];
    
    [_queryClient executeQuery:@""
                    parameters:@{}
                      tenantId:nil
                    completion:^(GREYResult<GREYQueryResult *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

- (void)testQueryWithInvalidNameFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Query validation"];
    
    [_queryClient executeQuery:@"123invalid"
                    parameters:@{}
                      tenantId:nil
                    completion:^(GREYResult<GREYQueryResult *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

#pragma mark - Mutation Validation Tests

- (void)testMutationWithEmptyNameFails {
    XCTestExpectation *expectation = [self expectationWithDescription:@"Mutation validation"];
    
    [_mutationClient executeMutation:@""
                          parameters:@{}
                            tenantId:nil
                          completion:^(GREYResult<GREYMutationResult *> *result) {
        XCTAssertTrue(result.isFailure);
        XCTAssertEqual(result.error.code, GREYErrorCodeValidationError);
        [expectation fulfill];
    }];
    
    [self waitForExpectationsWithTimeout:1.0 handler:nil];
}

@end
