//
//  GREYResultTests.m
//  GreySDKTests
//
//  Unit tests for GREYResult.
//

#import <XCTest/XCTest.h>
#import "GREYResult.h"

@interface GREYResultTests : XCTestCase
@end

@implementation GREYResultTests

#pragma mark - Success Tests

- (void)testOkCreatesSuccessResult {
    GREYResult *result = [GREYResult ok:@"hello"];
    
    XCTAssertTrue(result.isSuccess);
    XCTAssertFalse(result.isFailure);
    XCTAssertEqualObjects(result.value, @"hello");
    XCTAssertNil(result.error);
}

- (void)testOkWithNilValue {
    GREYResult *result = [GREYResult ok:[NSNull null]];
    
    XCTAssertTrue(result.isSuccess);
    XCTAssertNotNil(result.value);
}

#pragma mark - Failure Tests

- (void)testFailCreatesFailureResult {
    GREYError *error = [GREYError unauthorized];
    GREYResult *result = [GREYResult fail:error];
    
    XCTAssertFalse(result.isSuccess);
    XCTAssertTrue(result.isFailure);
    XCTAssertNil(result.value);
    XCTAssertEqual(result.error, error);
}

- (void)testFailWithCodeCreatesFailure {
    GREYResult *result = [GREYResult failWithCode:GREYErrorCodeNotFound
                                          message:@"Resource not found"
                                          details:nil];
    
    XCTAssertTrue(result.isFailure);
    XCTAssertEqual(result.error.code, GREYErrorCodeNotFound);
}

#pragma mark - GetOr Tests

- (void)testGetOrReturnsValueOnSuccess {
    GREYResult *result = [GREYResult ok:@42];
    
    id value = [result getOr:@0];
    XCTAssertEqualObjects(value, @42);
}

- (void)testGetOrReturnsDefaultOnFailure {
    GREYResult *result = [GREYResult fail:[GREYError unauthorized]];
    
    id value = [result getOr:@0];
    XCTAssertEqualObjects(value, @0);
}

#pragma mark - Map Tests

- (void)testMapTransformsSuccessValue {
    GREYResult *result = [GREYResult ok:@5];
    
    GREYResult *mapped = [result map:^id(NSNumber *value) {
        return @([value intValue] * 2);
    }];
    
    XCTAssertTrue(mapped.isSuccess);
    XCTAssertEqualObjects(mapped.value, @10);
}

- (void)testMapPassesThroughFailure {
    GREYError *error = [GREYError notFound];
    GREYResult *result = [GREYResult fail:error];
    
    GREYResult *mapped = [result map:^id(id value) {
        return @"transformed";
    }];
    
    XCTAssertTrue(mapped.isFailure);
    XCTAssertEqual(mapped.error, error);
}

#pragma mark - FlatMap Tests

- (void)testFlatMapTransformsSuccessValue {
    GREYResult *result = [GREYResult ok:@5];
    
    GREYResult *flatMapped = [result flatMap:^GREYResult *(NSNumber *value) {
        return [GREYResult ok:@([value intValue] * 2)];
    }];
    
    XCTAssertTrue(flatMapped.isSuccess);
    XCTAssertEqualObjects(flatMapped.value, @10);
}

- (void)testFlatMapCanReturnFailure {
    GREYResult *result = [GREYResult ok:@5];
    
    GREYResult *flatMapped = [result flatMap:^GREYResult *(NSNumber *value) {
        return [GREYResult fail:[GREYError validationErrorWithMessage:@"Invalid"]];
    }];
    
    XCTAssertTrue(flatMapped.isFailure);
}

#pragma mark - Callback Tests

- (void)testOnSuccessCalledForSuccess {
    __block BOOL called = NO;
    __block id capturedValue = nil;
    
    GREYResult *result = [GREYResult ok:@"test"];
    [result onSuccess:^(id value) {
        called = YES;
        capturedValue = value;
    }];
    
    XCTAssertTrue(called);
    XCTAssertEqualObjects(capturedValue, @"test");
}

- (void)testOnSuccessNotCalledForFailure {
    __block BOOL called = NO;
    
    GREYResult *result = [GREYResult fail:[GREYError unauthorized]];
    [result onSuccess:^(id value) {
        called = YES;
    }];
    
    XCTAssertFalse(called);
}

- (void)testOnFailureCalledForFailure {
    __block BOOL called = NO;
    __block GREYError *capturedError = nil;
    
    GREYError *error = [GREYError unauthorized];
    GREYResult *result = [GREYResult fail:error];
    [result onFailure:^(GREYError *err) {
        called = YES;
        capturedError = err;
    }];
    
    XCTAssertTrue(called);
    XCTAssertEqual(capturedError, error);
}

- (void)testOnFailureNotCalledForSuccess {
    __block BOOL called = NO;
    
    GREYResult *result = [GREYResult ok:@"test"];
    [result onFailure:^(GREYError *error) {
        called = YES;
    }];
    
    XCTAssertFalse(called);
}

#pragma mark - Match Tests

- (void)testMatchCallsSuccessHandlerOnSuccess {
    GREYResult *result = [GREYResult ok:@42];
    
    NSString *matched = [result matchSuccess:^id(NSNumber *value) {
        return [NSString stringWithFormat:@"Value: %@", value];
    } failure:^id(GREYError *error) {
        return @"Error occurred";
    }];
    
    XCTAssertEqualObjects(matched, @"Value: 42");
}

- (void)testMatchCallsFailureHandlerOnFailure {
    GREYResult *result = [GREYResult fail:[GREYError notFound]];
    
    NSString *matched = [result matchSuccess:^id(id value) {
        return @"Success";
    } failure:^id(GREYError *error) {
        return [NSString stringWithFormat:@"Error: %@", error.codeString];
    }];
    
    XCTAssertEqualObjects(matched, @"Error: not_found");
}

@end
