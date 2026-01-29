//
//  GREYQueryClient.m
//  GreySDK
//
//  Domain client for query operations.
//

#import "GREYQueryClient.h"

@implementation GREYQueryResult
@end

@interface GREYQueryClient ()
@property (nonatomic, strong) GREYQueryService *service;
@end

@implementation GREYQueryClient

- (instancetype)initWithService:(GREYQueryService *)service {
    self = [super init];
    if (self) {
        _service = service;
    }
    return self;
}

#pragma mark - Validation

- (GREYError *)validateQueryName:(NSString *)queryName {
    if (!queryName || queryName.length == 0) {
        return [GREYError validationErrorWithMessage:@"Query name is required"];
    }
    
    // Query name should be alphanumeric with underscores
    NSString *pattern = @"^[a-zA-Z][a-zA-Z0-9_]*$";
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:pattern
                                                                           options:0
                                                                             error:nil];
    NSRange range = NSMakeRange(0, queryName.length);
    NSUInteger matches = [regex numberOfMatchesInString:queryName options:0 range:range];
    
    if (matches == 0) {
        return [GREYError validationErrorWithMessage:@"Query name must be alphanumeric (with underscores) and start with a letter"];
    }
    
    return nil;
}

#pragma mark - Public Methods

- (void)executeQuery:(NSString *)queryName
          parameters:(NSDictionary<NSString *, id> *)parameters
            tenantId:(nullable NSString *)tenantId
          completion:(GREYQueryCompletion)completion {
    // Validate query name
    GREYError *nameError = [self validateQueryName:queryName];
    if (nameError) {
        completion([GREYResult fail:nameError]);
        return;
    }
    
    // Create request
    GREYQueryRequest *request = [GREYQueryRequest requestWithName:queryName
                                                       parameters:parameters];
    request.tenantId = tenantId;
    
    // Execute
    [_service query:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYQueryResponse *response = result.value;
            GREYQueryResult *queryResult = [[GREYQueryResult alloc] init];
            queryResult.data = response.data;
            queryResult.metadata = response.metadata;
            completion([GREYResult ok:queryResult]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)executeQuery:(NSString *)queryName
          completion:(GREYQueryCompletion)completion {
    [self executeQuery:queryName
            parameters:@{}
              tenantId:nil
            completion:completion];
}

@end
