//
//  GREYMutationClient.m
//  GreySDK
//
//  Domain client for mutation operations.
//

#import "GREYMutationClient.h"

@implementation GREYMutationResult
@end

@interface GREYMutationClient ()
@property (nonatomic, strong) GREYMutationService *service;
@end

@implementation GREYMutationClient

- (instancetype)initWithService:(GREYMutationService *)service {
    self = [super init];
    if (self) {
        _service = service;
    }
    return self;
}

#pragma mark - Validation

- (GREYError *)validateMutationName:(NSString *)mutationName {
    if (!mutationName || mutationName.length == 0) {
        return [GREYError validationErrorWithMessage:@"Mutation name is required"];
    }
    
    // Mutation name should be alphanumeric with underscores
    NSString *pattern = @"^[a-zA-Z][a-zA-Z0-9_]*$";
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:pattern
                                                                           options:0
                                                                             error:nil];
    NSRange range = NSMakeRange(0, mutationName.length);
    NSUInteger matches = [regex numberOfMatchesInString:mutationName options:0 range:range];
    
    if (matches == 0) {
        return [GREYError validationErrorWithMessage:@"Mutation name must be alphanumeric (with underscores) and start with a letter"];
    }
    
    return nil;
}

#pragma mark - Public Methods

- (void)executeMutation:(NSString *)mutationName
             parameters:(NSDictionary<NSString *, id> *)parameters
               tenantId:(nullable NSString *)tenantId
             completion:(GREYMutationCompletion)completion {
    // Validate mutation name
    GREYError *nameError = [self validateMutationName:mutationName];
    if (nameError) {
        completion([GREYResult fail:nameError]);
        return;
    }
    
    // Create request
    GREYMutationRequest *request = [GREYMutationRequest requestWithName:mutationName
                                                             parameters:parameters];
    request.tenantId = tenantId;
    
    // Execute
    [_service mutate:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYMutationResponse *response = result.value;
            GREYMutationResult *mutationResult = [[GREYMutationResult alloc] init];
            mutationResult.data = response.data;
            mutationResult.success = response.success;
            mutationResult.message = response.message;
            mutationResult.metadata = response.metadata;
            completion([GREYResult ok:mutationResult]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)executeMutation:(NSString *)mutationName
             completion:(GREYMutationCompletion)completion {
    [self executeMutation:mutationName
               parameters:@{}
                 tenantId:nil
               completion:completion];
}

@end
