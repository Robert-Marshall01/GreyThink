//
//  GREYUserClient.m
//  GreySDK
//
//  Domain client for user operations.
//

#import "GREYUserClient.h"

@implementation GREYUser

- (instancetype)init {
    self = [super init];
    if (self) {
        _metadata = @{};
    }
    return self;
}

@end

@interface GREYUserClient ()
@property (nonatomic, strong) GREYUserService *service;
@end

@implementation GREYUserClient

- (instancetype)initWithService:(GREYUserService *)service {
    self = [super init];
    if (self) {
        _service = service;
    }
    return self;
}

#pragma mark - Validation

- (GREYError *)validateUserId:(NSString *)userId {
    if (!userId || userId.length == 0) {
        return [GREYError validationErrorWithMessage:@"User ID is required"];
    }
    
    return nil;
}

#pragma mark - Private Helpers

- (GREYUser *)mapResponseToUser:(GREYUserResponse *)response {
    GREYUser *user = [[GREYUser alloc] init];
    user.userId = response.userId;
    user.email = response.email;
    user.displayName = response.displayName;
    user.tenantId = response.tenantId;
    user.metadata = response.metadata;
    return user;
}

#pragma mark - Public Methods

- (void)getCurrentUserWithCompletion:(GREYUserCompletion)completion {
    [_service getCurrentUserWithCompletion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYUserResponse *response = result.value;
            GREYUser *user = [self mapResponseToUser:response];
            completion([GREYResult ok:user]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

- (void)getUserById:(NSString *)userId
         completion:(GREYUserCompletion)completion {
    // Validate input
    GREYError *error = [self validateUserId:userId];
    if (error) {
        completion([GREYResult fail:error]);
        return;
    }
    
    // Create request and execute
    GREYGetUserRequest *request = [GREYGetUserRequest requestWithUserId:userId];
    
    [_service getUser:request completion:^(GREYResult *result) {
        if (result.isSuccess) {
            GREYUserResponse *response = result.value;
            GREYUser *user = [self mapResponseToUser:response];
            completion([GREYResult ok:user]);
        } else {
            completion([GREYResult fail:result.error]);
        }
    }];
}

@end
