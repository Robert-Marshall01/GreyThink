# Grey Multi-Tenant SDK for ABAP

Official ABAP SDK for the Grey Multi-Tenant platform using HTTP JSON transport.

## Features

- **Authentication**: Login, logout, and token refresh
- **User Management**: Get current user and user by ID
- **Project Operations**: List, create, and get projects
- **Query/Mutation**: Generic query and mutation execution
- **Error Handling**: Normalized error types with code, message, and details
- **Result Type**: Safe result handling for operation outcomes

## Installation

### abapGit

1. Install [abapGit](https://abapgit.org/) in your SAP system
2. Create a new online repository pointing to this package
3. Pull the package into your system

### Manual Installation

1. Create the package `ZGREY_SDK` in your system
2. Create all classes from the `src/` directory
3. Activate all objects

## Classes

| Class | Description |
|-------|-------------|
| `ZCL_GREY_SDK` | Main SDK facade |
| `ZCL_GREY_OPTIONS` | Configuration options |
| `ZCL_GREY_HTTP_CLIENT` | HTTP client wrapper |
| `ZCL_GREY_AUTH_CLIENT` | Authentication client |
| `ZCL_GREY_USER_CLIENT` | User operations client |
| `ZCL_GREY_PROJECTS_CLIENT` | Projects client |
| `ZCL_GREY_QUERY_CLIENT` | Query client |
| `ZCL_GREY_MUTATION_CLIENT` | Mutation client |
| `ZCL_GREY_ERROR` | Normalized error type |
| `ZCL_GREY_RESULT` | Result type (success/failure) |

## Quick Start

### Create SDK Instance

```abap
" Local development
DATA(lo_sdk) = zcl_grey_sdk=>local_client( ).

" Production
DATA(lo_sdk) = zcl_grey_sdk=>production_client( 'api.grey.io' ).

" Custom configuration
DATA(lo_options) = NEW zcl_grey_options(
  iv_host    = 'api.grey.io'
  iv_port    = 443
  iv_use_ssl = abap_true
  iv_timeout = 60
).
DATA(lo_sdk) = NEW zcl_grey_sdk( lo_options ).
```

### Authentication

```abap
" Login
DATA(lo_result) = lo_sdk->mo_auth->login(
  iv_email     = 'user@example.com'
  iv_password  = 'secure-password'
  iv_tenant_id = 'tenant-123'
).

IF lo_result->is_success( ).
  " Get tokens
  ASSIGN lo_result->mo_value->* TO FIELD-SYMBOL(<ls_tokens>).
  DATA(ls_tokens) = <ls_tokens>.
  
  " Set token for subsequent requests
  lo_sdk->set_auth_token( ls_tokens-access_token ).
  
  WRITE: / 'Logged in successfully'.
ELSE.
  WRITE: / 'Login failed:', lo_result->mo_error->mv_message.
ENDIF.

" Logout
DATA(lo_logout_result) = lo_sdk->mo_auth->logout( ).
IF lo_logout_result->is_success( ).
  lo_sdk->clear_auth_token( ).
ENDIF.

" Refresh token
DATA(lo_refresh_result) = lo_sdk->mo_auth->refresh( iv_refresh_token = lv_refresh_token ).
```

### User Operations

```abap
" Get current user
DATA(lo_user_result) = lo_sdk->mo_user->get_current_user( ).

IF lo_user_result->is_success( ).
  ASSIGN lo_user_result->mo_value->* TO FIELD-SYMBOL(<ls_user>).
  WRITE: / 'User:', <ls_user>-email.
ENDIF.

" Get user by ID
DATA(lo_user_by_id) = lo_sdk->mo_user->get_user( iv_user_id = 'user-456' ).
```

### Project Operations

```abap
" List projects
DATA(lo_list_result) = lo_sdk->mo_projects->list_projects(
  iv_limit  = 10
  iv_offset = 0
).

IF lo_list_result->is_success( ).
  ASSIGN lo_list_result->mo_value->* TO FIELD-SYMBOL(<ls_list>).
  LOOP AT <ls_list>-projects ASSIGNING FIELD-SYMBOL(<ls_project>).
    WRITE: / 'Project:', <ls_project>-name.
  ENDLOOP.
ENDIF.

" Create project
DATA(lo_create_result) = lo_sdk->mo_projects->create_project(
  iv_name        = 'New Project'
  iv_description = 'Project description'
).

IF lo_create_result->is_success( ).
  ASSIGN lo_create_result->mo_value->* TO FIELD-SYMBOL(<ls_new_project>).
  WRITE: / 'Created:', <ls_new_project>-project_id.
ENDIF.

" Get project by ID
DATA(lo_project_result) = lo_sdk->mo_projects->get_project( iv_project_id = 'proj-123' ).
```

### Query Operations

```abap
" Execute a query
DATA(lt_params) = VALUE zcl_grey_query_client=>ty_params(
  ( name = 'project_id' value = 'proj-123' )
  ( name = 'status'     value = 'active' )
).

DATA(lo_query_result) = lo_sdk->mo_query->query(
  iv_query_name = 'getProjectStats'
  it_parameters = lt_params
).

IF lo_query_result->is_success( ).
  ASSIGN lo_query_result->mo_value->* TO FIELD-SYMBOL(<ls_query>).
  " Process <ls_query>-data (raw JSON)
ENDIF.
```

### Mutation Operations

```abap
" Execute a mutation
DATA(lt_params) = VALUE zcl_grey_mutation_client=>ty_params(
  ( name = 'project_id' value = 'proj-123' )
  ( name = 'status'     value = 'completed' )
).

DATA(lo_mutation_result) = lo_sdk->mo_mutation->mutate(
  iv_mutation_name = 'updateProjectStatus'
  it_parameters    = lt_params
).

IF lo_mutation_result->is_success( ).
  ASSIGN lo_mutation_result->mo_value->* TO FIELD-SYMBOL(<ls_mutation>).
  IF <ls_mutation>-success = abap_true.
    WRITE: / 'Mutation succeeded:', <ls_mutation>-message.
  ENDIF.
ENDIF.
```

## Error Handling

### Error Codes

| Code | Description |
|------|-------------|
| `unauthorized` | Authentication required or token invalid |
| `forbidden` | Access denied |
| `not_found` | Resource not found |
| `validation_error` | Request validation failed |
| `network_error` | Network connectivity issue |
| `timeout` | Request timed out |
| `server_error` | Server error |
| `unknown` | Unknown error |

### Working with Errors

```abap
DATA(lo_result) = lo_sdk->mo_auth->login(
  iv_email    = 'user@example.com'
  iv_password = 'password'
).

IF lo_result->is_failure( ).
  DATA(lo_error) = lo_result->mo_error.
  
  " Check error type
  CASE lo_error->mv_code.
    WHEN zcl_grey_error=>gc_codes-unauthorized.
      WRITE: / 'Invalid credentials'.
    WHEN zcl_grey_error=>gc_codes-validation_error.
      WRITE: / 'Validation error:', lo_error->mv_message.
    WHEN OTHERS.
      WRITE: / 'Error:', lo_error->to_string( ).
  ENDCASE.
  
  " Check if retryable
  IF lo_error->is_retryable( ) = abap_true.
    WRITE: / 'This error can be retried'.
  ENDIF.
  
  " Get error details
  DATA(ls_error) = lo_error->to_structure( ).
  " ls_error-code, ls_error-message, ls_error-details
ENDIF.
```

### Creating Errors

```abap
" Factory methods
DATA(lo_error) = zcl_grey_error=>unauthorized( ).
DATA(lo_error) = zcl_grey_error=>validation_error( 'Invalid input' ).
DATA(lo_error) = zcl_grey_error=>not_found( iv_details = 'User not found' ).

" From HTTP status
DATA(lo_error) = zcl_grey_error=>from_http_status(
  iv_status  = 404
  iv_message = 'Resource not found'
  iv_details = 'The requested user does not exist'
).
```

## Result Type

The SDK uses `ZCL_GREY_RESULT` for type-safe error handling:

```abap
" Check result
IF lo_result->is_success( ).
  " Access value
  DATA(lo_value) = lo_result->mo_value.
ELSE.
  " Access error
  DATA(lo_error) = lo_result->mo_error.
ENDIF.

" Get value with default
DATA(lo_value) = lo_result->get_or( lr_default ).

" Create results
DATA(lo_ok) = zcl_grey_result=>ok( lr_value ).
DATA(lo_fail) = zcl_grey_result=>fail( lo_error ).
DATA(lo_fail) = zcl_grey_result=>fail_with(
  iv_code    = 'custom_error'
  iv_message = 'Something went wrong'
  iv_details = 'Additional details'
).
```

## Configuration Options

```abap
" Default options
DATA(lo_options) = NEW zcl_grey_options( ).

" Custom options
DATA(lo_options) = NEW zcl_grey_options(
  iv_host           = 'api.grey.io'
  iv_port           = 443
  iv_use_ssl        = abap_true
  iv_timeout        = 60
  iv_auth_token     = 'your-token'
  it_custom_headers = VALUE #(
    ( name = 'X-Custom-Header' value = 'value' )
  )
).

" Builder pattern (immutable copies)
DATA(lo_configured) = lo_options->with_auth_token( 'new-token' ).
DATA(lo_configured) = lo_options->with_timeout( 120 ).
DATA(lo_configured) = lo_options->with_header(
  iv_name  = 'X-Request-ID'
  iv_value = 'abc-123'
).

" Get base URL
DATA(lv_url) = lo_options->get_base_url( ).
" Returns: 'https://api.grey.io:443'
```

## Requirements

- SAP NetWeaver 7.50+ (ABAP 7.50+)
- HTTP client support (CL_HTTP_CLIENT)

## Notes

- JSON parsing is simplified for compatibility. For production use, consider using `/UI2/CL_JSON` or similar libraries.
- Ensure proper SSL certificates are configured for HTTPS connections.
- The SDK uses synchronous HTTP calls. For asynchronous patterns, consider wrapping in background tasks.

## License

MIT License - see LICENSE for details.
