*&---------------------------------------------------------------------*
*& Class ZCL_GREY_AUTH_CLIENT
*& Domain client for authentication operations
*&---------------------------------------------------------------------*
CLASS zcl_grey_auth_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Auth token structure
    TYPES:
      BEGIN OF ty_tokens,
        access_token  TYPE string,
        refresh_token TYPE string,
        expires_in    TYPE i,
      END OF ty_tokens.

    " Login request structure
    TYPES:
      BEGIN OF ty_login_request,
        email     TYPE string,
        password  TYPE string,
        tenant_id TYPE string,
      END OF ty_login_request.

    " Constructor
    METHODS constructor
      IMPORTING
        io_http_client TYPE REF TO zcl_grey_http_client.

    " Login with email and password
    METHODS login
      IMPORTING
        iv_email         TYPE string
        iv_password      TYPE string
        iv_tenant_id     TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Logout current session
    METHODS logout
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Refresh authentication token
    METHODS refresh
      IMPORTING
        iv_refresh_token TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_http_client TYPE REF TO zcl_grey_http_client.

    " Validation methods
    METHODS validate_email
      IMPORTING
        iv_email        TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS validate_password
      IMPORTING
        iv_password     TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS validate_refresh_token
      IMPORTING
        iv_token        TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    " JSON helpers
    METHODS build_login_json
      IMPORTING
        iv_email     TYPE string
        iv_password  TYPE string
        iv_tenant_id TYPE string OPTIONAL
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS build_refresh_json
      IMPORTING
        iv_refresh_token TYPE string
      RETURNING
        VALUE(rv_json)   TYPE string.

    METHODS parse_tokens_response
      IMPORTING
        iv_json          TYPE string
      RETURNING
        VALUE(rs_tokens) TYPE ty_tokens.
ENDCLASS.

CLASS zcl_grey_auth_client IMPLEMENTATION.

  METHOD constructor.
    mo_http_client = io_http_client.
  ENDMETHOD.

  METHOD validate_email.
    IF iv_email IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'Email is required' ).
      RETURN.
    ENDIF.

    " Basic email format check
    IF NOT iv_email CS '@' OR NOT iv_email CS '.'.
      ro_error = zcl_grey_error=>validation_error( 'Invalid email format' ).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD validate_password.
    IF iv_password IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'Password is required' ).
      RETURN.
    ENDIF.

    IF strlen( iv_password ) < 8.
      ro_error = zcl_grey_error=>validation_error( 'Password must be at least 8 characters' ).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD validate_refresh_token.
    IF iv_token IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'Refresh token is required' ).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD build_login_json.
    " Build JSON manually for compatibility
    rv_json = |{ "email": "{ iv_email }", "password": "{ iv_password }"|.
    IF iv_tenant_id IS NOT INITIAL.
      rv_json = |{ rv_json }, "tenant_id": "{ iv_tenant_id }"|.
    ENDIF.
    rv_json = |{ rv_json } }|.

    " Fix the opening brace
    rv_json = |\{{ rv_json }|.
    REPLACE '{ {' IN rv_json WITH '{'.
  ENDMETHOD.

  METHOD build_refresh_json.
    rv_json = |\{ "refresh_token": "{ iv_refresh_token }" \}|.
  ENDMETHOD.

  METHOD parse_tokens_response.
    " Simple JSON parsing - in production use /ui2/cl_json or similar
    " This is a simplified implementation

    " Extract access_token
    FIND REGEX '"access_token"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_tokens-access_token.

    " Extract refresh_token
    FIND REGEX '"refresh_token"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_tokens-refresh_token.

    " Extract expires_in
    DATA lv_expires_str TYPE string.
    FIND REGEX '"expires_in"\s*:\s*(\d+)' IN iv_json SUBMATCHES lv_expires_str.
    IF lv_expires_str IS NOT INITIAL.
      rs_tokens-expires_in = CONV i( lv_expires_str ).
    ENDIF.
  ENDMETHOD.

  METHOD login.
    " Validate inputs
    DATA(lo_email_error) = validate_email( iv_email ).
    IF lo_email_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_email_error ).
      RETURN.
    ENDIF.

    DATA(lo_password_error) = validate_password( iv_password ).
    IF lo_password_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_password_error ).
      RETURN.
    ENDIF.

    " Build request body
    DATA(lv_body) = build_login_json(
      iv_email     = iv_email
      iv_password  = iv_password
      iv_tenant_id = iv_tenant_id
    ).

    " Make request
    DATA(lo_http_result) = mo_http_client->post(
      iv_path = '/api/v1/auth/login'
      iv_body = lv_body
    ).

    " Handle response
    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse successful response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_tokens) = parse_tokens_response( CONV string( <lv_body> ) ).

    " Create result with tokens structure
    DATA: lr_tokens TYPE REF TO ty_tokens.
    CREATE DATA lr_tokens.
    lr_tokens->* = ls_tokens.
    ro_result = zcl_grey_result=>ok( lr_tokens ).
  ENDMETHOD.

  METHOD logout.
    " Make request
    DATA(lo_result) = mo_http_client->post( iv_path = '/api/v1/auth/logout' ).

    IF lo_result->is_success( ).
      " Clear token in HTTP client
      mo_http_client->clear_auth_token( ).
    ENDIF.

    ro_result = lo_result.
  ENDMETHOD.

  METHOD refresh.
    " Validate input
    DATA(lo_token_error) = validate_refresh_token( iv_refresh_token ).
    IF lo_token_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_token_error ).
      RETURN.
    ENDIF.

    " Build request body
    DATA(lv_body) = build_refresh_json( iv_refresh_token ).

    " Make request
    DATA(lo_http_result) = mo_http_client->post(
      iv_path = '/api/v1/auth/refresh'
      iv_body = lv_body
    ).

    " Handle response
    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse successful response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_tokens) = parse_tokens_response( CONV string( <lv_body> ) ).

    " Create result with tokens structure
    DATA: lr_tokens TYPE REF TO ty_tokens.
    CREATE DATA lr_tokens.
    lr_tokens->* = ls_tokens.
    ro_result = zcl_grey_result=>ok( lr_tokens ).
  ENDMETHOD.

ENDCLASS.
