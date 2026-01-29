*&---------------------------------------------------------------------*
*& Class ZCL_GREY_USER_CLIENT
*& Domain client for user operations
*&---------------------------------------------------------------------*
CLASS zcl_grey_user_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " User structure
    TYPES:
      BEGIN OF ty_user,
        user_id      TYPE string,
        email        TYPE string,
        display_name TYPE string,
        tenant_id    TYPE string,
      END OF ty_user.

    " Constructor
    METHODS constructor
      IMPORTING
        io_http_client TYPE REF TO zcl_grey_http_client.

    " Get current authenticated user
    METHODS get_current_user
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Get user by ID
    METHODS get_user
      IMPORTING
        iv_user_id       TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_http_client TYPE REF TO zcl_grey_http_client.

    METHODS validate_user_id
      IMPORTING
        iv_user_id      TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS parse_user_response
      IMPORTING
        iv_json        TYPE string
      RETURNING
        VALUE(rs_user) TYPE ty_user.
ENDCLASS.

CLASS zcl_grey_user_client IMPLEMENTATION.

  METHOD constructor.
    mo_http_client = io_http_client.
  ENDMETHOD.

  METHOD validate_user_id.
    IF iv_user_id IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'User ID is required' ).
    ENDIF.
  ENDMETHOD.

  METHOD parse_user_response.
    " Simple JSON parsing
    FIND REGEX '"user_id"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_user-user_id.
    FIND REGEX '"id"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_user-user_id.
    FIND REGEX '"email"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_user-email.
    FIND REGEX '"display_name"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_user-display_name.
    FIND REGEX '"name"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_user-display_name.
    FIND REGEX '"tenant_id"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_user-tenant_id.
  ENDMETHOD.

  METHOD get_current_user.
    " Make request
    DATA(lo_http_result) = mo_http_client->get( iv_path = '/api/v1/users/me' ).

    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_user) = parse_user_response( CONV string( <lv_body> ) ).

    " Create result
    DATA: lr_user TYPE REF TO ty_user.
    CREATE DATA lr_user.
    lr_user->* = ls_user.
    ro_result = zcl_grey_result=>ok( lr_user ).
  ENDMETHOD.

  METHOD get_user.
    " Validate input
    DATA(lo_error) = validate_user_id( iv_user_id ).
    IF lo_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_error ).
      RETURN.
    ENDIF.

    " Make request
    DATA(lv_path) = |/api/v1/users/{ iv_user_id }|.
    DATA(lo_http_result) = mo_http_client->get( iv_path = lv_path ).

    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_user) = parse_user_response( CONV string( <lv_body> ) ).

    " Create result
    DATA: lr_user TYPE REF TO ty_user.
    CREATE DATA lr_user.
    lr_user->* = ls_user.
    ro_result = zcl_grey_result=>ok( lr_user ).
  ENDMETHOD.

ENDCLASS.
