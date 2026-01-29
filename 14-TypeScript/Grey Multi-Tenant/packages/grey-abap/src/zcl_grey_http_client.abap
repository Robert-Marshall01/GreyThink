*&---------------------------------------------------------------------*
*& Class ZCL_GREY_HTTP_CLIENT
*& HTTP client wrapper using CL_HTTP_CLIENT
*&---------------------------------------------------------------------*
CLASS zcl_grey_http_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " HTTP method constants
    CONSTANTS:
      BEGIN OF gc_methods,
        get    TYPE string VALUE 'GET',
        post   TYPE string VALUE 'POST',
        put    TYPE string VALUE 'PUT',
        patch  TYPE string VALUE 'PATCH',
        delete TYPE string VALUE 'DELETE',
      END OF gc_methods.

    " Response structure
    TYPES:
      BEGIN OF ty_response,
        status_code TYPE i,
        body        TYPE string,
        headers     TYPE zcl_grey_options=>ty_headers,
      END OF ty_response.

    " Instance attributes
    DATA mo_options TYPE REF TO zcl_grey_options READ-ONLY.

    " Constructor
    METHODS constructor
      IMPORTING
        io_options TYPE REF TO zcl_grey_options.

    " Set auth token
    METHODS set_auth_token
      IMPORTING
        iv_token TYPE string.

    " Clear auth token
    METHODS clear_auth_token.

    " HTTP GET request
    METHODS get
      IMPORTING
        iv_path          TYPE string
        it_query_params  TYPE zcl_grey_options=>ty_headers OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " HTTP POST request
    METHODS post
      IMPORTING
        iv_path          TYPE string
        iv_body          TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " HTTP PUT request
    METHODS put
      IMPORTING
        iv_path          TYPE string
        iv_body          TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " HTTP DELETE request
    METHODS delete
      IMPORTING
        iv_path          TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Generic request method
    METHODS request
      IMPORTING
        iv_method        TYPE string
        iv_path          TYPE string
        iv_body          TYPE string OPTIONAL
        it_query_params  TYPE zcl_grey_options=>ty_headers OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_auth_token TYPE string.

    METHODS build_url
      IMPORTING
        iv_path         TYPE string
        it_query_params TYPE zcl_grey_options=>ty_headers OPTIONAL
      RETURNING
        VALUE(rv_url)   TYPE string.

    METHODS create_http_client
      IMPORTING
        iv_url             TYPE string
      EXPORTING
        eo_client          TYPE REF TO if_http_client
        ev_error           TYPE string
      RETURNING
        VALUE(rv_success)  TYPE abap_bool.

    METHODS set_request_headers
      IMPORTING
        io_client TYPE REF TO if_http_client.

    METHODS parse_response
      IMPORTING
        io_client        TYPE REF TO if_http_client
      RETURNING
        VALUE(rs_response) TYPE ty_response.

    METHODS handle_response
      IMPORTING
        is_response      TYPE ty_response
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.
ENDCLASS.

CLASS zcl_grey_http_client IMPLEMENTATION.

  METHOD constructor.
    mo_options = io_options.
    mv_auth_token = io_options->mv_auth_token.
  ENDMETHOD.

  METHOD set_auth_token.
    mv_auth_token = iv_token.
  ENDMETHOD.

  METHOD clear_auth_token.
    CLEAR mv_auth_token.
  ENDMETHOD.

  METHOD build_url.
    rv_url = |{ mo_options->get_base_url( ) }{ iv_path }|.

    IF it_query_params IS NOT INITIAL.
      DATA(lv_query) = ``.
      LOOP AT it_query_params ASSIGNING FIELD-SYMBOL(<ls_param>).
        IF sy-tabix = 1.
          lv_query = |?{ <ls_param>-name }={ <ls_param>-value }|.
        ELSE.
          lv_query = |{ lv_query }&{ <ls_param>-name }={ <ls_param>-value }|.
        ENDIF.
      ENDLOOP.
      rv_url = |{ rv_url }{ lv_query }|.
    ENDIF.
  ENDMETHOD.

  METHOD create_http_client.
    rv_success = abap_false.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
        ssl_id             = COND #( WHEN mo_options->mv_use_ssl = abap_true THEN 'ANONYM' )
      IMPORTING
        client             = eo_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).

    IF sy-subrc <> 0.
      ev_error = |Failed to create HTTP client: { sy-subrc }|.
      RETURN.
    ENDIF.

    " Set timeout
    eo_client->set_property(
      property = cl_http_client=>co_timeout_response
      value    = CONV string( mo_options->mv_timeout )
    ).

    rv_success = abap_true.
  ENDMETHOD.

  METHOD set_request_headers.
    " Content-Type
    io_client->request->set_header_field(
      name  = 'Content-Type'
      value = 'application/json'
    ).

    " Accept
    io_client->request->set_header_field(
      name  = 'Accept'
      value = 'application/json'
    ).

    " Authorization
    IF mv_auth_token IS NOT INITIAL.
      io_client->request->set_header_field(
        name  = 'Authorization'
        value = |Bearer { mv_auth_token }|
      ).
    ENDIF.

    " Custom headers
    LOOP AT mo_options->mt_custom_headers ASSIGNING FIELD-SYMBOL(<ls_header>).
      io_client->request->set_header_field(
        name  = <ls_header>-name
        value = <ls_header>-value
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_response.
    " Get status code
    io_client->response->get_status(
      IMPORTING
        code = rs_response-status_code
    ).

    " Get response body
    rs_response-body = io_client->response->get_cdata( ).

    " Get response headers (simplified)
    DATA(lv_content_type) = io_client->response->get_header_field( 'Content-Type' ).
    APPEND VALUE #( name = 'Content-Type' value = lv_content_type ) TO rs_response-headers.
  ENDMETHOD.

  METHOD handle_response.
    DATA: lr_body TYPE REF TO data.

    " Check for success status codes (2xx)
    IF is_response-status_code >= 200 AND is_response-status_code < 300.
      " Create reference to body string
      CREATE DATA lr_body TYPE string.
      ASSIGN lr_body->* TO FIELD-SYMBOL(<lv_body>).
      <lv_body> = is_response-body.
      ro_result = zcl_grey_result=>ok( lr_body ).
    ELSE.
      " Create error from HTTP status
      DATA(lo_error) = zcl_grey_error=>from_http_status(
        iv_status  = is_response-status_code
        iv_details = is_response-body
      ).
      ro_result = zcl_grey_result=>fail( lo_error ).
    ENDIF.
  ENDMETHOD.

  METHOD request.
    DATA: lo_client TYPE REF TO if_http_client,
          lv_error  TYPE string.

    " Build URL
    DATA(lv_url) = build_url(
      iv_path         = iv_path
      it_query_params = it_query_params
    ).

    " Create HTTP client
    IF create_http_client(
      EXPORTING iv_url    = lv_url
      IMPORTING eo_client = lo_client
                ev_error  = lv_error
    ) = abap_false.
      ro_result = zcl_grey_result=>fail(
        zcl_grey_error=>network_error( lv_error )
      ).
      RETURN.
    ENDIF.

    " Set HTTP method
    lo_client->request->set_method(
      SWITCH #( iv_method
        WHEN gc_methods-get    THEN 'GET'
        WHEN gc_methods-post   THEN 'POST'
        WHEN gc_methods-put    THEN 'PUT'
        WHEN gc_methods-patch  THEN 'PATCH'
        WHEN gc_methods-delete THEN 'DELETE'
        ELSE 'GET'
      )
    ).

    " Set headers
    set_request_headers( lo_client ).

    " Set request body if provided
    IF iv_body IS NOT INITIAL.
      lo_client->request->set_cdata( iv_body ).
    ENDIF.

    " Send request
    lo_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5
    ).

    IF sy-subrc <> 0.
      lo_client->close( ).
      ro_result = zcl_grey_result=>fail(
        zcl_grey_error=>network_error( |Send failed: { sy-subrc }| )
      ).
      RETURN.
    ENDIF.

    " Receive response
    lo_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4
    ).

    IF sy-subrc <> 0.
      DATA(lv_recv_error) = COND string(
        WHEN sy-subrc = 1 THEN 'Communication failure'
        ELSE |Receive failed: { sy-subrc }|
      ).
      lo_client->close( ).
      ro_result = zcl_grey_result=>fail(
        zcl_grey_error=>network_error( lv_recv_error )
      ).
      RETURN.
    ENDIF.

    " Parse and handle response
    DATA(ls_response) = parse_response( lo_client ).
    lo_client->close( ).

    ro_result = handle_response( ls_response ).
  ENDMETHOD.

  METHOD get.
    ro_result = request(
      iv_method       = gc_methods-get
      iv_path         = iv_path
      it_query_params = it_query_params
    ).
  ENDMETHOD.

  METHOD post.
    ro_result = request(
      iv_method = gc_methods-post
      iv_path   = iv_path
      iv_body   = iv_body
    ).
  ENDMETHOD.

  METHOD put.
    ro_result = request(
      iv_method = gc_methods-put
      iv_path   = iv_path
      iv_body   = iv_body
    ).
  ENDMETHOD.

  METHOD delete.
    ro_result = request(
      iv_method = gc_methods-delete
      iv_path   = iv_path
    ).
  ENDMETHOD.

ENDCLASS.
