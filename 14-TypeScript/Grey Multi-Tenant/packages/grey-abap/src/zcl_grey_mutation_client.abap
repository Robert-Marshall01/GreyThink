*&---------------------------------------------------------------------*
*& Class ZCL_GREY_MUTATION_CLIENT
*& Domain client for mutation operations
*&---------------------------------------------------------------------*
CLASS zcl_grey_mutation_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Mutation result structure
    TYPES:
      BEGIN OF ty_mutation_result,
        success  TYPE abap_bool,
        message  TYPE string,
        data     TYPE string,  " Raw JSON data
        metadata TYPE string,  " Raw JSON metadata
      END OF ty_mutation_result.

    " Mutation parameter (same as query)
    TYPES:
      BEGIN OF ty_param,
        name  TYPE string,
        value TYPE string,
      END OF ty_param,
      ty_params TYPE STANDARD TABLE OF ty_param WITH DEFAULT KEY.

    " Constructor
    METHODS constructor
      IMPORTING
        io_http_client TYPE REF TO zcl_grey_http_client.

    " Execute a mutation
    METHODS mutate
      IMPORTING
        iv_mutation_name TYPE string
        it_parameters    TYPE ty_params OPTIONAL
        iv_tenant_id     TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_http_client TYPE REF TO zcl_grey_http_client.

    METHODS validate_mutation_name
      IMPORTING
        iv_name         TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS build_mutation_json
      IMPORTING
        iv_mutation_name TYPE string
        it_parameters    TYPE ty_params OPTIONAL
        iv_tenant_id     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_json)   TYPE string.

    METHODS parse_mutation_response
      IMPORTING
        iv_json          TYPE string
      RETURNING
        VALUE(rs_result) TYPE ty_mutation_result.
ENDCLASS.

CLASS zcl_grey_mutation_client IMPLEMENTATION.

  METHOD constructor.
    mo_http_client = io_http_client.
  ENDMETHOD.

  METHOD validate_mutation_name.
    IF iv_name IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'Mutation name is required' ).
      RETURN.
    ENDIF.

    " Check alphanumeric with underscores, starting with letter
    DATA(lv_first_char) = iv_name(1).
    IF NOT ( lv_first_char CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz' ).
      ro_error = zcl_grey_error=>validation_error(
        'Mutation name must start with a letter'
      ).
      RETURN.
    ENDIF.

    " Check remaining characters
    DATA(lv_len) = strlen( iv_name ).
    DATA(lv_idx) = 1.
    WHILE lv_idx < lv_len.
      DATA(lv_char) = iv_name+lv_idx(1).
      IF NOT ( lv_char CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_' ).
        ro_error = zcl_grey_error=>validation_error(
          'Mutation name must be alphanumeric with underscores'
        ).
        RETURN.
      ENDIF.
      lv_idx = lv_idx + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD build_mutation_json.
    rv_json = |\{ "mutation_name": "{ iv_mutation_name }"|.

    " Add parameters
    IF it_parameters IS NOT INITIAL.
      rv_json = |{ rv_json }, "parameters": \{|.
      DATA(lv_first) = abap_true.
      LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<ls_param>).
        IF lv_first = abap_true.
          rv_json = |{ rv_json } "{ <ls_param>-name }": "{ <ls_param>-value }"|.
          lv_first = abap_false.
        ELSE.
          rv_json = |{ rv_json }, "{ <ls_param>-name }": "{ <ls_param>-value }"|.
        ENDIF.
      ENDLOOP.
      rv_json = |{ rv_json } \}|.
    ELSE.
      rv_json = |{ rv_json }, "parameters": \{\}|.
    ENDIF.

    IF iv_tenant_id IS NOT INITIAL.
      rv_json = |{ rv_json }, "tenant_id": "{ iv_tenant_id }"|.
    ENDIF.

    rv_json = |{ rv_json } \}|.
  ENDMETHOD.

  METHOD parse_mutation_response.
    " Extract success flag
    DATA lv_success_str TYPE string.
    FIND REGEX '"success"\s*:\s*(true|false)' IN iv_json SUBMATCHES lv_success_str.
    rs_result-success = xsdbool( lv_success_str = 'true' ).

    " Extract message
    FIND REGEX '"message"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_result-message.

    " Extract data section (simplified)
    DATA: lv_start TYPE i,
          lv_end   TYPE i.

    FIND '"data"' IN iv_json MATCH OFFSET lv_start.
    IF sy-subrc = 0.
      FIND ':' IN iv_json+lv_start MATCH OFFSET lv_end.
      IF sy-subrc = 0.
        DATA(lv_data_start) = lv_start + lv_end + 1.
        FIND '"metadata"' IN iv_json+lv_data_start MATCH OFFSET lv_end.
        IF sy-subrc = 0.
          rs_result-data = iv_json+lv_data_start(lv_end).
          CONDENSE rs_result-data.
          IF rs_result-data CP '*,'.
            DATA(lv_len) = strlen( rs_result-data ) - 1.
            rs_result-data = rs_result-data(lv_len).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " Extract metadata
    FIND '"metadata"' IN iv_json MATCH OFFSET lv_start.
    IF sy-subrc = 0.
      FIND ':' IN iv_json+lv_start MATCH OFFSET lv_end.
      IF sy-subrc = 0.
        rs_result-metadata = iv_json+lv_start.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD mutate.
    " Validate mutation name
    DATA(lo_error) = validate_mutation_name( iv_mutation_name ).
    IF lo_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_error ).
      RETURN.
    ENDIF.

    " Build request body
    DATA(lv_body) = build_mutation_json(
      iv_mutation_name = iv_mutation_name
      it_parameters    = it_parameters
      iv_tenant_id     = iv_tenant_id
    ).

    " Make request
    DATA(lo_http_result) = mo_http_client->post(
      iv_path = '/api/v1/mutate'
      iv_body = lv_body
    ).

    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_mutation_result) = parse_mutation_response( CONV string( <lv_body> ) ).

    " Create result
    DATA: lr_result TYPE REF TO ty_mutation_result.
    CREATE DATA lr_result.
    lr_result->* = ls_mutation_result.
    ro_result = zcl_grey_result=>ok( lr_result ).
  ENDMETHOD.

ENDCLASS.
