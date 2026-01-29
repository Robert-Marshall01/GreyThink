*&---------------------------------------------------------------------*
*& Class ZCL_GREY_PROJECTS_CLIENT
*& Domain client for project operations
*&---------------------------------------------------------------------*
CLASS zcl_grey_projects_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Project structure
    TYPES:
      BEGIN OF ty_project,
        project_id  TYPE string,
        name        TYPE string,
        description TYPE string,
        tenant_id   TYPE string,
        created_at  TYPE string,
        updated_at  TYPE string,
      END OF ty_project,
      ty_projects TYPE STANDARD TABLE OF ty_project WITH DEFAULT KEY.

    " Project list response
    TYPES:
      BEGIN OF ty_project_list,
        projects TYPE ty_projects,
        total    TYPE i,
      END OF ty_project_list.

    " Create project request
    TYPES:
      BEGIN OF ty_create_request,
        name        TYPE string,
        description TYPE string,
        tenant_id   TYPE string,
      END OF ty_create_request.

    " Constructor
    METHODS constructor
      IMPORTING
        io_http_client TYPE REF TO zcl_grey_http_client.

    " List projects with pagination
    METHODS list_projects
      IMPORTING
        iv_limit         TYPE i DEFAULT 10
        iv_offset        TYPE i DEFAULT 0
        iv_tenant_id     TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Create a new project
    METHODS create_project
      IMPORTING
        iv_name          TYPE string
        iv_description   TYPE string OPTIONAL
        iv_tenant_id     TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Get project by ID
    METHODS get_project
      IMPORTING
        iv_project_id    TYPE string
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_http_client TYPE REF TO zcl_grey_http_client.

    METHODS validate_project_name
      IMPORTING
        iv_name         TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS validate_project_id
      IMPORTING
        iv_project_id   TYPE string
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS validate_pagination
      IMPORTING
        iv_limit        TYPE i
        iv_offset       TYPE i
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    METHODS build_create_json
      IMPORTING
        iv_name        TYPE string
        iv_description TYPE string OPTIONAL
        iv_tenant_id   TYPE string OPTIONAL
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS parse_project_response
      IMPORTING
        iv_json           TYPE string
      RETURNING
        VALUE(rs_project) TYPE ty_project.

    METHODS parse_projects_list
      IMPORTING
        iv_json        TYPE string
      RETURNING
        VALUE(rs_list) TYPE ty_project_list.
ENDCLASS.

CLASS zcl_grey_projects_client IMPLEMENTATION.

  METHOD constructor.
    mo_http_client = io_http_client.
  ENDMETHOD.

  METHOD validate_project_name.
    IF iv_name IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'Project name is required' ).
      RETURN.
    ENDIF.

    IF strlen( iv_name ) < 3.
      ro_error = zcl_grey_error=>validation_error( 'Project name must be at least 3 characters' ).
      RETURN.
    ENDIF.

    IF strlen( iv_name ) > 100.
      ro_error = zcl_grey_error=>validation_error( 'Project name must be at most 100 characters' ).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD validate_project_id.
    IF iv_project_id IS INITIAL.
      ro_error = zcl_grey_error=>validation_error( 'Project ID is required' ).
    ENDIF.
  ENDMETHOD.

  METHOD validate_pagination.
    IF iv_limit < 1 OR iv_limit > 100.
      ro_error = zcl_grey_error=>validation_error( 'Limit must be between 1 and 100' ).
      RETURN.
    ENDIF.

    IF iv_offset < 0.
      ro_error = zcl_grey_error=>validation_error( 'Offset must be non-negative' ).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD build_create_json.
    rv_json = |\{ "name": "{ iv_name }"|.

    IF iv_description IS NOT INITIAL.
      rv_json = |{ rv_json }, "description": "{ iv_description }"|.
    ENDIF.

    IF iv_tenant_id IS NOT INITIAL.
      rv_json = |{ rv_json }, "tenant_id": "{ iv_tenant_id }"|.
    ENDIF.

    rv_json = |{ rv_json } \}|.
  ENDMETHOD.

  METHOD parse_project_response.
    FIND REGEX '"project_id"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-project_id.
    FIND REGEX '"id"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-project_id.
    FIND REGEX '"name"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-name.
    FIND REGEX '"description"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-description.
    FIND REGEX '"tenant_id"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-tenant_id.
    FIND REGEX '"created_at"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-created_at.
    FIND REGEX '"updated_at"\s*:\s*"([^"]*)"' IN iv_json SUBMATCHES rs_project-updated_at.
  ENDMETHOD.

  METHOD parse_projects_list.
    " Extract total
    DATA lv_total_str TYPE string.
    FIND REGEX '"total"\s*:\s*(\d+)' IN iv_json SUBMATCHES lv_total_str.
    IF lv_total_str IS NOT INITIAL.
      rs_list-total = CONV i( lv_total_str ).
    ENDIF.

    " Simple parsing for array items - finds each project object
    " In production, use proper JSON library like /ui2/cl_json
    DATA: lv_start TYPE i,
          lv_end   TYPE i,
          lv_item  TYPE string.

    " Find projects array
    FIND '"projects"' IN iv_json MATCH OFFSET lv_start.
    IF sy-subrc = 0.
      " Parse individual project objects (simplified)
      DATA(lv_remaining) = iv_json+lv_start.
      WHILE lv_remaining CS '{"'.
        FIND FIRST OCCURRENCE OF '{"' IN lv_remaining MATCH OFFSET lv_start.
        IF sy-subrc = 0.
          " Find matching closing brace (simplified - assumes no nested objects)
          FIND FIRST OCCURRENCE OF '"}' IN lv_remaining+lv_start MATCH OFFSET lv_end.
          IF sy-subrc = 0.
            lv_item = lv_remaining+lv_start(lv_end).
            lv_item = |{ lv_item }"\}|.
            APPEND parse_project_response( lv_item ) TO rs_list-projects.
            lv_remaining = lv_remaining+lv_start.
            SHIFT lv_remaining BY ( lv_end + 2 ) PLACES LEFT.
          ELSE.
            EXIT.
          ENDIF.
        ELSE.
          EXIT.
        ENDIF.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.

  METHOD list_projects.
    " Validate pagination
    DATA(lo_error) = validate_pagination(
      iv_limit  = iv_limit
      iv_offset = iv_offset
    ).
    IF lo_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_error ).
      RETURN.
    ENDIF.

    " Build query parameters
    DATA(lt_params) = VALUE zcl_grey_options=>ty_headers(
      ( name = 'limit'  value = CONV string( iv_limit ) )
      ( name = 'offset' value = CONV string( iv_offset ) )
    ).

    IF iv_tenant_id IS NOT INITIAL.
      APPEND VALUE #( name = 'tenant_id' value = iv_tenant_id ) TO lt_params.
    ENDIF.

    " Make request
    DATA(lo_http_result) = mo_http_client->get(
      iv_path         = '/api/v1/projects'
      it_query_params = lt_params
    ).

    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_list) = parse_projects_list( CONV string( <lv_body> ) ).

    " Create result
    DATA: lr_list TYPE REF TO ty_project_list.
    CREATE DATA lr_list.
    lr_list->* = ls_list.
    ro_result = zcl_grey_result=>ok( lr_list ).
  ENDMETHOD.

  METHOD create_project.
    " Validate name
    DATA(lo_error) = validate_project_name( iv_name ).
    IF lo_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_error ).
      RETURN.
    ENDIF.

    " Build request body
    DATA(lv_body) = build_create_json(
      iv_name        = iv_name
      iv_description = iv_description
      iv_tenant_id   = iv_tenant_id
    ).

    " Make request
    DATA(lo_http_result) = mo_http_client->post(
      iv_path = '/api/v1/projects'
      iv_body = lv_body
    ).

    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_project) = parse_project_response( CONV string( <lv_body> ) ).

    " Create result
    DATA: lr_project TYPE REF TO ty_project.
    CREATE DATA lr_project.
    lr_project->* = ls_project.
    ro_result = zcl_grey_result=>ok( lr_project ).
  ENDMETHOD.

  METHOD get_project.
    " Validate ID
    DATA(lo_error) = validate_project_id( iv_project_id ).
    IF lo_error IS BOUND.
      ro_result = zcl_grey_result=>fail( lo_error ).
      RETURN.
    ENDIF.

    " Make request
    DATA(lv_path) = |/api/v1/projects/{ iv_project_id }|.
    DATA(lo_http_result) = mo_http_client->get( iv_path = lv_path ).

    IF lo_http_result->is_failure( ).
      ro_result = lo_http_result.
      RETURN.
    ENDIF.

    " Parse response
    ASSIGN lo_http_result->mo_value->* TO FIELD-SYMBOL(<lv_body>).
    DATA(ls_project) = parse_project_response( CONV string( <lv_body> ) ).

    " Create result
    DATA: lr_project TYPE REF TO ty_project.
    CREATE DATA lr_project.
    lr_project->* = ls_project.
    ro_result = zcl_grey_result=>ok( lr_project ).
  ENDMETHOD.

ENDCLASS.
