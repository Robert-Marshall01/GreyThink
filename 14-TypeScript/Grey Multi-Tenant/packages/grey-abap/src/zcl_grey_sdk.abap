*&---------------------------------------------------------------------*
*& Class ZCL_GREY_SDK
*& Main SDK facade for the Grey Multi-Tenant platform
*&---------------------------------------------------------------------*
CLASS zcl_grey_sdk DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " SDK Version
    CONSTANTS gc_version TYPE string VALUE '1.0.0'.

    " Configuration options (read-only)
    DATA mo_options TYPE REF TO zcl_grey_options READ-ONLY.

    " Domain clients (read-only)
    DATA mo_auth     TYPE REF TO zcl_grey_auth_client READ-ONLY.
    DATA mo_user     TYPE REF TO zcl_grey_user_client READ-ONLY.
    DATA mo_projects TYPE REF TO zcl_grey_projects_client READ-ONLY.
    DATA mo_query    TYPE REF TO zcl_grey_query_client READ-ONLY.
    DATA mo_mutation TYPE REF TO zcl_grey_mutation_client READ-ONLY.

    " Constructor
    METHODS constructor
      IMPORTING
        io_options TYPE REF TO zcl_grey_options.

    " Factory methods
    CLASS-METHODS local_client
      RETURNING
        VALUE(ro_sdk) TYPE REF TO zcl_grey_sdk.

    CLASS-METHODS local_client_with_port
      IMPORTING
        iv_port       TYPE i
      RETURNING
        VALUE(ro_sdk) TYPE REF TO zcl_grey_sdk.

    CLASS-METHODS production_client
      IMPORTING
        iv_host       TYPE string
      RETURNING
        VALUE(ro_sdk) TYPE REF TO zcl_grey_sdk.

    CLASS-METHODS production_client_with_port
      IMPORTING
        iv_host       TYPE string
        iv_port       TYPE i
      RETURNING
        VALUE(ro_sdk) TYPE REF TO zcl_grey_sdk.

    " Set authentication token for all requests
    METHODS set_auth_token
      IMPORTING
        iv_token TYPE string.

    " Clear authentication token
    METHODS clear_auth_token.

    " Get SDK version
    METHODS get_version
      RETURNING
        VALUE(rv_version) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_http_client TYPE REF TO zcl_grey_http_client.
ENDCLASS.

CLASS zcl_grey_sdk IMPLEMENTATION.

  METHOD constructor.
    mo_options = io_options.

    " Create HTTP client
    mo_http_client = NEW zcl_grey_http_client( io_options ).

    " Create domain clients
    mo_auth     = NEW zcl_grey_auth_client( mo_http_client ).
    mo_user     = NEW zcl_grey_user_client( mo_http_client ).
    mo_projects = NEW zcl_grey_projects_client( mo_http_client ).
    mo_query    = NEW zcl_grey_query_client( mo_http_client ).
    mo_mutation = NEW zcl_grey_mutation_client( mo_http_client ).
  ENDMETHOD.

  METHOD local_client.
    ro_sdk = NEW zcl_grey_sdk( zcl_grey_options=>local_options( ) ).
  ENDMETHOD.

  METHOD local_client_with_port.
    ro_sdk = NEW zcl_grey_sdk( zcl_grey_options=>local_options_with_port( iv_port ) ).
  ENDMETHOD.

  METHOD production_client.
    ro_sdk = NEW zcl_grey_sdk( zcl_grey_options=>production_options( iv_host ) ).
  ENDMETHOD.

  METHOD production_client_with_port.
    ro_sdk = NEW zcl_grey_sdk(
      zcl_grey_options=>production_options_with_port(
        iv_host = iv_host
        iv_port = iv_port
      )
    ).
  ENDMETHOD.

  METHOD set_auth_token.
    mo_http_client->set_auth_token( iv_token ).
  ENDMETHOD.

  METHOD clear_auth_token.
    mo_http_client->clear_auth_token( ).
  ENDMETHOD.

  METHOD get_version.
    rv_version = gc_version.
  ENDMETHOD.

ENDCLASS.
