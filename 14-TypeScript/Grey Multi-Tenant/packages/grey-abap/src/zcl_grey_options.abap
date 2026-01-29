*&---------------------------------------------------------------------*
*& Class ZCL_GREY_OPTIONS
*& Configuration options for the Grey SDK
*&---------------------------------------------------------------------*
CLASS zcl_grey_options DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Configuration structure
    TYPES:
      BEGIN OF ty_header,
        name  TYPE string,
        value TYPE string,
      END OF ty_header,
      ty_headers TYPE STANDARD TABLE OF ty_header WITH DEFAULT KEY.

    " Instance attributes
    DATA mv_host           TYPE string READ-ONLY.
    DATA mv_port           TYPE i READ-ONLY.
    DATA mv_use_ssl        TYPE abap_bool READ-ONLY.
    DATA mv_timeout        TYPE i READ-ONLY.
    DATA mv_auth_token     TYPE string READ-ONLY.
    DATA mt_custom_headers TYPE ty_headers READ-ONLY.

    " Constructor
    METHODS constructor
      IMPORTING
        iv_host           TYPE string DEFAULT 'localhost'
        iv_port           TYPE i DEFAULT 8080
        iv_use_ssl        TYPE abap_bool DEFAULT abap_false
        iv_timeout        TYPE i DEFAULT 30
        iv_auth_token     TYPE string OPTIONAL
        it_custom_headers TYPE ty_headers OPTIONAL.

    " Factory methods
    CLASS-METHODS local_options
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    CLASS-METHODS local_options_with_port
      IMPORTING
        iv_port           TYPE i
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    CLASS-METHODS production_options
      IMPORTING
        iv_host           TYPE string
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    CLASS-METHODS production_options_with_port
      IMPORTING
        iv_host           TYPE string
        iv_port           TYPE i
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    " Builder methods
    METHODS with_auth_token
      IMPORTING
        iv_token          TYPE string
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    METHODS with_timeout
      IMPORTING
        iv_timeout        TYPE i
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    METHODS with_header
      IMPORTING
        iv_name           TYPE string
        iv_value          TYPE string
      RETURNING
        VALUE(ro_options) TYPE REF TO zcl_grey_options.

    " Get the base URL
    METHODS get_base_url
      RETURNING
        VALUE(rv_url) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_grey_options IMPLEMENTATION.

  METHOD constructor.
    mv_host = iv_host.
    mv_port = iv_port.
    mv_use_ssl = iv_use_ssl.
    mv_timeout = iv_timeout.
    mv_auth_token = iv_auth_token.
    mt_custom_headers = it_custom_headers.
  ENDMETHOD.

  METHOD local_options.
    ro_options = NEW zcl_grey_options( ).
  ENDMETHOD.

  METHOD local_options_with_port.
    ro_options = NEW zcl_grey_options( iv_port = iv_port ).
  ENDMETHOD.

  METHOD production_options.
    ro_options = NEW zcl_grey_options(
      iv_host    = iv_host
      iv_port    = 443
      iv_use_ssl = abap_true
    ).
  ENDMETHOD.

  METHOD production_options_with_port.
    ro_options = NEW zcl_grey_options(
      iv_host    = iv_host
      iv_port    = iv_port
      iv_use_ssl = abap_true
    ).
  ENDMETHOD.

  METHOD with_auth_token.
    ro_options = NEW zcl_grey_options(
      iv_host           = mv_host
      iv_port           = mv_port
      iv_use_ssl        = mv_use_ssl
      iv_timeout        = mv_timeout
      iv_auth_token     = iv_token
      it_custom_headers = mt_custom_headers
    ).
  ENDMETHOD.

  METHOD with_timeout.
    ro_options = NEW zcl_grey_options(
      iv_host           = mv_host
      iv_port           = mv_port
      iv_use_ssl        = mv_use_ssl
      iv_timeout        = iv_timeout
      iv_auth_token     = mv_auth_token
      it_custom_headers = mt_custom_headers
    ).
  ENDMETHOD.

  METHOD with_header.
    DATA(lt_headers) = mt_custom_headers.
    APPEND VALUE #( name = iv_name value = iv_value ) TO lt_headers.

    ro_options = NEW zcl_grey_options(
      iv_host           = mv_host
      iv_port           = mv_port
      iv_use_ssl        = mv_use_ssl
      iv_timeout        = mv_timeout
      iv_auth_token     = mv_auth_token
      it_custom_headers = lt_headers
    ).
  ENDMETHOD.

  METHOD get_base_url.
    DATA(lv_protocol) = COND string(
      WHEN mv_use_ssl = abap_true THEN 'https'
      ELSE 'http'
    ).

    rv_url = |{ lv_protocol }://{ mv_host }:{ mv_port }|.
  ENDMETHOD.

ENDCLASS.
