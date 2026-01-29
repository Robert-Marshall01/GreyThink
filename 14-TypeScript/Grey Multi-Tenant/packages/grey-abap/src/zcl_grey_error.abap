*&---------------------------------------------------------------------*
*& Class ZCL_GREY_ERROR
*& Normalized error type for the Grey SDK
*&---------------------------------------------------------------------*
CLASS zcl_grey_error DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Error code constants
    CONSTANTS:
      BEGIN OF gc_codes,
        unauthorized     TYPE string VALUE 'unauthorized',
        forbidden        TYPE string VALUE 'forbidden',
        not_found        TYPE string VALUE 'not_found',
        validation_error TYPE string VALUE 'validation_error',
        network_error    TYPE string VALUE 'network_error',
        timeout          TYPE string VALUE 'timeout',
        server_error     TYPE string VALUE 'server_error',
        unknown          TYPE string VALUE 'unknown',
      END OF gc_codes.

    " Error structure
    TYPES:
      BEGIN OF ty_error,
        code    TYPE string,
        message TYPE string,
        details TYPE string,
      END OF ty_error.

    " Instance attributes
    DATA mv_code    TYPE string READ-ONLY.
    DATA mv_message TYPE string READ-ONLY.
    DATA mv_details TYPE string READ-ONLY.

    " Constructor
    METHODS constructor
      IMPORTING
        iv_code    TYPE string
        iv_message TYPE string
        iv_details TYPE string OPTIONAL.

    " Factory methods
    CLASS-METHODS unauthorized
      IMPORTING
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS forbidden
      IMPORTING
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS not_found
      IMPORTING
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS validation_error
      IMPORTING
        iv_message      TYPE string
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS network_error
      IMPORTING
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS timeout
      IMPORTING
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS server_error
      IMPORTING
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    CLASS-METHODS from_http_status
      IMPORTING
        iv_status       TYPE i
        iv_message      TYPE string OPTIONAL
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_error) TYPE REF TO zcl_grey_error.

    " Instance methods
    METHODS is_retryable
      RETURNING
        VALUE(rv_retryable) TYPE abap_bool.

    METHODS to_structure
      RETURNING
        VALUE(rs_error) TYPE ty_error.

    METHODS to_string
      RETURNING
        VALUE(rv_string) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS get_message_for_code
      IMPORTING
        iv_code          TYPE string
      RETURNING
        VALUE(rv_message) TYPE string.
ENDCLASS.

CLASS zcl_grey_error IMPLEMENTATION.

  METHOD constructor.
    mv_code = iv_code.
    mv_message = iv_message.
    mv_details = iv_details.
  ENDMETHOD.

  METHOD get_message_for_code.
    CASE iv_code.
      WHEN gc_codes-unauthorized.
        rv_message = 'Authentication required or token invalid'.
      WHEN gc_codes-forbidden.
        rv_message = 'Access denied to the requested resource'.
      WHEN gc_codes-not_found.
        rv_message = 'Requested resource was not found'.
      WHEN gc_codes-validation_error.
        rv_message = 'Request validation failed'.
      WHEN gc_codes-network_error.
        rv_message = 'Network connectivity issue'.
      WHEN gc_codes-timeout.
        rv_message = 'Request timed out'.
      WHEN gc_codes-server_error.
        rv_message = 'Server returned an error'.
      WHEN OTHERS.
        rv_message = 'Unknown or unexpected error'.
    ENDCASE.
  ENDMETHOD.

  METHOD unauthorized.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-unauthorized
      iv_message = get_message_for_code( gc_codes-unauthorized )
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD forbidden.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-forbidden
      iv_message = get_message_for_code( gc_codes-forbidden )
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD not_found.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-not_found
      iv_message = get_message_for_code( gc_codes-not_found )
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD validation_error.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-validation_error
      iv_message = iv_message
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD network_error.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-network_error
      iv_message = get_message_for_code( gc_codes-network_error )
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD timeout.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-timeout
      iv_message = get_message_for_code( gc_codes-timeout )
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD server_error.
    ro_error = NEW zcl_grey_error(
      iv_code    = gc_codes-server_error
      iv_message = get_message_for_code( gc_codes-server_error )
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD from_http_status.
    DATA: lv_code TYPE string.

    CASE iv_status.
      WHEN 401.
        lv_code = gc_codes-unauthorized.
      WHEN 403.
        lv_code = gc_codes-forbidden.
      WHEN 404.
        lv_code = gc_codes-not_found.
      WHEN 400 OR 422.
        lv_code = gc_codes-validation_error.
      WHEN 408.
        lv_code = gc_codes-timeout.
      WHEN OTHERS.
        IF iv_status >= 500 AND iv_status < 600.
          lv_code = gc_codes-server_error.
        ELSE.
          lv_code = gc_codes-unknown.
        ENDIF.
    ENDCASE.

    DATA(lv_message) = COND string(
      WHEN iv_message IS NOT INITIAL
      THEN iv_message
      ELSE get_message_for_code( lv_code )
    ).

    ro_error = NEW zcl_grey_error(
      iv_code    = lv_code
      iv_message = lv_message
      iv_details = iv_details
    ).
  ENDMETHOD.

  METHOD is_retryable.
    rv_retryable = xsdbool(
      mv_code = gc_codes-network_error OR
      mv_code = gc_codes-timeout OR
      mv_code = gc_codes-server_error
    ).
  ENDMETHOD.

  METHOD to_structure.
    rs_error-code = mv_code.
    rs_error-message = mv_message.
    rs_error-details = mv_details.
  ENDMETHOD.

  METHOD to_string.
    IF mv_details IS NOT INITIAL.
      rv_string = |Grey Error [{ mv_code }]: { mv_message } - { mv_details }|.
    ELSE.
      rv_string = |Grey Error [{ mv_code }]: { mv_message }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
