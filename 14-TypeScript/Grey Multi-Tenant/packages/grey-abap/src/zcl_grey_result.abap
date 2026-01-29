*&---------------------------------------------------------------------*
*& Class ZCL_GREY_RESULT
*& Result type for operation outcomes (success or failure)
*&---------------------------------------------------------------------*
CLASS zcl_grey_result DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    " Instance attributes
    DATA mo_value TYPE REF TO data READ-ONLY.
    DATA mo_error TYPE REF TO zcl_grey_error READ-ONLY.

    " Check if result is success
    METHODS is_success
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

    " Check if result is failure
    METHODS is_failure
      RETURNING
        VALUE(rv_failure) TYPE abap_bool.

    " Get value or default
    METHODS get_or
      IMPORTING
        io_default      TYPE REF TO data
      RETURNING
        VALUE(ro_value) TYPE REF TO data.

    " Factory method for success
    CLASS-METHODS ok
      IMPORTING
        io_value        TYPE REF TO data
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Factory method for failure
    CLASS-METHODS fail
      IMPORTING
        io_error        TYPE REF TO zcl_grey_error
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

    " Factory for failure with components
    CLASS-METHODS fail_with
      IMPORTING
        iv_code         TYPE string
        iv_message      TYPE string
        iv_details      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_result) TYPE REF TO zcl_grey_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        io_value TYPE REF TO data OPTIONAL
        io_error TYPE REF TO zcl_grey_error OPTIONAL.
ENDCLASS.

CLASS zcl_grey_result IMPLEMENTATION.

  METHOD constructor.
    mo_value = io_value.
    mo_error = io_error.
  ENDMETHOD.

  METHOD is_success.
    rv_success = xsdbool( mo_error IS NOT BOUND ).
  ENDMETHOD.

  METHOD is_failure.
    rv_failure = xsdbool( mo_error IS BOUND ).
  ENDMETHOD.

  METHOD get_or.
    IF is_success( ).
      ro_value = mo_value.
    ELSE.
      ro_value = io_default.
    ENDIF.
  ENDMETHOD.

  METHOD ok.
    ro_result = NEW zcl_grey_result( io_value = io_value ).
  ENDMETHOD.

  METHOD fail.
    ro_result = NEW zcl_grey_result( io_error = io_error ).
  ENDMETHOD.

  METHOD fail_with.
    DATA(lo_error) = NEW zcl_grey_error(
      iv_code    = iv_code
      iv_message = iv_message
      iv_details = iv_details
    ).
    ro_result = NEW zcl_grey_result( io_error = lo_error ).
  ENDMETHOD.

ENDCLASS.
