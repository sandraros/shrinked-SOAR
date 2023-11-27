**********************************************************************
*
* https://github.com/sandraros/abap-soar
*
**********************************************************************
*
* MIT License
*
* Copyright (c) 2023 sandraros
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
**********************************************************************
CLASS ZCL_SHRINKEDSOAR_SOAR_MANAGER DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES ZIF_SHRINKEDSOAR_SOAR_MANAGER.

    ALIASES create FOR ZIF_SHRINKEDSOAR_SOAR_MANAGER~create.

    CLASS-METHODS create_with_dynamic_provider
      IMPORTING
        srp_id               TYPE csequence
        provider_class_name  TYPE csequence
        provider_method_name TYPE csequence
      RETURNING
        VALUE(result)        TYPE REF TO object
      RAISING
        cx_static_check
        cx_dynamic_check.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_manager,
             srp_id   TYPE ZSHRISOARS,
             instance TYPE REF TO ZCL_SHRINKEDSOAR_SOAR_MANAGER,
           END OF ty_manager.
    TYPES ty_managers TYPE HASHED TABLE OF ty_manager WITH UNIQUE KEY srp_id.

    CLASS-DATA managers TYPE ty_managers.

    DATA srp_id TYPE ZSHRISOARS.
    DATA info_generate_subroutine_pool TYPE ZIF_SHRINKEDSOAR_SOAR_MANAGER=>ty_generate_subroutine_pool.
    DATA provider TYPE REF TO ZIF_SHRINKEDSOAR_SOAR_PROVIDER.
    DATA ZSHRISOAR_INHD TYPE ZSHRISOAR_INHD.

    METHODS check_subroutine_pool
      IMPORTING
        subroutine_pool_name TYPE syrepid
      RAISING
        ZCX_SHRINKEDSOAR_SOAR.

    METHODS generate_subroutine_pool
      IMPORTING
        abap_source_code TYPE ZIF_SHRINKEDSOAR_SOAR_PROVIDER=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE ZIF_SHRINKEDSOAR_SOAR_MANAGER=>ty_generate_subroutine_pool
      RAISING
        ZCX_SHRINKEDSOAR_SOAR.

    METHODS get_hash_key
      IMPORTING
        abap_source_code TYPE ZIF_SHRINKEDSOAR_SOAR_PROVIDER=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE dyn_abap_dte_fingerprint
      RAISING
        cx_abap_message_digest.

    METHODS initialize
      RAISING
        ZCX_SHRINKEDSOAR_SOAR.

ENDCLASS.



CLASS ZCL_SHRINKEDSOAR_SOAR_MANAGER IMPLEMENTATION.

  METHOD check_subroutine_pool.
    TYPES:
      BEGIN OF ty_trdir,
        name TYPE trdir-name,
        subc TYPE trdir-subc,
      END OF ty_trdir.

    " Prevent CVA error:
    "   Security Checks for ABAP (CVA)
    "   Read on sensitive database tables/views
    "   Message number 11G0
    "   A sensitive database table/view was read in a customer system.
    DATA(tabname) = 'TRDIR' ##NO_TEXT.
    TRY.
        tabname = cl_abap_dyn_prg=>check_whitelist_str(
                   val       = tabname
                   whitelist = tabname ).
      CATCH cx_abap_not_in_whitelist INTO DATA(error).
    ENDTRY.

    DATA(trdir) = VALUE ty_trdir( ).
    SELECT SINGLE name, subc
        FROM (tabname)
        WHERE name = @subroutine_pool_name
        INTO @trdir.                                     "#EC CI_DYNTAB

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
        EXPORTING
          text  = 'Subroutine pool &1 does not exist'(004)
          msgv1 = subroutine_pool_name.
    ELSEIF trdir-subc <> 'S'.
      RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
        EXPORTING
          text  = 'Program &1 is of type &2 but should be S (Subroutine pool)'(005)
          msgv1 = subroutine_pool_name
          msgv2 = trdir-subc.
    ENDIF.

  ENDMETHOD.


  METHOD create_with_dynamic_provider.

    result = lcl_manager_w_dynamic_provider=>create(
        srp_id               = srp_id
        provider_class_name  = provider_class_name
        provider_method_name = provider_method_name ).

  ENDMETHOD.


  METHOD generate_subroutine_pool.

    TRY.

        " The next code is added for this Security Check:
        "   Appl. Comp. Check / Check Class / Message Code
        "   BC-ABA-LA-EPC / CL_CI_TEST_EXTENDED_CHECK_SEC / 1108
        "   Details of Analysis
        "   Operand ABAP_SOURCE_CODE in statement GENERATE is an ABAP command injection risk.

        DATA(verified_abap_source_code) = VALUE string_table( ).
        LOOP AT abap_source_code REFERENCE INTO DATA(abap_line).
          TRY.
              DATA(verified_abap_line) = cl_abap_dyn_prg=>check_whitelist_tab(
                              val       = abap_line->*
                              whitelist = VALUE #( ( condense( abap_line->* ) ) ) ).
            CATCH cx_abap_not_in_whitelist INTO DATA(error_2).
              RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
                EXPORTING
                  text = 'SOAR Internal Error. Please contact support.'(012)
                  previous = error_2.
          ENDTRY.
          INSERT verified_abap_line INTO TABLE verified_abap_source_code.
        ENDLOOP.

        GENERATE SUBROUTINE POOL verified_abap_source_code
            NAME         result-name
            MESSAGE      result-message
            LINE         result-line
            WORD         result-word
            INCLUDE      result-include
            MESSAGE-ID   result-message_id
            OFFSET       result-offset
            SHORTDUMP-ID result-shortdump_id.          "#EC CI_GENERATE

        IF sy-subrc <> 0.

          RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
            EXPORTING
              text  = 'Generation error &1 at line &2: &3'(003)
              msgv1 = |{ sy-subrc }|
              msgv2 = |{ result-line }|
              msgv3 = result-message.

        ENDIF.

      CATCH cx_sy_generate_subpool_full
            cx_sy_gen_source_too_wide
            INTO DATA(error).

        RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
          EXPORTING
            text     = 'Global generation error'(011)
            previous = error.

    ENDTRY.

  ENDMETHOD.


  METHOD initialize.

    DATA error TYPE REF TO cx_root.

    SELECT SINGLE *
        FROM ZSHRISOAR_INHD
        INTO @ZSHRISOAR_INHD
        WHERE srp_id = @srp_id.

    IF sy-subrc = 0
        AND ZSHRISOAR_INHD-subroutine_pool_name IS NOT INITIAL
        AND ZSHRISOAR_INHD-inactive = abap_false.

      check_subroutine_pool( ZSHRISOAR_INHD-subroutine_pool_name ).

      ZIF_SHRINKEDSOAR_SOAR_MANAGER~srp_name = ZSHRISOAR_INHD-subroutine_pool_name.

    ELSE.

      TRY.
          DATA(abap_source_code) = provider->get_abap_source_code( srp_id ).
        CATCH ZCX_SHRINKEDSOAR_SOAR INTO error.
          RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
            EXPORTING
              text     = 'Error while getting the ABAP source code'(009)
              previous = error.
      ENDTRY.

      " Calculate the hash key
      TRY.
          DATA(hash_key) = get_hash_key( abap_source_code ).
        CATCH cx_abap_message_digest INTO error.
          RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
            EXPORTING
              text = 'Hash key calculation error. Please contact the support.'(006).
      ENDTRY.

      " Check authorizations for the hash key
      AUTHORITY-CHECK OBJECT 'ZSHRISOARH'
          ID 'ZSHRISOARS'  FIELD srp_id
          ID 'ZSHRISOARH' FIELD hash_key.

      IF sy-subrc <> 0.

        AUTHORITY-CHECK OBJECT 'ZSHRISOARD'
            ID 'ZSHRISOARS'  FIELD srp_id
            ID 'ZSHRISOARD' FIELD sy-datum.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
            EXPORTING
              text  = 'This version of the ABAP code is not authorized (&1 - &2)'(010)
              msgv1 = srp_id
              msgv2 = hash_key.
        ENDIF.
      ENDIF.

      " Generate the subroutine pool
      info_generate_subroutine_pool = generate_subroutine_pool( abap_source_code ).

      ZIF_SHRINKEDSOAR_SOAR_MANAGER~srp_name = info_generate_subroutine_pool-name.

    ENDIF.

  ENDMETHOD.


  METHOD get_hash_key.

    cl_abap_message_digest=>calculate_hash_for_char(
      EXPORTING
        if_algorithm     = 'MD5' ##NO_TEXT
        if_data          = concat_lines_of( sep = |\n| table = abap_source_code )
      IMPORTING
        ef_hashb64string = DATA(md5_hash_key) ).

    result = md5_hash_key.

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_MANAGER~call_static_method.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    "=============
    " Preparation
    "=============

    IF result_object-parameter_name IS NOT INITIAL
        AND NOT line_exists( parameters[ name = result_object-parameter_name ] ).
      " The result parameter is missing, append it (TYPE REF TO OBJECT)
      DATA(parameters_local) = VALUE abap_parmbind_tab(
                      BASE parameters
                      ( name  = result_object-parameter_name
                        value = NEW dba_object_ref( ) ) ).
      ASSIGN parameters_local TO <parameters>.
    ELSE.
      ASSIGN parameters TO <parameters>.
    ENDIF.

    "=============
    " Execution
    "=============

    TRY.

        IF via_perform = abap_false.

          DATA(absolute_name) = |\\PROGRAM={ ZIF_SHRINKEDSOAR_SOAR_MANAGER~srp_name }\\CLASS={ class_name }|.
          CALL METHOD (absolute_name)=>(method_name)
            PARAMETER-TABLE <parameters>
            EXCEPTION-TABLE exceptions_.

        ELSE.

          PERFORM call_static_method IN PROGRAM (ZIF_SHRINKEDSOAR_SOAR_MANAGER~srp_name)
            USING class_name
                  method_name
                  <parameters>
                  exceptions_.

        ENDIF.

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
          EXPORTING
            text     = 'Error in method &2 of class &1'(007)
            msgv1    = class_name
            msgv2    = method_name
            previous = error.
    ENDTRY.

    "=============
    " Follow-up
    "=============

    IF result_object-parameter_name IS NOT INITIAL.

      DATA(ref_returning_parameter) = REF #( <parameters>[ name = result_object-parameter_name ] OPTIONAL ).
      ASSERT ref_returning_parameter IS BOUND.

      ASSIGN ref_returning_parameter->value->* TO FIELD-SYMBOL(<object>).
      TRY.
          result = <object>.
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
            EXPORTING
              text  = 'Receiving variable is of type &1 (must be REF TO OBJECT)'(001)
              msgv1 = cl_abap_typedescr=>describe_by_data( <object> )->absolute_name.
      ENDTRY.

      IF result_object-bound_optional = abap_false
            AND result IS NOT BOUND.
        RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
          EXPORTING
            text = 'Result is not bound'(002).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_MANAGER~create.

    DATA(manager) = REF #( managers[ srp_id = srp_id ] OPTIONAL ).

    IF manager IS NOT BOUND.

      DATA(manager_instance) = NEW ZCL_SHRINKEDSOAR_SOAR_MANAGER( ).

      manager_instance->srp_id = srp_id.
      manager_instance->provider = provider.
      manager_instance->initialize( ).

      INSERT VALUE #(
              srp_id   = srp_id
              instance = manager_instance
          ) INTO TABLE managers
          REFERENCE INTO manager.

    ENDIF.

    result = manager->instance.

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_MANAGER~create_object.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    TRY.

        IF via_perform = abap_false.

          DATA(absolute_name) = |\\PROGRAM={ ZIF_SHRINKEDSOAR_SOAR_MANAGER~srp_name }\\CLASS={ class_name }|.
          CREATE OBJECT result TYPE (absolute_name)
                                PARAMETER-TABLE parameters
                                EXCEPTION-TABLE exceptions_.

        ELSE.

          PERFORM create_object IN PROGRAM (ZIF_SHRINKEDSOAR_SOAR_MANAGER~srp_name)
            USING    class_name
                     parameters
                     exceptions_
            CHANGING result.

        ENDIF.

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR
          EXPORTING
            text     = 'Error in NEW / CREATE OBJECT of class &1'(008)
            msgv1    = class_name
            previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_MANAGER~get_last_generate_subr_pool.

    result = info_generate_subroutine_pool.

  ENDMETHOD.

ENDCLASS.
