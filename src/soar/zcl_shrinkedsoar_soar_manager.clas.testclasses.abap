*"* use this source file for your ABAP unit test classes

CLASS ltc_generate_subroutine_pool DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    INTERFACES ZIF_SHRINKEDSOAR_SOAR_PROVIDER.

  PRIVATE SECTION.

*    METHODS invalid_hash_key FOR TESTING RAISING cx_static_check.
    METHODS syntax_error FOR TESTING RAISING cx_static_check.

    DATA srp_id TYPE ZSHRISOARS.
    DATA abap_source_code TYPE ZIF_SHRINKEDSOAR_SOAR_PROVIDER=>ty_abap_source_code.

ENDCLASS.


CLASS ltc_generate_subroutine_pool_2 DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    INTERFACES ZIF_SHRINKEDSOAR_SOAR_PROVIDER.

  PRIVATE SECTION.

    METHODS create_object_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS create_object_perform FOR TESTING RAISING cx_static_check.
    METHODS factory_method_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS factory_method_perform FOR TESTING RAISING cx_static_check.

    CONSTANTS srp_id TYPE ZSHRISOARS VALUE 'ZSOAR_MANAGER_TEST_OUTSOURCED'.
    CLASS-DATA abap_source_code TYPE ZIF_SHRINKEDSOAR_SOAR_PROVIDER=>ty_abap_source_code.
    CLASS-DATA manager TYPE REF TO ZIF_SHRINKEDSOAR_SOAR_MANAGER.
    CLASS-DATA provider TYPE REF TO ltc_generate_subroutine_pool_2.
    CLASS-DATA class_setup_exception TYPE REF TO cx_root.

    CLASS-METHODS class_setup.
    METHODS setup.

ENDCLASS.


CLASS ltc_instantiate_inhousedev DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    INTERFACES ZIF_SHRINKEDSOAR_SOAR_PROVIDER.

  PRIVATE SECTION.

    METHODS create_object_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS create_object_perform FOR TESTING RAISING cx_static_check.
    METHODS factory_method_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS factory_method_perform FOR TESTING RAISING cx_static_check.

    CONSTANTS srp_id TYPE ZSHRISOARS VALUE 'ZSOAR_MANAGER_TEST_INHOUSEDEV'.

    CLASS-DATA manager TYPE REF TO ZIF_SHRINKEDSOAR_SOAR_MANAGER.
    CLASS-DATA provider TYPE REF TO ltc_generate_subroutine_pool_2.
    CLASS-DATA class_setup_exception TYPE REF TO cx_root.

    CLASS-METHODS class_setup.
    METHODS setup.

ENDCLASS.


CLASS ltc_generate_subroutine_pool IMPLEMENTATION.

*  METHOD invalid_hash_key.
*
*    srp_id = 'ZSOAR_MANAGER_TEST_OUTSOURCED'.
*    abap_source_code = VALUE #( ( `test` ) ).
*    " This is the part which checks the hash key and does GENERATE SUBROUTINE POOL.
*    TRY.
*        DATA(manager) = ZCL_SHRINKEDSOAR_SOAR_MANAGER=>ZIF_SHRINKEDSOAR_SOAR_MANAGER~create( srp_id   = srp_id
*                                                                   provider = me ).
*      CATCH cx_root INTO DATA(error) ##NO_HANDLER.
*    ENDTRY.
*
*    cl_abap_unit_assert=>assert_bound( error ).
*    cl_abap_unit_assert=>assert_equals( act = error->textid
*                                        exp = ZCX_SHRINKEDSOAR_SOAR=>ZCX_SHRINKEDSOAR_SOAR ).
*    cl_abap_unit_assert=>assert_equals( act = error->get_text( )
*                                        exp = 'Invalid hash key for ZSOAR_MANAGER_TEST_OUTSOURCED. Act: n4bQgYhMfWWaL+qgxVrQFaO/TxsrC4Is0V1sFbDwCgg=. Exp: aaaaaaaaaabbbbbbbbbbb. Please contact the support.' ).
*
*  ENDMETHOD.


  METHOD syntax_error.

    srp_id = 'ZSOAR_MANAGER_TEST_OUTSOURCED'.
    abap_source_code = VALUE #( ( `test` ) ).

    " This is the part which checks the hash key and does GENERATE SUBROUTINE POOL.
    TRY.
        DATA(manager) = ZCL_SHRINKEDSOAR_SOAR_MANAGER=>ZIF_SHRINKEDSOAR_SOAR_MANAGER~create( srp_id   = srp_id
                                                                   provider = me ).
      CATCH cx_root INTO DATA(error) ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( error ).
    cl_abap_unit_assert=>assert_equals( act = error->textid
                                        exp = ZCX_SHRINKEDSOAR_SOAR=>ZCX_SHRINKEDSOAR_SOAR ).
    cl_abap_unit_assert=>assert_equals( act = error->get_text( )
                                        exp = 'Generation error 4 at line 1: The last statement is not complete (period missing).' ).

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_PROVIDER~get_abap_source_code.

    result = abap_source_code.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_generate_subroutine_pool_2 IMPLEMENTATION.

  METHOD class_setup.

    abap_source_code = VALUE #(
            ( `PROGRAM zsoar_manager_test_inhousedev REDUCED FUNCTIONALITY.` )
            ( `INCLUDE zsoar_srpoo_forms.                                  ` )
            ( `CLASS lcl_test DEFINITION.                                  ` )
            ( `  PUBLIC SECTION.                                           ` )
            ( `    INTERFACES ZIF_SHRINKEDSOAR_SOAR_MGR_TEST.                       ` )
            ( `ENDCLASS.                                                   ` )
            ( `CLASS lcl_test IMPLEMENTATION.                              ` )
            ( `  METHOD ZIF_SHRINKEDSOAR_SOAR_MGR_TEST~create.                      ` )
            ( `    result = NEW lcl_test( ).                               ` )
            ( `  ENDMETHOD.                                                ` )
            ( `  METHOD ZIF_SHRINKEDSOAR_SOAR_MGR_TEST~square.                      ` )
            ( `    result = number ** 2.                                   ` )
            ( `  ENDMETHOD.                                                ` )
            ( `ENDCLASS.                                                   ` ) ).

    provider = NEW ltc_generate_subroutine_pool_2( ).

    TRY.
        manager = ZCL_SHRINKEDSOAR_SOAR_MANAGER=>ZIF_SHRINKEDSOAR_SOAR_MANAGER~create( srp_id   = srp_id
                                                             provider = provider ).
      CATCH ZCX_SHRINKEDSOAR_SOAR INTO class_setup_exception.
        RETURN.
    ENDTRY.

    cl_abap_unit_assert=>assert_char_cp( act = manager->srp_name
                                         exp = '%_T*' ).
    " Warning message
    cl_abap_unit_assert=>fail( msg   = |NOT AN ERROR, is for information. Generated subroutine pool: { manager->srp_name }|
                               level = if_aunit_constants=>tolerable
                               quit  = if_aunit_constants=>no ).

  ENDMETHOD.


  METHOD create_object_absolute_type.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD create_object_perform.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_absolute_type.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SHRINKEDSOAR_SOAR_MGR_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_perform.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SHRINKEDSOAR_SOAR_MGR_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD setup.

    cl_abap_unit_assert=>assert_not_bound( act = class_setup_exception msg = 'Exception during CLASS_SETUP' ).

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_PROVIDER~get_abap_source_code.

    result = abap_source_code.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_instantiate_inhousedev IMPLEMENTATION.

  METHOD class_setup.

    TYPES ty_table_zsoar_inhousedev TYPE STANDARD TABLE OF ZSHRISOAR_INHD WITH EMPTY KEY.

    DATA(table_zsoar_inhousedev) = VALUE ty_table_zsoar_inhousedev(
        ( srp_id               = 'ZSOAR_MANAGER_TEST_INHOUSEDEV'
          subroutine_pool_name = 'ZSOAR_MANAGER_TEST_INHOUSEDEV'
          inactive             = abap_false ) ).

    MODIFY ZSHRISOAR_INHD FROM TABLE @table_zsoar_inhousedev.
    IF sy-subrc <> 0.
      TRY.
          RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR EXPORTING text = 'MODIFY ZSHRISOAR_INHD failed'(013).
        CATCH ZCX_SHRINKEDSOAR_SOAR INTO class_setup_exception.
          RETURN.
      ENDTRY.
    ENDIF.

    provider = NEW ltc_generate_subroutine_pool_2( ).

    TRY.
        manager = ZCL_SHRINKEDSOAR_SOAR_MANAGER=>ZIF_SHRINKEDSOAR_SOAR_MANAGER~create( srp_id   = srp_id
                                                             provider = provider ).
      CATCH ZCX_SHRINKEDSOAR_SOAR INTO class_setup_exception.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD create_object_absolute_type.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD create_object_perform.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_absolute_type.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SHRINKEDSOAR_SOAR_MGR_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_perform.

    DATA(test) = CAST ZIF_SHRINKEDSOAR_SOAR_MGR_TEST( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SHRINKEDSOAR_SOAR_MGR_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD setup.

    cl_abap_unit_assert=>assert_not_bound( act = class_setup_exception msg = 'Exception during CLASS_SETUP' ).

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_PROVIDER~get_abap_source_code.

    cl_abap_unit_assert=>fail( msg = 'GET_ABAP_SOURCE_CODE is called (should not)' ).

  ENDMETHOD.

ENDCLASS.
