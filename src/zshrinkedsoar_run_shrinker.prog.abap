*&---------------------------------------------------------------------*
*& Report zshrinkedsoar_run_shrinker
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinkedsoar_run_shrinker.


DATA: BEGIN OF dummy_select_options,
        devclass TYPE devclass,
      END OF dummy_select_options.


* General information
* --------------------

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_frdevc VISIBLE LENGTH 60.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_frdevc TYPE devclass DEFAULT '$SOAR'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_todevc VISIBLE LENGTH 60.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_todevc TYPE devclass DEFAULT '$SHRINKEDSOAR_SOAR'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_trkorr VISIBLE LENGTH 60.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_trkorr TYPE trkorr DEFAULT ''.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_trace VISIBLE LENGTH 60.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_trace TYPE string LOWER CASE DEFAULT ''. " C:\temp\abapgit.zip
SELECTION-SCREEN END OF LINE.


* Create many objects with the same names:
* (authorization field and object, domain, data element)
* --------------------

SELECTION-SCREEN COMMENT /1(80) t_eponym.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_date VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_date TYPE xufield DEFAULT 'ZSHRISOARD'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_hash VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_hash TYPE xufield DEFAULT 'ZSHRISOARH'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_srp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_srp TYPE xufield DEFAULT 'ZSHRISOARS'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_srp_id VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_srp_id TYPE rollname DEFAULT 'ZSHRISOARS'.
SELECTION-SCREEN END OF LINE.


* tables
* --------------------
* ZSOAR_INHOUSEDEV

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_tabl_1 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_tabl_1 TYPE tabname DEFAULT 'ZSHRISOAR_INHD'.
SELECTION-SCREEN END OF LINE.


* classes
* --------------------
* ZCL_SOAR_MANAGER
* ZCX_SOAR

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_clas_1 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_clas_1 TYPE seoclsname DEFAULT 'ZCL_SHRINKEDSOAR_SOAR_MANAGER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_clas_2 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_clas_2 TYPE seoclsname DEFAULT 'ZCX_SHRINKEDSOAR_SOAR'.
SELECTION-SCREEN END OF LINE.


* interfaces
* --------------------
* ZIF_SOAR_MANAGER
* ZIF_SOAR_PROVIDER
* ZIF_SOAR_MANAGER_TEST

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_intf_1 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_intf_1 TYPE seoclsname DEFAULT 'ZIF_SHRINKEDSOAR_SOAR_MANAGER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_intf_2 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_intf_2 TYPE seoclsname DEFAULT 'ZIF_SHRINKEDSOAR_SOAR_PROVIDER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_intf_3 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_intf_3 TYPE seoclsname DEFAULT 'ZIF_SHRINKEDSOAR_SOAR_MGR_TEST'.
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  t_frdevc = 'Package containing https://github/sandraros/abap-soar objects'(t12).
  t_todevc = 'Target package in which new objects are to be created'(t18).
  t_trkorr = 'Transport request if the target package is transportable (optional)'(t19).
  t_trace  = 'Path and name of abapGit ZIP file to create, instead of creating the objects'(t20).
  t_eponym = 'Create many objects with the same names:'(t21).
  t_date   = '  - ZSOAR_DATE (authorization field and object, domain, data element)'(t22).
  t_hash   = '  - ZSOAR_HASH (authorization field and object, domain, data element)'(t23).
  t_srp    = '  - ZSOAR_SRP (authorization field)'(t24).
  t_srp_id = '  - ZSOAR_SRP_ID (data element)'(t25).
  t_tabl_1 = 'Create table as copy of ZSOAR_INHOUSEDEV'(t06).
  t_clas_1 = 'Create class as copy of ZCL_SOAR_MANAGER'(t07).
  t_clas_2 = 'Create class as copy of ZCX_SOAR'(t08).
  t_intf_1 = 'Create interface pool as copy of ZIF_SOAR_MANAGER'(t09).
  t_intf_2 = 'Create interface pool as copy of ZIF_SOAR_PROVIDER'(t10).
  t_intf_3 = 'Create interface pool as copy of ZIF_SOAR_MANAGER_TEST'(t24).


START-OF-SELECTION.
  TRY.
      CALL METHOD ('LCL_APP')=>('MAIN').
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
  ASSERT 1 = 1.


CLASS lcx_error DEFINITION
    INHERITING FROM cx_static_check.
ENDCLASS.


CLASS lcl_app DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_shrinker_user_exit_abapgit.

    CLASS-METHODS main
      RAISING
        zcx_shrinker.

  PRIVATE SECTION.

    DATA tabix_public_section TYPE i.
    DATA zip TYPE REF TO cl_abap_zip.
    DATA objects TYPE zcl_shrinker_copy_objects=>ty_object_copies.

    METHODS main_2
      RAISING
        zcx_shrinker.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD main.
    DATA(app) = NEW lcl_app( ).
    app->main_2( ).
  ENDMETHOD.


  METHOD main_2.

    "==============================================================================
    " Copy repository objects
    "==============================================================================

    objects = VALUE #(
        ( object = 'AUTH' source_obj_name = 'ZSOAR_DATE            ' target_obj_name = p_date )
        ( object = 'AUTH' source_obj_name = 'ZSOAR_HASH            ' target_obj_name = p_hash )
        ( object = 'AUTH' source_obj_name = 'ZSOAR_SRP             ' target_obj_name = p_srp )
        ( object = 'SUSO' source_obj_name = 'ZSOAR_DATE            ' target_obj_name = p_date )
        ( object = 'SUSO' source_obj_name = 'ZSOAR_HASH            ' target_obj_name = p_hash )
        ( object = 'DOMA' source_obj_name = 'ZSOAR_DATE            ' target_obj_name = p_date )
        ( object = 'DOMA' source_obj_name = 'ZSOAR_HASH            ' target_obj_name = p_hash )
        ( object = 'DTEL' source_obj_name = 'ZSOAR_DATE            ' target_obj_name = p_date )
        ( object = 'DTEL' source_obj_name = 'ZSOAR_HASH            ' target_obj_name = p_hash )
        ( object = 'DTEL' source_obj_name = 'ZSOAR_SRP_ID          ' target_obj_name = p_srp_id )
        ( object = 'TABL' source_obj_name = 'ZSOAR_INHOUSEDEV      ' target_obj_name = p_tabl_1 )
        ( object = 'CLAS' source_obj_name = 'ZCL_SOAR_MANAGER      ' target_obj_name = p_clas_1 )
        ( object = 'CLAS' source_obj_name = 'ZCX_SOAR              ' target_obj_name = p_clas_2 )
        ( object = 'INTF' source_obj_name = 'ZIF_SOAR_MANAGER      ' target_obj_name = p_intf_1 )
        ( object = 'INTF' source_obj_name = 'ZIF_SOAR_PROVIDER     ' target_obj_name = p_intf_2 )
        ( object = 'INTF' source_obj_name = 'ZIF_SOAR_MANAGER_TEST ' target_obj_name = p_intf_3 ) ).


    DATA(copy_objects) = zcl_shrinker_copy_objects=>create( ).
    DATA(soar_target) = copy_objects->run(
                        source_package = p_frdevc
                        target_package = p_todevc
                        user_exit      = me
                        objects        = objects
                        test_mode      = xsdbool( p_trace IS NOT INITIAL ) ).


    "==============================================================================
    " Save new ZIP to laptop for debug purpose
    "==============================================================================

    IF p_trace IS NOT INITIAL.
      soar_target->file_download( iv_path = p_trace
                                  iv_xstr = soar_target->get_zip( )->save( ) ).
    ENDIF.


    "==============================================================================
    " Show the abapGit log
    "==============================================================================

    soar_target->show_log( ).


  ENDMETHOD.


  METHOD zif_shrinker_user_exit_abapgit~is_to_be_deserialized.

    result = xsdbool( line_exists( objects[ object          = object
                                            target_obj_name = obj_name ] ) ).

  ENDMETHOD.

ENDCLASS.
