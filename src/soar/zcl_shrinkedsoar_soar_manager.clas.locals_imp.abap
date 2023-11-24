*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_manager_w_dynamic_provider DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES ZIF_SHRINKEDSOAR_SOAR_PROVIDER.

    CLASS-METHODS create
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

    TYPES:
      BEGIN OF ty_dynamic_provider,
        class_name  TYPE seoclsname,
        method_name TYPE seomtdname,
      END OF ty_dynamic_provider.

    DATA dynamic_provider TYPE ty_dynamic_provider.
    DATA provider TYPE REF TO ZIF_SHRINKEDSOAR_SOAR_PROVIDER.

ENDCLASS.



CLASS lcl_manager_w_dynamic_provider IMPLEMENTATION.

  METHOD create.

    DATA(provider) = NEW lcl_manager_w_dynamic_provider( ).

    provider->dynamic_provider = VALUE #(
        class_name  = provider_class_name
        method_name = provider_method_name ).

    result = ZCL_SHRINKEDSOAR_SOAR_MANAGER=>ZIF_SHRINKEDSOAR_SOAR_MANAGER~create(
                    srp_id   = srp_id
                    provider = provider ).

  ENDMETHOD.


  METHOD ZIF_SHRINKEDSOAR_SOAR_PROVIDER~get_abap_source_code.

    TRY.
        DATA(abap_code) = VALUE string_table( ).

        CALL METHOD (dynamic_provider-class_name)=>(dynamic_provider-method_name)
          RECEIVING
            result = abap_code.

        result = abap_code.

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE ZCX_SHRINKEDSOAR_SOAR EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
