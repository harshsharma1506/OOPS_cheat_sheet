REPORT z_oops_sheet.

CLASS lcl_sales DEFINITION.
  PUBLIC SECTION.
  INTERFACES: ZIF_DPK_KB_SORT_DEF. " using an interface which is in SE24 this is encapsulation
    METHODS:
      get_order,
      chk_private.
  PROTECTED SECTION.
    METHODS:
      get_order_100.
  PRIVATE SECTION.
    METHODS:
      get_order_priv. " if any inherited or any class accesses me i throw error saying method does not exist
ENDCLASS.

CLASS lcl_sales IMPLEMENTATION.
  METHOD get_order.
    SELECT * FROM vbak UP TO 10 ROWS INTO TABLE @DATA(lt_vbak).
  ENDMETHOD.
  METHOD get_order_prIv.
    WRITE 'private_chief'.
  ENDMETHOD.
  METHOD get_order_100.
    SELECT * FROM vbak UP TO 100 ROWS INTO TABLE @DATA(lt_vbak).
    IF sy-subrc = 0.
      WRITE: 'I am protected I can be called within this class or within inherited'.
    ENDIF.
  ENDMETHOD.
  METHOD ZIF_DPK_KB_SORT_DEF~get_output.
    SELECT SINGLE vbeln, ernam FROM vbak WHERE erdat = @sy-datum INTO @DATA(ls_vbak).
      IF sy-subrc = 0.
        write ls_vbak.
      ENDIF.
  ENDMETHOD.
  METHOD chk_private.
    me->get_order_priv( ). " I cant be accessed via inherited class whereas the get_order_100 can be.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sales_child DEFINITION INHERITING FROM lcl_sales.
  PUBLIC SECTION.
    METHODS: get_order REDEFINITION, " I am polymorphism
      get_more_order. " I am using protected method look inside with me operator
ENDCLASS.

CLASS lcl_sales_child IMPLEMENTATION.
  METHOD get_order.
    SELECT vbak~vbeln,
           vbap~posnr,
           vbak~ernam,
           vbap~netwr
      FROM vbak INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
      UP TO 10 ROWS INTO TABLE @DATA(lt_vbap).
    IF sy-subrc = 0.
      cl_demo_output=>display_data( lt_vbap ).
    ENDIF.
  ENDMETHOD.
  METHOD get_more_order.
    me->get_order_100( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_abs_demo DEFINITION ABSTRACT. " my object cant be created
  PUBLIC SECTION.
    METHODS: get_order_abs ABSTRACT, "inherit me and implemnet
      have_fun. " inherit me and use
ENDCLASS.

CLASS lcl_abs_demo IMPLEMENTATION.
  METHOD have_fun.
    TYPES: t_itab TYPE STANDARD TABLE OF i WITH DEFAULT KEY. "LOCAL table type
    DATA(itab) = VALUE t_itab( ( 10 ) ( 2 ) ( 11 ) ). "defualt values
    IF lines( itab ) > 0.
      cl_demo_output=>display_data( itab ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_finale DEFINITION INHERITING FROM lcl_abs_demo.
  PUBLIC SECTION.
    METHODS: get_order_ABS REDEFINITION.
ENDCLASS.

CLASS lcl_finale IMPLEMENTATION.
  METHOD get_order_abs.
    SELECT * FROM vbap UP TO 500 ROWS INTO TABLE @DATA(lt_abs).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(lv_var) = NEW lcl_sales( ).
  lv_var->get_order( ).
  lv_var->zif_dpk_kb_sort_def~get_output( ). " encapsulated call
  DATA(lv_var_1) = NEW lcl_sales_child( ).
  lv_var_1->get_order( ).
  lv_var_1->get_more_order( ).
  DATA(lv_var_2) = NEW lcl_finale( ).
  lv_var_2->have_fun( ).
  lv_var_2->get_order_abs( ).
