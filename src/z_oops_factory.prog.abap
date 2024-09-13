REPORT z_oops_factory.abap.

CLASS lcl_mach_master DEFINITION.
  PUBLIC SECTION.  "part of the machine easily available
    INTERFACES: z_test_har101.
    METHODS: get_machine_started,
      perform_basic_task,
      perform_medium_task,
      perform_critical_task.
  PROTECTED SECTION. " can't be accessed by outsiders but machine should be of the same way
    METHODS: medium_task_operate.
  PRIVATE SECTION. " BEWARE - DANGER AHEAD !!
    METHODS: critical_task_operate.
ENDCLASS.

CLASS lcl_mach_abstract DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: start_abstraction ABSTRACT,
      start_normal.
ENDCLASS.

CLASS lcl_mach_abstract IMPLEMENTATION.
  METHOD start_normal.
    WRITE: 'I am normal'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mach_abs_user DEFINITION INHERITING FROM lcl_mach_abstract.
  PUBLIC SECTION.
    METHODS: start_abstraction REDEFINITION.
ENDCLASS.

CLASS lcl_mach_abs_user IMPLEMENTATION.
  METHOD start_abstraction.
    WRITE: 'I am abstract and implemented'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mach_master IMPLEMENTATION.
  METHOD z_test_har101~teach_jm.
    WRITE i_jm1.
  ENDMETHOD.
  METHOD get_machine_started.
    WRITE 'I started !'.
  ENDMETHOD.
  METHOD perform_basic_task.
    DATA(lv_str) = | Hi I am a machine logged at { sy-uname } & JM factory |.
    WRITE lv_str.
  ENDMETHOD.
  METHOD medium_task_operate.
    DATA: lt_str TYPE STANDARD TABLE OF char5.
    lt_str = VALUE #( ( 'Harsh' )
                      ( 'jaya' )
                      ( 'uvais')
                      ( 'parul' )
                    ).
    cl_demo_output=>display( lt_str
    ).
  ENDMETHOD.
  METHOD perform_medium_task.
    me->medium_task_operate( ).
  ENDMETHOD.
  METHOD critical_task_operate.
    SELECT * FROM vbak UP TO 5 ROWS INTO TABLE @DATA(lt_critical).
    IF sy-subrc = 0.
      cl_demo_output=>display( lt_critical ).
    ENDIF.
  ENDMETHOD.
  METHOD perform_critical_task.
    me->critical_task_operate( ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_mach_copy DEFINITION INHERITING FROM lcl_mach_master.
  PUBLIC SECTION.
    METHODS: get_privacy_protection,
      perform_basic_task REDEFINITION.
ENDCLASS.

CLASS lcl_mach_copy IMPLEMENTATION.
  METHOD get_privacy_protection.
    me->get_machine_started( ).
    me->medium_task_operate( ).
  ENDMETHOD.
  METHOD perform_basic_task.
    WRITE ' I am not a copy !!'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  "factory we don't even have a plug !!!!!!
*--> before 7.40
DATA lo_obj TYPE REF TO lcl_mach_master. " memory hasnt been assigned - no use !!! where is our electricity
CREATE OBJECT lo_obj.
lo_obj->get_machine_started( ).

**--> after or in 7.40
  DATA(lo_obj) = NEW lcl_mach_master( ).
  lo_obj->z_test_har101~teach_jm( i_jm1 =  1 ).
  lo_obj->get_machine_started( ).
  lo_obj->perform_basic_task( ).
  lo_obj->perform_medium_task( ).
  lo_obj->perform_critical_task( ).

  DATA(lo_obj1) = NEW lcl_mach_copy( ).
  lo_obj1->get_privacy_protection( ).
  lo_obj1->perform_basic_task( ).

  DATA(lo_abs_use) = NEW lcl_mach_abs_user( ).
  lo_abs_use->start_normal( ).
  lo_abs_use->start_abstraction( ).
