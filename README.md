# OO - ABAP ( Object Oriented ABAP ) 

## This REPO is containing only one program which shows the use of all aspects of the OO - ABAP

![image](https://github.com/user-attachments/assets/f02fd4c2-c579-4246-8bb0-02c1555b3630)


#### 1. Inheritance via DEFINITION INHERITING FROM, along with PUBLIC , PRIVATE AND PROTECTED access control 
```CLASS <subclass> DEFINITION INHERITING FROM <superclass>.```

![image](https://github.com/user-attachments/assets/7b59204a-7102-40c9-b229-e084e0b6b4df)

#### 2. Syntax help 

```abap
CLASS sub_class Definition Inheriting From super_class
Public Section. 
METHODS: Addition1 Redefinition. 
ENDCLASS.

"Abstraction Syntax 
CLASS abstract_class DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: abstract_method ABSTRACT,
             normal_method. " this can have its implementation but abstract is always redefined and abstract class can never be called via 
                            " objects or anything always inherit
ENDCLASS.```

"Interface syntax ( Local ) 
INTERFACE <intf_name>. 
DATA..... 
CLASS-DATA..... 
METHODS..... 
CLASS-METHODS..... 
ENDINTERFACE.```

"NEW way of creating object
DATA(lo_obj) = NEW lcl_name( ).

## Rambling 
Class is nothing but a group which will have few sets of operations assigned to it. These sets of operations are called methods. 
You are a factory worker - > we have lots of tasks to be performed with the help of our machines. Now , every machine has its own mechanism and how it should operate. 

Let's take an example - 
1. We have this huge machine which has 5 sets of operations to be performed.
2. As a worker, we need to go and plug in that machine -> plugging in basically is like , finding a source to access the machine.
3. Our source of a class is an instance. Instance is basically bringing your class to life.

## Plugging in 
Before plugging in we must remeber that every machine has some parts which should be handled with care or shall remain closed for outsiders.
```abap
CLASS lcl_mach_master DEFINITION.
PUBLIC SECTION.  "part of the machine easily available 
METHODS: get_machine_started.
PROTECTED SECTION. " can't be accessed by outsiders but machine should be of the same way 

PRIVATE SECTION. " BEWARE - DANGER AHEAD !! 

ENDCLASS.

CLASS lcl_mach_master IMPLEMENTATION.
METHOD get_machine_started.
       write 'I started !'.
ENDMETHOD.
ENDCLASS.
```
Basically till now, we just built the machine where is the use ?

```abap
"factory we don't even have a plug !!!!!!
*--> before 7.40
DATA lo_obj TYPE REF TO lcl_mach_master. " memory hasnt been assigned - no use !!! where is our electricity
CREATE OBJECT lo_obj.
lo_obj->get_machine_started( ).

*--> after or in 7.40 
DATA(lo_obj) = NEW lcl_mach_master( ).
lo_obj->get_machine_started( ).
```

# Implement the first task
Since it was a very basic task , the design of the machine has this particular operation as public.

```abap
CLASS lcl_mach_master DEFINITION.
  PUBLIC SECTION.  "part of the machine easily available
    METHODS: get_machine_started,
             perform_basic_task.
  PROTECTED SECTION. " can't be accessed by outsiders but machine should be of the same way

  PRIVATE SECTION. " BEWARE - DANGER AHEAD !!

ENDCLASS.

CLASS lcl_mach_master IMPLEMENTATION.
  METHOD get_machine_started.
    WRITE 'I started !'.
  ENDMETHOD.
  METHOD perform_basic_task.
    DATA(lv_str) = | Hi I am a machine logged at { sy-uname } & JM factory |.
    WRITE lv_str.
  ENDMETHOD.
ENDCLASS.

DATA(lo_obj) = NEW lcl_mach_master( ).
  lo_obj->get_machine_started( ).
  lo_obj->perform_basic_task( ).
```
# Implement the medium task 
At JM factory ! we have one medium prority task - this needs to be handled with care, to prevent any health hazards for our lovely workers !!!
So, the manager comes up with an idea, how about we take the real task and wrap it as a public method , whereas the crucial work remains protected. 

```abap
CLASS lcl_mach_master DEFINITION.
  PUBLIC SECTION.  "part of the machine easily available
    METHODS: get_machine_started,
      perform_basic_task,
      perform_medium_task.
  PROTECTED SECTION. " can't be accessed by outsiders but machine should be of the same way
    METHODS: medium_task_operate.
  PRIVATE SECTION. " BEWARE - DANGER AHEAD !!
ENDCLASS.

LASS lcl_mach_master IMPLEMENTATION.
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
ENDCLASS.
```
In the above implementation, the medium task is called via perform_medium_task but the actual spark happens in the medium_task_operate. 

## Implementing the critical task 
So, there was one very critical task which could lead to even fire seize at the factory!!, to safely get around this operation the very intellignet manager again went to his workers and told them to wrap one more critical task with a public interface ( method ) and this time use the private section !. 

```abap

CLASS lcl_mach_master DEFINITION.
  PUBLIC SECTION.  "part of the machine easily available
    METHODS: get_machine_started,
      perform_basic_task,
      perform_medium_task,
      perform_critical_task.
  PROTECTED SECTION. " can't be accessed by outsiders but machine should be of the same way
    METHODS: medium_task_operate.
  PRIVATE SECTION. " BEWARE - DANGER AHEAD !!
    METHODS: critical_task_operate.
ENDCLASS.

CLASS lcl_mach_master IMPLEMENTATION.
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

START-OF-SELECTION.
  "factory we don't even have a plug !!!!!!

*--> after or in 7.40
  DATA(lo_obj) = NEW lcl_mach_master( ).
  lo_obj->get_machine_started( ).
  lo_obj->perform_basic_task( ).
  lo_obj->perform_medium_task( ).
  lo_obj->perform_critical_task( ).
```
One day, a co worker of the manager became very jealous, he was not able to understand the mastermind's plan behind all the design so, he thought of copying the designs and selling his own machines , such a crooked fellow !

## Inheritance 

He inherited all the operations that the master machine had and placed them in his own machine 

```abap
CLASS lcl_mach_copy DEFINITION INHERITING FROM lcl_mach_master.
  PUBLIC SECTION.
    METHODS: get_privacy_protection,
             perform_basic_task REDEFINITION.
ENDCLASS.

CLASS lcl_mach_copy IMPLEMENTATION.
  METHOD get_privacy_protection.
    me->medium_task_operate( ).
    me->critical_task_operate( ).
    me->get_machine_started( ).
    me->medium_task_operate( ).
  ENDMETHOD.
  method perform_basic_task.
  write ' I am not a copy !!'.
  ENDMETHOD.
ENDCLASS.
```
After copying everything, he sufferred a huge loss because he didn't know OO-ABAP as his previous manager. 

![image](https://github.com/user-attachments/assets/2876ccc7-fc32-45e7-99ee-bb99dd00c51e)

Just so you guys know, the cheater used polymorphism to reduce his guilt ( In ABAP, we only have over - riding :) ) 

## Polymorphism 

```abap
CLASS lcl_mach_copy DEFINITION INHERITING FROM lcl_mach_master.
  PUBLIC SECTION.
    METHODS: get_privacy_protection,
             perform_basic_task REDEFINITION.
ENDCLASS.
```

```abap
method perform_basic_task.
  write ' I am not a copy !!'.
ENDMETHOD.
```

Since the incident of that cunning fellow, the manager discovered the ways of hiding his actual implemnetations in the interfaces and letting the actual machine work its magic in the implemnenting class. 

## Encapsulation 

Basic Syntax 
```abap
INTERFACE if_vbak.
   DATA: ?? 
   METHODS: get_vbak.
ENDINTERFACE.
```
Definition 
```abap
interface Z_TEST_HAR101
  public .
  methods TEACH_JM
    importing
      !I_JM1 type I .
endinterface.
```
Implementation 
```abap
CLASS lcl_mach_master DEFINITION.
  PUBLIC SECTION.  "part of the machine easily available
    INTERFACES: z_test_har101.
ENDCLASS.

CLASS lcl_mach_master IMPLEMENTATION.
  METHOD z_test_har101~teach_jm.
    WRITE i_jm1.
  ENDMETHOD.
ENDCLASS.
```
The manager also learned about the concept of abstractiion , where he would have the abstract class with at least one abstract method

ABSTRACT class can't be instantiated !
```abap
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
```

If you instantiate - 

![image](https://github.com/user-attachments/assets/e60dc4a7-ffe3-4de6-a410-ef0d16ebce80)

Using Abstract class via inheritance 

```abap
CLASS lcl_mach_abs_user DEFINITION INHERITING FROM lcl_mach_abstract.
  PUBLIC SECTION.
    METHODS: start_abstraction REDEFINITION.
ENDCLASS.

CLASS lcl_mach_abs_user IMPLEMENTATION.
  METHOD start_abstraction.
    WRITE: 'I am abstract and implemented'.
  ENDMETHOD.
ENDCLASS.
```

One day at JM factory, one of the co workers asked him , what is the use of this checkbox in Tcode SE24 ? 
![image](https://github.com/user-attachments/assets/4f34be7a-8023-4a58-b2a6-42420695af75)

### Difference between pass by reference and pass by value 

Pass by reference acts as a reference to the actual parameters, whereas pass by value makes a local copy of the parameter and then operations are performed. 
According to SAP, pass by value is always recommended, but at times, for tables and structures pass by value may be costly for time, hence, in those case pass by reference should be used. 

( If you don't check that pass value box then it would automatically be pass by reference )

- Importing

  So, pass by reference for importing parameter has an egffect as follows:
  You won't be able to change the value of the importing parameter, but when using pass by value, you can
  actually over-write the value.

  ![image](https://github.com/user-attachments/assets/d55fd062-45c7-4bbe-845e-b4387c9e6548)

  ![image](https://github.com/user-attachments/assets/d42398fc-18ed-4924-b535-ebafe3adc00f)

  It will not give error, but if we check that box 
  
- Exporting
- Returning 

With these principles, Manager of JM factory became the best known manager in the town !

Moral of the story - > Be good at ABAP

