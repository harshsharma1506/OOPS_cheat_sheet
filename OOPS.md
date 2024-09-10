# OO - ABAP ( Object Oriented ABAP ) 

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
