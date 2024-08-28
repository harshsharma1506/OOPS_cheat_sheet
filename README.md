### This REPO is containing only one program which shows the use of all aspects of the OO - ABAP

![image](https://github.com/user-attachments/assets/f02fd4c2-c579-4246-8bb0-02c1555b3630)


#### 1. Inheritance via DEFINITION INHERITING FROM, along with PUBLIC , PRIVATE AND PROTECTED access control 
```CLASS <subclass> DEFINITION INHERITING FROM <superclass>.```

![image](https://github.com/user-attachments/assets/7b59204a-7102-40c9-b229-e084e0b6b4df)

#### 2. LOts of syntax help 

```CLASS sub_class Definition Inheriting From super_class
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
