class Z_TEST definition
  public
  final
  create public .

*"* public components of class Z_TEST
*"* do not include other source files here!!!
public section.

  class-methods TEACH_JM
    importing
      value(I_JM1) type I
    exporting
      value(E_JM1) type I .
protected section.
*"* protected components of class Z_TEST
*"* do not include other source files here!!!
private section.
*"* private components of class Z_TEST
*"* do not include other source files here!!!
ENDCLASS.



CLASS Z_TEST IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method Z_TEST=>TEACH_JM
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_JM1                          TYPE        I
* | [<---] E_JM1                          TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD teach_jm.
    i_jm1 = 1.
    e_jm1 = e_jm1 + 1.
  ENDMETHOD.
ENDCLASS.
