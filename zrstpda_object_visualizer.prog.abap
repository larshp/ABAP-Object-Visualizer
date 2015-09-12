*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZRSTPDA_OBJECT_VISUALIZER</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>ABAP Object Visualizer</SCRIPT_COMMENT>
*<SINGLE_RUN>X</SINGLE_RUN>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>

* See https://github.com/larshp/ABAP-Object-Visualizer

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2015 Lars Hvam Petersen
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
********************************************************************************

*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM cl_tpda_script_class_super.

  PUBLIC SECTION.
    METHODS:
      script REDEFINITION.

  PRIVATE SECTION.
    METHODS:
      handle
        IMPORTING iv_name TYPE string
        RAISING   cx_tpda.

ENDCLASS.                    "lcl_debugger_script DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.

  METHOD script.

    DATA: lx_tpda TYPE REF TO cx_tpda.


    TRY.
        handle( 'GO_CLASS' ).
      CATCH cx_tpda INTO lx_tpda.
        MESSAGE lx_tpda TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle.

    DATA: ls_info       TYPE tpda_scr_quick_info,
          lo_descr      TYPE REF TO cl_tpda_script_data_descr,
          lo_object     TYPE REF TO cl_tpda_script_objectdescr,
          lo_string     TYPE REF TO cl_tpda_script_stringdescr,
          lo_elem       TYPE REF TO cl_tpda_script_elemdescr,
          lo_oref       TYPE REF TO cl_tpda_script_orefdescr,
          lo_table      TYPE REF TO cl_tpda_script_tabledescr,
          lv_value      TYPE string,
          ls_vars       TYPE tpda_quick_vars,
          lv_name       TYPE string,
          lv_char8      TYPE c LENGTH 8,
          ls_tinfo      TYPE tpda_scr_object_quick,
          lt_attributes TYPE tpda_script_object_attribut_it.

    FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbobjref.


    lo_descr = cl_tpda_script_data_descr=>factory( iv_name ).
    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).

    CASE ls_info-metatype.
      WHEN cl_tpda_script_data_descr=>mt_simple.
        BREAK-POINT.
      WHEN cl_tpda_script_data_descr=>mt_struct.
        BREAK-POINT.
      WHEN cl_tpda_script_data_descr=>mt_string.
        lo_string ?= lo_descr.
        lv_value = lo_string->value( ).
      WHEN cl_tpda_script_data_descr=>mt_tab.
        lo_table ?= lo_descr.
        DO lo_table->linecnt( ) TIMES.
          lv_char8 = sy-index.
          CONDENSE lv_char8.
          CONCATENATE iv_name '[' lv_char8 ']' INTO lv_name.
          handle( lv_name ).
        ENDDO.
      WHEN cl_tpda_script_data_descr=>mt_datref.
        BREAK-POINT.
      WHEN cl_tpda_script_data_descr=>mt_object.
        lo_object ?= lo_descr.
        lt_attributes = lo_object->attributes( ).
        LOOP AT lt_attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
          CONCATENATE iv_name '-' <ls_attribute>-name INTO lv_name.
          handle( lv_name ).
        ENDLOOP.
      WHEN cl_tpda_script_data_descr=>mt_objref.
        ASSIGN ls_info-quickdata->* TO <ls_symobjref>.
        handle( <ls_symobjref>-instancename ).
    ENDCASE.

  ENDMETHOD.                    "script

*  DATA: lt_table TYPE STANDARD TABLE OF abaptxt255,
*        lv_rc    TYPE i.
*
*
*  APPEND iv_string TO lt_table.
*
*  cl_gui_frontend_services=>clipboard_export(
*    IMPORTING
*      data                 = lt_table
*    CHANGING
*      rc                   = lv_rc
*    EXCEPTIONS
*      cntl_error           = 1
*      error_no_gui         = 2
*      not_supported_by_gui = 3
*      no_authority         = 4
*      OTHERS               = 5 ).
*  ASSERT sy-subrc = 0.
*
*  MESSAGE i001(zobject_visualizer).

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>