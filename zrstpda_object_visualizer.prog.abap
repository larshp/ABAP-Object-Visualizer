*<SCRIPT:PERSISTENT>
REPORT zrstpda_object_visualizer.

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
CLASS lcl_debugger_script DEFINITION
    INHERITING FROM cl_tpda_script_class_super FINAL ##NEEDED.

  PUBLIC SECTION.
    METHODS:
      script REDEFINITION.

  PRIVATE SECTION.

    CONSTANTS: c_newline
      LIKE cl_abap_char_utilities=>newline
      VALUE cl_abap_char_utilities=>newline.

    DATA: mv_graph TYPE string.

    METHODS:
      to_clipboard,
      name
        IMPORTING iv_name        TYPE string
        RETURNING VALUE(rv_name) TYPE string,
      handle_tab
        IMPORTING iv_name  TYPE string
                  io_descr TYPE REF TO cl_tpda_script_data_descr
        RAISING   cx_tpda,
      handle_object
        IMPORTING iv_name  TYPE string
                  io_descr TYPE REF TO cl_tpda_script_data_descr
        RAISING   cx_tpda,
      handle_objref
        IMPORTING iv_name TYPE string
        RAISING   cx_tpda,
      handle_string
        IMPORTING iv_name  TYPE string
                  io_descr TYPE REF TO cl_tpda_script_data_descr
        RAISING   cx_tpda,
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

  METHOD name.

    rv_name = iv_name.
    REPLACE ALL OCCURRENCES OF '{' IN rv_name WITH '\{'.
    REPLACE ALL OCCURRENCES OF '}' IN rv_name WITH '\}'.

  ENDMETHOD.

  METHOD handle_tab.

    DATA: lv_name  TYPE string,
          lv_label TYPE string,
          lv_edges TYPE string,
          lo_table TYPE REF TO cl_tpda_script_tabledescr.


    lo_table ?= io_descr.
    lv_label = 'Table'(001).
    DO lo_table->linecnt( ) TIMES.
      lv_name = |{ iv_name }[{ sy-index }]|.

      lv_label = |{ lv_label } \|<f{ sy-index }> { sy-index }|.
      lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-index
        }> -> "{ name( lv_name ) }";{ c_newline }|.

      handle( lv_name ).
    ENDDO.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }{ lv_edges }|.

  ENDMETHOD.

  METHOD handle_object.

    DATA:  lo_object     TYPE REF TO cl_tpda_script_objectdescr,
           lv_name       TYPE string,
           lv_label      TYPE string,
           lv_edges      TYPE string,
           lt_attributes TYPE tpda_script_object_attribut_it.

    FIELD-SYMBOLS: <ls_attribute> LIKE LINE OF lt_attributes.


    lo_object ?= io_descr.
    lt_attributes = lo_object->attributes( ).

    lv_label = 'Object'(002).
    LOOP AT lt_attributes ASSIGNING <ls_attribute>.
      lv_label = |{ lv_label } \|<f{ sy-tabix }> { name( <ls_attribute>-name ) }|.
      CONCATENATE iv_name '-' <ls_attribute>-name INTO lv_name.
      lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-tabix
        }> -> "{ name( lv_name ) }";{ c_newline }|.

      handle( lv_name ).
    ENDLOOP.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }{ lv_edges }|.

  ENDMETHOD.

  METHOD handle_objref.

    DATA: ls_info  TYPE tpda_scr_quick_info,
          lv_label TYPE string.

    FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbobjref.


    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).

    ASSIGN ls_info-quickdata->* TO <ls_symobjref>.
    handle( <ls_symobjref>-instancename ).

    IF iv_name CA '{'.
      lv_label = 'ref'.
    ELSE.
      lv_label = iv_name.
    ENDIF.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }"{ name( iv_name ) }" -> "{ name( <ls_symobjref>-instancename ) }";{ c_newline
      }|.

  ENDMETHOD.

  METHOD handle_string.

    DATA: lo_string TYPE REF TO cl_tpda_script_stringdescr,
          lv_value  TYPE string.


    lo_string ?= io_descr.
    lv_value = lo_string->value( ).

    REPLACE ALL OCCURRENCES OF c_newline IN lv_value WITH space.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "'{ lv_value }'"{ c_newline
      }shape = "record" ];{ c_newline }|.

  ENDMETHOD.

  METHOD script.

    DATA: lx_tpda TYPE REF TO cx_tpda.


    TRY.
        handle( 'GO_CLASS' ).
        mv_graph = |digraph g \{{ c_newline
          }graph [{ c_newline
          }rankdir = "LR"{ c_newline
          }];{ c_newline
          }{ mv_graph }{ c_newline
          }\}|.
        to_clipboard( ).
      CATCH cx_tpda INTO lx_tpda.
        MESSAGE lx_tpda TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle.

    DATA: ls_info  TYPE tpda_scr_quick_info,
          lo_descr TYPE REF TO cl_tpda_script_data_descr.


    lo_descr = cl_tpda_script_data_descr=>factory( iv_name ).
    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).

    CASE ls_info-metatype.
      WHEN cl_tpda_script_data_descr=>mt_simple.
        BREAK-POINT.
      WHEN cl_tpda_script_data_descr=>mt_struct.
        BREAK-POINT.
      WHEN cl_tpda_script_data_descr=>mt_string.
        handle_string( iv_name  = iv_name
                       io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_tab.
        handle_tab( iv_name  = iv_name
                    io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_datref.
        BREAK-POINT.
      WHEN cl_tpda_script_data_descr=>mt_object.
        handle_object( iv_name  = iv_name
                       io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_objref.
        handle_objref( iv_name ).
    ENDCASE.

  ENDMETHOD.                    "script

  METHOD to_clipboard.

    DATA: lt_table TYPE STANDARD TABLE OF abaptxt255 ##NEEDED,
          lv_rc    TYPE i.


    SPLIT mv_graph AT c_newline INTO TABLE lt_table.

    cl_gui_frontend_services=>clipboard_export(
      IMPORTING
        data                 = lt_table
      CHANGING
        rc                   = lv_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        no_authority         = 4
        OTHERS               = 5 ).                       "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    MESSAGE 'Exported to clipboard'(003) TYPE 'I'.

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>