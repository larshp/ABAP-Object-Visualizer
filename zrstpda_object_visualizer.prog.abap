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

    DATA: mt_visited TYPE TABLE OF string,
          mv_graph   TYPE string.

    METHODS:
      popup
        RETURNING VALUE(rv_name) TYPE string,
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
      handle_simple
        IMPORTING iv_name  TYPE string
                  io_descr TYPE REF TO cl_tpda_script_data_descr
        RAISING   cx_tpda,
      handle_struct
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

  METHOD popup.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Object'(002).
    <ls_field>-field_obl = abap_true.
    <ls_field>-value     = 'GO_CLASS'.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Choose object'(005)
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0 OR lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.

    rv_name = <ls_field>-value.

  ENDMETHOD.

  METHOD handle_simple.

    DATA: lo_elem  TYPE REF TO cl_tpda_script_elemdescr,
          lv_value TYPE string.


    lo_elem ?= io_descr.
    lv_value = lo_elem->value( ).

    REPLACE ALL OCCURRENCES OF c_newline IN lv_value WITH space.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_value }"{ c_newline
      }shape = "record" ];{ c_newline }|.

  ENDMETHOD.

  METHOD handle_struct.

    DATA: lv_label      TYPE string,
          lv_edges      TYPE string,
          lv_name       TYPE string,
          lt_components TYPE tpda_script_struc_componentsit,
          lo_struct     TYPE REF TO cl_tpda_script_structdescr.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF lt_components.


    lo_struct ?= io_descr.

    lo_struct->components( IMPORTING p_components_it = lt_components ).

    lv_label = 'Structure'(004).
    LOOP AT lt_components ASSIGNING <ls_component>.
      lv_label = |{ lv_label } \|<f{ sy-tabix }> {
        name( <ls_component>-compname ) }\\{ c_newline }|.
      CONCATENATE iv_name '-' <ls_component>-compname INTO lv_name.
      lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-tabix
        }> -> "{ name( lv_name ) }";{ c_newline }|.

      handle( lv_name ).
    ENDLOOP.

    mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
      }label = "{ lv_label }"{ c_newline
      }shape = "record" ];{ c_newline
      }{ lv_edges }|.

  ENDMETHOD.

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

      lv_label = |{ lv_label } \|<f{ sy-index }> { sy-index }\\{ c_newline }|.
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
      lv_label = |{ lv_label } \|<f{ sy-tabix }> {
        name( <ls_attribute>-name ) }\\{ c_newline }|.
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
    IF <ls_symobjref>-instancename <> '{O:initial}'.
      handle( <ls_symobjref>-instancename ).
    ENDIF.

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

    DATA: lv_name TYPE string,
          lx_tpda TYPE REF TO cx_tpda.


    TRY.
        lv_name = popup( ).
        IF lv_name IS INITIAL.
          RETURN.
        ENDIF.
        CLEAR mt_visited.
        handle( lv_name ).
        mv_graph = |digraph g \{{ c_newline
          }graph [{ c_newline
          }rankdir = "LR"{ c_newline
          }];{ c_newline
          }{ mv_graph }{ c_newline
          }\}|.
        to_clipboard( ).
      CATCH cx_tpda_varname.
        MESSAGE 'Unknown variable'(006) TYPE 'I'.
      CATCH cx_tpda INTO lx_tpda.
        MESSAGE lx_tpda TYPE 'I'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle.

    DATA: ls_info  TYPE tpda_scr_quick_info,
          lo_descr TYPE REF TO cl_tpda_script_data_descr.


    READ TABLE mt_visited FROM iv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    APPEND iv_name TO mt_visited.

    lo_descr = cl_tpda_script_data_descr=>factory( iv_name ).
    ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).

    CASE ls_info-metatype.
      WHEN cl_tpda_script_data_descr=>mt_simple.
        handle_simple( iv_name  = iv_name
                       io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_struct.
        handle_struct( iv_name  = iv_name
                       io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_string.
        handle_string( iv_name  = iv_name
                       io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_tab.
        handle_tab( iv_name  = iv_name
                    io_descr = lo_descr ).
      WHEN cl_tpda_script_data_descr=>mt_datref.
* to be implemented
        ASSERT 1 = 1 + 1.
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