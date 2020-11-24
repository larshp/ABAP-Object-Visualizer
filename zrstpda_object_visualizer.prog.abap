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
* Copyright (c) 2020 Jacques Nomssi Nzali
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

    CONSTANTS:
      c_mode_aut TYPE char01 VALUE 'T',  " for ABAP Unit Test
      c_mode_url TYPE char01 VALUE 'U',
      c_mode_txt TYPE char01 VALUE space.
    CONSTANTS c_plantuml_server TYPE string
      VALUE 'http://www.plantuml.com/plantuml/img/'  ##NO_TEXT.

    TYPES tv_scale TYPE perct.
    CONSTANTS c_default_scale TYPE tv_scale VALUE '0.7'.
    TYPES: BEGIN OF ts_diagram_config,
             local_path     TYPE string,
             java_jar       TYPE string,
             java_appl      TYPE string,
             server_url     TYPE string,
             output_mode    TYPE char01,
             skip_dialog    TYPE flag,
             scale          TYPE tv_scale,
             shadowing      TYPE flag,
             display_source TYPE flag,
             hpages         TYPE sytabix,
             vpages         TYPE sytabix,
           END OF ts_diagram_config.

    CLASS lcl_file_name DEFINITION.
      PUBLIC SECTION.
        CLASS-METHODS new IMPORTING iv_mode        TYPE char01
                          RETURNING VALUE(ro_file) TYPE REF TO lcl_file_name.
        METHODS constructor IMPORTING iv_mode TYPE char01.
        METHODS dialog RETURNING VALUE(rv_user_action) TYPE i.
        METHODS get_prefix RETURNING VALUE(rv_name) TYPE string
                           RAISING   cx_dynamic_check.
        METHODS get_fullpath RETURNING VALUE(rv_name) TYPE string.
      PROTECTED SECTION.
        TYPES: BEGIN OF ts_fullpath,
                 title  TYPE string,
                 name   TYPE string,
                 ext    TYPE string,
                 path   TYPE string,
                 filter TYPE string,
               END OF ts_fullpath.
        DATA ms_file TYPE ts_fullpath.
    ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml DEFINITION
*----------------------------------------------------------------------*
    CLASS lcl_plant_uml DEFINITION.
      PUBLIC SECTION.
        METHODS constructor IMPORTING iv_diagram TYPE string.
        METHODS to_url IMPORTING iv_base_url   TYPE string DEFAULT c_plantuml_server
                       RETURNING VALUE(rv_url) TYPE string
                       RAISING   cx_dynamic_check.
        METHODS output IMPORTING is_cfg TYPE ts_diagram_config RAISING cx_dynamic_check.
      PROTECTED SECTION.
        TYPES tv_base64 TYPE c LENGTH 65.
        CONSTANTS:
          c_standard TYPE tv_base64 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='  ##NO_TEXT,
          c_plantuml TYPE tv_base64 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_0' ##NO_TEXT.
        DATA mv_diagram TYPE string.

        METHODS to_xstring IMPORTING iv_string         TYPE string
                           RETURNING VALUE(rv_xstring) TYPE xstring
                           RAISING   cx_dynamic_check.
        METHODS source IMPORTING iv_display_source TYPE flag
                       RETURNING VALUE(rv_source)  TYPE string.

        METHODS png_file_name IMPORTING io_name        TYPE REF TO lcl_file_name
                                        is_cfg         TYPE ts_diagram_config
                              RETURNING VALUE(rv_name) TYPE string.

        METHODS parameter_string IMPORTING io_name         TYPE REF TO lcl_file_name
                                           is_cfg          TYPE ts_diagram_config
                                 RETURNING VALUE(rv_param) TYPE string.
        METHODS show_html IMPORTING iv_html TYPE string
                                    iv_size TYPE string DEFAULT cl_abap_browser=>xlarge
                          RAISING   cx_dynamic_check.
        METHODS to_png IMPORTING io_name        TYPE REF TO lcl_file_name
                                 is_cfg         TYPE ts_diagram_config
                       RETURNING VALUE(rv_name) TYPE string.
    ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_file DEFINITION
*----------------------------------------------------------------------*
    CLASS lcl_file DEFINITION CREATE PRIVATE.
      PUBLIC SECTION.
        CONSTANTS:
          c_mode_txt TYPE char01 VALUE space,
          c_mode_png TYPE char01 VALUE 'P'.

        CLASS-METHODS download
          IMPORTING iv_data         TYPE xstring
                    io_name         TYPE REF TO lcl_file_name
          RETURNING VALUE(rv_subrc) TYPE sysubrc.
    ENDCLASS.                    "lcl_file DEFINITION

    CLASS lcl_file_name_dummy DEFINITION INHERITING FROM lcl_file_name.
      PUBLIC SECTION.
        METHODS dialog REDEFINITION.
    ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*

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
          to_plantuml 
            IMPORTING iv_diagram TYPE string,
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
          handle_dataref
            IMPORTING !iv_name TYPE string
                      io_descr TYPE REF TO cl_tpda_script_data_descr
            RAISING
                      cx_tpda ,
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

        APPEND VALUE #( tabname = 'ABAPTXT255'
                        fieldname = 'LINE'
                        fieldtext = 'Object'(002)
                        field_obl = abap_true
                        value     = 'GO_CLASS' ) TO lt_fields.

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

        rv_name = to_upper( VALUE #( lt_fields[ 1 ]-value OPTIONAL ) ).

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

        DATA: lv_label        TYPE string,
              lv_edges        TYPE string,
              lv_name         TYPE string,
              lt_components   TYPE tpda_script_struc_componentsit,
              lo_struct       TYPE REF TO cl_tpda_script_structdescr,
              lv_match_offset TYPE i.

        lo_struct ?= io_descr.

        lo_struct->components( IMPORTING p_components_it = lt_components ).
        FIND REGEX '\*$' IN iv_name MATCH OFFSET lv_match_offset.

        lv_label = 'Structure'(004).
        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
          lv_label = |{ lv_label } \|<f{ sy-tabix }> { name( <ls_component>-compname ) }\\{ c_newline }|.
          IF lv_match_offset = 0.
            lv_name = |{ iv_name }-{ <ls_component>-compname }|.
          ELSE.
            lv_name = |{ iv_name(lv_match_offset) }{ <ls_component>-compname }|.
          ENDIF.
          lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-tabix }> -> "{ name( lv_name ) }";{ c_newline }|.

          handle( lv_name ).
        ENDLOOP.

        mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
          }label = "{ lv_label }"{ c_newline
          }shape = "record" ];{ c_newline
          }{ lv_edges }|.

      ENDMETHOD.

      METHOD name.

        rv_name = iv_name.
        REPLACE ALL OCCURRENCES OF:
           '{' IN rv_name WITH '\{',
           '}' IN rv_name WITH '\}'.

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

        DATA: lo_object     TYPE REF TO cl_tpda_script_objectdescr,
              lv_name       TYPE string,
              lv_label      TYPE string,
              lv_color      TYPE string,
              lv_edges      TYPE string,
              lv_type       TYPE string,
              lt_attributes TYPE tpda_script_object_attribut_it.

        lo_object ?= io_descr.
        lt_attributes = lo_object->attributes( ).

        lv_label = 'Object'(002).
        LOOP AT lt_attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
          lv_label = |{ lv_label } \|<f{ sy-tabix }> { name( <ls_attribute>-name ) }\\{ c_newline }|.

          CONCATENATE iv_name '-' <ls_attribute>-name INTO lv_name.

          CASE <ls_attribute>-acckind.
            WHEN if_tpda_control=>ak_private.
              lv_color = 'red'.
              lv_type = 'private'.
            WHEN if_tpda_control=>ak_protected.
              lv_color = 'yellow'.
              lv_type = 'protected'.
            WHEN if_tpda_control=>ak_public.
              lv_color = 'green'.
              lv_type = 'public'.
            WHEN OTHERS.
              ASSERT 1 = 1 + 1.
          ENDCASE.
          lv_edges = |{ lv_edges }"{ name( iv_name ) }":<f{ sy-tabix
            }> -> "{ name( lv_name ) }" [fontcolor={ lv_color
            } label="{ lv_type }"];{ c_newline }|.

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

      METHOD handle_dataref.
        DATA: ls_info TYPE tpda_scr_quick_info.

        FIELD-SYMBOLS: <ls_symobjref> TYPE tpda_sys_symbdatref.

        ls_info = cl_tpda_script_data_descr=>get_quick_info( iv_name ).
        ASSIGN ls_info-quickdata->* TO <ls_symobjref>.

        FIND REGEX '^\{[A-Z]:initial\}$' IN <ls_symobjref>-instancename IGNORING CASE.
        IF sy-subrc <> 0.
          handle( <ls_symobjref>-instancename ).
        ENDIF.

        mv_graph = |{ mv_graph }"{ name( iv_name ) }" [{ c_newline
          }label = "ref"{ c_newline
          }shape = "record" ];{ c_newline
          }"{ name( iv_name ) }" -> "{ name( <ls_symobjref>-instancename ) }";{ c_newline }|.


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
            to_plantuml( |@startdot\n{ mv_graph }\n@enddot| ).
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

        TRY.

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
                handle_dataref( iv_name  = iv_name
                                io_descr = lo_descr ).
              WHEN cl_tpda_script_data_descr=>mt_object.
                handle_object( iv_name  = iv_name
                               io_descr = lo_descr ).
              WHEN cl_tpda_script_data_descr=>mt_objref.
                handle_objref( iv_name ).
            ENDCASE.
          CATCH cx_root.
            ASSERT 1 = 1 + 1.
        ENDTRY.

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
            OTHERS               = 5 ).                   "#EC CI_SUBRC
        ASSERT sy-subrc = 0.

        MESSAGE 'Exported to clipboard'(003) TYPE 'I'.

      ENDMETHOD.

      METHOD to_plantuml.
        DATA ls_cfg TYPE ts_diagram_config.
        
        ls_cfg-scale = c_default_scale.
        ls_cfg-server_url = c_plantuml_server.
        ls_cfg-hpages = 1.
        ls_cfg-vpages = 1.
        ls_cfg-output_mode = c_mode_url.

        NEW lcl_plant_uml( iv_diagram )->output( ls_cfg ).
      ENDMETHOD.
      
    ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_plant_uml IMPLEMENTATION
*----------------------------------------------------------------------*
    CLASS lcl_plant_uml IMPLEMENTATION.

      METHOD constructor.
        mv_diagram = iv_diagram.
      ENDMETHOD.                    "constructor

      METHOD source.
        CLEAR rv_source.
        CHECK iv_display_source EQ abap_true.
        rv_source = |<p>{ mv_diagram }</p>|.
      ENDMETHOD.

      METHOD show_html.
        cl_abap_browser=>show_html( html_string = iv_html
                                    size = iv_size
                                    context_menu = abap_true ).
      ENDMETHOD.

      METHOD output.
        CASE is_cfg-output_mode.
          WHEN c_mode_url.
            show_html( |<img src="{ to_url( ) }"/>\n{ source( is_cfg-display_source ) }| ).

          WHEN OTHERS.
*       export data as PlantUML source
            lcl_file=>download( io_name = lcl_file_name=>new( is_cfg-output_mode )
                                iv_data = to_xstring( mv_diagram ) ).
        ENDCASE.
      ENDMETHOD.                    "output

      METHOD to_url.
        DATA lv_bin TYPE xstring.
*   for PlantUML Server: Convert to UTF-8, then deflate, then encode (base64 variant)
        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = to_xstring( mv_diagram )   " UTF-8
            compress_level = 9
          IMPORTING
            gzip_out       = lv_bin ).

        rv_url = iv_base_url &&
                 translate( val = cl_http_utility=>encode_x_base64( lv_bin )
                            from = c_standard
                            to =   c_plantuml ).
      ENDMETHOD.                    "to_url

      METHOD to_xstring.
        cl_abap_conv_out_ce=>create( encoding = 'UTF-8' )->convert( EXPORTING data = iv_string
                                                                    IMPORTING buffer = rv_xstring ).
      ENDMETHOD.                    "to_xstring

      METHOD parameter_string.
        rv_param = |-jar { is_cfg-java_jar } -o { is_cfg-local_path } "{ io_name->get_fullpath( ) }"|.
      ENDMETHOD.

      METHOD png_file_name.
        TRY.
            rv_name = |{ is_cfg-local_path }{ io_name->get_prefix( ) }.png|.
          CATCH cx_dynamic_check.
            CLEAR rv_name.
        ENDTRY.
      ENDMETHOD.

      METHOD to_png.
        CLEAR rv_name.
        cl_gui_frontend_services=>execute(
          EXPORTING application = is_cfg-java_appl
                    parameter = parameter_string( io_name = io_name
                                                  is_cfg = is_cfg )
                    synchronous = 'X'
          EXCEPTIONS OTHERS = 1 ).
        CHECK sy-subrc EQ 0.
        rv_name = png_file_name( io_name = io_name
                                 is_cfg = is_cfg ).
      ENDMETHOD.

    ENDCLASS.                    "lcl_plant_uml IMPLEMENTATION

    CLASS lcl_file_name_dummy IMPLEMENTATION.

      METHOD dialog.
        ms_file-path = |test.txt|.
        rv_user_action = cl_gui_frontend_services=>action_cancel.
      ENDMETHOD.

    ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_file IMPLEMENTATION
*----------------------------------------------------------------------*
    CLASS lcl_file IMPLEMENTATION.

      METHOD download.
        rv_subrc = 1.
        CHECK io_name->dialog( ) NE cl_gui_frontend_services=>action_cancel.

        rv_subrc = cl_uml_utilities=>save_xml_local( xml = iv_data
                                                     filename = io_name->get_fullpath( ) ).
      ENDMETHOD.

    ENDCLASS.                    "lcl_file IMPLEMENTATION

    CLASS lcl_file_name IMPLEMENTATION.

      METHOD new.
        CASE iv_mode.
          WHEN c_mode_aut.
            ro_file = NEW lcl_file_name_dummy( iv_mode ).
          WHEN OTHERS.
            ro_file = NEW lcl_file_name( iv_mode ).
        ENDCASE.
      ENDMETHOD.

      METHOD constructor.
        CASE iv_mode.
          WHEN c_mode_txt.
            ms_file = VALUE #( title = |Save UML text source|
                               ext = |.txt| ).
          WHEN OTHERS.
            ms_file = VALUE #( title = |Save As...|
                               ext = |.txt| ).
        ENDCASE.
      ENDMETHOD.

      METHOD get_prefix.
        rv_name = shift_right( val = ms_file-name
                               places = strlen( ms_file-ext ) ).
      ENDMETHOD.

      METHOD get_fullpath.
        rv_name = ms_file-path.
      ENDMETHOD.

      METHOD dialog.
        DATA lv_path TYPE string ##needed.

        CLEAR rv_user_action.

        cl_gui_frontend_services=>file_save_dialog(
          EXPORTING
            window_title      = ms_file-title           " Window Title
            default_extension = ms_file-ext             " Default Extension
            file_filter       = ms_file-filter
          CHANGING
            filename = ms_file-name          " File Name to Save
            path = lv_path                   " Path to File
            fullpath = ms_file-path          " Path + File Name
            user_action = rv_user_action
    " User Action (C Class Const ACTION_OK, ACTION_OVERWRITE etc)
*   file_encoding =
          EXCEPTIONS
            OTHERS = 0 ).
      ENDMETHOD.

    ENDCLASS.
