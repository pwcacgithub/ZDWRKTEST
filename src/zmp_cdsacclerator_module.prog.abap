*----------------------------------------------------------------------*
***INCLUDE ZDWRK_CDS_MP_MODULE.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_1000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'PF1000'.
* SET TITLEBAR 'TITLE1000' OF PROGRAM sy-repid WITH 'Step 1 : Enter SQL & CDS view details'.
  SET TITLEBAR 'TITLE1000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  IF sy-ucomm EQ 'BUT1'.
*  CALL SCREEN '1010'.
    CALL SCREEN '1001'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
  SET TITLEBAR 'TITLE1001'.

  CLEAR gt_values.

  gt_values = VALUE #( ( key = 'Inner Join' text = 'Inner Join' )
                       ( key = 'Left Outer Join' text = 'Left Outer Join' )
                       ( key = 'Right Outer Join' text = 'Right Outer Join' ) ).

  DO 4 TIMES.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = CONV vrm_id( |GS_SCREEN1001-JOIN{ sy-index }| )
        values          = gt_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

  ENDDO.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_1001 INPUT.

  DATA bool.

  CLEAR bool.
  DO 4 TIMES.
    ASSIGN COMPONENT |TAB{ sy-index }| OF STRUCTURE gs_tabname TO FIELD-SYMBOL(<fs_lhstab>).
    ASSIGN COMPONENT |TAB{ sy-index + 1 }| OF STRUCTURE gs_tabname TO FIELD-SYMBOL(<fs_rhstab>).
    ASSIGN COMPONENT |JOIN{ sy-index }| OF STRUCTURE gs_screen1001 TO FIELD-SYMBOL(<fs_tab>).

    IF <fs_rhstab> IS NOT INITIAL AND <fs_tab> IS INITIAL.
      MESSAGE |Enter join type for { <fs_lhstab> } and { <fs_rhstab> }| TYPE 'I'.
      bool = abap_true.
      EXIT.
    ENDIF.
  ENDDO.

  CHECK bool EQ abap_false.

  CASE sy-ucomm.
    WHEN 'FC_NEXT'.
      IF gs_tabname IS INITIAL.
        MESSAGE 'Enter atleast one table to proceed!!' TYPE 'I'.
      ELSEIF gs_tabname-tab2 IS INITIAL.
        CALL SCREEN '1003'.
      ELSE.
        CALL SCREEN '1002'.
      ENDIF.
    WHEN 'FC_PREV'.
*      CALL SCREEN '1000'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1002 OUTPUT.
  SET PF-STATUS 'PF1002'.
  SET TITLEBAR 'TITLE1002'.

  CLEAR: gt_values.

  DO 5 TIMES.
    DATA(tabname) = |GS_TABNAME-TAB{ sy-index }|.
    ASSIGN (tabname) TO FIELD-SYMBOL(<tab>).
    IF <tab> IS ASSIGNED AND <tab> IS NOT INITIAL.
      APPEND VALUE #( key = <tab> text = <tab> ) TO gt_values.
    ENDIF.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = CONV vrm_id( |GS_SCREEN1002-LHS_TABLE| )
      values          = gt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = CONV vrm_id( |GS_SCREEN1002-RHS_TABLE| )
      values          = gt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.

  CASE sy-ucomm.
    WHEN 'FC_NEXT'.
      CALL SCREEN '1003'.
    WHEN 'FC_PREV'.
      LEAVE TO SCREEN 0.
    WHEN 'FC_ADD' OR 'ENTER'.
      PERFORM add_joinfields.
    WHEN 'FC_SUG'.
      PERFORM suggest_joinfields.
    WHEN  OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1003 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1003 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
  SET TITLEBAR 'TITLE1003'.

  DATA: but_tab1 TYPE c LENGTH 30,
        but_tab2 TYPE c LENGTH 30,
        but_tab3 TYPE c LENGTH 30,
        but_tab4 TYPE c LENGTH 30,
        but_tab5 TYPE c LENGTH 30.

  but_tab1 = |Fields for { gs_tabname-tab1 }|.
  but_tab2 = |Fields for { gs_tabname-tab2 }|.
  but_tab3 = |Fields for { gs_tabname-tab3 }|.
  but_tab4 = |Fields for { gs_tabname-tab4 }|.
  but_tab5 = |Fields for { gs_tabname-tab5 }|.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'BUT_TAB2'.
        IF gs_tabname-tab2 IS INITIAL.
          screen-invisible = 1.
        ENDIF.
      WHEN 'BUT_TAB3'.
        IF gs_tabname-tab3 IS INITIAL.
          screen-invisible = 1.
        ENDIF.
      WHEN 'BUT_TAB4'.
        IF gs_tabname-tab4 IS INITIAL.
          screen-invisible = 1.
        ENDIF.
      WHEN 'BUT_TAB5'.
        IF gs_tabname-tab5 IS INITIAL.
          screen-invisible = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1003 INPUT.

  CASE sy-ucomm.
    WHEN 'FC_NEXT'.
      PERFORM generate_ddl_source.
      CALL SCREEN '1004'.
    WHEN 'FC_PREV'.
      LEAVE TO SCREEN 0.
    WHEN 'FC_TAB1'.
      PERFORM display_fields USING gs_tabname-tab1.
    WHEN 'FC_TAB2'.
      PERFORM display_fields USING gs_tabname-tab2.
    WHEN 'FC_TAB3'.
      PERFORM display_fields USING gs_tabname-tab3.
    WHEN 'FC_TAB4'.
      PERFORM display_fields USING gs_tabname-tab4.
    WHEN 'FC_TAB5'.
      PERFORM display_fields USING gs_tabname-tab5.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1004 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1004 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  DATA:
*     reference to wrapper class of control based on OO Framework
    note_editor               TYPE REF TO cl_gui_textedit,
*     reference to custom container: necessary to bind text edit control
    textedit_custom_container TYPE REF TO cl_gui_custom_container.


  TYPES notepad_line(100) TYPE c.
  DATA: lt_texttable TYPE notepad_line OCCURS 0.

  IF textedit_custom_container IS NOT BOUND.


* create SAP Custom Container; define container object in dynpro
    CREATE OBJECT textedit_custom_container
      EXPORTING
        container_name              = 'CUSTOM_CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
*   add your handling
    ENDIF.

* call constructor to initialize and link the text edit control
* to SAP Custom Container
    CREATE OBJECT note_editor
      EXPORTING
        parent                     = textedit_custom_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.

* without toolbar?
    CALL METHOD note_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false
      EXCEPTIONS
        OTHERS       = 1.

* without statusbar?
    CALL METHOD note_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false
      EXCEPTIONS
        OTHERS         = 1.
*

    note_editor->set_readonly_mode( ).


  ENDIF.

  DATA lv_ddlstr TYPE string.
  lv_ddlstr = gv_ddl_source.

  CLEAR lt_texttable.
  WHILE lv_ddlstr IS NOT INITIAL.
    IF strlen( lv_ddlstr ) GE 100.
      DATA(line) = lv_ddlstr(100).
      lv_ddlstr = lv_ddlstr+100.
    ELSE.
      line = lv_ddlstr.
      CLEAR lv_ddlstr.
    ENDIF.
    APPEND line TO lt_texttable.
  ENDWHILE.

* and send text to note editor
  CALL METHOD note_editor->set_text_as_stream
    EXPORTING
      text   = lt_texttable
    EXCEPTIONS
      OTHERS = 1.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1004 INPUT.

  CASE sy-ucomm.
    WHEN 'FC_PREV'.
*      textedit_custom_container->free( ).
*      note_editor->free( ).
      LEAVE TO SCREEN 0.
    WHEN 'FC_GEN'.
      PERFORM generate_cds_view.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_FIELDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_fields INPUT.
  CASE sy-ucomm.
    WHEN 'FC_DEL'.
      DELETE gt_tabfield INDEX tblcrtl1003-current_line.
*  	WHEN .
*  	WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  DELETE_SELECTED_ROWS_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE delete_selected_rows_1002 INPUT.
  CASE sy-ucomm.
    WHEN 'FC_DEL'.
      DELETE gt_joinfield INDEX tbl_control-current_line.
*  	WHEN .
*  	WHEN OTHERS.
  ENDCASE.
ENDMODULE.
