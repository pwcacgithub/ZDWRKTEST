*&---------------------------------------------------------------------*
*& Include          ZMP_CDSACCLERATOR_SUBROUTINE
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form display_fields
*&---------------------------------------------------------------------*
FORM display_fields USING fp_tab.

  DATA lt_fieldcat TYPE TABLE OF slis_fieldcat_alv.

  DATA lt_fieldselection TYPE TABLE OF ty_fieldlist.

  DATA ls_fieldsel LIKE LINE OF lt_fieldselection.

  DATA lv_exit.

  IF fp_tab IS INITIAL.
    MESSAGE 'Please enter a valid table name' TYPE 'I'.
    EXIT.
  ENDIF.

  DATA(lv_callback_program) = sy-repid.

  lt_fieldcat = VALUE #( ( fieldname = 'FIELDNAME' tabname = 'LT_FIELDSELECTION' seltext_m = 'Field Name' outputlen = 30 ) ).

  SELECT ' ' AS checkbox,
         fieldname
    FROM dd03l
    INTO TABLE @lt_fieldselection
    WHERE tabname = @fp_tab
    AND   fieldname NE 'MANDT'.

  IF sy-subrc NE 0.
    MESSAGE 'Please enter a valid table name' TYPE 'I'.
    EXIT.
  ENDIF.

  DELETE lt_fieldselection WHERE fieldname(1) EQ '.'.

  ls_fieldsel-checkbox = abap_true.

  LOOP AT gt_tabfield INTO DATA(ls_tabfield) WHERE tabname = fp_tab.
    MODIFY lt_fieldselection FROM ls_fieldsel TRANSPORTING checkbox WHERE fieldname = ls_tabfield-fieldname.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = |Select fields for { fp_tab } table|
      i_tabname            = 'LT_FIELDSELECTION'
      i_checkbox_fieldname = 'CHECKBOX'
      i_zebra              = abap_true
      it_fieldcat          = lt_fieldcat
      i_callback_program   = lv_callback_program
    IMPORTING
      e_exit               =  lv_exit
    TABLES
      t_outtab             = lt_fieldselection
    EXCEPTIONS
      program_error                 = 1
      OTHERS                        = 2.

  IF sy-subrc EQ 0 AND lv_exit EQ abap_false.
    DELETE lt_fieldselection WHERE checkbox NE abap_true.

    DATA(lv_lastindex) = line_index( gt_tabfield[ tabname = fp_tab ] ).
    DELETE gt_tabfield WHERE tabname = fp_tab.

    LOOP AT lt_fieldselection INTO DATA(ls_fieldselection).
      IF lv_lastindex IS INITIAL.
        APPEND VALUE #( tabname = fp_tab fieldname = ls_fieldselection-fieldname ) TO gt_tabfield.
      ELSE.
        INSERT VALUE #( tabname = fp_tab fieldname = ls_fieldselection-fieldname ) INTO gt_tabfield INDEX lv_lastindex.
        lv_lastindex = lv_lastindex + 1.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_joinfields
*&---------------------------------------------------------------------*
FORM add_joinfields .

  IF gs_screen1002 IS INITIAL.
    MESSAGE 'Enter all necessary data' TYPE 'I'.
    EXIT.
  ENDIF.
  IF gs_screen1002-lhs_table EQ gs_screen1002-rhs_table.
    MESSAGE 'Same tables mentioned for LHS and RHS' TYPE 'I'.
    EXIT.
  ENDIF.
  APPEND VALUE #( lhs_tab = gs_screen1002-lhs_table
                  lhs_tilde = ' ~ '
                  lhs_field = gs_screen1002-lhs_field
                  equal = ' = '
                  rhs_tab = gs_screen1002-rhs_table
                  rhs_tilde = ' ~ '
                  rhs_field = gs_screen1002-rhs_field ) TO gt_joinfield.
  CLEAR gs_screen1002.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form generate_ddl_source
*&---------------------------------------------------------------------*
FORM generate_ddl_source .

  RANGES lr_tab FOR dd03l-tabname.

*  CONSTANTS lv_newline TYPE string VALUE '\r\n'.

  DATA: lv_annotations  TYPE cstring,
        lv_field_string TYPE cstring,
        join_stmt       TYPE cstring.

  "Annotations
  lv_annotations = |@AbapCatalog.sqlViewName: '{ gv_sqlname }'\r\n| &
                   |@AbapCatalog.compiler.compareFilter: true\r\n| &
                   |@EndUserText.label: '{ gv_sqldesc }'\r\n|.

*  lv_field_string = REDUCE string( INIT field_string TYPE string
*                                   FOR s_field IN gt_tabfield
*                                   NEXT field_string = |{ field_string },\r\n{ s_field-tabname }.{ s_field-fieldname }| ).

  lv_field_string = REDUCE string( INIT field_string TYPE string
                                   FOR s_field IN gt_tabfield
                                   NEXT field_string = cond #( LET flstr = |{ s_field-tabname }.{ s_field-fieldname }| header = |{ field_string }, \r\n| IN
                                   WHEN s_field-key EQ abap_true AND s_field-alias IS NOT INITIAL THEN |{ header }Key { flstr } as { s_field-alias }|
                                   WHEN s_field-key EQ abap_true THEN |{ header }Key { flstr }|
                                   WHEN s_field-alias IS NOT INITIAL THEN |{ header }{ flstr } as { s_field-alias }|
                                   ELSE |{ header }{ flstr }| ) ).

*Remove comma from string
  lv_field_string = to_upper( lv_field_string ).
  WHILE  NOT ( lv_field_string(1) CA sy-abcde ).
    lv_field_string = lv_field_string+1.
  ENDWHILE.
  DO 4 TIMES.

    ASSIGN COMPONENT |TAB{ sy-index }| OF STRUCTURE gs_tabname TO FIELD-SYMBOL(<fs_lhstable>).

    ASSIGN COMPONENT |TAB{ sy-index + 1 }| OF STRUCTURE gs_tabname TO FIELD-SYMBOL(<fs_rhstable>).

    ASSIGN COMPONENT |JOIN{ sy-index }| OF STRUCTURE gs_screen1001 TO FIELD-SYMBOL(<fs_jointype>).

    CHECK <fs_rhstable> IS NOT INITIAL.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_lhstable> ) TO lr_tab.

    DATA(on_cond) = REDUCE string( INIT oncond TYPE string
                                   FOR s_joinfield IN gt_joinfield
                                   WHERE ( ( lhs_tab IN lr_tab OR rhs_tab IN lr_tab )
                                   AND   ( lhs_tab EQ <fs_rhstable> OR rhs_tab EQ <fs_rhstable> ) )
                                   NEXT oncond = |{ oncond } AND { s_joinfield-lhs_tab }.{ s_joinfield-lhs_field } = { s_joinfield-rhs_tab }.{ s_joinfield-rhs_field }\r\n| ).

     IF on_cond IS NOT INITIAL.
      on_cond = on_cond+5.
    ENDIF.

    join_stmt = |{ join_stmt } { <fs_jointype> } { <fs_rhstable> } ON { on_cond }|.
  ENDDO.

  IF join_stmt IS NOT INITIAL.
    join_stmt = join_stmt+1.
  ENDIF.

  gv_ddl_source = |{ lv_annotations }| & |DEFINE VIEW { gv_cdsname } AS SELECT FROM { gs_tabname-tab1 }\r\n| &
  |{ join_stmt }| &
  |\{\r\n| &
  |{ lv_field_string }\r\n| &
  |\}\r\n|.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form generate_cds_view
*&---------------------------------------------------------------------*
FORM generate_cds_view .

  DATA(lr_ddl) = cl_dd_ddl_handler_factory=>create( ).

  DATA(lr_ddl_internal) = cl_dd_ddl_handler_factory=>create_internal( ).

  DATA(ls_ddddlsrcv) = VALUE ddddlsrcv( ddlname = gv_cdsname
                                        ddtext = gv_cdsdesc
                                        ddlanguage = sy-langu
                                        source = gv_ddl_source ).

*Save the DDL source
  TRY.
      CALL METHOD lr_ddl->save
        EXPORTING
          name         = CONV ddlname( gv_cdsname )
          put_state    = 'N'
          ddddlsrcv_wa = ls_ddddlsrcv
        IMPORTING
          save_date    = DATA(save_date)
          save_time    = DATA(save_time).
    CATCH cx_dd_ddl_save.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
  ENDTRY.

*Activate the DDL source, this will create SQL view in SE11
  TRY.
      CALL METHOD lr_ddl->activate
        EXPORTING
          name = CONV ddlname( gv_cdsname ).
    CATCH cx_dd_ddl_activate.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
  ENDTRY.

  MESSAGE |The view { gv_sqlname } has been generated successfully using DDL source { gv_cdsname } , Kindly check SE11 !!| TYPE 'S'.

ENDFORM.

FORM suggest_joinfields.
  TABLES dd03l.
  RANGES lr_tabname FOR dd03l-tabname.

  DATA lt_fieldcat TYPE TABLE OF slis_fieldcat_alv.

  TYPES: BEGIN OF ty_suggest,
          checkbox.
          include type ty_joinfield.
  TYPES END OF ty_suggest.

  DATA lt_suggest TYPE TABLE OF ty_suggest.

  DO 5 TIMES.
    ASSIGN COMPONENT |TAB{ sy-index }| OF STRUCTURE gs_tabname TO FIELD-SYMBOL(<fs_tabname>).
    CHECK <fs_tabname> IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_tabname> ) TO lr_tabname.
  ENDDO.

  SELECT tabname, fieldname, rollname
      FROM dd03l
      INTO TABLE @DATA(lt_dd03l)
      WHERE tabname IN @lr_tabname
      AND keyflag EQ @abap_true
      AND fieldname NE 'MANDT'.

  LOOP AT lt_dd03l INTO DATA(ls_loop1).
    LOOP AT lt_dd03l INTO DATA(ls_loop2) WHERE tabname NE ls_loop1-tabname AND rollname EQ ls_loop1-rollname.
      CHECK NOT line_exists( lt_suggest[ lhs_tab = ls_loop2-tabname lhs_field = ls_loop2-fieldname
                                         rhs_tab = ls_loop1-tabname rhs_field = ls_loop1-fieldname ] ).

      APPEND VALUE #( checkbox = cond #( WHEN line_exists( gt_joinfield[ lhs_tab = ls_loop1-tabname
                                                                         lhs_field = ls_loop1-fieldname
                                                                         rhs_tab = ls_loop2-tabname
                                                                         rhs_field = ls_loop2-fieldname ] ) THEN abap_true
                                         ELSE abap_false )
                      lhs_tab = ls_loop1-tabname
                      lhs_tilde = '~'
                      lhs_field = ls_loop1-fieldname
                      equal = '='
                      rhs_tab = ls_loop2-tabname
                      rhs_tilde = '~'
                      rhs_field = ls_loop2-fieldname ) TO lt_suggest.
    ENDLOOP.
  ENDLOOP.

  lt_fieldcat = VALUE #( ( fieldname = 'CHECKBOX' tabname = 'LT_SUGGEST' outputlen = 2 )
                         ( fieldname = 'LHS_TAB' tabname = 'LT_SUGGEST' seltext_m = 'LHS Table' outputlen = 15 )
                         ( fieldname = 'LHS_FIELD' tabname = 'LT_SUGGEST' seltext_m = 'LHS Field' outputlen = 15 )
                         ( fieldname = 'EQUAL' tabname = 'LT_SUGGEST' outputlen = 1 )
                         ( fieldname = 'RHS_TAB' tabname = 'LT_SUGGEST' seltext_m = 'RHS Table' outputlen = 15 )
                         ( fieldname = 'RHS_FIELD' tabname = 'LT_SUGGEST' seltext_m = 'RHS Field' outputlen = 15 ) ).

  DATA lv_exit.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title              = |Select Join conditions|
      i_tabname            = 'LT_SUGGEST'
      i_checkbox_fieldname = 'CHECKBOX'
      i_zebra              = abap_true
      it_fieldcat          = lt_fieldcat
      i_callback_program   = sy-repid
    IMPORTING
      e_exit               = lv_exit
    TABLES
      t_outtab             = lt_suggest
    EXCEPTIONS
      program_error        = 1
      OTHERS               = 2.

  IF lv_exit NE abap_true.
*    TYPES ty_joinfieldtt TYPE TABLE OF ty_joinfield.,.
**    DATA(lt_selected_rows) =  VALUE ty_joinfieldtt( FOR s_sug IN lt_suggest WHERE ( checkbox = abap_true ) ( lhs_table = s_sug-lhs_table ) ).
*     DATA(lt_selected_rows) = lt_suggest[ checkbox = abap_true ].
*     APPEND LINES OF CONV ty_joinfield( lt_selected_rows ) TO gt_joinfield.
    LOOP AT lt_suggest INTO DATA(ls_suggest) WHERE checkbox EQ abap_true.
      CHECK NOT line_exists( gt_joinfield[  lhs_tab = ls_suggest-lhs_tab
                                            rhs_tab = ls_suggest-rhs_tab
                                            lhs_field = ls_suggest-lhs_field
                                            rhs_field = ls_suggest-rhs_field ] ).
      APPEND CORRESPONDING ty_joinfield( ls_suggest ) TO gt_joinfield.
    ENDLOOP.
*      APPEND LINES OF VALUE #( FOR s_sug IN lt_suggest WHERE ( checkbox = abap_true ) ( lhs_table = s_sug-lhs_tab ) ) to gt_joinfield.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  KEY_SELECT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE key_select INPUT.
BREAK-POINT.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_FIELD_TABLE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_field_table INPUT.
  MODIFY gt_tabfield FROM gs_tabfield TRANSPORTING key alias WHERE tabname = gs_tabfield-tabname AND fieldname = gs_tabfield-fieldname.
ENDMODULE.
