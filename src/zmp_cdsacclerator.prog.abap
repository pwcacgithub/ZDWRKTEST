*&---------------------------------------------------------------------*
*& Modulpool ZDWRK_CDS_MP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zmp_cdsacclerator.

INITIALIZATION.

TYPES:BEGIN OF ty_tabname,
         tab1 TYPE tabname,
         tab2 TYPE tabname,
         tab3 TYPE tabname,
         tab4 TYPE tabname,
         tab5 TYPE tabname,
       END OF ty_tabname,

       BEGIN OF ty_tabfield,
         key,
         tabname TYPE tabname,
         fieldname TYPE fieldname,
         alias LENGTH 30,
       END OF ty_tabfield,

       BEGIN OF ty_joinfield,
         lhs_tab TYPE tabname,
         lhs_tilde TYPE c LENGTH 3,
         lhs_field TYPE fieldname,
         equal TYPE c LENGTH 3,
         rhs_tab  TYPE tabname,
         rhs_tilde TYPE c LENGTH 3,
         rhs_field TYPE fieldname,
       END OF ty_joinfield,

*       BEGIN OF ty_joinfield,
*         lhs_tab TYPE tabname,
*         lhs_tilde TYPE c LENGTH 20,
*         lhs_field TYPE fieldname,
*         equal TYPE c LENGTH 20,
*         rhs_tab  TYPE tabname,
*         rhs_tilde TYPE c LENGTH 20,
*         rhs_field TYPE fieldname,
*       END OF ty_joinfield,

      BEGIN OF ty_fieldlist,
        checkbox,
        fieldname TYPE fieldname,
      END OF ty_fieldlist.

DATA: gv_sqlname TYPE dbobj_name,
      gv_cdsname TYPE dbobj_name,
      gv_sqldesc TYPE string,
      gv_cdsdesc TYPE string.

DATA: gs_tabname TYPE ty_tabname.

DATA: gt_tabfield TYPE TABLE OF ty_tabfield,
      gs_tabfield TYPE ty_tabfield,
      gt_joinfield TYPE TABLE OF ty_joinfield,
      gs_joinfield TYPE ty_joinfield.

DATA: gt_values TYPE TABLE OF vrm_value,
      gv_ddl_source TYPE cstring.

DATA: BEGIN OF gs_screen1002,
        lhs_table TYPE tabname,
        lhs_field TYPE fieldname,
        rhs_table TYPE tabname,
        rhs_field TYPE fieldname,
      END OF gs_screen1002,

      BEGIN OF gs_screen1001,
        join1 TYPE c LENGTH 20,
        join2 TYPE c LENGTH 20,
        join3 TYPE c LENGTH 20,
        join4 TYPE c LENGTH 20,
      END OF gs_screen1001.

CONTROLS tbl_control TYPE TABLEVIEW USING SCREEN 1002.
CONTROLS tblcrtl1003 TYPE TABLEVIEW USING SCREEN 1003.



INCLUDE zmp_cdsacclerator_subroutine.

INCLUDE zmp_cdsacclerator_module.
