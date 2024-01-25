*&---------------------------------------------------------------------*
*& Report  Y369_USEREXIT                                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

 REPORT y369_userexit LINE-SIZE 150 LINE-COUNT 180.

 .
*&--------------------------------------------------------------------*&
* Finding the user-exits of a SAP transaction code
*
* Enter the transaction code in which you are looking for the user-exit
* and it will list you the list of user-exits in the transaction code.
* Also a drill down is possible which will help you to branch to SMOD.
* And moreover you can view the function module plus it also gives the
* indication that whether the particular function module is implemented
* or not
*&--------------------------------------------------------------------*&

 TABLES: tstc, tadir,
         trdir, tfdir, enlfdir, tstct.

*TYPE-POOLS: slis.

 TYPES: BEGIN OF t_inc,
        name(400),
        END OF t_inc.

 TYPES: BEGIN OF t_tstc.
         INCLUDE STRUCTURE tstc.
 TYPES:  ttext      LIKE tstct-ttext,
         v_devclass LIKE tadir-devclass,
        END OF t_tstc.

 DATA: field1(30).
 DATA: value1(100).
 DATA: i_tstc    TYPE STANDARD TABLE OF t_tstc WITH HEADER LINE,
       i_tstcf   TYPE STANDARD TABLE OF tstc WITH HEADER LINE,
       i_tadir   TYPE STANDARD TABLE OF tadir WITH HEADER LINE,
       i_tadirf  TYPE STANDARD TABLE OF tadir WITH HEADER LINE,
       i_tadirf1 TYPE STANDARD TABLE OF tadir WITH HEADER LINE,
       i_trdir   TYPE STANDARD TABLE OF trdir WITH HEADER LINE,
       i_tfdir   TYPE STANDARD TABLE OF tfdir WITH HEADER LINE,
       i_enlfdir TYPE STANDARD TABLE OF enlfdir WITH HEADER LINE.

 DATA: pname      LIKE tfdir-pname,
       include    LIKE tfdir-include,
*       ic         TYPE icon_d,
       ic(2)       TYPE c,
       v_devclass LIKE tadir-devclass,
       type(6).

 SELECT-OPTIONS : p_tcode FOR tstc-tcode.

* PARAMETER: P_RB01 RADIOBUTTON GROUP <G1> DEFAULT 'X',
*            P_RB02 RADIOBUTTON GROUP <G1>.


 TYPES: BEGIN OF t_mod,
        name    LIKE modsapt-name,
        modtext LIKE modsapt-modtext,
        typ     LIKE modsap-typ,
        member  LIKE modsap-member,
       END OF t_mod.

 DATA: v_no   TYPE i,
       v_f(1) TYPE c,
       v_data TYPE c.

 TYPES: BEGIN OF z_userexits,
        tcode   TYPE t_tstc-tcode,
        ttext   TYPE t_tstc-ttext,
        name    TYPE t_mod-name,
        modtext TYPE t_mod-modtext,
*       ic      TYPE icon_d,
        ic(2)   TYPE c,
        type(6),
        member  TYPE t_mod-member,
        v_devclass LIKE tadir-devclass,
        END OF z_userexits.

 DATA: it_outtab TYPE STANDARD TABLE OF z_userexits,
       wa_outtab TYPE z_userexits.

 DATA : jtab  TYPE tadir OCCURS 0 WITH HEADER LINE,
        t     TYPE t_inc OCCURS 0 WITH HEADER LINE,
        i_mod TYPE t_mod OCCURS 0 WITH HEADER LINE.

* DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
*       wa_fieldcat TYPE slis_fieldcat_alv,
*       it_events   TYPE slis_t_event,
*       is_selfield TYPE slis_selfield.

*&--------------------------------------------------------------------*&

 START-OF-SELECTION.


  SELECT tstc~tcode pgmna dypno menue cinfo arbgb ttext FROM tstc INNER
JOIN
  tstct ON tstc~tcode = tstct~tcode
  INTO TABLE I_TSTC WHERE tstc~tcode NOT  LIKE
  'Z%' AND tstc~tcode NOT LIKE 'Y%' AND sprsl = sy-langu
  AND tstc~tcode IN p_tcode.


   IF NOT i_tstc[] IS INITIAL.

     SELECT * FROM tadir INTO TABLE i_tadir FOR ALL ENTRIES IN i_tstc
   WHERE pgmid = 'R3TR' AND object = 'PROG' AND obj_name = i_tstc-pgmna.

*select * from tadir into table i_tadir where pgmid = 'R3TR' AND
*object='PROG'.

     SORT i_tadir BY pgmid object obj_name.

     DELETE ADJACENT DUPLICATES FROM i_tadir COMPARING pgmid object
  obj_name.

     SELECT * FROM trdir INTO TABLE i_trdir FOR ALL ENTRIES IN i_tstc
     WHERE name = i_tstc-pgmna.

     SORT i_trdir BY name.

     DELETE ADJACENT DUPLICATES FROM i_trdir COMPARING name.

     LOOP AT i_trdir WHERE subc = 'F'.
       LOOP AT i_tstc WHERE pgmna = i_trdir-name.
         MOVE i_tstc TO i_tstcf.
         APPEND i_tstcf.
       ENDLOOP.
     ENDLOOP.

     IF NOT i_tstcf[] IS INITIAL.

      SELECT * FROM tfdir INTO TABLE i_tfdir FOR ALL ENTRIES IN i_tstcf
WHERE
               pname = i_tstcf-pgmna.

       SORT i_tfdir BY pname.

       DELETE ADJACENT DUPLICATES FROM i_tfdir COMPARING pname.
     ENDIF.

*       Additional Attributes for Function Modules
     IF NOT i_tfdir[] IS INITIAL.

       SELECT * FROM enlfdir INTO TABLE i_enlfdir FOR ALL ENTRIES IN
i_tfdir
          WHERE funcname = i_tfdir-funcname.
       SORT i_enlfdir BY funcname.
       DELETE ADJACENT DUPLICATES FROM i_enlfdir COMPARING funcname.
     ENDIF.


*       Directory of Repository Objects
     SELECT * FROM tadir INTO TABLE i_tadirf
     WHERE pgmid = 'R3TR'    AND object = 'FUGR'.

     LOOP AT i_enlfdir.
       LOOP AT i_tadirf WHERE obj_name+0(26) = i_enlfdir-area.
         MOVE i_tadirf TO i_tadirf1.
         APPEND i_tadirf1.
       ENDLOOP.
     ENDLOOP.

*   Get the  SAP enhancements of the development class of the object
     LOOP AT i_tstc.
      READ TABLE i_tadir WITH KEY obj_name = i_tstc-pgmna BINARY SEARCH
.
       IF sy-subrc = 0.
         i_tstc-v_devclass = i_tadir-devclass.
       ELSE.
         READ TABLE i_tfdir WITH KEY pname = i_tstc-pgmna BINARY SEARCH.
         IF sy-subrc = 0.
           READ TABLE i_enlfdir WITH KEY funcname = i_tfdir-funcname
BINARY SEARCH.
           IF sy-subrc = 0.
             READ TABLE i_tadirf1 WITH KEY obj_name+0(26) =
i_enlfdir-area BINARY SEARCH.
             IF sy-subrc = 0.
               i_tstc-v_devclass = i_tadirf1-devclass.
             ENDIF.
           ENDIF.
         ENDIF.
       ENDIF.
       MODIFY i_tstc.
     ENDLOOP.

     SELECT * FROM tadir INTO TABLE jtab FOR ALL ENTRIES IN i_tstc
                   WHERE pgmid = 'R3TR'
                     AND object = 'SMOD'
                     AND devclass = i_tstc-v_devclass.

     LOOP AT i_tstc .
       CLEAR:v_f.
       v_f = 'Y'.
       IF NOT jtab[] IS INITIAL.
         LOOP AT jtab WHERE pgmid = 'R3TR'
         AND object = 'SMOD'
         AND devclass = i_tstc-v_devclass.

*      Getting the components of a exit.
           CLEAR:i_mod.
           REFRESH:i_mod.

           SELECT a~name a~modtext b~typ b~member
           INTO TABLE i_mod
           FROM modsapt AS a
           INNER JOIN modsap AS b ON b~name = a~name
           WHERE a~sprsl = sy-langu
           AND a~name = jtab-obj_name
           AND b~typ <> ''.
           LOOP AT i_mod.
             CLEAR:pname,include.

* Deriving the include name inorder to chech whether the exit is *
*implemented or not
             SELECT pname include INTO
             (pname ,include)
             FROM tfdir
             WHERE funcname = i_mod-member.
             ENDSELECT.
             pname = pname+3(37).
             CONCATENATE pname 'U' include INTO pname.
             REFRESH t.
             CLEAR t.
* Reads the function module

             READ REPORT pname INTO t.

             LOOP AT t WHERE name CS 'include'.
               CLEAR:pname,field1.
               SHIFT t-name LEFT DELETING LEADING space.
               SPLIT t-name AT ' ' INTO field1 pname.
               SHIFT pname RIGHT DELETING TRAILING ''.
               SHIFT pname RIGHT DELETING TRAILING '.'.
               SHIFT pname LEFT DELETING LEADING space.

             ENDLOOP.

             FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
             REFRESH t.
             CLEAR t.
*   read the include file
*read table i_tadir with key obj_name = pname.

             READ REPORT pname INTO t.
             IF sy-subrc = 0.
*               ic = icon_okay.
              ic = 'Y'.
             ELSE.
               ic = ''.
             ENDIF.

*             for exit types
             IF i_mod-typ = 'E'.
               type = 'Func.'.
             ELSEIF i_mod-typ = 'S'.
               type = 'Screen'.
             ELSEIF i_mod-typ = 'C'.
               type = 'Menu'.
             ENDIF.

*             IF ic = icon_okay.
              if ic = 'Y'.

*  Fetch entries in internal table.
               wa_outtab-tcode   = i_tstc-tcode.
               wa_outtab-ttext   = i_tstc-ttext.
               wa_outtab-name    = i_mod-name.
               wa_outtab-modtext = i_mod-modtext.
               wa_outtab-member  = i_mod-member.
               wa_outtab-type    = type.
               wa_outtab-ic      = ic.
               wa_outtab-v_devclass = i_tstc-v_devclass.
               APPEND wa_outtab TO it_outtab.
               CLEAR wa_outtab.

             ENDIF.
             CLEAR:pname,ic,field1,type.

           ENDLOOP.

         ENDLOOP.
         v_data = 'X'.
*       ELSE.
*         FORMAT COLOR COL_NEGATIVE INTENSIFIED ON.
*         WRITE:/(95) 'No User Exit exists'.
       ENDIF.
     ENDLOOP.

*   ELSE.
*     FORMAT COLOR COL_NEGATIVE INTENSIFIED ON.
*     WRITE:/(95) 'Transaction Code Does Not Exist'.
   ENDIF.

   IF v_data = 'X'.


*     PERFORM build_field_catalog.
*     PERFORM alv_display.
     Perform Write_ouput.

      ENDIF.


*&------------------------------------------------*
*&      Form  build_field_catalog
*&------------------------------------------------*

* FORM build_field_catalog .
*
*   wa_fieldcat-seltext_l = 'Transaction Code'.
*   wa_fieldcat-col_pos   = 1.
*   wa_fieldcat-outputlen = 15.
*   wa_fieldcat-fieldname = 'TCODE'.
*   wa_fieldcat-just      = 'C'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Transaction Text'.
*   wa_fieldcat-col_pos   = 2.
*   wa_fieldcat-outputlen = 20.
*   wa_fieldcat-fieldname = 'TTEXT'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Exit Name'.
*   wa_fieldcat-col_pos   = 3.
*   wa_fieldcat-outputlen = 20.
*   wa_fieldcat-fieldname = 'NAME'.
*   wa_fieldcat-hotspot   = 'X'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Description'.
*   wa_fieldcat-col_pos   = 4.
*   wa_fieldcat-outputlen = 70.
*   wa_fieldcat-fieldname = 'MODTEXT'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Imp.'.
*   wa_fieldcat-col_pos   = 5.
*   wa_fieldcat-outputlen = 5.
*   wa_fieldcat-fieldname = 'IC'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Type'.
*   wa_fieldcat-col_pos   = 6.
*   wa_fieldcat-outputlen = 10.
*   wa_fieldcat-fieldname = 'TYPE'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Function Name'.
*   wa_fieldcat-col_pos   = 7.
*   wa_fieldcat-outputlen = 20.
*   wa_fieldcat-fieldname = 'MEMBER'.
*   wa_fieldcat-hotspot = 'X'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*   wa_fieldcat-seltext_l = 'Package'.
*   wa_fieldcat-col_pos   = 8.
*   wa_fieldcat-outputlen = 15.
*   wa_fieldcat-fieldname = 'V_DEVCLASS'.
*   wa_fieldcat-just      = 'C'.
*   APPEND wa_fieldcat TO it_fieldcat.
*   CLEAR wa_fieldcat.
*
*
* ENDFORM.                    " build_field_catalog

*&---------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------*
*       text
*----------------------------------------------------*

* FORM alv_display .
*
*   PERFORM alv_report_events TABLES it_events.
*
*   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
**   I_INTERFACE_CHECK                 = ' '
**   I_BYPASSING_BUFFER                = ' '
**   I_BUFFER_ACTIVE                   = ' '
*      i_callback_program                = sy-repid
**   I_CALLBACK_PF_STATUS_SET          = ' '
**   I_CALLBACK_USER_COMMAND           = ' '
**   i_callback_top_of_page            = ' '
**   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**   i_callback_html_end_of_list       = ' '
**   I_STRUCTURE_NAME                  =
**   I_BACKGROUND_ID                   = ' '
**   I_GRID_TITLE                      =
**   I_GRID_SETTINGS                   =
**   IS_LAYOUT                         =
*     it_fieldcat                       = it_fieldcat
**   IT_EXCLUDING                      =
**   IT_SPECIAL_GROUPS                 =
**   IT_SORT                           =
**   IT_FILTER                         =
**   IS_SEL_HIDE                       =
**   I_DEFAULT                         = 'X'
*      i_save                            = 'X'
**   IS_VARIANT                        =
*   it_events                         = it_events
**   IT_EVENT_EXIT                     =
**   IS_PRINT                          =
**   IS_REPREP_ID                      =
**   I_SCREEN_START_COLUMN             = 0
**   I_SCREEN_START_LINE               = 0
**   I_SCREEN_END_COLUMN               = 0
**   I_SCREEN_END_LINE                 = 0
**   IT_ALV_GRAPHICS                   =
**   IT_HYPERLINK                      =
**   IT_ADD_FIELDCAT                   =
**   IT_EXCEPT_QINFO                   =
**   I_HTML_HEIGHT_TOP                 =
**   I_HTML_HEIGHT_END                 =
**    IMPORTING
**   E_EXIT_CAUSED_BY_CALLER           =
**   ES_EXIT_CAUSED_BY_USER            =
*     TABLES
*       t_outtab                          = it_outtab
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
*             .
*   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*   ENDIF.
*
*
* ENDFORM.                    " alv_display


*&-----------------------------------------------------*
*&      Form  alv_report_events
*&------------------------------------------------------*
*       text
*-------------------------------------------------------*
*      -->P_IT_EVENTS  text
*-------------------------------------------------------*

* FORM alv_report_events  TABLES lt_events TYPE slis_t_event.
*
*   CLEAR   lt_events.
*   REFRESH lt_events.
*
*   lt_events-name = slis_ev_user_command.
*   lt_events-form = 'USER_COMMAND'.
*   APPEND lt_events.
*
* ENDFORM.                    " alv_report_events

**
**-------------------------------------------------------------------*
**       FORM USER_COMMAND                                           *
**-------------------------------------------------------------------*
**       Subroutine attached as callback form to ABAP List Viewer    *
**   --> UCOMM    - user command code passed from ALV                *
**   --> SELFIELD - information record describing current line/field *
**-------------------------------------------------------------------*
*
* FORM user_command USING v_ucomm LIKE sy-ucomm
*                         is_selfield     TYPE slis_selfield.
*
*   CASE v_ucomm.
*     WHEN '&IC1'.                                  "ALV record
*selection
*       PERFORM display_transaction USING is_selfield.
*   ENDCASE.
*
* ENDFORM.                               " USER_COMMAND
*
**&-----------------------------------------------------------------
**
**&      Form  display_record
**&-----------------------------------------------------------------
**
**       text
**------------------------------------------------------------------
**
**      -->P_IS_SELFIELD  text
**------------------------------------------------------------------
**
*
* FORM display_transaction  USING is_selfield TYPE slis_selfield.
*
*   CLEAR wa_outtab.
*   READ TABLE it_outtab INTO wa_outtab
*                 INDEX is_selfield-tabindex.
*
*   IF sy-subrc = 0.
*
*     CASE is_selfield-fieldname.
*
*       WHEN 'NAME'.
*
** Jump to transaction SMOD
*         SET PARAMETER ID 'MON' FIELD wa_outtab-name.
*         CALL TRANSACTION 'SMOD' AND SKIP FIRST SCREEN.
*
*       WHEN 'MEMBER'.
*
** Viewing the function
*         SUBMIT wb_mngr_start_from_tool_access AND RETURN
*             WITH action   = 'DISPLAY'
**          WITH OBJdata  = p_object_data
*             WITH obj_type = 'FF'
*             WITH obj_name = wa_outtab-member
*             WITH encl_obj = ''
*             WITH position = ''
*             WITH include  = ''
*             WITH version  = ''
*             WITH tool     = 'CL_FUNCTION_BUILDER'
*             WITH newwndow = ''
*             WITH objlist  = ''.
*
*     ENDCASE.
*   ENDIF.
*
* ENDFORM.                    " display_transaction
*&---------------------------------------------------------------*
*&      Form  Write_ouput
*&---------------------------------------------------------------*
*       text
*----------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------*
form Write_ouput .

write : /'Transaction Code',22'Exitname',31'Type',
40'Function Name',72'Package'.

Loop at it_outtab into wa_outtab.

write : / wa_outtab-tcode,wa_outtab-name , wa_outtab-type,
wa_outtab-member,wa_outtab-v_devclass.

endloop.
endform.     " Write_ouput
