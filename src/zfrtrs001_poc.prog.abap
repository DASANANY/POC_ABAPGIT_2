*&---------------------------------------------------------------------*
*& Report  ZFRTRS001                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
* AUTHOR:  Adrian Borja        DATE: 11/28/2011                        *
*                                                                      *
* DESCRIPTION:                                                         *
* This program is a copy of Program ZFRTRI011 for its retrieval of     *
* Billing from BKPF to be processed in Brand Matrix so that custom     *
* table ZFTMATRIXQUEUE is populated. Another program will actually     *
* process these identified Billing Documents                           *
************************************************************************
* CHANGE HISTORY                                                       *
*                                                                      *
* Mod Date     Changed By     Description                    Chng ID   *
* 11/28/2011   BORJAAD        Initial Creation             GD2K925906  *
*                             R19 SAC ID4480 CRQ314093                 *
* 12/12/2011   BORJAAD        Addition for CRQ314093       GD2K925964  *
*                             ID4480, new column to                    *
*                             ZFTMATRIXQUEUE for BM                    *
*                             relevant condition type                  *
* 01/24/2011   AGCOPRJE       Subtract 1 Hour on the last  GD2K926005  *
*                             Execution time to get accurate           *
*                             Accounting docs and record it            *
*                             to table ZFTMATRIXQUEUE.                 *
*                             INC889395                                *
************************************************************************
REPORT  zfrtrs001_poc MESSAGE-ID zf600.
* INCLUDES
INCLUDE ZFRTRS001_N01.
*INCLUDE <cntn01>.
* Type-pools
TYPES: BEGIN OF t_zmatrix_billing,
         vbeln  TYPE vbeln_vf,
         status TYPE zstatus_matrix,
         datum  TYPE datum,
         bukrs  TYPE bukrs,
       END OF t_zmatrix_billing,
       BEGIN OF t_icompdate_upd,
         zbukrs TYPE t9control-zbukrs,
         zdatex TYPE t9control-zdatex,
         ztimex TYPE t9control-ztimex,
       END OF t_icompdate_upd,
       BEGIN OF t_zmatrix_billing_aux,
         vbeln  TYPE bkpf-awkey,
         status TYPE zstatus_matrix,
         datum  TYPE datum,
         bukrs  TYPE bukrs,
       END OF t_zmatrix_billing_aux,
       BEGIN OF t_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         budat TYPE bkpf-budat,
         awkey TYPE bkpf-awkey,
       END OF t_bkpf,
       BEGIN OF t_bkpf2,
         bukrs  TYPE bkpf-bukrs,    "Company Code
         belnr  TYPE bkpf-belnr,    "Accounting document number
         gjahr  TYPE bkpf-gjahr,    "Fiscal year
         xblnr  TYPE bkpf-xblnr,    "Reference document number
         awkey  TYPE vbrk-vbeln,    "Object key
       END OF t_bkpf2,
       BEGIN OF t_company,
         bukrs TYPE bukrs,
         date  TYPE d,
         time  TYPE t,
       END OF t_company,
       BEGIN OF t_t9fbm005,
         bukrs TYPE t9fbm005-bukrs,
         type  TYPE t9fbm005-type,
       END OF t_t9fbm005,
       BEGIN OF t_sopcklsti1,
        transf_bin TYPE sopcklsti1-transf_bin,
        head_start TYPE sopcklsti1-head_start,
        head_num   TYPE sopcklsti1-head_num,
        body_start TYPE sopcklsti1-body_start,
        body_num   TYPE sopcklsti1-body_num,
        doc_type   TYPE sopcklsti1-doc_type,
        obj_name   TYPE sopcklsti1-obj_name,
        obj_descr  TYPE sopcklsti1-obj_descr,
        obj_langu  TYPE sopcklsti1-obj_langu,
        doc_size   TYPE sopcklsti1-doc_size,
        mess_type  TYPE sopcklsti1-mess_type,
       END OF t_sopcklsti1,

       BEGIN OF t_solisti1,
         line TYPE solisti1-line,
       END OF t_solisti1,

       BEGIN OF t_somlreci1,
         receiver   TYPE somlreci1-receiver,
         rec_type   TYPE somlreci1-rec_type,
         rec_id     TYPE somlreci1-rec_id,
         reply_doc  TYPE somlreci1-reply_doc,
         rec_date   TYPE somlreci1-rec_date,
         proxy_id   TYPE somlreci1-proxy_id,
         retrn_code TYPE somlreci1-retrn_code,
         express    TYPE sosndatti1-express,
         copy       TYPE sosndatti1-copy,
         blind_copy TYPE sosndatti1-blind_copy,
         no_forward TYPE sosndatti1-no_forward,
         no_print   TYPE sosndatti1-no_print,
         to_answer  TYPE sosndatti1-to_answer,
         to_do_expl TYPE sosndatti1-to_do_expl,
         to_do_grp  TYPE sosndatti1-to_do_grp,
         com_type   TYPE soextatti1-com_type,
         lfdnr      TYPE soextatti1-lfdnr,
         fax        TYPE soextatti1-fax,
         country    TYPE soextatti1-country,
         spool_id   TYPE soextatti1-spool_id,
         notif_del  TYPE soextatti1-notif_del,
         notif_read TYPE soextatti1-notif_read,
         notif_ndel TYPE soextatti1-notif_ndel,
         sap_body   TYPE soextatti1-sap_body,
       END OF t_somlreci1.
TYPE-POOLS abap.
* TABLES
TABLES: bkpf,
        vbrk,
        t9control,
        zmatrix_billing.
FIELD-SYMBOLS: <fs_company>  TYPE t_company,
               <fs_company2> TYPE t_company.
DATA: i_log TYPE STANDARD TABLE OF zfs_bm_log,
      x_log TYPE zfs_bm_log.
* DATA
DATA: i_vbrk            TYPE STANDARD TABLE OF vbrk,
      i_t9fbm005        TYPE STANDARD TABLE OF t_t9fbm005,
      i_vbrp            TYPE STANDARD TABLE OF vbrp,
      x_vbrp            TYPE vbrp,
      x_t9fbm005        TYPE t_t9fbm005,
      x_vbrk            TYPE vbrk,
      i_zt9control      TYPE TABLE OF zt9control,
      x_zt9control      TYPE zt9control,
      i_icompdate_upd   TYPE STANDARD TABLE OF t_icompdate_upd,
      x_icompdate_upd   TYPE t_icompdate_upd,
      i_bkpf2           TYPE STANDARD TABLE OF t_bkpf,
      x_bkpf2           TYPE t_bkpf,
      i_zmatbilling     TYPE STANDARD TABLE OF t_zmatrix_billing,
      i_zmatbillingexst TYPE STANDARD TABLE OF t_zmatrix_billing,
      i_zmatbilling_aux TYPE STANDARD TABLE OF t_zmatrix_billing_aux,
      x_zmatbillingexst TYPE t_zmatrix_billing,
      x_zmatbilling_aux TYPE t_zmatrix_billing_aux,
      x_zmatbilling     TYPE t_zmatrix_billing,
      x_control         TYPE zt9control,
      i_zftqueue        TYPE STANDARD TABLE OF zftmatrixqueue,
      i_zftdelete       TYPE STANDARD TABLE OF zftmatrixqueue,
      i_zftdelete_temp  TYPE STANDARD TABLE OF zftmatrixqueue,
      x_zftdelete       TYPE zftmatrixqueue,
      i_zftexisting     TYPE STANDARD TABLE OF zftmatrixqueue,
      x_zftexisting     TYPE zftmatrixqueue,
      x_zftqueue        TYPE zftmatrixqueue,
      v_type            TYPE t9fbm005-type,
      v_type1           TYPE t9fbm005-type,
      v_type2           TYPE t9fbm005-type,
      v_tabix           TYPE sy-tabix,
      v_datum           TYPE sy-datum,
      "date and time of the system whe you
      v_uzeit           TYPE sy-uzeit,
      i_company         TYPE STANDARD TABLE OF t_company,
      x_company         TYPE t_company,
      i_bkpf            TYPE STANDARD TABLE OF t_bkpf2,
      x_bkpf            TYPE t_bkpf2.

CONSTANTS: c_vbrk(4)    TYPE c  VALUE 'VBRK',
           c_x          TYPE c VALUE 'X',
           c_sta        TYPE c VALUE 'S',
           c_int(3)     TYPE c VALUE 'INT',
           c_err        TYPE c VALUE 'E',
           c_matrix     TYPE sy-repid
                        VALUE 'ZFRTRR047/ZFRTRI011/ZFRTRR008',
           c_fkartp      TYPE t9fbm002-tlevel VALUE 'FKARTPASSB',
           c_a2          TYPE t9fbm002-data_type VALUE 'A2'.

* RANGES
DATA:   r_blart TYPE RANGE OF bkpf-blart,
        r_fkart TYPE RANGE OF vbrk-fkart.
DATA:   i_message      TYPE STANDARD TABLE OF t_solisti1,
        x_message      TYPE t_solisti1,
        i_packing_list TYPE STANDARD TABLE OF t_sopcklsti1,
        x_packing_list TYPE t_sopcklsti1,
        i_receivers    TYPE STANDARD TABLE OF t_somlreci1,
        x_receivers    TYPE t_somlreci1,
        v_sent_all(1)  TYPE c,
        x_doc_data     TYPE sodocchgi1,
        v_lines        TYPE i.

*********************************************************************
* SELECTION SCREEN
*********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN EVENTS    - validate user input                  *
*----------------------------------------------------------------------*
*insert the default values for the select option s_blart: 'RV' 'RR' DP'
AT SELECTION-SCREEN OUTPUT.

  PERFORM f_insert_default_t9con.

AT SELECTION-SCREEN ON s_bukrs.
  CLEAR: x_company.
  REFRESH: i_company.
  SELECT bukrs
  FROM t001
  INTO TABLE i_company
  WHERE bukrs IN s_bukrs.

  IF sy-subrc EQ 0.
    IF i_company[] IS NOT INITIAL.
      REFRESH: i_t9fbm005.
      SELECT bukrs
             type
      FROM t9fbm005
      INTO TABLE i_t9fbm005
      FOR ALL ENTRIES IN i_company
      WHERE bukrs EQ i_company-bukrs.
      IF sy-subrc EQ 0.
        SORT i_t9fbm005 BY bukrs.
      ENDIF.
    ENDIF.
    LOOP AT i_company ASSIGNING <fs_company2>.
      v_tabix = sy-tabix.
      CLEAR: v_type1,
             x_t9fbm005.
      READ TABLE i_t9fbm005
      INTO x_t9fbm005
      WITH KEY bukrs = <fs_company2>-bukrs
      BINARY SEARCH.
      IF sy-subrc NE 0.
        MESSAGE e013 WITH <fs_company2>-bukrs.
      ELSE.
        MOVE: x_t9fbm005-bukrs TO <fs_company2>-bukrs,
              x_t9fbm005-type TO v_type1.
        IF v_tabix NE 1.
          IF v_type1 NE v_type2.
            MESSAGE e014.
          ENDIF.
        ENDIF.
        v_type2 = v_type1.
      ENDIF.
    ENDLOOP.
  ENDIF.
*********************************************************************
*INITIALIZATION
*********************************************************************
INITIALIZATION.

  PERFORM f_initialization.

*********************************************************************
*Begin of Process
*********************************************************************
START-OF-SELECTION.
  PERFORM f_check_selection_entry.
  CLEAR: x_company.
  READ TABLE i_company INTO x_company INDEX 1.
  PERFORM f_get_cc_type USING x_company-bukrs
                        CHANGING v_type.
  PERFORM f_get_last_execution.
  PERFORM f_get_fi_docs.
  PERFORM f_get_billing_values.
  PERFORM f_save_billing_docs.

END-OF-SELECTION.

*update the table t9control
  PERFORM f_update_t9control.
  IF sy-batch EQ c_x.
    PERFORM f_email_message_body.
    CLEAR v_lines.
    DESCRIBE TABLE i_message LINES v_lines.
    IF v_lines GT 1.
      PERFORM f_send_email.
      CLEAR v_lines.
    ENDIF.
  ENDIF.

  PERFORM f_delete_flag.
*********************************************************************
*End of Process
*********************************************************************

*&---------------------------------------------------------------------*
*&      Form  f_save_billing_docs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_save_billing_docs.
  CONSTANTS: c_lcmplt       TYPE c VALUE 'C',
*Begin of Insert BORJAAD 12/12/2011 GD2K925964 ID4480
             c_lzzy3o       TYPE kschl VALUE 'Y3O',
             c_lzzy3b       TYPE kschl VALUE 'Y3B',
             c_lzzynb       TYPE kschl VALUE 'YNB',
             c_lyob         TYPE kschl VALUE 'YOB',
             c_lyor         TYPE kschl VALUE 'YOR',
             c_lyoc         TYPE kschl VALUE 'YOC',
             c_lzzyod       TYPE kschl VALUE 'YOD',
             c_lybr         TYPE kschl VALUE 'YBR',
             c_lybc         TYPE kschl VALUE 'YBC',
             c_lzzybd       TYPE kschl VALUE 'YBD',
             c_lyrc         TYPE kschl VALUE 'YRC',
             c_lzzyrd       TYPE kschl VALUE 'YRD',
             c_lindicator   TYPE c VALUE 'X'.
*End of Insert BORJAAD 12/12/2011 GD2K925964 ID4480
  DATA: v_ltabix TYPE i,
        x_ldet    TYPE zfs_bm_scenario_det.
  CLEAR: v_ltabix,
         x_ldet.
  REFRESH: i_zftexisting.
  SELECT *
  FROM zftmatrixqueue
  INTO TABLE i_zftexisting.
  IF sy-subrc EQ 0.
    SORT i_zftexisting BY bukrs belnr vbeln.
    DELETE ADJACENT DUPLICATES FROM i_zftexisting
                          COMPARING bukrs
                                    belnr
                                    vbeln.
  ENDIF.

  CLEAR: x_vbrp,
         x_ldet,
         x_zftqueue.
  LOOP AT i_vbrp INTO x_vbrp.
    READ TABLE i_zftqueue
    WITH KEY vbeln = x_vbrp-vbeln
             TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      CONTINUE.
    ELSE.
      CLEAR: x_ldet.
      MOVE-CORRESPONDING x_vbrp TO x_ldet.
      IF x_ldet IS NOT INITIAL.
        CLEAR: x_vbrk.
        READ TABLE i_vbrk INTO x_vbrk WITH KEY vbeln = x_vbrp-vbeln.
        IF sy-subrc EQ 0.
          CLEAR: x_bkpf.
          READ TABLE i_bkpf INTO x_bkpf WITH KEY awkey = x_vbrp-vbeln.
          IF sy-subrc EQ 0.
            MOVE:  x_vbrk-bukrs TO x_zftqueue-bukrs,
                   x_vbrk-vbeln TO x_zftqueue-vbeln,
                   x_bkpf-belnr TO x_zftqueue-belnr,
                   x_bkpf-gjahr TO x_zftqueue-gjahr.
*Begin of Insert BORJAAD 12/12/2011 GD2K925964 ID4480
            CASE c_lindicator.
              WHEN x_ldet-zzy3o.
                MOVE: c_lzzy3o TO x_zftqueue-kschl.
              WHEN x_ldet-zzy3b.
                MOVE: c_lzzy3b TO x_zftqueue-kschl.
              WHEN x_ldet-zzynb.
                MOVE: c_lzzynb TO x_zftqueue-kschl.
              WHEN x_ldet-yob.
                MOVE: c_lyob   TO x_zftqueue-kschl.
              WHEN x_ldet-yor.
                MOVE: c_lyor   TO x_zftqueue-kschl.
              WHEN x_ldet-yoc.
                MOVE: c_lyoc   TO x_zftqueue-kschl.
              WHEN x_ldet-zzyod.
                MOVE: c_lzzyod TO x_zftqueue-kschl.
              WHEN x_ldet-ybr.
                MOVE: c_lybr   TO x_zftqueue-kschl.
              WHEN x_ldet-ybc.
                MOVE: c_lybc   TO x_zftqueue-kschl.
              WHEN x_ldet-zzybd.
                MOVE: c_lzzybd TO x_zftqueue-kschl.
              WHEN x_ldet-yrc.
                MOVE: c_lyrc   TO x_zftqueue-kschl.
              WHEN x_ldet-zzyrd.
                MOVE: c_lzzyrd TO x_zftqueue-kschl.
              WHEN OTHERS.
                "Do nothing
            ENDCASE.
*End of Insert BORJAAD 12/12/2011 GD2K925964 ID4480
            APPEND x_zftqueue TO i_zftqueue.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: x_vbrp,
           x_ldet.
  ENDLOOP.

  REFRESH: i_zmatbilling.
  SELECT vbeln
         status
         datum
         bukrs
  FROM zmatrix_billing
  INTO TABLE i_zmatbilling
  WHERE status EQ space
    AND bukrs IN s_bukrs.
  IF sy-subrc EQ 0.
    SORT i_zmatbilling BY vbeln.
  ENDIF.

  REFRESH: i_zmatbillingexst.
  IF i_zftexisting[] IS NOT INITIAL.
    SELECT vbeln
           status
           datum
           bukrs
    FROM zmatrix_billing
    INTO TABLE i_zmatbillingexst
    FOR ALL ENTRIES IN i_zftexisting
    WHERE vbeln  EQ i_zftexisting-vbeln
      AND status EQ c_lcmplt.
    IF sy-subrc EQ 0.
      SORT i_zmatbillingexst BY vbeln.
    ENDIF.
*Begin of Change BORJAAD 12/12/2011 GD2K925964 ID4480
*  ELSEIF i_zftqueue[] IS NOT INITIAL.
  ENDIF.
  IF i_zftqueue[] IS NOT INITIAL.
*End of Change BORJAAD 12/12/2011 GD2K925964 ID4480
    SELECT vbeln
           status
           datum
           bukrs
    FROM zmatrix_billing
*Begin of Change BORJAAD 12/12/2011 GD2K925964 ID4480
*    INTO TABLE i_zmatbillingexst
    APPENDING TABLE i_zmatbillingexst
*End of Change BORJAAD 12/12/2011 GD2K925964 ID4480
    FOR ALL ENTRIES IN i_zftqueue
    WHERE vbeln  EQ i_zftqueue-vbeln
      AND status EQ c_lcmplt.
    IF sy-subrc EQ 0.
      SORT i_zmatbillingexst BY vbeln.
    ENDIF.
  ENDIF.
*Begin of Insert BORJAAD 12/12/2011 GD2K925964 ID4480
  IF i_zmatbillingexst[] IS NOT INITIAL.
    SORT i_zmatbillingexst BY vbeln
                              status
                              datum
                              bukrs.
    DELETE ADJACENT DUPLICATES FROM i_zmatbillingexst COMPARING vbeln
                                                                status
                                                                datum
                                                                bukrs.
    SORT i_zmatbillingexst BY vbeln.
  ENDIF.
*End of Insert BORJAAD 12/12/2011 GD2K925964 ID4480

  IF i_zmatbilling[] IS NOT INITIAL.
    CLEAR: x_zmatbilling,
           x_zmatbilling_aux.
    REFRESH: i_zmatbilling_aux.
    LOOP AT i_zmatbilling INTO x_zmatbilling.
      MOVE: x_zmatbilling-vbeln  TO x_zmatbilling_aux-vbeln,
            x_zmatbilling-status TO x_zmatbilling_aux-status,
            x_zmatbilling-datum  TO x_zmatbilling_aux-datum,
            x_zmatbilling-bukrs  TO x_zmatbilling_aux-bukrs.
      APPEND x_zmatbilling_aux   TO i_zmatbilling_aux.
      CLEAR: x_zmatbilling,
             x_zmatbilling_aux.
    ENDLOOP.
    IF i_zmatbilling_aux[] IS NOT INITIAL.
      REFRESH: i_bkpf2.
      SELECT bukrs
             belnr
             gjahr
             budat
             awkey
      FROM bkpf
      INTO TABLE i_bkpf2
      FOR ALL ENTRIES IN i_zmatbilling_aux
      WHERE awkey EQ i_zmatbilling_aux-vbeln.
      IF sy-subrc EQ 0.
        SORT i_bkpf2 BY awkey.
        CLEAR: x_zmatbilling,
               x_zftqueue.
        LOOP AT i_zmatbilling INTO x_zmatbilling.
          READ TABLE i_zftqueue
          WITH KEY vbeln = x_zmatbilling-vbeln
          TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            CONTINUE.
          ELSE.
            CLEAR: x_bkpf2.
            READ TABLE i_bkpf2
            INTO x_bkpf2
            WITH KEY awkey = x_zmatbilling-vbeln
            BINARY SEARCH.
            IF sy-subrc EQ 0.
              CLEAR: x_zftqueue.
              MOVE: x_bkpf2-gjahr       TO x_zftqueue-gjahr,
                    x_bkpf2-belnr       TO x_zftqueue-belnr,
                    x_zmatbilling-bukrs TO x_zftqueue-bukrs,
                    x_zmatbilling-vbeln TO x_zftqueue-vbeln.
              APPEND x_zftqueue TO i_zftqueue.
            ENDIF.
          ENDIF.
          CLEAR: x_zftqueue,
                 x_zmatbilling.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: x_zftqueue,
         v_ltabix.
  SORT i_zftqueue BY bukrs belnr vbeln.
  DELETE ADJACENT DUPLICATES FROM i_zftqueue COMPARING bukrs
                                                       belnr
                                                       vbeln.
  IF i_zftexisting[] IS NOT INITIAL.
    LOOP AT i_zftqueue INTO x_zftqueue.
      CLEAR: v_ltabix.
      v_ltabix = sy-tabix.
      READ TABLE i_zftexisting WITH KEY bukrs = x_zftqueue-bukrs
                                        belnr = x_zftqueue-belnr
                                        vbeln = x_zftqueue-vbeln
                                        TRANSPORTING NO FIELDS
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE i_zftqueue INDEX v_ltabix.
        IF sy-subrc EQ 0.
          "do nothing
        ENDIF.
      ENDIF.
      CLEAR: x_zftqueue,
             v_ltabix.
    ENDLOOP.
  ENDIF.
  IF i_zmatbillingexst[] IS NOT INITIAL.
    CLEAR: x_zmatbillingexst.
    REFRESH: i_zftdelete.
    SORT i_zftexisting BY vbeln.
    SORT i_zftqueue BY vbeln.
    LOOP AT i_zmatbillingexst INTO x_zmatbillingexst.
      CLEAR: x_zftexisting.
      READ TABLE i_zftexisting
      INTO x_zftexisting
      WITH KEY vbeln = x_zmatbillingexst-vbeln
               BINARY SEARCH.
      IF sy-subrc EQ 0.
        APPEND x_zftexisting TO i_zftdelete.
      ELSE.
        CLEAR: x_zftqueue.
        READ TABLE i_zftqueue
        INTO x_zftqueue
        WITH KEY vbeln = x_zmatbillingexst-vbeln
                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          APPEND x_zftqueue TO i_zftdelete.
        ENDIF.
      ENDIF.
      CLEAR: x_zmatbillingexst,
             x_zftexisting.
    ENDLOOP.
    IF i_zftdelete[] IS NOT INITIAL.
      SORT i_zftdelete BY bukrs belnr vbeln.
      REFRESH: i_zftdelete_temp.
      SELECT *
      INTO TABLE i_zftdelete_temp
      FROM zftmatrixqueue
      FOR ALL ENTRIES IN i_zftdelete
      WHERE bukrs EQ i_zftdelete-bukrs
        AND belnr EQ i_zftdelete-belnr
        AND vbeln EQ i_zftdelete-vbeln.
      IF sy-subrc EQ 0 AND
        i_zftdelete_temp[] IS NOT INITIAL.
        DELETE zftmatrixqueue FROM TABLE i_zftdelete_temp.
        COMMIT WORK.
        IF sy-subrc EQ 0.
          "Do nothing
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF i_zftdelete_temp[] IS NOT INITIAL.
    CLEAR: x_zftdelete.
    LOOP AT i_zftdelete_temp INTO x_zftdelete.
      DELETE i_zftqueue WHERE vbeln EQ x_zftdelete-vbeln.
      IF sy-subrc EQ 0.
        "Do nothing
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF i_zftdelete[] IS NOT INITIAL.
    CLEAR: x_zftdelete.
    LOOP AT i_zftdelete INTO x_zftdelete.
      DELETE i_zftqueue WHERE vbeln EQ x_zftdelete-vbeln.
      IF sy-subrc EQ 0.
        "Do nothing
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF i_zftqueue[] IS NOT INITIAL.
    SORT i_zftqueue BY bukrs belnr vbeln.
    MODIFY zftmatrixqueue FROM TABLE i_zftqueue.
    IF sy-subrc EQ 0.
      "Do nothing
    ENDIF.
  ENDIF.

ENDFORM.                    "f_save_billing_docs

*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
*       Initialize all the global data
*----------------------------------------------------------------------*
FORM f_initialization.

  CLEAR: x_vbrk.

  REFRESH: i_vbrk.

* Get the Billing Types not to be treated
  CALL FUNCTION 'Z_IC_BRAND_GET_FIXED_VALUES'
    EXPORTING
      i_repid             = c_matrix
      i_type              = c_a2
      i_level             = c_fkartp
    TABLES
      t_range             = r_fkart
    EXCEPTIONS
      data_type_not_found = 1
      no_data_found       = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " f_initialization

*&---------------------------------------------------------------------*
*&      Form  f_get_billing_values
*&---------------------------------------------------------------------*
*       Get the sales invoice data to create the file
*----------------------------------------------------------------------*
FORM f_get_billing_values.
  SORT: i_bkpf BY awkey.
  IF i_bkpf[] IS NOT INITIAL.
    REFRESH: i_vbrk,
             i_vbrp.
    SELECT * INTO TABLE i_vbrk
             FROM vbrk
             FOR ALL ENTRIES IN i_bkpf
             WHERE vbeln EQ i_bkpf-awkey AND
                   fkart NOT IN r_fkart.
    IF sy-subrc EQ 0.
      SORT i_vbrk BY vbeln.
    ENDIF.
    IF i_vbrk[] IS NOT INITIAL.
      REFRESH: i_vbrp.
      SELECT *
      INTO TABLE i_vbrp
      FROM vbrp
      FOR ALL ENTRIES IN i_vbrk
      WHERE vbeln EQ i_vbrk-vbeln.
      IF sy-subrc EQ 0.
        SORT i_vbrp BY vbeln posnr.
      ENDIF.
    ENDIF.
  ENDIF.

  v_datum = sy-datum.
  v_uzeit = sy-uzeit.
ENDFORM.                    " f_get_billing_values

*&---------------------------------------------------------------------*
*&      Form  f_get_fi_docs
*&---------------------------------------------------------------------*
*       Get the Fi Documents
*----------------------------------------------------------------------*
FORM f_get_fi_docs.

  IF i_company[] IS NOT INITIAL.
    REFRESH: i_bkpf.
    SELECT bukrs
           belnr
           gjahr
           xblnr
           awkey
    FROM bkpf
    INTO TABLE i_bkpf
    FOR ALL ENTRIES IN i_company
    WHERE bukrs EQ i_company-bukrs
      AND blart IN r_blart
      AND    ( cpudt GT i_company-date
      OR     ( cpudt EQ i_company-date
      AND     cputm  GT i_company-time ) )
      AND     awtyp EQ c_vbrk.
    IF sy-subrc EQ 0.
      SORT i_bkpf BY awkey.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_get_fi_docs
*&---------------------------------------------------------------------*
*&      Form  INSERT_DEFAULT_T9CON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_insert_default_t9con.
  CONSTANTS: c_level TYPE t9fbm002-tlevel VALUE '01',
             c_type TYPE t9fbm002-data_type VALUE 'A3'.

  REFRESH: r_blart.

  CALL FUNCTION 'Z_IC_BRAND_GET_FIXED_VALUES'
    EXPORTING
      i_repid             = 'ZFRTRI011'
      i_type              = c_type
      i_level             = c_level
    TABLES
      t_range             = r_blart
    EXCEPTIONS
      data_type_not_found = 1
      no_data_found       = 2
      OTHERS              = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_INSERT_DEFAULT_T9CON
*&---------------------------------------------------------------------*
*&      Form  f_get_last_execution
*&---------------------------------------------------------------------*
*       Get the last execution of the job for this company code
*----------------------------------------------------------------------*
FORM f_get_last_execution .

* START OF INSERT AGCOPRJE 01/24/2012 INC889395 GD2K926005
  CONSTANTS: c_l005959 TYPE sy-uzeit VALUE '005959',
             c_l230000 TYPE sy-uzeit VALUE '230000',
             c_l1 TYPE c VALUE '1',
             c_l10000 TYPE sy-uzeit VALUE '010000'.
* END OF INSERT AGCOPRJE 01/24/2012 INC889395 GD2K926005

*-We obtain the last date in which the program was executed
  IF i_company[] IS NOT INITIAL.
    REFRESH: i_icompdate_upd.
    SELECT zbukrs
           zdatex
           ztimex
    FROM t9control
    INTO TABLE i_icompdate_upd
    FOR ALL ENTRIES IN i_company
    WHERE zbukrs EQ i_company-bukrs
      AND zrepid EQ sy-repid
      AND zhkont EQ abap_false.
    IF sy-subrc EQ 0.
      SORT i_icompdate_upd BY zbukrs.
      LOOP AT i_company ASSIGNING <fs_company>.
        CLEAR: x_icompdate_upd.
        READ TABLE i_icompdate_upd
        INTO x_icompdate_upd
        WITH KEY zbukrs = <fs_company>-bukrs
                 BINARY SEARCH.
        IF sy-subrc EQ 0.
* START OF CHANGE AGCOPRJE 01/24/2012 INC889395 GD2K926005
* Will subtract 1 hour (10000) on the last execution time to get accurate
* accounting doc. If it happens that the last execution time is less than or
* equal to 00:59:59, The program will subtract 1 day and time will be
* 23:00:00 plus the calculated minutes of last execution time.
*          <fs_company>-date = x_icompdate_upd-zdatex.
*          <fs_company>-time = x_icompdate_upd-ztimex.
          IF x_icompdate_upd-ztimex <= c_l005959.
            <fs_company>-date = ABS( x_icompdate_upd-zdatex - c_l1 ).
            <fs_company>-time = ABS( c_l230000 + x_icompdate_upd-ztimex ).
          ELSE.
            <fs_company>-date = x_icompdate_upd-zdatex.
            <fs_company>-time = ABS( x_icompdate_upd-ztimex - c_l10000 ).
          ENDIF.
* END OF CHANGE AGCOPRJE 01/24/2012 INC889395 GD2K926005
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " f_get_last_execution
*&---------------------------------------------------------------------*
*&      Form  UPDATE_T9CONTROL
*&---------------------------------------------------------------------*
*       update the table t9control with the actual data
*----------------------------------------------------------------------*
FORM f_update_t9control.
  CLEAR: x_company.
  LOOP AT i_company INTO x_company.
    t9control-zbukrs = x_company-bukrs.
    t9control-zrepid = sy-repid.
    t9control-zdatex = v_datum.
    t9control-ztimex = v_uzeit.
    t9control-zusname = sy-uname.
    t9control-zdatold = x_company-date.
    t9control-ztimold = x_company-time.
    MODIFY t9control.
    CLEAR: x_company.
  ENDLOOP.
ENDFORM.                    " F_UPDATE_T9CONTROL

*&---------------------------------------------------------------------*
*&      Form  f_get_cc_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_BUKRS  text
*      <--P_V_TYPE  text
*----------------------------------------------------------------------*
FORM f_get_cc_type  USING    pi_bukrs
                    CHANGING po_type.

  SELECT SINGLE type INTO po_type
                     FROM t9fbm005
                    WHERE bukrs EQ pi_bukrs.
  IF sy-subrc EQ 0.
    "Do nothing
  ENDIF.
ENDFORM.                    " f_get_cc_type

*&---------------------------------------------------------------------*
*&      Form  f_check_selection_entry
*&---------------------------------------------------------------------*
* Check input in the selection screen to avoid parallel runs.
*----------------------------------------------------------------------*
FORM f_check_selection_entry.

  REFRESH: i_zt9control.

  CLEAR: x_zt9control,
         x_control.

  PERFORM f_check_blank_zbukrs.

  SORT i_company BY bukrs.

  IF i_company[] IS NOT INITIAL.
    SELECT *
    INTO TABLE i_zt9control
    FROM zt9control
    FOR ALL ENTRIES IN i_company
    WHERE zbukrs EQ i_company-bukrs
      AND zrepid EQ sy-repid
      AND zhkont EQ space.

    IF sy-subrc IS INITIAL.
      SORT i_zt9control BY zbukrs
                           zrepid
                           zhkont.
      DELETE ADJACENT DUPLICATES FROM i_zt9control COMPARING ALL FIELDS.
      CLEAR: x_company.
      LOOP AT i_company INTO x_company.
        CLEAR: x_control,
               x_zt9control.

        x_control-zbukrs  = x_company-bukrs.
        x_control-zrepid  = sy-repid.
        x_control-zhkont  = space.
        x_control-zdatex  = sy-datum.
        x_control-ztimex  = sy-uzeit.
        x_control-zusname = sy-uname.
        x_control-zflag   = c_x.
        CLEAR: x_zt9control.
        READ TABLE i_zt9control INTO x_zt9control
        WITH KEY zbukrs = x_control-zbukrs
                 zrepid = sy-repid
                 zhkont = space
        BINARY SEARCH.

        IF sy-subrc IS INITIAL.
          IF x_zt9control-zflag EQ c_x.
            MESSAGE text-e01 TYPE c_sta DISPLAY LIKE c_err.
            IF sy-batch IS NOT INITIAL.
              CLEAR: x_log.
              REFRESH: i_log.
              x_log-type = c_err.
              x_log-message = text-e01.

              APPEND x_log TO i_log.
              CLEAR: x_log.
              PERFORM f_email_message_body.
              CLEAR v_lines.
              DESCRIBE TABLE i_message LINES v_lines.
              IF v_lines GT 1.
                PERFORM f_send_email.
                CLEAR v_lines.
              ENDIF.
            ENDIF.
            LEAVE LIST-PROCESSING.
          ELSE.
            MODIFY zt9control FROM x_control.
            IF sy-subrc IS INITIAL.
              "Do nothing.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: x_company.
      ENDLOOP.

      COMMIT WORK AND WAIT.
    ELSE.
      CLEAR: x_company.
      LOOP AT i_company INTO x_company.
        CLEAR: x_control.

        x_control-zbukrs  = x_company-bukrs.
        x_control-zrepid  = sy-repid.
        x_control-zhkont  = space.
        x_control-zdatex  = sy-datum.
        x_control-ztimex  = sy-uzeit.
        x_control-zusname = sy-uname.
        x_control-zflag   = c_x.

        INSERT into zt9control values x_control.
        IF sy-subrc IS INITIAL.
          "Do nothing.
        ENDIF.
        CLEAR: x_company.
      ENDLOOP.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_check_selection_entry

*&---------------------------------------------------------------------*
*&      Form f_delete_flag
*&---------------------------------------------------------------------*
* Delete flag from table ZT9CONTROL.
*----------------------------------------------------------------------*
FORM f_delete_flag.

  REFRESH: i_zt9control.

  CLEAR: x_zt9control.

  IF i_company[] IS NOT INITIAL.
    SELECT *
    INTO TABLE i_zt9control
    FROM zt9control
    FOR ALL ENTRIES IN i_company
    WHERE zbukrs EQ i_company-bukrs
      AND zrepid EQ sy-repid
      AND zhkont EQ space
      AND zflag  EQ c_x.

    IF sy-subrc IS INITIAL.
      CLEAR: x_zt9control.
      LOOP AT i_zt9control INTO x_zt9control.
        x_zt9control-zflag = space.
        MODIFY zt9control FROM x_zt9control.
        IF sy-subrc IS INITIAL.
          "Do nothing.
        ENDIF.

        CLEAR: x_zt9control.
      ENDLOOP.

      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

* Company Code parameter is blank when run.
  IF s_bukrs-low IS INITIAL.
    CLEAR: x_zt9control.

    SELECT SINGLE *
    INTO x_zt9control
    FROM zt9control
    WHERE zbukrs EQ space
      AND zrepid EQ sy-repid
      AND zhkont EQ space
      AND zflag  EQ c_x.
*   Remove the status flag X.
    IF sy-subrc IS INITIAL.
      x_zt9control-zflag = space.
      MODIFY zt9control FROM x_zt9control.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR: x_zt9control.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_delete_flag

*&---------------------------------------------------------------------*
*&      Form  f_check_blank_zbukrs
*&---------------------------------------------------------------------*
* If the program is executed without a Company Code, and the next run is
* made using a Company Code, the error message should also be triggered.
*----------------------------------------------------------------------*
FORM f_check_blank_zbukrs.

  CLEAR: x_zt9control,
         x_control.

  SELECT SINGLE *
  INTO x_zt9control
  FROM zt9control
  WHERE zbukrs EQ space
    AND zrepid EQ sy-repid
    AND zhkont EQ space.
* An entry already existing in the table.
  IF sy-subrc IS INITIAL.
    x_control-zbukrs  = space.
    x_control-zrepid  = sy-repid.
    x_control-zhkont  = space.
    x_control-zdatex  = sy-datum.
    x_control-ztimex  = sy-uzeit.
    x_control-zusname = sy-uname.
    x_control-zflag   = c_x.
*   Status was flag X.
    IF x_zt9control-zflag EQ c_x.
      MESSAGE text-e01 TYPE c_sta DISPLAY LIKE c_err.

      IF sy-batch IS NOT INITIAL.
        CLEAR x_log.
        REFRESH i_log.
        x_log-type = c_err.
        x_log-message = text-e01.
        APPEND x_log TO i_log.
        PERFORM f_email_message_body.
        CLEAR v_lines.
        DESCRIBE TABLE i_message LINES v_lines.
        IF v_lines GT 1.
          PERFORM f_send_email.
          CLEAR v_lines.
        ENDIF.
      ENDIF.

      LEAVE LIST-PROCESSING.
*   Company Code parameter is blank when run.
    ELSEIF s_bukrs-low IS INITIAL.
      MODIFY zt9control FROM x_control.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
* No entry exist yet.
  ELSE.
*   Company Code parameter is blank when run.
    IF s_bukrs-low IS INITIAL.
      x_control-zbukrs  = space.
      x_control-zrepid  = sy-repid.
      x_control-zhkont  = space.
      x_control-zdatex  = sy-datum.
      x_control-ztimex  = sy-uzeit.
      x_control-zusname = sy-uname.
      x_control-zflag   = c_x.

      INSERT into zt9control values x_control.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "f_check_blank_zbukrs

*&---------------------------------------------------------------------*
*&      Form  f_email_message_body
*&---------------------------------------------------------------------*
*       Subroutine for creating message body
*          -> this subroutine fills up the table for the message body,
*             this was filled by getting the error messages of the
*             background job, Billing document number, item number,
*             company code, document type,& group name
*----------------------------------------------------------------------*
FORM f_email_message_body.

  CONSTANTS: c_lcomma(1) TYPE c VALUE ','.

  REFRESH: i_message.
  x_message = text-003.
  APPEND x_message TO i_message.
  CLEAR: x_message,
         x_log.

  LOOP AT i_log INTO x_log WHERE type EQ c_err.

    CONCATENATE x_log-vbeln
                x_log-posnr
                x_log-bukrs
                x_log-blart
                x_log-group
                x_log-message
         INTO x_message SEPARATED BY c_lcomma.

    APPEND x_message TO i_message.
    CLEAR: x_message,
           x_log.
  ENDLOOP.

ENDFORM.                    "F_EMAIL_MESSAGE_BODY

*&---------------------------------------------------------------------*
*&      Form  f_send_email.
*&---------------------------------------------------------------------*
*   creates the email: sender, reciever, and message body
*----------------------------------------------------------------------*
FORM f_send_email.

  DATA: v_lmtitle   TYPE sodocchgi1-obj_descr.
  CONSTANTS: c_lperiod TYPE c VALUE '.',
             c_lf      TYPE c VALUE 'F',
             c_lsaprpt TYPE string VALUE 'SAPRPT',
             c_lraw    TYPE string VALUE 'RAW'.

  CLEAR: v_lmtitle.

* Email Subject.
  CONCATENATE text-004 s_bukrs-low INTO v_lmtitle SEPARATED BY space.
  CONCATENATE v_lmtitle c_lperiod INTO v_lmtitle.
* Fill the document data.
  x_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  x_doc_data-obj_langu = sy-langu.
  x_doc_data-obj_name  = c_lsaprpt.
  x_doc_data-obj_descr =  v_lmtitle.
  x_doc_data-sensitivty = c_lf.


* Describe the body of the message
  CLEAR i_packing_list.
  REFRESH i_packing_list.
  x_packing_list-transf_bin = space.
  x_packing_list-head_start = 1.
  x_packing_list-head_num = 0.
  x_packing_list-body_start = 1.

  DESCRIBE TABLE i_message LINES x_packing_list-body_num.

  x_packing_list-doc_type = c_lraw.
  APPEND x_packing_list TO i_packing_list.
  CLEAR x_packing_list.

* Add the recipients email address
  PERFORM f_get_receivers.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = x_doc_data
      put_in_outbox              = c_x
      commit_work                = c_x
    IMPORTING
      sent_to_all                = v_sent_all
    TABLES
      packing_list               = i_packing_list
      contents_txt               = i_message
      receivers                  = i_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc EQ 0.
    PERFORM f_mail_execute_program.
  ENDIF.


ENDFORM.                    "F_SEND_EMAIL

*&---------------------------------------------------------------------*
*&      Form  f_get_receivers
*&---------------------------------------------------------------------*
*retrieves the contact details of those who
*will recieve the error email notification
*----------------------------------------------------------------------*
FORM f_get_receivers.

  TYPES: BEGIN OF t_luser,
            agr_name TYPE agr_users-agr_name,
            uname    TYPE agr_users-uname,
         END   OF t_luser,

         BEGIN OF t_lusr21,
            bname TYPE usr21-bname,
            persnumber  TYPE usr21-persnumber,
         END OF t_lusr21,

        BEGIN OF t_ladr6,
            persnumber TYPE adr6-persnumber,
            smtp_addr  TYPE adr6-smtp_addr,
        END OF t_ladr6.

  DATA: i_luser  TYPE STANDARD TABLE OF t_luser,
        i_lusr21 TYPE STANDARD TABLE OF t_lusr21,
        i_ladr6  TYPE STANDARD TABLE OF t_ladr6,
        x_ladr6  TYPE t_ladr6,
        v_lemail TYPE  somlreci1-receiver.

  CONSTANTS: c_lcontroller TYPE string
                VALUE 'RTR_BTRM_CONTROLLER',
             c_lu(1) TYPE c VALUE 'U'.


  CLEAR: x_ladr6,
         i_receivers,
         v_lemail.

  REFRESH: i_luser,
           i_lusr21,
           i_ladr6,
           i_receivers.

  SELECT agr_name
         uname
          FROM agr_users
          INTO TABLE i_luser
          WHERE agr_name EQ c_lcontroller.

  IF sy-subrc EQ 0
    AND i_luser IS NOT INITIAL.
    SELECT bname
           persnumber
           FROM usr21
           INTO TABLE i_lusr21
               FOR ALL ENTRIES IN i_luser
               WHERE bname EQ i_luser-uname.

    IF sy-subrc EQ 0
      AND i_lusr21 IS NOT INITIAL.
      SELECT persnumber
             smtp_addr
             FROM adr6
             INTO TABLE i_ladr6
                 FOR ALL ENTRIES IN i_lusr21
                 WHERE persnumber EQ i_lusr21-persnumber.
    ENDIF.
  ENDIF.
  CLEAR: x_ladr6.
  LOOP AT i_ladr6 INTO x_ladr6.

    CLEAR v_lemail.

    v_lemail = x_ladr6-smtp_addr.
    x_receivers-receiver = v_lemail.
    x_receivers-rec_type = c_lu.
    x_receivers-com_type = c_int.
    x_receivers-notif_del = c_x.
    x_receivers-notif_ndel = c_x.

    APPEND x_receivers TO i_receivers.
    CLEAR: x_receivers,
           x_ladr6.
  ENDLOOP.

ENDFORM.                    "f_get_receivers

*&---------------------------------------------------------------------*
*&      Form  f_mail_execute_program
*&---------------------------------------------------------------------*
* this subroutine executes the sending process,
*if the system is unavailable it will wait for 2 seconds
*----------------------------------------------------------------------*
FORM f_mail_execute_program.

  WAIT UP TO 2 SECONDS.
  SUBMIT rsconn01 WITH mode EQ c_int
                  WITH output EQ c_x
                  AND RETURN.

ENDFORM.                    "F_MAIL_EXECUTE_PROGRAM
