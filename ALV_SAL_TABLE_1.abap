REPORT zbcr017.

*----------------------------------------------------------------------*
* Projeto .......: Programa auxiliar ABAP
* Data desenv ...: 18.08.2017 13:46:46
* Autor .........: Carlos Augusto
* Solicitante ...: Carlos Augusto
* Objetivo ......: Verificar utilização de objetos na request
*
* Transação .....: N/A
*----------------------------------------------------------------------*
TABLES: e070, ctsproject, e071.
TYPE-POOLS: sinfo.
*&---------------------------------------------------------------------*
*                       TYPES
*&---------------------------------------------------------------------*
TYPES:
BEGIN OF ty_saida,
  trkorr    TYPE e071-trkorr,
  as4user   TYPE e070-as4user,
  as4date   TYPE e070-as4date,
  text      TYPE ddtext,
  pgmid     TYPE e071-pgmid,
  object    TYPE e071-object,
  obj_name  TYPE e071-obj_name,
  as4pos    TYPE e071-as4pos,
  obj_text  TYPE rseui_set-obj_text,
  reference TYPE e070a-reference,
  descriptn TYPE ctsproject-descriptn,
  cont      TYPE i,
END OF ty_saida,

BEGIN OF ty_e070,
  trkorr     TYPE e070-trkorr,
  trfunction TYPE e070-trfunction,
  trstatus   TYPE e070-trstatus,
  tarsystem  TYPE e070-tarsystem,
  korrdev    TYPE e070-korrdev,
  as4user    TYPE e070-as4user,
  as4date    TYPE e070-as4date,
  as4time    TYPE e070-as4time,
  strkorr    TYPE e070-strkorr,
  reference  TYPE e070a-reference,
  descriptn  TYPE ctsproject-descriptn,
END OF ty_e070,
tyt_e070 TYPE TABLE OF ty_e070.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_trk_pr FOR ctsproject-trkorr,
                s_trkorr FOR e070-trkorr,
                s_object FOR e071-object.
PARAMETERS: p_obj TYPE xfeld.
SELECTION-SCREEN END OF BLOCK b2.
INCLUDE zialv_layout.

CLASS: lcl_main DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*                       CLASS lcl_main DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS : start.

  PRIVATE SECTION.
    METHODS : get_data,
              display,
              get_obj_text IMPORTING object          TYPE e071-object
                                     obj_name        TYPE e071-obj_name
                           RETURNING value(obj_text) TYPE rseui_set-obj_text,
              process_e071 IMPORTING e070  TYPE ty_e070
                                     tabix TYPE sy-tabix,
              on_link_click FOR EVENT link_click
                            OF cl_salv_events_table
                            IMPORTING row column.

    CLASS-DATA : lr_main  TYPE REF TO lcl_main,
                 lr_table TYPE REF TO cl_salv_table.

    DATA: it_e070  TYPE tyt_e070,
          it_e071  TYPE TABLE OF e071,
          it_ko100 TYPE TABLE OF ko100,
          it_saida TYPE TABLE OF ty_saida.

ENDCLASS.                    "lcl_main DEFINITION
*&---------------------------------------------------------------------*
*       CLASS lcl_main IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD start.

    CREATE OBJECT lr_main.

    lr_main->get_data( ).

    lr_main->display( ).

  ENDMETHOD.                    "start
  METHOD get_data.

    DATA: ls_e070             TYPE ty_e070,
          lv_object_type      TYPE euobj-id,
          lv_object_name      TYPE eu_name,
          lv_enclosing_object TYPE eu_name,
          lt_objectlist       TYPE TABLE OF rseui_set,
          ls_objectlist       TYPE          rseui_set,
          lt_devclass         TYPE TABLE OF sinfo_devclass1,
          gh_parameter        TYPE TABLE OF rsparams,
          ls_parameter        TYPE rsparams,
          lv_sel_all          TYPE c.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = me->it_ko100.

    "(Sistema de transporte: cabeçalho de ordens/tarefas)
    SELECT e070~trkorr   e070~trfunction
           e070~trstatus e070~tarsystem
           e070~korrdev  e070~as4user
           e070~as4date  e070~as4time
           e070~strkorr  e070a~reference
           ctsproject~descriptn
      FROM e070
      INNER JOIN e070a
      ON  e070a~trkorr    EQ e070~trkorr
      AND e070a~attribute EQ 'SAP_CTS_PROJECT'
      LEFT OUTER JOIN ctsproject
      ON e070a~reference EQ ctsproject~trkorr
      INTO TABLE me->it_e070
      WHERE e070~trfunction EQ 'K'
        AND e070~trkorr     IN s_trkorr
        AND e070a~reference IN s_trk_pr.

    IF sy-subrc IS INITIAL.
      "(Sistema de transporte: cabeçalho de ordens/tarefas)
      SELECT *
        FROM e071
        INTO TABLE me->it_e071
        FOR ALL ENTRIES IN me->it_e070
        WHERE trkorr EQ me->it_e070-trkorr
          AND object IN s_object[].

    ENDIF.

    "Ordena para busca otimizada
    SORT: me->it_ko100 BY pgmid object.

    IF p_obj EQ abap_true.
      "Ordena pra eliminar duplicados
      SORT: me->it_e071 BY pgmid object obj_name.
      DELETE ADJACENT DUPLICATES FROM me->it_e071
        COMPARING pgmid object obj_name.

      "Processa todos objetos
      CLEAR ls_e070.
      me->process_e071( e070  = ls_e070
                        tabix = 1 ).
    ELSE.
      "Ordena para busca otimizada
      SORT: me->it_e071 BY trkorr.

      "Processa request
      LOOP AT me->it_e070 INTO ls_e070.
        "Posiciona na request em processamento
        READ TABLE me->it_e071
          WITH KEY trkorr = ls_e070-trkorr
                   BINARY SEARCH
                   TRANSPORTING NO FIELDS.

        "Processa objetos da request em processamento
        me->process_e071( e070  = ls_e070
                          tabix = sy-tabix ).

      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "start

  METHOD display.

    DATA: lr_layout         TYPE REF TO cl_salv_layout,
          lr_functions_list TYPE REF TO cl_salv_functions_list,
          ls_layout_key     TYPE        salv_s_layout_key,
          lr_columns        TYPE REF TO cl_salv_columns_table,
          lr_column         TYPE REF TO cl_salv_column,
          lo_column_table   TYPE REF TO cl_salv_column_table,
          lr_events         TYPE REF TO cl_salv_events_table.

    cl_salv_table=>factory( IMPORTING r_salv_table = me->lr_table
                             CHANGING t_table      = me->it_saida ).

    "Configura layout
    lr_layout = me->lr_table->get_layout( ).
    ls_layout_key-report = sy-repid.
    lr_layout->set_key( ls_layout_key ).
    lr_layout->set_save_restriction(
      cl_salv_layout=>restrict_none ).
    lr_layout->set_initial_layout( p_layout ).

    "Configura funções
    lr_functions_list = me->lr_table->get_functions( ).
    lr_functions_list->set_all( abap_true ).

    lr_columns =  me->lr_table->get_columns( ).
    lr_columns->set_optimize( abap_true ).

    IF p_obj EQ abap_true.
      lr_column ?= lr_columns->get_column( columnname = 'TRKORR' ).
      lr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( columnname = 'AS4USER' ).
      lr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( columnname = 'AS4DATE' ).
      lr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( columnname = 'REFERENCE' ).
      lr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( columnname = 'DESCRIPTN' ).
      lr_column->set_technical( value = if_salv_c_bool_sap=>true ).
    ENDIF.

    lo_column_table ?= lr_columns->get_column( columnname = 'OBJ_NAME' ).
    lo_column_table->set_cell_type( if_salv_c_cell_type=>hotspot ).

    "Eventos
    "CREATE OBJECT me->lr_lcl_handle_events.
    lr_events = me->lr_table->get_event( ).
    "SET HANDLER me->lr_lcl_handle_events->on_link_click FOR lr_events.
    SET HANDLER me->on_link_click FOR lr_events.

    lr_table->display( ).
  ENDMETHOD.                    "start

  METHOD on_link_click.

    DATA: ls_saida           TYPE ty_saida,
          lv_objname         TYPE e071-obj_name,
          lv_nclosing_object TYPE rseuap-encl_obj,
          lv_aux             TYPE i,
          lv_object(75)      TYPE c.


    READ TABLE lcl_main=>lr_main->it_saida INTO ls_saida INDEX row.

    IF ls_saida-object EQ 'MESS'.
      lv_aux = STRLEN( ls_saida-obj_name ).
      SUBTRACT 3 FROM lv_aux.
      lv_nclosing_object = ls_saida-obj_name(lv_aux).
      lv_objname = ls_saida-obj_name+lv_aux.
    ELSEIF ls_saida-object EQ 'METH'.
      SPLIT ls_saida-obj_name AT space
        INTO lv_nclosing_object lv_objname.
      CONDENSE: lv_nclosing_object, lv_objname NO-GAPS.
    ELSE.
      lv_objname = ls_saida-obj_name.
    ENDIF.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation        = 'SHOW'
        object_name      = lv_objname
        object_type      = ls_saida-object
        enclosing_object = lv_nclosing_object.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.                    "on_link_click

  METHOD process_e071.

    DATA: ls_e071       TYPE e071,
          ls_saida      TYPE ty_saida,
          lv_exist      TYPE c,
          lt_references TYPE akb_except_type,
          ls_ko100      TYPE ko100.

    LOOP AT me->it_e071 INTO ls_e071 FROM tabix.
      IF e070           IS NOT INITIAL AND
         ls_e071-trkorr NE e070-trkorr.
        EXIT.
      ENDIF.

      "Verifica se o objeto existe
      CLEAR lv_exist.
      CALL FUNCTION 'TR_CHECK_EXIST'
        EXPORTING
          iv_pgmid             = ls_e071-pgmid
          iv_object            = ls_e071-object
          iv_obj_name          = ls_e071-obj_name
        IMPORTING
          e_exist              = lv_exist
        EXCEPTIONS
          tr_no_check_function = 1
          OTHERS               = 2.

      CHECK lv_exist EQ abap_true.

      "Busca utilização de objetos
      FREE lt_references[].
      CALL FUNCTION 'AKB_WHERE_USED_LIST'
        EXPORTING
          obj_type   = ls_e071-object
          obj_name   = ls_e071-obj_name(40)
        IMPORTING
          references = lt_references.

      CLEAR ls_saida.
      MOVE-CORRESPONDING: ls_e071 TO ls_saida.
      MOVE-CORRESPONDING: e070 TO ls_saida.

      ls_saida-cont = LINES( lt_references[] ).

      CLEAR ls_ko100.
      READ TABLE me->it_ko100 INTO ls_ko100
        WITH KEY pgmid  = ls_e071-pgmid
                 object = ls_e071-object
                 BINARY SEARCH.

      ls_saida-text = ls_ko100-text.

      ls_saida-obj_text = me->get_obj_text(
        object   = ls_e071-object
        obj_name = ls_e071-obj_name ).

      APPEND ls_saida TO me->it_saida.
    ENDLOOP.

  ENDMETHOD.                    "process_e071

  METHOD get_obj_text.

    CASE object.
      WHEN 'TRAN'.
        SELECT SINGLE ttext
          FROM tstct
          INTO obj_text
          WHERE tcode EQ obj_name(20)
            AND sprsl EQ sy-langu.
      WHEN 'TABL'.
        SELECT SINGLE ddtext
          FROM dd02t
          INTO obj_text
          WHERE tabname    EQ obj_name(30)
            AND ddlanguage EQ sy-langu.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "get_obj_text

ENDCLASS.                    "lcl_main IMPLEMENTATION
*&---------------------------------------------------------------------*
*       START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  lcl_main=>start( ).
