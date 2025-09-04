CLASS zcl_jp_get_data_report_fi DEFINITION
  PUBLIC
*  FINAL
  INHERITING FROM cx_rap_query_provider
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           tt_ranges             TYPE TABLE OF ty_range_option,

           tt_returns            TYPE TABLE OF bapiret2,

           tt_soquytienmat       TYPE TABLE OF zjp_c_soquytienmat,

           tt_bangcandoiphatsinh TYPE TABLE OF zjp_c_bangcandoiphatsinh,

           tt_phieuketoan        TYPE TABLE OF zjp_c_phieuketoan,

           tt_phieuketoan_items  TYPE TABLE OF zjp_c_phieuketoan_items.

    "Custom Entities
    INTERFACES if_rap_query_provider.

    CLASS-DATA: gt_soquytienmat       TYPE tt_soquytienmat,

                gt_bangcandoiphatsinh TYPE tt_bangcandoiphatsinh,

                gt_phieuketoan        TYPE tt_phieuketoan,

                gt_phieuketoan_items  TYPE tt_phieuketoan_items.

    CLASS-METHODS: get_soquytienmat IMPORTING ir_companycode        TYPE tt_ranges
                                              ir_glaccount          TYPE tt_ranges
                                              ir_accountingdocument TYPE tt_ranges OPTIONAL
                                              ir_postingdate        TYPE tt_ranges
                                              ir_fiscalyear         TYPE tt_ranges OPTIONAL
                                              ir_documentdate       TYPE tt_ranges OPTIONAL
                                              ir_businesspartner    TYPE tt_ranges OPTIONAL
                                    EXPORTING e_soquytienmat        TYPE tt_soquytienmat
                                              e_return              TYPE tt_returns .

    CLASS-METHODS: get_bangcandoiphatsinh IMPORTING ir_companycode        TYPE tt_ranges
                                                    ir_glaccount          TYPE tt_ranges
                                                    ir_accountingdocument TYPE tt_ranges OPTIONAL
                                                    ir_postingdate        TYPE tt_ranges
                                                    ir_fiscalyear         TYPE tt_ranges OPTIONAL
                                                    ir_documentdate       TYPE tt_ranges OPTIONAL
                                                    ir_businesspartner    TYPE tt_ranges OPTIONAL
                                          EXPORTING e_bangcandoiphatsinh  TYPE tt_bangcandoiphatsinh
                                                    e_return              TYPE tt_returns .

    CLASS-METHODS: get_phieuketoan IMPORTING ir_companycode        TYPE tt_ranges
                                             ir_accountingdocument TYPE tt_ranges
                                             ir_fiscalyear         TYPE tt_ranges
                                             ir_documentitem       TYPE tt_ranges OPTIONAL
                                             ir_postingdate        TYPE tt_ranges OPTIONAL
                                             ir_documentdate       TYPE tt_ranges OPTIONAL
                                             ir_documenttype       TYPE tt_ranges OPTIONAL
                                             ir_customer           TYPE tt_ranges OPTIONAL
                                             ir_supplier           TYPE tt_ranges OPTIONAL
                                   EXPORTING e_phieuketoan         TYPE tt_phieuketoan
                                             e_phieuketoan_items   TYPE tt_phieuketoan_items
                                             e_return              TYPE tt_returns .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JP_GET_DATA_REPORT_FI IMPLEMENTATION.


  METHOD get_bangcandoiphatsinh.

*    SELECT
*        CompanyCode,
*        accountingdocument,
*        GLAccount,
*        companycodecurrency,
*        DebitCreditCode,
*        AmountInCompanyCodeCurrency,
*        postingdate
*    FROM I_GLAccountLineItem
*    WHERE CompanyCode  IN @ir_companycode
**      AND AccountingDocument IN @ir_accountingdocument
**      AND FiscalYear   IN @ir_fiscalyear
*      AND PostingDate  IN @ir_postingdate
*
*      AND GLAccount    = '1120101001'
*      AND Ledger = '0L'
*    INTO TABLE @DATA(lt_check).

    SELECT
        CompanyCode,
        GLAccount,
        companycodecurrency,
        DebitCreditCode,
        SUM( AmountInCompanyCodeCurrency ) AS AmountInCompanyCodeCurrency
    FROM I_GLAccountLineItem
    WHERE CompanyCode  IN @ir_companycode
*      AND AccountingDocument IN @ir_accountingdocument
*      AND FiscalYear   IN @ir_fiscalyear
      AND PostingDate  IN @ir_postingdate
      AND GLAccount    IN @ir_glaccount
      AND AccountingDocument NOT LIKE 'B%'
      AND Ledger = '0L'
    GROUP BY CompanyCode, GLAccount, CompanyCodeCurrency, DebitCreditCode
    INTO TABLE @DATA(lt_GLAccountLineItem).

    DATA: ls_postingdate LIKE LINE OF ir_postingdate,
          lv_startdate   TYPE budat.

    READ TABLE ir_postingdate INTO ls_postingdate INDEX 1.
    IF sy-subrc EQ 0.
      lv_startdate = ls_postingdate-low.
    ENDIF.

    SELECT
        CompanyCode,
        GLAccount,
        companycodecurrency,
        SUM( AmountInCompanyCodeCurrency ) AS AmountInCompanyCodeCurrency
    FROM I_GLAccountLineItem
    WHERE CompanyCode  IN @ir_companycode
      AND PostingDate  LT @lv_startdate
      AND GLAccount    IN @ir_glaccount
      AND Ledger = '0L'
    GROUP BY CompanyCode, GLAccount, CompanyCodeCurrency
    INTO TABLE @DATA(lt_StartBalanceGL).

***Sort
    SORT lt_StartBalanceGL BY companycode glaccount .
    SORT lt_glaccountlineitem BY CompanyCode GLAccount DebitCreditCode ASCENDING.

    DATA: ls_bangcandoiphatsinh LIKE LINE OF e_bangcandoiphatsinh.

    LOOP AT lt_glaccountlineitem INTO DATA(ls_glaccountlineitem).
      MOVE-CORRESPONDING ls_glaccountlineitem TO ls_bangcandoiphatsinh.
      IF ls_glaccountlineitem-DebitCreditCode = 'S'.
        ls_bangcandoiphatsinh-DebitBalanceofReportingPeriod = ls_glaccountlineitem-amountincompanycodecurrency.
      ELSE.
        ls_bangcandoiphatsinh-CreditBalanceofReportingPeriod = ls_glaccountlineitem-amountincompanycodecurrency.
      ENDIF.

      ls_bangcandoiphatsinh-PostingdateFrom = ls_postingdate-low.
      ls_bangcandoiphatsinh-PostingdateTo = ls_postingdate-high.

      COLLECT ls_bangcandoiphatsinh INTO e_bangcandoiphatsinh.
      CLEAR: ls_bangcandoiphatsinh.
    ENDLOOP.

    LOOP AT e_bangcandoiphatsinh ASSIGNING FIELD-SYMBOL(<fs_bangcandoiphatsinh>).
      READ TABLE lt_startbalancegl INTO DATA(ls_startbalancegl) WITH KEY CompanyCode = <fs_bangcandoiphatsinh>-CompanyCode
          GLAccount = <fs_bangcandoiphatsinh>-GLAccount
          BINARY SEARCH.
      IF sy-subrc EQ 0.

      ELSE.

      ENDIF.

      <fs_bangcandoiphatsinh>-StartingBalanceInCompanyCode = ls_startbalancegl-amountincompanycodecurrency.
      <fs_bangcandoiphatsinh>-EndingBalanceInCompanyCode = ls_startbalancegl-amountincompanycodecurrency
          + <fs_bangcandoiphatsinh>-DebitBalanceofReportingPeriod
          + <fs_bangcandoiphatsinh>-CreditBalanceofReportingPeriod .

      SELECT SINGLE GLAccountName FROM I_GlAccountTextInCompanycode
      WHERE CompanyCode = @<fs_bangcandoiphatsinh>-CompanyCode
      AND GLAccount = @<fs_bangcandoiphatsinh>-GLAccount
      AND Language = 'E'
      INTO @<fs_bangcandoiphatsinh>-GLAccountName.

      CLEAR: ls_startbalancegl.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_phieuketoan.
    DATA: ls_phieuketoan       TYPE zjp_c_phieuketoan,
          ls_phieuketoan_items TYPE zjp_c_phieuketoan_items.

    DATA: lt_phieuketoan       TYPE tt_phieuketoan,
          lt_phieuketoan_items TYPE tt_phieuketoan_items.

    SELECT
          CompanyCode ,
          AccountingDocument ,
          FiscalYear ,
          LedgerGLLineItem ,

          PostingDate ,
          DocumentDate ,
          AccountingDocumentType ,

          GLAccount ,
          DocumentItemText ,
          DebitCreditCode ,

          CompanyCodeCurrency ,
          AmountInCompanyCodeCurrency ,

          TransactionCurrency ,
          AmountInTransactionCurrency ,

          Customer ,
          Supplier,

          AccountingDocCreatedByUser,
          CreationDate ,
          CreationDateTime,

          IsReversal,
          IsReversed
    FROM I_GLAccountLineItem

    WHERE CompanyCode IN @ir_companycode
      AND AccountingDocument IN @ir_accountingdocument
      AND FiscalYear IN @ir_fiscalyear
      AND PostingDate IN @ir_postingdate
      AND DocumentDate IN @ir_documentdate
      AND AccountingDocumentType IN @ir_documenttype
      AND IsReversal EQ ''
      AND IsReversed EQ ''
      AND Ledger = '0L'
      AND LedgerGLLineItem IN @ir_documentitem
      INTO TABLE @DATA(lt_data).

    SORT lt_data BY CompanyCode AccountingDocument FiscalYear PostingDate DocumentDate LedgerGLLineItem ASCENDING.

    DATA: lv_index TYPE int4.

    LOOP AT lt_data INTO DATA(ls_data).

      IF ls_data-TransactionCurrency NE ls_data-CompanyCodeCurrency.
        SELECT SINGLE AbsoluteExchangeRate FROM I_JournalEntry
        WHERE CompanyCode = @ls_data-CompanyCode
          AND AccountingDocument = @ls_data-AccountingDocument
          AND FiscalYear = @ls_data-FiscalYear
        INTO @DATA(lv_exchangerate).

      ENDIF.

      READ TABLE lt_phieuketoan ASSIGNING FIELD-SYMBOL(<lfs_phieuketoan>) WITH KEY CompanyCode = ls_data-CompanyCode
           AccountingDocument = ls_data-AccountingDocument
           FiscalYear = ls_data-FiscalYear.
      IF sy-subrc NE 0.
        ls_phieuketoan-CompanyCode = ls_data-CompanyCode.
        ls_phieuketoan-AccountingDocument = ls_data-AccountingDocument.
        ls_phieuketoan-FiscalYear = ls_data-FiscalYear.
        ls_phieuketoan-PostingDate = ls_data-PostingDate.
        ls_phieuketoan-DocumentDate = ls_data-DocumentDate.

        IF ( ls_data-DebitCreditCode = 'S' AND ls_data-AmountInCompanyCodeCurrency < 0 )

        OR ( ls_data-DebitCreditCode = 'H' AND ls_data-AmountInCompanyCodeCurrency > 0 ).

          ls_phieuketoan-IsNegativePosting = 'X'.

        ENDIF.

        IF lv_exchangerate IS NOT INITIAL.
          ls_phieuketoan-AbsoluteExchangeRate = lv_exchangerate * 1000.
        ENDIF.

        ls_phieuketoan-AccountingDocumentType = ls_data-AccountingDocumentType.
        ls_phieuketoan-TransactionCurrency = ls_data-TransactionCurrency.
        ls_phieuketoan-CompanyCodeCurrency = ls_data-CompanyCodeCurrency.

        ls_phieuketoan-Customer = ls_data-Customer.
        ls_phieuketoan-Supplier = ls_data-Supplier.

        ls_phieuketoan-CreationByUser = ls_data-AccountingDocCreatedByUser.
        ls_phieuketoan-CreationDate = ls_data-CreationDate.
        ls_phieuketoan-CreationDateTime = ls_data-CreationDateTime.

        APPEND ls_phieuketoan TO lt_phieuketoan.
        CLEAR: ls_phieuketoan.
      ELSE.
        lv_index = sy-tabix.
        IF <lfs_phieuketoan>-Customer IS INITIAL AND <lfs_phieuketoan>-Supplier IS INITIAL.
          <lfs_phieuketoan>-Customer = ls_data-Customer.
          <lfs_phieuketoan>-Supplier = ls_data-Supplier.
        ENDIF.
      ENDIF.

      ls_phieuketoan_items-CompanyCode = ls_data-CompanyCode.
      ls_phieuketoan_items-AccountingDocument = ls_data-AccountingDocument.
      ls_phieuketoan_items-FiscalYear = ls_data-FiscalYear.
      ls_phieuketoan_items-AccountingDocumentType = ls_data-AccountingDocumentType.

      ls_phieuketoan_items-PostingDate = ls_data-PostingDate.
      ls_phieuketoan_items-DocumentDate = ls_data-DocumentDate.

      ls_phieuketoan_items-LegderGLItem = ls_data-LedgerGLLineItem.

      ls_phieuketoan_items-GLAccount = ls_data-GLAccount.
      ls_phieuketoan_items-DocumentItemText = ls_data-DocumentItemText.

      IF ( ls_data-DebitCreditCode = 'S' AND ls_data-AmountInCompanyCodeCurrency < 0 )

      OR ( ls_data-DebitCreditCode = 'H' AND ls_data-AmountInCompanyCodeCurrency > 0 ).

        ls_phieuketoan_items-IsNegativePosting = 'X'.

      ENDIF.

      IF lv_exchangerate IS NOT INITIAL.
        ls_phieuketoan_items-AbsoluteExchangeRate = lv_exchangerate * 1000.
      ENDIF.

      ls_phieuketoan_items-TransactionCurrency = ls_data-TransactionCurrency.
      ls_phieuketoan_items-CompanyCodeCurrency = ls_data-CompanyCodeCurrency.

      IF ls_data-DebitCreditCode = 'S'.
        ls_phieuketoan_items-DebitAmountInCompanyCode = ls_data-AmountInCompanyCodeCurrency.
        ls_phieuketoan_items-DebitAmountInTransaction = ls_data-AmountInTransactionCurrency.
      ELSE.
        ls_phieuketoan_items-CreditAmountInCompanyCode = ls_data-AmountInCompanyCodeCurrency .
        ls_phieuketoan_items-CreditAmountInTransaction = ls_data-AmountInTransactionCurrency .
      ENDIF.

      ls_phieuketoan_items-DebitCreditCode = ls_data-DebitCreditCode.

      ls_phieuketoan_items-Customer = ls_data-Customer.
      ls_phieuketoan_items-Supplier = ls_data-Supplier.

      APPEND ls_phieuketoan_items TO lt_phieuketoan_items.
      CLEAR: ls_phieuketoan_items.

      CLEAR: lv_exchangerate.

    ENDLOOP.

    IF ir_customer IS NOT INITIAL.
      DELETE lt_phieuketoan WHERE Customer NOT IN ir_customer.
    ENDIF.

    IF ir_supplier IS NOT INITIAL.
      DELETE lt_phieuketoan WHERE Supplier NOT IN ir_supplier.
    ENDIF.

    MOVE-CORRESPONDING lt_phieuketoan TO e_phieuketoan.
    MOVE-CORRESPONDING lt_phieuketoan_items TO e_phieuketoan_items.

  ENDMETHOD.


  METHOD get_soquytienmat.

    DATA: ls_return TYPE bapiret2.
    DATA(lo_common_app) = zcl_jp_common_core=>get_instance( ).

    SELECT
        a~companycode,
        a~accountingdocument,
        a~fiscalyear,
        a~LedgerGLLineItem,
        a~accountingdocumentitem,
        a~postingdate,
        a~documentdate,
        a~customer,
        a~supplier,
        a~GLAccount,
        a~CompanyCodeCurrency,
        a~TransactionCurrency,
        a~DocumentItemText,
        a~DebitCreditCode,
*        a~isnegativeposting,
        a~AmountInCompanyCodeCurrency,
        a~AmountInTransactionCurrency,
        b~DocumentReferenceID,
        b~AccountingDocumentType,
        b~ReverseDocument,
        b~ReverseDocumentFiscalYear,
        b~AccountingDocCreatedByUser,
        b~AccountingDocumentCreationDate,
        b~Creationtime
    FROM I_GLAccountLineItem AS a INNER JOIN I_JournalEntry AS b
        ON a~CompanyCode = b~CompanyCode
        AND a~AccountingDocument = b~AccountingDocument
        AND a~FiscalYear = b~FiscalYear
    WHERE a~CompanyCode  IN @ir_companycode
      AND a~AccountingDocument IN @ir_accountingdocument
      AND a~PostingDate  IN @ir_postingdate
      AND a~DocumentDate IN @ir_documentdate
      AND a~GLAccount    IN @ir_glaccount
      AND a~FiscalYear   IN @ir_fiscalyear
      AND (  a~Customer IN @ir_businesspartner OR a~Supplier IN @ir_businesspartner )
      AND a~Ledger = '0L'
      AND a~GLAccount LIKE '111%'
    INTO TABLE @DATA(lt_data).

***""" Message Lỗi
    IF sy-subrc NE 0.
      ls_return-type = 'E'.
      ls_return-message = 'Không có dữ liệu'.
*      APPEND ls_return TO e_return.
*      EXIT.
    ENDIF.

    SORT lt_data BY CompanyCode AccountingDocument FiscalYear ASCENDING.

    DATA: lv_index TYPE sy-tabix.
    DATA: lt_data_temp LIKE lt_data.

    LOOP AT lt_data INTO DATA(ls_data) WHERE ReverseDocument IS NOT INITIAL.
      lv_index = sy-tabix.
      READ TABLE lt_data INTO DATA(ls_data_temp) WITH KEY CompanyCode = ls_data-CompanyCode
          AccountingDocument = ls_data-ReverseDocument
          FiscalYear = ls_data-ReverseDocumentFiscalYear BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE lt_data INDEX lv_index.
        APPEND ls_data_temp TO lt_data_temp.
      ELSE.
        READ TABLE lt_data_temp TRANSPORTING NO FIELDS WITH KEY CompanyCode = ls_data-CompanyCode
        AccountingDocument = ls_data-AccountingDocument
        FiscalYear = ls_data-FiscalYear BINARY SEARCH.
        IF sy-subrc EQ 0.
          DELETE lt_data INDEX lv_index.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    SELECT * FROM firud_cf_off_acc
*    WITH PRIVILEGED ACCESS
*    into table @data(lt_firud_cf_off_acc).

    SELECT
        a~companycode,
        a~accountingdocument,
        a~fiscalyear,
        a~LedgerGLLineItem,
        a~accountingdocumentitem,
        a~postingdate,
        a~documentdate,
        a~GLAccount,
        a~FinancialAccountType,
        a~DebitCreditCode,
        b~DocumentReferenceID,
        a~customer,
        a~supplier,
        a~OffsettingAccount,
        a~OffsettingAccountType,
        a~OffsettingLedgerGLLineItem,
        a~AmountInCompanyCodeCurrency,
        a~AmountInTransactionCurrency
    FROM I_GLAccountLineItem AS a INNER JOIN I_JournalEntry AS b
        ON a~CompanyCode = b~CompanyCode
        AND a~AccountingDocument = b~AccountingDocument
        AND a~FiscalYear = b~FiscalYear
    FOR ALL ENTRIES IN @lt_data
    WHERE a~CompanyCode = @lt_data-CompanyCode
      AND a~AccountingDocument = @lt_data-AccountingDocument
      AND a~FiscalYear = @lt_data-FiscalYear
      AND a~Ledger = '0L'
      AND a~OffsettingLedgerGLLineItem NE ''
    INTO TABLE @DATA(lt_offsettingAccount).

    SORT lt_offsettingAccount BY CompanyCode AccountingDocument FiscalYear OffsettingLedgerGLLineItem ASCENDING.

    SELECT
          a~bukrs,
          a~gjahr,
          a~belnr,
          a~docln,
          a~offs_item,
          b~GLAccount,
          b~customer,
          b~supplier,
          c~DocumentReferenceID,
          a~drcrk,
          a~racct,
          a~lokkt,
          a~ktop2,
          a~blart,
          a~budat,
          a~rmvct,
          a~mwskz,
          a~rfarea,
          a~buzei,
          a~hsl,
          a~rhcur,
          a~ksl,
          a~rkcur
     FROM zfirud_cf_off AS a
     INNER JOIN I_GLAccountLineItem AS b ON a~bukrs = b~CompanyCode
                                        AND a~docln = b~LedgerGLLineItem
                                        AND a~belnr = b~AccountingDocument
                                        AND a~gjahr = b~FiscalYear
                                        AND a~rldnr = b~Ledger
     INNER JOIN I_JournalEntry AS c ON a~bukrs = c~CompanyCode
                                   AND a~belnr = c~AccountingDocument
                                   AND a~gjahr = c~FiscalYear
    FOR ALL ENTRIES IN @lt_data
    WHERE a~bukrs = @lt_data-CompanyCode
      AND a~belnr = @lt_data-AccountingDocument
      AND a~gjahr = @lt_data-FiscalYear
      AND b~Ledger = '0L'
      AND a~rldnr = '0L'
    INTO TABLE @DATA(lt_firud_cf_off).


***""" Tính số dư đầu kỳ
    lo_common_app->get_glaccount_balance(
        EXPORTING
        ir_companycode = ir_companycode
        ir_glaccount = ir_glaccount
        ir_date = ir_postingdate
        IMPORTING
        o_startbalance = DATA(lt_startbalance)
        o_endbalance = DATA(lt_endbalance)
    ).

    SORT lt_startbalance BY companycode glaccount ASCENDING.
    SORT lt_endbalance BY companycode glaccount ASCENDING.

    READ TABLE lt_startbalance INTO DATA(ls_startbalance) INDEX 1.

***""" Sort data.
    SORT lt_data BY CompanyCode FiscalYear PostingDate DocumentDate AccountingDocument LedgerGLLineItem ASCENDING.
    SORT lt_offsettingaccount BY CompanyCode AccountingDocument FiscalYear OffsettingLedgerGLLineItem ASCENDING.
    SORT lt_firud_cf_off BY bukrs belnr gjahr offs_item ASCENDING.
***"""------------------------------------------------------------------"""***
    DATA: ls_soquytienmat TYPE zjp_c_soquytienmat,
          lv_stt          TYPE int4 VALUE IS INITIAL.

    DATA: wa_document TYPE zst_document_info.
    DATA: lv_debitcocode  TYPE dmbtr,
          lv_creditcocode TYPE dmbtr,
          lv_debittrans   TYPE dmbtr,
          lv_credittrans  TYPE dmbtr.

    CLEAR: lv_index.

    LOOP AT lt_data INTO ls_data.

      lv_stt = lv_stt + 1.

      ls_soquytienmat-StartingBalanceInCoCode = ls_startbalance-amountincompanycode.
      ls_soquytienmat-StartingBalanceInTrans = ls_startbalance-amountintransaction.

      ls_soquytienmat-stt                    = lv_stt.
      ls_soquytienmat-CompanyCode            = ls_data-CompanyCode.
      ls_soquytienmat-AccountingDocument     = ls_data-AccountingDocument.
      ls_soquytienmat-FiscalYear             = ls_data-FiscalYear.
      ls_soquytienmat-AccountingDocumentItem = ls_data-AccountingDocumentItem.
      ls_soquytienmat-PostingDate            = ls_data-PostingDate.
      ls_soquytienmat-DocumentDate           = ls_data-DocumentDate.

      ls_soquytienmat-AccountingDocumentType = ls_data-AccountingDocumentType.
      ls_soquytienmat-GLAccount              = ls_data-GLAccount.

      ls_soquytienmat-DebitCreditCode        = ls_data-DebitCreditCode.
      ls_soquytienmat-CompanyCodeCurrency    = ls_data-CompanyCodeCurrency.
      ls_soquytienmat-TransactionCurrency    = ls_data-TransactionCurrency.

      wa_document-companycode                = ls_data-CompanyCode.
      wa_document-accountingdocument         = ls_data-AccountingDocument.
      wa_document-fiscalyear                 = ls_data-FiscalYear.

      IF ls_data-Customer IS NOT INITIAL.
*        wa_document-customer = ls_data-Customer.
*
*        lo_common_app->get_businesspartner_details(
*            EXPORTING
*            i_document = wa_document
*            IMPORTING
*            o_BPdetails = DATA(ls_BP_detail)
*        ).
*
*        ls_soquytienmat-Doituong = ls_BP_detail-BPname.

      ELSEIF ls_data-Supplier IS NOT INITIAL.
*        wa_document-supplier = ls_data-Supplier.
*
*        lo_common_app->get_businesspartner_details(
*            EXPORTING
*            i_document = wa_document
*            IMPORTING
*            o_BPdetails = ls_BP_detail
*        ).
*        ls_soquytienmat-Doituong = ls_BP_detail-BPname.
      ENDIF.

      ls_soquytienmat-businesspartner = wa_document-customer.
      ls_soquytienmat-Diengiai        = ls_data-DocumentItemText.

*      ls_soquytienmat-IsNegativePosting = ls_data-IsNegativePosting.

      ls_soquytienmat-CreationUser    = ls_data-AccountingDocCreatedByUser.
      ls_soquytienmat-CreationDate    = ls_data-AccountingDocumentCreationDate.
      ls_soquytienmat-CreationTime    = ls_data-CreationTime.

      IF ( ls_data-DebitCreditCode = 'S' AND ls_data-AmountInCompanyCodeCurrency < 0 )
      OR ( ls_data-DebitCreditCode = 'H' AND ls_data-AmountInTransactionCurrency > 0 ).
        ls_soquytienmat-IsNegativePosting = 'X'.
      ENDIF.

      READ TABLE lt_offsettingaccount INTO DATA(ls_offsettingaccount)
      WITH KEY companycode = ls_data-CompanyCode
               AccountingDocument = ls_data-AccountingDocument
               FiscalYear = ls_data-FiscalYear
               OffsettingLedgerGLLineItem = ls_data-LedgerGLLineItem BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_soquytienmat-OffsettingAccount = ls_offsettingaccount-GLAccount.
        IF ls_offsettingaccount-Customer IS NOT INITIAL.
          ls_soquytienmat-businesspartner = ls_offsettingaccount-Customer.
          wa_document-customer = ls_offsettingaccount-Customer.

          lo_common_app->get_businesspartner_details(
              EXPORTING
              i_document = wa_document
              IMPORTING
              o_BPdetails = DATA(ls_BP_detail)
          ).

          ls_soquytienmat-Doituong = ls_BP_detail-BPname.
        ELSE.
          ls_soquytienmat-businesspartner = ls_offsettingaccount-Supplier.
          wa_document-supplier = ls_offsettingaccount-Supplier.

          lo_common_app->get_businesspartner_details(
              EXPORTING
              i_document = wa_document
              IMPORTING
              o_BPdetails = ls_BP_detail
          ).
          ls_soquytienmat-Doituong = ls_BP_detail-BPname.
        ENDIF.

        IF ls_data-DebitCreditCode = 'S'.
          ls_soquytienmat-DebitAmountInCoCode =  abs( ls_offsettingaccount-AmountInCompanyCodeCurrency ).
          ls_soquytienmat-DebitAmountInTrans =  abs( ls_offsettingaccount-AmountInTransactionCurrency ).

          ls_soquytienmat-SoHieuCTThu = ls_offsettingaccount-DocumentReferenceID.

          IF ls_soquytienmat-IsNegativePosting IS NOT INITIAL.
            ls_soquytienmat-DebitAmountInCoCode = ls_soquytienmat-DebitAmountInCoCode * -1.
            ls_soquytienmat-DebitAmountInTrans = ls_soquytienmat-DebitAmountInTrans * -1.
          ENDIF.
        ELSE.
          ls_soquytienmat-CreditAmountInCoCode =  ls_offsettingaccount-AmountInCompanyCodeCurrency .
          ls_soquytienmat-CreditAmountInTrans =  ls_offsettingaccount-AmountInTransactionCurrency .

          ls_soquytienmat-SoHieuCTChi = ls_offsettingaccount-DocumentReferenceID.
        ENDIF.

        ls_soquytienmat-BalanceInCoCode = ls_soquytienmat-DebitAmountInCoCode - ls_soquytienmat-CreditAmountInCoCode
                                          + ls_startbalance-amountincompanycode.
        ls_soquytienmat-BalanceInTrans =  ls_soquytienmat-DebitAmountInTrans - ls_soquytienmat-CreditAmountInTrans
                                          + ls_startbalance-amountintransaction.

        ls_startbalance-amountincompanycode =  ls_soquytienmat-BalanceInCoCode.
        ls_startbalance-amountintransaction =  ls_soquytienmat-BalanceInTrans.

        APPEND ls_soquytienmat TO e_soquytienmat.

        CLEAR: ls_soquytienmat, wa_document.

        CONTINUE.
      ELSE.
        READ TABLE lt_firud_cf_off TRANSPORTING NO FIELDS WITH KEY bukrs = ls_data-CompanyCode
          belnr = ls_data-AccountingDocument
          gjahr = ls_data-FiscalYear
          offs_item = ls_data-LedgerGLLineItem BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_index = sy-tabix.
          LOOP AT lt_firud_cf_off INTO DATA(ls_firud_cf_off) FROM lv_index.
            IF NOT ( ls_firud_cf_off-bukrs = ls_data-CompanyCode AND
             ls_firud_cf_off-belnr = ls_data-AccountingDocument AND
             ls_firud_cf_off-gjahr = ls_data-FiscalYear AND
             ls_firud_cf_off-offs_item = ls_data-LedgerGLLineItem
             ) .
              EXIT.
            ENDIF.
            ls_soquytienmat-OffsettingAccount = ls_firud_cf_off-GLAccount.
            IF ls_firud_cf_off-Customer IS NOT INITIAL.
              ls_soquytienmat-businesspartner = ls_firud_cf_off-Customer.
              wa_document-customer = ls_firud_cf_off-Customer.

              lo_common_app->get_businesspartner_details(
                  EXPORTING
                  i_document = wa_document
                  IMPORTING
                  o_BPdetails = ls_BP_detail
              ).

              ls_soquytienmat-Doituong = ls_BP_detail-BPname.
            ELSE.
              ls_soquytienmat-businesspartner = ls_firud_cf_off-Supplier.
              wa_document-supplier = ls_firud_cf_off-Supplier.

              lo_common_app->get_businesspartner_details(
                  EXPORTING
                  i_document = wa_document
                  IMPORTING
                  o_BPdetails = ls_BP_detail
              ).
              ls_soquytienmat-Doituong = ls_BP_detail-BPname.
            ENDIF.

            IF ls_data-DebitCreditCode = 'S'.
              ls_soquytienmat-DebitAmountInCoCode = abs( ls_firud_cf_off-hsl ).
              ls_soquytienmat-DebitAmountInTrans = abs( ls_firud_cf_off-ksl ).

              IF ls_soquytienmat-IsNegativePosting IS NOT INITIAL.
                ls_soquytienmat-DebitAmountInCoCode = ls_soquytienmat-DebitAmountInCoCode * -1.
                ls_soquytienmat-DebitAmountInTrans = ls_soquytienmat-DebitAmountInTrans * -1.
              ENDIF.

              ls_soquytienmat-SoHieuCTThu = ls_firud_cf_off-DocumentReferenceID.
            ELSE.
              ls_soquytienmat-CreditAmountInCoCode = abs( ls_firud_cf_off-hsl ).
              ls_soquytienmat-CreditAmountInTrans = abs( ls_firud_cf_off-ksl ).

              ls_soquytienmat-SoHieuCTChi = ls_firud_cf_off-DocumentReferenceID.
            ENDIF.

            ls_soquytienmat-BalanceInCoCode = ls_soquytienmat-DebitAmountInCoCode - ls_soquytienmat-CreditAmountInCoCode
                                          + ls_startbalance-amountincompanycode.
            ls_soquytienmat-BalanceInTrans =  ls_soquytienmat-DebitAmountInTrans - ls_soquytienmat-CreditAmountInTrans
                                          + ls_startbalance-amountintransaction.

            ls_startbalance-amountincompanycode =  ls_soquytienmat-BalanceInCoCode.
            ls_startbalance-amountintransaction =  ls_soquytienmat-BalanceInTrans.

            APPEND ls_soquytienmat TO e_soquytienmat.
            CLEAR: ls_soquytienmat, wa_document.
          ENDLOOP.

          CLEAR: ls_soquytienmat, wa_document.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR: ls_BP_detail.

      "S - Debit = Nợ <-> Thu / H - Credit = Có <-> Chi
*      IF ls_data-IsNegativePosting IS NOT INITIAL.
*        IF ls_data-DebitCreditCode = 'S'.
*          ls_soquytienmat-CreditAmountInCoCode = ls_data-AbsoluteAmountInCoCodeCrcy * -1.
*          ls_soquytienmat-CreditAmountInTrans = ls_data-AbsoluteAmountInTransacCrcy * -1.
*
*          ls_soquytienmat-sohieuCTchi = ls_data-DocumentReferenceID.
*        ELSE.
*          ls_soquytienmat-DebitAmountInCoCode = ls_data-AbsoluteAmountInCoCodeCrcy * -1.
*          ls_soquytienmat-DebitAmountInTrans = ls_data-AbsoluteAmountInTransacCrcy * -1.
*
*          ls_soquytienmat-sohieuCTthu = ls_data-DocumentReferenceID.
*        ENDIF.
*
*        ls_soquytienmat-BalanceInCoCode = ls_soquytienmat-DebitAmountInCoCode + ls_soquytienmat-CreditAmountInCoCode
*                       + ls_startbalance-amountincompanycode.
*        ls_soquytienmat-BalanceInTrans = ls_soquytienmat-DebitAmountInTrans + ls_soquytienmat-CreditAmountInTrans
*                       + ls_startbalance-amountintransaction.
*      ELSE.
*        IF ls_data-DebitCreditCode = 'S'.
*          ls_soquytienmat-DebitAmountInCoCode = ls_data-AbsoluteAmountInCoCodeCrcy.
*          ls_soquytienmat-DebitAmountInTrans = ls_data-AbsoluteAmountInTransacCrcy.
*
*          ls_soquytienmat-sohieuCTthu = ls_data-DocumentReferenceID.
*        ELSE.
*          ls_soquytienmat-CreditAmountInCoCode = ls_data-AbsoluteAmountInCoCodeCrcy.
*          ls_soquytienmat-CreditAmountInTrans = ls_data-AbsoluteAmountInTransacCrcy.
*
*          ls_soquytienmat-sohieuCTchi = ls_data-DocumentReferenceID.
*        ENDIF.
*
*        ls_soquytienmat-BalanceInCoCode = ls_soquytienmat-DebitAmountInCoCode - ls_soquytienmat-CreditAmountInCoCode
*                       + ls_startbalance-amountincompanycode.
*        ls_soquytienmat-BalanceInTrans = ls_soquytienmat-DebitAmountInTrans - ls_soquytienmat-CreditAmountInTrans
*                       + ls_startbalance-amountintransaction.
*      ENDIF.

      IF ls_data-DebitCreditCode = 'S'.
        ls_soquytienmat-DebitAmountInCoCode = ls_data-AmountInCompanyCodeCurrency.
        ls_soquytienmat-DebitAmountInTrans = ls_data-AmountInTransactionCurrency.

        ls_soquytienmat-SoHieuCTThu = ls_data-DocumentReferenceID.
      ELSE.
        ls_soquytienmat-CreditAmountInCoCode = abs( ls_data-AmountInCompanyCodeCurrency ).
        ls_soquytienmat-CreditAmountInTrans = abs( ls_data-AmountInTransactionCurrency ).

        ls_soquytienmat-SoHieuCTChi = ls_data-DocumentReferenceID.
      ENDIF.

      ls_soquytienmat-BalanceInCoCode = ls_data-AmountInCompanyCodeCurrency + ls_startbalance-amountincompanycode.
      ls_soquytienmat-BalanceInTrans =  ls_data-AmountInTransactionCurrency + ls_startbalance-amountintransaction.

      "Số dư
*      READ TABLE lt_startbalance ASSIGNING FIELD-SYMBOL(<fs_startbalance>)
*      WITH KEY companycode = ls_soquytienmat-CompanyCode
*               glaccount = ls_soquytienmat-GLAccount BINARY SEARCH.
*      IF sy-subrc EQ 0.

      ls_startbalance-amountincompanycode =  ls_soquytienmat-BalanceInCoCode.
      ls_startbalance-amountintransaction =  ls_soquytienmat-BalanceInTrans.

*      ENDIF.

      APPEND ls_soquytienmat TO e_soquytienmat.

      CLEAR: ls_soquytienmat, wa_document.

    ENDLOOP.

  ENDMETHOD.


  METHOD if_rap_query_provider~select.
**--- Custom Entities ---**
    DATA: ls_page_info          TYPE zcl_jp_common_core=>st_page_info,

          ir_companycode        TYPE zcl_jp_common_core=>tt_ranges,
          ir_accountingdocument TYPE zcl_jp_common_core=>tt_ranges,
          ir_glaccount          TYPE zcl_jp_common_core=>tt_ranges,
          ir_fiscalyear         TYPE zcl_jp_common_core=>tt_ranges,
          ir_postingdate        TYPE zcl_jp_common_core=>tt_ranges,
          ir_documentdate       TYPE zcl_jp_common_core=>tt_ranges,
          ir_statussap          TYPE zcl_jp_common_core=>tt_ranges,
          ir_einvoicenumber     TYPE zcl_jp_common_core=>tt_ranges,
          ir_einvoicetype       TYPE zcl_jp_common_core=>tt_ranges,
          ir_currencytype       TYPE zcl_jp_common_core=>tt_ranges,
          ir_usertype           TYPE zcl_jp_common_core=>tt_ranges,
          ir_typeofdate         TYPE zcl_jp_common_core=>tt_ranges,
          ir_createdbyuser      TYPE zcl_jp_common_core=>tt_ranges,
          ir_enduser            TYPE zcl_jp_common_core=>tt_ranges,
          ir_testrun            TYPE zcl_jp_common_core=>tt_ranges,

          ir_businesspartner    TYPE zcl_jp_common_core=>tt_ranges,

          ir_supplier           TYPE zcl_jp_common_core=>tt_ranges,
          ir_customer           TYPE zcl_jp_common_core=>tt_ranges,
          ir_documenttype       TYPE zcl_jp_common_core=>tt_ranges,

          ir_documentitem       TYPE zcl_jp_common_core=>tt_ranges.
          .

    DATA: lt_returns TYPE tt_returns.
    DATA: lo_report_fi TYPE REF TO zcl_jp_get_data_report_fi.

    FREE: lt_returns.

    DATA(lt_req_elements) = io_request->get_requested_elements( ).

    DATA(lt_aggr_element) = io_request->get_aggregation( )->get_aggregated_elements( ).

    DATA(lv_entity_id) = io_request->get_entity_id( ).

    lo_report_fi = NEW #( ).

    DATA(lo_common_app) = zcl_jp_common_core=>get_instance( ).

    lo_common_app->get_fillter_app(
        EXPORTING
            io_request  = io_request
            io_response = io_response
        IMPORTING
            ir_companycode        = ir_companycode
            ir_accountingdocument = ir_accountingdocument
            ir_fiscalyear         = ir_fiscalyear
            ir_glaccount          = ir_glaccount
            ir_postingdate        = ir_postingdate
            ir_documentdate       = ir_documentdate

*            ir_statussap          = ir_statussap
*            ir_einvoicenumber     = ir_einvoicenumber
*            ir_einvoicetype       = ir_einvoicetype
*            ir_currencytype       = ir_currencytype
*            ir_usertype           = ir_usertype
*            ir_typeofdate         = ir_typeofdate
*            ir_createdbyuser      = ir_createdbyuser
*            ir_enduser            = ir_enduser
*            ir_testrun            = ir_testrun

            ir_businesspartner     = ir_businesspartner

            ir_documenttype         = ir_documenttype
            ir_customer             = ir_customer
            ir_supplier             = ir_supplier

            ir_documentitem         = ir_documentitem

            wa_page_info          = ls_page_info
    ).

    IF ls_page_info-page_size < 0.
      ls_page_info-page_size = 50.
    ENDIF.

    DATA(max_rows) = COND #( WHEN ls_page_info-page_size = if_rap_query_paging=>page_size_unlimited THEN 0
               ELSE ls_page_info-page_size ).

    max_rows = ls_page_info-page_size + ls_page_info-offset.

    CASE lv_entity_id.

      WHEN 'ZJP_C_SOQUYTIENMAT'.
        lo_report_fi->get_soquytienmat(
            EXPORTING
            ir_companycode   = ir_companycode
            ir_glaccount    = ir_glaccount
            ir_accountingdocument = ir_accountingdocument
            ir_postingdate  = ir_postingdate
            ir_fiscalyear   = ir_fiscalyear
            ir_documentdate = ir_documentdate
            IMPORTING
            e_soquytienmat = gt_soquytienmat
            e_return       = lt_returns
        ).

        IF lt_returns IS NOT INITIAL.
          READ TABLE lt_returns INTO DATA(ls_returns) INDEX 1.

          RAISE EXCEPTION TYPE zcl_jp_get_data_report_fi
              MESSAGE ID ''
              TYPE ls_returns-type
              NUMBER ''
              WITH |{ ls_returns-message }|.
          RETURN.

        ENDIF.

        DATA: lt_soquytienmat TYPE tt_soquytienmat.

        LOOP AT gt_soquytienmat INTO DATA(ls_soquytienmat).
          IF sy-tabix > ls_page_info-offset.
            IF sy-tabix > max_rows.
              EXIT.
            ELSE.
              APPEND ls_soquytienmat TO lt_soquytienmat.
            ENDIF.
          ENDIF.
        ENDLOOP.


        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( gt_soquytienmat ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_soquytienmat ).
        ENDIF.

      WHEN 'ZJP_C_BANGCANDOIPHATSINH'.

        lo_report_fi->get_bangcandoiphatsinh(
            EXPORTING
            ir_companycode   = ir_companycode
            ir_glaccount    = ir_glaccount
            ir_accountingdocument = ir_accountingdocument
            ir_postingdate  = ir_postingdate
            ir_fiscalyear   = ir_fiscalyear
            ir_documentdate = ir_documentdate
            IMPORTING
            e_bangcandoiphatsinh = gt_bangcandoiphatsinh
            e_return       = lt_returns
        ).

        DATA: lt_bangcandoiphatsinh TYPE tt_bangcandoiphatsinh.

        LOOP AT gt_bangcandoiphatsinh INTO DATA(ls_bangcandoiphatsinh).
          IF sy-tabix > ls_page_info-offset.
            IF sy-tabix > max_rows.
              EXIT.
            ELSE.
              APPEND ls_bangcandoiphatsinh TO lt_bangcandoiphatsinh.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( gt_bangcandoiphatsinh ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_bangcandoiphatsinh ).
        ENDIF.

      WHEN 'ZJP_C_PHIEUKETOAN'.
        lo_report_fi->get_phieuketoan(
            EXPORTING
            ir_companycode   = ir_companycode
*            ir_glaccount    = ir_glaccount
            ir_accountingdocument = ir_accountingdocument
            ir_postingdate  = ir_postingdate
            ir_fiscalyear   = ir_fiscalyear
            ir_documentdate = ir_documentdate
            ir_documenttype = ir_documenttype
            ir_customer     = ir_customer
            ir_supplier     = ir_supplier
            IMPORTING
            e_phieuketoan = gt_phieuketoan
            e_return       = lt_returns
        ).

        DATA: lt_phieuketoan TYPE tt_phieuketoan.

        LOOP AT gt_phieuketoan INTO DATA(ls_phieuketoan).
          IF sy-tabix > ls_page_info-offset.
            IF sy-tabix > max_rows.
              EXIT.
            ELSE.
              APPEND ls_phieuketoan TO lt_phieuketoan.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( gt_phieuketoan ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_phieuketoan ).
        ENDIF.
      WHEN 'ZJP_C_PHIEUKETOAN_ITEMS'.
        lo_report_fi->get_phieuketoan(
              EXPORTING
              ir_companycode   = ir_companycode
*              ir_glaccount    = ir_glaccount
              ir_accountingdocument = ir_accountingdocument
              ir_postingdate  = ir_postingdate
              ir_fiscalyear   = ir_fiscalyear
              ir_documentdate = ir_documentdate
              ir_documentitem = ir_documentitem
              IMPORTING
              e_phieuketoan = gt_phieuketoan
              e_phieuketoan_items = gt_phieuketoan_items
              e_return       = lt_returns
          ).

        DATA: lt_phieuketoan_items TYPE tt_phieuketoan_items.

        LOOP AT gt_phieuketoan_items INTO DATA(ls_phieuketoan_items).
          IF sy-tabix > ls_page_info-offset.
            IF sy-tabix > max_rows.
              EXIT.
            ELSE.
              APPEND ls_phieuketoan_items TO lt_phieuketoan_items.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records( lines( gt_phieuketoan_items ) ).
        ENDIF.

        IF io_request->is_data_requested( ).
          io_response->set_data( lt_phieuketoan_items ).
        ENDIF.
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
