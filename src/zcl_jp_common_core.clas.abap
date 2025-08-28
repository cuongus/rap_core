CLASS zcl_jp_common_core DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_range_option,
             sign   TYPE c LENGTH 1,
             option TYPE c LENGTH 2,
             low    TYPE string,
             high   TYPE string,
           END OF ty_range_option,

           BEGIN OF ty_page_info,
             paging           TYPE REF TO if_rap_query_paging,
             page_size        TYPE int8,
             offset           TYPE int8,
             requested_fields TYPE if_rap_query_request=>tt_requested_elements,
             sort_order       TYPE if_rap_query_request=>tt_sort_elements,
             ro_filter        TYPE REF TO if_rap_query_filter,
             entity_id        TYPE string,
           END OF ty_page_info,

           BEGIN OF ty_Balance,
             companycode         TYPE bukrs,
             GLaccount           TYPE hkont,
             AmountinCompanycode TYPE zde_dmbtr,
             AmountinTransaction TYPE zde_dmbtr,
             CompanycodeCurrency TYPE waers,
             TransactionCurrency TYPE waers,
           END OF ty_Balance,

           BEGIN OF zy_fins_acdoc_header_in,
             companycode            TYPE bukrs,
             accountingdocument     TYPE belnr_d,
             fiscalyear             TYPE gjahr,
             transactioncode        TYPE tcode,
             accountingdocumenttype TYPE blart,
             postingdate            TYPE budat,
             documentdate           TYPE bldat,
           END OF zy_fins_acdoc_header_in,

           BEGIN OF zy_fins_acdoc_item_in,
             accountingdocumentitem     TYPE buzei,
             clearingaccountingdocument TYPE augbl,
             postingkey                 TYPE bschl,
             financialaccounttype       TYPE koart,
             debitcreditcode            TYPE shkzg,
             businessarea               TYPE gsber,
             taxcode                    TYPE mwskz,
           END OF zy_fins_acdoc_item_in,

           tt_ranges              TYPE TABLE OF ty_range_option,
           st_page_info           TYPE ty_page_info,

           tt_Balance             TYPE TABLE OF ty_Balance,

           z_fins_acdoc_header_in TYPE zy_fins_acdoc_header_in,
           z_fins_acdoc_item_in   TYPE zy_fins_acdoc_item_in
           .

    INTERFACES if_oo_adt_classrun.

    CLASS-DATA:
      "Instance Singleton
      mo_instance             TYPE REF TO zcl_jp_common_core,

      "Table customer info
      gt_businesspartner_info TYPE SORTED TABLE OF zst_businesspartner_info WITH UNIQUE KEY bpnumber.

    CLASS-METHODS:
      "Contructor
      get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_jp_common_core,

      "Get fillter app
      get_fillter_app IMPORTING io_request             TYPE REF TO if_rap_query_request
                                io_response            TYPE REF TO if_rap_query_response

                      EXPORTING ir_companycode         TYPE tt_ranges
                                ir_accountingdocument  TYPE tt_ranges
                                ir_glaccount           TYPE tt_ranges
                                ir_fiscalyear          TYPE tt_ranges
                                ir_buzei               TYPE tt_ranges
                                ir_postingdate         TYPE tt_ranges
                                ir_documentdate        TYPE tt_ranges

                                ir_statussap           TYPE tt_ranges
                                ir_einvoicenumber      TYPE tt_ranges
                                ir_einvoicetype        TYPE tt_ranges
                                ir_currencytype        TYPE tt_ranges
                                ir_usertype            TYPE tt_ranges
                                ir_TypeOfDate          TYPE tt_ranges

                                ir_createdbyuser       TYPE tt_ranges
                                ir_enduser             TYPE tt_ranges
                                ir_testrun             TYPE tt_ranges

                                ir_businesspartner     TYPE tt_ranges

                                ir_documenttype        TYPE tt_ranges
                                ir_customer            TYPE tt_ranges
                                ir_supplier            TYPE tt_ranges

                                ir_documentitem        TYPE tt_ranges

                                ir_documentsource      TYPE tt_ranges
                                ir_fiscalyearsource    TYPE tt_ranges
                                ir_fiscalperiod        TYPE tt_ranges
                                ir_transactioncurrency TYPE tt_ranges

                                wa_page_info           TYPE st_page_info

                      ,

      "Method get BusinessPartner info
      get_businesspartner_details IMPORTING i_document  TYPE zst_document_info OPTIONAL
                                  EXPORTING o_BPdetails TYPE zst_businesspartner_info,

      "Method get Company Code info
      get_companycode_details IMPORTING i_companycode TYPE bukrs
                              EXPORTING o_companycode TYPE zst_companycode_info,

      "Method get Address ID
      get_address_id_details IMPORTING AddressID          TYPE ad_addrnum
                             EXPORTING o_addressiddetails TYPE zst_addresId_info,

      "Method get GLAccount Details
      get_GLAccount_Details IMPORTING GLAccount   TYPE hkont
                                      CompanyCode TYPE bukrs
                            EXPORTING o_GLAccount TYPE zst_GLAccount_info,

      "Method get Số dư đầu kỳ/Cuối kỳ GLaccount
      get_GLaccount_Balance IMPORTING ir_companycode TYPE tt_ranges
                                      ir_GLaccount   TYPE tt_ranges
                                      ir_date        TYPE tt_ranges

                            EXPORTING o_startbalance TYPE tt_balance
                                      o_endbalance   TYPE tt_balance
                            ,

      get_cfid IMPORTING g_accountingdocheader TYPE z_fins_acdoc_header_in
                         g_accoutingdocitem    TYPE z_fins_acdoc_item_in
               EXPORTING o_cfid                TYPE ztb_cfid_temp.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_jp_common_core IMPLEMENTATION.


  METHOD get_address_id_details.

    "Customer Address
    SELECT addresseefullname ,                          "#EC CI_NOFIELD
     housenumber ,
     streetname ,
     streetprefixname1 ,
     streetprefixname2 ,
     streetsuffixname1 ,
     streetsuffixname2 ,
     districtname ,
     cityname ,
     country
     FROM i_address_2
     WITH PRIVILEGED ACCESS
     WHERE addressid = @addressid ORDER BY PRIMARY KEY
     INTO TABLE @DATA(lt_address_2) .

    READ TABLE lt_address_2 INTO DATA(ls_address_2) INDEX 1.

    o_addressiddetails-addressname = ls_address_2-AddresseeFullName.
    o_addressiddetails-address =
    |{ ls_address_2-StreetName }, { ls_address_2-StreetPrefixName1 }, { ls_address_2-StreetPrefixName2 }, { ls_address_2-StreetSuffixName1 }, { ls_address_2-DistrictName }, { ls_address_2-CityName }|.

    REPLACE ALL OCCURRENCES OF |, , , , , ,| IN o_addressiddetails-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, , , , ,| IN o_addressiddetails-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, , , ,| IN o_addressiddetails-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, , ,| IN o_addressiddetails-address WITH |,|.
    REPLACE ALL OCCURRENCES OF |, ,| IN o_addressiddetails-address WITH |,|.


    SHIFT o_addressiddetails-address LEFT DELETING LEADING ','.
    SHIFT o_addressiddetails-address RIGHT DELETING TRAILING ','.
    SHIFT o_addressiddetails-address LEFT DELETING LEADING ''.

    "Customer Email
    SELECT SINGLE EmailAddress FROM I_AddressEmailAddress_2 "#EC CI_NOFIELD
    WITH PRIVILEGED ACCESS
    WHERE AddressID = @o_addressiddetails-addressid
    INTO @o_addressiddetails-emailaddress
    .

    "Customer Telephone
    SELECT SINGLE PhoneExtensionNumber FROM I_AddressPhoneNumber_2 "#EC CI_NOFIELD
    WITH PRIVILEGED ACCESS
    WHERE AddressID = @o_addressiddetails-addressid
    INTO @o_addressiddetails-telephonenumber
    .

  ENDMETHOD.


  METHOD get_businesspartner_details.

    CLEAR: o_BPdetails.

    DATA: lv_url TYPE string VALUE IS INITIAL. "API read BP Details
    DATA: lv_country TYPE land1 VALUE IS INITIAL.

    "--- Kiểm tra mã Business partner có phải là khách vãng lai - onetime?
    IF i_document-customer IS NOT INITIAL.
      SELECT SINGLE isonetimeaccount FROM i_customer WHERE Customer = @i_document-customer "#EC CI_NOFIELD
        INTO @DATA(lv_xcpdk).
    ELSE.
      SELECT SINGLE isonetimeaccount FROM I_Supplier WHERE Customer = @i_document-supplier "#EC CI_NOFIELD
        INTO @lv_xcpdk.
    ENDIF.

    IF sy-subrc NE 0.
      CLEAR: lv_xcpdk.
    ENDIF.

    IF lv_xcpdk IS NOT INITIAL.
      SELECT SINGLE                                     "#EC CI_NOFIELD
            businesspartnername1 AS name1,
            businesspartnername2 AS name2,
            businesspartnername3 AS name3,
            businesspartnername4 AS name4,
            streetaddressname AS stras,
            cityname AS ort01,
            taxid1 AS stcd1,
            accountingclerkinternetaddress AS intad,
            Country AS land1
        FROM i_onetimeaccountcustomer
        WHERE accountingdocument = @i_document-accountingdocument AND
              companycode        = @i_document-companycode AND
              fiscalyear         = @i_document-fiscalyear
        INTO @DATA(ls_bsec).

      IF sy-subrc EQ 0. "Nếu Mã khách lẻ

        o_BPdetails-BPname = |{ ls_bsec-name2 } { ls_bsec-name3 } { ls_bsec-name4 } | .
        IF ls_bsec-name2 IS INITIAL AND ls_bsec-name3 IS INITIAL AND ls_bsec-name4 IS INITIAL.
          o_BPdetails-BPname = ls_bsec-name1 .
        ENDIF.
        o_BPdetails-BPaddress = |{ ls_bsec-stras }{ ls_bsec-ort01 }| .
        o_BPdetails-identificationnumber  = ls_bsec-stcd1.
        o_BPdetails-emailaddress = ls_bsec-intad.
        "Country
        lv_country = ls_bsec-land1.
      ENDIF.

    ELSE. "Trường hợp Businesspartner ko phải là khách vãng lai

      IF i_document-customer IS NOT INITIAL.
        READ TABLE gt_businesspartner_info INTO o_BPdetails WITH KEY bpnumber = i_document-customer BINARY SEARCH.
      ELSE.
        READ TABLE gt_businesspartner_info INTO o_BPdetails WITH KEY bpnumber = i_document-supplier BINARY SEARCH.
      ENDIF.

      IF sy-subrc NE 0.
        DATA(lv_index) = sy-index.

        IF i_document-customer IS NOT INITIAL.
          SELECT SINGLE cus~Customer AS BPnumber,       "#EC CI_NOFIELD
                        cus~addressid,
                        cus~VATRegistration,
                        cus~isonetimeaccount,
                        cus~createdbyuser,
                        cus~creationdate,
                        bp~creationtime
          FROM i_customer AS cus
          INNER JOIN i_businesspartner AS bp ON cus~Customer = bp~BusinessPartner
          WHERE cus~Customer = @i_document-customer
          INTO CORRESPONDING FIELDS OF @o_BPdetails
          .
        ELSE.
          SELECT SINGLE Supp~Supplier AS BPnumber,      "#EC CI_NOFIELD
                        Supp~addressid,
                        Supp~VATRegistration,
                        Supp~isonetimeaccount,
                        Supp~createdbyuser,
                        Supp~creationdate,
                        bp~creationtime
          FROM I_Supplier AS Supp
          INNER JOIN i_businesspartner AS bp ON Supp~Supplier = bp~BusinessPartner
          WHERE Supp~Supplier = @i_document-supplier
          INTO CORRESPONDING FIELDS OF @o_BPdetails
          .
        ENDIF.


        "Customer Identification Number
        SELECT SINGLE BPIdentificationNumber FROM I_BuPaIdentification "#EC CI_NOFIELD
        INTO @o_BPdetails-identificationnumber
*       WHERE BPIdentificationType = ?
        .

        "Customer Address
        zcl_jp_common_core=>get_address_id_details(
            EXPORTING
            addressid = o_BPdetails-addressid
            IMPORTING
            o_addressiddetails = DATA(ls_addressid)
        ).
**-----------------------------------------------------------------------**
        o_BPdetails-BPname          = ls_addressid-addressname.
        o_BPdetails-BPaddress       = ls_addressid-address.
        o_BPdetails-emailaddress    = ls_addressid-emailaddress.
        o_BPdetails-telephonenumber = ls_addressid-telephonenumber.

        INSERT o_BPdetails INTO TABLE gt_businesspartner_info.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_companycode_details.

    SELECT SINGLE                                       "#EC CI_NOFIELD
              companycode,
              CompanyCodeName,
              addressid,
              vatregistration,
              Currency,
              Country
*              createdbyuser,
*              creationdate,
*              creationtime
    FROM I_CompanyCode
    WHERE CompanyCode = @i_companycode
    INTO @DATA(ls_companycode)
    .                                                   "#EC CI_NOFIELD

    MOVE-CORRESPONDING ls_companycode TO o_companycode.

    zcl_jp_common_core=>get_address_id_details(
        EXPORTING
        addressid = o_companycode-addressid
        IMPORTING
        o_addressiddetails = DATA(ls_addressiddetails)
    ).

*    o_companycode-companycodename = ls_addressiddetails-addressname.
    IF ls_companycode-Country = 'VN'.
      ls_addressiddetails-address = |{ ls_addressiddetails-address }, Việt Nam|.
    ELSE.
      SELECT SINGLE CountryName FROM I_CountryText
      WHERE Country = @ls_companycode-Country
      INTO @DATA(lv_countryname).
      IF sy-subrc = 0.
        ls_addressiddetails-address = |{ ls_addressiddetails-address }, { lv_countryname }|.
      ENDIF.
    ENDIF.

    o_companycode-companycodeaddr = ls_addressiddetails-address.

  ENDMETHOD.


  METHOD get_fillter_app.

    wa_page_info-paging            = io_request->get_paging( ).

    wa_page_info-page_size         = io_request->get_paging( )->get_page_size( ).

    wa_page_info-offset            = io_request->get_paging( )->get_offset( ).

    wa_page_info-requested_fields  = io_request->get_requested_elements( ).

    wa_page_info-sort_order        = io_request->get_sort_elements( ).

    wa_page_info-ro_filter         = io_request->get_filter( ).

    wa_page_info-entity_id         = io_request->get_entity_id( ).

    TRY.
        DATA(lr_ranges) = wa_page_info-ro_filter->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
        "handle exception
    ENDTRY.

    DATA: ls_postingdate LIKE LINE OF ir_postingdate.

    LOOP AT lr_ranges INTO DATA(ls_ranges).
      CASE ls_ranges-name.
        WHEN 'COMPANYCODE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_companycode.
        WHEN 'ACCOUNTINGDOCUMENT'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_accountingdocument.
        WHEN 'FISCALYEAR'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_fiscalyear.
        WHEN 'ACCOUNTINGDOCUMENTITEM'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_buzei.
        WHEN 'CASHACCOUNTING' OR 'GLACCOUNT'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_glaccount.
        WHEN 'POSTINGDATE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_postingdate.
        WHEN 'DOCUMENTDATE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_documentdate.
        WHEN 'EINVOICENUMBER'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_einvoicenumber.
        WHEN 'EINVOICETYPE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_einvoicetype.
        WHEN 'CURRENCYTYPE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_currencytype.
        WHEN 'STATUSSAP'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_statussap.
        WHEN 'TYPEOFDATE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_typeofdate.
        WHEN 'USERTYPE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_usertype.
        WHEN 'CREATEDBYUSER'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_createdbyuser.
        WHEN 'ENDUSER'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_enduser.
        WHEN 'TESTRUN'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_testrun.
        WHEN 'DOITUONG'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_businesspartner.
        WHEN 'POSTINGDATEFROM'.
          READ TABLE ls_ranges-range INTO DATA(ls_range) INDEX 1.
          ls_postingdate-low = ls_range-low.
        WHEN 'POSTINGDATETO'.
          READ TABLE ls_ranges-range INTO ls_range INDEX 1.
          ls_postingdate-high = ls_range-low.
        WHEN 'ACCOUNTINGDOCUMENTTYPE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_documenttype.
        WHEN 'CUSTOMER'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_customer.
        WHEN 'SUPPLIER'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_supplier.
        WHEN 'LEGDERGLITEM'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_documentitem.
        WHEN 'TRANSACTIONCURRENCY'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_transactioncurrency.
        WHEN 'FISCALPERIOD'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_fiscalperiod.
        WHEN 'ACCOUTINGDOCUMENTSOURCE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_documentsource.
        WHEN 'FISCALYEARSOURCE'.
          MOVE-CORRESPONDING ls_ranges-range TO ir_fiscalyearsource.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF ls_postingdate-high IS NOT INITIAL.
      ls_postingdate-option = 'BT'.
    ELSE.
      ls_postingdate-option = 'EQ'.
    ENDIF.

    ls_postingdate-sign = 'I'.
    IF ls_postingdate-low IS NOT INITIAL OR ls_postingdate-high IS NOT INITIAL.
      APPEND ls_postingdate TO ir_postingdate.
    ENDIF.

  ENDMETHOD.


  METHOD get_GLaccount_Balance.

    FREE: o_endbalance, o_startbalance.

    DATA: lv_startdate TYPE budat VALUE IS INITIAL,
          lv_enddate   TYPE budat VALUE IS INITIAL.

    DATA: ls_balance TYPE ty_Balance.

    READ TABLE ir_date INTO DATA(ls_date) INDEX 1.

    lv_startdate = ls_date-low.
    lv_enddate = ls_date-high.

    IF lv_startdate IS NOT INITIAL.

      SELECT
          CompanyCode,
          GLAccount,
          DebitCreditCode,
          CompanyCodeCurrency,
          TransactionCurrency,
       SUM( AmountInCompanyCodeCurrency ) AS AmountinCompanyCode ,
       SUM( AmountInTransactionCurrency ) AS AmountinTransaction
      FROM I_GLAccountLineItem
      WHERE CompanyCode        IN @ir_companycode
        AND GLAccount          IN @ir_glaccount
        AND PostingDate        LT @lv_startdate
        AND Ledger = '0L'
      GROUP BY CompanyCode, GLAccount, DebitCreditCode, CompanyCodeCurrency, TransactionCurrency
      INTO TABLE @DATA(lt_startbalance)
     .

      LOOP AT lt_startbalance INTO DATA(ls_startbalance).
        MOVE-CORRESPONDING ls_startbalance TO ls_balance.
        COLLECT ls_balance INTO o_startbalance.
        CLEAR: ls_balance.
      ENDLOOP.

    ENDIF.

    IF lv_enddate IS NOT INITIAL.

      SELECT
           CompanyCode,
           GLAccount,
           DebitCreditCode,
           CompanyCodeCurrency,
           TransactionCurrency,
       SUM( AmountInCompanyCodeCurrency ) AS AmountinCompanyCode ,
       SUM( AmountInTransactionCurrency ) AS AmountinTransaction
       FROM I_GLAccountLineItem
       WHERE CompanyCode        IN @ir_companycode
         AND GLAccount          IN @ir_glaccount
         AND PostingDate        LE @lv_enddate
         AND Ledger = '0L'
       GROUP BY CompanyCode, GLAccount, DebitCreditCode, CompanyCodeCurrency, TransactionCurrency
       INTO TABLE @DATA(lt_endbalance)
      .

      LOOP AT lt_endbalance INTO DATA(ls_endbalance).
        MOVE-CORRESPONDING ls_endbalance TO ls_balance.
        COLLECT ls_balance INTO o_endbalance.
        CLEAR: ls_balance.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD get_glaccount_details.
    CLEAR: o_glaccount.

    SELECT SINGLE a~glaccount, b~GLAccountName, a~GLAccountCurrency, a~createdbyuser, a~creationdate
    FROM I_GLAccountInCompanyCode AS a INNER JOIN I_GlAccountTextInCompanycode AS b
    ON a~GLAccount = b~GLAccount
    AND a~CompanyCode = b~CompanyCode
    WHERE a~GLAccount = @glaccount
    AND a~CompanyCode = @companycode
    INTO CORRESPONDING FIELDS OF @o_glaccount
    .

  ENDMETHOD.


  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                           THEN mo_instance
                                           ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
*    SELECT * FROM ztb_cfid_temp INTO TABLE @DATA(lt_delete).
*    DELETE ztb_cfid_temp FROM TABLE @lt_delete.

*    DATA(o_firu_cash) = new cl_firu_cash_flow_off_acc( ).

  ENDMETHOD.

  METHOD get_cfid.
    CLEAR: o_cfid.
    DATA: ls_cfid_temp TYPE ztb_cfid_temp.

    SELECT * FROM ztb_cfid_temp
    WHERE zcount < 2
    INTO TABLE @DATA(lt_cfid_temp).

    SORT lt_cfid_temp BY documentitem ASCENDING.

    IF g_accountingdocheader-transactioncode = 'FBCJ'.
      READ TABLE lt_cfid_temp INTO ls_cfid_temp INDEX 1.
      IF sy-subrc EQ 0.
        ls_cfid_temp-zcount = ls_cfid_temp-zcount + 1.
        IF ls_cfid_temp-ztype = 'CashPayments' AND g_accoutingdocitem-debitcreditcode = 'H'.
          o_cfid = ls_cfid_temp.
          DELETE ztb_cfid_temp FROM @ls_cfid_temp.
        ELSEIF ls_cfid_temp-ztype = 'CashReceipts' AND g_accoutingdocitem-debitcreditcode = 'S'.
          o_cfid = ls_cfid_temp.
          DELETE ztb_cfid_temp FROM @ls_cfid_temp.
        ENDIF.
      ENDIF.

      IF ls_cfid_temp-zcount = 2.
        DELETE ztb_cfid_temp FROM @ls_cfid_temp.
      ENDIF.
    ELSEIF g_accountingdocheader-transactioncode = 'FB70'.

      READ TABLE lt_cfid_temp INTO ls_cfid_temp INDEX 1.
      IF sy-subrc EQ 0.
        IF g_accoutingdocitem-financialaccounttype = 'D'.
          o_cfid = ls_cfid_temp.
          DELETE ztb_cfid_temp FROM @ls_cfid_temp.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
