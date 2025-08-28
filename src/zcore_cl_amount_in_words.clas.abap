class zcore_cl_amount_in_words definition
  public
  final
  create public .

  public section.
    interfaces if_oo_adt_classrun.
    interfaces if_http_service_extension.
    constants:
      method_post type string value 'POST',
      method_get  type string value 'GET'.
    CLASS-METHODS:   read_amount IMPORTING i_amount          TYPE fins_vwcur12
                                           i_waers           TYPE waers
                                           i_lang            TYPE any DEFAULT 'VI'
                                 RETURNING VALUE(e_in_words) TYPE string.

  protected section.
  private section.
    types:
      begin of ts_get_request,
        amount type string,
        waers  type string,
        lang   type string,
      end of ts_get_request,
      begin of ts_get_response,
        result type string,
      end of ts_get_response.
    data:
      request_method type string,
      request_body   type string,
      response_body  type string,
      request_data   type ts_get_request,
      response_data  type ts_get_response.

    class-methods:
      spell_amount_vi importing number        type any
                      returning value(result) type string,
      spell_amount_en importing number        type any
                      returning value(result) type string,
      read_single_number importing number        type any
                                   lang          type any
                         returning value(result) type string.
ENDCLASS.



CLASS ZCORE_CL_AMOUNT_IN_WORDS IMPLEMENTATION.


  method if_http_service_extension~handle_request.
    request_method = request->get_header_field( i_name = '~request_method' ).
    request_body = request->get_text( ).

    case request_method.
      when method_get.
      when method_post.
        data:
          i_waers  type waers,
          i_amount type fins_vwcur12,
          i_langu  type string.

        try.
            xco_cp_json=>data->from_string( request_body )->apply( value #(
          ( xco_cp_json=>transformation->camel_case_to_underscore )
          ( xco_cp_json=>transformation->boolean_to_abap_bool ) )
           )->write_to( ref #( request_data ) ).

            i_waers  = request_data-waers.
            i_amount = request_data-amount.
            i_langu  = request_data-lang.
            response_data-result = me->read_amount( i_waers  = i_waers
                                                    i_amount = i_amount
                                                    i_lang   = i_langu ).

            response_body = xco_cp_json=>data->from_abap( response_data )->apply( value #(
            ( xco_cp_json=>transformation->underscore_to_pascal_case )
            ) )->to_string( ).

            response->set_text( i_text = response_body ).
          catch cx_root into data(lx_root).
            response->set_text( i_text = |{ lx_root->get_longtext( ) }| ).
            return.
        endtry.

    endcase.

  endmethod.


  method if_oo_adt_classrun~main.
    data: lt_amount type table of fins_vwcur12.

    append '1.11' to lt_amount.
    append '1.10' to lt_amount.
    append '1.01' to lt_amount.
    append '41.11' to lt_amount.
    append '111' to lt_amount.
    append '101' to lt_amount.
    append '100' to lt_amount.
    append '1111' to lt_amount.
    append '1101' to lt_amount.
    append '1011' to lt_amount.
    append '1001' to lt_amount.
    append '1000' to lt_amount.
    append '1060' to lt_amount.
    append '100000' to lt_amount.
    append '909099009991' to lt_amount.

    loop at lt_amount into data(lv_amount).
      out->write( |{ lv_amount }| ).
      out->write( |{ zcore_cl_amount_in_words=>read_amount( i_amount = lv_amount i_waers = 'VND' i_lang = 'VI' ) }| ).
      out->write( |{ zcore_cl_amount_in_words=>read_amount( i_amount = lv_amount i_waers = 'VND' i_lang = 'EN' ) }| ).
      out->write( |{ zcore_cl_amount_in_words=>read_amount( i_amount = lv_amount i_waers = 'USD' i_lang = 'VI' ) }| ).
      out->write( |{ zcore_cl_amount_in_words=>read_amount( i_amount = lv_amount i_waers = 'USD' i_lang = 'EN' ) }| ).
    endloop.
  endmethod.


  method read_amount.
    data: lv_ext_num         type string,
          lv_decimal         type c,
          lv_integer_part    type string,
          lv_fractional_part type string.

    data: lv_amount_temp    type n length 20,
          lv_integ_part_str type string,
          lv_fract_part_str type string,
          lv_off            type int4.

    data: lv_first_char type c length 1.

    lv_ext_num = i_amount.
    condense lv_ext_num.

    case i_lang.
      when 'VI'.
        if lv_ext_num = '0.00' or lv_ext_num is initial.
          e_in_words = 'Không'.
        else.
          lv_decimal = '.'.
          split lv_ext_num at lv_decimal into lv_integer_part lv_fractional_part.

*          process integer part
          condense lv_integer_part.
          lv_amount_temp = lv_integer_part.
          if lv_amount_temp > 0.
            lv_integ_part_str = spell_amount_vi( exporting number = lv_integer_part ).
          else.
            clear: lv_integ_part_str.
          endif.

*          process fractional part
          condense lv_fractional_part.
          lv_amount_temp = lv_fractional_part.
          if lv_amount_temp > 0.
            lv_fract_part_str = spell_amount_vi( exporting number = lv_amount_temp ).
            if i_waers = 'VND'.
              do strlen( lv_fractional_part ) times.
                if lv_fractional_part+lv_off(1) eq '0'.
                  concatenate 'không' lv_fract_part_str into lv_fract_part_str separated by space.
                else.
                  exit.
                endif.
                lv_off += 1.
              enddo.
              clear: lv_off.
            endif.
          else.
            clear: lv_fract_part_str.
          endif.

          if lv_integ_part_str is not initial and
             lv_fract_part_str is not initial.
            e_in_words = |{ lv_integ_part_str } lẻ { lv_fract_part_str }|.
          elseif lv_integ_part_str is not initial.
            e_in_words = |{ lv_integ_part_str }|.
          else.
            e_in_words = |không lẻ { lv_fract_part_str }|.
          endif.
        endif.
        if i_waers = 'VND'.
          e_in_words = |{ e_in_words } đồng|.
        elseif i_waers = 'USD'.
          e_in_words = |{ e_in_words } đô la Mỹ|.
        endif.
      when 'EN'.
        if lv_ext_num = '0.00' or lv_ext_num is initial.
          if i_waers = 'VND'.
            e_in_words = 'zero Vietnamese dong'.
          elseif i_waers = 'USD'.
            e_in_words = 'zero dollar'.
          endif.
        else.
          lv_decimal = '.'.
          split lv_ext_num at lv_decimal into lv_integer_part lv_fractional_part.

*          process integer part
          condense lv_integer_part.
          lv_amount_temp = lv_integer_part.
          if lv_amount_temp > 0.
            lv_integ_part_str = spell_amount_en( exporting number = lv_integer_part ).
          else.
            clear: lv_integ_part_str.
          endif.

*          process fractional part
          condense lv_fractional_part.
          lv_amount_temp = lv_fractional_part.
          if lv_amount_temp > 0.
            lv_fract_part_str = spell_amount_en( exporting number = lv_amount_temp ).
            if i_waers = 'VND'.
              do strlen( lv_fractional_part ) times.
                if lv_fractional_part+lv_off(1) eq '0'.
                  concatenate 'zero' lv_fract_part_str into lv_fract_part_str separated by space.
                else.
                  exit.
                endif.
                lv_off += 1.
              enddo.
              clear: lv_off.
            endif.
          else.
            clear: lv_fract_part_str.
          endif.

          if lv_integ_part_str is not initial and
             lv_fract_part_str is not initial.
            if i_waers = 'VND'.
              e_in_words = |{ lv_integ_part_str } point { lv_fract_part_str } Vietnamese dong|.
            elseif i_waers = 'USD'.
              if lv_integ_part_str eq 'one' and
                 lv_fract_part_str eq 'one'.
                e_in_words = |{ lv_integ_part_str } dollar and { lv_fract_part_str } cent|.
              elseif lv_integ_part_str eq 'one'.
                e_in_words = |{ lv_integ_part_str } dollar and { lv_fract_part_str } cents|.
              elseif lv_fract_part_str eq 'one'.
                e_in_words = |{ lv_integ_part_str } dollars and { lv_fract_part_str } cent|.
              else.
                e_in_words = |{ lv_integ_part_str } dollars and { lv_fract_part_str } cents|.
              endif.
            endif.
          elseif lv_integ_part_str is not initial.
            if i_waers = 'VND'.
              e_in_words = |{ lv_integ_part_str } Vietnamese dong|.
            elseif i_waers = 'USD'.
              if lv_integ_part_str eq 'one'.
                e_in_words = |{ lv_integ_part_str } dollar|.
              else.
                e_in_words = |{ lv_integ_part_str } dollars|.
              endif.
            endif.
          else.
            if i_waers = 'VND'.
              e_in_words = |zero point { lv_fract_part_str } Vietnamese dong|.
            elseif i_waers = 'USD'.
              if lv_fract_part_str eq 'one'.
                e_in_words = |{ lv_fract_part_str } cent|.
              else.
                e_in_words = |{ lv_fract_part_str } cents|.
              endif.
            endif.
          endif.
        endif.
    endcase.

*    capitalize first character
    condense e_in_words.
    lv_first_char = e_in_words(1).
    translate lv_first_char to upper case.
    e_in_words = |{ lv_first_char }{ substring( val = e_in_words off = 1 len = strlen( e_in_words ) - 1 ) }|.
  endmethod.


  method read_single_number.
    case lang.
      when 'VI'.
        case number.
          when '0'.
            result = ''.
          when '1'.
            result = 'một'.
          when '2'.
            result = 'hai'.
          when '3'.
            result = 'ba'.
          when '4'.
            result = 'bốn'.
          when '5'.
            result = 'năm'.
          when '6'.
            result = 'sáu'.
          when '7'.
            result = 'bảy'.
          when '8'.
            result = 'tám'.
          when '9'.
            result = 'chín'.
          when '10'.
            result = 'mười'.
          when '11'.
            result = 'mười một'.
          when '12'.
            result = 'mười hai'.
          when '13'.
            result = 'mười ba'.
          when '14'.
            result = 'mười bốn'.
          when '15'.
            result = 'mười lăm'.
          when '16'.
            result = 'mười sáu'.
          when '17'.
            result = 'mười bảy'.
          when '18'.
            result = 'mười tám'.
          when '19'.
            result = 'mười chín'.
          when '20'.
            result = 'hai mươi'.
          when '30'.
            result = 'ba mươi'.
          when '40'.
            result = 'bốn mươi'.
          when '50'.
            result = 'năm mươi'.
          when '60'.
            result = 'sáu mươi'.
          when '70'.
            result = 'bảy mươi'.
          when '80'.
            result = 'tám mươi'.
          when '90'.
            result = 'chín mươi'.
        endcase.
      when 'EN'.
        case number.
          when '0'.
            result = ''.
          when '1'.
            result = 'one'.
          when '2'.
            result = 'two'.
          when '3'.
            result = 'three'.
          when '4'.
            result = 'four'.
          when '5'.
            result = 'five'.
          when '6'.
            result = 'six'.
          when '7'.
            result = 'seven'.
          when '8'.
            result = 'eight'.
          when '9'.
            result = 'nine'.
          when '10'.
            result = 'ten'.
          when '11'.
            result = 'eleven'.
          when '12'.
            result = 'twelve'.
          when '13'.
            result = 'thirteen'.
          when '14'.
            result = 'fourteen'.
          when '15'.
            result = 'fifteen'.
          when '16'.
            result = 'sixteen'.
          when '17'.
            result = 'seventeen'.
          when '18'.
            result = 'eighteen'.
          when '19'.
            result = 'nineteen'.
          when '20'.
            result = 'twenty'.
          when '30'.
            result = 'thirty'.
          when '40'.
            result = 'forty'.
          when '50'.
            result = 'fifty'.
          when '60'.
            result = 'sixty'.
          when '70'.
            result = 'seventy'.
          when '80'.
            result = 'eighty'.
          when '90'.
            result = 'ninety'.
        endcase.
    endcase.
  endmethod.


  method spell_amount_en.
    data: lv_amount1 type int4,
          lv_amount2 type int4,
          lv_return  type string,
          lv_return1 type string,
          lv_return2 type string.
    if number = 0.
      "lv_return = 'zero'.
    elseif number < 20.
      lv_return = read_single_number( exporting number = number
                                                lang   = 'EN' ).
      "return units[number]
    elseif number < 100.
      lv_amount1 = number div 10.
      lv_amount2 = number mod 10.
      lv_amount1 = lv_amount1 * 10.
      lv_return1 = read_single_number( exporting number = lv_amount1
                                                 lang   = 'EN' ).
      lv_return2 = read_single_number( exporting number = lv_amount2
                                                 lang   = 'EN' ).
      concatenate lv_return1 lv_return2 into lv_return separated by space.
      "return tens[number // 10] + (" " + units[number % 10] if number % 10 != 0 else "")
    elseif number < 1000.
      lv_amount1 = number div 100.
      lv_amount2 = number mod 100.
      lv_return1 = read_single_number( exporting number = lv_amount1
                                                 lang   = 'EN' ).
      lv_return2 = spell_amount_en( exporting number = lv_amount2 ).
      concatenate lv_return1 'hundred' lv_return2 into lv_return separated by space.
      "return units[number // 100] + " hundred" + (" and " + number_to_words(number % 100) if number % 100 != 0 else "")
    elseif number < 1000000.
      lv_amount1 = number div 1000.
      lv_amount2 = number mod 1000.
      lv_return1 = spell_amount_en( exporting number = lv_amount1 ).
      lv_return2 = spell_amount_en( exporting number = lv_amount2 ).
      concatenate lv_return1 'thousand' lv_return2 into lv_return separated by space.
      "return number_to_words(number // 1000) + " thousand" + (" " + number_to_words(number % 1000) if number % 1000 != 0 else "")
    elseif number < 1000000000.
      lv_amount1 = number div 1000000.
      lv_amount2 = number mod 1000000.
      lv_return1 = spell_amount_en( exporting number = lv_amount1 ).
      lv_return2 = spell_amount_en( exporting number = lv_amount2 ).
      concatenate lv_return1 'million' lv_return2 into lv_return separated by space.
      "return number_to_words(number // 1000000) + " million" + (" " + number_to_words(number % 1000000) if number % 1000000 != 0 else "")
    elseif number < 1000000000000.
      lv_amount1 = number div 1000000000.
      lv_amount2 = number mod 1000000000.
      lv_return1 = spell_amount_en( exporting number = lv_amount1 ).
      lv_return2 = spell_amount_en( exporting number = lv_amount2 ).
      concatenate lv_return1 'billion' lv_return2 into lv_return separated by space.
      "return number_to_words(number // 1000000000) + " billion" + (" " + number_to_words(number % 1000000000) if number % 1000000000 != 0 else "")
    else.
      lv_return = 'Number out of range'.
      "return "Number out of range"
    endif.

    return lv_return.
  endmethod.


  method spell_amount_vi.
    data: lv_amount1 type int4,
          lv_amount2 type int4,
          lv_return  type string,
          lv_return1 type string,
          lv_return2 type string,
          lv_string  type string,
          lv_off     type int4.
    lv_string = number.
    condense lv_string.

    if number = 0.
      "lv_return = 'không'.
    elseif number < 20.
      lv_return = read_single_number( exporting number = number
                                                lang   = 'VI' ).
    elseif number < 100 and lv_string+1(1) eq '1'.
      lv_return = read_single_number( exporting number = number - 1
                                                lang   = 'VI' ).
      concatenate lv_return 'mốt' into lv_return separated by space.
    elseif number < 100.
      lv_amount1 = number div 10.
      lv_amount2 = number mod 10.
      lv_amount1 = lv_amount1 * 10.
      lv_return1 = read_single_number( exporting number = lv_amount1
                                                 lang   = 'VI' ).
      lv_return2 = read_single_number( exporting number = lv_amount2
                                                 lang   = 'VI' ).
      concatenate lv_return1 lv_return2 into lv_return separated by space.
      "return tens[number // 10] + (" " + units[number % 10] if number % 10 != 0 else "")
    elseif number < 1000.
      lv_amount1 = number div 100.
      lv_amount2 = number mod 100.
      lv_return1 = read_single_number( exporting number = lv_amount1
                                                 lang   = 'VI' ).
      lv_return2 = spell_amount_vi( exporting number = lv_amount2 ).
      if lv_string+1(1) eq '0' and
         lv_string+2(1) ne '0'.
        concatenate lv_return1 'trăm linh' lv_return2 into lv_return separated by space.
      else.
        concatenate lv_return1 'trăm' lv_return2 into lv_return separated by space.
      endif.
      "return units[number // 100] + " trăm" + (" and " + number_to_words(number % 100) if number % 100 != 0 else "")
    elseif number < 1000000.
      lv_amount1 = number div 1000.
      lv_amount2 = number mod 1000.
      lv_return1 = spell_amount_vi( exporting number = lv_amount1 ).
      lv_return2 = spell_amount_vi( exporting number = lv_amount2 ).
      lv_off = strlen( lv_string ) - 3.
      if lv_string+lv_off(1) eq '0'.
        lv_off += 1.
        if lv_string+lv_off(1) eq '0'.
          lv_off += 1.
          if lv_string+lv_off(1) eq '0'.
            concatenate lv_return1 'nghìn' lv_return2 into lv_return separated by space.
          else.
            concatenate lv_return1 'nghìn không trăm linh' lv_return2 into lv_return separated by space.
          endif.
        else.
          concatenate lv_return1 'nghìn không trăm' lv_return2 into lv_return separated by space.
        endif.
      else.
        concatenate lv_return1 'nghìn' lv_return2 into lv_return separated by space.
      endif.
      "return number_to_words(number // 1000) + " nghìn" + (" " + number_to_words(number % 1000) if number % 1000 != 0 else "")
    elseif number < 1000000000.
      lv_amount1 = number div 1000000.
      lv_amount2 = number mod 1000000.
      lv_return1 = spell_amount_vi( exporting number = lv_amount1 ).
      lv_return2 = spell_amount_vi( exporting number = lv_amount2 ).
      concatenate lv_return1 'triệu' lv_return2 into lv_return separated by space.
      "return number_to_words(number // 1000000) + " triệu" + (" " + number_to_words(number % 1000000) if number % 1000000 != 0 else "")
    elseif number < 1000000000000.
      lv_amount1 = number div 1000000000.
      lv_amount2 = number mod 1000000000.
      lv_return1 = spell_amount_vi( exporting number = lv_amount1 ).
      lv_return2 = spell_amount_vi( exporting number = lv_amount2 ).
      concatenate lv_return1 'tỷ' lv_return2 into lv_return separated by space.
      "return number_to_words(number // 1000000000) + " tỷ" + (" " + number_to_words(number % 1000000000) if number % 1000000000 != 0 else "")
    else.
      lv_return = 'Số tiền quá lớn'.
      "return "Number out of range"
    endif.

    return lv_return.
  endmethod.
ENDCLASS.
