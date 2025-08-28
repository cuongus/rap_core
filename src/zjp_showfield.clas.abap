CLASS zjp_showfield DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZJP_SHOWFIELD IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.

    CASE iv_entity.

      WHEN 'ZJP_C_SOQUYTIENMAT'.

      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
