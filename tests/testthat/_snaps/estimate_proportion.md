# Compare estimate_proportion to ESCI_One_Proportion example

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases  n         P      P_LL
      1   My outcome variable               Affected     8 22 0.3636364 0.1976126
      2   My outcome variable           Not Affected    14 22 0.6363636 0.4283818
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.5716182 0.09541133  0.3846154 0.2276777 0.5415531
      2 0.8023874 0.09541133  0.6153846 0.4584469 0.7723223
      
      -- es_proportion --
        outcome_variable_name  case_label   effect effect_size        LL        UL
      1   My outcome variable P_ Affected Affected   0.3636364 0.1976126 0.5716182
                SE effect_size_adjusted     ta_LL     ta_UL cases  n
      1 0.09541133            0.3846154 0.2276777 0.5415531     8 22
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Call estimate_proportion with vector

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases  n         P      P_LL
      1   My outcome variable              Depressed     8 22 0.3636364 0.1388521
      2   My outcome variable           NotDepressed    14 22 0.6363636 0.3696213
      3   My outcome variable                Missing     3 NA        NA        NA
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.6303787 0.09541133  0.3846154 0.1626554 0.6065753
      2 0.8611479 0.09541133  0.6153846 0.3934247 0.8373446
      3        NA         NA         NA        NA        NA
      
      -- es_proportion --
        outcome_variable_name   case_label    effect effect_size        LL        UL
      1   My outcome variable P_ Depressed Depressed   0.3636364 0.1388521 0.6303787
                SE effect_size_adjusted     ta_LL     ta_UL cases  n
      1 0.09541133            0.3846154 0.1626554 0.6065753     8 22
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Call estimate_proportion with dataframe

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = dep_data
      Outcome variable(s) = My outcome variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases  n         P      P_LL
      1     depression_status              Depressed     8 22 0.3636364 0.1388521
      2     depression_status           NotDepressed    14 22 0.6363636 0.3696213
      3     depression_status                Missing     3 NA        NA        NA
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.6303787 0.09541133  0.3846154 0.1626554 0.6065753
      2 0.8611479 0.09541133  0.6153846 0.3934247 0.8373446
      3        NA         NA         NA        NA        NA
      
      -- es_proportion --
        outcome_variable_name   case_label    effect effect_size        LL        UL
      1     depression_status P_ Depressed Depressed   0.3636364 0.1388521 0.6303787
                SE effect_size_adjusted     ta_LL     ta_UL cases  n
      1 0.09541133            0.3846154 0.1626554 0.6065753     8 22
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

