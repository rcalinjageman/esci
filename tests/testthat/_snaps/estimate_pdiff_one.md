# Compare estimate_pdiff_one to ESCI_One_Proportion example

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
      
      -- es_proportion_difference --
        outcome_variable_name  case_label                                effect
      1   My outcome variable P_ Affected                   My outcome variable
      2   My outcome variable P_ Affected                       Reference value
      3   My outcome variable P_ Affected My outcome variable ‒ Reference value
        effect_size         LL         UL         SE effect_size_adjusted      ta_LL
      1   0.3636364  0.1976126 0.57161816 0.09541133            0.3846154  0.2276777
      2   0.5000000         NA         NA         NA                   NA         NA
      3  -0.1363636 -0.3023874 0.07161816 0.09541133           -0.1153846 -0.2723223
             ta_UL cases  n       type
      1 0.54155306     8 22 Comparison
      2         NA     8 22  Reference
      3 0.04155306     8 22 Difference
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Call estimate_pdiff_one with vector

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = dep_status
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases  n         P      P_LL
      1            dep_status              Depressed     8 22 0.3636364 0.1388521
      2            dep_status           NotDepressed    14 22 0.6363636 0.3696213
      3            dep_status                Missing     3 NA        NA        NA
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.6303787 0.09541133  0.3846154 0.1626554 0.6065753
      2 0.8611479 0.09541133  0.6153846 0.3934247 0.8373446
      3        NA         NA         NA        NA        NA
      
      -- es_proportion_difference --
        outcome_variable_name   case_label                       effect effect_size
      1            dep_status P_ Depressed                   dep_status   0.3636364
      2            dep_status P_ Depressed              Reference value   0.0000000
      3            dep_status P_ Depressed dep_status ‒ Reference value   0.3636364
               LL        UL         SE effect_size_adjusted     ta_LL     ta_UL cases
      1 0.1388521 0.6303787 0.09541133            0.3846154 0.1626554 0.6065753     8
      2        NA        NA         NA                   NA        NA        NA     8
      3 0.1388521 0.6303787 0.09541133            0.3846154 0.1626554 0.6065753     8
         n       type
      1 22 Comparison
      2 22  Reference
      3 22 Difference
      [1] "Missing values were present; these were *not* counted as part of the total sample size."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Call estimate_proportion with dataframe

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = depression_status
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases  n         P      P_LL
      1     depression_status              Depressed     8 22 0.3636364 0.1388521
      2     depression_status           NotDepressed    14 22 0.6363636 0.3696213
      3     depression_status                Missing     3 NA        NA        NA
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.6303787 0.09541133  0.3846154 0.1626554 0.6065753
      2 0.8611479 0.09541133  0.6153846 0.3934247 0.8373446
      3        NA         NA         NA        NA        NA
      
      -- es_proportion_difference --
        outcome_variable_name   case_label                              effect
      1     depression_status P_ Depressed                   depression_status
      2     depression_status P_ Depressed                     Reference value
      3     depression_status P_ Depressed depression_status ‒ Reference value
        effect_size        LL        UL         SE effect_size_adjusted     ta_LL
      1   0.3636364 0.1388521 0.6303787 0.09541133            0.3846154 0.1626554
      2   0.0000000        NA        NA         NA                   NA        NA
      3   0.3636364 0.1388521 0.6303787 0.09541133            0.3846154 0.1626554
            ta_UL cases  n       type
      1 0.6065753     8 22 Comparison
      2        NA     8 22  Reference
      3 0.6065753     8 22 Difference
      [1] "Missing values were present; these were *not* counted as part of the total sample size."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

