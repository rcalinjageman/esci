# Compare estimate_pdiff_ind_contrast to ESCI_Two_proportions example

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        grouping_variable_name grouping_variable_level outcome_variable_name
      1   My grouping variable                Original   My outcome variable
      2   My grouping variable                Original   My outcome variable
      3   My grouping variable             Replication   My outcome variable
      4   My grouping variable             Replication   My outcome variable
        outcome_variable_level cases   n         P      P_LL      P_UL       P_SE
      1             egocentric    78 252 0.3095238 0.2557208 0.3692792 0.02896953
      2         Not egocentric   174 252 0.6904762 0.6307208 0.7442792 0.02896953
      3             egocentric    10  20 0.5000000 0.2999620 0.7000380 0.10206207
      4         Not egocentric    10  20 0.5000000 0.2999620 0.7000380 0.10206207
        P_adjusted     ta_LL     ta_UL
      1     0.3125 0.2648494 0.3601506
      2     0.6875 0.6398494 0.7351506
      3     0.5000 0.3321228 0.6678772
      4     0.5000 0.3321228 0.6678772
      
      -- es_proportion_difference --
              type outcome_variable_name   case_label grouping_variable_name
      1 Comparison   My outcome variable P_egocentric   My grouping variable
      2  Reference   My outcome variable P_egocentric   My grouping variable
      3 Difference   My outcome variable P_egocentric   My grouping variable
                        effect effect_size          LL        UL         SE
      1            Replication   0.5000000  0.29996201 0.7000380 0.10206207
      2               Original   0.3095238  0.25572077 0.3692792 0.02896953
      3 Replication ‒ Original   0.1904762 -0.02757339 0.4055261 0.11048660
        effect_size_adjusted       ta_LL     ta_UL
      1            0.5000000 0.332122830 0.6678772
      2            0.3125000 0.264849371 0.3601506
      3            0.1889764 0.007242087 0.3707107
      
      -- es_odds_ratio --
        outcome_variable_name   case_label grouping_variable_name
      1   My outcome variable P_egocentric   My grouping variable
                        effect effect_size        SE        LL       UL     ta_LL
      1 Replication / Original   0.4498567 0.2056321 0.1836475 1.101954 0.2120998
            ta_UL
      1 0.9541313
      
      -- es_phi --
        grouping_variable_name outcome_variable_name
      1   My grouping variable   My outcome variable
                                              effect effect_size         SE
      1 My grouping variable and My outcome variable   0.1062688 0.06521775
                 LL        UL
      1 -0.02260905 0.2316724
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Test different estimate_pdiff_ind_contrast calls

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        grouping_variable_name grouping_variable_level outcome_variable_name
      1   My grouping variable                 Control   My outcome variable
      2   My grouping variable                 Control   My outcome variable
      3   My grouping variable                    Drug   My outcome variable
      4   My grouping variable                    Drug   My outcome variable
      5   My grouping variable                 Therapy   My outcome variable
      6   My grouping variable                 Therapy   My outcome variable
        outcome_variable_level cases  n   P      P_LL      P_UL       P_SE P_adjusted
      1              Depressed    10 50 0.2 0.1113372 0.3331072 0.05657501  0.2222222
      2          Not Depressed    40 50 0.8 0.6668928 0.8886628 0.05657501  0.7777778
      3              Depressed    20 50 0.4 0.2763554 0.5384594 0.06686451  0.4074074
      4          Not Depressed    30 50 0.6 0.4615406 0.7236446 0.06686451  0.5925926
      5              Depressed    30 50 0.6 0.4615406 0.7236446 0.06686451  0.5925926
      6          Not Depressed    20 50 0.4 0.2763554 0.5384594 0.06686451  0.4074074
            ta_LL     ta_UL
      1 0.1291646 0.3152798
      2 0.6847202 0.8708354
      3 0.2974251 0.5173897
      4 0.4826103 0.7025749
      5 0.4826103 0.7025749
      6 0.2974251 0.5173897
      
      -- es_proportion_difference --
              type outcome_variable_name  case_label grouping_variable_name
      1 Comparison   My outcome variable P_Depressed   My grouping variable
      2  Reference   My outcome variable P_Depressed   My grouping variable
      3 Difference   My outcome variable P_Depressed   My grouping variable
                              effect effect_size        LL        UL         SE
      1           (Drug and Therapy)         0.5 0.4056985 0.5943015 0.04811389
      2                      Control         0.2 0.1113372 0.3331072 0.05657501
      3 (Drug and Therapy) ‒ Control         0.3 0.1462012 0.4382144 0.07449453
        effect_size_adjusted     ta_LL     ta_UL
      1            0.5000000 0.4208597 0.5791403
      2            0.2222222 0.1291646 0.3152798
      3            0.2922078 0.1696752 0.4147404
      
      -- es_odds_ratio --
        outcome_variable_name  case_label grouping_variable_name
      1   My outcome variable P_Depressed   My grouping variable
                              effect effect_size        SE        LL        UL
      1 (Drug and Therapy) / Control   0.3857272 0.1731468 0.1600256 0.9297603
          ta_LL     ta_UL
      1 0.18434 0.8071252
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

