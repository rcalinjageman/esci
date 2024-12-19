# Compare estimate_pdiff_two to ESCI_Two_proportions example

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
      1               Affected    78 252 0.3095238 0.2557208 0.3692792 0.02896953
      2           Not Affected   174 252 0.6904762 0.6307208 0.7442792 0.02896953
      3               Affected    10  20 0.5000000 0.2999620 0.7000380 0.10206207
      4           Not Affected    10  20 0.5000000 0.2999620 0.7000380 0.10206207
        P_adjusted     ta_LL     ta_UL
      1     0.3125 0.2648494 0.3601506
      2     0.6875 0.6398494 0.7351506
      3     0.5000 0.3321228 0.6678772
      4     0.5000 0.3321228 0.6678772
      
      -- es_proportion_difference --
              type outcome_variable_name case_label grouping_variable_name
      1 Comparison   My outcome variable P_Affected   My grouping variable
      2  Reference   My outcome variable P_Affected   My grouping variable
      3 Difference   My outcome variable P_Affected   My grouping variable
                        effect effect_size          LL        UL         SE
      1            Replication   0.5000000  0.29996201 0.7000380 0.10206207
      2               Original   0.3095238  0.25572077 0.3692792 0.02896953
      3 Replication ‒ Original   0.1904762 -0.02757339 0.4055261 0.11048660
        effect_size_adjusted       ta_LL     ta_UL
      1            0.5000000 0.332122830 0.6678772
      2            0.3125000 0.264849371 0.3601506
      3            0.1889764 0.007242087 0.3707107
      
      -- es_odds_ratio --
        outcome_variable_name case_label grouping_variable_name
      1   My outcome variable P_Affected   My grouping variable
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

