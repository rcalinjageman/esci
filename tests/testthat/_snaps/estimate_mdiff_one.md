# Compare estimate_mdiff one to ESCI_Data_two, pen group

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = transcription
      
      ---Overview---
        outcome_variable_name     mean  mean_LL  mean_UL median median_LL median_UL
      1         transcription 8.811765 7.154642 10.46889    8.6       6.5      11.1
              sd min  max  q1     q3  n missing df   mean_SE median_SE
      1 4.749339   1 20.1 5.2 11.275 34       0 33 0.8145049  1.021201
      
      -- es_mean --
        outcome_variable_name        effect effect_size       LL       UL        SE
      1         transcription transcription    8.811765 7.154642 10.46889 0.8145049
        df    ta_LL    ta_UL
      1 33 7.746606 9.876923
      
      -- es_median --
        outcome_variable_name        effect effect_size  LL   UL       SE df ta_LL
      1         transcription transcription         8.6 6.5 11.1 1.021201 33   7.1
        ta_UL
      1  10.8
      
      -- es_mean_difference --
        outcome_variable_name                          effect effect_size        LL
      1         transcription                   transcription    8.811765  7.154642
      2         transcription                 Reference value   10.000000        NA
      3         transcription transcription ‒ Reference value   -1.188235 -2.845358
                UL        SE df     ta_LL     ta_UL       type
      1 10.4688874 0.8145049 33  7.746606  9.876923 Comparison
      2         NA        NA NA  7.746606  9.876923  Reference
      3  0.4688874 0.8145049 33 -2.253394 -0.123077 Difference
      
      -- es_median_difference --
        outcome_variable_name                          effect effect_size   LL   UL
      1         transcription                   transcription         8.6  6.5 11.1
      2         transcription                 Reference value        10.0   NA   NA
      3         transcription transcription ‒ Reference value        -1.4 -3.5  1.1
              SE df ta_LL ta_UL       type
      1 1.021201 33   7.1  10.8 Comparison
      2       NA NA    NA    NA  Reference
      3 1.021201 33  -2.9   0.8 Difference
      
      -- es_smd --
        outcome_variable_name                          effect effect_size         LL
      1         transcription transcription ‒ Reference value  -0.2444527 -0.5838873
                UL numerator denominator        SE df   d_biased
      1 0.09856549 -1.188235    4.749339 0.1740983 33 -0.2501896
      This standardized mean difference is called d_1 because the standardizer used was s. d_1 has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Compare estimate_mdiff_one to ESCI_summary_two, pen group

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      
      ---Overview---
        outcome_variable_name  mean  mean_LL  mean_UL   sd   n  df   mean_SE
      1   My outcome variable 12.09 11.01117 13.16883 5.52 103 102 0.5439018
      
      -- es_mean --
        outcome_variable_name              effect effect_size       LL       UL
      1   My outcome variable My outcome variable       12.09 11.01117 13.16883
               SE  df    ta_LL    ta_UL
      1 0.5439018 102 11.38842 12.79158
      
      -- es_mean_difference --
        outcome_variable_name                                effect effect_size
      1   My outcome variable                   My outcome variable       12.09
      2   My outcome variable                       Reference value        0.00
      3   My outcome variable My outcome variable ‒ Reference value       12.09
              LL       UL        SE  df    ta_LL    ta_UL       type
      1 11.01117 13.16883 0.5439018 102 11.38842 12.79158 Comparison
      2       NA       NA        NA  NA 11.38842 12.79158  Reference
      3 11.01117 13.16883 0.5439018 102 11.38842 12.79158 Difference
      
      -- es_smd --
        outcome_variable_name                                effect effect_size
      1   My outcome variable My outcome variable ‒ Reference value    2.174067
              LL       UL numerator denominator       SE  df d_biased
      1 1.817286 2.527352     12.09        5.52 0.181166 102 2.190217
      This standardized mean difference is called d_1 because the standardizer used was s. d_1 has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

