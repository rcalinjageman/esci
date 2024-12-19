# estimate_magnitude of pen group from ESCI_summary_two: summary data, 95% CI

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
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# estimate_magnitude of pen group from ESCI_summary_two: summary data, 99% CI

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = transcription
      
      ---Overview---
        outcome_variable_name     mean  mean_LL  mean_UL median median_LL median_UL
      1         transcription 8.811765 6.585497 11.03803    8.6       5.2      11.3
              sd min  max  q1     q3  n missing df   mean_SE median_SE
      1 4.749339   1 20.1 5.2 11.275 34       0 33 0.8145049  1.021201
      
      -- es_mean --
        outcome_variable_name        effect effect_size       LL       UL        SE
      1         transcription transcription    8.811765 6.585497 11.03803 0.8145049
        df    ta_LL    ta_UL
      1 33 7.070224 10.55331
      
      -- es_median --
        outcome_variable_name        effect effect_size  LL   UL       SE df ta_LL
      1         transcription transcription         8.6 5.2 11.3 1.021201 33   5.2
        ta_UL
      1  11.2
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Compare estimate_magnitude to statpsych::ci.mean example

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      
      ---Overview---
        outcome_variable_name mean  mean_LL  mean_UL   sd  n df   mean_SE
      1   My outcome variable 24.5 23.33267 25.66733 3.65 40 39 0.5771157
      
      -- es_mean --
        outcome_variable_name              effect effect_size       LL       UL
      1   My outcome variable My outcome variable        24.5 23.33267 25.66733
               SE df    ta_LL    ta_UL
      1 0.5771157 39 23.74765 25.25235
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

