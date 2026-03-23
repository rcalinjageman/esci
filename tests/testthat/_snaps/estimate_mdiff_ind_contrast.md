# Compare estimate_mdiff_ind_contrast to ESCI_Ind_groups_contrasts, Halagappa

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = % time near target
      Grouping variable(s) = Diet
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1    % time near target                   Diet                 NFree10 37.5
      2    % time near target                   Diet                 AFree10 31.9
      3    % time near target                   Diet                 ADiet10 41.2
      4    % time near target                   Diet                 NFree17 33.4
      5    % time near target                   Diet                 AFree17 29.9
      6    % time near target                   Diet                 ADiet17 38.3
         mean_LL  mean_UL   sd  n  df  mean_SE
      1 32.32519 42.67481 10.0 19 108 2.610673
      2 26.72519 37.07481 13.5 19 108 2.610673
      3 36.02519 46.37481 14.8 19 108 2.610673
      4 28.22519 38.57481 10.0 19 108 2.610673
      5 24.72519 35.07481  8.7 19 108 2.610673
      6 33.12519 43.47481 10.0 19 108 2.610673
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison    % time near target                   Diet
      2  Reference    % time near target                   Diet
      3 Difference    % time near target                   Diet
                                                                       effect
      1                                     (NFree10 and AFree10 and AFree17)
      2                                     (ADiet10 and NFree17 and ADiet17)
      3 (NFree10 and AFree10 and AFree17) ‒ (ADiet10 and NFree17 and ADiet17)
        effect_size        LL         UL       SE  df     ta_LL      ta_UL
      1   33.100000 30.112324 36.0876762 1.507273 108 30.599306 35.6006939
      2   37.633333 34.645657 40.6210095 1.507273 108 35.132639 40.1340273
      3   -4.533333 -8.758546 -0.3081211 2.131606 108 -8.069849 -0.9968181
      
      -- es_smd --
        outcome_variable_name grouping_variable_name
      1    % time near target                   Diet
                                                                       effect
      1 (NFree10 and AFree10 and AFree17) ‒ (ADiet10 and NFree17 and ADiet17)
        effect_size         LL          UL numerator denominator        SE  df
      1  -0.3955976 -0.7655978 -0.02380328 -4.533333    11.37966 0.1892368 108
          d_biased
      1 -0.3983716
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_99
    Output
      Analysis of raw data:
      Outcome variable(s) = % time near target
      Grouping variable(s) = Diet
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1    % time near target                   Diet                 NFree10 37.5
      2    % time near target                   Diet                 AFree10 31.9
      3    % time near target                   Diet                 ADiet10 41.2
      4    % time near target                   Diet                 NFree17 33.4
      5    % time near target                   Diet                 AFree17 29.9
      6    % time near target                   Diet                 ADiet17 38.3
        mean_LL mean_UL   sd  n  df  mean_SE
      1 30.6545 44.3455 10.0 19 108 2.610673
      2 25.0545 38.7455 13.5 19 108 2.610673
      3 34.3545 48.0455 14.8 19 108 2.610673
      4 26.5545 40.2455 10.0 19 108 2.610673
      5 23.0545 36.7455  8.7 19 108 2.610673
      6 31.4545 45.1455 10.0 19 108 2.610673
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison    % time near target                   Diet
      2  Reference    % time near target                   Diet
      3 Difference    % time near target                   Diet
                                                                       effect
      1                                     (NFree10 and AFree10 and AFree17)
      2                                     (ADiet10 and NFree17 and ADiet17)
      3 (NFree10 and AFree10 and AFree17) ‒ (ADiet10 and NFree17 and ADiet17)
        effect_size        LL        UL       SE  df     ta_LL      ta_UL
      1   33.100000  29.14775 37.052250 1.507273 108 29.540768 36.6592320
      2   37.633333  33.68108 41.585584 1.507273 108 34.074101 41.1925653
      3   -4.533333 -10.12266  1.055993 2.131606 108 -9.566847  0.5001808
      
      -- es_smd --
        outcome_variable_name grouping_variable_name
      1    % time near target                   Diet
                                                                       effect
      1 (NFree10 and AFree10 and AFree17) ‒ (ADiet10 and NFree17 and ADiet17)
        effect_size         LL        UL numerator denominator        SE  df
      1  -0.3955976 -0.8821595 0.0927238 -4.533333    11.37966 0.1892368 108
          d_biased
      1 -0.3983716
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

---

    Code
      mytest
    Output
      $properties
      $properties$effect_size_name
      [1] "mean"
      
      $properties$alpha
      [1] 0.05
      
      $properties$interval_null
      [1] FALSE
      
      $properties$rope
      [1] 0 0
      
      $properties$rope_units
      [1] "raw"
      
      
      $point_null
                  test_type outcome_variable_name
      1 Nil Hypothesis Test    % time near target
                                                                       effect
      1 (NFree10 and AFree10 and AFree17) ‒ (ADiet10 and NFree17 and ADiet17)
        null_words confidence        LL         UL                             CI
      1       0.00         95 -8.758546 -0.3081211 95% CI [-8.758546, -0.3081211]
                             CI_compare         t  df         p p_result
      1 The 95% CI does not contain H_0 -2.126722 108 0.0357203 p < 0.05
        null_decision                                           conclusion
      1    Reject H_0 At α = 0.05, 0.00 is not a plausible value of μ_diff
        significant
      1        TRUE
      

# Test different call types to estimate_mdiff_ind_contrasts and compare to statpsych::ci.lc.median.bs

    Code
      from_vector
    Output
      Analysis of raw data:
      Outcome variable(s) = rattan_motivation
      Grouping variable(s) = rattan_condition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1     rattan_motivation       rattan_condition                 Chaling 5.264706
      2     rattan_motivation       rattan_condition                 Comfort 3.333333
      3     rattan_motivation       rattan_condition                 Control 4.447368
         mean_LL  mean_UL median median_LL median_UL       sd min max   q1    q3  n
      1 4.478385 6.051027   5.50  4.713293  6.286707 1.448249   2 7.0 4.50 6.000 17
      2 2.569166 4.097500   3.25  1.434014  5.065986 1.917412   1 6.0 1.50 5.375 18
      3 3.703583 5.191154   4.50  3.244372  5.755628 1.432701   2 6.5 3.25 6.000 19
        missing df   mean_SE median_SE
      1       0 51 0.3916755 0.4013886
      2       0 51 0.3806401 0.9265407
      3       0 51 0.3704879 0.6406383
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison     rattan_motivation       rattan_condition
      2  Reference     rattan_motivation       rattan_condition
      3 Difference     rattan_motivation       rattan_condition
                                 effect effect_size        LL       UL        SE df
      1                         Chaling    5.264706 4.4783846 6.051027 0.3916755 51
      2           (Comfort and Control)    3.890351 3.3571605 4.423541 0.2655881 51
      3 Chaling ‒ (Comfort and Control)    1.374355 0.4243059 2.324404 0.4732301 51
            ta_LL    ta_UL
      1 4.6085379 5.920874
      2 3.4454151 4.335287
      3 0.5815597 2.167150
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison     rattan_motivation       rattan_condition
      2  Reference     rattan_motivation       rattan_condition
      3 Difference     rattan_motivation       rattan_condition
                                 effect effect_size       LL       UL        SE
      1                         Chaling       5.500 4.713293 6.286707 0.4013886
      2           (Comfort and Control)       3.875 2.771097 4.978903 0.5632262
      3 Chaling ‒ (Comfort and Control)       1.625 0.269452 2.980548 0.6916188
            ta_LL    ta_UL
      1 4.8397745 6.160226
      2 2.9485753 4.801425
      3 0.4873882 2.762612
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                          effect
      1     rattan_motivation       rattan_condition Chaling ‒ (Comfort and Control)
        effect_size        LL       UL numerator denominator        SE df  d_biased
      1    0.838449 0.2378052 1.431475  1.374355    1.614919 0.3045133 51 0.8510363
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      from_df_nc
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = motivation
      Grouping variable(s) = condition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1            motivation              condition                 Chaling 5.264706
      2            motivation              condition                 Comfort 3.333333
      3            motivation              condition                 Control 4.447368
         mean_LL  mean_UL median median_LL median_UL       sd min max   q1    q3  n
      1 4.520085 6.009327   5.50  4.713293  6.286707 1.448249   2 7.0 4.50 6.000 17
      2 2.379827 4.286840   3.25  1.434014  5.065986 1.917412   1 6.0 1.50 5.375 18
      3 3.756829 5.137908   4.50  3.244372  5.755628 1.432701   2 6.5 3.25 6.000 19
        missing df   mean_SE median_SE
      1       0 16 0.3512521 0.4013886
      2       0 17 0.4519385 0.9265407
      3       0 18 0.3286841 0.6406383
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      from_df
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = motivation
      Grouping variable(s) = condition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1            motivation              condition                 Chaling 5.264706
      2            motivation              condition                 Comfort 3.333333
      3            motivation              condition                 Control 4.447368
         mean_LL  mean_UL median median_LL median_UL       sd min max   q1    q3  n
      1 4.520085 6.009327   5.50  4.713293  6.286707 1.448249   2 7.0 4.50 6.000 17
      2 2.379827 4.286840   3.25  1.434014  5.065986 1.917412   1 6.0 1.50 5.375 18
      3 3.756829 5.137908   4.50  3.244372  5.755628 1.432701   2 6.5 3.25 6.000 19
        missing df   mean_SE median_SE
      1       0 16 0.3512521 0.4013886
      2       0 17 0.4519385 0.9265407
      3       0 18 0.3286841 0.6406383
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison            motivation              condition
      2  Reference            motivation              condition
      3 Difference            motivation              condition
                                 effect effect_size        LL       UL        SE
      1                         Chaling    5.264706 4.5200847 6.009327 0.3512521
      2           (Comfort and Control)    3.890351 3.3208046 4.459897 0.2794108
      3 Chaling ‒ (Comfort and Control)    1.374355 0.4635767 2.285133 0.4488301
           df     ta_LL    ta_UL
      1 16.00 4.6514606 5.877951
      2 31.43 3.4168041 4.363898
      3 35.43 0.6162742 2.132436
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison            motivation              condition
      2  Reference            motivation              condition
      3 Difference            motivation              condition
                                 effect effect_size       LL       UL        SE
      1                         Chaling       5.500 4.713293 6.286707 0.4013886
      2           (Comfort and Control)       3.875 2.771097 4.978903 0.5632262
      3 Chaling ‒ (Comfort and Control)       1.625 0.269452 2.980548 0.6916188
            ta_LL    ta_UL
      1 4.8397745 6.160226
      2 2.9485753 4.801425
      3 0.4873882 2.762612
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                          effect
      1            motivation              condition Chaling ‒ (Comfort and Control)
        effect_size        LL       UL numerator denominator        SE df d_biased
      1   0.8383183 0.2641892 1.437597  1.374355    1.615191 0.2993442 51 0.850893
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      from_tidy
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = motivation
      Grouping variable(s) = condition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1            motivation              condition                 Chaling 5.264706
      2            motivation              condition                 Comfort 3.333333
      3            motivation              condition                 Control 4.447368
         mean_LL  mean_UL median median_LL median_UL       sd min max   q1    q3  n
      1 4.520085 6.009327   5.50  4.713293  6.286707 1.448249   2 7.0 4.50 6.000 17
      2 2.379827 4.286840   3.25  1.434014  5.065986 1.917412   1 6.0 1.50 5.375 18
      3 3.756829 5.137908   4.50  3.244372  5.755628 1.432701   2 6.5 3.25 6.000 19
        missing df   mean_SE median_SE
      1       0 16 0.3512521 0.4013886
      2       0 17 0.4519385 0.9265407
      3       0 18 0.3286841 0.6406383
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison            motivation              condition
      2  Reference            motivation              condition
      3 Difference            motivation              condition
                                 effect effect_size        LL       UL        SE
      1                         Chaling    5.264706 4.5200847 6.009327 0.3512521
      2           (Comfort and Control)    3.890351 3.3208046 4.459897 0.2794108
      3 Chaling ‒ (Comfort and Control)    1.374355 0.4635767 2.285133 0.4488301
           df     ta_LL    ta_UL
      1 16.00 4.6514606 5.877951
      2 31.43 3.4168041 4.363898
      3 35.43 0.6162742 2.132436
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison            motivation              condition
      2  Reference            motivation              condition
      3 Difference            motivation              condition
                                 effect effect_size       LL       UL        SE
      1                         Chaling       5.500 4.713293 6.286707 0.4013886
      2           (Comfort and Control)       3.875 2.771097 4.978903 0.5632262
      3 Chaling ‒ (Comfort and Control)       1.625 0.269452 2.980548 0.6916188
            ta_LL    ta_UL
      1 4.8397745 6.160226
      2 2.9485753 4.801425
      3 0.4873882 2.762612
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                          effect
      1            motivation              condition Chaling ‒ (Comfort and Control)
        effect_size        LL       UL numerator denominator        SE df d_biased
      1   0.8383183 0.2641892 1.437597  1.374355    1.615191 0.2993442 51 0.850893
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Compare estimate_mdiff_ind_contrast to statpsych::ci.lc.median example and 

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1   My outcome variable   My grouping variable                      G1 33.5
      2   My outcome variable   My grouping variable                      G2 37.9
      3   My outcome variable   My grouping variable                      G3 38.0
      4   My outcome variable   My grouping variable                      G4 44.1
         mean_LL  mean_UL   sd  n df  mean_SE
      1 29.55368 37.44632 3.84 10  9 1.214315
      2 33.95368 41.84632 3.84 10  9 1.214315
      3 34.24894 41.75106 3.65 10  9 1.154231
      4 38.98211 49.21789 4.98 10  9 1.574814
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name
      1 Comparison   My outcome variable   My grouping variable
      2  Reference   My outcome variable   My grouping variable
      3 Difference   My outcome variable   My grouping variable
                           effect effect_size       LL       UL        SE    df
      1               (G1 and G2)       35.70 33.22843 38.17157 0.8586501 18.00
      2               (G3 and G4)       41.05 38.20995 43.89005 0.9762543 16.50
      3 (G1 and G2) ‒ (G3 and G4)       -5.35 -8.90028 -1.79972 1.3001356 33.52
            ta_LL     ta_UL
      1 33.508399 37.891601
      2 38.536208 43.563792
      3 -8.526063 -2.173937
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                    effect
      1   My outcome variable   My grouping variable (G1 and G2) ‒ (G3 and G4)
        effect_size        LL         UL numerator denominator      SE df  d_biased
      1   -1.273964 -2.252465 -0.3500611     -5.35     4.11139 0.36928 36 -1.301263
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

