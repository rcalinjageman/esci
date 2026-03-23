# Compare estimate_mdiff_paired to ESCI_Data_paired, Thomasaon1

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = structure(list("Participant ID" = structure(1:12, levels = c("1", 
       Analysis of raw data:
      Data frame = "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), class = "factor"), 
       Analysis of raw data:
      Data frame =     Pretest = c(13, 12, 12, 9, 14, 17, 14, 9, 6, 7, 11, 15), 
       Analysis of raw data:
      Data frame =     Posttest = c(14, 13, 16, 12, 15, 18, 13, 10, 10, 8, 14, 16
       Analysis of raw data:
      Data frame =     )), removedRows = list(), addedRows = list(list(start = 0L, 
       Analysis of raw data:
      Data frame =     end = 11L)), transforms = list(), row.names = c(NA, 12L), class = "data.frame")
      
      ---Overview---
        outcome_variable_name     mean   mean_LL  mean_UL median median_LL median_UL
      1               Pretest 11.58333  9.476776 13.68989   12.0         9        14
      2              Posttest 13.25000 11.410019 15.08998   13.5        10        16
              sd min max   q1    q3  n missing df   mean_SE median_SE
      1 3.315483   6  17  9.0 14.00 12       0 11 0.9570974  1.208488
      2 2.895922   8  18 11.5 15.25 12       0 11 0.8359806  1.450186
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name             effect
      1  Comparison                Posttest                Pretest           Posttest
      2   Reference                Posttest                Pretest            Pretest
      11 Difference                Posttest                Pretest Posttest ‒ Pretest
         effect_size        LL        UL        SE df     ta_LL     ta_UL
      1    13.250000 11.410019 15.089981 0.8359806 11 11.748675 14.751325
      2    11.583333  9.476776 13.689890 0.9570974 11  9.864497 13.302170
      11    1.666667  0.715218  2.618115 0.4322831 11  0.890336  2.442997
      
      -- es_smd --
        comparison_measure_name reference_measure_name             effect effect_size
      1                Posttest                Pretest Posttest - Pretest      0.5105
            LL     UL numerator denominator      SE d_biased df
      1 0.1806 0.8902  1.666667    3.112779 0.18102   0.5105 11
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name               effect effect_size     LL
      1         Pretest        Posttest Pretest and Posttest      0.8924 0.6289
            UL     SE  n df  ta_LL  ta_UL
      1 0.9672 0.0614 12 10 0.6883 0.9596
      
      -- es_median_difference --
              type comparison_measure_name reference_measure_name             effect
        Comparison                Posttest                Pretest           Posttest
      1  Reference                Posttest                Pretest            Pretest
      2 Difference                Posttest                Pretest Posttest ‒ Pretest
        effect_size        LL        UL       SE     ta_LL     ta_UL
               13.5 10.000000 16.000000 1.450186 10.000000 16.000000
      1        12.0  9.000000 14.000000 1.208488  9.000000 14.000000
      2         1.5 -1.589665  4.589665 1.576389 -1.589665  4.589665
      
      -- es_mean_ratio --
        comparison_measure_name reference_measure_name             effect effect_size
      1                Posttest                Pretest Posttest / Pretest    1.143885
             LL       UL comparison_mean reference_mean
      1 1.05031 1.245797           13.25       11.58333
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        comparison_measure_name reference_measure_name             effect effect_size
      1                Posttest                Pretest Posttest / Pretest       1.125
               LL       UL comparison_median reference_median
      1 0.8723319 1.450853              13.5               12
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

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
                  test_type outcome_variable_name             effect null_words
      1 Nil Hypothesis Test    Posttest - Pretest Posttest ‒ Pretest       0.00
        confidence       LL       UL                          CI
      1         95 0.715218 2.618115 95% CI [0.715218, 2.618115]
                             CI_compare        t df           p p_result
      1 The 95% CI does not contain H_0 3.855498 11 0.002674001 p < 0.05
        null_decision                                           conclusion
      1    Reject H_0 At α = 0.05, 0.00 is not a plausible value of μ_diff
        significant
      1        TRUE
      

---

    Code
      estimate_99
    Output
      Analysis of raw data:
      Data frame = structure(list("Participant ID" = structure(1:12, levels = c("1", 
       Analysis of raw data:
      Data frame = "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), class = "factor"), 
       Analysis of raw data:
      Data frame =     Pretest = c(13, 12, 12, 9, 14, 17, 14, 9, 6, 7, 11, 15), 
       Analysis of raw data:
      Data frame =     Posttest = c(14, 13, 16, 12, 15, 18, 13, 10, 10, 8, 14, 16
       Analysis of raw data:
      Data frame =     )), removedRows = list(), addedRows = list(list(start = 0L, 
       Analysis of raw data:
      Data frame =     end = 11L)), transforms = list(), row.names = c(NA, 12L), class = "data.frame")
      
      ---Overview---
        outcome_variable_name     mean   mean_LL  mean_UL median median_LL median_UL
      1               Pretest 11.58333  8.610774 14.55589   12.0         7        15
      2              Posttest 13.25000 10.653606 15.84639   13.5        10        16
              sd min max   q1    q3  n missing df   mean_SE median_SE
      1 3.315483   6  17  9.0 14.00 12       0 11 0.9570974  1.208488
      2 2.895922   8  18 11.5 15.25 12       0 11 0.8359806  1.450186
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name             effect
      1  Comparison                Posttest                Pretest           Posttest
      2   Reference                Posttest                Pretest            Pretest
      11 Difference                Posttest                Pretest Posttest ‒ Pretest
         effect_size        LL        UL        SE df      ta_LL     ta_UL
      1    13.250000 10.653606 15.846394 0.8359806 11 10.9777384 15.522262
      2    11.583333  8.610774 14.555893 0.9570974 11  8.9818669 14.184800
      11    1.666667  0.324079  3.009254 0.4322831 11  0.4916869  2.841646
      
      -- es_smd --
        comparison_measure_name reference_measure_name             effect effect_size
      1                Posttest                Pretest Posttest - Pretest      0.5105
            LL     UL numerator denominator      SE d_biased df
      1 0.0692 1.0017  1.666667    3.112779 0.18102   0.5105 11
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name               effect effect_size     LL
      1         Pretest        Posttest Pretest and Posttest      0.8924 0.4887
            UL     SE  n df  ta_LL  ta_UL
      1 0.9781 0.0614 12 10 0.5494 0.9742
      
      -- es_median_difference --
              type comparison_measure_name reference_measure_name             effect
        Comparison                Posttest                Pretest           Posttest
      1  Reference                Posttest                Pretest            Pretest
      2 Difference                Posttest                Pretest Posttest ‒ Pretest
        effect_size        LL        UL       SE     ta_LL     ta_UL
               13.5 10.000000 16.000000 1.450186 10.000000 16.000000
      1        12.0  7.000000 15.000000 1.208488  7.000000 15.000000
      2         1.5 -2.560508  5.560508 1.576389 -2.560508  5.560508
      
      -- es_mean_ratio --
        comparison_measure_name reference_measure_name             effect effect_size
      1                Posttest                Pretest Posttest / Pretest    1.143885
              LL       UL comparison_mean reference_mean
      1 1.014099 1.290281           13.25       11.58333
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        comparison_measure_name reference_measure_name             effect effect_size
      1                Posttest                Pretest Posttest / Pretest       1.125
               LL       UL comparison_median reference_median
      1 0.8053215 1.571577              13.5               12
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Compare estimate_mdiff_paired to ESCI_Summary_paired, penlaptop1

    Code
      estimate
    Output
      Analysis of raw data:
      
      ---Overview---
        outcome_variable_name  mean  mean_LL  mean_UL   sd  n df mean_SE
      1                Before 12.88 11.06827 14.69173 3.40 16 15    0.85
      2                 After 14.25 11.96935 16.53065 4.28 16 15    1.07
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name         effect
      1  Comparison                   After                 Before          After
      2   Reference                   After                 Before         Before
      11 Difference                   After                 Before After ‒ Before
         effect_size         LL        UL     SE df      ta_LL     ta_UL
      1        14.25 11.9693490 16.530651 1.0700 15 12.3742361 16.125764
      2        12.88 11.0682679 14.691732 0.8500 15 11.3899072 14.370093
      11        1.37  0.2350031  2.504997 0.5325 15  0.4365007  2.303499
      
      -- es_smd --
        comparison_measure_name reference_measure_name         effect effect_size
      1                   After                 Before After - Before      0.3424
            LL     UL numerator denominator      SE d_biased df
      1 0.0511 0.6578      1.37    3.865126 0.15477   0.3424 15
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name           effect effect_size     LL     UL
      1          Before           After Before and After      0.8707 0.6431 0.9518
             SE  n df  ta_LL  ta_UL
      1 0.06244 16 14 0.6915 0.9429
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

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
                  test_type outcome_variable_name         effect null_words
      1 Nil Hypothesis Test        After - Before After ‒ Before       0.00
        confidence        LL       UL                           CI
      1         95 0.2350031 2.504997 95% CI [0.2350031, 2.504997]
                             CI_compare       t df          p p_result null_decision
      1 The 95% CI does not contain H_0 2.57277 15 0.02121729 p < 0.05    Reject H_0
                                                  conclusion significant
      1 At α = 0.05, 0.00 is not a plausible value of μ_diff        TRUE
      

---

    Code
      estimate_99
    Output
      Analysis of raw data:
      
      ---Overview---
        outcome_variable_name  mean  mean_LL  mean_UL   sd  n df mean_SE
      1                Before 12.88 10.37529 15.38471 3.40 16 15    0.85
      2                 After 14.25 11.09702 17.40298 4.28 16 15    1.07
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name         effect
      1  Comparison                   After                 Before          After
      2   Reference                   After                 Before         Before
      11 Difference                   After                 Before After ‒ Before
         effect_size         LL        UL     SE df       ta_LL     ta_UL
      1        14.25 11.0970172 17.402983 1.0700 15 11.46534608 17.034654
      2        12.88 10.3752940 15.384706 0.8500 15 10.66789175 15.092108
      11        1.37 -0.1991246  2.939125 0.5325 15 -0.01582076  2.755821
      
      -- es_smd --
        comparison_measure_name reference_measure_name         effect effect_size
      1                   After                 Before After - Before      0.3424
             LL     UL numerator denominator      SE d_biased df
      1 -0.0442 0.7531      1.37    3.865126 0.15477   0.3424 15
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name           effect effect_size     LL     UL
      1          Before           After Before and After      0.8707 0.5318 0.9655
             SE  n df  ta_LL  ta_UL
      1 0.06244 16 14 0.5796 0.9605
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Ensure negative values processed correctly

    Code
      estimate
    Output
      Analysis of raw data:
      
      ---Overview---
        outcome_variable_name mean  mean_LL  mean_UL median median_LL median_UL
      1                    y2 21.6 4.167124 39.03288     28        14        40
      2                    y1 11.2 3.954738 18.44526     12         6        19
              sd min max q1   q3  n missing df  mean_SE median_SE
      1 31.47970 -67  52 17 40.0 15       0 14 8.128023  6.171216
      2 13.08325 -21  35  7 18.5 15       0 14 3.378081  3.085608
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name  effect
      1  Comparison                      y1                     y2      y1
      2   Reference                      y1                     y2      y2
      11 Difference                      y1                     y2 y1 ‒ y2
         effect_size         LL         UL       SE df      ta_LL     ta_UL
      1         11.2   3.954738 18.4452623 3.378081 14   5.250152 17.149848
      2         21.6   4.167124 39.0328761 8.128023 14   7.284030 35.915970
      11       -10.4 -21.379887  0.5798875 5.119338 14 -19.416741 -1.383259
      
      -- es_smd --
        comparison_measure_name reference_measure_name  effect effect_size      LL
      1                      y1                     y2 y1 - y2     -0.4157 -0.8901
            UL numerator denominator    SE d_biased df
      1 0.0272     -10.4    24.10542 0.234  -0.4157 14
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name    effect effect_size     LL     UL      SE
      1              y2              y1 y2 and y1      0.9336 0.7957 0.9766 0.03431
         n df  ta_LL ta_UL
      1 15 13 0.8267 0.972
      
      -- es_median_difference --
              type comparison_measure_name reference_measure_name  effect effect_size
        Comparison                      y1                     y2      y1          12
      1  Reference                      y1                     y2      y2          28
      2 Difference                      y1                     y2 y1 ‒ y2         -16
               LL        UL       SE     ta_LL     ta_UL
          6.00000 19.000000 3.085608   6.00000 19.000000
      1  14.00000 40.000000 6.171216  14.00000 40.000000
      2 -25.23798 -6.762016 4.713344 -25.23798 -6.762016
      
      -- es_mean_ratio --
        comparison_measure_name reference_measure_name  effect effect_size        LL
      1                      y1                     y2 y1 / y2   0.5185185 0.3809957
               UL comparison_mean reference_mean
      1 0.7056811            11.2           21.6
      [1] "WARNING!  Your dataset includes negative values.  This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        comparison_measure_name reference_measure_name  effect effect_size LL UL
      1                      y1                     y2 y1 / y2          NA NA NA
        comparison_median reference_median
      1                NA               NA
      [1] "WARNING!  Your dataset includes negative values.  This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.
      Warnings:
      * The ratio between group effect size is appropriate only for true ratio scales where values < 0 are impossible.  Your data include at least one negative value, so the requested ratio effect size is not reported.

# Test different types of calls to estimate_mdiff_paired

    Code
      from_vector
    Output
      Analysis of raw data:
      
      ---Overview---
        outcome_variable_name     mean  mean_LL  mean_UL median median_LL median_UL
      1            bk_wrapper 2.266667 1.757755 2.775578      2         1         3
      2            wc_wrapper 1.833333 1.506871 2.159795      2         1         2
               sd min max q1 q3  n missing df   mean_SE median_SE
      1 1.3628908   1   5  1  3 30       0 29 0.2488287 0.4936052
      2 0.8742813   1   4  1  2 30       0 29 0.1596212 0.2468026
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name
      1  Comparison              wc_wrapper             bk_wrapper
      2   Reference              wc_wrapper             bk_wrapper
      11 Difference              wc_wrapper             bk_wrapper
                          effect effect_size         LL         UL        SE df
      1               wc_wrapper   1.8333333  1.5068713 2.15979534 0.1596212 29
      2               bk_wrapper   2.2666667  1.7577549 2.77557845 0.2488287 29
      11 wc_wrapper ‒ bk_wrapper  -0.4333333 -0.9398777 0.07321103 0.2476711 29
              ta_LL      ta_UL
      1   1.5621166  2.1045500
      2   1.8438751  2.6894582
      11 -0.8541581 -0.0125086
      
      -- es_smd --
        comparison_measure_name reference_measure_name                  effect
      1              wc_wrapper             bk_wrapper wc_wrapper - bk_wrapper
        effect_size      LL     UL  numerator denominator      SE d_biased df
      1     -0.3719 -0.8166 0.0596 -0.4333333    1.144954 0.22353  -0.3719 29
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name                    effect effect_size      LL
      1      bk_wrapper      wc_wrapper bk_wrapper and wc_wrapper       0.328 -0.0423
           UL      SE  n df  ta_LL  ta_UL
      1 0.612 0.16572 30 28 0.0184 0.5727
      
      -- es_median_difference --
              type comparison_measure_name reference_measure_name
        Comparison              wc_wrapper             bk_wrapper
      1  Reference              wc_wrapper             bk_wrapper
      2 Difference              wc_wrapper             bk_wrapper
                         effect effect_size        LL       UL        SE     ta_LL
                     wc_wrapper           2  1.000000 2.000000 0.2468026  1.000000
      1              bk_wrapper           2  1.000000 3.000000 0.4936052  1.000000
      2 wc_wrapper ‒ bk_wrapper           0 -1.109202 1.109202 0.5659300 -1.109202
           ta_UL
        2.000000
      1 3.000000
      2 1.109202
      
      -- es_mean_ratio --
        comparison_measure_name reference_measure_name                  effect
      1              wc_wrapper             bk_wrapper wc_wrapper / bk_wrapper
        effect_size        LL       UL comparison_mean reference_mean
      1   0.8088235 0.6385273 1.024538        1.833333       2.266667
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        comparison_measure_name reference_measure_name                  effect
      1              wc_wrapper             bk_wrapper wc_wrapper / bk_wrapper
        effect_size        LL       UL comparison_median reference_median
      1           1 0.5239318 1.908646                 2                2
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      from_df_strings
    Output
      Analysis of raw data:
      Data frame = structure(list(wc = c(2, 3, 2, 1, 1, 2, 1, 1, 3, 2, 1, 1, 2, 
       Analysis of raw data:
      Data frame = 4, 1, 1, 4, 2, 2, 1, 2, 2, 1, 3, 2, 2, 1, 1, 2, 2), bk = c(4, 
       Analysis of raw data:
      Data frame = 4, 3, 2, 2, 5, 1, 1, 3, 1, 1, 2, 4, 3, 1, 1, 1, 3, 1, 1, 1, 5, 
       Analysis of raw data:
      Data frame = 1, 4, 1, 3, 2, 4, 2, 1)), row.names = c(NA, 30L), class = "data.frame")
      
      ---Overview---
        outcome_variable_name     mean  mean_LL  mean_UL median median_LL median_UL
      1                    bk 2.266667 1.757755 2.775578      2         1         3
      2                    wc 1.833333 1.506871 2.159795      2         1         2
               sd min max q1 q3  n missing df   mean_SE median_SE
      1 1.3628908   1   5  1  3 30       0 29 0.2488287 0.4936052
      2 0.8742813   1   4  1  2 30       0 29 0.1596212 0.2468026
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name  effect
      1  Comparison                      wc                     bk      wc
      2   Reference                      wc                     bk      bk
      11 Difference                      wc                     bk wc ‒ bk
         effect_size         LL         UL        SE df      ta_LL      ta_UL
      1    1.8333333  1.5068713 2.15979534 0.1596212 29  1.5621166  2.1045500
      2    2.2666667  1.7577549 2.77557845 0.2488287 29  1.8438751  2.6894582
      11  -0.4333333 -0.9398777 0.07321103 0.2476711 29 -0.8541581 -0.0125086
      
      -- es_smd --
        comparison_measure_name reference_measure_name  effect effect_size      LL
      1                      wc                     bk wc - bk     -0.3719 -0.8166
            UL  numerator denominator      SE d_biased df
      1 0.0596 -0.4333333    1.144954 0.22353  -0.3719 29
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name    effect effect_size      LL    UL      SE
      1              bk              wc bk and wc       0.328 -0.0423 0.612 0.16572
         n df  ta_LL  ta_UL
      1 30 28 0.0184 0.5727
      
      -- es_median_difference --
              type comparison_measure_name reference_measure_name  effect effect_size
        Comparison                      wc                     bk      wc           2
      1  Reference                      wc                     bk      bk           2
      2 Difference                      wc                     bk wc ‒ bk           0
               LL       UL        SE     ta_LL    ta_UL
         1.000000 2.000000 0.2468026  1.000000 2.000000
      1  1.000000 3.000000 0.4936052  1.000000 3.000000
      2 -1.109202 1.109202 0.5659300 -1.109202 1.109202
      
      -- es_mean_ratio --
        comparison_measure_name reference_measure_name  effect effect_size        LL
      1                      wc                     bk wc / bk   0.8088235 0.6385273
              UL comparison_mean reference_mean
      1 1.024538        1.833333       2.266667
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        comparison_measure_name reference_measure_name  effect effect_size        LL
      1                      wc                     bk wc / bk           1 0.5239318
              UL comparison_median reference_median
      1 1.908646                 2                2
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      from_df_tidy
    Output
      Analysis of raw data:
      Data frame = structure(list(wc = c(2, 3, 2, 1, 1, 2, 1, 1, 3, 2, 1, 1, 2, 
       Analysis of raw data:
      Data frame = 4, 1, 1, 4, 2, 2, 1, 2, 2, 1, 3, 2, 2, 1, 1, 2, 2), bk = c(4, 
       Analysis of raw data:
      Data frame = 4, 3, 2, 2, 5, 1, 1, 3, 1, 1, 2, 4, 3, 1, 1, 1, 3, 1, 1, 1, 5, 
       Analysis of raw data:
      Data frame = 1, 4, 1, 3, 2, 4, 2, 1)), row.names = c(NA, 30L), class = "data.frame")
      
      ---Overview---
        outcome_variable_name     mean  mean_LL  mean_UL median median_LL median_UL
      1                    bk 2.266667 1.757755 2.775578      2         1         3
      2                    wc 1.833333 1.506871 2.159795      2         1         2
               sd min max q1 q3  n missing df   mean_SE median_SE
      1 1.3628908   1   5  1  3 30       0 29 0.2488287 0.4936052
      2 0.8742813   1   4  1  2 30       0 29 0.1596212 0.2468026
      
      -- es_mean_difference --
               type comparison_measure_name reference_measure_name  effect
      1  Comparison                      wc                     bk      wc
      2   Reference                      wc                     bk      bk
      11 Difference                      wc                     bk wc ‒ bk
         effect_size         LL         UL        SE df      ta_LL      ta_UL
      1    1.8333333  1.5068713 2.15979534 0.1596212 29  1.5621166  2.1045500
      2    2.2666667  1.7577549 2.77557845 0.2488287 29  1.8438751  2.6894582
      11  -0.4333333 -0.9398777 0.07321103 0.2476711 29 -0.8541581 -0.0125086
      
      -- es_smd --
        comparison_measure_name reference_measure_name  effect effect_size      LL
      1                      wc                     bk wc - bk     -0.3719 -0.8166
            UL  numerator denominator      SE d_biased df
      1 0.0596 -0.4333333    1.144954 0.22353  -0.3719 29
      This standardized mean difference is called d_average because the standardizer used was s_average. d_average has been corrected for bias. Correction for bias can be important when df < 50.
      
      -- es_r --
        x_variable_name y_variable_name    effect effect_size      LL    UL      SE
      1              bk              wc bk and wc       0.328 -0.0423 0.612 0.16572
         n df  ta_LL  ta_UL
      1 30 28 0.0184 0.5727
      
      -- es_median_difference --
              type comparison_measure_name reference_measure_name  effect effect_size
        Comparison                      wc                     bk      wc           2
      1  Reference                      wc                     bk      bk           2
      2 Difference                      wc                     bk wc ‒ bk           0
               LL       UL        SE     ta_LL    ta_UL
         1.000000 2.000000 0.2468026  1.000000 2.000000
      1  1.000000 3.000000 0.4936052  1.000000 3.000000
      2 -1.109202 1.109202 0.5659300 -1.109202 1.109202
      
      -- es_mean_ratio --
        comparison_measure_name reference_measure_name  effect effect_size        LL
      1                      wc                     bk wc / bk   0.8088235 0.6385273
              UL comparison_mean reference_mean
      1 1.024538        1.833333       2.266667
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        comparison_measure_name reference_measure_name  effect effect_size        LL
      1                      wc                     bk wc / bk           1 0.5239318
              UL comparison_median reference_median
      1 1.908646                 2                2
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

