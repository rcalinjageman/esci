# Compare estimate_mdiff_2x2_between to ESCI_Ind_groups_2_x_2, Frenda

    Code
      estimates
    Output
      Analysis of raw data:
      Outcome variable(s) = False Memory Score, False Memory Score, False Memory Score, False Memory Score, False Memory Score
      Grouping variable(s) = Testing Time - Rest
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1    False Memory Score           Testing Time         Evening - Sleep 1.50
      2    False Memory Score           Testing Time      Evening - No Sleep 1.14
      3    False Memory Score           Testing Time         Morning - Sleep 1.38
      4    False Memory Score           Testing Time      Morning - No Sleep 2.22
          mean_LL  mean_UL   sd  n df   mean_SE grouping_variable_A_name
      1 0.9535683 2.046432 1.38 26 99 0.2753891             Testing Time
      2 0.5935683 1.686432 0.96 26 99 0.2753891             Testing Time
      3 0.8227468 1.937253 1.50 25 99 0.2808429             Testing Time
      4 1.6735683 2.766432 1.68 26 99 0.2753891             Testing Time
        grouping_variable_B_name grouping_variable_A_level grouping_variable_B_level
      1                     Rest                   Evening                     Sleep
      2                     Rest                   Evening                  No Sleep
      3                     Rest                   Morning                     Sleep
      4                     Rest                   Morning                  No Sleep
      
      -- es_mean_difference --
               type outcome_variable_name grouping_variable_name
      1  Comparison    False Memory Score           Testing Time
      2   Reference    False Memory Score           Testing Time
      3  Difference    False Memory Score           Testing Time
      4  Comparison    False Memory Score                   Rest
      5   Reference    False Memory Score                   Rest
      6  Difference    False Memory Score                   Rest
      7  Comparison    False Memory Score                   Rest
      8   Reference    False Memory Score                   Rest
      9  Difference    False Memory Score                   Rest
      10 Comparison    False Memory Score                   Rest
      11  Reference    False Memory Score                   Rest
      12 Difference    False Memory Score                   Rest
      13 Comparison    False Memory Score            Interaction
      14  Reference    False Memory Score            Interaction
      15 Difference    False Memory Score            Interaction
                                   effect effect_size          LL        UL        SE
      1                           Morning        1.80  1.40976969 2.1902303 0.1966672
      2                           Evening        1.32  0.93361441 1.7063856 0.1947295
      3                        Difference        0.48 -0.06915710 1.0291571 0.2767626
      4                          No Sleep        1.68  1.29361441 2.0663856 0.1947295
      5                             Sleep        1.44  1.04976969 1.8302303 0.1966672
      6                        Difference        0.24 -0.30915710 0.7891571 0.2767626
      7                          No Sleep        2.22  1.67356826 2.7664317 0.2753891
      8                             Sleep        1.38  0.82274678 1.9372532 0.2808429
      9                        Difference        0.84  0.05953937 1.6204606 0.3933343
      10                         No Sleep        1.14  0.59356826 1.6864317 0.2753891
      11                            Sleep        1.50  0.95356826 2.0464317 0.2753891
      12                       Difference       -0.36 -1.13277117 0.4127712 0.3894590
      13 Simple effect of Rest at Morning        0.84  0.05953937 1.6204606 0.3933343
      14 Simple effect of Rest at Evening       -0.36 -1.13277117 0.4127712 0.3894590
      15        Difference of differences        1.20  0.10168580 2.2983142 0.5535253
         df       ta_LL     ta_UL                                    effect_type
      1  99  1.47345559 2.1265444                    Main effect of Testing Time
      2  99  0.99667285 1.6433271                    Main effect of Testing Time
      3  99  0.02046578 0.9395342                    Main effect of Testing Time
      4  99  1.35667285 2.0033271                            Main effect of Rest
      5  99  1.11345559 1.7665444                            Main effect of Rest
      6  99 -0.21953422 0.6995342                            Main effect of Rest
      7  99  1.76274636 2.6772536 Simple effect of Rest at Testing Time: Morning
      8  99  0.91369095 1.8463090 Simple effect of Rest at Testing Time: Morning
      9  99  0.18691117 1.4930888 Simple effect of Rest at Testing Time: Morning
      10 99  0.68274636 1.5972536 Simple effect of Rest at Testing Time: Evening
      11 99  1.04274636 1.9572536 Simple effect of Rest at Testing Time: Evening
      12 99 -1.00665430 0.2866543 Simple effect of Rest at Testing Time: Evening
      13 99  0.18691117 1.4930888           Interaction of Testing Time and Rest
      14 99 -1.00665430 0.2866543           Interaction of Testing Time and Rest
      15 99  0.28093156 2.1190684           Interaction of Testing Time and Rest
                   effects_complex
      1                    Morning
      2                    Evening
      3          Morning ‒ Evening
      4                   No Sleep
      5                      Sleep
      6           No Sleep ‒ Sleep
      7          Morning: No Sleep
      8             Morning: Sleep
      9  Morning: No Sleep ‒ Sleep
      10         Evening: No Sleep
      11            Evening: Sleep
      12 Evening: No Sleep ‒ Sleep
      13 Morning: No Sleep ‒ Sleep
      14 Evening: No Sleep ‒ Sleep
      15 Difference of differences
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                    effect
      1    False Memory Score           Testing Time                Difference
      2    False Memory Score                   Rest                Difference
      3    False Memory Score                   Rest                Difference
      4    False Memory Score                   Rest                Difference
      5    False Memory Score            Interaction Difference of differences
        effect_size          LL        UL numerator denominator        SE df
      1   0.3392309 -0.05078217 0.7275568      0.48    1.404214 0.1985595 99
      2   0.1696154 -0.21782845 0.5562068      0.24    1.404214 0.1974616 99
      3   0.5936540  0.03700651 1.1473703      0.84    1.404214 0.2832613 99
      4  -0.2544232 -0.79853128 0.2909631     -0.36    1.404214 0.2779374 99
      5   0.8480772  0.06443356 1.6275360      1.20    1.404214 0.3987579 99
          d_biased                                    effect_type
      1  0.3418281                    Main effect of Testing Time
      2  0.1709141                            Main effect of Rest
      3  0.5981992 Simple effect of Rest at Testing Time: Morning
      4 -0.2563711 Simple effect of Rest at Testing Time: Evening
      5  0.8545703           Interaction of Testing Time and Rest
                  effects_complex
      1         Morning ‒ Evening
      2          No Sleep ‒ Sleep
      3 Morning: No Sleep ‒ Sleep
      4 Evening: No Sleep ‒ Sleep
      5 Difference of differences
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      mytesta
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
                  test_type outcome_variable_name     effect null_words confidence
      1 Nil Hypothesis Test    False Memory Score Difference       0.00         95
                LL       UL                            CI              CI_compare
      1 -0.0691571 1.029157 95% CI [-0.0691571, 1.029157] The 95% CI contains H_0
               t df          p p_result      null_decision
      1 1.734338 99 0.08597067 p ≥ 0.05 Fail to reject H_0
                                                   conclusion significant
      1 At α = 0.05, 0.00 remains a plausible value of μ_diff       FALSE
      

---

    Code
      mytestb
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
                  test_type outcome_variable_name     effect null_words confidence
      1 Nil Hypothesis Test    False Memory Score Difference       0.00         95
                LL        UL                             CI              CI_compare
      1 -0.3091571 0.7891571 95% CI [-0.3091571, 0.7891571] The 95% CI contains H_0
                t df         p p_result      null_decision
      1 0.8671691 99 0.3879464 p ≥ 0.05 Fail to reject H_0
                                                   conclusion significant
      1 At α = 0.05, 0.00 remains a plausible value of μ_diff       FALSE
      

---

    Code
      mytesti
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
                  test_type outcome_variable_name                    effect
      1 Nil Hypothesis Test    False Memory Score Difference of differences
        null_words confidence        LL       UL                           CI
      1       0.00         95 0.1016858 2.298314 95% CI [0.1016858, 2.298314]
                             CI_compare        t df          p p_result null_decision
      1 The 95% CI does not contain H_0 2.167923 99 0.03256134 p < 0.05    Reject H_0
                                                  conclusion significant
      1 At α = 0.05, 0.00 is not a plausible value of μ_diff        TRUE
      

---

    Code
      mytests1
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
                  test_type outcome_variable_name     effect null_words confidence
      1 Nil Hypothesis Test    False Memory Score Difference       0.00         95
                LL       UL                            CI
      1 0.05953937 1.620461 95% CI [0.05953937, 1.620461]
                             CI_compare        t df          p p_result null_decision
      1 The 95% CI does not contain H_0 2.135588 99 0.03517985 p < 0.05    Reject H_0
                                                  conclusion significant
      1 At α = 0.05, 0.00 is not a plausible value of μ_diff        TRUE
      

---

    Code
      mytests2
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
                  test_type outcome_variable_name     effect null_words confidence
      1 Nil Hypothesis Test    False Memory Score Difference       0.00         95
               LL        UL                            CI              CI_compare
      1 -1.132771 0.4127712 95% CI [-1.132771, 0.4127712] The 95% CI contains H_0
                 t df        p p_result      null_decision
      1 -0.9243591 99 0.357547 p ≥ 0.05 Fail to reject H_0
                                                   conclusion significant
      1 At α = 0.05, 0.00 remains a plausible value of μ_diff       FALSE
      

# Compare estimate_mdiff_2x2_between to statpsych::ci.lc.stdmean.bs, Frenda

    Code
      estimates
    Output
      Analysis of raw data:
      Outcome variable(s) = False Memory Score, False Memory Score, False Memory Score, False Memory Score, False Memory Score
      Grouping variable(s) = Testing Time - Rest
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1    False Memory Score           Testing Time         Evening - Sleep 1.50
      2    False Memory Score           Testing Time      Evening - No Sleep 1.14
      3    False Memory Score           Testing Time         Morning - Sleep 1.38
      4    False Memory Score           Testing Time      Morning - No Sleep 2.22
          mean_LL  mean_UL   sd  n df   mean_SE grouping_variable_A_name
      1 0.9426059 2.057394 1.38 26 25 0.2706403             Testing Time
      2 0.7522476 1.527752 0.96 26 25 0.1882715             Testing Time
      3 0.7608304 1.999170 1.50 25 24 0.3000000             Testing Time
      4 1.5414333 2.898567 1.68 26 25 0.3294751             Testing Time
        grouping_variable_B_name grouping_variable_A_level grouping_variable_B_level
      1                     Rest                   Evening                     Sleep
      2                     Rest                   Evening                  No Sleep
      3                     Rest                   Morning                     Sleep
      4                     Rest                   Morning                  No Sleep
      
      -- es_mean_difference --
               type outcome_variable_name grouping_variable_name
      1  Comparison    False Memory Score           Testing Time
      2   Reference    False Memory Score           Testing Time
      3  Difference    False Memory Score           Testing Time
      4  Comparison    False Memory Score                   Rest
      5   Reference    False Memory Score                   Rest
      6  Difference    False Memory Score                   Rest
      7  Comparison    False Memory Score                   Rest
      8   Reference    False Memory Score                   Rest
      9  Difference    False Memory Score                   Rest
      10 Comparison    False Memory Score                   Rest
      11  Reference    False Memory Score                   Rest
      12 Difference    False Memory Score                   Rest
      13 Comparison    False Memory Score            Interaction
      14  Reference    False Memory Score            Interaction
      15 Difference    False Memory Score            Interaction
                                   effect effect_size          LL        UL        SE
      1                           Morning        1.80  1.35221241 2.2477876 0.2227969
      2                           Evening        1.32  0.98790888 1.6520911 0.1648426
      3                        Difference        0.48 -0.07078234 1.0307823 0.2771489
      4                          No Sleep        1.68  1.29645382 2.0635462 0.1897367
      5                             Sleep        1.44  1.03387273 1.8461273 0.2020187
      6                        Difference        0.24 -0.31078234 0.7907823 0.2771489
      7                          No Sleep        2.22  1.54143331 2.8985667 0.3294751
      8                             Sleep        1.38  0.76083043 1.9991696 0.3000000
      9                        Difference        0.84 -0.05557518 1.7355752 0.4455938
      10                         No Sleep        1.14  0.75224761 1.5277524 0.1882715
      11                            Sleep        1.50  0.94260594 2.0573941 0.2706403
      12                       Difference       -0.36 -1.02418224 0.3041822 0.3296852
      13 Simple effect of Rest at Morning        0.84 -0.05557518 1.7355752 0.4455938
      14 Simple effect of Rest at Evening       -0.36 -1.02418224 0.3041822 0.3296852
      15        Difference of differences        1.20  0.09843532 2.3015647 0.5542979
               df       ta_LL     ta_UL
      1  48.73990  1.42643123 2.1735688
      2  44.60524  1.04310735 1.5968927
      3  87.91926  0.01927583 0.9407242
      4  39.75348  1.36046476 1.9995352
      5  48.26899  1.10120657 1.7787934
      6  87.91926 -0.22072417 0.7007242
      7  25.00000  1.65721014 2.7827899
      8  24.00000  0.86673538 1.8932646
      9  48.73990  0.09286247 1.5871375
      10 25.00000  0.81840579 1.4615942
      11 25.00000  1.03770833 1.9622917
      12 44.60524 -0.91378530 0.1937853
      13 48.73990  0.09286247 1.5871375
      14 44.60524 -0.91378530 0.1937853
      15 87.91926  0.27855166 2.1214483
                                            effect_type           effects_complex
      1                     Main effect of Testing Time                   Morning
      2                     Main effect of Testing Time                   Evening
      3                     Main effect of Testing Time         Morning ‒ Evening
      4                             Main effect of Rest                  No Sleep
      5                             Main effect of Rest                     Sleep
      6                             Main effect of Rest          No Sleep ‒ Sleep
      7  Simple effect of Rest at Testing Time: Morning         Morning: No Sleep
      8  Simple effect of Rest at Testing Time: Morning            Morning: Sleep
      9  Simple effect of Rest at Testing Time: Morning Morning: No Sleep ‒ Sleep
      10 Simple effect of Rest at Testing Time: Evening         Evening: No Sleep
      11 Simple effect of Rest at Testing Time: Evening            Evening: Sleep
      12 Simple effect of Rest at Testing Time: Evening Evening: No Sleep ‒ Sleep
      13           Interaction of Testing Time and Rest Morning: No Sleep ‒ Sleep
      14           Interaction of Testing Time and Rest Evening: No Sleep ‒ Sleep
      15           Interaction of Testing Time and Rest Difference of differences
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                    effect
      1    False Memory Score           Testing Time                Difference
      2    False Memory Score                   Rest                Difference
      3    False Memory Score                   Rest                Difference
      4    False Memory Score                   Rest                Difference
      5    False Memory Score            Interaction Difference of differences
        effect_size          LL        UL numerator denominator        SE df
      1   0.3389929 -0.05593994 0.7391145      0.48    1.405205 0.2028237 99
      2   0.1694965 -0.22432486 0.5659121      0.24    1.405205 0.2015948 99
      3   0.5932376 -0.04238852 1.2379439      0.84    1.405205 0.3266214 99
      4  -0.2542447 -0.72666264 0.2142817     -0.36    1.405205 0.2400412 99
      5   0.8474823  0.05531974 1.6526166      1.20    1.405205 0.4074812 99
          d_biased                                    effect_type
      1  0.3415873                    Main effect of Testing Time
      2  0.1707936                            Main effect of Rest
      3  0.5977777 Simple effect of Rest at Testing Time: Morning
      4 -0.2561904 Simple effect of Rest at Testing Time: Evening
      5  0.8539682           Interaction of Testing Time and Rest
                  effects_complex
      1         Morning ‒ Evening
      2          No Sleep ‒ Sleep
      3 Morning: No Sleep ‒ Sleep
      4 Evening: No Sleep ‒ Sleep
      5 Difference of differences
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Test different call types to estimate_mdiff_2x2between and compare to statpsych::ci.lc.median.bs

    Code
      estimates
    Output
      Analysis of raw data:
      Data frame = structure(list(v1 = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), levels = c("Music", "Silence"
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = ), class = "factor"), v2 = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("Calm", "Stressed"
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = ), class = "factor"), o = c(5, 5, 4, 4, 2, 5, 6, 5, 10, 10, 8, 
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = 9, 11, 9, 10, 8, 8, 11, 10, 9, 8, 9, 10, 9, 15, 15, 13, 14, 15, 
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
       Analysis of raw data:
      Data frame = 15, 13, 15)), row.names = c(NA, 32L), class = "data.frame")
      Outcome variable(s) = o, o, o, o, o
      Grouping variable(s) = v1 - v2
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level   mean
      1                     o                     v2            Music - Calm  9.250
      2                     o                     v2        Music - Stressed 14.375
      3                     o                     v2          Silence - Calm  4.500
      4                     o                     v2      Silence - Stressed  9.375
          mean_LL   mean_UL median median_LL median_UL        sd min max    q1 q3 n
      1  8.384636 10.115364    9.0  7.894785  10.10521 1.0350983   8  11  8.75 10 8
      2 13.609100 15.140900   15.0 14.263190  15.73681 0.9161254  13  15 13.75 15 8
      3  3.500764  5.499236    5.0  3.526380   6.47362 1.1952286   2   6  4.00  5 8
      4  8.488266 10.261734    9.5  8.394785  10.60521 1.0606602   8  11  8.75 10 8
        missing df   mean_SE median_SE grouping_variable_A_name
      1       0  7 0.3659625 0.5638955                       v1
      2       0  7 0.3238992 0.3759303                       v1
      3       0  7 0.4225771 0.7518606                       v1
      4       0  7 0.3750000 0.5638955                       v1
        grouping_variable_B_name grouping_variable_A_level grouping_variable_B_level
      1                       v2                     Music                      Calm
      2                       v2                     Music                  Stressed
      3                       v2                   Silence                      Calm
      4                       v2                   Silence                  Stressed
      
      -- es_mean_difference --
               type outcome_variable_name grouping_variable_name
      1  Comparison                     o                     v1
      2   Reference                     o                     v1
      3  Difference                     o                     v1
      4  Comparison                     o                     v2
      5   Reference                     o                     v2
      6  Difference                     o                     v2
      7  Comparison                     o                     v2
      8   Reference                     o                     v2
      9  Difference                     o                     v2
      10 Comparison                     o                     v2
      11  Reference                     o                     v2
      12 Difference                     o                     v2
      13 Comparison                     o            Interaction
      14  Reference                     o            Interaction
      15 Difference                     o            Interaction
                                 effect effect_size        LL        UL        SE
      1                         Silence      6.9375  6.330821  7.544179 0.2824874
      2                           Music     11.8125 11.287682 12.337318 0.2443559
      3                      Difference     -4.8750 -5.641320 -4.108680 0.3735089
      4                        Stressed     11.8750 11.342556 12.407444 0.2477578
      5                            Calm      6.8750  6.274364  7.475636 0.2795085
      6                      Difference      5.0000  4.233680  5.766320 0.3735089
      7                        Stressed      9.3750  8.488266 10.261734 0.3750000
      8                            Calm      4.5000  3.500764  5.499236 0.4225771
      9                      Difference      4.8750  3.661641  6.088359 0.5649747
      10                       Stressed     14.3750 13.609100 15.140900 0.3238992
      11                           Calm      9.2500  8.384636 10.115364 0.3659625
      12                     Difference      5.1250  4.075364  6.174636 0.4887119
      13 Simple effect of v2 at Silence      4.8750  3.661641  6.088359 0.5649747
      14   Simple effect of v2 at Music      5.1250  4.075364  6.174636 0.4887119
      15      Difference of differences     -0.2500 -1.782640  1.282640 0.7470179
               df     ta_LL     ta_UL                        effect_type
      1  13.80490  6.439455  7.435545                  Main effect of v1
      2  13.79635 11.381664 12.243336                  Main effect of v1
      3  27.04290 -5.511158 -4.238842                  Main effect of v1
      4  13.70994 11.437968 12.312032                  Main effect of v2
      5  13.72000  6.381988  7.368012                  Main effect of v2
      6  27.04290  4.363842  5.636158                  Main effect of v2
      7   7.00000  8.664533 10.085467 Simple effect of v2 at v1: Silence
      8   7.00000  3.699394  5.300606 Simple effect of v2 at v1: Silence
      9  13.80490  3.878910  5.871090 Simple effect of v2 at v1: Silence
      10  7.00000 13.761347 14.988653   Simple effect of v2 at v1: Music
      11  7.00000  8.556655  9.943345   Simple effect of v2 at v1: Music
      12 13.79635  4.263328  5.986672   Simple effect of v2 at v1: Music
      13 13.80490  3.878910  5.871090           Interaction of v1 and v2
      14 13.79635  4.263328  5.986672           Interaction of v1 and v2
      15 27.04290 -1.522315  1.022315           Interaction of v1 and v2
                   effects_complex
      1                    Silence
      2                      Music
      3            Silence ‒ Music
      4                   Stressed
      5                       Calm
      6            Stressed ‒ Calm
      7          Silence: Stressed
      8              Silence: Calm
      9   Silence: Stressed ‒ Calm
      10           Music: Stressed
      11               Music: Calm
      12    Music: Stressed ‒ Calm
      13  Silence: Stressed ‒ Calm
      14    Music: Stressed ‒ Calm
      15 Difference of differences
      
      -- es_median_difference --
               type outcome_variable_name grouping_variable_name
      1  Comparison                     o                     v1
      2   Reference                     o                     v1
      3  Difference                     o                     v1
      4  Comparison                     o                     v2
      5   Reference                     o                     v2
      6  Difference                     o                     v2
      7  Comparison                     o                     v2
      8   Reference                     o                     v2
      9  Difference                     o                     v2
      10 Comparison                     o                     v2
      11  Reference                     o                     v2
      12 Difference                     o                     v2
      13 Comparison                     o            Interaction
      14  Reference                     o            Interaction
      15 Difference                     o            Interaction
                                 effect effect_size        LL         UL        SE
      1                         Silence        7.25  6.328988  8.1710123 0.4699129
      2                           Music       12.00 11.335849 12.6641514 0.3388590
      3                      Difference       -4.75 -5.885500 -3.6144997 0.5793475
      4                        Stressed       12.25 11.585849 12.9141514 0.3388590
      5                            Calm        7.00  6.078988  7.9210123 0.4699129
      6                      Difference        5.25  4.114500  6.3855003 0.5793475
      7                        Stressed        9.50  8.394785 10.6052148 0.5638955
      8                            Calm        5.00  3.526380  6.4736198 0.7518606
      9                      Difference        4.50  2.657975  6.3420247 0.9398258
      10                       Stressed       15.00 14.263190 15.7368099 0.3759303
      11                           Calm        9.00  7.894785 10.1052148 0.5638955
      12                     Difference        6.00  4.671697  7.3283029 0.6777180
      13 Simple effect of v2 at Silence        4.50  2.657975  6.3420247 0.9398258
      14   Simple effect of v2 at Music        6.00  4.671697  7.3283029 0.6777180
      15      Difference of differences       -1.50 -3.771001  0.7710006 1.1586950
             ta_LL      ta_UL                        effect_type
      1   6.477062  8.0229379                  Main effect of v1
      2  11.442627 12.5573735                  Main effect of v1
      3  -5.702942 -3.7970581                  Main effect of v1
      4  11.692627 12.8073735                  Main effect of v2
      5   6.227062  7.7729379                  Main effect of v2
      6   4.297058  6.2029419                  Main effect of v2
      7   8.572474 10.4275255 Simple effect of v2 at v1: Silence
      8   3.763299  6.2367007 Simple effect of v2 at v1: Silence
      9   2.954124  6.0458759 Simple effect of v2 at v1: Silence
      10 14.381650 15.6183503   Simple effect of v2 at v1: Music
      11  8.072474  9.9275255   Simple effect of v2 at v1: Music
      12  4.885253  7.1147469   Simple effect of v2 at v1: Music
      13  2.954124  6.0458759           Interaction of v1 and v2
      14  4.885253  7.1147469           Interaction of v1 and v2
      15 -3.405884  0.4058838           Interaction of v1 and v2
                   effects_complex
      1                    Silence
      2                      Music
      3            Silence ‒ Music
      4                   Stressed
      5                       Calm
      6            Stressed ‒ Calm
      7          Silence: Stressed
      8              Silence: Calm
      9   Silence: Stressed ‒ Calm
      10           Music: Stressed
      11               Music: Calm
      12    Music: Stressed ‒ Calm
      13  Silence: Stressed ‒ Calm
      14    Music: Stressed ‒ Calm
      15 Difference of differences
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                    effect
      1                     o                     v1                Difference
      2                     o                     v2                Difference
      3                     o                     v2                Difference
      4                     o                     v2                Difference
      5                     o            Interaction Difference of differences
        effect_size        LL        UL numerator denominator        SE df   d_biased
      1  -4.4898249 -6.050227 -3.178857    -4.875    1.056443 0.7325059 28 -4.6145422
      2   4.6049486  3.270077  6.195651     5.000    1.056443 0.7463337 28  4.7328638
      3   4.4898249  2.950805  6.278279     4.875    1.056443 0.8488610 28  4.6145422
      4   4.7200723  3.235320  6.467051     5.125    1.056443 0.8244361 28  4.8511854
      5  -0.2302474 -1.719578  1.246292    -0.250    1.056443 0.7566135 28 -0.2366432
                               effect_type           effects_complex
      1                  Main effect of v1           Silence ‒ Music
      2                  Main effect of v2           Stressed ‒ Calm
      3 Simple effect of v2 at v1: Silence  Silence: Stressed ‒ Calm
      4   Simple effect of v2 at v1: Music    Music: Stressed ‒ Calm
      5           Interaction of v1 and v2 Difference of differences
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

