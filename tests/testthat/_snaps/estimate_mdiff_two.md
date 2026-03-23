# Compare estimate_mdiff_two to ESCI_Data_two, penlaptop1

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = % Transcription
      Grouping variable(s) = Note-taking type
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level  mean
      1       % Transcription       Note-taking type              Ref-Laptop  6.88
      2       % Transcription       Note-taking type                Comp-Pen 12.09
          mean_LL   mean_UL   sd   n  df   mean_SE
      1  5.412432  8.347568 4.22  48 149 0.7426914
      2 11.088156 13.091844 5.52 103 149 0.5070029
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name                effect
      1 Comparison       % Transcription       Note-taking type              Comp-Pen
      2  Reference       % Transcription       Note-taking type            Ref-Laptop
      3 Difference       % Transcription       Note-taking type Comp-Pen ‒ Ref-Laptop
        effect_size        LL        UL        SE  df     ta_LL     ta_UL
      1       12.09 11.088156 13.091844 0.5070029 149 11.250837 12.929163
      2        6.88  5.412432  8.347568 0.7426914 149  5.650738  8.109262
      3        5.21  3.433079  6.986921 0.8992455 149  3.721619  6.698381
      
      -- es_smd --
        outcome_variable_name grouping_variable_name                effect
      1       % Transcription       Note-taking type Comp-Pen ‒ Ref-Laptop
        effect_size        LL       UL numerator denominator        SE  df d_biased
      1    1.007425 0.6448059 1.366989      5.21    5.145517 0.1842337 149 1.012532
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_99
    Output
      Analysis of raw data:
      Outcome variable(s) = % Transcription
      Grouping variable(s) = Note-taking type
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level  mean
      1       % Transcription       Note-taking type                  Laptop  6.88
      2       % Transcription       Note-taking type                     Pen 12.09
          mean_LL   mean_UL   sd   n  df   mean_SE
      1  4.942149  8.817851 4.22  48 149 0.7426914
      2 10.767114 13.412886 5.52 103 149 0.5070029
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name       effect
      1 Comparison       % Transcription       Note-taking type          Pen
      2  Reference       % Transcription       Note-taking type       Laptop
      3 Difference       % Transcription       Note-taking type Pen ‒ Laptop
        effect_size        LL        UL        SE  df     ta_LL     ta_UL
      1       12.09 10.767114 13.412886 0.5070029 149 10.897714 13.282286
      2        6.88  4.942149  8.817851 0.7426914 149  5.133461  8.626539
      3        5.21  2.863664  7.556336 0.8992455 149  3.095303  7.324697
      
      -- es_smd --
        outcome_variable_name grouping_variable_name       effect effect_size
      1       % Transcription       Note-taking type Pen ‒ Laptop    1.007425
               LL      UL numerator denominator        SE  df d_biased
      1 0.5315012 1.48061      5.21    5.145517 0.1842337 149 1.012532
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
                  test_type outcome_variable_name                effect null_words
      1 Nil Hypothesis Test       % Transcription Comp-Pen ‒ Ref-Laptop       0.00
        confidence       LL       UL                          CI
      1         95 3.433079 6.986921 95% CI [3.433079, 6.986921]
                             CI_compare        t  df            p p_result
      1 The 95% CI does not contain H_0 5.793746 149 3.951374e-08 p < 0.05
        null_decision                                           conclusion
      1    Reject H_0 At α = 0.05, 0.00 is not a plausible value of μ_diff
        significant
      1        TRUE
      

---

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = transcription
      Grouping variable(s) = condition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level
      1         transcription              condition                     Pen
      2         transcription              condition                  Laptop
             mean   mean_LL  mean_UL median median_LL median_UL       sd min  max
      1  8.811765  6.724559 10.89897    8.6  6.598482  10.60152 4.749339 1.0 20.1
      2 14.519355 12.333487 16.70522   12.8  9.469511  16.13049 7.285576 1.2 34.7
          q1     q3  n missing df  mean_SE median_SE
      1 5.20 11.275 34       0 63 1.044470  1.021201
      2 9.45 17.850 31       0 63 1.093842  1.699260
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name       effect
      1 Comparison         transcription              condition       Laptop
      2  Reference         transcription              condition          Pen
      3 Difference         transcription              condition Laptop ‒ Pen
        effect_size        LL        UL       SE df     ta_LL     ta_UL
      1   14.519355 12.333487 16.705223 1.093842 63 12.693293 16.345416
      2    8.811765  6.724559 10.898971 1.044470 63  7.068125 10.555405
      3    5.707590  2.685265  8.729915 1.512417 63  3.182757  8.232423
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name       effect
      1 Comparison         transcription              condition       Laptop
      2  Reference         transcription              condition          Pen
      3 Difference         transcription              condition Laptop ‒ Pen
        effect_size        LL        UL       SE     ta_LL     ta_UL
      1        12.8 9.4695114 16.130489 1.699260 10.004966 15.595034
      2         8.6 6.5984823 10.601518 1.021201  6.920273 10.279727
      3         4.2 0.3143563  8.085644 1.982508  0.939065  7.460935
      
      -- es_smd --
        outcome_variable_name grouping_variable_name       effect effect_size
      1         transcription              condition Laptop ‒ Pen   0.9259595
               LL       UL numerator denominator        SE df  d_biased
      1 0.4098655 1.435414   5.70759    6.090252 0.2616245 63 0.9371681
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      -- es_mean_ratio --
        outcome_variable_name grouping_variable_name       effect effect_size
      1         transcription              condition Laptop / Pen    1.647724
              LL       UL comparison_mean reference_mean
      1 1.273045 2.132677        14.51935       8.811765
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        outcome_variable_name grouping_variable_name       effect effect_size      LL
      1         transcription              condition Laptop / Pen    1.488372 1.06252
              UL comparison_median reference_median
      1 2.084903              12.8              8.6
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
                  test_type outcome_variable_name       effect null_words confidence
      1 Nil Hypothesis Test         transcription Laptop ‒ Pen       0.00         95
              LL       UL                          CI                      CI_compare
      1 2.685265 8.729915 95% CI [2.685265, 8.729915] The 95% CI does not contain H_0
              t df            p p_result null_decision
      1 3.77382 63 0.0003579282 p < 0.05    Reject H_0
                                                  conclusion significant
      1 At α = 0.05, 0.00 is not a plausible value of μ_diff        TRUE
      

---

    Code
      estimate_99
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = transcription
      Grouping variable(s) = condition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level
      1         transcription              condition                     Pen
      2         transcription              condition                  Laptop
             mean   mean_LL  mean_UL median median_LL median_UL       sd min  max
      1  8.811765  6.037502 11.58603    8.6  5.969560  11.23044 4.749339 1.0 20.1
      2 14.519355 11.613953 17.42476   12.8  8.422996  17.17700 7.285576 1.2 34.7
          q1     q3  n missing df  mean_SE median_SE
      1 5.20 11.275 34       0 63 1.044470  1.021201
      2 9.45 17.850 31       0 63 1.093842  1.699260
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name       effect
      1 Comparison         transcription              condition       Laptop
      2  Reference         transcription              condition          Pen
      3 Difference         transcription              condition Laptop ‒ Pen
        effect_size        LL       UL       SE df     ta_LL     ta_UL
      1   14.519355 11.613953 17.42476 1.093842 63 11.908346 17.130363
      2    8.811765  6.037502 11.58603 1.044470 63  6.318608 11.304922
      3    5.707590  1.690390  9.72479 1.512417 63  2.097438  9.317742
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name       effect
      1 Comparison         transcription              condition       Laptop
      2  Reference         transcription              condition          Pen
      3 Difference         transcription              condition Laptop ‒ Pen
        effect_size         LL        UL       SE      ta_LL     ta_UL
      1        12.8  8.4229960 17.177004 1.699260  8.8469299 16.753070
      2         8.6  5.9695599 11.230440 1.021201  6.2243306 10.975669
      3         4.2 -0.9066015  9.306602 1.982508 -0.4120026  8.812003
      
      -- es_smd --
        outcome_variable_name grouping_variable_name       effect effect_size
      1         transcription              condition Laptop ‒ Pen   0.9259595
               LL      UL numerator denominator        SE df  d_biased
      1 0.2490821 1.59688   5.70759    6.090252 0.2616245 63 0.9371681
      This standardized mean difference is called d_s because the standardizer used was s_p. d_s has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      -- es_mean_ratio --
        outcome_variable_name grouping_variable_name       effect effect_size
      1         transcription              condition Laptop / Pen    1.647724
              LL       UL comparison_mean reference_mean
      1 1.169397 2.321705        14.51935       8.811765
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        outcome_variable_name grouping_variable_name       effect effect_size
      1         transcription              condition Laptop / Pen    1.488372
               LL       UL comparison_median reference_median
      1 0.9557473 2.317821              12.8              8.6
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Compare estimate_mdiff_two to statpsych::ci.mean2 example

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1   My outcome variable   My grouping variable                 Control 10.3
      2   My outcome variable   My grouping variable                 Treated 15.4
          mean_LL  mean_UL   sd  n df   mean_SE
      1  8.924592 11.67541 2.15 20 19 0.4807546
      2 14.056336 16.74366 2.67 30 29 0.4874731
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison   My outcome variable   My grouping variable           Treated
      2  Reference   My outcome variable   My grouping variable           Control
      3 Difference   My outcome variable   My grouping variable Treated ‒ Control
        effect_size        LL        UL        SE    df     ta_LL     ta_UL
      1        15.4 14.056336 16.743664 0.4874731 29.00 14.199831 16.600169
      2        10.3  8.924592 11.675408 0.4807546 19.00  9.079132 11.520868
      3         5.1  3.260609  6.939391 0.6846568 46.17  3.450067  6.749933
      
      -- es_smd --
        outcome_variable_name grouping_variable_name            effect effect_size
      1   My outcome variable   My grouping variable Treated ‒ Control    2.070897
              LL       UL numerator denominator        SE       df d_biased
      1 1.187684 3.013583       5.1    2.423984 0.5604936 47.99877 2.103974
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Compare estimate_mdiff_two to statpsych::ci.median2 example

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = myoutcome
      Grouping variable(s) = mycondition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level mean
      1             myoutcome            mycondition                 Group 1 34.2
      2             myoutcome            mycondition                 Group 2 41.5
         mean_LL  mean_UL median median_LL median_UL       sd min max    q1    q3  n
      1 28.33027 40.06973   34.5  27.21846  41.78154 5.711587  26  43 29.75 38.50 10
      2 34.87072 48.12928   43.0  34.59823  51.40177 6.450667  31  49 36.75 46.75 10
        missing df  mean_SE median_SE
      1       0  9 1.806162  2.826871
      2       0  9 2.039880  3.261774
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison             myoutcome            mycondition           Group 1
      2  Reference             myoutcome            mycondition           Group 2
      3 Difference             myoutcome            mycondition Group 1 ‒ Group 2
        effect_size        LL         UL       SE    df     ta_LL      ta_UL
      1        34.2  28.33027 40.0697304 1.806162  9.00  29.10403 39.2959748
      2        41.5  34.87072 48.1292751 2.039880  9.00  35.74460 47.2553953
      3        -7.3 -15.15591  0.5559134 2.724579 17.74 -14.26400 -0.3359968
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison             myoutcome            mycondition           Group 1
      2  Reference             myoutcome            mycondition           Group 2
      3 Difference             myoutcome            mycondition Group 1 ‒ Group 2
        effect_size        LL        UL       SE     ta_LL     ta_UL
      1        34.5  27.21846 41.781536 2.826871  27.92372 41.076285
      2        43.0  34.59823 51.401773 3.261774  35.41198 50.588021
      3        -8.5 -19.61803  2.618029 4.316291 -18.54119  1.541194
      
      -- es_smd --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 ‒ Group 2   -1.146719
               LL         UL numerator denominator        SE       df  d_biased
      1 -2.448433 0.07467368      -7.3    6.092345 0.6550008 17.73989 -1.198225
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      -- es_mean_ratio --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 / Group 2   0.8240964
               LL      UL comparison_mean reference_mean
      1 0.6694745 1.01443            34.2           41.5
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 / Group 2   0.8023256
               LL      UL comparison_median reference_median
      1 0.5919106 1.08754              34.5               43
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

# Compare estimate_mdiff_two to statpsych::ci.ratio.mean2 and ci.ratio.median2 example

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = myoutcome
      Grouping variable(s) = mycondition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1             myoutcome            mycondition                 Group 1 41.50000
      2             myoutcome            mycondition                 Group 2 36.38462
         mean_LL  mean_UL median median_LL median_UL       sd min max    q1    q3  n
      1 34.87072 48.12928     43  34.59823  51.40177 6.450667  31  49 36.75 46.75 10
      2 30.67493 42.09430     37  29.66461  44.33539 6.739664  26  49 32.00 40.00 13
        missing df  mean_SE median_SE
      1       0  9 2.039880  3.261774
      2       0 12 1.869247  2.847778
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison             myoutcome            mycondition           Group 1
      2  Reference             myoutcome            mycondition           Group 2
      3 Difference             myoutcome            mycondition Group 1 ‒ Group 2
        effect_size        LL       UL       SE    df     ta_LL    ta_UL
      1   41.500000 34.870725 48.12928 2.039880  9.00 35.744605 47.25540
      2   36.384615 30.674928 42.09430 1.869247 12.00 31.373169 41.39606
      3    5.115385 -2.760385 12.99115 2.766802 19.92 -1.881447 12.11222
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison             myoutcome            mycondition           Group 1
      2  Reference             myoutcome            mycondition           Group 2
      3 Difference             myoutcome            mycondition Group 1 ‒ Group 2
        effect_size        LL       UL       SE     ta_LL    ta_UL
      1          43 34.598227 51.40177 3.261774 35.411979 50.58802
      2          37 29.664610 44.33539 2.847778 30.375078 43.62492
      3           6 -5.153373 17.15337 4.330013 -4.073115 16.07312
      
      -- es_smd --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 ‒ Group 2   0.7470702
                LL       UL numerator denominator       SE       df  d_biased
      1 -0.3558306 1.890873  5.115385    6.596749 0.517002 20.79173 0.7754403
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      -- es_mean_ratio --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 / Group 2    1.140592
               LL       UL comparison_mean reference_mean
      1 0.9324275 1.395229            41.5       36.38462
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 / Group 2    1.162162
               LL       UL comparison_median reference_median
      1 0.8642465 1.562773                43               37
      [1] "This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

---

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      Outcome variable(s) = myoutcome
      Grouping variable(s) = mycondition
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1             myoutcome            mycondition                 Group 1 23.70000
      2             myoutcome            mycondition                 Group 2 25.46154
          mean_LL  mean_UL median  median_LL median_UL       sd min max   q1   q3  n
      1 -2.399473 49.79947   37.5 -0.8577566  75.85776 36.48455 -47  49 31.5 45.5 10
      2  8.614096 42.30898   35.0 28.1304096  41.86959 27.87955 -39  49 27.0 40.0 13
        missing df   mean_SE median_SE
      1       0  9 11.537427 19.570644
      2       0 12  7.732396  3.504957
      
      -- es_mean_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison             myoutcome            mycondition           Group 1
      2  Reference             myoutcome            mycondition           Group 2
      3 Difference             myoutcome            mycondition Group 1 ‒ Group 2
        effect_size         LL       UL        SE    df      ta_LL    ta_UL
      1   23.700000  -2.399473 49.79947 11.537427  9.00   2.550593 44.84941
      2   25.461538   8.614096 42.30898  7.732396 12.00  11.680186 39.24289
      3   -1.761538 -31.143649 27.62057 13.888922 16.42 -25.971998 22.44892
      
      -- es_median_difference --
              type outcome_variable_name grouping_variable_name            effect
      1 Comparison             myoutcome            mycondition           Group 1
      2  Reference             myoutcome            mycondition           Group 2
      3 Difference             myoutcome            mycondition Group 1 ‒ Group 2
        effect_size          LL       UL        SE      ta_LL    ta_UL
      1        37.5  -0.8577566 75.85776 19.570644   5.309156 69.69084
      2        35.0  28.1304096 41.86959  3.504957  29.234858 40.76514
      3         2.5 -36.4680480 41.46805 19.882022 -30.203017 35.20302
      
      -- es_smd --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 ‒ Group 2 -0.05195366
                LL        UL numerator denominator        SE       df    d_biased
      1 -0.8920995 0.7850884 -1.761538    32.46838 0.4541995 17.98107 -0.05425397
      This standardized mean difference is called d_avg because the standardizer used was s_avg. d_avg has been corrected for bias. Correction for bias can be important when df < 50.  See the rightmost column for the biased value.
      
      -- es_mean_ratio --
        outcome_variable_name grouping_variable_name            effect effect_size
      1             myoutcome            mycondition Group 1 / Group 2   0.9308157
              LL       UL comparison_mean reference_mean
      1 0.275107 3.149385            23.7       25.46154
      [1] "WARNING!  Your dataset includes negative values.  This effect-size measure is appropriate only for true ratio scales."
      
      -- es_median_ratio --
        outcome_variable_name grouping_variable_name            effect effect_size LL
      1             myoutcome            mycondition Group 1 / Group 2          NA NA
        UL comparison_median reference_median
      1 NA                NA               NA
      [1] "WARNING!  Your dataset includes negative values.  This effect-size measure is appropriate only for true ratio scales."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.
      Warnings:
      * The ratio between group effect size is appropriate only for true ratio scales where values < 0 are impossible.  Your data include at least one negative value, so the requested ratio effect size is not reported.

