# estimate_rdiff_two compared to ESCI_Two_Correlations, summary data

    Code
      estimate
    Output
      Analysis of summary data:
      
      -- es_r --
        grouping_variable_name grouping_variable_level        x_variable_name
      1                 Gender                 Females Satisfaction with life
      2                 Gender                   Males Satisfaction with life
          y_variable_name  effect effect_size        LL        UL        SE  n df
      1 Body satisfaction Females        0.41 0.1685419 0.6005378 0.1092338 59 57
      2 Body satisfaction   Males        0.53 0.2811300 0.7063492 0.1060255 47 45
            ta_LL     ta_UL
      1 0.2091420 0.5729339
      2 0.3242716 0.6817387
      
      -- es_r_difference --
              type grouping_variable_name grouping_variable_level
      1 Comparison                 Gender                   Males
      2  Reference                 Gender                 Females
      3 Difference                 Gender       Males  -  Females
               x_variable_name   y_variable_name            effect effect_size
      1 Satisfaction with life Body satisfaction             Males        0.53
      2 Satisfaction with life Body satisfaction           Females        0.41
      3 Satisfaction with life Body satisfaction Males  -  Females        0.12
                LL        UL        SE  n df      ta_LL     ta_UL        rz       sem
      1  0.2811300 0.7063492 0.1060255 47 45  0.3242716 0.6817387 0.5901452 0.1507557
      2  0.1685419 0.6005378 0.1092338 59 57  0.2091420 0.5729339 0.4356112 0.1336306
      3 -0.1934341 0.4190001        NA NA NA -0.1424341 0.3717311 0.1545339 0.2014557
                z            p
      1 3.9145801 9.056166e-05
      2 3.2598159 1.114846e-03
      3 0.7670863 4.430302e-01
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# estimate_rdiff_two of from Thomason1, ESCI_Scatterplots

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      
      ---Overview---
        outcome_variable_name grouping_variable_name grouping_variable_level     mean
      1                ls_pre                  major              Humanities 12.83333
      2                ls_pre                  major                 Science 10.33333
      3               ls_post                  major              Humanities 14.66667
      4               ls_post                  major                 Science 11.83333
          mean_LL  mean_UL median median_LL median_UL       sd min max    q1    q3 n
      1 10.063406 15.60326   12.5  8.860115  16.13988 2.639444   9  17 12.00 13.75 6
      2  6.482224 14.18444   10.0  5.905130  14.09487 3.669696   6  15  7.50 13.25 6
      3 12.399627 16.93371   14.5 11.770086  17.22991 2.160247  12  18 13.25 15.75 6
      4  8.690862 14.97580   11.5  7.860115  15.13988 2.994439   8  16 10.00 13.75 6
        missing df   mean_SE median_SE
      1       0  5 1.0775487  1.857118
      2       0  5 1.4981470  2.089258
      3       0  5 0.8819171  1.392839
      4       0  5 1.2224747  1.857118
      
      -- es_r_difference --
              type grouping_variable_name grouping_variable_level x_variable_name
      1 Comparison                  major                 Science          ls_pre
      2  Reference                  major              Humanities          ls_pre
      3 Difference                  major  Science  -  Humanities          ls_pre
        y_variable_name                 effect effect_size         LL        UL
      1         ls_post                Science  0.87969308  0.1536278 0.9842484
      2         ls_post             Humanities  0.86521602  0.0953317 0.9822849
      3         ls_post Science  -  Humanities  0.01447705 -0.7209657 0.7914286
               SE  n df      ta_LL     ta_UL        rz       sem          z
      1 0.1011329  6  4  0.3246022 0.9774135 1.3744088 0.5773503 2.38054590
      2 0.1124300  6  4  0.2706371 0.9746090 1.3137294 0.5773503 2.27544611
      3        NA NA NA -0.5512903 0.6170328 0.0606794 0.8164966 0.07431678
                 p
      1 0.01728701
      2 0.02287918
      3 0.94075833
      
      -- es_r --
        grouping_variable_name grouping_variable_level x_variable_name
      1                  major              Humanities          ls_pre
      2                  major                 Science          ls_pre
        y_variable_name     effect effect_size        LL        UL        SE n df
      1         ls_post Humanities   0.8652160 0.0953317 0.9822849 0.1124300 6  4
      2         ls_post    Science   0.8796931 0.1536278 0.9842484 0.1011329 6  4
            ta_LL     ta_UL
      1 0.2706371 0.9746090
      2 0.3246022 0.9774135
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Compare estimate_rdiff_two to statpsych::ci.cor

    Code
      estimate
    Output
      Analysis of summary data:
      
      -- es_r --
        grouping_variable_name grouping_variable_level        x_variable_name
      1                 Gender                 Females Satisfaction with life
      2                 Gender                   Males Satisfaction with life
          y_variable_name  effect effect_size        LL        UL         SE   n df
      1 Body satisfaction Females         0.6 0.4041348 0.7405304 0.06432242 100 98
      2 Body satisfaction   Males         0.6 0.3981150 0.7436090 0.06601096  95 93
            ta_LL     ta_UL
      1 0.4251094 0.7288743
      2 0.4197736 0.7317547
      
      -- es_r_difference --
              type grouping_variable_name grouping_variable_level
      1 Comparison                 Gender                   Males
      2  Reference                 Gender                 Females
      3 Difference                 Gender       Males  -  Females
               x_variable_name   y_variable_name            effect effect_size
      1 Satisfaction with life Body satisfaction             Males         0.6
      2 Satisfaction with life Body satisfaction           Females         0.6
      3 Satisfaction with life Body satisfaction Males  -  Females         0.0
                LL        UL         SE   n df      ta_LL     ta_UL        rz
      1  0.3981150 0.7436090 0.06601096  95 93  0.4197736 0.7317547 0.6931472
      2  0.4041348 0.7405304 0.06432242 100 98  0.4251094 0.7288743 0.6931472
      3 -0.2459803 0.2428719         NA  NA NA -0.2215630 0.2189658 0.0000000
              sem        z            p
      1 0.1042572 6.648434 2.962274e-11
      2 0.1015346 6.826708 8.688528e-12
      3 0.1455295 0.000000 1.000000e+00
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

