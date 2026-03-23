# estimate_rdiff_two compared to ESCI_Two_Correlations, summary data

    Code
      estimate
    Output
      Analysis of summary data:
      
      -- es_r --
        grouping_variable_name grouping_variable_level        x_variable_name
      1                 Gender                 Females Satisfaction with life
      2                 Gender                   Males Satisfaction with life
          y_variable_name  effect effect_size     LL     UL      SE  n df  ta_LL
      1 Body satisfaction Females        0.41 0.1685 0.6005 0.10923 59 57 0.2091
      2 Body satisfaction   Males        0.53 0.2811 0.7063 0.10603 47 45 0.3243
         ta_UL
      1 0.5729
      2 0.6817
      
      -- es_r_difference --
              type grouping_variable_name grouping_variable_level
      1 Comparison                 Gender                   Males
      2  Reference                 Gender                 Females
      3 Difference                 Gender       Males  -  Females
               x_variable_name   y_variable_name            effect effect_size
      1 Satisfaction with life Body satisfaction             Males        0.53
      2 Satisfaction with life Body satisfaction           Females        0.41
      3 Satisfaction with life Body satisfaction Males  -  Females        0.12
             LL     UL      SE  n df   ta_LL  ta_UL        rz       sem         z
      1  0.2811 0.7063 0.10603 47 45  0.3243 0.6817 0.5901452 0.1507557 3.9145801
      2  0.1685 0.6005 0.10923 59 57  0.2091 0.5729 0.4356112 0.1336306 3.2598159
      3 -0.1934 0.4190      NA NA NA -0.1424 0.3717 0.1545339 0.2014557 0.7670863
                   p
      1 9.056166e-05
      2 1.114846e-03
      3 4.430302e-01
      
      
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
        y_variable_name                 effect effect_size      LL     UL      SE  n
      1         ls_post                Science      0.8797  0.1536 0.9842 0.10113  6
      2         ls_post             Humanities      0.8652  0.0953 0.9823 0.11243  6
      3         ls_post Science  -  Humanities      0.0145 -0.7210 0.7914      NA NA
        df   ta_LL  ta_UL         rz       sem          z          p
      1  4  0.3246 0.9774 1.37443942 0.5773503 2.38059891 0.01728452
      2  4  0.2706 0.9746 1.31366568 0.5773503 2.27533571 0.02288580
      3 NA -0.5513 0.6170 0.06077374 0.8164966 0.07443233 0.94066639
      
      -- es_r --
        grouping_variable_name grouping_variable_level x_variable_name
      1                  major              Humanities          ls_pre
      2                  major                 Science          ls_pre
        y_variable_name     effect effect_size     LL     UL      SE n df  ta_LL
      1         ls_post Humanities      0.8652 0.0953 0.9823 0.11243 6  4 0.2706
      2         ls_post    Science      0.8797 0.1536 0.9842 0.10113 6  4 0.3246
         ta_UL
      1 0.9746
      2 0.9774
      
      
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
          y_variable_name  effect effect_size     LL     UL      SE   n df  ta_LL
      1 Body satisfaction Females         0.6 0.4041 0.7405 0.06432 100 98 0.4251
      2 Body satisfaction   Males         0.6 0.3981 0.7436 0.06601  95 93 0.4198
         ta_UL
      1 0.7289
      2 0.7318
      
      -- es_r_difference --
              type grouping_variable_name grouping_variable_level
      1 Comparison                 Gender                   Males
      2  Reference                 Gender                 Females
      3 Difference                 Gender       Males  -  Females
               x_variable_name   y_variable_name            effect effect_size
      1 Satisfaction with life Body satisfaction             Males         0.6
      2 Satisfaction with life Body satisfaction           Females         0.6
      3 Satisfaction with life Body satisfaction Males  -  Females         0.0
             LL     UL      SE   n df   ta_LL  ta_UL        rz       sem        z
      1  0.3981 0.7436 0.06601  95 93  0.4198 0.7318 0.6931472 0.1042572 6.648434
      2  0.4041 0.7405 0.06432 100 98  0.4251 0.7289 0.6931472 0.1015346 6.826708
      3 -0.2460 0.2429      NA  NA NA -0.2216 0.2190 0.0000000 0.1455295 0.000000
                   p
      1 2.962274e-11
      2 8.688528e-12
      3 1.000000e+00
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

