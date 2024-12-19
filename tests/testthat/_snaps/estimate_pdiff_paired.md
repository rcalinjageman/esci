# pdiff_paired tests - Bonett & Price 2012 - Example 1

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases   n    P      P_LL
      1             9th grade          Answered True   110 200 0.55 0.4807377
      2             9th grade         Answered False    90 200 0.45 0.3826985
      3            12th grade          Answered True    82 200 0.41 0.3442291
      4            12th grade         Answered False   118 200 0.59 0.5206996
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.6173015 0.03483836  0.5490196 0.4917156 0.6063236
      2 0.5192623 0.03483836  0.4509804 0.3936764 0.5082844
      3 0.4793004 0.03445760  0.4117647 0.3550870 0.4684424
      4 0.6557709 0.03445760  0.5882353 0.5315576 0.6449130
      
      -- es_proportion_difference --
              type comparison_measure_name reference_measure_name      case_label
      1 Comparison              12th grade              9th grade P_Answered True
      2  Reference              12th grade              9th grade P_Answered True
      3 Difference              12th grade              9th grade P_Answered True
                        effect effect_size         LL          UL         SE
      1             12th grade        0.41  0.3442291  0.47930036 0.03445760
      2              9th grade        0.55  0.4807377  0.61730153 0.03483836
      3 12th grade ‒ 9th grade       -0.14 -0.2198621 -0.05736562 0.04145395
        effect_size_adjusted      ta_LL       ta_UL
      1            0.4117647  0.3550870  0.46844241
      2            0.5490196  0.4917156  0.60632361
      3           -0.1386139 -0.2195936 -0.05763415
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# pdiff_paired tests - Bonett & Price 2012 - Example 2

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases  n         P      P_LL
      1       Before Hypnosis                No Pain    22 39 0.5641026 0.4097074
      2       Before Hypnosis                   Pain    17 39 0.4358974 0.2934284
      3        After Hypnosis                No Pain    30 39 0.7692308 0.6137743
      4        After Hypnosis                   Pain     9 39 0.2307692 0.1254022
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.7065716 0.07573205  0.5581395 0.4335714 0.6827077
      2 0.5902926 0.07573205  0.4418605 0.3172923 0.5664286
      3 0.8745978 0.06653783  0.7441860 0.6347411 0.8536310
      4 0.3862257 0.06653783  0.2558140 0.1463690 0.3652589
      
      -- es_proportion_difference --
              type comparison_measure_name reference_measure_name case_label
      1 Comparison          After Hypnosis        Before Hypnosis  P_No Pain
      2  Reference          After Hypnosis        Before Hypnosis  P_No Pain
      3 Difference          After Hypnosis        Before Hypnosis  P_No Pain
                                  effect effect_size          LL        UL         SE
      1                   After Hypnosis   0.7692308 0.613774292 0.8745978 0.06653783
      2                  Before Hypnosis   0.5641026 0.409707436 0.7065716 0.07573205
      3 After Hypnosis ‒ Before Hypnosis   0.2051282 0.001300316 0.3889436 0.09889041
        effect_size_adjusted      ta_LL     ta_UL
      1            0.7441860 0.63474105 0.8536310
      2            0.5581395 0.43357139 0.6827077
      3            0.1951220 0.02653624 0.3637077
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# pdiff_paired tests - Bonett & Price 2012 - Example 3

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases   n   P      P_LL
      1       Diagnostician 2          Schizophrenic    40 100 0.4 0.3095447
      2       Diagnostician 2      Not Schizoprhenic    60 100 0.6 0.5018524
      3       Diagnostician 1          Schizophrenic    60 100 0.6 0.5018524
      4       Diagnostician 1      Not Schizoprhenic    40 100 0.4 0.3095447
             P_UL       P_SE P_adjusted     ta_LL     ta_UL
      1 0.4981476 0.04811389  0.4038462 0.3247058 0.4829865
      2 0.6904553 0.04811389  0.5961538 0.5170135 0.6752942
      3 0.6904553 0.04811389  0.5961538 0.5170135 0.6752942
      4 0.4981476 0.04811389  0.4038462 0.3247058 0.4829865
      
      -- es_proportion_difference --
              type comparison_measure_name reference_measure_name      case_label
      1 Comparison         Diagnostician 1        Diagnostician 2 P_Schizophrenic
      2  Reference         Diagnostician 1        Diagnostician 2 P_Schizophrenic
      3 Difference         Diagnostician 1        Diagnostician 2 P_Schizophrenic
                                   effect effect_size         LL        UL         SE
      1                   Diagnostician 1         0.6 0.50185235 0.6904553 0.04811389
      2                   Diagnostician 2         0.4 0.30954466 0.4981476 0.04811389
      3 Diagnostician 1 ‒ Diagnostician 2         0.2 0.09425814 0.2978987 0.05195008
        effect_size_adjusted      ta_LL     ta_UL
      1            0.5961538 0.51701354 0.6752942
      2            0.4038462 0.32470585 0.4829865
      3            0.1960784 0.08315116 0.3090057
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# pdiff_paired tests - Bonett & Price 2012 - Example 4

    Code
      estimate
    Output
      Analysis of raw data:
      Outcome variable(s) = My outcome variable
      Grouping variable(s) = My grouping variable
      
      ---Overview---
        outcome_variable_name outcome_variable_level cases    n         P      P_LL
      1           Adult Court               Rearrest   673 2097 0.3209347 0.3013082
      2           Adult Court            No Rearrest  1424 2097 0.6790653 0.6587571
      3        Juvenile Court               Rearrest   448 2097 0.2136385 0.1966413
      4        Juvenile Court            No Rearrest  1649 2097 0.7863615 0.7682739
             P_UL        P_SE P_adjusted     ta_LL     ta_UL
      1 0.3412429 0.010187616  0.3212756 0.3045184 0.3380327
      2 0.6986918 0.010187616  0.6787244 0.6619673 0.6954816
      3 0.2317261 0.008950364  0.2141837 0.1994617 0.2289058
      4 0.8033587 0.008950364  0.7858163 0.7710942 0.8005383
      
      -- es_proportion_difference --
              type comparison_measure_name reference_measure_name case_label
      1 Comparison          Juvenile Court            Adult Court P_Rearrest
      2  Reference          Juvenile Court            Adult Court P_Rearrest
      3 Difference          Juvenile Court            Adult Court P_Rearrest
                              effect effect_size         LL          UL          SE
      1               Juvenile Court   0.2136385  0.1966413  0.23172611 0.008950364
      2                  Adult Court   0.3209347  0.3013082  0.34124294 0.010187616
      3 Juvenile Court ‒ Adult Court  -0.1072961 -0.1333205 -0.08106726 0.013330165
        effect_size_adjusted      ta_LL       ta_UL
      1            0.2141837  0.1994617  0.22890576
      2            0.3212756  0.3045184  0.33803272
      3           -0.1071939 -0.1295039 -0.08488393
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

