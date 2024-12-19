# Some tests for meta_r; needs development 

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size        LL        UL         SE  k diamond_ratio
      1    My effect    0.427545 0.3751808 0.4771892 0.03186265 16      2.033423
        diamond_ratio_LL diamond_ratio_UL I2 I2_LL I2_UL PI_LL PI_UL            p
      1         1.472544         3.195625 NA    NA    NA    NA    NA 1.242679e-46
            width FE_effect_size RE_effect_size FE_CI_width RE_CI_width         z
      1 0.1248993       0.427545      0.4060902   0.1248993   0.2539731 0.4568887
      
      -- es_heterogeneity --
        effect_label moderator_variable_name moderator_level       measure estimate
      1    My effect            My moderator         Overall Diamond Ratio 2.033423
              LL       UL
      1 1.472544 3.195625
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

