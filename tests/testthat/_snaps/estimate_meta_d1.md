# Tests for meta_d1

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size       LL       UL        SE k diamond_ratio
      1    My effect    2.450985 2.052157 2.849813 0.2034873 7      1.085254
        diamond_ratio_LL diamond_ratio_UL I2 I2_LL I2_UL PI_LL PI_UL            p
      1                1         2.490043 NA    NA    NA    NA    NA 2.063193e-33
            width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 0.7976557       2.450985       2.451144   0.7976557    0.865659
      
      -- es_heterogeneity --
        effect_label moderator_variable_name moderator_level       measure estimate
      1    My effect            My moderator         Overall Diamond Ratio 1.085254
        LL       UL
      1  1 2.490043
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_d1
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label moderator_variable_name moderator_variable_level effect_size
      1    My effect                  subset                  Overall    2.450985
      2    My effect                  subset                      00s    2.256909
      3    My effect                  subset                      90s    2.718832
              LL       UL        SE k diamond_ratio diamond_ratio_LL diamond_ratio_UL
      1 2.052157 2.849813 0.2034873 7      1.085254                1         2.490043
      2 1.733156 2.780663 0.2672261 4      1.080164                1         3.924416
      3 2.103536 3.334128 0.3139324 3      1.073667                1         8.742721
        I2 I2_LL I2_UL PI_LL PI_UL            p     width FE_effect_size
      1 NA    NA    NA    NA    NA 2.063193e-33 0.7976557       2.450985
      2 NA    NA    NA    NA    NA 3.022513e-17 1.0475070       2.256909
      3 NA    NA    NA    NA    NA 4.694309e-18 1.2305925       2.718832
        RE_effect_size FE_CI_width RE_CI_width
      1       2.451144   0.7976557    0.865659
      2       2.263560   1.0475070    1.131479
      3       2.706928   1.2305925    1.321246
      
      -- es_heterogeneity --
         effect_label moderator_variable_name moderator_level       measure estimate
      1     My effect                  subset         Overall Diamond Ratio 1.085254
      11    My effect                  subset             00s Diamond Ratio 1.080164
      12    My effect                  subset             90s Diamond Ratio 1.073667
         LL       UL
      1   1 2.490043
      11  1 3.924416
      12  1 8.742721
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      -- es_meta_difference --
                       type effect_label moderator_variable_name moderator_level
      Comparison Comparison    My effect                  subset             90s
      Reference   Reference    My effect                  subset             00s
      Difference Difference    My effect                  subset       90s ‒ 00s
                 effect_size         LL       UL        SE         p
      Comparison   2.7188322  2.1035360 3.334128 0.3139324 0.0000000
      Reference    2.2569090  1.7331555 2.780663 0.2672261 0.0000000
      Difference   0.4619232 -0.3461035 1.269950 0.4122661 0.2625224
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

