# Compare meta_d2 to ESCI_d_two_groups and d_subsets

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size        LL        UL        SE k diamond_ratio
      1    My effect    0.531961 0.2211897 0.8427323 0.1585597 8      1.598972
        diamond_ratio_LL diamond_ratio_UL       I2 I2_LL    I2_UL      PI_LL   PI_UL
      1                1         2.813104 53.89929     0 85.64031 -0.1620683 1.22599
                  p     width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 0.000793773 0.6215426      0.3736769       0.531961   0.3887139   0.6215426
      
      -- es_heterogeneity --
             effect_label moderator_variable_name moderator_level       measure
      1         My effect            My moderator         Overall Diamond Ratio
      H^2       My effect            My moderator         Overall           H^2
      I^2(%)    My effect            My moderator         Overall        I^2(%)
      tau       My effect            My moderator         Overall           tau
      tau^2     My effect            My moderator         Overall         tau^2
               estimate LL         UL
      1       1.5989717  1  2.8131044
      H^2     2.1691640  1  6.9639408
      I^2(%) 53.8992902  0 85.6403144
      tau     0.3166193  0  0.7150993
      tau^2   0.1002478  0  0.5113671
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size        LL        UL        SE k diamond_ratio
      1    My effect    0.531961 0.2211897 0.8427323 0.1585597 8      1.598972
        diamond_ratio_LL diamond_ratio_UL       I2 I2_LL    I2_UL      PI_LL   PI_UL
      1                1         2.813104 53.89929     0 85.64031 -0.1620683 1.22599
                  p     width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 0.000793773 0.6215426      0.3736769       0.531961   0.3887139   0.6215426
      
      -- es_heterogeneity --
             effect_label moderator_variable_name moderator_level       measure
      1         My effect            My moderator         Overall Diamond Ratio
      H^2       My effect            My moderator         Overall           H^2
      I^2(%)    My effect            My moderator         Overall        I^2(%)
      tau       My effect            My moderator         Overall           tau
      tau^2     My effect            My moderator         Overall         tau^2
               estimate LL         UL
      1       1.5989717  1  2.8131044
      H^2     2.1691640  1  6.9639408
      I^2(%) 53.8992902  0 85.6403144
      tau     0.3166193  0  0.7150993
      tau^2   0.1002478  0  0.5113671
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_vary
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label moderator_variable_name moderator_variable_level effect_size
      1    My effect                  subset                  Overall  0.31127135
      2    My effect                  subset                  Germany  0.80735059
      3    My effect                  subset                      USA  0.04858171
                LL        UL        SE k diamond_ratio diamond_ratio_LL
      1  0.1022717 0.5202710 0.1066344 8      1.470508                1
      2  0.4521458 1.1625553 0.1812302 6      1.000000                1
      3 -0.2098970 0.3070604 0.1318793 2      1.000000                1
        diamond_ratio_UL I2 I2_LL I2_UL PI_LL PI_UL            p     width
      1         2.655908 NA    NA    NA    NA    NA 3.510981e-03 0.4179992
      2         1.000000 NA    NA    NA    NA    NA 8.395823e-06 0.7104095
      3         1.000000 NA    NA    NA    NA    NA 7.125898e-01 0.5169573
        FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1     0.31127135     0.46308537   0.4179992   0.6146712
      2     0.80735059     0.80735059   0.7104095   0.7104095
      3     0.04858171     0.04858171   0.5169573   0.5169573
      
      -- es_heterogeneity --
         effect_label moderator_variable_name moderator_level       measure estimate
      1     My effect                  subset         Overall Diamond Ratio 1.470508
      11    My effect                  subset         Germany Diamond Ratio 1.000000
      12    My effect                  subset             USA Diamond Ratio 1.000000
         LL       UL
      1   1 2.655908
      11  1 1.000000
      12  1 1.000000
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      -- es_meta_difference --
                       type effect_label moderator_variable_name moderator_level
      Comparison Comparison    My effect                  subset             USA
      Reference   Reference    My effect                  subset         Germany
      Difference Difference    My effect                  subset   USA ‒ Germany
                 effect_size         LL         UL        SE            p
      Comparison  0.04858171 -0.2098970  0.3070604 0.1318793 7.125898e-01
      Reference   0.80735059  0.4521458  1.1625553 0.1812302 8.395823e-06
      Difference -0.75876888 -1.1980656 -0.3194721 0.2241351 7.109555e-04
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

