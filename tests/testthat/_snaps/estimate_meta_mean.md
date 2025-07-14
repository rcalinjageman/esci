# Compare meta_mean to ESCI_Original 7

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size       LL       UL       SE k diamond_ratio
      1    My effect     443.086 412.6203 473.5517 15.54402 7      1.416818
        diamond_ratio_LL diamond_ratio_UL I2 I2_LL I2_UL PI_LL PI_UL             p
      1                1         3.099506 NA    NA    NA    NA    NA 1.008768e-178
           width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 60.93144        443.086       442.3668    60.93144    86.32876
      
      -- es_heterogeneity --
        effect_label moderator_variable_name moderator_level       measure estimate
      1    My effect            My moderator         Overall Diamond Ratio 1.416818
        LL       UL
      1  1 3.099506
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_re
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size       LL       UL       SE k diamond_ratio
      1    My effect    442.3668 399.2024 485.5311 22.02305 7      1.416818
        diamond_ratio_LL diamond_ratio_UL       I2 I2_LL    I2_UL    PI_LL    PI_UL
      1                1         3.099506 42.91989     0 88.26186 358.0917 526.6418
                   p    width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 9.677849e-90 86.32876        443.086       442.3668    60.93144    86.32876
      
      -- es_heterogeneity --
             effect_label moderator_variable_name moderator_level       measure
      1         My effect            My moderator         Overall Diamond Ratio
      H^2       My effect            My moderator         Overall           H^2
      I^2(%)    My effect            My moderator         Overall        I^2(%)
      tau       My effect            My moderator         Overall           tau
      tau^2     My effect            My moderator         Overall         tau^2
                estimate LL           UL
      1         1.416818  1     3.099506
      H^2       1.751924  1     8.519236
      I^2(%)   42.919887  0    88.261859
      tau      36.930157  0   116.783411
      tau^2  1363.836510  0 13638.365101
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_90
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size       LL       UL       SE k diamond_ratio
      1    My effect     443.086 417.5184 468.6536 15.54402 7      1.416818
        diamond_ratio_LL diamond_ratio_UL I2 I2_LL I2_UL PI_LL PI_UL             p
      1                1         3.099506 NA    NA    NA    NA    NA 1.008768e-178
           width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 51.13527        443.086       442.3668    51.13527    72.44938
      
      -- es_heterogeneity --
        effect_label moderator_variable_name moderator_level       measure estimate
      1    My effect            My moderator         Overall Diamond Ratio 1.416818
        LL       UL
      1  1 3.099506
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 90% expected coverage.

---

    Code
      estimate_90
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label effect_size       LL       UL       SE k diamond_ratio
      1    My effect    442.3668 406.1421 478.5915 22.02305 7      1.416818
        diamond_ratio_LL diamond_ratio_UL       I2 I2_LL    I2_UL    PI_LL    PI_UL
      1                1         3.099506 42.91989     0 88.26186 371.6409 513.0926
                   p    width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 9.677849e-90 72.44938        443.086       442.3668    51.13527    72.44938
      
      -- es_heterogeneity --
             effect_label moderator_variable_name moderator_level       measure
      1         My effect            My moderator         Overall Diamond Ratio
      H^2       My effect            My moderator         Overall           H^2
      I^2(%)    My effect            My moderator         Overall        I^2(%)
      tau       My effect            My moderator         Overall           tau
      tau^2     My effect            My moderator         Overall         tau^2
                estimate LL           UL
      1         1.416818  1     3.099506
      H^2       1.751924  1     8.519236
      I^2(%)   42.919887  0    88.261859
      tau      36.930157  0   116.783411
      tau^2  1363.836510  0 13638.365101
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 90% expected coverage.

