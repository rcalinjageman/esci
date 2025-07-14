# Compare meta_mdiff_two to ESCI_Original_two_groups

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
                                      effect_label effect_size         LL        UL
      1 Brain Photo Rating - No Brain Photo Rating  0.09431632 0.01498243 0.1736502
                SE  k diamond_ratio diamond_ratio_LL diamond_ratio_UL       I2 I2_LL
      1 0.04047722 10      1.397517                1          2.66465 34.93494     0
           I2_UL       PI_LL     PI_UL         p     width FE_effect_size
      1 82.43217 -0.06646225 0.2550949 0.0198004 0.1586678     0.06845248
        RE_effect_size FE_CI_width RE_CI_width
      1     0.09431632   0.1135355   0.1586678
      
      -- es_heterogeneity --
                                           effect_label moderator_variable_name
      1      Brain Photo Rating - No Brain Photo Rating            My moderator
      H^2    Brain Photo Rating - No Brain Photo Rating            My moderator
      I^2(%) Brain Photo Rating - No Brain Photo Rating            My moderator
      tau    Brain Photo Rating - No Brain Photo Rating            My moderator
      tau^2  Brain Photo Rating - No Brain Photo Rating            My moderator
             moderator_level       measure     estimate LL         UL
      1              Overall Diamond Ratio  1.397516897  1  2.6646505
      H^2            Overall           H^2  1.536923194  1  5.6922238
      I^2(%)         Overall        I^2(%) 34.934939901  0 82.4321736
      tau            Overall           tau  0.071349448  0  0.2109230
      tau^2          Overall         tau^2  0.005090744  0  0.0444885
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_fe
    Output
      Analysis of raw data:
      
      -- es_meta --
                                      effect_label effect_size         LL        UL
      1 Brain Photo Rating - No Brain Photo Rating  0.06845248 0.01168473 0.1252202
                SE  k diamond_ratio diamond_ratio_LL diamond_ratio_UL I2 I2_LL I2_UL
      1 0.02896367 10      1.397517                1          2.66465 NA    NA    NA
        PI_LL PI_UL          p     width FE_effect_size RE_effect_size FE_CI_width
      1    NA    NA 0.01810854 0.1135355     0.06845248     0.09431632   0.1135355
        RE_CI_width
      1   0.1586678
      
      -- es_heterogeneity --
                                      effect_label moderator_variable_name
      1 Brain Photo Rating - No Brain Photo Rating            My moderator
        moderator_level       measure estimate LL      UL
      1         Overall Diamond Ratio 1.397517  1 2.66465
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_mod
    Output
      Analysis of raw data:
      
      -- es_meta --
                                      effect_label moderator_variable_name
      1 Brain Photo Rating - No Brain Photo Rating                     mod
      2 Brain Photo Rating - No Brain Photo Rating                     mod
      3 Brain Photo Rating - No Brain Photo Rating                     mod
        moderator_variable_level effect_size          LL        UL         SE  k
      1                  Overall  0.09431632  0.01498243 0.1736502 0.04047722 10
      2                 Critique  0.21663310  0.08825315 0.3450130 0.06550118  4
      3                   Simple  0.03243698 -0.03085464 0.0957286 0.03229224  6
        diamond_ratio diamond_ratio_LL diamond_ratio_UL       I2 I2_LL    I2_UL
      1      1.397517                1         2.664650 34.93494     0 82.43217
      2      1.000000                1         3.079149  0.00000     0 88.41460
      3      1.000000                1         3.729327 14.90783     0 90.89257
              PI_LL     PI_UL            p     width FE_effect_size RE_effect_size
      1 -0.06646225 0.2550949 0.0198003989 0.1586678     0.06845248     0.09431632
      2  0.08825315 0.3450130 0.0009419452 0.2567599     0.21663310     0.21663310
      3 -0.03085464 0.0957286 0.3151461867 0.1466803     0.03243698     0.03243698
        FE_CI_width RE_CI_width
      1   0.1135355   0.1586678
      2   0.2567599   0.2567599
      3   0.1265832   0.1265832
      
      -- es_heterogeneity --
                                            effect_label moderator_variable_name
      1       Brain Photo Rating - No Brain Photo Rating                     mod
      11      Brain Photo Rating - No Brain Photo Rating                     mod
      12      Brain Photo Rating - No Brain Photo Rating                     mod
      H^2     Brain Photo Rating - No Brain Photo Rating                     mod
      H^21    Brain Photo Rating - No Brain Photo Rating                     mod
      H^22    Brain Photo Rating - No Brain Photo Rating                     mod
      I^2(%)  Brain Photo Rating - No Brain Photo Rating                     mod
      I^2(%)1 Brain Photo Rating - No Brain Photo Rating                     mod
      I^2(%)2 Brain Photo Rating - No Brain Photo Rating                     mod
      tau     Brain Photo Rating - No Brain Photo Rating                     mod
      tau1    Brain Photo Rating - No Brain Photo Rating                     mod
      tau2    Brain Photo Rating - No Brain Photo Rating                     mod
      tau^2   Brain Photo Rating - No Brain Photo Rating                     mod
      tau^21  Brain Photo Rating - No Brain Photo Rating                     mod
      tau^22  Brain Photo Rating - No Brain Photo Rating                     mod
              moderator_level       measure     estimate LL          UL
      1               Overall Diamond Ratio  1.397516897  1  2.66465049
      11             Critique Diamond Ratio  1.000000000  1  3.07914898
      12               Simple Diamond Ratio  1.000000000  1  3.72932744
      H^2             Overall           H^2  1.536923194  1  5.69222383
      H^21           Critique           H^2  1.000000000  1  8.63155437
      H^22             Simple           H^2  1.175196315  1 10.98004513
      I^2(%)          Overall        I^2(%) 34.934939901  0 82.43217360
      I^2(%)1        Critique        I^2(%)  0.000000000  0 88.41460116
      I^2(%)2          Simple        I^2(%) 14.907833952  0 90.89256931
      tau             Overall           tau  0.071349448  0  0.21092298
      tau1           Critique           tau  0.000000000  0  0.37763163
      tau2             Simple           tau  0.036030451  0  0.27194023
      tau^2           Overall         tau^2  0.005090744  0  0.04448850
      tau^21         Critique         tau^2  0.000000000  0  0.14260565
      tau^22           Simple         tau^2  0.001298193  0  0.07395149
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      -- es_meta_difference --
                       type                               effect_label
      Comparison Comparison Brain Photo Rating - No Brain Photo Rating
      Reference   Reference Brain Photo Rating - No Brain Photo Rating
      Difference Difference Brain Photo Rating - No Brain Photo Rating
                 moderator_variable_name   moderator_level effect_size          LL
      Comparison                     mod          Critique  0.21663310  0.08825315
      Reference                      mod            Simple  0.03243698 -0.03085464
      Difference                     mod Critique ‒ Simple  0.18419611  0.04106247
                        UL         SE            p
      Comparison 0.3450130 0.06550118 0.0009419452
      Reference  0.0957286 0.03229224 0.3151461867
      Difference 0.3273298 0.07302871 0.0116609301
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

---

    Code
      estimate_mod_fe
    Output
      Analysis of raw data:
      
      -- es_meta --
                                      effect_label moderator_variable_name
      1 Brain Photo Rating - No Brain Photo Rating                     mod
      2 Brain Photo Rating - No Brain Photo Rating                     mod
      3 Brain Photo Rating - No Brain Photo Rating                     mod
        moderator_variable_level effect_size          LL        UL         SE  k
      1                  Overall  0.06845248  0.01168473 0.1252202 0.02896367 10
      2                 Critique  0.21663310  0.08825315 0.3450130 0.06550118  4
      3                   Simple  0.03243698 -0.03085464 0.0957286 0.03229224  6
        diamond_ratio diamond_ratio_LL diamond_ratio_UL I2 I2_LL I2_UL PI_LL PI_UL
      1      1.397517                1         2.664650 NA    NA    NA    NA    NA
      2      1.000000                1         3.079149 NA    NA    NA    NA    NA
      3      1.000000                1         3.729327 NA    NA    NA    NA    NA
                   p     width FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1 0.0181085442 0.1135355     0.06845248     0.09431632   0.1135355   0.1586678
      2 0.0009419452 0.2567599     0.21663310     0.21663310   0.2567599   0.2567599
      3 0.3151461867 0.1265832     0.03243698     0.03243698   0.1265832   0.1265832
      
      -- es_heterogeneity --
                                       effect_label moderator_variable_name
      1  Brain Photo Rating - No Brain Photo Rating                     mod
      11 Brain Photo Rating - No Brain Photo Rating                     mod
      12 Brain Photo Rating - No Brain Photo Rating                     mod
         moderator_level       measure estimate LL       UL
      1          Overall Diamond Ratio 1.397517  1 2.664650
      11        Critique Diamond Ratio 1.000000  1 3.079149
      12          Simple Diamond Ratio 1.000000  1 3.729327
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      -- es_meta_difference --
                       type                               effect_label
      Comparison Comparison Brain Photo Rating - No Brain Photo Rating
      Reference   Reference Brain Photo Rating - No Brain Photo Rating
      Difference Difference Brain Photo Rating - No Brain Photo Rating
                 moderator_variable_name   moderator_level effect_size          LL
      Comparison                     mod          Critique  0.21663310  0.08825315
      Reference                      mod            Simple  0.03243698 -0.03085464
      Difference                     mod Critique ‒ Simple  0.18419611  0.04106247
                        UL         SE            p
      Comparison 0.3450130 0.06550118 0.0009419452
      Reference  0.0957286 0.03229224 0.3151461867
      Difference 0.3273298 0.07302871 0.0116609301
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

