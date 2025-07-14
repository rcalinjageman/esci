# Some tests for meta_pdiff_two; needs development 

    Code
      estimate
    Output
      Analysis of raw data:
      
      -- es_meta --
        effect_label moderator_variable_name moderator_variable_level effect_size
      1    My effect                 setting                  Overall   0.5579311
      2    My effect                 setting                In-Person   0.7654911
      3    My effect                 setting                   Online   0.4646801
                 LL        UL        SE k diamond_ratio diamond_ratio_LL
      1  0.11751343 0.9983488 0.2247070 4             1                1
      2 -0.02552414 1.5565063 0.4035866 2             1                1
      3 -0.06551995 0.9948802 0.2705152 2             1                1
        diamond_ratio_UL I2 I2_LL    I2_UL       PI_LL     PI_UL          p     width
      1         5.079969  0     0 94.67705  0.11751343 0.9983488 0.01303079 0.8808354
      2        17.550734  0     0 99.65651 -0.02552414 1.5565063 0.05786481 1.5820305
      3        26.193770  0     0 99.58311 -0.06551995 0.9948802 0.08584046 1.0604001
        FE_effect_size RE_effect_size FE_CI_width RE_CI_width
      1      0.5579311      0.5579311   0.8808354   0.8808354
      2      0.7654911      0.7654911   1.5820305   1.5820305
      3      0.4646801      0.4646801   1.0604001   1.0604001
      
      -- es_heterogeneity --
              effect_label moderator_variable_name moderator_level       measure
      1          My effect                 setting         Overall Diamond Ratio
      11         My effect                 setting       In-Person Diamond Ratio
      12         My effect                 setting          Online Diamond Ratio
      H^2        My effect                 setting         Overall           H^2
      H^21       My effect                 setting       In-Person           H^2
      H^22       My effect                 setting          Online           H^2
      I^2(%)     My effect                 setting         Overall        I^2(%)
      I^2(%)1    My effect                 setting       In-Person        I^2(%)
      I^2(%)2    My effect                 setting          Online        I^2(%)
      tau        My effect                 setting         Overall           tau
      tau1       My effect                 setting       In-Person           tau
      tau2       My effect                 setting          Online           tau
      tau^2      My effect                 setting         Overall         tau^2
      tau^21     My effect                 setting       In-Person         tau^2
      tau^22     My effect                 setting          Online         tau^2
              estimate LL         UL
      1              1  1   5.079969
      11             1  1  17.550734
      12             1  1  26.193770
      H^2            1  1  18.786568
      H^21           1  1 291.125405
      H^22           1  1 239.872084
      I^2(%)         0  0  94.677048
      I^2(%)1        0  0  99.656505
      I^2(%)2        0  0  99.583111
      tau            0  0   2.200472
      tau1           0  0  10.000000
      tau2           0  0  10.000000
      tau^2          0  0   4.842076
      tau^21         0  0 100.000000
      tau^22         0  0 100.000000
      [1] "As of version 1.0.2 esci has implemented an improved method for calculating the CI for the diamond ratio; these will no longer match those presented in the 2nd edition of <i>Introduction to the New Statistics</i>."
      
      -- es_meta_difference --
                       type effect_label moderator_variable_name    moderator_level
      Comparison Comparison    My effect                 setting             Online
      Reference   Reference    My effect                 setting          In-Person
      Difference Difference    My effect                 setting Online ‒ In-Person
                 effect_size          LL        UL        SE          p
      Comparison   0.4646801 -0.06551995 0.9948802 0.2705152 0.08584046
      Reference    0.7654911 -0.02552414 1.5565063 0.4035866 0.05786481
      Difference  -0.3008110 -1.25308049 0.6514585 0.4858607 0.53583069
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

