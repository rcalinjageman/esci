# estimate_r compared to ESCI_One_correlation, summary data

    Code
      estimate
    Output
      Analysis of summary data:
      
      -- es_r --
        x_variable_name y_variable_name                          effect effect_size
      1   My x variable   My y variable My x variable and My y variable         0.4
                LL        UL        SE  n df      ta_LL     ta_UL
      1 0.03953651 0.6606395 0.1559841 30 28 0.09986615 0.6250829
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# estimate_r of from Thomason1, ESCI_Scatterplots

    Code
      estimate
    Output
      Analysis of raw data:
      Data frame = data
      
      ---Overview---
        outcome_variable_name     mean   mean_LL  mean_UL median median_LL median_UL
      1                ls_pre 11.58333  9.476776 13.68989   12.0         9        14
      2               ls_post 13.25000 11.410019 15.08998   13.5        10        16
              sd min max   q1    q3  n missing df   mean_SE median_SE
      1 3.315483   6  17  9.0 14.00 12       0 11 0.9570974  1.208488
      2 2.895922   8  18 11.5 15.25 12       0 11 0.8359806  1.450186
      
      -- es_r --
        x_variable_name y_variable_name             effect effect_size      LL
      1          ls_pre         ls_post ls_pre and ls_post   0.8923908 0.62894
              UL         SE  n df     ta_LL     ta_UL
      1 0.967157 0.06139936 12 10 0.6882891 0.9596343
      
      -- regression --
            component    values        LL       UL
      1 Intercept (a) 4.2212267 0.8856544 7.556799
      2     Slope (b) 0.7794624 0.5017391 1.057186
      [1] "Ŷ = 4.221 + 0.7795*X"
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 95% expected coverage.

# Compare estimate_r to statpsych::ci.cor

    Code
      estimate
    Output
      Analysis of summary data:
      
      -- es_r --
        x_variable_name y_variable_name                          effect effect_size
      1   My x variable   My y variable My x variable and My y variable        0.91
              LL        UL         SE   n  df     ta_LL     ta_UL
      1 0.853283 0.9445006 0.01685618 105 103 0.8598598 0.9417717
      
      
      Note: LL and UL are lower and upper boundaries of confidence intervals with 99% expected coverage.

