test_estimate_meta_smd_two <- function() {

  testd <- data.frame(
    study = c(paste("Damisch", seq(1:6)), "Calin 1", "Calin 2"),
    my_smd = c(0.83, 0.986, 0.66, 0.78, 0.979, 0.86, 0.05, 0.047),
    smd_corrected = c(0.806, 0.963, 0.647, 0.758, 0.950, 0.835, 0.050, 0.047),
    n1 = c(14, 17, 20, 15, 14, 14, 58, 54),
    n2 = c(14, 17, 21, 14, 14, 14, 66, 57),
    subset = as.factor(c(rep("Germany", times = 6), rep("USA", times = 2)))
  )

  # Compare to esci - my study CIs come out a bit different due to correction
  meta_smd_two(testd, my_smd, n1, n2, study)
  estimate <- meta_smd_two(
    testd,
    my_smd,
    n1,
    n2,
    study,
    assume_equal_variance = TRUE,
    esci_vi = TRUE
  )
  estimate$raw_data
  estimate

  # Compare to esci without bias correction - seems perfect
  estimate <- meta_smd_two(
    testd,
    my_smd,
    n1,
    n2,
    study,
    assume_equal_variance = TRUE,
    correct_bias = FALSE,
    esci_vi = TRUE
  )
  estimate$raw_data
  estimate

  # Compare to esci with fixed effects, 99% CI, no correction - seems perfect
  estimate <- meta_smd_two(
    testd,
    my_smd,
    n1,
    n2,
    study,
    random_effects = FALSE,
    assume_equal_variance = TRUE,
    correct_bias = FALSE,
    esci_vi = TRUE,
    conf_level = 0.99
  )
  estimate$raw_data
  estimate


  # Compare to esci - with analytic SE, RE, 95% CI, no correction
  # Only a *very* minor difference
  estimate <- meta_smd_two(
    testd,
    my_smd,
    n1,
    n2,
    study,
    assume_equal_variance = TRUE,
    correct_bias = FALSE,
    esci_vi = FALSE
  )
  estimate$raw_data
  estimate

  # Compare to esci - with analytic SE, RE, 95% CI, no correction
  # With correction, analytic SEs are a bit further from what esci has
  # Not a very big deal, but some mismatch
  estimate <- meta_smd_two(
    testd,
    my_smd,
    n1,
    n2,
    study,
    assume_equal_variance = TRUE,
    correct_bias = TRUE,
    esci_vi = FALSE
  )
  estimate$raw_data
  estimate

  # Subset - matches perfectly when esci_vi and no bias correction
  # With correction, CIs for individual studies don't perfectly match
  # With analytic vi rather than esci_vi some slight difference
  estimate <- meta_smd_two(
    testd,
    my_smd,
    n1,
    n2,
    study,
    subset,
    contrast = c(-1, 1),
    assume_equal_variance = TRUE,
    correct_bias = FALSE,
    esci_vi = TRUE
  )
  estimate$raw_data
  estimate




  # Bad calls with good errors
  bad_test <- testd
  bad_test[c(1, 3), "n1"] <- 1.1
  meta_smd_two(
    bad_test,
    my_smd,
    n1,
    n2,
    study,
    subset,
    contrast = c(-1, 1),
    assume_equal_variance = TRUE,
    correct_bias = FALSE,
    esci_vi = TRUE
  )

}

