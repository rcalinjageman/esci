test_that("Compare meta_d2 to ESCI_d_two_groups and d_subsets", {

  # Comparisons are not perfect to ESCI due to difference in SE calculations

  testd <- data.frame(
    study = c(paste("Damisch", seq(1:6)), "Calin 1", "Calin 2"),
    my_smd = c(0.83, 0.986, 0.66, 0.78, 0.979, 0.86, 0.05, 0.047),
    smd_corrected = c(0.806, 0.963, 0.647, 0.758, 0.950, 0.835, 0.050, 0.047),
    n1 = c(14, 17, 20, 15, 14, 14, 58, 54),
    n2 = c(14, 17, 21, 14, 14, 14, 66, 57),
    subset = as.factor(c(rep("Germany", times = 6), rep("USA", times = 2)))
  )

  # Compare to esci - my study CIs come out a bit different due to correction
  estimate <- esci::meta_d2(
    data = testd,
    ds = smd_corrected,
    comparison_ns = n1,
    reference_ns = n2,
    labels = study,
    assume_equal_variance = TRUE,
    random_effects = TRUE
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)


  # Random effects or not, equal variance or not
  estimate_d2 <- esci::meta_d2(
    data = testd,
    ds = smd_corrected,
    comparison_ns = n1,
    reference_ns = n2,
    labels = study,
    assume_equal_variance = FALSE,
    random_effects = TRUE
  )
  testthat::expect_s3_class(estimate_d2, "esci_estimate")
  testthat::expect_snapshot(estimate)


  for (aeq in c(TRUE, FALSE)) {
    for (rw in c(TRUE, FALSE)) {
      for (mod in c("NULL", "subset")) {
        estimate_vary <- esci::meta_d2(
          data = testd,
          ds = smd_corrected,
          comparison_ns = n1,
          reference_ns = n2,
          labels = study,
          moderator = !!mod,
          assume_equal_variance = aeq,
          random_effects = rw
        )
        testthat::expect_s3_class(estimate_vary, "esci_estimate")
      }
    }
  }

  testthat::expect_snapshot(estimate_vary)

  # Plot
  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})

