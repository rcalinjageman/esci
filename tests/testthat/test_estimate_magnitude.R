test_that("estimate_magnitude vs. pen group from ESCI_summary_two: summary data, 95% CI", {
  # At 95% CI

  estimate <- estimate_magnitude(
    mean = 6.88,
    sd = 1,
    n = 10
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, 6.88)

})
