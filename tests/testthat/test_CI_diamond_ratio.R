test_that("CI_diamond_ratio compare to example from Cairns et al. 2022", {

  # Data set from Cairns et al., 2022, Figure 1
  mydata <- esci::data_mccabemichael_brain

  # Use esci to obtain effect sizes and sample variances, storing only raw_data
  mydata <- esci::meta_mdiff_two(
    data = mydata,
    comparison_means = "M Brain",
    comparison_ns = "n Brain",
    comparison_sds = "s Brain",
    reference_means = "M No Brain",
    reference_ns = "n No Brain",
    reference_sds = "s No Brain",
    random_effects = FALSE
  )$raw_data

  # Conduct fixed effects meta-analysis
  FE <- metafor::rma(
    data = mydata,
    yi = effect_size,
    vi = sample_variance,
    method="FE"
  )
  # Conduct random effect meta-analysis
  RE <- metafor::rma(
    data = mydata,
    yi = effect_size,
    vi = sample_variance,
    method="DL"
  )

  # Get the diamond ratio
  res <- esci::CI_diamond_ratio(
    RE = RE,
    FE = FE,
    vi = mydata$sample_variance
  )

  testthat::expect_equal(res$diamond_ratio, 1.4, tolerance = 1e-2)
  testthat::expect_equal(res$LL, 1, tolerance = 1e-2)
  testthat::expect_equal(res$UL, 3.09, tolerance = 1e-2)


})

