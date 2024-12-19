test_that("Compare meta_mean to ESCI_Original 7", {

  # Compare to ESCI_Original_7 from UTNS

  original_7 <- data.frame(
    study_name = c(
      "Aden (1993)"	,
      "Buggs (1995)"	,
      "Crazed (1999)"	,
      "Dudley (2003)"	,
      "Evers (2005)"	,
      "Fox (2009)",
      "Mine (2011)"
    ),
    rt_mean = c(
      454	,
      317	,
      430	,
      525	,
      479	,
      387,
      531
    ),
    rt_sd = c(
      142	,
      158	,
      137	,
      260	,
      144	,
      165,
      233
    ),
    rt_n = c(
      24	,
      7	,
      20	,
      8	,
      14	,
      13,
      18
    ),
    subset = as.factor(
      c(
        "90s",
        "90s",
        "90s",
        "00s",
        "00s",
        "00s",
        "00s"
      )
    ),
    d1_unbiased = c(
      3.091587,
      1.742751,
      3.012857,
      1.793487,
      3.130074,
      2.195209,
      2.17667
    )
  )


  # Fixed effect, 95% CI
  estimate <- esci::meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    random_effects = FALSE
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)

  # Random effects, 95% CI
  estimate_re <- esci::meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    random_effects = TRUE
  )

  testthat::expect_s3_class(estimate_re, "esci_estimate")
  testthat::expect_snapshot(estimate_re)


  # Fixed effect, 90% CI
  estimate_90 <- esci::meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    random_effects = FALSE,
    conf_level = 0.90
  )


  testthat::expect_s3_class(estimate_90, "esci_estimate")
  testthat::expect_snapshot(estimate_90)


  # Random effects, 90% CI
  estimate_90 <- esci::meta_mean(
    original_7,
    rt_mean,
    rt_sd,
    rt_n,
    study_name,
    random_effects = TRUE,
    conf_level = 0.90
  )

  testthat::expect_s3_class(estimate_90, "esci_estimate")
  # testthat::expect_equal(estimate$es_meta$effect_size[[1]], 442.3667673)
  # testthat::expect_equal(estimate$es_meta$LL[[1]], 406.1420797)
  # testthat::expect_equal(estimate$es_meta$UL[[1]], 478.5914549)
  testthat::expect_snapshot(estimate_90)

  # testthat::expect_equal(estimate$es_heterogeneity$estimate[[3]], 42.9198867499529)


  # Plot
  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})

