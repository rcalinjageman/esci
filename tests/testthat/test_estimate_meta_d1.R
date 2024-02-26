test_that("Tests for meta_d1", {

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
  estimate <- esci::meta_d1(
    original_7,
    d1_unbiased,
    rt_n,
    study_name,
    random_effects = FALSE
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_meta$effect_size[[1]], 2.4509852)
  testthat::expect_equal(estimate$es_meta$LL[[1]], 2.05215734)
  testthat::expect_equal(estimate$es_meta$UL[[1]], 2.849813)


  estimate <- esci::meta_d1(
    data = original_7,
    ds = d1_unbiased,
    ns = rt_n,
    moderator = subset,
    labels = study_name,
    random_effects = FALSE
  )

  testthat::expect_s3_class(estimate, "esci_estimate")


  # Plot
  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(suppressWarnings(myplot), "ggplot")


})

