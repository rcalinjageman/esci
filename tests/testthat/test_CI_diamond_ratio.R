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


  # Data set from Cairns et al., 2022, Figure 2
  library(metafor)
  mydata <- get(data(dat.bangertdrowns2004))
  mydata <- mydata[1:22,]

  # Conduct fixed effects meta-analysis
  FE <- metafor::rma(
    data = mydata,
    yi = yi,
    vi = vi,
    method="FE"
  )

  # Conduct random effect meta-analysis
  RE <- metafor::rma(
    data = mydata,
    yi = yi,
    vi = vi,
    method="DL"
  )

  # Get the diamond ratio
  res <- esci::CI_diamond_ratio(
    RE = RE,
    FE = FE,
    vi = mydata$vi
  )

  testthat::expect_equal(res$diamond_ratio, 1.55, tolerance = 1e-2)
  testthat::expect_equal(res$LL, 1.18, tolerance = 1e-2)
  testthat::expect_equal(res$UL, 2.34, tolerance = 1e-2)


  # Data set from Cairns et al., 2022, Figure 4
  library(metafor)
  mydata <- get(data(dat.normand1999))
  mydata <- metafor::escalc(
    measure="SMD",
    m1i=m2i,
    sd1i=sd2i,
    n1i=n2i,
    m2i=m1i,
    sd2i=sd1i,
    n2i=n1i,
    data=mydata
  )

  # Conduct fixed effects meta-analysis
  FE <- metafor::rma(
    data = mydata,
    yi = yi,
    vi = vi,
    method="FE"
  )

  # Conduct random effect meta-analysis
  RE <- metafor::rma(
    data = mydata,
    yi = yi,
    vi = vi,
    method="DL"
  )

  # Get the diamond ratio
  res <- esci::CI_diamond_ratio(
    RE = RE,
    FE = FE,
    vi = mydata$vi
  )

  testthat::expect_equal(res$diamond_ratio, 4.21, tolerance = 1e-2)
  testthat::expect_equal(res$LL, 2.81, tolerance = 1e-2)
  testthat::expect_equal(res$UL, 8.99, tolerance = 1e-2)



  # # Data set from anchor_estimate_ma
  # mydata <- esci::data_anchor_estimate_ma
  # myless <- mydata
  # mymult <- 10
  # myscale <- 1
  #
  # for (x in 1:8) {
  #
  #
  #   estimate_less <- esci::meta_mdiff_two(
  #     data = myless,
  #     comparison_means = "M High",
  #     comparison_ns = "n High",
  #     comparison_sds = "s High",
  #     reference_means = "M Low",
  #     reference_ns = "n Low",
  #     reference_sds = "s Low",
  #     random_effects = FALSE
  #   )
  #   print(paste("Multiplier: ", myscale))
  #   print(estimate_less$es_heterogeneity)
  #
  #   myless$`M Low` <- myless$`M Low` / mymult
  #   myless$`s Low` <- myless$`s Low` / mymult
  #   myless$`M High` <- myless$`M High` / mymult
  #   myless$`s High` <- myless$`s High` / mymult
  #   myscale <- myscale / mymult
  # }


})

