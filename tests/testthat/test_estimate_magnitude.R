test_that("estimate_magnitude of pen group from ESCI_summary_two: summary data, 95% CI", {
  # At 95% CI

  data("data_penlaptop1")

  estimate <- esci::estimate_magnitude(
    data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
    outcome_variable = transcription
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, 8.811764706)
  testthat::expect_equal(estimate$es_mean$LL, 7.154641985)
  testthat::expect_equal(estimate$es_mean$UL, 10.46888743)

  testthat::expect_equal(estimate$overview$mean, 8.811764706)
  testthat::expect_equal(estimate$overview$mean_LL, 7.154641985)
  testthat::expect_equal(estimate$overview$mean_UL, 10.46888743)

  myplot <- plot_magnitude(estimate)
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("estimate_magnitude of pen group from ESCI_summary_two: summary data, 99% CI", {
  # At 99% CI

  data("data_penlaptop1")

  estimate <- esci::estimate_magnitude(
    data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
    outcome_variable = transcription,
    conf_level = 0.99
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, 8.811764706)
  testthat::expect_equal(estimate$es_mean$LL, 6.585497431)
  testthat::expect_equal(estimate$es_mean$UL, 11.03803198)

  testthat::expect_equal(estimate$overview$mean, 8.811764706)
  testthat::expect_equal(estimate$overview$mean_LL, 6.585497431)
  testthat::expect_equal(estimate$overview$mean_UL, 11.03803198)

  try(myplot <- plot_magnitude(estimate), silent = TRUE)
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Compare estimate_magnitude to statpsych::ci.mean1 example", {

  mymean <- 24.5
  mysd <- 3.65
  myn <- 40

  estimate <- esci::estimate_magnitude(
    mean = mymean,
    sd = mysd,
    n = myn
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.mean1(.05, mymean, mysd, myn)
    )
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, mysp$Estimate)
  testthat::expect_equal(estimate$es_mean$LL, mysp$LL)
  testthat::expect_equal(estimate$es_mean$UL, mysp$UL)

  testthat::expect_equal(estimate$overview$mean, mysp$Estimate)
  testthat::expect_equal(estimate$overview$mean_LL, mysp$LL)
  testthat::expect_equal(estimate$overview$mean_UL, mysp$UL)

  try(myplot <- plot_magnitude(estimate), silent = TRUE)
  testthat::expect_s3_class(myplot, "ggplot")


})



test_that("Compare estimate_magnitude to statpsych::ci.mean1 example", {

  mymean <- 24.5
  mysd <- 3.65
  myn <- 40
  myconfs <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconfs) {
    estimate <- esci::estimate_magnitude(
      mean = mymean,
      sd = mysd,
      n = myn,
      conf_level = myconf_level
    )

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.mean1(1 - myconf_level, mymean, mysd, myn)
      )
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean$effect_size, mysp$Estimate)
    testthat::expect_equal(estimate$es_mean$LL, mysp$LL)
    testthat::expect_equal(estimate$es_mean$UL, mysp$UL)

    testthat::expect_equal(estimate$overview$mean, mysp$Estimate)
    testthat::expect_equal(estimate$overview$mean_LL, mysp$LL)
    testthat::expect_equal(estimate$overview$mean_UL, mysp$UL)

  }

  try(myplot <- plot_magnitude(estimate), silent = TRUE)
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Compare estimate_magnitude to statpsych::ci.median1 example", {

  myconfs <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconfs) {
    y <- c(30, 20, 15, 10, 10, 60, 20, 25, 20, 30, 10, 5, 50, 40,
           20, 10, 0, 20, 50)

    estimate <- esci::estimate_magnitude(
      outcome_variable = y,
      conf_level = myconf_level
    )

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.median1(1 - myconf_level, y)
      )
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_median$effect_size, mysp$Estimate)
    testthat::expect_equal(estimate$es_median$LL, mysp$LL)
    testthat::expect_equal(estimate$es_median$UL, mysp$UL)

    testthat::expect_equal(estimate$overview$median, mysp$Estimate)
    testthat::expect_equal(estimate$overview$median_LL, mysp$LL)
    testthat::expect_equal(estimate$overview$median_UL, mysp$UL)

  }

  myplot <- plot_magnitude(estimate)
  testthat::expect_s3_class(myplot, "ggplot")


})
