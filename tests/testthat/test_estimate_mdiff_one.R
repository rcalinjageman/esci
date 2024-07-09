test_that("Compare estimate_mdiff one to ESCI_Data_two, pen group", {
  # At 95% CI

  data("data_penlaptop1")
  myreference_means <- c(0, -10, 10)

  for (myreference_mean in myreference_means) {
    estimate <- esci::estimate_mdiff_one(
      data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
      outcome_variable = transcription,
      reference_mean = myreference_mean
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean$effect_size, 8.811764706)
    testthat::expect_equal(estimate$es_mean$LL, 7.154641985)
    testthat::expect_equal(estimate$es_mean$UL, 10.46888743)

    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 8.811764706 - myreference_mean)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], 7.154641985 - myreference_mean)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], 10.46888743 - myreference_mean)


    testthat::expect_equal(estimate$overview$mean, 8.811764706)
    testthat::expect_equal(estimate$overview$mean_LL, 7.154641985)
    testthat::expect_equal(estimate$overview$mean_UL, 10.46888743)


  }

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})




test_that("Compare estimate_mdiff_one to ESCI_summary_two, pen group", {
  # At 95% CI

  mymean <- 12.09
  mysd <- 5.52
  myn <- 103

  estimate <- esci::estimate_mdiff_one(
    comparison_mean = mymean,
    comparison_sd = mysd,
    comparison_n = myn
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, 12.09)
  testthat::expect_equal(estimate$es_mean$LL, 11.01117343)
  testthat::expect_equal(estimate$es_mean$UL, 13.16882657)


  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})



test_that("Compare estimate_mdiff_one to statpsych::ci.mean example", {
  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  mymean <- 24.5
  mysd <- 3.65
  myn <- 40
  myconfs <- c(0.90, 0.95, 0.99)
  myreference_means <- c(0, -10, 10)

  for (myreference_mean in myreference_means) {
    for (myconf_level in myconfs) {
      estimate <- esci::estimate_mdiff_one(
        comparison_mean = mymean,
        comparison_sd = mysd,
        comparison_n = myn,
        reference_mean = myreference_mean,
        conf_level = myconf_level
      )

      if (statpsych_version > 150) {
        mysp <- as.list(
          as.data.frame(
            statpsych::ci.mean(1 - myconf_level, mymean, mysd, myn)
          )
        )

      } else {
        mysp <- as.list(
          as.data.frame(
            statpsych::ci.mean1(1 - myconf_level, mymean, mysd, myn)
          )
        )

      }


      testthat::expect_s3_class(estimate, "esci_estimate")

      testthat::expect_equal(estimate$es_mean$effect_size, mysp$Estimate)
      testthat::expect_equal(estimate$es_mean$LL, mysp$LL)
      testthat::expect_equal(estimate$es_mean$UL, mysp$UL)

      testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate - myreference_mean)
      testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL - myreference_mean)
      testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL - myreference_mean)

      testthat::expect_equal(estimate$overview$mean, mysp$Estimate)
      testthat::expect_equal(estimate$overview$mean_LL, mysp$LL)
      testthat::expect_equal(estimate$overview$mean_UL, mysp$UL)

    }

  }

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Compare estimate_mdiff_one to statpsych::ci.median example", {

  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  myconfs <- c(0.90, 0.95, 0.99)
  myreference_means <- c(0, -10, 10)
  y <- c(30, 20, 15, 10, 10, 60, 20, 25, 20, 30, 10, 5, 50, 40,
         20, 10, 0, 20, 50)

  for (myreference_mean in myreference_means) {
    for (myconf_level in myconfs) {

      estimate <- esci::estimate_mdiff_one(
        outcome_variable = y,
        reference_mean = myreference_mean,
        conf_level = myconf_level
      )

      if (statpsych_version > 150) {
        mysp <- as.list(
          as.data.frame(
            statpsych::ci.median(1 - myconf_level, y)
          )
        )

      } else {
        mysp <- as.list(
          as.data.frame(
            statpsych::ci.median1(1 - myconf_level, y)
          )
        )

      }

      testthat::expect_s3_class(estimate, "esci_estimate")

      testthat::expect_equal(estimate$es_median$effect_size, mysp$Estimate)
      testthat::expect_equal(estimate$es_median$LL, mysp$LL)
      testthat::expect_equal(estimate$es_median$UL, mysp$UL)

      testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate - myreference_mean)
      testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL - myreference_mean)
      testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL - myreference_mean)


      testthat::expect_equal(estimate$overview$median, mysp$Estimate)
      testthat::expect_equal(estimate$overview$median_LL, mysp$LL)
      testthat::expect_equal(estimate$overview$median_UL, mysp$UL)

    }
  }

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")



})
