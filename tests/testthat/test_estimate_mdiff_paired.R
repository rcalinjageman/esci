test_that("Compare estimate_mdiff_paired to ESCI_Data_paired, Thomasaon1", {
  # At 95% CI

  data("data_thomason1")

  estimate <- esci::estimate_mdiff_paired(
    data = data_thomason1,
    comparison_measure = posttest,
    reference_measure = pretest
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 1.666666667)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], 0.715217957)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 2.618115376)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 11)
  testthat::expect_equal(estimate$es_r$effect_size, .89239080809841)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$t, 3.855498)
  testthat::expect_equal(mytest$point_null$p, .002674000908089)


  # At 99% CI
  estimate <- esci::estimate_mdiff_paired(
    data = data_thomason1,
    comparison_measure = posttest,
    reference_measure = pretest,
    conf_level = 0.99
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 1.666666667)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], 0.324078968)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 3.009254365)

  # Plot
  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")




})





test_that("Compare estimate_mdiff_paired to ESCI_Summary_paired, penlaptop1", {
  # At 95% CI

  # Check - from summary data, esci in excel summary two example
  sd1 <- 4.28
  sd2 <- 3.4
  sdiff <- 2.13

  cor <- (sd1^2 + sd2^2 - sdiff^2) / (2*sd1*sd2)

  estimate <- estimate_mdiff_paired(
    comparison_mean = 14.25,
    comparison_sd = 4.28,
    reference_mean = 12.88,
    reference_sd = 3.4,
    n = 16,
    correlation = 0.87072223749,
    comparison_measure_name = "After",
    reference_measure_name = "Before"
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 1.37)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], 0.235003117)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 2.504996883)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 15)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$t, 2.572769953052)
  testthat::expect_equal(mytest$point_null$p, .02121728585978)


  # At 99% CI
  estimate <- estimate_mdiff_paired(
    comparison_mean = 14.25,
    comparison_sd = 4.28,
    reference_mean = 12.88,
    reference_sd = 3.4,
    n = 16,
    correlation = 0.87072223749,
    comparison_measure_name = "After",
    reference_measure_name = "Before",
    conf_level = 0.99
  )


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 1.37)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], -0.19912461)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 2.93912461)

  # Plot
  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})




test_that("Compare estimate_mdiff_paired to statpsych::ci.mean.ps example and statpsych::ci.stdmean.ps", {
  comparison_mean <- 58.2
  reference_mean <- 51.4
  comparison_sd <- 7.43
  reference_sd <- 8.92
  n_pairs <- 30
  paired_r <- 0.537
  myconf_level <- 0.95

  myconfs <- c(0.90, 0.95, 0.99)


  for (myconf_level in myconfs) {
    estimate <- esci::estimate_mdiff_paired(
      comparison_mean = comparison_mean,
      comparison_sd = comparison_sd,
      reference_mean = reference_mean,
      reference_sd = reference_sd,
      n = n_pairs,
      correlation = paired_r,
      conf_level = myconf_level
    )


    mysp <- as.list(
      as.data.frame(
        statpsych::ci.mean.ps(
          alpha = 1 - myconf_level,
          m1 = comparison_mean,
          m2 = reference_mean,
          sd1 = comparison_sd,
          sd2 = reference_sd,
          cor = paired_r,
          n = n_pairs
        )
      )
    )


    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)

    mytest <- test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)


    mysp <- as.list(
      as.data.frame(
        statpsych::ci.stdmean.ps(
          alpha = 1 - myconf_level,
          m1 = comparison_mean,
          m2 = reference_mean,
          sd1 = comparison_sd,
          sd2 = reference_sd,
          cor = paired_r,
          n = n_pairs
        )
      )[1, ]
    )

    if (!is.null(mysp$`adj Estimate`)) {
      mysp$Estimate <- mysp$`adj Estimate`
    }

    testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
    # testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
    # testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
    # testthat::expect_equal(estimate$es_smd$SE, mysp$SE)

  }



  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Compare estimate_mdiff_paired to statpsych::ci.median.ps example and ci.ratio.mean.ps and ci.ratio.median.ps", {

  y1 <- c(21, 4, 9, 12, 35, 18, 10, 22, 24, 1, 6, 8, 13, 16, 19)
  y2 <- c(67, 28, 30, 28, 52, 40, 25, 37, 44, 10, 14, 20, 28, 40, 51)
  myconf_level <- 0.95

  myconfs <- c(0.90, 0.95, 0.99)


  for (myconf_level in myconfs) {

    estimate <- esci::estimate_mdiff_paired(
      comparison_measure = y1,
      reference_measure = y2,
      conf_level = myconf_level
    )

    # Median difference
    mysp <- as.list(
      as.data.frame(
        statpsych::ci.median.ps(
          alpha = 1 - myconf_level,
          y1 = y1,
          y2 = y2
        )
      )
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$`Median1-Median2`)
    testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)


    # Mean ratio
    mysp <- as.list(
      as.data.frame(
        statpsych::ci.ratio.mean.ps(
          alpha = 1 - myconf_level,
          y1 = y1,
          y2 = y2
        )
      )
    )

    testthat::expect_s3_class(estimate, "esci_estimate")

    testthat::expect_equal(estimate$es_mean_ratio$effect_size, mysp$`Mean1/Mean2`)
    testthat::expect_equal(estimate$es_mean_ratio$LL, mysp$LL)
    testthat::expect_equal(estimate$es_mean_ratio$UL, mysp$UL)
    testthat::expect_equal(estimate$es_mean_ratio$comparison_mean, mysp$Mean1)
    testthat::expect_equal(estimate$es_mean_ratio$reference_mean, mysp$Mean2)


    # Media ratio
    mysp <- as.list(
      as.data.frame(
        statpsych::ci.ratio.median.ps(
          alpha = 1 - myconf_level,
          y1 = y1,
          y2 = y2
        )
      )
    )

    testthat::expect_equal(estimate$es_median_ratio$effect_size, mysp$`Median1/Median2`)
    testthat::expect_equal(estimate$es_median_ratio$LL, mysp$LL)
    testthat::expect_equal(estimate$es_median_ratio$UL, mysp$UL)
    testthat::expect_equal(estimate$es_median_ratio$comparison_median, mysp$Median1)
    testthat::expect_equal(estimate$es_median_ratio$reference_median, mysp$Median2)

  }

  suppressWarnings(myplot <- plot_mdiff(estimate, effect_size = "median"))
  testthat::expect_s3_class(myplot, "ggplot")



})


test_that("Test different types of calls to estimate_mdiff_paired", {

  bk_wrapper <- c(
    4	,
    4	,
    3	,
    2	,
    2	,
    5	,
    1	,
    1	,
    3	,
    1	,
    1	,
    2	,
    4	,
    3	,
    1	,
    1	,
    1	,
    3	,
    1	,
    1	,
    1	,
    5	,
    1	,
    4	,
    1	,
    3	,
    2	,
    4	,
    2	,
    1
  )

  wc_wrapper <- c(
    2	,
    3	,
    2	,
    1	,
    1	,
    2	,
    1	,
    1	,
    3	,
    2	,
    1	,
    1	,
    2	,
    4	,
    1	,
    1	,
    4	,
    2	,
    2	,
    1	,
    2	,
    2	,
    1	,
    3	,
    2	,
    2	,
    1	,
    1	,
    2	,
    2

  )

  wrapper <- data.frame(
    "wc" = wc_wrapper,
    "bk" = bk_wrapper
  )

  # Check - vector
  from_vector <- estimate_mdiff_paired(
    comparison_measure = wc_wrapper,
    reference_measure = bk_wrapper
  )
  testthat::expect_s3_class(from_vector, "esci_estimate")

  # Check data frame, column as strings
  from_df_strings <- estimate_mdiff_paired(
    data = wrapper,
    comparison_measure = "wc",
    reference_measure = "bk"
  )
  testthat::expect_s3_class(from_df_strings, "esci_estimate")

  # Check data frame, columns as tidy
  from_df_tidy <- estimate_mdiff_paired(wrapper, wc, bk)
  testthat::expect_s3_class(from_df_tidy, "esci_estimate")

})
