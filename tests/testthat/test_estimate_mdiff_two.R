test_that("Compare estimate_mdiff_two to ESCI_Data_two, penlaptop1", {
  # At 95% CI


  # Check - from summary data, esci in excel summary two example
  estimate <- estimate_mdiff_two(
    comparison_mean = 12.09,
    comparison_sd = 5.52,
    comparison_n = 103,
    reference_mean = 6.88,
    reference_sd = 4.22,
    reference_n = 48,
    grouping_variable_levels = c("Ref-Laptop", "Comp-Pen"),
    outcome_variable_name = "% Transcription",
    grouping_variable_name = "Note-taking type",
    assume_equal_variance = TRUE
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)

  # At 99% CI
  estimate_99 <- estimate_mdiff_two(
    comparison_mean = 12.09,
    comparison_sd = 5.52,
    comparison_n = 103,
    reference_mean = 6.88,
    reference_sd = 4.22,
    reference_n = 48,
    grouping_variable_levels = c("Laptop", "Pen"),
    outcome_variable_name = "% Transcription",
    grouping_variable_name = "Note-taking type",
    conf_level = 0.99,
    assume_equal_variance = TRUE
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate_99)

  # Plot
  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  # NHST
  mytest <- test_mdiff(estimate)
  testthat::expect_snapshot(mytest)


})



test_that("Compare estimate_mdiff_two to ESCI_Data_two, penlaptop1", {
  # At 95% CI

  data("data_penlaptop1")

  estimate <- esci::estimate_mdiff_two(
    data = data_penlaptop1,
    outcome_variable = transcription,
    grouping_variable = condition,
    switch_comparison_order = FALSE,
    assume_equal_variance = TRUE
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  mytest <- test_mdiff(estimate)
  testthat::expect_snapshot(mytest)


  # At 99% CI
  estimate_99 <- esci::estimate_mdiff_two(
    data = data_penlaptop1,
    outcome_variable = transcription,
    grouping_variable = condition,
    switch_comparison_order = FALSE,
    assume_equal_variance = TRUE,
    conf_level = 0.99
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate_99)

  # Plot
  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("Compare estimate_mdiff_two to statpsych::ci.mean2 example", {
  comparison_mean <- 15.4
  reference_mean <- 10.3
  comparison_sd <- 2.67
  reference_sd <- 2.15
  comparison_n <- 30
  reference_n <- 20


  myconfs <- c(0.90, 0.95, 0.99)
  myaeqs <- c(TRUE, FALSE)

  for (myaeq in myaeqs) {

    for (myconf_level in myconfs) {
      estimate <- esci::estimate_mdiff_two(
        comparison_mean = comparison_mean,
        comparison_sd = comparison_sd,
        comparison_n = comparison_n,
        reference_mean = reference_mean,
        reference_sd = reference_sd,
        reference_n = reference_n,
        conf_level = myconf_level,
        grouping_variable_levels = c("Control", "Treated"),
        assume_equal_variance = myaeq
      )

      row_select <- if (myaeq) 1 else 2

      mysp <- as.list(
        as.data.frame(
          statpsych::ci.mean2(
            alpha = 1 - myconf_level,
            m1 = comparison_mean,
            m2 = reference_mean,
            sd1 = comparison_sd,
            sd2 = reference_sd,
            n1 = comparison_n,
            n2 = reference_n
          )
        )[row_select, ]
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

    }


  }

  testthat::expect_snapshot(estimate)


  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Compare estimate_mdiff_two to statpsych::ci.median2 example", {

  y1 <- c(32, 39, 26, 35, 43, 27, 40, 37, 34, 29)
  y2 <- c(36, 44, 47, 42, 49, 39, 46, 31, 33, 48)

  mydata <- data.frame(
    myoutcome = c(y1, y2),
    mycondition = as.factor(
      c(
        rep("Group 1", times = length(y1)),
        rep("Group 2", times = length(y2))
      )
    )
  )

  myconfs <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconfs) {

    estimate <- esci::estimate_mdiff_two(
      data = mydata,
      outcome_variable = "myoutcome",
      grouping_variable = "mycondition",
      conf_level = myconf_level,
      switch_comparison_order = TRUE
    )

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.median2(
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

  }

  testthat::expect_snapshot(estimate)


  suppressWarnings(myplot <- plot_mdiff(estimate, effect_size = "median"))
  testthat::expect_s3_class(myplot, "ggplot")



})


test_that("Compare estimate_mdiff_two to statpsych::ci.ratio.mean2 and ci.ratio.median2 example", {

  y2 <- c(32, 39, 26, 35, 43, 27, 40, 37, 34, 29, 49, 42, 40)
  y1 <- c(36, 44, 47, 42, 49, 39, 46, 31, 33, 48)

  mydata <- data.frame(
    myoutcome = c(y1, y2),
    mycondition = as.factor(
      c(
        rep("Group 1", times = length(y1)),
        rep("Group 2", times = length(y2))
      )
    )
  )

  myconfs <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconfs) {

    estimate <- esci::estimate_mdiff_two(
      data = mydata,
      outcome_variable = "myoutcome",
      grouping_variable = "mycondition",
      conf_level = myconf_level,
      switch_comparison_order = TRUE
    )

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.ratio.mean2(
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

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.ratio.median2(
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

  testthat::expect_snapshot(estimate)


})




test_that("Compare estimate_mdiff_two to statpsych::ci.ratio.mean2 and ci.ratio.median2 example", {

  y2 <- c(-32, -39, 26, 35, 43, 27, 40, 37, 34, 29, 49, 42, 40)
  y1 <- c(36, 44, -47, -42, 49, 39, 46, 31, 33, 48)

  mydata <- data.frame(
    myoutcome = c(y1, y2),
    mycondition = as.factor(
      c(
        rep("Group 1", times = length(y1)),
        rep("Group 2", times = length(y2))
      )
    )
  )

  myconf_level <- 0.95

    estimate <- esci::estimate_mdiff_two(
      data = mydata,
      outcome_variable = "myoutcome",
      grouping_variable = "mycondition",
      conf_level = myconf_level,
      switch_comparison_order = TRUE
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_snapshot(estimate)



})
