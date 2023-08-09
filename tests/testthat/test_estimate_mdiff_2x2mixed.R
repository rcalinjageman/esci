test_that("Compare estimate_mdiff_2mixed to statpsych::ci.2x2.mean.mixed, Example", {

  # example_data <- data.frame(
  #   pretest = c(
  #     19, 18, 19, 20, 17, 16, 16, 10, 12,  9, 13, 15
  #   ),
  #   posttest = c(
  #     18, 19, 20, 17, 20, 16, 19, 16, 16, 14, 16, 18
  #   ),
  #   condition = as.factor(
  #     c(
  #       rep("Control", times = 6),
  #       rep("Treated", times = 6)
  #     )
  #   )
  # )
  #
  # estimates <- esci::estimate_mdiff_2x2_mixed(
  #   data = example_data,
  #   outcome_variable_level1 = pretest,
  #   outcome_variable_level2 = posttest,
  #   grouping_variable = condition,
  #   repeated_measures_name = "Time"
  # )
  #
  # plot_mdiff(estimates$interaction)

  y11 <- c(18, 19, 20, 17, 20, 16)
  y12 <- c(19, 18, 19, 20, 17, 16)
  y21 <- c(19, 16, 16, 14, 16, 18)
  y22 <- c(16, 10, 12,  9, 13, 15)

  myconf_level <- 0.95

  mydf <- data.frame(
    posttest = c(y11, y21),
    pretest = c(y12, y22),
    Between = as.factor(
      c(
        rep("B1", times = length(y11)),
        rep("B2", times = length(y21))
      )
    )
  )


  myconfs <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconfs) {

    estimates <- esci::estimate_mdiff_2x2_mixed(
      data = mydf,
      outcome_variable_level1 = "pretest",
      outcome_variable_level2 = "posttest",
      grouping_variable = "Between",
      repeated_measures_name = "Rmeasures",
      conf_level = myconf_level
    )


    # Main effect of Between-Subjects Factor
    estimate <- estimates$main_effect_A

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.2x2.mean.mixed(
          alpha = 1 - myconf_level,
          y11 = y21,
          y12 = y22,
          y21 = y11,
          y22 = y12
        )
      )[3, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)


    mytest <- test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)

    # Main effect of B
    estimate <- estimates$main_effect_B

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.2x2.mean.mixed(
          alpha = 1 - myconf_level,
          y11 = y21,
          y12 = y22,
          y21 = y11,
          y22 = y12
        )
      )[2, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)


    mytest <- test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)


    # Interaction
    estimate <- estimates$interaction

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.2x2.mean.mixed(
          alpha = 1 - myconf_level,
          y11 = y21,
          y12 = y22,
          y21 = y11,
          y22 = y12
        )
      )[1, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)


    mytest <- test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)

  }


})

