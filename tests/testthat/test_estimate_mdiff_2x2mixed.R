test_that("estimate_mdiff_2mixed with Self-explain data set", {
 data <- esci::data_selfexplain

 estimate <- esci::estimate_mdiff_2x2_mixed(data = data, Pretest, Posttest, Condition)

 mysmds <- as.data.frame(
     statpsych::ci.2x2.stdmean.mixed(
      alpha = (1 - 0.95),
      y11 = data[data$Condition == "Self-Explain", "Posttest"],
      y12 = data[data$Condition == "More Practice", "Posttest"],
      y21 = data[data$Condition == "Self-Explain", "Pretest"],
      y22 = data[data$Condition == "More Practice", "Pretest"]
    )
 )


 esci_columns <- c("effect_size", "LL", "UL", "SE", "d_biased")
 statpsych_columns <- c("adj Estimate","LL", "UL", "SE", "Estimate")


 colnames(mysmds) <- c("d_biased", "effect_size", "SE", "LL", "UL")
 rownames(mysmds) <- as.integer(c(5, 2, 1, 3, 4, 10, 11))

 testthat::expect_equal(estimate$es_smd[5, esci_columns], mysmds[1, esci_columns])
 testthat::expect_equal(estimate$es_smd[2, esci_columns], mysmds[2, esci_columns])
 testthat::expect_equal(estimate$es_smd[1, esci_columns], mysmds[3, esci_columns])
 testthat::expect_equal(estimate$es_smd[3, esci_columns], mysmds[4, esci_columns])
 testthat::expect_equal(estimate$es_smd[4, esci_columns], mysmds[5, esci_columns])


 esci_columns <- c("effect_size", "LL", "UL", "SE")

 mymedians <- as.data.frame(
   statpsych::ci.2x2.median.mixed(
     alpha = (1 - 0.95),
     y11 = data[data$Condition == "Self-Explain", "Posttest"],
     y12 = data[data$Condition == "More Practice", "Posttest"],
     y21 = data[data$Condition == "Self-Explain", "Pretest"],
     y22 = data[data$Condition == "More Practice", "Pretest"]
   )
 )
 mymedians$effect_size <- mymedians$Estimate
 rownames(mymedians) <- as.integer(c(15, 3, 6, 9, 12, 20, 21))

 testthat::expect_equal(estimate$es_median_difference[15, esci_columns], mymedians[1, esci_columns])
 testthat::expect_equal(estimate$es_median_difference[3, esci_columns], mymedians[2, esci_columns])
 testthat::expect_equal(estimate$es_median_difference[6, esci_columns], mymedians[3, esci_columns])
 testthat::expect_equal(estimate$es_median_difference[9, esci_columns], mymedians[4, esci_columns])
 testthat::expect_equal(estimate$es_median_difference[12, esci_columns], mymedians[5, esci_columns])




 testthat::expect_snapshot(estimate)

}
)

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


    statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

    if (statpsych_version >= 170) {

      mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.mean.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y11,
            y21 = y22,
            y22 = y12
          )
        )[3, ]
      )
    } else {
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

    }

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)

    mytest <- esci::test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)

    # Main effect of B
    estimate <- estimates$main_effect_B

    if (statpsych_version >= 170) {
      mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.mean.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y11,
            y21 = y22,
            y22 = y12
          )
        )[2, ]
      )
    } else {
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
    }

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)


    mytest <- esci::test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)


    # Interaction
    estimate <- estimates$interaction


    if (statpsych_version >= 170) {

      mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.mean.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y11,
            y21 = y22,
            y22 = y12
          )
        )[1, ]
      )

      d_mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.stdmean.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y11,
            y21 = y22,
            y22 = y12
          )
        )[1, ]
      )

      mdn_mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.median.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y11,
            y21 = y22,
            y22 = y12
          )
        )[1, ]
      )
    } else {

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

      d_mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.stdmean.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y22,
            y21 = y11,
            y22 = y12
          )
        )[1, ]
      )

      mdn_mysp <- as.list(
        as.data.frame(
          statpsych::ci.2x2.median.mixed(
            alpha = 1 - myconf_level,
            y11 = y21,
            y12 = y22,
            y21 = y11,
            y22 = y12
          )
        )[1, ]
      )


    }

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_mean_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_mean_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_mean_difference$SE[3], mysp$SE)
    testthat::expect_equal(estimate$es_mean_difference$df[3], mysp$df)


    testthat::expect_equal(estimate$es_smd$effect_size[1], d_mysp$`adj Estimate`)
    testthat::expect_equal(estimate$es_smd$LL[1], d_mysp$LL)
    testthat::expect_equal(estimate$es_smd$UL[1], d_mysp$UL)
    testthat::expect_equal(estimate$es_smd$SE[1], d_mysp$SE)
    #testthat::expect_equal(estimate$es_smd$df[1], d_mysp$df)

    testthat::expect_equal(estimate$es_median_difference$effect_size[3], mdn_mysp$Estimate)
    testthat::expect_equal(estimate$es_median_difference$LL[3], mdn_mysp$LL)
    testthat::expect_equal(estimate$es_median_difference$UL[3], mdn_mysp$UL)
    testthat::expect_equal(estimate$es_median_difference$SE[3], mdn_mysp$SE)
    testthat::expect_equal(estimate$es_median_difference$df[3], mdn_mysp$df)



    mytest <- esci::test_mdiff(estimate)
    testthat::expect_equal(mytest$point_null$t, mysp$t)
    testthat::expect_equal(mytest$point_null$p, mysp$p)

  }

  testthat::expect_snapshot(estimates)

})

