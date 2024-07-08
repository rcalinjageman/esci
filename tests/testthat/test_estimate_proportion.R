test_that("Compare estimate_proportion to ESCI_One_Proportion example", {
  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  estimate <- esci::estimate_proportion(
    cases = c(8, 22-8),
    outcome_variable_levels = c("Affected", "Not Affected")
  )

  testthat::expect_s3_class(estimate, "esci_estimate")


  if (statpsych_version > 150) {
    mysp <- as.list(
      as.data.frame(
        statpsych::ci.prop(.05, 8, 22)
      )[1, ]
    )

  } else {
    mysp <- as.list(
      as.data.frame(
        statpsych::ci.prop1(.05, 8, 22)
      )[1, ]
    )

  }

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion$effect_size, .3636363636)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion$LL, mysp$LL)
  testthat::expect_equal(estimate$es_proportion$UL, mysp$UL)


  myplot <- plot_proportion(estimate)
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("Call estimate_proportion with vector", {
  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  dep_status <- as.factor(
    c(
      rep("Depressed", 8),
      rep("NotDepressed", 22-8),
      NA,
      NA,
      NA
    )
  )

  myconf_level <- 0.95
  myconf_levels <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconf_levels) {

    estimate <- estimate_proportion(
      outcome_variable = dep_status,
      count_NA = FALSE,
      conf_level = myconf_level
    )

    if (statpsych_version > 150) {
      mysp <- as.list(
        as.data.frame(
          statpsych::ci.prop(1 - myconf_level, 8, 22)
        )[1, ]
      )

    } else {
      mysp <- as.list(
        as.data.frame(
          statpsych::ci.prop1(1 - myconf_level, 8, 22)
        )[1, ]
      )

    }

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_proportion$effect_size, .3636363636)
    # Can't compare the CI to esci because statpsych uses a slight adjustment
    #  in its calculation of the CI
    testthat::expect_equal(estimate$es_proportion$LL, mysp$LL)
    testthat::expect_equal(estimate$es_proportion$UL, mysp$UL)


  }

  myplot <- plot_proportion(estimate)
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Call estimate_proportion with dataframe", {

  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  dep_status <- as.factor(
    c(
      rep("Depressed", 8),
      rep("NotDepressed", 22-8),
      NA,
      NA,
      NA
    )
  )

  dep_data <- data.frame(
    depression_status = dep_status,
    other = as.factor(c(rep("G1", 12), rep("G2", 13)))
  )

  myconf_level <- 0.95
  myconf_levels <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconf_levels) {

    estimate <- estimate_proportion(
      dep_data,
      depression_status,
      count_NA = FALSE,
      conf_level = myconf_level
    )

    if (statpsych_version > 150) {
      mysp <- as.list(
        as.data.frame(
          statpsych::ci.prop(1 - myconf_level, 8, 22)
        )[1, ]
      )

    } else {
      mysp <- as.list(
        as.data.frame(
          statpsych::ci.prop1(1 - myconf_level, 8, 22)
        )[1, ]
      )
    }


    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_proportion$effect_size, .3636363636)
    # Can't compare the CI to esci because statpsych uses a slight adjustment
    #  in its calculation of the CI
    testthat::expect_equal(estimate$es_proportion$LL, mysp$LL)
    testthat::expect_equal(estimate$es_proportion$UL, mysp$UL)


  }

  myplot <- plot_proportion(estimate)
  testthat::expect_s3_class(myplot, "ggplot")


})
