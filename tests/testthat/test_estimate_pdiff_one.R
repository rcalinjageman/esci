test_that("Compare estimate_pdiff_one to ESCI_One_Proportion example", {
  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  # Esci one proportion - 8/22
  estimate <- estimate_pdiff_one(
    comparison_cases = 8,
    comparison_n = 22,
    reference_p = 0.5
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
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], .3636363636)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion_difference$LL[1], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[1], mysp$UL)


  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], .3636363636 - 0.5)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL - 0.5)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL - 0.5)

  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("Call estimate_pdiff_one with vector", {

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

    estimate <- estimate_pdiff_one(
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
    testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], .3636363636)
    # Can't compare the CI to esci because statpsych uses a slight adjustment
    #  in its calculation of the CI
    testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


  }

  suppressWarnings(myplot <- plot_pdiff(estimate))
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

    estimate <- estimate_pdiff_one(
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
    testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], .3636363636)
    # Can't compare the CI to esci because statpsych uses a slight adjustment
    #  in its calculation of the CI
    testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


  }

  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})
