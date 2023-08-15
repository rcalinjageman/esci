test_that("Compare estimate_pdiff_two to ESCI_Two_proportions example", {

  # Esci two proportion - 78/252 vs 10/20
  estimate <- estimate_pdiff_two(
    comparison_cases = 10,
    comparison_n = 20,
    reference_cases = 78,
    reference_n = 252,
    grouping_variable_levels = c("Original", "Replication"),
    conf_level = 0.95
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.prop2(.05, 10, 78, 20, 252)
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], 10/20)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[2], 78/252)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], 10/20 - 78/252)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)

  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("Call estimate_pdiff_two with vector", {

  psych_stat <- treat <- NULL

  psych_status <- as.factor(
    sample(
      x = c("Depressed", "Anxious"),
      size = 300,
      replace = TRUE
    )
  )

  treatment <- as.factor(
    sample(
      x = c("Control", "Treated", "Other"),
      size = 300,
      replace = TRUE
    )
  )

  my_proportion_data <- data.frame(
    psych_stat = psych_status,
    treat = treatment
  )

  myconf_level <- 0.95
  myconf_levels <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconf_levels) {

    estimate <- estimate_pdiff_two(
      outcome_variable = psych_status,
      grouping_variable = treatment,
      conf_level = myconf_level
    )

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.prop2(
          alpha = 1 - myconf_level,
          f1 = estimate$overview$cases[3],
          f2 = estimate$overview$cases[1],
          n1 = estimate$overview$n[3],
          n2 = estimate$overview$n[1]
        )
      )[1, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


  }

  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Call estimate_pdiff_two with dataframe", {

  psych_stat <- treat <- NULL

  psych_status <- as.factor(
    sample(
      x = c("Depressed", "Anxious"),
      size = 300,
      replace = TRUE
    )
  )

  treatment <- as.factor(
    sample(
      x = c("Control", "Treated", "Other"),
      size = 300,
      replace = TRUE
    )
  )

  my_proportion_data <- data.frame(
    psych_stat = psych_status,
    treat = treatment
  )

  myconf_level <- 0.95
  myconf_levels <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconf_levels) {

    estimate <- estimate_pdiff_two(
      my_proportion_data,
      "psych_stat",
      "treat",
      case_label = "Anxious",
      conf_level = myconf_level
    )

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.prop2(
          alpha = 1 - myconf_level,
          f1 = estimate$overview$cases[3],
          f2 = estimate$overview$cases[1],
          n1 = estimate$overview$n[3],
          n2 = estimate$overview$n[1]
        )
      )[1, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


  }

  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")



})
