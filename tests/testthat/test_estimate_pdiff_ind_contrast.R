test_that("Compare estimate_pdiff_ind_contrast to ESCI_Two_proportions example", {

  # Esci two proportion - 78/252 vs 10/20
  estimate <- estimate_pdiff_ind_contrast(
    cases = c(78, 10),
    ns = c(252, 20),
    case_label = "egocentric",
    grouping_variable_levels = c("Original", "Replication"),
    contrast = c(-1, 1),
    conf_level = 0.95
  )


  mysp <- as.list(
    as.data.frame(
      statpsych::ci.prop2(.05, 10, 78, 20, 252)
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)

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


test_that("Call estimate_pdiff_ind_contrast with vector", {

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

    estimate <- estimate_pdiff_ind_contrast(
      outcome_variable = psych_status,
      grouping_variable = treatment,
      contrast = c("Control" = -1, "Treated" = 0, "Other" = 1),
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

  # testthat::expect_snapshot(estimate)


  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("Call estimate_pdiff_ind_contrast with dataframe", {

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

    estimate <- estimate_pdiff_ind_contrast(
      my_proportion_data,
      "psych_stat",
      "treat",
      contrast = c("Control" = -1, "Treated" = 0, "Other" = 1),
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

  # testthat::expect_snapshot(estimate)


  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")



})


test_that("Test different estimate_pdiff_ind_contrast calls", {

  estimate <- estimate_pdiff_ind_contrast(
    cases = c(10, 20, 30),
    ns = c(50, 50, 50),
    case_label = "Depressed",
    grouping_variable_levels = c("Control", "Drug", "Therapy"),
    contrast = c(-1, 1/2, 1/2)
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.prop.bs(
        alpha = 0.05,
        f = c(10, 20, 30),
        n = c(50, 50, 50),
        v = c(-1, 1/2, 1/2)
      )
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], (20+30)/(50+50))
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[2], 10/50)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], (20+30)/(50+50) - 10/50)
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


})
