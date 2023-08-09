test_that("Compare estimate_mdiff_2x2_between to ESCI_Ind_groups_2_x_2, Frenda", {
  # At 95% CI

  # Setup
  means <- c(1.5, 1.14, 1.38, 2.22)
  sds <- c(1.38, .96,1.5, 1.68)
  ns <- c(26, 26, 25, 26)
  grouping_variable_A_levels <- c("Evening", "Morning")
  grouping_variable_B_levels <- c("Sleep", "No Sleep")

  # Check - match esci with 95% CI
  estimates <- estimate_mdiff_2x2_between(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_A_levels = grouping_variable_A_levels,
    grouping_variable_B_levels = grouping_variable_B_levels,
    grouping_variable_A_name = "Testing Time",
    grouping_variable_B_name = "Rest",
    outcome_variable_name = "False Memory Score",
    assume_equal_variance = TRUE
  )


  # Main effect A
  estimate <- estimates$main_effect_A

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 0.48)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], -0.069157098725)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 1.029157098725)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 99)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$p, .08597066926)

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


  # Main effect B
  estimate <- estimates$main_effect_B

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 0.24)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], -0.309157098725)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 0.789157098725)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 99)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$p, .38794636218)

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


  # Interaction
  estimate <- estimates$interaction

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 1.2)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], 0.101685802551)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 2.298314197449)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 99)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$p, .03256133745)

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


  # simple_effect_B_at_A2
  estimate <- estimates$simple_effect_B_at_A2

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], 0.84)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], 0.059539372542)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 1.620460627458)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 99)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$p, .03517984586)

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


  # simple_effect_B_at_A1
  estimate <- estimates$simple_effect_B_at_A1

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], -0.36)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], -1.132771172668)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 0.412771172668)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 99)

  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$p, .35754695877)

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})



test_that("Compare estimate_mdiff_2x2_between to statpsych::ci.lc.stdmean.bs, Frenda", {

  # Setup
  means <- c(1.5, 1.14, 1.38, 2.22)
  sds <- c(1.38, .96,1.5, 1.68)
  ns <- c(26, 26, 25, 26)
  grouping_variable_A_levels <- c("Evening", "Morning")
  grouping_variable_B_levels <- c("Sleep", "No Sleep")

  # Check - match esci with 95% CI
  estimates <- estimate_mdiff_2x2_between(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_A_levels = grouping_variable_A_levels,
    grouping_variable_B_levels = grouping_variable_B_levels,
    grouping_variable_A_name = "Testing Time",
    grouping_variable_B_name = "Rest",
    outcome_variable_name = "False Memory Score",
    assume_equal_variance = FALSE
  )


  # Main effect A
  estimate <- estimates$main_effect_A

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.stdmean.bs(
        alpha = 1 - 0.95,
        m = means,
        sd = sds,
        n = ns,
        v = estimate$properties$contrast
      )
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
  testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
  testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
  testthat::expect_equal(estimate$es_smd$SE, mysp$SE)

  # Main effect B
  estimate <- estimates$main_effect_B

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.stdmean.bs(
        alpha = 1 - 0.95,
        m = means,
        sd = sds,
        n = ns,
        v = estimate$properties$contrast
      )
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
  testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
  testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
  testthat::expect_equal(estimate$es_smd$SE, mysp$SE)


  # Interaction
  estimate <- estimates$interaction

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.stdmean.bs(
        alpha = 1 - 0.95,
        m = means,
        sd = sds,
        n = ns,
        v = estimate$properties$contrast
      )
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
  testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
  testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
  testthat::expect_equal(estimate$es_smd$SE, mysp$SE)

  # simple_effect_B_at_A2
  estimate <- estimates$simple_effect_B_at_A2

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.stdmean.bs(
        alpha = 1 - 0.95,
        m = means,
        sd = sds,
        n = ns,
        v = estimate$properties$contrast
      )
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
  testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
  testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
  testthat::expect_equal(estimate$es_smd$SE, mysp$SE)


  # simple_effect_B_at_A1
  estimate <- estimates$simple_effect_B_at_A1

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.stdmean.bs(
        alpha = 1 - 0.95,
        m = means,
        sd = sds,
        n = ns,
        v = estimate$properties$contrast
      )
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
  testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
  testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
  testthat::expect_equal(estimate$es_smd$SE, mysp$SE)

})


test_that("Test different call types to estimate_mdiff_2x2between and compare to statpsych::ci.lc.median.bs", {

  sound <- c(
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'
  )

  stress <- c(
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'

  )

  score <- c(
    5	,
    5	,
    4	,
    4	,
    2	,
    5	,
    6	,
    5	,
    10	,
    10	,
    8	,
    9	,
    11	,
    9	,
    10	,
    8	,
    8	,
    11	,
    10	,
    9	,
    8	,
    9	,
    10	,
    9	,
    15	,
    15	,
    13	,
    14	,
    15	,
    15	,
    13	,
    15

  )

  mydf <- data.frame(
    v1 = as.factor(sound),
    v2 = as.factor(stress),
    o = score
  )

  estimates <- estimate_mdiff_2x2_between(
    mydf,
    grouping_variable_A = v1,
    grouping_variable_B = v2,
    outcome_variable = o
  )

  testthat::expect_s3_class(estimates, "esci_estimate")

  # Main effect of A
  estimate <- estimates$main_effect_A

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.median.bs(
        alpha = 1 - 0.95,
        m = estimate$overview$median,
        se = estimate$overview$median_SE,
        v = estimate$properties$contrast
      )
    )
  )

  testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
  testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
  testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)


  # Main effect of B
  estimate <- estimates$main_effect_B

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.median.bs(
        alpha = 1 - 0.95,
        m = estimate$overview$median,
        se = estimate$overview$median_SE,
        v = estimate$properties$contrast
      )
    )
  )

  testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
  testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
  testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)


  # Interaction
  estimate <- estimates$interaction

  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.median.bs(
        alpha = 1 - 0.95,
        m = estimate$overview$median,
        se = estimate$overview$median_SE,
        v = estimate$properties$contrast
      )
    )
  )

  testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
  testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
  testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)

})



test_that("Compare estimate_mdiff_2x2between to statpsych::ci.2x2.median.bs example ", {

  y11 = c(14, 15, 11, 7, 16, 12, 15, 16, 10, 9)
  y12 = c(18, 24, 14, 18, 22, 21, 16, 17, 14, 13)
  y21 = c(16, 11, 10, 17, 13, 18, 12, 16, 6, 15)
  y22 = c(18, 17, 11, 9, 9, 13, 18, 15, 14, 11)
  myconf_level <- 0.95

  mydf <- data.frame(
    y = c(y11, y12, y21, y22),
    A = as.factor(
      c(
        rep("A1", times = length(y11)+length(y12)),
        rep("A2", times = length(y21)+length(y22))
      )
    ),
    B = as.factor(
      c(
        rep("B1", times = length(y11)),
        rep("B2", times = length(y12)),
        rep("B1", times = length(y21)),
        rep("B2", times = length(y22))
      )
    )
  )


  myconfs <- c(0.90, 0.95, 0.99)

  for (myconf_level in myconfs) {

    estimates <- esci::estimate_mdiff_2x2_between(
      data = mydf,
      outcome_variable = "y",
      grouping_variable_A = "A",
      grouping_variable_B = "B",
      conf_level = myconf_level,
      assume_equal_variance = TRUE
    )


    # Main effect of A
    estimate <- estimates$main_effect_A

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.2x2.median.bs(
          alpha = 1 - myconf_level,
          y11 = y22,
          y12 = y21,
          y21 = y12,
          y22 = y11
        )
      )[2, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)

    suppressWarnings(myplot <- plot_mdiff(estimate, effect_size = "median"))
    testthat::expect_s3_class(myplot, "ggplot")


    # Main effect of B
    estimate <- estimates$main_effect_B

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.2x2.median.bs(
          alpha = 1 - myconf_level,
          y11 = y22,
          y12 = y21,
          y21 = y12,
          y22 = y11
        )
      )[3, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)

    suppressWarnings(myplot <- plot_mdiff(estimate, effect_size = "median"))
    testthat::expect_s3_class(myplot, "ggplot")


    # Interaction
    estimate <- estimates$interaction

    mysp <- as.list(
      as.data.frame(
        statpsych::ci.2x2.median.bs(
          alpha = 1 - myconf_level,
          y11 = y22,
          y12 = y21,
          y21 = y12,
          y22 = y11
        )
      )[1, ]
    )

    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
    testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
    testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
    testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)

    suppressWarnings(myplot <- plot_mdiff(estimate, effect_size = "median"))
    testthat::expect_s3_class(myplot, "ggplot")

  }


})

