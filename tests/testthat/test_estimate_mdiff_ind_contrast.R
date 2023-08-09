test_that("Compare estimate_mdiff_ind_contrast to ESCI_Ind_groups_contrasts, Halagappa", {
  # At 95% CI

  data("data_halagappa_et_al_2007")

  estimate <- estimate_mdiff_ind_contrast(
    means = data_halagappa_et_al_2007$m,
    sds = data_halagappa_et_al_2007$s,
    ns = data_halagappa_et_al_2007$n,
    grouping_variable_levels = as.character(data_halagappa_et_al_2007$condition),
    assume_equal_variance = TRUE,
    contrast = c(
      "NFree10" = 1/3,
      "AFree10" = 1/3,
      "ADiet10" = -1/3,
      "NFree17" = -1/3,
      "AFree17" = 1/3,
      "ADiet17" = -1/3
    ),
    grouping_variable_name = "Diet",
    outcome_variable_name = "% time near target"
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], -4.533333333)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], -8.75854555)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], -0.308121117)
  testthat::expect_equal(estimate$es_mean_difference$df[3], 108)

  group_means <- c(
    37.500000000,	31.900000000,	41.200000000,	33.400000000,	29.900000000,	38.300000000
  )
  group_ss <- c(
    10.000000,	13.500000,	14.800000,	10.000000,	8.700000,	10.000000
  )
  group_LLs <- c(
    32.325193008,	26.725193008,	36.025193008,	28.225193008,	24.725193008,	33.125193008
  )
  group_ULs <- c(
    42.674806992,	37.074806992,	46.374806992,	38.574806992,	35.074806992,	43.474806992
  )

  testthat::expect_equal(estimate$overview$mean, group_means)
  testthat::expect_equal(estimate$overview$sd, group_ss)
  testthat::expect_equal(estimate$overview$mean_LL, group_LLs)
  testthat::expect_equal(estimate$overview$mean_UL, group_ULs)

  # At 99% CI
  estimate <- estimate_mdiff_ind_contrast(
    means = data_halagappa_et_al_2007$m,
    sds = data_halagappa_et_al_2007$s,
    ns = data_halagappa_et_al_2007$n,
    grouping_variable_levels = as.character(data_halagappa_et_al_2007$condition),
    assume_equal_variance = TRUE,
    contrast = c(
      "NFree10" = 1/3,
      "AFree10" = 1/3,
      "ADiet10" = -1/3,
      "NFree17" = -1/3,
      "AFree17" = 1/3,
      "ADiet17" = -1/3
    ),
    grouping_variable_name = "Diet",
    outcome_variable_name = "% time near target",
    conf_level = 0.99
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean_difference$effect_size[3], -4.533333333)
  testthat::expect_equal(estimate$es_mean_difference$LL[3], -10.1226593)
  testthat::expect_equal(estimate$es_mean_difference$UL[3], 1.055992629)

  # Plot
  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  # NHST
  mytest <- test_mdiff(estimate)
  testthat::expect_equal(mytest$point_null$p, .0357202984311926)

})

test_that("Test different call types to estimate_mdiff_ind_contrasts and compare to statpsych::ci.lc.median.bs", {


  rattan_motivation <- c(
    5.5	,
    5	,
    5.5	,
    6	,
    1	,
    2.5	,
    4.5	,
    1	,
    3.5	,
    1.5	,
    5.5	,
    6	,
    1.5	,
    1	,
    3.5	,
    2.5	,
    3	,
    1	,
    2	,
    6	,
    4.5	,
    4.5	,
    6	,
    7	,
    3	,
    7	,
    3.5	,
    5	,
    4.5	,
    5.5	,
    6.5	,
    6	,
    6	,
    7	,
    5.5	,
    6	,
    2.5	,
    4.5	,
    3.5	,
    6	,
    5	,
    6	,
    3.5	,
    4	,
    3	,
    5.5	,
    3	,
    6	,
    3	,
    5	,
    6	,
    6.5	,
    3.5	,
    2
  )

  rattan_score <- c(
    73,
    88,
    97,
    65,
    67,
    87,
    94,
    45,
    88,
    77,
    68,
    98,
    99,
    78,
    69,
    35,
    54,
    53,
    89,
    78,
    99,
    86,
    79,
    85,
    69,
    87,
    98,
    97,
    96,
    95,
    76,
    79,
    78,
    65,
    57,
    85,
    48,
    34,
    65,
    43,
    55,
    47,
    86,
    43,
    26,
    54,
    53,
    38,
    43,
    26,
    45,
    23,
    44,
    55
  )

  rattan_condition <- as.factor(
    c(
      rep("Comfort", 18),
      rep("Chaling", 17),
      rep("Control", 19)
    )
  )

  rattan <- data.frame(
    motivation = rattan_motivation,
    score = rattan_score,
    condition = rattan_condition,
    other_outcome = rnorm(n = 18+17+19, mean = 100, sd = 15)
  )

  contrast <- c("Comfort" = -1/2, "Chaling" = 1, "Control" = -1/2)

  # Check - works with vector
  from_vector <- estimate_mdiff_ind_contrast(
    outcome_variable = rattan_motivation,
    grouping_variable = rattan_condition,
    contrast = contrast,
    assume_equal_variance = TRUE
  )
  testthat::expect_s3_class(from_vector, "esci_estimate")


  mysp <- as.list(
    as.data.frame(
      statpsych::ci.lc.median.bs(
        alpha = 1 - 0.95,
        m = from_vector$overview$median,
        se = from_vector$overview$median_SE,
        v = c(1, -1/2, -1/2)
      )
    )
  )

  estimate <- from_vector
  testthat::expect_equal(estimate$es_median_difference$effect_size[3], mysp$Estimate)
  testthat::expect_equal(estimate$es_median_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_median_difference$UL[3], mysp$UL)
  testthat::expect_equal(estimate$es_median_difference$SE[3], mysp$SE)


  # Check - works with dataframe and no contrast
  from_df_nc <- estimate_mdiff_ind_contrast(
    data = rattan,
    outcome_variable = motivation,
    grouping_variable = condition,
    contrast = NULL
  )
  testthat::expect_s3_class(from_df_nc, "esci_estimate")


  # Check - works with dataframe
  from_df <- estimate_mdiff_ind_contrast(
    data = rattan,
    outcome_variable = motivation,
    grouping_variable = condition,
    contrast = contrast
  )
  testthat::expect_s3_class(from_df, "esci_estimate")


  # Check - Data frame - tidycolumns
  from_tidy <- estimate_mdiff_ind_contrast(
    rattan, motivation, condition,
    contrast = contrast
  )
  testthat::expect_s3_class(from_tidy, "esci_estimate")


  # Check - data frame multiple dvs
  from_df_dvs <- estimate_mdiff_ind_contrast(
    data = rattan,
    outcome_variable = c("motivation", "other_outcome"),
    grouping_variable = condition,
    contrast = contrast
  )
  testthat::expect_s3_class(from_df_dvs, "esci_estimate")


  # Check - warning if group levels auto-generated
  # estimate <- estimate_mdiff_ind_contrast(
  #   means = means,
  #   sds = sds,
  #   ns = ns,
  #   contrast = c(1/2, 1/2, -1/3, -1/3, -1/3, 0),
  #   assume_equal_variance = TRUE
  # )



})



test_that("Compare estimate_mdiff_ind_contrast to statpsych::ci.lc.median example and ", {

  m <- c(33.5, 37.9, 38.0, 44.1)
  sd <- c(3.84, 3.84, 3.65, 4.98)
  n <- c(10,10,10,10)
  v <- c(.5, .5, -.5, -.5)
  myaeq <- TRUE
  myconf_level <- 0.95


  myconfs <- c(0.90, 0.95, 0.99)
  myaeqs <- c(TRUE, FALSE)

  for (myaeq in myaeqs) {
    for (myconf_level in myconfs) {

      estimate <- esci::estimate_mdiff_ind_contrast(
        means = m,
        sds = sd,
        ns = n,
        contrast = v,
        conf_level = myconf_level,
        grouping_variable_levels = c("G1", "G2", "G3", "G4"),
        assume_equal_variance = myaeq
      )

      row_select <- if (myaeq) 1 else 2

      mysp <- as.list(
        as.data.frame(
          statpsych::ci.lc.mean.bs(
            alpha = 1 - myconf_level,
            m = m,
            sd = sd,
            n = n,
            v = v
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

      if (!myaeq) {
        mysp <- as.list(
          as.data.frame(
            statpsych::ci.lc.stdmean.bs(
              alpha = 1 - myconf_level,
              m = m,
              sd = sd,
              n = n,
              v = v
            )
          )[1, ]
        )

        testthat::expect_equal(estimate$es_smd$effect_size, mysp$Estimate)
        testthat::expect_equal(estimate$es_smd$LL, mysp$LL)
        testthat::expect_equal(estimate$es_smd$UL, mysp$UL)
        testthat::expect_equal(estimate$es_smd$SE, mysp$SE)
      }

    }


  }


  suppressWarnings(myplot <- plot_mdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})

