test_that("estimate_rdiff_two compared to ESCI_Two_Correlations, summary data", {

  # Underlying CI approach has changed, so CIs no longer match
  #  this test checks against values provided from
  #  statpsych

  estimate <- esci::estimate_rdiff_two(
    comparison_r = .53,
    comparison_n = 47,
    reference_r = .41,
    reference_n = 59,
    grouping_variable_levels = c("Females", "Males"),
    x_variable_name = "Satisfaction with life",
    y_variable_name = "Body satisfaction",
    grouping_variable_name = "Gender",
    conf_level = .95
  )

  r1 <- as.list(
    as.data.frame(
      statpsych::ci.cor(
        alpha = 1 - 0.95,
        cor = .53,
        s = 0,
        n = 47
      )
    )
  )

  r2 <- as.list(
    as.data.frame(
      statpsych::ci.cor(
        alpha = 1 - 0.95,
        cor = .41,
        s = 0,
        n = 59
      )
    )
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.cor2(
        alpha = 1 - 0.95,
        cor1 = 0.53,
        cor2 = 0.41,
        n1 = 47,
        n2 = 59
      )
    )
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)

  testthat::expect_equal(estimate$es_r$effect_size[[1]], r2$Estimate)
  testthat::expect_equal(estimate$es_r$LL[[1]], r2$LL)
  testthat::expect_equal(estimate$es_r$UL[[1]], r2$UL)
  testthat::expect_equal(estimate$es_r$SE[[1]], r2$SE)

  testthat::expect_equal(estimate$es_r$effect_size[[2]], r1$Estimate)
  testthat::expect_equal(estimate$es_r$LL[[2]], r1$LL)
  testthat::expect_equal(estimate$es_r$UL[[2]], r1$UL)
  testthat::expect_equal(estimate$es_r$SE[[2]], r1$SE)

  testthat::expect_equal(estimate$es_r_difference$effect_size[[3]], mysp$Estimate)
  testthat::expect_equal(estimate$es_r_difference$LL[[3]], mysp$LL)
  testthat::expect_equal(estimate$es_r_difference$UL[[3]], mysp$UL)


  suppressWarnings(myplot <- esci::plot_correlation(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("estimate_rdiff_two of from Thomason1, ESCI_Scatterplots", {
  thomason1 <- data.frame(
    ls_pre = c(
      13,
      12,
      12,
      9,
      14,
      17,
      14,
      9,
      6,
      7,
      11,
      15
    ),
    ls_post = c(
      14,
      13,
      16,
      12,
      15,
      18,
      13,
      10,
      10,
      8,
      14,
      16
    ),
    major = c(
      "Humanities",
      "Humanities",
      "Humanities",
      "Humanities",
      "Humanities",
      "Humanities",
      "Science",
      "Science",
      "Science",
      "Science",
      "Science",
      "Science"
    )
  )

  thomason1$major <- as.factor(thomason1$major)

  estimate <- esci::estimate_rdiff_two(
    thomason1,
    ls_pre,
    ls_post,
    major
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.cor2(
        alpha = 1 - 0.95,
        cor1 = estimate$es_r$effect_size[[2]],
        cor2 = estimate$es_r$effect_size[[1]],
        n1 = estimate$es_r$n[[2]],
        n2 = estimate$es_r$n[[1]]
      )
    )
  )


  testthat::expect_snapshot(estimate)


  testthat::expect_equal(estimate$es_r_difference$effect_size[[3]], mysp$Estimate)
  testthat::expect_equal(estimate$es_r_difference$LL[[3]], mysp$LL)
  testthat::expect_equal(estimate$es_r_difference$UL[[3]], mysp$UL)

  suppressWarnings(try(myplot <- esci::plot_rdiff(estimate), silent = TRUE))
  testthat::expect_s3_class(myplot, "ggplot")

  suppressWarnings(splot <- esci::plot_scatter(estimate))
  testthat::expect_s3_class(splot, "ggplot")


})



test_that("Compare estimate_rdiff_two to statpsych::ci.cor", {

  myconfs <- c(0.95, 0.99)
  myrs <- seq(from = -0.90, to = 0.90, by = 0.5)
  myr2s <- seq(from = -0.90, to = 0.90, by = 0.5)
  myns <- seq(from = 5, to = 105, by = 45)

  for (myn in myns) {
    for (myconf_level in myconfs) {
      for (myr in myrs) {
        for (myr2 in myr2s) {

          suppressWarnings(
            estimate <- esci::estimate_rdiff_two(
              comparison_r = myr,
              comparison_n = myn,
              reference_r = myr2,
              reference_n = myn + 5,
              grouping_variable_levels = c("Females", "Males"),
              x_variable_name = "Satisfaction with life",
              y_variable_name = "Body satisfaction",
              grouping_variable_name = "Gender",
              conf_level = myconf_level
            )
          )

          mysp <- as.list(
            as.data.frame(
              statpsych::ci.cor2(
                alpha = 1 - myconf_level,
                cor1 = myr,
                cor2 = myr2,
                n1 = myn,
                n2 = myn + 5
              )
            )
          )

          testthat::expect_equal(estimate$es_r_difference$effect_size[[3]], mysp$Estimate)
          testthat::expect_equal(estimate$es_r_difference$LL[[3]], mysp$LL)
          testthat::expect_equal(estimate$es_r_difference$UL[[3]], mysp$UL)

        }
      }
    }
  }

  testthat::expect_snapshot(estimate)

})
