test_that("estimate_r compared to ESCI_One_correlation, summary data", {

  # Underlying CI approach has changed, so CIs no longer match
  #  this test checks against values provided from
  #  statpsych

  estimate <- esci::estimate_r(
    r = 0.4,
    n = 30
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.cor(
        alpha = 1 - 0.95,
        cor = 0.4,
        s = 0,
        n = 30
      )
    )
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_r$effect_size[[1]], mysp$Estimate)
  testthat::expect_equal(estimate$es_r$LL[[1]], mysp$LL)
  testthat::expect_equal(estimate$es_r$UL[[1]], mysp$UL)

  suppressWarnings(myplot <- esci::plot_correlation(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})


test_that("estimate_r of from Thomason1, ESCI_Scatterplots", {
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
    )
  )

  estimate <- esci::estimate_r(
    thomason1,
    ls_pre,
    ls_post
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.cor(
        alpha = 1 - 0.95,
        cor = estimate$es_r$effect_size,
        s = 0,
        n = estimate$es_r$n
      )
    )
  )


  testthat::expect_equal(estimate$es_r$effect_size[[1]], mysp$Estimate)
  testthat::expect_equal(estimate$es_r$LL[[1]], mysp$LL)
  testthat::expect_equal(estimate$es_r$UL[[1]], mysp$UL)

  suppressWarnings(try(myplot <- esci::plot_correlation(estimate), silent = TRUE))
  testthat::expect_s3_class(myplot, "ggplot")

  suppressWarnings(splot <- esci::plot_scatter(estimate))
  testthat::expect_s3_class(splot, "ggplot")


})


test_that("Compare estimate_r to statpsych::ci.cor", {

  myconfs <- c(0.90, 0.95, 0.99)
  myrs <- seq(from = -0.99, to = 0.99, by = 0.1)
  myns <- seq(from = 5, to = 105, by = 20)

  for (myn in myns) {
    for (myconf_level in myconfs) {
      for (myr in myrs) {
        estimate <- esci::estimate_r(
          r = myr,
          n = myn,
          conf_level = myconf_level
        )

        mysp <- as.list(
          as.data.frame(
            statpsych::ci.cor(
              alpha = 1 - myconf_level,
              cor = myr,
              s = 0,
              n = myn
            )
          )
        )

        testthat::expect_equal(estimate$es_r$effect_size[[1]], mysp$Estimate)
        testthat::expect_equal(estimate$es_r$LL[[1]], mysp$LL)
        testthat::expect_equal(estimate$es_r$UL[[1]], mysp$UL)
        testthat::expect_equal(estimate$es_r$SE[[1]], mysp$SE)
        testthat::expect_equal(estimate$es_r$n, myn)


      }
    }

  }
})
