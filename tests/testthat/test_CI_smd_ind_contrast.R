test_that("CI_smd_ind_contrast compare to publshed examples", {

  # Example from Kline, 2013
  #  Data in Table 3.4
  #  Worked out in Chapter 7
  #  See p. 202, non-central approach
  # With equal variance assumed and no correction, should give:
  #   d_s = -0.8528028 [-2.121155, 0.4482578]

  res <- esci::CI_smd_ind_contrast(
    means = c(13, 11, 15),
    sds = c(2.738613, 2.236068, 2.000000),
    ns = c(5, 5, 5),
    contrast = c(1, 0, -1),
    conf_level = 0.95,
    assume_equal_variance = TRUE,
    correct_bias = FALSE
  )



  testthat::expect_equal(res$effect_size, -0.8528028, tolerance = 1e-5)
  testthat::expect_equal(res$LL, -2.121155, tolerance = 1e-5)
  testthat::expect_equal(res$UL, 0.4482578, tolerance = 1e-5)
  testthat::expect_snapshot(res)



  # Example from statpsych, no corrrection for bias, d_avg

  m <- c(33.5, 37.9, 38.0, 44.1)
  sd <- c(3.84, 3.84, 3.65, 4.98)
  n <- c(10,10,10,10)
  v <- c(.5, .5, -.5, -.5)
  sres <- statpsych::ci.lc.stdmean.bs(.05, m, sd, n, v)

  res <- esci::CI_smd_ind_contrast(
    means = m,
    sds = sd,
    ns = n,
    contrasts <- v,
    assume_equal_variance = FALSE,
    correct_bias = FALSE
  )


  testthat::expect_equal(res$effect_size, sres[1, 1], tolerance = 1e-5)
  testthat::expect_equal(res$LL, sres[1, 4], tolerance = 1e-5)
  testthat::expect_equal(res$UL, sres[1, 5], tolerance = 1e-5)


  # Now same example but with correction for bias
  res <- esci::CI_smd_ind_contrast(
    means = m,
    sds = sd,
    ns = n,
    contrasts <- v,
    assume_equal_variance = FALSE,
    correct_bias = TRUE
  )


  testthat::expect_equal(res$effect_size, sres[1, 2], tolerance = 1e-5)
  testthat::expect_equal(res$LL, sres[1, 4], tolerance = 1e-5)
  testthat::expect_equal(res$UL, sres[1, 5], tolerance = 1e-5)


})

