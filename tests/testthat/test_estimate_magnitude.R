test_that("estimate_magnitude vs. pen group from ESCI_summary_two: summary data, 95% CI", {
  # At 95% CI
  summary_two <- test_ESCI_data()$summary_two
  estimate <- estimate_magnitude(
    mean = summary_two$pen_m,
    sd = summary_two$pen_s,
    n = summary_two$pen_n
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, 6.88)
  testthat::expect_equal(estimate$es_mean$LL, 5.654639732)
  testthat::expect_equal(estimate$es_mean$UL, 8.105360268)
})


test_that("estimate_magnitude vs. pen group from ESCI_summary_two: summary data, 99% CI", {
  # At 99% CI
  summary_two <- test_ESCI_data()$summary_two
  estimate <- estimate_magnitude(
    mean = summary_two$pen_m,
    sd = summary_two$pen_s,
    n = summary_two$pen_n,
    conf_level = 0.99
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, 6.88)
  testthat::expect_equal(estimate$es_mean$LL, 5.244825001)
  testthat::expect_equal(estimate$es_mean$UL, 8.515174999)
})

test_that("estimate_magnitude vs. raw data from ESCI-describe: match overview", {
  describe <- test_ESCI_data()$describe

  estimates <- list()
  estimates$from_data_frame <- estimate_magnitude(
    describe,
    laptop_transcription
  )
  myvec <- describe$laptop_transcription
  estimates$from_vector <- estimate_magnitude(
    outcome_variable = myvec,
    outcome_variable_name = "transcription %"
  )

  for (estimate in estimates) {
    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$overview$mean, 14.51935484)
    testthat::expect_equal(estimate$overview$sd, 7.285575674)
    testthat::expect_equal(estimate$overview$median, 12.8)
    testthat::expect_equal(estimate$overview$q1, 9.45)
    testthat::expect_equal(estimate$overview$q3, 17.85)
  }
})

test_that("estimate_magnitude vs. raw data from pen group of ESCI_data_two: match CI", {
  data_two <- test_ESCI_data()$data_two
  pen <- data_two[data_two$condition == "Pen", ]
  pen$condition <- NULL

  estimates <- list()

  # From data frame
  estimates$from_data_frame <- estimate_magnitude(
    pen,
    transcription
  )

  # From vector
  myvec <- pen$transcription
  estimates$from_vector <- estimate_magnitude(
    outcome_variable = myvec,
    outcome_variable_name = "transcription %"
  )

  # From data frame with some NA values
  estimates$from_data_frame_NA <- estimate_magnitude(
    rbind(pen, data.frame(transcription = c(NA, NA, NA))),
    transcription
  )

  # From vector
  myvec <- pen$transcription
  estimates$from_vector_NA <- estimate_magnitude(
    outcome_variable = c(myvec, NA, NA),
    outcome_variable_name = "transcription %"
  )

  for (estimate in estimates) {
    testthat::expect_s3_class(estimate, "esci_estimate")
    testthat::expect_equal(estimate$es_mean$effect_size, 8.811764706)
    testthat::expect_equal(estimate$es_mean$LL, 7.154641985)
    testthat::expect_equal(estimate$es_mean$UL, 10.46888743)
    testthat::expect_equal(estimate$es_mean$df, 33)
  }
})


test_that("estimate_magnitude from ESCI_data_paired: jamovi style, match CI2", {
  data_paired <- test_ESCI_data()$data_paired

  estimate <- estimate_magnitude(
    data = data_paired,
    outcome_variable = c("pretest", "posttest")
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_mean$effect_size, c(11.58333333, 13.25))
  testthat::expect_equal(estimate$es_mean$LL, c(9.476776251, 11.41001901))
  testthat::expect_equal(estimate$es_mean$UL, c(13.68989042, 15.08998099))
  testthat::expect_equal(estimate$overview$sd, c(3.315482505, 2.895921897))
  testthat::expect_equal(estimate$es_mean$df, c(11, 11))

})


test_that("estimate_magnitude sensible errors", {
  data_two <- test_ESCI_data()$data_two
  pen <- data_two[data_two$condition == "Pen", ]
  pen$condition <- NULL

  # Data passed, don't include summary
  terror <- testthat::expect_error(
    estimate_magnitude(
      pen, transcription1, mean = 10
    )
  )
  terror <- testthat::expect_error(
    estimate_magnitude(
      pen, transcription1, sd = 1
    )
  )
  terror <- testthat::expect_error(
    estimate_magnitude(
      pen, transcription1, n = 1
    )
  )


  # Column names
  terror <- testthat::expect_error(
    estimate_magnitude(
      pen, transcription1
    )
  )
  testthat::expect_s3_class(terror, "error_invalid_column_name")

  # CI out of range
  terror <- testthat::expect_error(
    estimate_magnitude(
      pen, transcription, conf_level = 0
    )
  )
  testthat::expect_s3_class(terror, "error_out_of_range")

  terror <- testthat::expect_error(
    estimate_magnitude(
      pen, transcription, conf_level = 1
    )
  )
  testthat::expect_s3_class(terror, "error_out_of_range")


  # Summary data - bad sd or n
  # CI out of range
  summary_two <- test_ESCI_data()$summary_two
  terror <- testthat::expect_error(
    estimate <- estimate_magnitude(
      mean = summary_two$pen_m,
      sd = -1,
      n = summary_two$pen_n
    )
  )
  testthat::expect_s3_class(terror, "error_out_of_range")


})
