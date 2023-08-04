
jamovi_add_htest_rdiff <- function(self, estimate) {
  evaluate_h <- self$options$evaluate_hypotheses

  if(evaluate_h) {
    # Test results

    rope_upper <- jamovi_sanitize(
      self$options$null_boundary,
      na_ok = FALSE,
      return_value = 0,
      lower = 0,
      lower_inclusive = TRUE,
      upper = 1,
      upper_inclusive = TRUE,
      convert_to_number = TRUE
    )

    test_results <- test_rdiff(
      estimate,
      rope = c(rope_upper * -1, rope_upper),
      output_html = TRUE
    )

    estimate$point_null <- test_results$point_null
    estimate$interval_null <- test_results$interval_null

    if (!is.null(names(rope_upper))) {
      self$results$point_null$setVisible(TRUE)
      self$results$interval_null$setVisible(FALSE)
    }

  }

  return(estimate)

}
