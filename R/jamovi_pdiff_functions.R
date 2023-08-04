jamovi_peffect_html <- function(tfix) {

  if (is.null(tfix)) return(NULL)
  if (nrow(tfix) == 0) return(tfix)

  tfix$case_label <- gsub("P_", "<i>P</i><sub>", tfix$case_label)

  tfix$effect_plus <- paste(
    tfix$case_label,
    "</sub>: ",
    tfix$effect,
    sep = ""
  )

#
#
#   if (is.null(tfix)) return(NULL)
#   if (nrow(tfix) == 0) return(tfix)
#   columns_to_fix <- c("effect")
#   for (x in 1:nrow(tfix)) {
#     for (c in columns_to_fix) {
#       old_effect <- tfix[x, c]
#       if (!is.null(old_effect)) {
#         last_under <- gregexpr("P_", old_effect, fixed=TRUE)[[1]]
#         last_under <- last_under[length(last_under)]
#
#         tfix[x, c] <- paste(
#           substr(old_effect, 1, last_under -2),
#           " <i>P</i><sub>",
#           substr(old_effect, last_under + 2, nchar(old_effect)),
#           "</sub>",
#           sep = ""
#         )
#       }
#
#     }
#
#   }

  return(tfix)

}



jamovi_add_htest_pdiff <- function(self, estimate) {
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
      convert_to_number = TRUE,
      my_value_name = "Hypothesis Evaluation: <i>H</i><sub>0</sub> boundary"
    )

    test_results <- test_pdiff(
      estimate,
      rope = c(rope_upper * -1, rope_upper),
      output_html = TRUE
    )

    # estimate$warnings <- c(
    #   estimate$warnings,
    #   names(rope_upper)
    # )

    estimate$point_null <- jamovi_peffect_html(test_results$point_null)
    estimate$interval_null <- jamovi_peffect_html(test_results$interval_null)

    if (!is.null(names(rope_upper))) {
      self$results$point_null$setVisible(TRUE)
      self$results$interval_null$setVisible(FALSE)
    }

  }

  return(estimate)

}
