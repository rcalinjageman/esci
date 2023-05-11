test_pdiff_paired <- function() {
  # From summary data -------------------
  pdiff_paired <- estimate_pdiff_paired(
    cases_consistent = 60,
    cases_inconsistent = 50,
    not_cases_consistent = 68,
    not_cases_inconsistent = 22,
    conf_level = 0.95
  )

  estimate_pdiff_paired(
    cases_consistent = 60,
    cases_inconsistent = 50,
    not_cases_consistent = 68,
    not_cases_inconsistent = 22,
    case_label = "Answered True",
    not_case_label = "Answered False",
    comparison_measure_name = "9th grade",
    reference_measure_name = "12th grade",
    conf_level = 0.95
  )


  # From raw data, data frame -------------------
  pre_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed"),
      size = 300,
      replace = TRUE,
      prob = c(0.75, 0.25)
    )
  )
  post_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed"),
      size = 300,
      replace = TRUE,
      prob = c(0.25, 0.75)
    )
  )

  d_treat <- data.frame(
    "before" = pre_test,
    "after" = post_test
  )

  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after"
  )


  # More than 2 levels -------------------
  pre_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer"),
      size = 300,
      replace = TRUE,
      prob = c(0.65, 0.25, 0.10)
    )
  )
  post_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer"),
      size = 300,
      replace = TRUE,
      prob = c(0.25, 0.65, 0.10)
    )
  )

  d_treat <- data.frame(
    "before" = pre_test,
    "after" = post_test
  )

  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after",
    case_label = "No Answer"
  )


  estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after",
    case_label = 3
  )


  # With NA values --------------------------------
  pre_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer", NA),
      size = 300,
      replace = TRUE,
      prob = c(0.65, 0.25, 0.05, 0.05)
    )
  )
  post_test <- as.factor(
    sample(
      c("Depressed", "Not Depressed", "No Answer", NA),
      size = 300,
      replace = TRUE,
      prob = c(0.25, 0.65, 0.05, 0.05)
    )
  )

  d_treat <- data.frame(
    "before" = pre_test,
    "after" = post_test
  )

  estimate <- estimate_pdiff_paired(
    data = d_treat,
    reference_measure = "before",
    comparison_measure = "after",
    case_label = "No Answer",
    count_NA = TRUE
  )

  # As a vector
  estimate <- estimate_pdiff_paired(
    reference_measure = pre_test,
    comparison_measure = post_test,
    case_label = "No Answer"
  )


  bh <- c(
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"
  )

  ah <- c(
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	No Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"	,
    "	Pain	"
  )

  bh <- trimws(bh)
  ah <- trimws(ah)

  mydf <- data.frame(before_h = factor(bh, levels = c("Pain", "No Pain")), after_h = factor(ah, levels = c("Pain", "No Pain")))
  mydf
  estimate <- estimate_pdiff_paired(mydf, after_h, before_h)


}
