test_pdiff_two <- function() {
  psych_stat <- treat <- NULL

  pdiff_two <- estimate_pdiff_two(
    comparison_cases = 10,
    comparison_n = 20,
    reference_cases = 78,
    reference_n = 252,
    conf_level = 0.95
  )

  pdiff_two <- estimate_pdiff_two(
    comparison_cases = 20,
    comparison_n = 200,
    reference_cases = 10,
    reference_n = 100,
    count_NA = FALSE,
    case_label = "Depressed",
    not_case_label = "Anxious",
    grouping_variable_levels = c("Original", "Replication"),
    outcome_variable_name = "Eval",
    grouping_variable_name = "Study",
    conf_level = 0.95
  )

  # o <- pdiff_two$overview
  # ctable <- NULL
  #
  # for (mylevel in levels(as.factor(o$grouping_variable_level))) {
  #   ctable <- rbind(
  #     ctable,
  #     as.data.frame(t(o[o$grouping_variable_level == mylevel, "cases"]))
  #   )
  # }
  # colnames(ctable) <-  t(o[o$grouping_variable_level == mylevel, "outcome_variable_level"])
  # rownames(ctable) <- levels(as.factor(o$grouping_variable_level))
  #
  # X <- chisq.test(ctable)

  estimate_pdiff_two(
    comparison_cases = 10,
    comparison_n = 20,
    reference_cases = 78,
    reference_n = 252,
    case_label = "Depressed",
    not_case_label = "Anxious",
    grouping_variable_levels = c("Original", "Replication"),
    outcome_variable_name = "Eval",
    grouping_variable_name = "Study",
    conf_level = 0.95
  )


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

  pdiff_two <- estimate_pdiff_two(
    outcome_variable = psych_status,
    grouping_variable = treatment
  )

  plot_pdiff(pdiff_two)

  estimate_pdiff_two(
    outcome_variable = psych_status,
    grouping_variable = treatment,
    case_label = 2
  )

  pdiff_two <- estimate_pdiff_two(
    outcome_variable = psych_status,
    grouping_variable = treatment,
    case_label = "Depressed"
  )

  estimate_pdiff_two(
    my_proportion_data,
    psych_stat,
    treat,
    case_label = "Depressed"
  )


  estimate_pdiff_two(
    my_proportion_data,
    psych_stat,
    treat,
    case_label = "Anxious"
  )


}
