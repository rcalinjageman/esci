test_pdiff_one <- function() {

  # Esci one proportion - 8/22
  pdiff_one <- estimate_pdiff_one(
    comparison_cases = 8,
    comparison_n = 22,
    reference_p = 0.10
  )


  estimate_pdiff_one(
    comparison_cases = 8,
    comparison_n = 22,
    case_label = "Depressed",
    outcome_variable_name = "My Variable",
    conf_level = 0.99
  )


  dep_status <- as.factor(
    c(
      rep("Depressed", 8),
      rep("NotDepressed", 22-8),
      NA,
      NA,
      NA
    )
  )

  pdiff_one <- estimate_pdiff_one(
    outcome_variable = dep_status,
    reference_p = 0.10,
    count_NA = FALSE
  )

  estimate_pdiff_one(
    outcome_variable = dep_status,
    outcome_variable_name = "Something",
    reference_p = 0.10,
    case_label = "Depressed",
    count_NA = TRUE
  )

  estimate_pdiff_one(
    outcome_variable = dep_status,
    outcome_variable_name = "Something",
    reference_p = 0.10,
    case_label = 2,
    count_NA = TRUE
  )

  dep_data <- data.frame(
    depression_status = dep_status,
    other = as.factor(c(rep("G1", 12), rep("G2", 13)))
  )

  estimate_pdiff_one(
    dep_data,
    depression_status,
    reference_p = 0.10,
    count_NA = TRUE
  )

  estimate_pdiff_one(
    dep_data,
    "depression_status",
    reference_p = 0.10,
    count_NA = TRUE
  )
  estimate <- estimate_pdiff_one(
    dep_data,
    c("depression_status", "other"),
    reference_p = 0.10,
    case_label = 2
  )


}
