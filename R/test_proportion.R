test_proportion <- function() {
  depression_status <- NULL

  # Esci one proportion - 8/22
  estimate_proportion(
    cases = c(8, 22-8)
  )
  # Should yield:
  #outcome_variable_name outcome_variable_level count  n         P      P_LL      P_UL       P_SE
  #1   My Outcome Variable                 Level1     8 22 0.3636364 0.1976126 0.5716182 0.09541133


  estimate_proportion(
    cases = c(8, 22-8),
    case_label = 2,
    outcome_variable_name = "My Variable",
    outcome_variable_levels = c("Affected", "Not Affected"),
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

  estimate <- estimate_proportion(outcome_variable = dep_status, count_NA = FALSE)
  estimate_proportion(
    outcome_variable = dep_status,
    outcome_variable_name = "Something",
    case_label = "Depressed",
    count_NA = TRUE
  )


  dep_data <- data.frame(
    depression_status = dep_status,
    other = as.factor(c(rep("G1", 12), rep("G2", 13)))
  )

  estimate_proportion(dep_data, depression_status, count_NA = TRUE)
  estimate_proportion(dep_data, "depression_status", count_NA = TRUE)
  estimate <- estimate_proportion(dep_data, c("depression_status", "other"), case_label = 2)


}

