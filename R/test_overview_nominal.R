test_overview_nominal <- function() {

  # Esci one proportion - 8/22
  overview_nominal(cases = c(8, 22-8))
  # Should yield:
  #outcome_variable_name outcome_variable_level count  n         P      P_LL      P_UL       P_SE
  #1   My Outcome Variable                 Level1     8 22 0.3636364 0.1976126 0.5716182 0.09541133


  overview_nominal(
    cases = c(8, 22-8),
    outcome_variable_levels = c("Depressed", "Not-Depressed"),
    outcome_variable_name = "Depression Status",
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

  overview_nominal(outcome_variable = dep_status, count_NA = FALSE)
  overview_nominal(outcome_variable = dep_status, count_NA = TRUE)


  dep_data <- data.frame(
    depression_status = dep_status,
    other = as.factor(c(rep("G1", 12), rep("G2", 13)))
  )

  overview_nominal(dep_data, depression_status, count_NA = TRUE)
  overview_nominal(dep_data, "depression_status", count_NA = TRUE)

  overview_nominal(dep_data, c("depression_status", "other"))

  dep_data$other[2:3] <- NA

  overview_nominal(dep_data, depression_status, other, count_NA = TRUE)

}
