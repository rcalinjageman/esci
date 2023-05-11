test_pdiff_ind_contrast <- function() {
  pdiff_contrast <- estimate_pdiff_ind_contrast(
    cases = c(78, 10),
    ns = c(252, 20),
    case_label = "egocentric",
    grouping_variable_levels = c("Replication", "Original"),
    contrast = c(1, -1),
    conf_level = 0.95
  )


  pdiff_contrast <- estimate_pdiff_ind_contrast(
    cases = c(10, 20, 30),
    ns = c(50, 50, 50),
    case_label = "Depressed",
    grouping_variable_levels = c("Control", "Drug", "Therapy"),
    contrast = c(-1, 1/2, 1/2)
  )

  pdiff_contrast <- estimate_pdiff_ind_contrast(
    cases = c(48, 50, 45),
    ns = c(50, 50, 50),
    case_label = "Depressed",
    grouping_variable_levels = c("Control", "Drug", "Therapy"),
    contrast = c(-1, 1/2, 1/2)
  )


  my_outcome <- as.factor(
    sample(
      x = c("Depressed", "Not Depressed", "No Answer"),
      size = 300,
      replace = TRUE
    )
  )

  my_group <- as.factor(
    sample(
      x = c("Drug", "Therapy", "Control"),
      size = 300,
      replace = TRUE
    )
  )

  # Vector
  estimate <- estimate_pdiff_ind_contrast(
    outcome_variable = my_outcome,
    grouping_variable = my_group,
    contrast = c(1, 0, -1)
  )

  # Vector, different case label, named contrast
  estimate <- estimate_pdiff_ind_contrast(
    outcome_variable = my_outcome,
    grouping_variable = my_group,
    contrast = c("Control" = -1, "Therapy" = 1), case_label = "Not Depressed"
  )

  # Vector, NA in grouping variable
  my_group[10:12] <- NA
  estimate <- estimate_pdiff_ind_contrast(
    outcome_variable = my_outcome,
    grouping_variable = my_group,
    contrast = c("Control" = -1, "Missing" = 1), case_label = "Not Depressed"
  )


  # Vector, NA in grouping variable
  my_outcome[50:52] <- NA
  estimate <- estimate_pdiff_ind_contrast(
    outcome_variable = my_outcome,
    grouping_variable = my_group,
    contrast = c("Control" = -1, "Missing" = 1), case_label = "Not Depressed",
    count_NA = TRUE
  )



  # Data frame
  my_outcome <- as.factor(
    sample(
      x = c("Depressed", "Not Depressed", "No Answer"),
      size = 300,
      replace = TRUE
    )
  )

  another_outcome <- as.factor(
    sample(
      x = c("Anxious", "Not Anxious", "No Answer"),
      size = 300,
      replace = TRUE
    )
  )

  my_group <- as.factor(
    sample(
      x = c("Drug", "Therapy", "Control"),
      size = 300,
      replace = TRUE
    )
  )

  mydf <- data.frame(
    outcome = my_outcome,
    anxiety = another_outcome,
    gv = my_group
  )

  estimate_pdiff_ind_contrast(
    mydf, outcome, gv
  )

  estimate_pdiff_ind_contrast(
    mydf, outcome, gv, contrast = c("Drug" = 1, "Control" = -1)
  )

  estimate_pdiff_ind_contrast(
    mydf, "outcome", "gv", contrast = c("Drug" = 1, "Control" = -1)
  )

  # Jamovi
  estimate_pdiff_ind_contrast(
    mydf, c("outcome", "anxiety"), "gv", contrast = c("Drug" = 1, "Control" = -1)
  )

}
