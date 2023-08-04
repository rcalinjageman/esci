test_overview <- function() {
  # Esci in excel  - independent groups from summary data example ----------

  # Setup
  means <- c(37.5, 31.9, 41.2, 33.4, 29.9, 38.3)
  sds <- c(10, 13.5, 14.8, 10, 8.7, 10)
  ns <- c(19, 19, 19, 19, 19, 19)
  grouping_variable_levels <- c(
    "NFree10", "AFree10", "ADiet10", "NFree17", "AFree17", "ADiety17"
  )
  contrast <- c(1/2, 1/2, -1/3, -1/3, -1/3, 0)

  # Check - match esci with 95% CI
  estimate_mdiff_ind_contrast(
    means = means,
    sds = sds,
    ns = ns,
    contrast = contrast,
    grouping_variable_levels = grouping_variable_levels,
    assume_equal_variance = TRUE
  )
  # For difference, should return -0.1333 95% CI [-4.8573, 4.5906]

  # Check - match esci with 99% CI
  estimate_mdiff_ind_contrast(
    means = means,
    sds = sds,
    ns = ns,
    contrast = contrast,
    grouping_variable_levels = grouping_variable_levels,
    outcome_variable_name = "% time near target",
    grouping_variable_name = "Condition",
    conf_level = 0.99,
    assume_equal_variance = TRUE
  )
  # For difference, should return -0.1333 95% CI [-6.3824, 6.11572]

  rattan_motivation <- c(
    5.5	,
    5	,
    5.5	,
    6	,
    1	,
    2.5	,
    4.5	,
    1	,
    3.5	,
    1.5	,
    5.5	,
    6	,
    1.5	,
    1	,
    3.5	,
    2.5	,
    3	,
    1	,
    2	,
    6	,
    4.5	,
    4.5	,
    6	,
    7	,
    3	,
    7	,
    3.5	,
    5	,
    4.5	,
    5.5	,
    6.5	,
    6	,
    6	,
    7	,
    5.5	,
    6	,
    2.5	,
    4.5	,
    3.5	,
    6	,
    5	,
    6	,
    3.5	,
    4	,
    3	,
    5.5	,
    3	,
    6	,
    3	,
    5	,
    6	,
    6.5	,
    3.5	,
    2
  )

  rattan_score <- c(
    73,
    88,
    97,
    65,
    67,
    87,
    94,
    45,
    88,
    77,
    68,
    98,
    99,
    78,
    69,
    35,
    54,
    53,
    89,
    78,
    99,
    86,
    79,
    85,
    69,
    87,
    98,
    97,
    96,
    95,
    76,
    79,
    78,
    65,
    57,
    85,
    48,
    34,
    65,
    43,
    55,
    47,
    86,
    43,
    26,
    54,
    53,
    38,
    43,
    26,
    45,
    23,
    44,
    55
  )

  rattan_condition <- as.factor(
    c(
      rep("Comfort", 18),
      rep("Chaling", 17),
      rep("Control", 19)
    )
  )

  rattan <- data.frame(
    motivation = rattan_motivation,
    score = rattan_score,
    condition = rattan_condition,
    other_outcome = rnorm(n = 18+17+19, mean = 100, sd = 15)
  )

  contrast <- c("Comfort" = -1/2, "Chaling" = 1, "Control" = -1/2)

  # Check - works with vector
  estimate_mdiff_ind_contrast(
    outcome_variable = rattan_motivation,
    grouping_variable = rattan_condition,
    contrast = contrast,
    assume_equal_variance = TRUE
  )


  # Check - works with dataframe
  mdiff_contrast <- estimate_mdiff_ind_contrast(
    data = rattan,
    outcome_variable = motivation,
    grouping_variable = condition,
    contrast = NULL
  )

  # Check - works with dataframe
  mdiff_contrast <- estimate_mdiff_ind_contrast(
    data = rattan,
    outcome_variable = motivation,
    grouping_variable = condition,
    contrast = contrast
  )

  # Check - Data frame - tidycolumns
  estimate_mdiff_ind_contrast(
    rattan, motivation, condition,
    contrast = contrast
  )

  # Check - data frame multiple dvs
  estimate_mdiff_ind_contrast(
    data = rattan,
    outcome_variable = c("motivation", "other_outcome"),
    grouping_variable = condition,
    contrast = contrast
  )

  # Check - warning if group levels auto-generated
  estimate <- estimate_mdiff_ind_contrast(
    means = means,
    sds = sds,
    ns = ns,
    contrast = c(1/2, 1/2, -1/3, -1/3, -1/3, 0),
    assume_equal_variance = TRUE
  )
  estimate
}
