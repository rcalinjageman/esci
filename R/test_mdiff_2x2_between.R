test_mdiff_2x2_between <- function() {
  # Esci in excel  - independent groups from summary data example ----------

  # Setup
  means <- c(1.5, 1.14, 1.38, 2.22)
  sds <- c(1.38, .96,1.5, 1.68)
  ns <- c(26, 26, 25, 26)
  grouping_variable_A_levels <- c("Evening", "Morning")
  grouping_variable_B_levels <- c("Sleep", "No Sleep")

  # Check - match esci with 95% CI
  estimate <- estimate_mdiff_2x2_between(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_A_levels = grouping_variable_A_levels,
    grouping_variable_B_levels = grouping_variable_B_levels,
    grouping_variable_A_name = "Testing Time",
    grouping_variable_B_name = "Rest",
    outcome_variable_name = "False Memory Score",
    assume_equal_variance = TRUE
  )
  estimate$main_effect_A
  estimate$interaction
  plot_mdiff(estimate$main_effect_A)
  plot_mdiff(estimate$main_effect_B)
  plot_mdiff(estimate$simple_effect_B_at_A1)
  plot_mdiff(estimate$simple_effect_B_at_A2)
  plot_mdiff(estimate$interaction)



  sound <- c(
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'
  )

  stress <- c(
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'

  )

  score <- c(
    5	,
    5	,
    4	,
    4	,
    2	,
    5	,
    6	,
    5	,
    10	,
    10	,
    8	,
    9	,
    11	,
    9	,
    10	,
    8	,
    8	,
    11	,
    10	,
    9	,
    8	,
    9	,
    10	,
    9	,
    15	,
    15	,
    13	,
    14	,
    15	,
    15	,
    13	,
    15

  )

  mydf <- data.frame(
    v1 = as.factor(sound),
    v2 = as.factor(stress),
    o = score
  )

  restimate <- estimate_mdiff_2x2_between(
    mydf,
    grouping_variable_A = v1,
    grouping_variable_B = v2,
    outcome_variable = o
  )

  myplot <- plot_mdiff(restimate$interaction)
  plot_interaction(restimate)


  sound <- c(
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Silence'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music'	,
    'Music' ,
    "zOther",
    "zOther",
    "zOther",
    NA,
    "zOther"
  )

  stress <- c(
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Calm'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed'	,
    'Stressed',
    "Calm",
    "Stressed",
    "Calm",
    "Stressed",
    NA

  )

  score <- c(
    5	,
    5	,
    4	,
    NA	,
    2	,
    5	,
    6	,
    5	,
    10	,
    10	,
    8	,
    9	,
    11	,
    9	,
    10	,
    8	,
    8	,
    11	,
    10	,
    9	,
    8	,
    9	,
    10	,
    9	,
    15	,
    15	,
    NA	,
    14	,
    15	,
    15	,
    13	,
    15,
    12,
    13,
    12,
    14,
    15

  )

  mydf <- data.frame(
    v1 = as.factor(sound),
    v2 = as.factor(stress),
    o = score
  )

  restimate <- estimate_mdiff_2x2_between(
    mydf,
    grouping_variable_A = v1,
    grouping_variable_B = v2,
    outcome_variable = o
  )

  myplot <- plot_mdiff(restimate$interaction)

}
