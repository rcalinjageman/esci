test_mdiff_two <- function() {
  transcription_scores_c <- note_taking_c <- PlantGrowth <- weight <- group <- NULL

  # Check - from summary data, esci in excel summary two example
  mdiffestimate <- estimate_mdiff_two(
    comparison_mean = 12.09,
    comparison_sd = 5.52,
    comparison_n = 103,
    reference_mean = 6.88,
    reference_sd = 4.22,
    reference_n = 48,
    grouping_variable_levels = c("Ref-Laptop", "Comp-Pen"),
    outcome_variable_name = "% Transcription",
    grouping_variable_name = "Note-taking type",
    assume_equal_variance = TRUE
  )
  # Should give - Mdiff = 5.21  95% CI [3.433079  6.986921]

  estimate_mdiff_two(
    comparison_mean = 12.09,
    comparison_sd = 5.52,
    comparison_n = 103,
    reference_mean = 6.88,
    reference_sd = 4.22,
    reference_n = 48,
    grouping_variable_levels = c("Laptop", "Pen"),
    outcome_variable_name = "% Transcription",
    grouping_variable_name = "Note-taking type",
    conf_level = 0.99,
    assume_equal_variance = TRUE
  )
  # Should give - Mdiff = 5.21  95% CI [2.863664  7.556336]

  transcription_scores <- c(
    12.1	,
    6.5	,
    8.1	,
    7.6	,
    12.2	,
    10.8	,
    1	,
    2.9	,
    14.4	,
    8.4	,
    17.7	,
    20.1	,
    2.1	,
    11.1	,
    11.2	,
    10.7	,
    1.9	,
    5.2	,
    9.7	,
    5.2	,
    2.4	,
    7.1	,
    8.7	,
    8	,
    11.3	,
    8.5	,
    9.1	,
    4.5	,
    9.2	,
    13.3	,
    18.3	,
    2.8	,
    5.1	,
    12.4	,
    13.7	,
    21.1	,
    15.2	,
    30.4	,
    12.8	,
    9.6	,
    9.3	,
    17.7	,
    15.4	,
    8.7	,
    12.8	,
    10.6	,
    5.1	,
    16.7	,
    17.7	,
    8.7	,
    26.4	,
    18	,
    19	,
    16.9	,
    18.8	,
    8.5	,
    1.2	,
    11.5	,
    21.4	,
    10.3	,
    9	,
    12.8	,
    12	,
    34.7	,
    4.1

  )

  note_taking <- as.factor(
    c(
      rep("Pen", 34),
      rep("Laptop", 31)
    )
  )

  pen_laptop <- data.frame(
    "note_taking_c" = note_taking,
    "transcription_scores_c" = transcription_scores,
    "other" = rnorm(n = 65, mean = 100, sd = 15)
  )

  mdiff_two <- estimate_mdiff_two(
    outcome_variable = transcription_scores,
    grouping_variable = note_taking,
    assume_equal_variance = TRUE
  )

  estimate <- estimate_mdiff_two(pen_laptop, transcription_scores_c, note_taking_c)

  estimate_mdiff_two(
    data = pen_laptop,
    outcome_variable = c("transcription_scores_c", "other"),
    grouping_variable = "note_taking_c"
  )

  estimate_mdiff_two(
    PlantGrowth, weight, group
  )

  estimate_mdiff_two(
    data = datasets::PlantGrowth,
    outcome_variable = "weight",
    grouping_variable = "group"
  )

}
