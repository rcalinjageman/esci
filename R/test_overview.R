test_overview <- function() {
  # Esci in excel  - independent groups from summary data example ----------

  # Setup
  means <- c(37.5, 31.9, 41.2, 33.4, 29.9, 38.3)
  sds <- c(10, 13.5, 14.8, 10, 8.7, 10)
  ns <- c(19, 19, 19, 19, 19, 19)
  grouping_variable_levels <- c(
    "NFree10", "AFree10", "ADiet10", "NFree17", "AFree17", "ADiety17"
  )

  # Check - same result as in esci with 95% CI
  res <- overview(
    means = means,
    sds = sds,
    ns = ns,
    assume_equal_variance = TRUE,
    outcome_variable_name = "% time near target",
    grouping_variable_levels = grouping_variable_levels,
    grouping_variable_name = "Diet"
  )
  res
  # Should return mean_LL of 32.2253, 26.7252, 36.0252, 28.2252, 24.7252, 33.1252
  # Should return mean_UL of 42.6748, 37.0748, 46.3748, 38.5748, 35.0748, 43.4748

  # Check - same result as in esci with 99% CI
  res <- overview(
    means = means,
    sds = sds,
    ns = ns,
    assume_equal_variance = TRUE,
    outcome_variable_name = "% time near target",
    grouping_variable_levels = grouping_variable_levels,
    conf_level = 0.99
  )
  res
  # Should return mean_LL of 30.6545...
  # Should return mean_UL of 44.3455...

  # Check - ok to submit just 1 group
  overview(
    means = means[1],
    sds = sds[1],
    ns = ns[1],
    assume_equal_variance = TRUE,
    outcome_variable_name = "% time near target",
    grouping_variable_levels = grouping_variable_levels[1]
  )
  # Should return mean_LL of 32.6802
  # Should return mean_UL of 42.3198


  # Esci in excel - data two, pen group ---------------------------------
  # Setup
  transcription_score <- c(
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

  condition <- c(
    rep("Pen", 34),
    rep("Laptop", 31)
  )

  pen_laptop <- data.frame(
    transcription_score = transcription_score,
    condition = as.factor(condition),
    other_outcome = rnorm(n = 65, mean = 100, sd = 15)
  )

  # Check - returns same result as esci
  overview(
    outcome_variable = transcription_score,
    grouping_variable = condition,
    assume_equal_variance = FALSE
  )
  # Should return Laptop: M = 14.5194,  95% CI [11.8570, 17.1917]
  # Should return Pen: M =     8.81176, 95% CI [7.15464, 10.4689]

  # Check - ok to have an na in outcome_variable
  overview(
    outcome_variable = c(transcription_score, NA),
    grouping_variable = c(condition, "Pen"),
    outcome_variable_name = "transcription score",
    grouping_variable_name = "condition",
    assume_equal_variance = FALSE
  )

  # Check - ok to have an na in grouping_variable
  # Outcomes missing grups do not factor into CIs even if equal var is assumed
  overview(
    outcome_variable = c(transcription_score, rnorm(100, 14.5, 7.2)),
    grouping_variable = c(condition, rep(NA, 100)),
    assume_equal_variance = TRUE
  )

  # Check - ok to have an a group of n = 1
  # Note that the group doesn't factor into CIs because variance undefined
  overview(
    outcome_variable = c(transcription_score, 11.2),
    grouping_variable = c(condition, "Group_of_1"),
    assume_equal_variance = TRUE
  )

  # Check - vector doesn't have to have grouping variable
  overview(
    outcome_variable = c(transcription_score, NA),
    assume_equal_variance = FALSE
  )

  # Check - works ok with data frame
  overview(
    data = pen_laptop,
    outcome_variable = "transcription_score",
    grouping_variable = "condition",
    assume_equal_variance = FALSE
  )

  # Check - data frame can skip grouping variable
  overview(
    data = pen_laptop,
    outcome_variable = "transcription_score",
    assume_equal_variance = FALSE
  )

  # Check - list of outcome variables
  overview(
    data = pen_laptop,
    outcome_variable = c("transcription_score", "other_outcome"),
    grouping_variable = "condition",
    assume_equal_variance = TRUE
  )

  # Check - grouping variable not needed with list of outcome variables
  overview(
    data = pen_laptop,
    outcome_variable = c("transcription_score", "other_outcome"),
    assume_equal_variance = TRUE
  )

  # Check - tidy approach to passing column names
  overview(pen_laptop, transcription_score)
  overview(pen_laptop, transcription_score, condition, conf_level = 0.99)


  # Caught errors
  # Setup
  means <- c(37.5, 31.9, 41.2, 33.4, 29.9, 38.3)
  sds <- c(10, 13.5, 14.8, 10, 8.7, 10)
  ns <- c(19, 19, 19, 19, 19, 19)
  grouping_variable_levels <- c(
    "NFree10", "AFree10", "ADiet10", "NFree17", "AFree17", "ADiety17"
  )

  # Caught - conf_level out of bounds
  overview(
    means = means,
    sds = sds,
    ns = ns,
    conf_level = 95,
  )

  # Caught -  assume_equal_variance not a logical
  overview(
    means = means,
    sds = sds,
    ns = ns,
    assume_equal_variance = 2
  )

  # Caught - sd wrong length
  overview(
    means = means,
    sds = c(sds, 1),
    ns = ns,
  )

  # Caught - n wrong length
  overview(
    means = means,
    sds = sds,
    ns = c(ns, 1),
  )

  # Caught - means has na
  overview(
    means = c(means, NA),
    sds = c(sds, 1),
    ns = c(ns, 1),
  )

  # Caught - sds has na
  overview(
    means = c(means, 15),
    sds = c(sds, NA),
    ns = c(ns, 1),
  )

  # Caught - n has na
  overview(
    means = c(means, 15),
    sds = c(sds, 1),
    ns = c(ns, NA),
  )

  # Caught - n is negative
  overview(
    means = c(means, 15),
    sds = c(sds, 1),
    ns = c(ns, -1),
  )

  # Caught - n is not an integer
  overview(
    means = c(means, 15),
    sds = c(sds, 1),
    ns = c(ns, 10.2),
  )

  # Caught - sd is negative
  overview(
    means = c(means, 15),
    sds = c(sds, -1),
    ns = c(ns, 10),
  )

  # Caught - typo in outcome variable
  overview(
    data = pen_laptop,
    outcome_variable = c("transcription_score_typo", "other_outcome"),
    grouping_variable = "condition",
    assume_equal_variance = TRUE
  )

  # Caught - typo in grouping variable
  overview(
    data = pen_laptop,
    outcome_variable = c("transcription_score", "other_outcome"),
    grouping_variable = "condition_typo",
    assume_equal_variance = TRUE
  )


}
