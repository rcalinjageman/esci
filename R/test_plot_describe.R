test_plot_describe <- function() {


  pen_transcription <- c(
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
    12.4
  )

  # Check - vector
  estimate <- estimate_magnitude(
    outcome_variable = pen_transcription,
    conf_level = 0.99
  )


  plot_describe(estimate)



}
