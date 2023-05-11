test_mdiff_paired <- function() {

  sd1 <- 4.28
  sd2 <- 3.4
  sdiff <- 2.13

  cor <- (sd1^2 + sd2^2 - sdiff^2) / (2*sd1*sd2)

  estimate <- estimate_mdiff_paired(
    comparison_mean = 14.25,
    comparison_sd = 4.28,
    reference_mean = 12.88,
    reference_sd = 3.4,
    n = 16,
    correlation = 0.87072223749,
    comparison_measure_name = "After",
    reference_measure_name = "Before"
  )


  bk_wrapper <- c(
    4	,
    4	,
    3	,
    2	,
    2	,
    5	,
    1	,
    1	,
    3	,
    1	,
    1	,
    2	,
    4	,
    3	,
    1	,
    1	,
    1	,
    3	,
    1	,
    1	,
    1	,
    5	,
    1	,
    4	,
    1	,
    3	,
    2	,
    4	,
    2	,
    1
  )

  wc_wrapper <- c(
    2	,
    3	,
    2	,
    1	,
    1	,
    2	,
    1	,
    1	,
    3	,
    2	,
    1	,
    1	,
    2	,
    4	,
    1	,
    1	,
    4	,
    2	,
    2	,
    1	,
    2	,
    2	,
    1	,
    3	,
    2	,
    2	,
    1	,
    1	,
    2	,
    2

  )

  wrapper <- data.frame(
    "wc" = wc_wrapper,
    "bk" = bk_wrapper
  )

  # Check - vector
  estimate_mdiff_paired(
    comparison_measure = wc_wrapper,
    reference_measure = bk_wrapper
  )

  # Check data frame, column as strings
  mdiff_paired <- estimate_mdiff_paired(
    data = wrapper,
    comparison_measure = "wc",
    reference_measure = "bk"
  )

  # Check data frame, columns as tidy
  estimate <- estimate_mdiff_paired(wrapper, wc, bk)

}
