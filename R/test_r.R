test_r <- function() {

  # Summary example from esci, should give .046 and .665
  estimate_r_summary <- estimate_correlation(
    r = 0.4,
    n = 30
  )


  # Summary example from esci, should give -.072, .725
  estimate_correlation(
    r = 0.4,
    n = 30,
    x_variable_name = "LSAT1",
    y_variable_name = "LSAT2",
    conf_level = 0.99
  )

  # From raw data
  ls_pre <- c(
    13,
    12,
    12,
    9,
    14,
    17,
    14,
    9,
    6,
    7,
    11,
    15
  )
  ls_post <- c(
    14,
    13,
    16,
    12,
    15,
    18,
    13,
    10,
    10,
    8,
    14,
    16
  )

  thomason1 <- data.frame(
    lsat_pre = ls_pre,
    lsat_post = ls_post,
    lsat_pre_missing = ls_pre,
    other = rnorm(n = 12),
    and_another = rnorm(n = 12),
    and_more = rnorm(n = 12)
  )

  thomason1[11, "lsat_pre_missing"] <- NA

  # Should give r = .892 95% CI [.653, .97]
  estimate_r_raw <- estimate_correlation(
    thomason1,
    lsat_pre,
    lsat_post
  )

  myplot <- plot_scatter(estimate_r_raw, show_mean_lines = TRUE, plot_as_z = TRUE, show_r = TRUE)

  plot_scatter(estimate_r_raw)


  estimate <- estimate_correlation(
    thomason1,
    lsat_pre_missing,
    lsat_post
  )
  estimate


  estimate_correlation(
    data = thomason1,
    x = "lsat_pre",
    y = c("lsat_post", "other", "and_another", "and_more", "lsat_pre_missing")
  )


  estimate_correlation(
    x = thomason1$lsat_pre,
    y = thomason1$lsat_post
  )




}
