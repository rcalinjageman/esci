test_r <- function() {

  # Summary example from esci, should give:
  #  comparison r 95% CI [.286, .709]
  #  reference r 95% CI [.172, .603]
  #  difference .12 95% CI [-.191, .418]
  estimate_rdiff_summary <- estimate_rdiff_two(
    comparison_r = .53,
    comparison_n = 47,
    reference_r = .41,
    reference_n = 59
  )


  rdiff_two <- estimate_rdiff_two(
    comparison_r = .53,
    comparison_n = 45,
    reference_r = .41,
    reference_n = 59,
    grouping_variable_levels = c("Females", "Males"),
    x_variable_name = "Satisfaction with life",
    y_variable_name = "Body satisfaction",
    grouping_variable_name = "Gender",
    conf_level = .95
  )


  myr2 <- data.frame(
    thex = rnorm(n = 100),
    they = rnorm(n = 100),
    thegroup = as.factor(sample(x = c("Men", "Women"), size = 100, replace = TRUE))
  )

  estimate_rdiff_raw <- estimate_rdiff_two(
    myr2,
    thex,
    they,
    thegroup
  )


  myr2 <- data.frame(
    thex = rnorm(n = 150),
    they = rnorm(n = 150),
    thegroup = as.factor(sample(x = c("Men", "Women", "Other"), size = 150, replace = TRUE))
  )

  myr2$thex[10:20] <- NA

  estimate_rdiff_raw <- estimate_rdiff_two(
    myr2,
    thex,
    they,
    thegroup
  )
  myplot <- plot_scatter(estimate_rdiff_raw)



  mystart <- rnorm(n = 50)
  otherstart <- rnorm(n = 50)
  myr2 <- data.frame(
    thex = c(mystart, otherstart, rnorm(n = 50)),
    they = c(mystart + rnorm(n = 50), 0 -  otherstart + rnorm(n = 50), rnorm(n = 50)),
    thegroup = as.factor(c(rep("Men",50), rep("Other", 50), rep("Women", 50)))
  )

  estimate_rdiff_raw <- estimate_rdiff_two(
    myr2,
    thex,
    they,
    thegroup
  )
  myplot <- plot_scatter(estimate_rdiff_raw)
  myplot
}
