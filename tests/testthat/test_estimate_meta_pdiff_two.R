test_that("Some tests for meta_pdiff_two; needs development ", {

  esci_meta_pdiff_two <- data.frame(
    studies = c(
      "Online",
      "Original",
      "Online Pilot",
      "Exact replication"
    ),
    control_egocentric = c(
      33,
      4,
      4,
      7
    ),
    control_sample_size = c(
      101,
      33,
      10,
      53
    ),
    power_egocentric = c(
      48,
      8,
      4,
      11
    ),
    power_sample_size = c(
      105,
      24,
      12,
      56
    ),
    setting = as.factor(
      c(
        "Online",
        "In-Person",
        "Online",
        "In-Person"
      )
    )
  )

  for (etype in c("RD", "RR", "OR", "AS", "PETO")) {
    for (mymod in c("NULL", "setting")) {
      estimate <- esci::meta_pdiff_two(
        esci_meta_pdiff_two,
        power_egocentric,
        power_sample_size,
        control_egocentric,
        control_sample_size,
        studies,
        moderator = !!mymod,
        reported_effect_size = etype
      )
      testthat::expect_s3_class(estimate, "esci_estimate")
    }
  }


  testthat::expect_snapshot(estimate)

  # Plot
  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})

