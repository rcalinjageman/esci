test_that("Some tests for meta_proportion; needs development ", {

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

  for (mymodel in c(TRUE, FALSE)) {
    for (mymod in c("NULL", "setting")) {
      estimate <- esci::meta_proportion(
        esci_meta_pdiff_two,
        control_egocentric,
        control_sample_size,
        studies,
        moderator = !!mymod,
        random_effects = mymodel
      )
      testthat::expect_s3_class(estimate, "esci_estimate")
    }
  }

  # Plot
  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})

