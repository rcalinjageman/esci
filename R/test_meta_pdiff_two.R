test_estimate_meta_pdiff_two <- function() {

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

  estimate <- meta_pdiff_two(
    esci_meta_pdiff_two,
    power_egocentric,
    power_sample_size,
    control_egocentric,
    control_sample_size,
    studies,
    reported_effect_size = "RD"
  )
  estimate$raw_data
  estimate
  estimate$properties$effect_size_name_html

  estimate <- meta_pdiff_two(
    esci_meta_pdiff_two,
    power_egocentric,
    power_sample_size,
    control_egocentric,
    control_sample_size,
    studies,
    reported_effect_size = "OR"
  )
  estimate$raw_data
  estimate
  estimate$properties$effect_size_name_html



  # Bad calls with good errors ----------------------
  bad_data <- esci_meta_pdiff_two
  bad_data[c(1), "egocentric"] <- 1.1
  bad_data[c(1), "egocentric"] <- NA
  bad_data[c(1), "sample_size"] <- -1
  bad_data[c(1), "sample_size"] <- NA
  estimate <- meta_proportion(
    bad_data,
    egocentric,
    sample_size,
    studies,
    random_effects = FALSE,
    conf_level = 0.99
  )

}

