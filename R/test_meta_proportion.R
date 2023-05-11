test_estimate_meta_proportion <- function() {

  esci_meta_proportion <- data.frame(
    studies = c(
      "Online",
      "Original",
      "Online Pilot",
      "Exact replication"
    ),
    egocentric = c(
      33,
      4,
      4,
      7
    ),
    sample_size = c(
      101,
      33,
      10,
      53
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

  estimate <- meta_proportion(
    esci_meta_proportion,
    egocentric,
    sample_size,
    studies
  )
  estimate$raw_data
  estimate


  estimate <- meta_proportion(
    esci_meta_proportion,
    egocentric,
    sample_size,
    studies,
    setting
  )
  estimate$raw_data
  estimate

  # Bad calls with good errors ----------------------
  bad_data <- esci_meta_proportion
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

