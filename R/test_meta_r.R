test_estimate_meta_sr <- function() {

  esci_single_r <- data.frame(
    studies = c(
      'Violin, viola'	,
      'Strings'	,
      'Piano'	,
      'Piano'	,
      'Piano'	,
      'Piano'	,
      'Piano'	,
      'Piano'	,
      'Piano'	,
      'All'	,
      'Piano'	,
      'Piano'	,
      'Band'	,
      'Music majors'	,
      'Music majors'	,
      'All'
    ),
    rvalues = c(
      .67,
      .51,
      .4,
      .46,
      .47,
      .228,
      -.224,
      .104,
      .322,
      .231,
      .67,
      .41,
      .34,
      .31,
      .54,
      .583
    ),
    sample_size = c(
      109,
      55,
      19,
      30,
      19,
      52,
      24,
      52,
      16,
      97,
      57,
      107,
      178,
      64,
      19,
      135
    ),
    subsets = as.factor(
      c(
        'Strings'	,
        'Strings'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Piano'	,
        'Strings'	,
        'Strings'	,
        'Strings'	,
        'Strings'
      )
    )
  )

  estimate <- meta_r(
    esci_single_r,
    rvalues,
    sample_size,
    studies,
    random_effects = TRUE
  )
  estimate$raw_data
  estimate

  estimate <- meta_r(
    esci_single_r,
    rvalues,
    sample_size,
    studies,
    subsets,
    random_effects = FALSE
  )
  estimate$raw_data
  estimate

  estimate <- meta_r(
    esci_single_r,
    rvalues,
    sample_size,
    studies,
    subsets,
    contrast = c(0, 1, -1),
    random_effects = FALSE,
    conf_level = 0.99
  )
  estimate$raw_data
  estimate


  # Bad calls with good errors ----------------------
  bad_r <- esci_single_r
  bad_r[c(1, 10), "rvalues"] <- 1.1
  bad_r[c(1, 10), "rvalues"] <- NA
  bad_r[c(1, 10), "sample_size"] <- -1
  bad_r[c(1, 10), "sample_size"] <- NA
  estimate <- meta_r(
    bad_r,
    rvalues,
    sample_size,
    studies,
    subsets,
    contrast = c(0, 1, -1),
    random_effects = FALSE,
    conf_level = 0.99
  )

}

