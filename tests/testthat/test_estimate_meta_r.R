test_that("Some tests for meta_r; needs development ", {

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

  for (mymodel in c(TRUE, FALSE)) {
    for (mymod in c("subsets", "NULL")) {
      estimate <- meta_r(
        esci_single_r,
        rvalues,
        sample_size,
        studies,
        moderator = !!mymod,
        random_effects = mymodel
      )
      testthat::expect_s3_class(estimate, "esci_estimate")
    }
  }

  testthat::expect_snapshot(estimate)

  testthat::expect_equal(estimate$es_meta$effect_size[[1]], 0.42754504)
  testthat::expect_equal(estimate$es_meta$LL[[1]], 0.37518079)
  testthat::expect_equal(estimate$es_meta$UL[[1]], 0.47718919)

  # Plot
  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(myplot, "ggplot")


})

