test_that("pdiff_paired tests - Bonett & Price 2012 - Example 1", {

  # Example 1 from Bonett & Price, 2012
  estimate <- estimate_pdiff_paired(
    cases_consistent = 60,
    cases_inconsistent = 50,
    not_cases_inconsistent = 22,
    not_cases_consistent = 68,
    case_label = "Answered True",
    not_case_label = "Answered False",
    reference_measure_name = "9th grade",
    comparison_measure_name = "12th grade",
    conf_level = 0.95
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.prop.ps(.05, 60, 50, 22, 68)
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], 82/200)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[2], 110/200)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], 82/200 - 110/200)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI

  # Need an update that checks for statpsych 1.3 vs 1.4
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)

  suppressWarnings(myplot <- plot_pdiff(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

})


test_that("pdiff_paired tests - Bonett & Price 2012 - Example 2", {

  # Example 2 from Bonett & Price, 2012
  estimate <- estimate_pdiff_paired(
    cases_consistent = 18,
    cases_inconsistent = 4,
    not_cases_inconsistent = 12,
    not_cases_consistent = 5,
    case_label = "No Pain",
    not_case_label = "Pain",
    reference_measure_name = "Before Hypnosis",
    comparison_measure_name = "After Hypnosis",
    conf_level = 0.95
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.prop.ps(.05, 18, 4, 12, 5)
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], 30/39)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[2], 22/39)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], 30/39 - 22/39)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


})


test_that("pdiff_paired tests - Bonett & Price 2012 - Example 3", {

  # Example 3 from Bonett & Price, 2012
  estimate <- estimate_pdiff_paired(
    cases_consistent = 35,
    cases_inconsistent = 5,
    not_cases_inconsistent = 25,
    not_cases_consistent = 35,
    case_label = "Schizophrenic",
    not_case_label = "Not Schizoprhenic",
    reference_measure_name = "Diagnostician 2",
    comparison_measure_name = "Diagnostician 1",
    conf_level = 0.95
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.prop.ps(.05, 35, 5, 25, 35)
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], 60/100)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[2], 40/100)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], 60/100 - 40/100)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


})


test_that("pdiff_paired tests - Bonett & Price 2012 - Example 4", {

  # Example 4 from Bonett & Price, 2012
  estimate <- estimate_pdiff_paired(
    cases_consistent = 158,
    cases_inconsistent = 515,
    not_cases_inconsistent = 290,
    not_cases_consistent = 1134,
    case_label = "Rearrest",
    not_case_label = "No Rearrest",
    reference_measure_name = "Adult Court",
    comparison_measure_name = "Juvenile Court",
    conf_level = 0.95
  )

  mysp <- as.list(
    as.data.frame(
      statpsych::ci.prop.ps(.05, 158, 515, 290, 1134)
    )[1, ]
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[1], (158+290)/2097 )
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[2], (158+515)/2097)
  testthat::expect_equal(estimate$es_proportion_difference$effect_size[3], (158+290)/2097 - (158+515)/2097)
  # Can't compare the CI to esci because statpsych uses a slight adjustment
  #  in its calculation of the CI
  testthat::expect_equal(estimate$es_proportion_difference$LL[3], mysp$LL)
  testthat::expect_equal(estimate$es_proportion_difference$UL[3], mysp$UL)


})



