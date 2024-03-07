test_that("overview to esci examples", {

  # Setup
  mydata <- esci::data_halagappa

  # Check - same result as in esci with 95% CI
  res <- overview(
    means = mydata$Mean,
    sds = mydata$SD,
    ns = mydata$n,
    assume_equal_variance = TRUE,
    outcome_variable_name = "% time near target",
    grouping_variable_levels = as.character(mydata$Groups),
    grouping_variable_name = "Diet"
  )

  # Should return mean_LL of 32.32519301	26.72519301	36.02519301	28.22519301	24.72519301	33.12519301
  # Should return mean_UL of 42.67480699	37.07480699	46.37480699	38.57480699	35.07480699	43.47480699


  testthat::expect_equal(res[res$grouping_variable_level == "NFree10", "mean_LL"], 32.32519301)
  testthat::expect_equal(res[res$grouping_variable_level == "NFree10", "mean_UL"], 42.67480699)

  testthat::expect_equal(res[res$grouping_variable_level == "AFree10", "mean_LL"], 26.72519301)
  testthat::expect_equal(res[res$grouping_variable_level == "AFree10", "mean_UL"], 37.07480699)

  testthat::expect_equal(res[res$grouping_variable_level == "ADiet10", "mean_LL"], 36.02519301)
  testthat::expect_equal(res[res$grouping_variable_level == "ADiet10", "mean_UL"], 46.37480699)

  testthat::expect_equal(res[res$grouping_variable_level == "NFree17", "mean_LL"], 28.22519301)
  testthat::expect_equal(res[res$grouping_variable_level == "NFree17", "mean_UL"], 38.57480699)

  testthat::expect_equal(res[res$grouping_variable_level == "AFree17", "mean_LL"], 24.72519301)
  testthat::expect_equal(res[res$grouping_variable_level == "AFree17", "mean_UL"], 35.07480699)

  testthat::expect_equal(res[res$grouping_variable_level == "ADiet17", "mean_LL"], 33.12519301)
  testthat::expect_equal(res[res$grouping_variable_level == "ADiet17", "mean_UL"], 43.47480699)


  # Check - same result as in esci with 99% CI
  res <- overview(
    means = mydata$Mean,
    sds = mydata$SD,
    ns = mydata$n,
    assume_equal_variance = TRUE,
    outcome_variable_name = "% time near target",
    grouping_variable_levels = as.character(mydata$Groups),
    grouping_variable_name = "Diet",
    conf_level = .99
  )

  # Should return mean_LL of 30.65450169	25.05450169	34.35450169	26.55450169	23.05450169	31.45450169
  # Should return mean_UL of 44.34549831	38.74549831	48.04549831	40.24549831	36.74549831	45.14549831

  testthat::expect_equal(res[res$grouping_variable_level == "NFree10", "mean_LL"], 30.65450169)
  testthat::expect_equal(res[res$grouping_variable_level == "NFree10", "mean_UL"], 44.34549831)

  testthat::expect_equal(res[res$grouping_variable_level == "AFree10", "mean_LL"], 25.05450169)
  testthat::expect_equal(res[res$grouping_variable_level == "AFree10", "mean_UL"], 38.74549831)

  testthat::expect_equal(res[res$grouping_variable_level == "ADiet10", "mean_LL"], 34.35450169)
  testthat::expect_equal(res[res$grouping_variable_level == "ADiet10", "mean_UL"], 48.04549831)

  testthat::expect_equal(res[res$grouping_variable_level == "NFree17", "mean_LL"], 26.55450169)
  testthat::expect_equal(res[res$grouping_variable_level == "NFree17", "mean_UL"], 40.24549831)

  testthat::expect_equal(res[res$grouping_variable_level == "AFree17", "mean_LL"], 23.05450169)
  testthat::expect_equal(res[res$grouping_variable_level == "AFree17", "mean_UL"], 36.74549831)

  testthat::expect_equal(res[res$grouping_variable_level == "ADiet17", "mean_LL"], 31.45450169)
  testthat::expect_equal(res[res$grouping_variable_level == "ADiet17", "mean_UL"], 45.14549831)


  #Ok for just one group
  res <- overview(
    means = mydata$Mean[1],
    sds = mydata$SD[1],
    ns = mydata$n[1],
    assume_equal_variance = TRUE,
    outcome_variable_name = "% time near target",
    grouping_variable_levels = as.character(mydata$Groups)[1],
    grouping_variable_name = "Diet"
  )

  testthat::expect_equal(res[res$grouping_variable_level == "NFree10", "mean_LL"], 32.68015428)
  testthat::expect_equal(res[res$grouping_variable_level == "NFree10", "mean_UL"], 42.31984572)


  # data frame
  mydata <- esci::data_penlaptop1

  res <- overview(
    data = mydata,
    outcome_variable = transcription,
    grouping_variable = condition,
    assume_equal_variance = FALSE
  )

  testthat::expect_equal(res[res$grouping_variable_level == "Pen", "mean"], 8.81176, tolerance = 1e-5)
  testthat::expect_equal(res[res$grouping_variable_level == "Pen", "mean_LL"], 7.15464, tolerance = 1e-5)
  testthat::expect_equal(res[res$grouping_variable_level == "Pen", "mean_UL"], 10.4689, tolerance = 1e-5)

  testthat::expect_equal(res[res$grouping_variable_level == "Laptop", "mean"], 14.5194, tolerance = 1e-5)
  testthat::expect_equal(res[res$grouping_variable_level == "Laptop", "mean_LL"], 11.847, tolerance = 1e-5)
  testthat::expect_equal(res[res$grouping_variable_level == "Laptop", "mean_UL"], 17.1917, tolerance = 1e-5)


})

