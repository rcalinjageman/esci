test_that("overview_nominal to esci examples", {

  # Esci one proportion - 8/22
  res <-overview_nominal(cases = c(8, 22-8))
  # Should yield:
  #outcome_variable_name outcome_variable_level count  n         P      P_LL      P_UL       P_SE
  #1   My Outcome Variable                 Level1     8 22 0.3636364 0.1976126 0.5716182 0.09541133
  testthat::expect_equal(res[res$outcome_variable_level == "Level1", "P"], 0.3636364, tolerance = 1e-5)
  testthat::expect_equal(res[res$outcome_variable_level == "Level1", "P_LL"], 0.1976126, tolerance = 1e-5)
  testthat::expect_equal(res[res$outcome_variable_level == "Level1", "P_UL"], 0.5716182, tolerance = 1e-5)


  # Same but from dataframe and include some NAs that are not counted
  dep_status <- as.factor(
    c(
      rep("Depressed", 8),
      rep("NotDepressed", 22-8),
      NA,
      NA,
      NA
    )
  )

  res <- overview_nominal(outcome_variable = dep_status, count_NA = FALSE)

  testthat::expect_equal(res[res$outcome_variable_level == "Depressed", "P"], 0.3636364, tolerance = 1e-5)
  testthat::expect_equal(res[res$outcome_variable_level == "Depressed", "P_LL"], 0.1976126, tolerance = 1e-5)
  testthat::expect_equal(res[res$outcome_variable_level == "Depressed", "P_UL"], 0.5716182, tolerance = 1e-5)



})

