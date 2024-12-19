test_that("Compare meta_mdiff_two to ESCI_Original_two_groups", {

  # Comparisons are not perfect to ESCI due to difference in SE calculations

  esci_test <- data.frame(
    study_name = c("McCabe 1", "McCabe 2", paste("Michael", seq(1:10), sep = " ")),
    nbM = c(2.89, 2.69, 2.90, 2.62, 2.96, 2.93, 2.86, 2.50, 2.41, 2.54, 2.73, 2.66),
    nbS = c(0.79, 0.55, 0.58, 0.54, 0.36, 0.60, 0.59, 0.84, 0.78, 0.66, 0.67, 0.65),
    nbN = c(28, 26, 98, 42, 24, 184, 274, 58, 34, 99, 98, 94),
    bM = c(3.12, 3.00, 2.86, 2.85, 3.07, 2.89, 2.91, 2.60, 2.74, 2.72, 2.68, 2.64),
    bS = c(0.65, 0.54, 0.61, 0.57, 0.55, 0.60, 0.52, 0.83, 0.51, 0.68, 0.69, 0.71),
    bN = c(26, 28, 99, 33, 21, 184, 255, 55, 34, 95, 93, 97),
    mod = as.factor(
      c("Simple", "Critique", "Simple","Simple","Simple","Simple","Simple","Critique","Critique","Critique", "Critique","Critique")
    ),
    ds = c(0.31217944,  0.56073138, -0.06693802,  0.41136192,  0.23581389, -0.06652995,  0.08958082,  0.11892778,  0.49506069,  0.26765910, -0.07326, -0.02925)
  )

  esci_test <- esci_test[1:10, ]


  # Random effects
  estimate <- esci::meta_mdiff_two(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = TRUE
  )

  testthat::expect_s3_class(estimate, "esci_estimate")
  testthat::expect_snapshot(estimate)


  suppressWarnings(myplot <- esci::plot_meta(estimate))
  testthat::expect_s3_class(myplot, "ggplot")

  # Fixed effects
  estimate_fe <- esci::meta_mdiff_two(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    assume_equal_variance = TRUE,
    random_effects = FALSE
  )

  testthat::expect_s3_class(estimate_fe, "esci_estimate")
  testthat::expect_snapshot(estimate_fe)


  suppressWarnings(myplot <- esci::plot_meta(estimate_fe))
  testthat::expect_s3_class(myplot, "ggplot")


  # Random effects with moderator
  estimate_mod <- esci::meta_mdiff_two(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    contrast = c(1, -1),
    assume_equal_variance = TRUE,
    moderator = mod,
    random_effects = TRUE
  )

  testthat::expect_s3_class(estimate_mod, "esci_estimate")
  testthat::expect_snapshot(estimate_mod)

  suppressWarnings(myplot <- esci::plot_meta(estimate_mod))
  testthat::expect_s3_class(myplot, "ggplot")


  # Fixed effects with moderator
  estimate_mod_fe <- esci::meta_mdiff_two(
    data = esci_test,
    comparison_means = bM,
    comparison_sds = bS,
    comparison_ns = bN,
    reference_means = nbM,
    reference_sds = nbS,
    reference_ns = nbN,
    labels = study_name,
    effect_label = "Brain Photo Rating - No Brain Photo Rating",
    contrast = c(1, -1),
    assume_equal_variance = TRUE,
    moderator = mod,
    random_effects = FALSE
  )

  testthat::expect_s3_class(estimate_mod_fe, "esci_estimate")
  testthat::expect_snapshot(estimate_mod_fe)
  suppressWarnings(myplot <- esci::plot_meta(estimate_mod_fe))
  testthat::expect_s3_class(myplot, "ggplot")

})

