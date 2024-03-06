test_diff_base <- function(
    estimate,
    effect_size = c("mean", "median", "P", "r"),
    rope = c(0, 0),
    rope_units = c("raw", "sd"),
    output_html = FALSE
)

{


  # Input Checks ---------------------
  # This function expects:
  #   estimate should be of class estimate
  #   effect_size = "mean" | "median" | "P" | "r"
  #   rope_lower <= 0
  #   rope_upper >= 0
  #   If rope_lower and rope_upper = 0, only traditional nil test returned
  #   0 < alpha < 1
  #   rope_units = "sd" | "raw"
  esci_assert_type(estimate, "is.estimate")
  effect_size <- match.arg(effect_size)
  if (length(rope) < 1) rope[[2]] <- rope[[1]]
  rope_lower <- rope[[1]]
  rope_upper <- rope[[2]]
  # esci_assert_range(
  #   var = rope_lower,
  #   upper = 0,
  #   upper_inclusive = TRUE
  # )
  # esci_assert_range(
  #   var = rope_upper,
  #   lower = 0,
  #   lower_inclusive = TRUE
  # )

  rope_units <- match.arg(rope_units)

  if (effect_size == "median" & is.null(estimate$es_median_difference)) {
    stop("Effect size is median, but estimate object passed doesn't have a median difference table.")
  }

  if (effect_size == "median" & is.null(estimate$es_mean_difference)) {
    stop("Effect size is mean, but estimate object passed doesn't have a mean difference table.")
  }

  if (rope_units == "sd" & is.null(estimate$es_smd)) {
    stop("Units are sd, but estimate object passed doesn't have a standardized-mean difference table.")

  }

  # Prep ------------------------------------------
  etable <- switch(
    effect_size,
    "mean" = "es_mean_difference",
    "median" = "es_median_difference",
    "P" = "es_proportion_difference",
    "r" = "es_r_difference"
  )



  reference_value <- 0
  effect_is_difference <- TRUE
  if (effect_size == "r" & is.null(estimate$es_r_difference)) {
    etable <- "es_r"
    effect_is_difference <- FALSE
    estimate$es_r$type <- "Difference"
  } else if (is.na(estimate[[etable]][2, "LL"])) {
    reference_value <- estimate[[etable]][2, "effect_size"]
    effect_is_difference <- FALSE
    null_value <- reference_value
  }


  if (output_html) {
    null_symbol <- "<i>H</i><sub>0</sub>"
    p_symbol <- "<i>p</i>"
    statistic <- switch(
      effect_size,
      "mean" = "<i>M</i>",
      "median" = "<i>Mdn</i>",
      "P" = "<i>P</i>",
      "r" = "<i>r</i>"
    )
    parameter <- switch(
      effect_size,
      "mean" = "&mu;",
      "median" = "&eta;",
      "P" = "&Pi;",
      "r" = "&rho;"
    )
    difference_marker <- "<sub>diff</sub>"
  } else {
    null_symbol <- "H_0"
    p_symbol <- "p"
    statistic <- switch(
      effect_size,
      "mean" = "M",
      "median" = "Mdn",
      "P" = "P",
      "r" = "r"
    )
    parameter <- switch(
      effect_size,
      "mean" = "\U003BC",
      "median" = "\U003B7",
      "P" = "\U03A0",
      "r" = "\U0370"
    )
    difference_marker <- "_diff"
  }

  if (effect_is_difference) {
    statistic <- paste(statistic, difference_marker, sep = "")
    parameter <- paste(parameter, difference_marker, sep = "")
  }


  alpha <- 1 - estimate$properties$conf_level
  confidence <- estimate$properties$conf_level*100
  confidence_2alpha <- (1 - 2*alpha)*100

  interval_null <- if (rope_lower != rope_upper) TRUE else FALSE

  # Reduce down to differences
  effect_sizes <- estimate[[etable]][
    estimate[[etable]]$type == "Difference",
  ]


  res <- list()
  res$properties <- list(
    effect_size_name = effect_size,
    alpha = alpha,
    interval_null = interval_null,
    rope = rope,
    rope_units = rope_units
  )

  # Loop through all effect sizes
  for (my_row in 1:nrow(effect_sizes)) {

    # Get single effect size and contrast to work with
    es <- as.list(effect_sizes[my_row, ])

    #### Set rope limits
    if (rope_units == "sd") {
      sd <- estimate$es_smd[[my_row, "denominator"]]
      this_rope_upper <- rope_upper * sd
      this_rope_lower <- rope_lower * sd
    } else {
      this_rope_upper <- rope_upper
      this_rope_lower <- rope_lower
    }


    # Nil hypothesis test ------------------------------
    if (effect_size == "mean" | effect_size == "P") {
      df <- es$df
      if (is.null(df)) df <- NA
      if (is.null(es$effect_size_adjusted)) {
        t <- es$effect_size / es$SE
      } else {
        t <- es$effect_size_adjusted / es$SE
      }
      if (is.na(df))
        p <- 2*pnorm(-abs(t))
      else
        p <- 2*pt(-abs(t), df=df)
      significant <- p < alpha
    } else {
      if (etable == "es_r") {
        df <- es$df
        myr <- es$effect_size
        null_value <- this_rope_lower + ((this_rope_upper - this_rope_lower)/2)
        rz <- esci_trans_r_to_z(myr)
        rnull <- esci_trans_r_to_z(null_value)
        rsem <- 1 / sqrt(df - 1)

        t <- abs(rz-rnull)/rsem

        # t <- myr / sqrt(1-myr^2) * sqrt(df)
        #p <- 2*pt(-abs(t), df=df)
        p <- 2 * pnorm(q=t, lower.tail=FALSE)
        significant <- p < alpha
      } else {

        if (effect_size == "median") {
          df <- NA
          t <- NA
          p <- NA
          significant <- (0 < es$LL | 0 > es$UL)
        } else {
          df <- NA
          t <- es$z
          p <- es$p
          significant <- p < alpha

        }

      }

    }

    if (effect_size == "r") {
      es$outcome_variable_name <- paste(es$x_variable_name, " and ", es$y_variable_name, sep = "")
    }

    # null_words <- glue::glue("{parameter} is exactly 0")
    null_words <- glue::glue("{if (etable == 'es_r') format(null_value, nsmall=2) else format(reference_value, nsmall=2)}")

    rope_words <- glue::glue("({format(reference_value, nsmall=2)}, {format(reference_value, nsmall=2)})")
    if (etable == 'es_r') {
      rope_words <- glue::glue("({format(null_value, nsmall=2)}, {format(null_value, nsmall=2)})")

    }

    CI <- glue::glue("{confidence}% CI [{format(es$LL+reference_value, nsmall = 2)}, {format(es$UL+reference_value, nsmall = 2)}]")

    CI_compare <- if (significant)
      glue::glue("The {confidence}% CI does not contain {null_symbol}")
    else
      glue::glue("The {confidence}% CI contains {null_symbol}")

    # CI_compare <- if (significant)
    #   glue::glue("No")
    # else
    #   glue::glue("Yes")

    null_decision <- if (significant)
      glue::glue("Reject {null_symbol}")
    else
      glue::glue("Fail to reject {null_symbol}")

    p_result <- if (significant)
      glue::glue("{p_symbol} < {alpha}")
    else
      glue::glue("{p_symbol} \U002265 {alpha}")

    conclusion <- if (significant)
      glue::glue("At \U03B1 = {alpha}, {if (etable == 'es_r') format(null_value, nsmall=2) else format(reference_value, nsmall=2)} is not a plausible value of {parameter}")
    else
      glue::glue("At \U03B1 = {alpha}, {if (etable == 'es_r') format(null_value, nsmall=2) else format(reference_value, nsmall=2)} remains a plausible value of {parameter}")

    case_label_use <- "-99101"
    if (!is.null(es$case_label)) case_label_use <- es$case_label

    nil_result <- list(
      test_type = "Nil Hypothesis Test",
      outcome_variable_name = es$outcome_variable_name,
      case_label = case_label_use,
      effect = es$effect,
      null_words = null_words,
      confidence = confidence,
      LL = es$LL,
      UL = es$UL,
      CI = CI,
      CI_compare = CI_compare,
      t = t,
      df = df,
      p = p,
      p_result = p_result,
      null_decision = null_decision,
      conclusion = conclusion,
      significant = significant
    )

    if (is.null(nil_result$outcome_variable_name)) {
      nil_result$outcome_variable_name <- paste(
        es$comparison_measure_name,
        es$reference_measure_name,
        sep = " - "
      )
    }

    if (is.null(es$case_label)) nil_result$case_label <- NULL

    res$point_null <- rbind(
      res$point_null,
      as.data.frame(nil_result)
    )

    if (!(this_rope_lower == 0 & this_rope_upper == 0)) {
      eq_significant <- (es$ta_LL > this_rope_lower & es$ta_UL < this_rope_upper)
      me_significant <- (es$LL >= this_rope_upper | es$UL <= this_rope_lower)

      significant <- (eq_significant | me_significant)

      null_words <- glue::glue("({format(this_rope_lower+reference_value, nsmall = 2)}, {format(this_rope_upper+reference_value, nsmall = 2)})")


      if(output_html) {
        CI <- glue::glue("
        {confidence}% CI [{format(es$LL+reference_value, nsmall = 2)}, {format(es$UL+reference_value, nsmall = 2)}]
        </br>
        {confidence_2alpha}% CI [{format(es$ta_LL+reference_value, nsmall = 2)}, {format(es$ta_UL+reference_value, nsmall = 2)}]
        ")

      } else {
        CI <- glue::glue("
        {confidence}% CI [{format(es$LL+reference_value, nsmall = 2)}, {format(es$UL+reference_value, nsmall = 2)}]
        {confidence_2alpha}% CI [{format(es$ta_LL+reference_value, nsmall = 2)}, {format(es$ta_UL+reference_value, nsmall = 2)}]
        ")

      }


      CI_compare <- if (me_significant)
        glue::glue("{confidence}% CI fully outside {null_symbol}")
      else
        glue::glue("{confidence}% CI has values inside and outside {null_symbol}")

      if (eq_significant) {
        CI_compare <- glue::glue("{confidence_2alpha}% CI fully inside {null_symbol}")
      }

      p_result <- if (me_significant | eq_significant)
        glue::glue("{p_symbol} < {alpha}")
      else
        glue::glue("{p_symbol} \U002265 {alpha}")


      # conclusion <- if (me_significant)
      #   glue::glue("At \U03B1 = {alpha}, conclude {parameter} is substantive")
      # else
      #   glue::glue("At \U03B1 = {alpha}, not clear if {parameter} is substantive or negligible")
      #
      # if (eq_significant) {
      #   conclusion <- glue::glue("At \U03B1 = {alpha}, conclude {parameter} is negligible")
      # }

      preport <- parameter
      if (!effect_is_difference) {
        if (null_value != 0) {
            preport <- paste("(", parameter, " \U2212 ", null_value, ")", sep = "")
        }
      }

      conclusion <- if (me_significant)
        glue::glue("At \U03B1 = {alpha}, conclude {preport} is substantive")
      else
        glue::glue("At \U03B1 = {alpha}, not clear if {preport} is substantive or negligible")

      if (eq_significant) {
        conclusion <- glue::glue("At \U03B1 = {alpha}, conclude {preport} is negligible")
      }


      interval_result <- list(
        test_type = "Practical significance test",
        outcome_variable_name = es$outcome_variable_name,
        case_label = case_label_use,
        effect = es$effect,
        rope = null_words,
        confidence = confidence,
        CI = CI,
        rope_compare = CI_compare,
        p_result = p_result,
        conclusion = conclusion,
        significant = significant
      )


      if (is.null(interval_result$outcome_variable_name)) {
        interval_result$outcome_variable_name <- paste(
          es$comparison_measure_name,
          es$reference_measure_name,
          sep = " - "
        )
      }

      if (is.null(es$case_label)) interval_result$case_label <- NULL

      res$interval_null <- rbind(
        res$interval_null,
        as.data.frame(interval_result)
      )
    } # End interval tests

  } # Continue looping through effects


  return(res)

}



#' Test a hypothesis about a difference in a continuous outcome variable.
#'
#' @description
#' `test_mdiff` is suitable for conducting a testing a hypothesis about the
#' magnitude of difference between two conditions for a continuous outcome
#' variable.  It can test hypotheses about differences in means or medians for
#' both independent and paired designs.
#'
#'
#' @details
#' This function can be passed an esci_estimate object generated by
#' [esci::estimate_mdiff_one()], [esci::estimate_mdiff_two()],
#' [esci::estimate_mdiff_paired()], or [esci::estimate_mdiff_ind_contrast()].
#'
#' It can test hypotheses about a specific value for the difference (a
#' point null) or about a range of values (an interval null)
#'
#'
#' @param estimate - An esci_estimate object generated by an
#' estimate_mdiff_ function
#'
#' @param effect_size - One of 'mean' or 'median'.  The effect size selected
#' must be available in the esci_estimate object; medians are only available
#' when the estimate was generated from raw data.
#' @param rope - A two-element vector defining the Region of Practical
#' Equivalence (ROPE). Specify c(0, 0) to test a point null of exactly 0.
#' Specify any two ascending values to test an interval null (e.g. c(-1, 1) to
#' test the hypothesis tha the difference is between -1 and 1).
#' @param rope_units - One of 'raw' (default) or 'sd', specifies the units of
#' the ROPE. If 'sd' is specified, the rope is defined in standard deviation
#' units (e.g. c(-1, 1) is taken as between -1 and 1 *standard deviations*
#' from 0).  When sd is used, the ROPE is converted to raw scores and then
#' the test is conducted on raw scores.
#' @param output_html - TRUE to return results in HTML; FALSE (default) to return
#' standard output
#'
#' @return Returns a list with 1-2 data frames
#' - **point_null**  - always returned
#'     - *test_type* - 'Nil hypothesis test', meaning a test against H0 = 0
#'     - *outcome_variable_name* - Name of the outcome variable
#'     - *effect* - Label for the effect being tested
#'     - *null_words* - Express the null in words
#'     - *confidence* - Confidence level, integer (95 for 95%, etc.)
#'     - *LL* - Lower boundary of the confidence% CI for the effect
#'     - *UL* - Upper boundary of the confidence% CI for the effect
#'     - *CI* - Character representation of the CI for the effect
#'     - *CI_compare* - Text description of relation between CI and null
#'     - *t* - If applicable, t value for hypothesis test
#'     - *df* - If applicable, degrees of freedom for hypothesis test
#'     - *p* - If applicable, p value for hypothesis test
#'     - *p_result* - Text representation of p value obtained
#'     - *null_decision* - Text represention of the decision for the null
#'     - *conclusion* - Text representation of conclusion to draw
#'     - *significant* - TRUE/FALSE if significant at alpha = 1-CI
#' - **interval_null** - returned only if an interval null is specified
#'     - *test_type* - 'Practical significance test', meaning a test against an
#'        interval null
#'     - *outcome_variable_name* -
#'     - *effect* - Name of the outcome variable
#'     - *rope* - Test representaiton of null interval
#'     - *confidence* - Confidence level, integer (95 for 95%, etc.)
#'     - *CI* - Character representation of the CI for the effect
#'     - *rope_compare* - Text description of relation between CI and null interval
#'     - *p_result* - Text representation of p value obtained
#'     - *conclusion* - Text representation of conclusion to draw
#'     - *significant* - TRUE/FALSE if significant at alpha = 1-CI
#'
#' @examples
#' # example code
#' data("data_penlaptop1")
#'
#' estimate <- esci::estimate_mdiff_two(
#'   data = data_penlaptop1,
#'   outcome_variable = transcription,
#'   grouping_variable = condition,
#'   switch_comparison_order = TRUE,
#'   assume_equal_variance = TRUE
#' )
#'
#' # Test mean difference against point null of 0
#' esci::test_mdiff(
#'   estimate,
#'   effect_size = "mean"
#' )
#'
#' # Test median difference against point null of 0
#' #  Note that t, df, p return NA because test is completed
#' #  by interval.
#' esci::test_mdiff(
#'   estimate,
#'   effect_size = "median"
#' )
#'
#' # Test mean difference against interval null of -10 to 10
#' esci::test_mdiff(
#'   estimate,
#'   effect_size = "mean",
#'   rope = c(-10, 10)
#' )
#'
#' # Test mean difference against interval null of d (-0.20, 0.20) d = 0.2 is often
#' # thought of as a small effect, so this test examines if the effect is
#' # negligible (clearly between negligble and small), substantive (clearly more
#' # than small), or unclear. The d boundaries provided are converted to raw scores
#' # and then the CI of the observed effect is compared to the raw-score boundaries
#' esci::test_mdiff(
#'   estimate,
#'   effect_size = "mean",
#'   rope = c(-0.2, 0.2),
#'   rope_units = "sd"
#' )
#'
#'
#' @export
test_mdiff <- function(
    estimate,
    effect_size = c("mean", "median"),
    rope = c(0, 0),
    rope_units = c("raw", "sd"),
    output_html = FALSE
)

{


  # Input Checks ---------------------
  # This function expects:
  #   estimate should be of class estimate
  #   effect_size = "mean" | "median"
  #   rope_lower <= 0
  #   rope_upper >= 0
  #   If rope_lower and rope_upper = 0, only traditional nil test returned
  #   0 < alpha < 1
  #   rope_units = "sd" | "raw"
  esci_assert_type(estimate, "is.estimate")
  effect_size <- match.arg(effect_size)
  if (length(rope) < 1) rope[[2]] <- rope[[1]]
  rope_lower <- rope[[1]]
  rope_upper <- rope[[2]]
  esci_assert_range(
    var = rope_lower,
    upper = 0,
    upper_inclusive = TRUE
  )
  esci_assert_range(
    var = rope_upper,
    lower = 0,
    lower_inclusive = TRUE
  )

  rope_units <- match.arg(rope_units)

  if (effect_size == "median" & is.null(estimate$es_median_difference)) {
    stop("Effect size is median, but estimate object passed doesn't have a median difference table.")
  }

  if (effect_size == "median" & is.null(estimate$es_mean_difference)) {
    stop("Effect size is mean, but estimate object passed doesn't have a mean difference table.")
  }

  if (rope_units == "sd" & is.null(estimate$es_smd)) {
    stop("Units are sd, but estimate object passed doesn't have a standardized-mean difference table.")

  }


  return(
    test_diff_base(
      estimate = estimate,
      effect_size = effect_size,
      rope = rope,
      rope_units = rope_units,
      output_html = output_html
    )
  )

}



#' Test a hypothesis about a difference in proportion
#'
#'
#' @description
#' `test_pdiff` is suitable for testing a hypothesis about a
#' difference in proportions between two conditions with a categorical outcome
#' variable.  It can test hypotheses for both independent and paired designs.
#'
#'
#' @details
#' This function can be passed an esci_estimate object generated by
#' [esci::estimate_pdiff_one()], [esci::estimate_pdiff_two()],
#' [esci::estimate_pdiff_paired()], or [esci::estimate_pdiff_ind_contrast()].
#'
#' It can test hypotheses about a specific value for the difference (a
#' point null) or about a range of values (an interval null)
#'
#'
#' @param estimate - An esci_estimate object generated by an estimate_pdiff_ function
#'
#' @param rope - A two-element vector defining the Region of Practical
#'   Equivalence (ROPE). Specify c(0, 0) to test a point null of exactly 0.
#'   Specify any two ascending values between -1 and 1 to test an interval null
#'   (e.g. c(-.25, .25) to test the hypothesis that the difference in proportion
#'   is between -.25 and .25).
#' @param output_html - TRUE to return results in HTML; FALSE (default) to
#'   return standard output
#'
#'
#' @inherit test_mdiff return
#'
#'
#' @examples
#' estimate <- estimate_pdiff_two(
#'   comparison_cases = 10,
#'   comparison_n = 20,
#'   reference_cases = 78,
#'   reference_n = 252,
#'   grouping_variable_levels = c("Original", "Replication"),
#'   conf_level = 0.95
#' )
#'
#' # Test against null of exactly
#' test_pdiff(estimate)
#'
#' # Test against null of (-0.1, 0.1)
#' test_pdiff(estimate, rope = c(-0.1, 0.1))
#'
#' @export
test_pdiff <- function(
    estimate,
    rope = c(0, 0),
    output_html = FALSE
)
{


  # Input Checks ---------------------
  # This function expects:
  #   estimate should be of class estimate
  #   rope_lower <= 0
  #   rope_upper >= 0
  #   If rope_lower and rope_upper = 0, only traditional nil test returned
  #   0 < alpha < 1
  esci_assert_type(estimate, "is.estimate")
  if (length(rope) == 1) rope[[2]] <- rope[[1]]
  rope_lower <- rope[[1]]
  rope_upper <- rope[[2]]
  esci_assert_range(
    var = rope_lower,
    upper = 0,
    upper_inclusive = TRUE
  )
  esci_assert_range(
    var = rope_upper,
    lower = 0,
    lower_inclusive = TRUE
  )


  return(
    test_diff_base(
      estimate = estimate,
      effect_size = "P",
      rope = rope,
      rope_units = "raw",
      output_html = output_html
    )
  )

}


#' Test a hypothesis about a difference in correlation strength
#'
#'
#' @description
#' `test_rdiff` is suitable for testing a hypothesis about a
#' difference in correlation (*r*) between two conditions.
#' At the moment, it can only test hypotheses for independent-group designs.
#'
#'
#' @details
#' This function can be passed an esci_estimate object generated by
#' [esci::estimate_rdiff_two()].
#'
#' It can test hypotheses about a specific value for the difference (a
#' point null) or about a range of values (an interval null)
#'
#' @param estimate - An esci_estimate object generated by an estimate_rdiff_
#'   function
#'
#' @param rope - A two-element vector defining the Region of Practical
#'   Equivalence (ROPE). Specify c(0, 0) to test a point null of exactly 0.
#'   Specify any two ascending values between -1 and 1 to test an interval null
#'   (e.g. c(-.25, .25) to test the hypothesis that the difference in
#'   correlation is between -.25 and .25).
#' @param output_html - TRUE to return results in HTML; FALSE (default) to
#'   return standard output
#'
#'
#' @inherit test_mdiff return
#'
#'
#' @examples
#' # example code
#' estimate <- esci::estimate_rdiff_two(
#'   comparison_r = .53,
#'   comparison_n = 45,
#'   reference_r = .41,
#'   reference_n = 59,
#'   grouping_variable_levels = c("Females", "Males"),
#'   x_variable_name = "Satisfaction with life",
#'   y_variable_name = "Body satisfaction",
#'   grouping_variable_name = "Gender",
#'   conf_level = .95
#' )
#' test_rdiff(estimate)
#'
#'
#' @export
test_rdiff <- function(
    estimate,
    rope = c(0, 0),
    output_html = FALSE
)

{


  # Input Checks ---------------------
  # This function expects:
  #   estimate should be of class estimate
  #   rope_lower <= 0
  #   rope_upper >= 0
  #   If rope_lower and rope_upper = 0, only traditional nil test returned
  #   0 < alpha < 1
  esci_assert_type(estimate, "is.estimate")
  if (length(rope) == 1) rope[[2]] <- rope[[1]]
  rope_lower <- rope[[1]]
  rope_upper <- rope[[2]]
  esci_assert_range(
    var = rope_lower,
    upper = 0,
    upper_inclusive = TRUE
  )
  esci_assert_range(
    var = rope_upper,
    lower = 0,
    lower_inclusive = TRUE
  )


  return(
    test_diff_base(
      estimate = estimate,
      effect_size = "r",
      rope = rope,
      rope_units = "raw",
      output_html = output_html
    )
  )

}


#' Test a hypothesis about the strength of a Pearson's *r* correlation
#'
#'
#' @description
#' `test_correlation` is suitable for testing a hypothesis about a
#' the strength of correlation between two continuous variables (designs
#' in which Pearson's *r* is a suitable measure of correlation).
#'
#'
#' @details
#' This function can be passed an esci_estimate object generated by
#' [esci::estimate_r()].
#'
#' It can test hypotheses about a specific value for the difference (a
#' point null) or about a range of values (an interval null)
#'
#'
#' @param estimate - An esci_estimate object generated by the estimate_r
#'   function
#'
#' @param rope - A two-element vector defining the Region of Practical
#'   Equivalence (ROPE). Specify c(0, 0) to test a point null of exactly 0.
#'   Specify any two ascending values between -1 and 1 to test an interval null
#'   (e.g. c(.25, .45) to test the hypothesis that Pearson's *r* in the
#'   population (rho) is between .25 and .45).
#' @param output_html - TRUE to return results in HTML; FALSE (default) to
#'   return standard output
#'
#'
#' @inherit test_mdiff return
#'
#'
#' @examples
#' # example code
#' estimate <- esci::estimate_r(r = 0.536, n = 50)
#'
#' # Test against a point null of exactly 0
#' test_correlation(estimate)
#'
#' # Test against an interval null (-0.1, 0.1)
#' test_correlation(estimate, rope = c(-0.1, 0.1))
#'
#'
#' @export
test_correlation <- function(
    estimate,
    rope = c(0, 0),
    output_html = FALSE
)

{


  # Input Checks ---------------------
  # This function expects:
  #   estimate should be of class estimate
  #   rope_lower <= 0
  #   rope_upper >= 0
  #   If rope_lower and rope_upper = 0, only traditional nil test returned
  #   0 < alpha < 1
  esci_assert_type(estimate, "is.estimate")
  if (length(rope) == 1) rope[[2]] <- rope[[1]]
  rope_lower <- rope[[1]]
  rope_upper <- rope[[2]]
  esci_assert_range(
    var = rope_lower,
    upper = 1,
    upper_inclusive = TRUE
  )
  esci_assert_range(
    var = rope_upper,
    lower = rope_lower,
    lower_inclusive = TRUE
  )


  return(
    test_diff_base(
      estimate = estimate,
      effect_size = "r",
      rope = rope,
      rope_units = "raw",
      output_html = output_html
    )
  )

}
