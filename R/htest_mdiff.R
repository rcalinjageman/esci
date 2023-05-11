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
