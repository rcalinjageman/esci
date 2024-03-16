#' Estimates for a repeated-measures study with two measures of a continuous
#' variable
#'
#'
#' @description Returns object
#' `estimate_mdiff_paired` is suitable for a simple paired design
#' with a continuous outcome variable.  It provides estimates and CIs for the
#' population mean difference between the repeated measures, the standardized
#' mean difference (SMD; Cohen's d) between the repeated measures, and the
#' median difference between the repeated measures (raw data only).  You can
#' pass raw data or summary data.
#'
#'
#' @details
#' Reach for this function in place of a paired-samples *t*-test.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can test hypotheses with
#' [esci::test_mdiff()].
#'
#' The estimated mean differences are from [statpsych::ci.mean.ps()].
#'
#' The estimated SMDs are from [esci::CI_smd_ind_contrast()].
#'
#' The estimated median differences are from [statpsych::ci.median.ps()].
#'
#'
#' @param data For raw data - a data frame or tibble
#' @param comparison_measure For raw data - The column name of comparison
#'   measure of the outcome variable, or a vector of numeric data
#' @param reference_measure For raw data - The column name of the reference
#'   measure of the outcome variable, or a vector of numeric data
#' @param comparison_mean For summary data, a numeric
#' @param comparison_sd For summary data, numeric > 0
#' @param reference_mean For summary data, a numeric
#' @param reference_sd For summary data, numeric > 0
#' @param n For summary data, a numeric integer > 0
#' @param correlation For summary data, correlation between measures, a numeric
#'   that is > -1 and < 1
#' @param comparison_measure_name For summary data - An optional character
#'   label for the comparison measure.  Defaults to 'Comparison measure'
#' @param reference_measure_name For summary data - An optional character
#'   label for the reference measure.  Defaults to 'Reference measure'
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param save_raw_data For raw data; defaults to TRUE; set to FALSE to save
#'   memory by not returning raw data in estimate object
#'
#'
#' @return Returns object of class esci_estimate
#' - **overview**
#'     - *outcome_variable_name* -
#'     - *mean* -
#'     - *mean_LL* -
#'     - *mean_UL* -
#'     - *median* -
#'     - *median_LL* -
#'     - *median_UL* -
#'     - *sd* -
#'     - *min* -
#'     - *max* -
#'     - *q1* -
#'     - *q3* -
#'     - *n* -
#'     - *missing* -
#'     - *df* -
#'     - *mean_SE* -
#'     - *median_SE* -
#' - **es_mean_difference**
#'     - *type* -
#'     - *comparison_measure_name* -
#'     - *reference_measure_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_smd**
#'     - *comparison_measure_name* -
#'     - *reference_measure_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *numerator* -
#'     - *denominator* -
#'     - *SE* -
#'     - *d_biased* -
#'     - *df* -
#' - **es_r**
#'     - *x_variable_name* -
#'     - *y_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *n* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_median_difference**
#'     - *type* -
#'     - *comparison_measure_name* -
#'     - *reference_measure_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_mean_ratio**
#'     - *comparison_measure_name* -
#'     - *reference_measure_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *comparison_mean* -
#'     - *reference_mean* -
#' - **es_median_ratio**
#'     - *comparison_measure_name* -
#'     - *reference_measure_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *comparison_median* -
#'     - *reference_median* -
#' - **raw_data**
#'     - *comparison_measure* -
#'     - *reference_measure* -
#'
#'
#' @examples
#' # From raw data
#' data("data_thomason_1")
#'
#' estimate_from_raw <- esci::estimate_mdiff_paired(
#'   data = esci::data_thomason_1,
#'   comparison_measure = Posttest,
#'   reference_measure = Pretest
#' )
#'
#' # To visualize the estimated median difference (raw data only)
#' myplot_from_raw <- esci::plot_mdiff(estimate_from_raw, effect_size = "median")
#'
#' # To conduct a hypothesis test
#' res_htest_from_raw <- esci::test_mdiff(
#'   estimate_from_raw,
#'   effect_size = "median",
#'   rope = c(-2, 2)
#' )
#'
#'
# # From summary data, ESCI in excel Summary Two example
#' sd1 <- 4.28
#' sd2 <- 3.4
#' sdiff <- 2.13
#'
#' cor <- (sd1^2 + sd2^2 - sdiff^2) / (2*sd1*sd2)
#'
#' estimate_from_summary <- esci::estimate_mdiff_paired(
#'   comparison_mean = 14.25,
#'   comparison_sd = 4.28,
#'   reference_mean = 12.88,
#'   reference_sd = 3.4,
#'   n = 16,
#'   correlation = 0.87072223749,
#'   comparison_measure_name = "After",
#'   reference_measure_name = "Before"
#' )
#'
#' # To visualize the estimated mean difference
#' myplot_from_summary <- esci::plot_mdiff(
#'   estimate_from_summary,
#'   effect_size = "mean"
#' )
#'
#' # To conduct a hypothesis test
#' res_htest_from_summary <- esci::test_mdiff(
#'   estimate_from_summary,
#'   effect_size = "mean",
#'   rope = c(-2, 2)
#' )
#'
#'
#' @export
estimate_mdiff_paired <- function(
  data = NULL,
  comparison_measure = NULL,
  reference_measure = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  reference_mean = NULL,
  reference_sd = NULL,
  n = NULL,
  correlation = NULL,
  comparison_measure_name = "Comparison measure",
  reference_measure_name = "Reference measure",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"

  # Check to see if summary data has been passed
  if (
    !is.null(comparison_mean) |
    !is.null(comparison_sd) |
    !is.null(reference_mean) |
    !is.null(reference_sd) |
    !is.null(n) |
    !is.null(correlation)
    ) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(comparison_measure)) stop(
      "You have passed summary statistics,
      so don't pass the 'comparison_measure' parameter used for raw data.")
    if(!is.null(reference_measure)) stop(
      "You have passed summary statistics,
      so don't pass the 'reference_measure' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first be sure summary data is not passed
    if(!is.null(comparison_mean))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_mean' parameter used for summary data.")
    if(!is.null(comparison_sd))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_sd' parameter used for summary data.")
    if(!is.null(reference_mean))  stop(
      "You have passed raw data,
      so don't pass the 'reference_mean' parameter used for summary data.")
    if(!is.null(reference_sd))  stop(
      "You have passed raw data,
      so don't pass the 'reference_sd' parameter used for summary data.")
    if(!is.null(n))  stop(
      "You have passed raw data,
      so don't pass the 'n' parameter used for summary data.")
    if(!is.null(correlation))  stop(
      "You have passed raw data,
      so don't pass the 'correlation' parameter used for summary data.")

    # Check reference_measure -- if it is an unquoted column name
    #  turn it into a string and store back to grouping_variable
    is_column_name <- try(reference_measure, silent = TRUE)
    if(is(is_column_name, "try-error")) {
      reference_measure_enquo <- rlang::enquo(reference_measure)
      reference_measure_enquo_name <- try(
        eval(rlang::as_name(reference_measure_enquo)), silent = TRUE
      )
      if (!is(reference_measure_enquo_name, "try-error")) {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        reference_measure <- reference_measure_enquo_name
      }
    }

    # Now we have to figure out what type of raw data:
    #   could be tidy column names, string column names, or vectors
    # We check to see if we have a tidy column name by trying to evaluate it
    is_column_name <- try(comparison_measure, silent = TRUE)
    if(is(is_column_name, "try-error")) {
      # Column names have been passed, check if need to be quoted up

      comparison_measure_enquo <- rlang::enquo(comparison_measure)
      comparison_measure_quoname <- try(
        eval(rlang::as_name(comparison_measure_enquo)), silent = TRUE
      )
      if (!is(comparison_measure_quoname, "try-error")) {
        # This only succeeds if outcome_variable was passed unquoted
        # Reset outcome_variable to be fully quoted
        comparison_measure <- comparison_measure_quoname
      }
      analysis_type <- "data.frame"

    } else if (is(comparison_measure, "numeric")) {
      # At this stage, we know that y was not a tidy column name,
      #  so it should be either a vector of raw data (class = numeric)
      #  or a vector of column names passed as strings
      analysis_type <- "vector"
    } else if (is(comparison_measure, "character")) {
      # Ok, must have been string column names
      if (length(comparison_measure) == 1) {
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"
      }
    }
  }

  if(analysis_type == "data.frame") {
    return(
      estimate_mdiff_paired.data.frame(
        data = data,
        comparison_measure = comparison_measure,
        reference_measure = reference_measure,
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )
    )
  } else if (analysis_type == "jamovi") {
    stop("Not implemented yet - or ever?")

  } else if (analysis_type == "summary") {
    return(
      estimate_mdiff_paired.summary(
        comparison_mean = comparison_mean,
        comparison_sd = comparison_sd,
        reference_mean = reference_mean,
        reference_sd = reference_sd,
        n = n,
        correlation = correlation,
        grouping_variable_levels = c(
          comparison_measure_name,
          reference_measure_name
        ),
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {
    grouping_variable_levels <- c(NULL, NULL)
    if (is.null(comparison_measure_name) | comparison_measure_name == "Comparison measure") {
      grouping_variable_levels[1] <- deparse(substitute(comparison_measure))
    }

    if (is.null(reference_measure_name) | reference_measure_name == "Reference measure") {
      grouping_variable_levels[2] <- deparse(substitute(reference_measure))
    }

    return(
      estimate_mdiff_paired.vector(
        comparison_measure = comparison_measure,
        reference_measure = reference_measure,
        grouping_variable_levels = grouping_variable_levels,
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )
    )
  }


  stop("Something went wrong dispatching this function")

}


estimate_mdiff_paired.summary <- function(
  comparison_mean,
  comparison_sd,
  reference_mean,
  reference_sd,
  n,
  correlation,
  grouping_variable_levels = c("Comparison measure", "Reference measure"),
  conf_level = conf_level
) {

  # Input checks ----------------------------

  # Check means
  esci_assert_type(comparison_mean, "is.numeric")
  esci_assert_type(reference_mean, "is.numeric")
  # Check sds
  esci_assert_type(comparison_sd, "is.numeric")
  esci_assert_type(reference_sd, "is.numeric")
  esci_assert_range(
    comparison_sd,
    lower = 0,
    lower_inclusive = FALSE
  )
  esci_assert_range(
    reference_sd,
    lower = 0,
    lower_inclusive = FALSE
  )
  # Check ns
  esci_assert_type(n, "is.numeric")
  esci_assert_type(n, "is.whole.number")
  esci_assert_range(
    n,
    lower = 2,
    lower_inclusive = TRUE
  )
  # Check correlation
  esci_assert_type(correlation, "is.numeric")
  esci_assert_range(
    correlation,
    lower = -1,
    lower_inclusive = TRUE,
    upper = 1,
    upper_inclusive = TRUE
  )
  # Check labels


  # Initialize -----------------------------
  warnings <- c(NULL)
  contrast <- c(-1, 1)
  names(contrast) <- grouping_variable_levels

  # Analysis --------------------------
  overview <- rbind(
    estimate_magnitude.summary(
      mean = reference_mean,
      sd = reference_sd,
      n = n,
      outcome_variable_name = grouping_variable_levels[2],
      conf_level = conf_level
    )$overview,
    estimate_magnitude.summary(
      mean = comparison_mean,
      sd = comparison_sd,
      n = n,
      outcome_variable_name = grouping_variable_levels[1],
      conf_level = conf_level
    )$overview
  )

  es_mean_difference <- wrapper_ci.mean.ps(
    comparison_mean = comparison_mean,
    comparison_sd = comparison_sd,
    reference_mean = reference_mean,
    reference_sd = reference_sd,
    n = n,
    correlation = correlation,
    grouping_variable_levels = grouping_variable_levels,
    conf_level = conf_level
  )


  smd <- wrapper_ci.stdmean.ps(
    comparison_mean = comparison_mean,
    comparison_sd = comparison_sd,
    reference_mean = reference_mean,
    reference_sd = reference_sd,
    n = n,
    correlation = correlation,
    grouping_variable_levels = grouping_variable_levels,
    conf_level = conf_level
  )

  es_r <- wrapper_ci.cor(
    r = correlation,
    n = n,
    conf_level = conf_level,
    x_variable_name = grouping_variable_levels[2],
    y_variable_name = grouping_variable_levels[1]
  )

  # output prep -----------------------------------------
  estimate <- list(
    overview = overview,
    es_mean_difference = es_mean_difference,
    es_smd = smd$es_smd,
    es_r = es_r,
    es_smd_properties = smd$es_smd_properties
  )

  estimate$properties <- list(
    outcome_variable_name = NULL,
    grouping_variable_name = NULL,
    grouping_variable_levels = grouping_variable_levels,
    contrast = contrast,
    conf_level = conf_level,
    data_type = "summary",
    data_source <- NULL,
    warnings <- warnings
  )

  estimate$es_mean_difference_properties <- list(
    effect_size_name = "M_Diff",
    effect_size_name_html = "<i>M</i><sub>diff</sub>",
    effect_size_category = "difference",
    effect_size_precision = "magnitude",
    conf_level = conf_level,
    error_distribution = "t_dist"
  )

  class(estimate) <- "esci_estimate"


  return(estimate)

}


estimate_mdiff_paired.vector <- function(
  comparison_measure,
  reference_measure,
  grouping_variable_levels,
  conf_level = conf_level,
  save_raw_data = save_raw_data
) {

  # Input checks -------------------------------------
  # Check comparison_measure
  esci_assert_type(comparison_measure, "is.numeric")
  esci_assert_type(reference_measure, "is.numeric")
  comparison_measure_report <- esci_assert_vector_valid_length(
    comparison_measure,
    lower = 2,
    lower_inclusive = FALSE,
    na.rm = TRUE
  )
  # Check reference measure

  reference_measure_report <- esci_assert_vector_valid_length(
    reference_measure,
    lower = 2,
    lower_inclusive = FALSE,
    na.rm = TRUE
  )


  if (comparison_measure_report$valid != reference_measure_report$valid) {
    # vectors not of same length!
    msg <- glue::glue("
The comparison_measure and reference_measure are not the same length.
The comparison_measure has {comparison_measure_report$valid} valid values;
The outcome_variable length is {reference_measure_report$valid} valid values.
    ")
    stop(msg)
  }

  # Initialize -----------------------
  # Remove NA values from both vectors, pairwise
  reference_measure <- reference_measure[!is.na(comparison_measure)]
  comparison_measure <- comparison_measure[!is.na(comparison_measure)]

  comparison_measure <- comparison_measure[!is.na(reference_measure)]
  reference_measure <- reference_measure[!is.na(reference_measure)]

  # Analysis --------------------------
  mydf <- data.frame(
    "comparison_measure" = comparison_measure,
    "reference_measure" = reference_measure
  )
  colnames(mydf) <- grouping_variable_levels

  estimate <- estimate_mdiff_paired.data.frame(
    data = mydf,
    comparison_measure = grouping_variable_levels[1],
    reference_measure = grouping_variable_levels[2],
    conf_level = conf_level,
    save_raw_data <- save_raw_data
  )

  # Output prep -------------------------
  # Update estimate properties
  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL
  estimate$properties$warnings <- c(
    estimate$properties$warnings,
    warnings
  )


  return(estimate)
}


estimate_mdiff_paired.data.frame <- function(
  data,
  comparison_measure,
  reference_measure,
  conf_level = conf_level,
  save_raw_data = save_raw_data
) {

  # Input checks------------------


  # Initialization -----------------
  warnings <- c(NULL)
  contrast <- c(-1, 1)
  grouping_variable_levels <- c(comparison_measure, reference_measure)
  names(contrast) <- grouping_variable_levels


  # Analysis ----------------------------
  # Overview
  missing <- estimate_magnitude.jamovi(
    data = data,
    outcome_variables = c(reference_measure, comparison_measure),
    conf_level = conf_level
  )$overview

  # Deal wtih NA
  data <- data[!is.na(data[[reference_measure]]), ]
  data <- data[!is.na(data[[comparison_measure]]), ]

  # dispatch to .summary to obtain es_mean_difference and es_smd
  estimate <- estimate_mdiff_paired.summary(
    comparison_mean = mean(data[[comparison_measure]]),
    comparison_sd = sd(data[[comparison_measure]]),
    reference_mean = mean(data[[reference_measure]]),
    reference_sd = sd(data[[reference_measure]]),
    n = nrow(data),
    correlation = cor(data[[comparison_measure]], data[[reference_measure]], use = "complete.obs"),
    grouping_variable_levels = grouping_variable_levels,
    conf_level = conf_level
  )


  es_mdn <- wrapper_ci.median.ps(
    comparison_measure = data[[comparison_measure]],
    reference_measure = data[[reference_measure]],
    grouping_variable_levels = grouping_variable_levels,
    conf_level = conf_level
  )

  es_mdn_ta <- wrapper_ci.median.ps(
    comparison_measure = data[[comparison_measure]],
    reference_measure = data[[reference_measure]],
    grouping_variable_levels = grouping_variable_levels,
    conf_level = 1 - ((1 - conf_level)*2)
  )

  es_mdn$es_median_difference$ta_LL <- es_mdn$es_median_difference$LL
  es_mdn$es_median_difference$ta_UL <- es_mdn$es_median_difference$UL

  estimate$es_median_difference <- es_mdn$es_median_difference
  estimate$es_median_difference_properties <- es_mdn$es_median_difference_properties

  estimate$es_mean_ratio <- wrapper_ci_ratio.ps(
    mean_or_median = "mean",
    comparison_measure = data[[comparison_measure]],
    reference_measure = data[[reference_measure]],
    grouping_variable_levels = grouping_variable_levels,
    conf_level = conf_level
  )


  no_negs <- (all(data[[comparison_measure]][!is.na(data[[comparison_measure]])] >= 0) & all(data[[reference_measure]][!is.na(data[[reference_measure]])] >= 0))

  estimate$es_mean_ratio_properties <- list(
    message_html = paste(
      if (no_negs) "" else "WARNING!  Your dataset includes negative values.  ",
      "This effect-size measure is appropriate only for true ratio scales.",
      sep = ""
    )
  )

  if (!no_negs) {
    estimate$warnings <- c(
      estimate$warnings,
      "neg_values" = "The ratio between group effect size is appropriate only for true ratio scales where values < 0 are impossible.  Your data include at least one negative value, so the requested ratio effect size is not reported."
    )
  }


  estimate$es_median_ratio <- wrapper_ci_ratio.ps(
    mean_or_median = "median",
    comparison_measure = data[[comparison_measure]],
    reference_measure = data[[reference_measure]],
    grouping_variable_levels = grouping_variable_levels,
    conf_level = conf_level
  )

  estimate$es_median_ratio_properties <- estimate$es_mean_ratio_properties

  estimate$overview <- estimate_magnitude.jamovi(
    data = data,
    outcome_variables = c(reference_measure, comparison_measure),
    conf_level = conf_level
  )$overview


  if (sum(missing$missing) > 0) {
    estimate$overview$missing <- missing$missing
    estimate$overview_propertes <- list()
    invalid <- max(missing$n + missing$missing) - min(estimate$overview$n)
    estimate$overview_properties$message <-
      paste(
        "N_pairs = ",
        max(estimate$overview$n),
        ".  There were ",
        invalid,
        " rows with incomplete data.  All analyses are based only on the ",
        max(estimate$overview$n),
        " rows with complete data.",
        sep = ""
      )
    estimate$overview_properties$message_html <-
      paste(
        "<i>N</i><sub>pairs</sub> = ",
        max(estimate$overview$n),
        "<br>There were ",
        invalid,
        " rows with incomplete data.<br>All analyses are based only on the ",
        max(estimate$overview$n),
        " rows with complete data.",
        sep = ""
      )
  } else {
    estimate$overview_properties$message <-
      paste(
        "N_pairs = ",
        max(estimate$overview$n),
        ".",
        sep = ""
      )

    estimate$overview_properties$message_html <-
      paste(
        "<i>N</i><sub>pairs</sub> = ",
        max(estimate$overview$n),
        ".",
        sep = ""
      )
  }

  # Prep output --------------------------------------

  # raw data
  if (save_raw_data) {
    estimate$raw_data <- data.frame(
      "comparison_measure" = data[[comparison_measure]],
      "reference_measure" = data[[reference_measure]]
    )
  }

  # update estimate properties
  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))
  estimate$properties$warnings <- c(
    estimate$properties$warnings,
    warnings
  )

  return(estimate)

}


