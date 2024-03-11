#' Estimates for a two-group study with a continuous outcome variable
#'
#'
#' @description
#' \loadmathjax
#' `estimate_mdiff_two` is suitable for a simple two-group design
#' with a continuous outcome variable.  It provides estimates and CIs for the
#' population mean difference between the repeated measures, the standardized
#' mean difference (SMD; Cohen's d) between the repeated measures, and the
#' median difference between the repeated measures (raw data only).  You can
#' pass raw data or summary data.
#'
#'
#' @details
#' Reach for this function in place of an independent-samples *t*-test.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can test hypotheses with
#' [esci::test_mdiff()].
#'
#' The estimated mean differences are from [statpsych::ci.mean2()].
#'
#' The estimated SMDs are from [esci::CI_smd_ind_contrast()].
#'
#' The estimated median differences are from [statpsych::ci.median2()].
#'
#'
#' @param data For raw data - a data.frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param grouping_variable For raw data - The column name of the grouping
#'   variable, or a vector of group names
#' @param comparison_mean For summary data, a numeric
#' @param comparison_sd For summary data, numeric > 0
#' @param comparison_n For summary data, a numeric integer > 0
#' @param reference_mean For summary data, a numeric
#' @param reference_sd For summary data, numeric > 0
#' @param reference_n For summary data, a numeric integer > 0
#' @param grouping_variable_levels For summary data - An optional vector of
#'   2 group labels
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param grouping_variable_name Optional friendly name for the grouping
#'   variable.  Defaults to 'My grouping variable' or the grouping variable
#'   column name if a data.frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param assume_equal_variance Defaults to FALSE
#' @param save_raw_data For raw data; defaults to TRUE; set to FALSE to save
#'   memory by not returning raw data in estimate object
#' @param switch_comparison_order Defaults to FALSE
#'
#'
#' @return Returns object of class esci_estimate
#' - **es_mean_difference**
#'     - *type* -
#'     - *outcome_variable_name* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_median_difference**
#'     - *type* -
#'     - *outcome_variable_name* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_smd**
#'     - *outcome_variable_name* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *numerator* -
#'     - *denominator* -
#'     - *SE* -
#'     - *df* -
#'     - *d_biased* -
#' - **es_mean_ratio**
#'     - *outcome_variable_name* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *comparison_mean* -
#'     - *reference_mean* -
#' - **es_median_ratio**
#'     - *outcome_variable_name* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *comparison_median* -
#'     - *reference_median* -
#' - **overview**
#'     - *outcome_variable_name* -
#'     - *grouping_variable_name* -
#'     - *grouping_variable_level* -
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
#' - **raw_data**
#'     - *grouping_variable* -
#'     - *outcome_variable* -
#'
#'
#' @examples
#' # From summary data
#' estimate <- estimate_mdiff_two(
#'   comparison_mean = 12.09,
#'   comparison_sd = 5.52,
#'   comparison_n = 103,
#'   reference_mean = 6.88,
#'   reference_sd = 4.22,
#'   reference_n = 48,
#'   grouping_variable_levels = c("Ref-Laptop", "Comp-Pen"),
#'   outcome_variable_name = "% Transcription",
#'   grouping_variable_name = "Note-taking type",
#'   assume_equal_variance = TRUE
#' )
#'
#' \dontrun{
#' # To visualize the estimated mean difference
#' plot_mdiff(estimate, effect_size = "mean")
#' }

#'
#' # From raw data
#'   data("data_penlaptop1")
#'
#'   estimate <- esci::estimate_mdiff_two(
#'     data = data_penlaptop1,
#'     outcome_variable = transcription,
#'     grouping_variable = condition,
#'     switch_comparison_order = TRUE,
#'     assume_equal_variance = TRUE
#'   )
#'
#' \dontrun{
#' # To visualize the estimated median difference (raw data only)
#' plot_mdiff(estimate, effect_size = "median")
#' }
#'
#'
#' @export
estimate_mdiff_two <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  comparison_n = NULL,
  reference_mean = NULL,
  reference_sd = NULL,
  reference_n = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE,
  switch_comparison_order = FALSE
) {

  analysis_type <- "Undefined"

  # Check to see if summary data has been passed
  if (
    !is.null(comparison_mean) |
    !is.null(comparison_sd) |
    !is.null(comparison_n) |
    !is.null(reference_mean) |
    !is.null(reference_sd) |
    !is.null(reference_n)
    ) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(grouping_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'outcome_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data

    return(
      estimate_mdiff_ind_contrast(
        means = c(reference_mean, comparison_mean),
        sds = c(reference_sd, comparison_sd),
        ns = c(reference_n, comparison_n),
        grouping_variable_levels = grouping_variable_levels,
        grouping_variable_name = grouping_variable_name,
        outcome_variable_name = outcome_variable_name,
        contrast = if (switch_comparison_order)
            c(1, -1)
          else
            c(-1, 1),
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(comparison_mean))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_mean' parameter used for summary data.")
    if(!is.null(comparison_sd))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_sd' parameter used for summary data.")
    if(!is.null(comparison_n))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_n' parameter used for summary data.")
    if(!is.null(reference_mean))  stop(
      "You have passed raw data,
      so don't pass the 'reference_mean' parameter used for summary data.")
    if(!is.null(reference_sd))  stop(
      "You have passed raw data,
      so don't pass the 'reference_sd' parameter used for summary data.")
    if(!is.null(reference_n))  stop(
      "You have passed raw data,
      so don't pass the 'reference_n' parameter used for summary data.")
    if(!is.null(grouping_variable_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_levels' parameter used for summary data.")


    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

      is_char <- try(
        is.character(grouping_variable), silent = TRUE
      )
      if (is(is_char, "try-error")) {
        # If not a character, must have been quoted
        grouping_variable_enquo <- rlang::enquo(grouping_variable)
        grouping_variable_quoname <- try(
          eval(rlang::as_name(grouping_variable_enquo)), silent = TRUE
        )
        if (!is(grouping_variable_quoname, "try-error")) {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          grouping_variable <- grouping_variable_quoname
          grouping_variable_name <- grouping_variable
        } else {
          stop("Could not parse grouping_variable")
        }
      }


      is_char <- try(
        is.character(outcome_variable), silent = TRUE
      )
      if (is(is_char, "try-error")) {
        # If not a character, must have been quoted
        outcome_variable_enquo <- rlang::enquo(outcome_variable)
        outcome_variable_quoname <- try(
          eval(rlang::as_name(outcome_variable_enquo)), silent = TRUE
        )
        if (!is(outcome_variable_quoname, "try-error")) {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          outcome_variable <- outcome_variable_quoname
          outcome_variable_name <- outcome_variable
        } else {
          stop("Could not parse outcome_variable")
        }
      }

      if (length(outcome_variable) == 1) {
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"
      }

    }


    if (analysis_type == "vector") {
      if (is.null(grouping_variable_name) | grouping_variable_name == "My grouping variable") {
        grouping_variable_name <-  deparse(substitute(grouping_variable))
      }
      if (outcome_variable_name == "My outcome variable") {
        outcome_variable_name <- deparse(substitute(outcome_variable))
      }
      grouping_variable_levels <- levels(as.factor(grouping_variable))
    } else {
      grouping_variable_levels <- levels(as.factor(data[[grouping_variable]]))
    }
    if (length(grouping_variable_levels) < 2) {
      stop(
        paste(
          "Not enough levels in grouping_variable; 2 required; only",
          length(grouping_variable_levels),
          "found in the grouping_variable",
          grouping_variable_name,
          sep = " "
        )
      )
    }

    contrast <- if(switch_comparison_order)
      c(1, -1)
    else
      c(-1, 1)
    names(contrast) <- grouping_variable_levels[1:2]

    estimate <- estimate_mdiff_ind_contrast(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        contrast = contrast,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
    )
    if (length(grouping_variable_levels) > 2) {
      estimate$warnings <- c(
        estimate$warnings,
        paste(
          "The grouping variable (",
          grouping_variable_name,
          ") had",
          length(grouping_variable_levels),
          "levels.  Only the first 2 levels were used for effect-size calculations.",
          sep = " "
        )
      )
    }
    return(estimate)

  }


  stop("Something went wrong dispatching this function")

}


