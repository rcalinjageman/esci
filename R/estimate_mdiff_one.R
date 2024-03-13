#' Estimates for a single-group design with a continuous outcome
#' variable compared to a reference or population value
#'
#' @description Returns object
#' `estimate_mdiff_one` is suitable for a single-group design
#' with a continuous outcome variable that is compared to a reference
#' or population value.  It can express estimates as mean differences,
#' standardized mean differences (Cohen's d) or median differences
#' (raw data only).  You can pass raw data or summary data.
#'
#'
#' @details
#' Reach for this function in place of a *z*-test or one-sample *t*-test.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can test hypotheses with
#' [esci::test_mdiff()].
#'
#' The estimated mean differences are from [statpsych::ci.mean1()].
#'
#' The estimated SMDs are from [esci::CI_smd_one()].
#'
#' The estimated median differences are from [statpsych::ci.median1()]
#'
#'
#' @param data For raw data - a data frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param comparison_mean For summary data, a numeric
#' @param comparison_sd For summary data, numeric > 0
#' @param comparison_n For summary data, a numeric integer > 0
#' @param reference_mean Reference value, defaults to 0
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
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
#' - **es_mean**
#'     - *outcome_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_median**
#'     - *outcome_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **raw_data**
#'     - *grouping_variable* -
#'     - *outcome_variable* -
#' - **es_mean_difference**
#'     - *outcome_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#'     - *type* -
#' - **es_median_difference**
#'     - *outcome_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *df* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#'     - *type* -
#' - **es_smd**
#'     - *outcome_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *numerator* -
#'     - *denominator* -
#'     - *SE* -
#'     - *df* -
#'     - *d_biased* -
#'
#'
#' @examples
#' # From raw data
#' data("data_penlaptop1")
#' estimate <- esci::estimate_mdiff_one(
#'   data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
#'   outcome_variable = transcription,
#'   reference_mean = 10
#' )
#' estimate
#'
#' \dontrun{
#' # To visualize the estimate
#' plot_mdiff(estimate)
#' }
#'
#' \dontrun{
#' # To conduct a hypothesis test
#' test_mdiff(estimate, effect_size = "mean", rope = c(-2, 2))
#' }
#'
#'
#' # From summary data
#' mymean <- 12.09
#' mysd <- 5.52
#' myn <- 103
#'
#' estimate <- esci::estimate_mdiff_one(
#'   comparison_mean = mymean,
#'   comparison_sd = mysd,
#'   comparison_n = myn,
#'   reference_mean = 12
#' )
#' estimate
#'
#' \dontrun{
#' # To visualize the estimate
#' plot_mdiff(estimate)
#' }
#'
#' \dontrun{
#' # To conduct a hypothesis test
#' test_mdiff(estimate, effect_size = "mean", rope = c(-2, 2))
#' }
#'
#' @export
estimate_mdiff_one <- function(
  data = NULL,
  outcome_variable = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  comparison_n = NULL,
  reference_mean = 0,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"

  esci_assert_type(reference_mean, "is.numeric")

  # Check to see if summary data has been passed
  if (!is.null(comparison_mean)) {
    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'outcome_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data

    analysis_type <- "summary"

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


    # Now we have to figure out what type of raw data:
    #   could be tidy column names, string column names, or vectors
    # We check to see if we have a tidy column name by trying to evaluate it
    is_column_name <- try(outcome_variable, silent = TRUE)

    if(is(is_column_name, "try-error")) {
      # Column names have been passed, check if need to be quoted up

      outcome_variable_enquo <- rlang::enquo(outcome_variable)
      outcome_variable_quoname <- try(
        eval(rlang::as_name(outcome_variable_enquo)), silent = TRUE
      )
      if (!is(outcome_variable_quoname, "try-error")) {
        # This only succeeds if outcome_variable was passed unquoted
        # Reset outcome_variable to be fully quoted
        outcome_variable <- outcome_variable_quoname
        outcome_variable_name <- outcome_variable
      }

      # Ready to be analyzed as a list of string column names
      analysis_type <- "data.frame"

    } else if (is(outcome_variable, "numeric")) {
      # At this stage, we know that y was not a tidy column name,
      #  so it should be either a vector of raw data (class = numeric)
      #  or a vector of column names passed as strings
      analysis_type <- "vector"
    } else if (is(outcome_variable, "character")) {
      # Ok, must have been string column names
      if (length(outcome_variable) == 1) {
        if (outcome_variable_name == "My outcome variable") {
          outcome_variable_name <- outcome_variable
        }
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"
      }
    }
  }

  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if (analysis_type == "summary") {
    estimate <- estimate_magnitude(
      mean = comparison_mean,
      sd = comparison_sd,
      n = comparison_n,
      outcome_variable_name = outcome_variable_name,
      conf_level = conf_level
    )
  } else if (analysis_type == "vector") {
    if (outcome_variable_name == "My outcome variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }

    estimate <- estimate_magnitude(
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable_name,
      conf_level = conf_level,
      save_raw_data = save_raw_data
    )

    estimate_ta <- estimate_magnitude(
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable_name,
      conf_level = 1 - ((1-conf_level)*2),
      save_raw_data = save_raw_data
    )

  } else if (analysis_type == "data.frame") {
    estimate <- estimate_magnitude(
      data = data,
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable_name,
      conf_level = conf_level,
      save_raw_data = save_raw_data
    )

    estimate_ta <- estimate_magnitude(
      data = data,
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable_name,
      conf_level = 1 - ((1-conf_level)*2),
      save_raw_data = save_raw_data
    )

  } else if (analysis_type == "jamovi") {
    estimate <- estimate_mdiff_one.jamovi(
      data = data,
      outcome_variables = outcome_variable,
      reference_mean = reference_mean,
      conf_level = conf_level,
      save_raw_data = save_raw_data
    )

    estimate_ta  <- estimate_mdiff_one.jamovi(
      data = data,
      outcome_variables = outcome_variable,
      reference_mean = reference_mean,
      conf_level = 1 - ((1-conf_level)*2),
      save_raw_data = save_raw_data
    )

    return(estimate)
  }

  if (!is.null(estimate)) {
    # Store some info from the results ------------------
    comparison_mean = estimate$overview$mean
    comparison_sd = estimate$overview$sd
    comparison_n = estimate$overview$n
    effect_label = paste(
      outcome_variable_name,
      "\U2012 Reference value",
      sep = " "
    )


    # es_mean difference table ---------------------------
    estimate$es_mean_difference <- rbind(
      estimate$es_mean,
      estimate$es_mean,
      estimate$es_mean
    )
    estimate$es_mean_difference$type <- c(
      "Comparison",
      "Reference",
      "Difference"
    )
    estimate$es_mean_difference$effect <- c(
      outcome_variable_name,
      "Reference value",
      effect_label
    )
    estimate$es_mean_difference[2, c("effect_size", "LL", "UL", "SE", "df")] <-
      c(reference_mean, NA, NA, NA, NA)
    estimate$es_mean_difference[3, c("effect_size", "LL", "UL", "ta_LL", "ta_UL")] <-
      estimate$es_mean_difference[3, c("effect_size", "LL", "UL", "ta_LL", "ta_UL")] - reference_mean

    # estimate$es_mean <- NULL
    # estimate$es_mean_properties <- NULL


    # es_median_difference_table
    if (!is.null(estimate$es_median)) {
      estimate$es_median_difference <- estimate$es_mean_difference
      estimate$es_median_difference$effect_size[1] <- estimate$es_median$effect_size[[1]]
      estimate$es_median_difference$effect_size[3] <- estimate$es_median$effect_size[[1]] - reference_mean
      estimate$es_median_difference$LL[1] <- estimate$es_median$LL[[1]]
      estimate$es_median_difference$LL[3] <- estimate$es_median$LL[[1]] - reference_mean
      estimate$es_median_difference$UL[1] <- estimate$es_median$UL[[1]]
      estimate$es_median_difference$UL[3] <- estimate$es_median$UL[[1]] - reference_mean
      estimate$es_median_difference$SE[c(1, 3)] <- estimate$es_median$SE[[1]]
      estimate$es_median_difference$df[c(1, 3)] <- estimate$es_median$df[[1]]
      estimate$es_median_difference$ta_LL[1] <- estimate_ta$es_median$LL[[1]]
      estimate$es_median_difference$ta_LL[3] <- estimate_ta$es_median$LL[[1]] - reference_mean
      estimate$es_median_difference$ta_UL[1] <- estimate_ta$es_median$UL[[1]]
      estimate$es_median_difference$ta_UL[3] <- estimate_ta$es_median$UL[[1]] - reference_mean
      estimate$es_median_difference$ta_LL[2] <- NA
      estimate$es_median_difference$ta_UL[2] <- NA
    }

    # SMD --------------------
    smd_one <- CI_smd_one(
      mean = comparison_mean,
      sd = comparison_sd,
      n = comparison_n,
      reference_mean = reference_mean,
      correct_bias = TRUE,
      conf_level = conf_level
    )


    estimate$es_smd_properties <- smd_one$properties
    smd_one$properties <- NULL
    smd_one <- list(list(effect = effect_label), smd_one)

    estimate$es_smd <- as.data.frame(smd_one)

    estimate$es_smd <- cbind(
      outcome_variable_name = outcome_variable_name,
      estimate$es_smd
    )


    # smd_one <- wrapper_ci.stdmean1(
    #   comparison_mean = comparison_mean,
    #   comparison_sd = comparison_sd,
    #   comparison_n = comparison_n,
    #   reference_mean = reference_mean,
    #   effect_label = effect_label,
    #   conf_level = conf_level
    # )


    # Other properties ---------------------------
    estimate$properties$contrast <- c(1, -1)
    names(estimate$properties$contrast) <- c(
      outcome_variable_name,
      "Reference value"
    )
    estimate$es_mean_difference_properties <- list(
      effect_size_name = "M_Diff",
      effect_size_name_html = "<i>M</i><sub>diff</sub>",
      effect_size_category = "difference",
      effect_size_precision = "magnitude",
      conf_level = conf_level,
      error_distribution = "norm"
    )


    return(estimate)
  }


  stop("Something went wrong dispatching this function")

}



estimate_mdiff_one.jamovi <- function(
  data,
  outcome_variables,
  reference_mean = 0,
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  res <- list()

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.data-frame, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the .vector version of this function
    res[[outcome_variable]] <- estimate_mdiff_one(
      data = data,
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable,
      reference_mean = reference_mean,
      conf_level = conf_level,
      save_raw_data = save_raw_data
    )

  }

  res <- esci_estimate_consolidate(res)
  class(res) <- "esci_estimate"

  return(res)

}
