#' Estimates for a continuous variable with no grouping (single-group design)
#'
#' @description
#' `estimate_magnitude` is suitable for a single group design with a
#' continuous outcome variable.  It estimates the population mean and
#' population median (raw data only) with confidence intervals.  You can
#' pass raw data or summary data.
#'
#'
#' @details
#' Reach for this function in place of a one-sample *t*-test or *z*-test.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_magnitude()].
#'
#' If you want to compare your sample to a known value or reference, then
#' use [esci::estimate_mdiff_one()].
#'
#' The estimated mean is from [statpsych::ci.mean()].
#'
#' The estimated median is from [statpsych::ci.median()]
#'
#'
#' @param data For raw data - A data frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param mean For summary data - A numeric representing the mean of the outcome
#'   variable
#' @param sd For summary data - A numeric > 0, standard deviation of the outcome
#'   variable
#' @param n For summary data - An integer > 0, sample size of the outcome
#'   variable
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param save_raw_data For raw data; defaults to TRUE; set to FALSE to save
#'   memory by not returning raw data in estimate object
#'
#'
#' @return Returns an object of class esci_estimate
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
#'
#'
#' @examples
#' # From raw data
#' data("data_penlaptop1")
#'
#' estimate_from_raw <- esci::estimate_magnitude(
#'   data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
#'     outcome_variable = transcription
#' )
#'
#' # To visualize the estimate
#' myplot_from_raw <- esci::plot_magnitude(
#'   estimate_from_raw,
#'   effect_size = "median"
#' )
#'
#'
#' # From summary data
#' mymean <- 24.5
#' mysd <- 3.65
#' myn <- 40
#'
#' estimate_from_summary <- esci::estimate_magnitude(
#'   mean = mymean,
#'   sd = mysd,
#'   n = myn
#' )
#'
#' # To visualize the estimate
#' myplot_from_summary <- esci::plot_magnitude(
#'   estimate_from_summary,
#'   effect_size = "mean"
#' )
#'
#'
#' @export
estimate_magnitude <- function(
  data = NULL,
  outcome_variable = NULL,
  mean = NULL,
  sd = NULL,
  n = NULL,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(mean)) {

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
    if(!is.null(mean))  stop(
      "You have passed raw data,
      so don't pass the 'mean' parameter used for summary data.")
    if(!is.null(sd))  stop(
      "You have passed raw data,
      so don't pass the 'sd' parameter used for summary data.")
    if(!is.null(n))  stop(
      "You have passed raw data,
      so don't pass the 'n' parameter used for summary data.")

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

  if(analysis_type == "data.frame") {
    return(
      estimate_magnitude.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )

      # estimate_magnitude.data.frame(
      #   data = data,
      #   outcome_variable = make.names(outcome_variable),
      #   conf_level = conf_level,
      #   save_raw_data = save_raw_data
      # )
    )
  } else if (analysis_type == "jamovi") {
    return(
      estimate_magnitude.jamovi(
        data = data,
        outcome_variables = outcome_variable,
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )
    )


  } else if (analysis_type == "summary") {
    return(
      estimate_magnitude.summary(
        mean = mean,
        sd = sd,
        n = n,
        outcome_variable_name = outcome_variable_name,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {
    if (outcome_variable_name == "My outcome variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }

    return(
      estimate_magnitude.vector(
        outcome_variable = outcome_variable,
        outcome_variable_name = outcome_variable_name,
        conf_level = conf_level,
        save_raw_data = save_raw_data
        )
    )
  }

  stop("Something went wrong dispatching this function")

}


# Handles construction of the effect_sizes and standardized_effect_sizes tables
estimate_magnitude.base <- function(
  overview_table,
  outcome_variable_name,
  conf_level
) {

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # outcome_variable_name - non-zero length character
  # conf_level should be a numeric value > 0 and <1

  # Should already be checked
  # mean, sd, and n should already be checked to be numerics
  #  with sd > 0 and n an integer > 0

  # Check variable names
  esci_assert_type(outcome_variable_name, "is.character")

  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )

  # Prepare esci_estimate object that will be returned-------------------------
  estimate <- list()
  estimate$properties <- list(
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = NULL,
    contrast = NULL,
    conf_level = conf_level
  )
  estimate$es_mean_properties <- list(
    effect_size_name = "M",
    effect_size_name_html = "<i>M</i>",
    effect_size_category = "simple",
    effect_size_precision = "magnitude",
    conf_level = conf_level,
    error_distribution = "t_dist"
  )
  class(estimate) <- "esci_estimate"
  estimate$overview <- overview_table

  estimate$es_mean <- data.frame(
    "outcome_variable_name" = overview_table$outcome_variable_name,
    "effect" = overview_table$outcome_variable_name,
    "effect_size" = overview_table$mean,
    "LL" = overview_table$mean_LL,
    "UL" = overview_table$mean_UL,
    "SE" = overview_table$mean_SE,
    "df" = overview_table$df
  )

  alpha <- 1 - conf_level
  estimate$es_mean$tcrit <- stats::qt(p = alpha * 2, df = estimate$es_mean$df, lower.tail = FALSE)
  estimate$es_mean$ta_LL <- estimate$es_mean$effect_size - (estimate$es_mean$SE * estimate$es_mean$tcrit)
  estimate$es_mean$ta_UL <- estimate$es_mean$effect_size + (estimate$es_mean$SE * estimate$es_mean$tcrit)
  estimate$es_mean$tcrit <- NULL


  if (!is.null(overview_table$median)) {
    estimate$es_median <- data.frame(
      "outcome_variable_name" = overview_table$outcome_variable_name,
      "effect" = overview_table$outcome_variable_name,
      "effect_size" = overview_table$median,
      "LL" = overview_table$median_LL,
      "UL" = overview_table$median_UL,
      "SE" = overview_table$median_SE,
      "df" = overview_table$df
    )
    estimate$es_median_properties <- list(
      effect_size_name = "Mdn",
      effect_size_name_html = "<i>Mdn</i>",
      effect_size_category = "simple",
      effect_size_precision = "magnitude",
      conf_level = conf_level,
      error_distribution = "t_dist"
    )
  }



  return(estimate)

}


estimate_magnitude.summary <- function(
  mean,
  sd,
  n,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # mean - numeric data
  # sds  - numeric, >0
  # ns - numeric integer > 1

  # The base function will check:
  #  conf_level is >0 and <1
  #  outcome_variable_name - optional, non-zero length character

  # Check mean
  esci_assert_type(mean, "is.numeric")
  # Check sd
  esci_assert_type(sd, "is.numeric")
  esci_assert_range(
    sd,
    lower = 0,
    lower_inclusive = FALSE
  )
  # Check n
  esci_assert_type(n, "is.numeric")
  esci_assert_type(n, "is.numeric")
  esci_assert_type(n, "is.whole.number")
  esci_assert_range(
    n,
    lower = 2,
    lower_inclusive = TRUE
  )


  # Do analysis ------------------------------------
  overview_table <- overview.summary(
    means = mean,
    sds = sd,
    ns = n,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = FALSE
  )

  estimate <- estimate_magnitude.base(
      overview_table = overview_table,
      outcome_variable_name = outcome_variable_name,
      conf_level = conf_level
  )


  estimate$properties$data_type <- "summary"
  estimate$properties$data_source <- NULL

  return(estimate)

}


estimate_magnitude.vector <- function(
  outcome_variable,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  # Input checks --------------------------------------------------------------
  # This function expects:
  #  outcome_variable to be a vector of numeric data:
  #      with > 2 valid rows
  #  save_raw_data is a logical, TRUE or FALSE


  # Check outcome variable
  esci_assert_type(outcome_variable, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    outcome_variable,
    lower = 2,
    lower_inclusive = FALSE,
    na.invalid = FALSE
  )

  # Check save_raw_data
  esci_assert_type(save_raw_data, "is.logical")


  # Do the analysis --------------------------------------------------
  # Create overview -- which will gracefully deal with missing and n=0 or n=1
  all_overview <- overview.vector(
    outcome_variable = outcome_variable,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = FALSE
  )

  # From the overview function, get just the valid groups
  no_miss_overview <- all_overview[row.names(all_overview) != "missing", ]
  overview <- no_miss_overview[no_miss_overview$n > 1, ]

  # Dispatch only valid groups to base function
  estimate <- estimate_magnitude.base(
    overview_table = overview,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level
  )

  estimate$overview <- all_overview
  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL

  # 2 alpha CI for median
  mdn_2a <- wrapper_ci.median(
    alpha = (1 - conf_level)*2,
    y =outcome_variable[!is.na(outcome_variable)]
  )

  estimate$es_median$ta_LL <- mdn_2a[1, "LL"]
  estimate$es_median$ta_UL <- mdn_2a[1, "UL"]

  # Store raw data -----------------------------------------------
  if (save_raw_data) {
    estimate$raw_data <- data.frame(
      grouping_variable = outcome_variable_name,
      outcome_variable = outcome_variable
    )
  }

  return(estimate)
}


estimate_magnitude.data.frame <- function(
  data,
  outcome_variable,
  conf_level = 0.95,
  save_raw_data = TRUE
) {


  # Input Checks -------------------------------------------------------------
  # This function expects:
  #   data to be a data frame
  #   outcome_variable to be a numeric column in data, with more than 2 rows

  esci_assert_type(data, "is.data.frame")

  # Validate this outcome variable
  esci_assert_valid_column_name(data, outcome_variable)
  esci_assert_column_type(data, outcome_variable, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Now pass along to the .vector version of this function
  estimate <- estimate_magnitude.vector(
    outcome_variable = data[[outcome_variable]],
    outcome_variable_name = outcome_variable,
    conf_level = conf_level,
    save_raw_data = save_raw_data
  )

  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))

  return(estimate)

}


estimate_magnitude.jamovi <- function(
  data,
  outcome_variables,
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  res <- list()

  # Cycle through the list of columns;
  #  for each call estimate_magnitude.data-frame, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the .vector version of this function
    res[[outcome_variable]] <- estimate_magnitude.data.frame(
      data = data,
      outcome_variable = outcome_variable,
      conf_level = conf_level,
      save_raw_data = save_raw_data
    )

  }

  res <- esci_estimate_consolidate(res)
  class(res) <- "esci_estimate"

  return(res)

}


