#' Estimates for a single-group design with a categorical outcome
#' variable compared to a reference or population value.
#'
#' @description Returns object
#' `estimate_pdiff_one` is suitable for a single-group design
#' (between subjects) with a categorical outcome variable.  It calculates
#' the effect sizes with respect to a reference or population proportion
#' (default value of 0).  It returns the estimated difference between the
#' in proportion from the reference/population value.  You can pass raw data or
#' summary data.
#'
#'
#' @details
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_pdiff()] and you can test hypotheses with
#' [esci::test_pdiff()].
#'
#' The estimated proportion differences are from [statpsych::ci.prop1()] (renamed
#' ci.prop as of statpsych 1.6).
#'
#'
#' @param data For raw data - a dataframe or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, which must be a factor, or a vector that is a factor
#' @param comparison_cases For summary data, a numeric integer > 0
#' @param comparison_n For summary data, a numeric integer >= count
#' @param reference_p Reference proportion, numeric >=0 and <=1
#' @param case_label An optional numeric or character label for the
#'   count level.
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param count_NA Logical to count NAs (TRUE) in total N or not (FALSE)
#'
#'
#' @return Returns an object of class esci_estimate
#' - **overview**
#'     - *outcome_variable_name* -
#'     - *outcome_variable_level* -
#'     - *cases* -
#'     - *n* -
#'     - *P* -
#'     - *P_LL* -
#'     - *P_UL* -
#'     - *P_SE* -
#'     - *P_adjusted* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_proportion_difference**
#'     - *outcome_variable_name* -
#'     - *case_label* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *effect_size_adjusted* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#'     - *cases* -
#'     - *n* -
#'     - *type* -
#'
#'
#' @examples
#' # From raw data
#' data("data_campus_involvement")
#'
#' estimate_from_raw <- esci::estimate_pdiff_one(
#'   esci::data_campus_involvement,
#'   CommuterStatus,
#'   reference_p = 0.50
#' )
#'
#' # To visualize the estimate
#' myplot_from_raw <- esci::plot_pdiff(estimate_from_raw)
#'
#' # To conduct a hypothesis test
#' res_htest_from_raw <- esci::test_pdiff(estimate_from_raw)
#'
#'
#' # From summary data
#' estimate_from_summary  <- esci::estimate_pdiff_one(
#'   comparison_cases = 8,
#'   comparison_n = 22,
#'   reference_p = 0.5
#' )
#'
#' # To visualize the estimate
#' myplot_from_summary <- esci::plot_pdiff(estimate_from_summary)
#'
#' # To conduct a hypothesis test
#' res_htest_from_summary <- esci::test_pdiff(estimate_from_summary)
#'
#'
#' @export
estimate_pdiff_one <- function(
  data = NULL,
  outcome_variable = NULL,
  comparison_cases = NULL,
  comparison_n = NULL,
  reference_p = 0,
  case_label = 1,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  count_NA = FALSE
) {


  # Check inputs -----------------------------------------------
  # * reference_p should be a numeric >= 0 and <= 1
  # * comparison_cases should be a numeric integer > 0
  # * comparison_n should be a numeric integer >= count
  # * all other inputs checked when dispatched to estimate_proportion

  # Check reference_p
  esci_assert_type(reference_p, "is.numeric")
  esci_assert_range(
    reference_p,
    lower = 0,
    upper = 1,
    lower_inclusive = TRUE,
    upper_inclusive = TRUE
  )




  # Figure out analysis type ---------------------------------------
  analysis_type <- "Undefined"

  # Check to see if summary data has been passed
  if (!is.null(comparison_cases)) {
    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'outcome_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data

    # Check comparison_cases
    esci_assert_type(comparison_cases, "is.numeric")
    esci_assert_type(comparison_cases, "is.whole.number")
    esci_assert_range(
      comparison_cases,
      lower = 0,
      lower_inclusive = TRUE,
    )

    # Check comparison_n
    esci_assert_type(comparison_n, "is.numeric")
    esci_assert_type(comparison_n, "is.whole.number")
    esci_assert_range(
      comparison_n,
      lower = comparison_cases,
      lower_inclusive = TRUE,
    )

    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(comparison_cases))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_cases' parameter used for summary data.")
    if(!is.null(comparison_n))  stop(
      "You have passed raw data,
      so don't pass the 'comparison_n' parameter used for summary data.")

    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

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


  }

  # Dispatch to estimate_proportion

  if (analysis_type == "summary") {
    if (is.null(case_label)) case_label <- "Affected"
    if (is.numeric(case_label) | case_label == 1) case_label <- "Affected"

    estimate <- estimate_proportion(
      cases = c(comparison_cases, comparison_n - comparison_cases),
      outcome_variable_name = outcome_variable_name,
      outcome_variable_levels = c(
        case_label,
        paste("Not", case_label, sep = " ")
      ),
      conf_level = conf_level,
      count_NA = count_NA
    )
  } else if (analysis_type == "vector") {
    if (outcome_variable_name == "My outcome variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }

    estimate <- estimate_proportion(
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable_name,
      case_label = case_label,
      conf_level = conf_level,
      count_NA = count_NA
    )
  } else if (analysis_type == "data.frame") {
    estimate <- estimate_proportion(
      data = data,
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable_name,
      case_label = case_label,
      conf_level = conf_level,
      count_NA = count_NA
    )
  } else if (analysis_type == "jamovi") {
    estimate <- estimate_pdiff_one.jamovi(
      data = data,
      outcome_variables = outcome_variable,
      reference_p = reference_p,
      case_label = case_label,
      conf_level = conf_level,
      count_NA = count_NA
    )
    return(estimate)
  }

  # Now change to an estimated difference
  if (!is.null(estimate)) {

    # Store some info from the results ------------------
    comparison_p = estimate$es_proportion$effect_size
    case_label <- estimate$es_proportion$effect
    comparison_label <- outcome_variable_name
    reference_label <- "Reference value"
    effect_label = paste(
      outcome_variable_name,
      " \U2012 Reference value",
      sep = ""
    )

    # es_proportion_difference table ---------------------------
    estimate$es_proportion_difference <- rbind(
      estimate$es_proportion,
      estimate$es_proportion,
      estimate$es_proportion
    )
    estimate$es_proportion_difference$type <- c(
      "Comparison",
      "Reference",
      "Difference"
    )
    estimate$es_proportion_difference$effect <- c(
      comparison_label,
      reference_label,
      effect_label
    )
    estimate$es_proportion_difference[2, c("effect_size", "LL", "UL", "SE", "effect_size_adjusted", "ta_LL", "ta_UL")] <-
      c(reference_p, NA, NA, NA, NA, NA, NA)
    estimate$es_proportion_difference[3, c("effect_size", "LL", "UL", "effect_size_adjusted", "ta_LL", "ta_UL")] <-
      estimate$es_proportion_difference[3, c("effect_size", "LL", "UL", "effect_size_adjusted", "ta_LL", "ta_UL")] - reference_p


    estimate$es_proportion <- NULL
    estimate$es_proportion_properties <- NULL


    # Other properties ---------------------------
    estimate$properties$contrast <- c(1, -1)
    names(estimate$properties$contrast) <- c(
      outcome_variable_name,
      "Reference Mean"
    )
    estimate$es_proportion_difference_properties <- list(
      effect_size_name = "P_Diff",
      effect_size_name_html = "<i>P</i><sub>diff</sub>",
      effect_size_category = "difference",
      effect_size_precision = "proportion",
      conf_level = conf_level,
      error_distribution = "norm"
    )

    if ("Missing" %in% estimate$overview$outcome_variable_level) {
      estimate$es_proportion_difference_properties$message <- if (count_NA)
        "Missing values were present; these were counted as part of the total sample size."
      else
        "Missing values were present; these were *not* counted as part of the total sample size."

      estimate$es_proportion_difference_properties$message_html <- if (count_NA)
        "Missing values were present; these were counted as part of the total sample size."
      else
        "Missing values were present; these were <b>not</b> counted as part of the total sample size."

      estimate$overview_properties <- list()
      estimate$overview_properties$message <- estimate$es_proportion_difference_properties$message
      estimate$overview_properties$message_html <- estimate$es_proportion_difference_properties$message_html

    }

    return(estimate)
  }


  stop("Something went wrong dispatching this function")

}



estimate_pdiff_one.jamovi <- function(
  data,
  outcome_variables,
  reference_p = 0,
  case_label = 1,
  conf_level = 0.95,
  count_NA = FALSE
) {

  res <- list()

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.data-frame, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the .vector version of this function
    res[[outcome_variable]] <- estimate_pdiff_one(
      data = data,
      outcome_variable = outcome_variable,
      outcome_variable_name = outcome_variable,
      reference_p = reference_p,
      case_label = case_label,
      conf_level = conf_level,
      count_NA = count_NA
    )

  }

  res <- esci_estimate_consolidate(res)
  class(res) <- "esci_estimate"

  return(res)

}
