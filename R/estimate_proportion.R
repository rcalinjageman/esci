#' Estimates for a categorical variable with no grouping (single-group design)
#'
#' @description
#' `estimate_proportion` is suitable for a single group design with a
#' categorical outcome variable.  It estimates the population proportion
#' for the frequency of each level of the outcome variable, with confidence
#' intervals.  You can pass raw data or summary data.
#'
#'
#' @details
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_proportion()].
#'
#' If you want to compare your estimate to a known value or reference, then
#' use [esci::estimate_pdiff_one()].
#'
#' The estimated proportions are from [statpsych::ci.prop1()].
#'
#'
#' @param data For raw data - a data frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, which must be a factor, or a vector that is a factor
#' @param cases For summary data - A vector of cases
#' @param case_label A numeric or string indicating which level
#'   of the factor to estimate.  Defaults to 1, meaning first level is analyzed
#' @param outcome_variable_levels For summary data - optional vector of 2
#'   characters indicating name of the count level and name of the not count
#'   level.  Defaults to "Affected" and "Not Affected"
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
#' - **es_proportion**
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
#'
#'
#' @examples
#' estimate <- esci::estimate_proportion(
#'   cases = c(8, 22-8),
#'   outcome_variable_levels = c("Affected", "Not Affected")
#' )
#'
#' \dontrun{
#' # To visualize the estimate
#' plot_proportion(estimate)
#' }
#'
#' @export
estimate_proportion <- function(
  data = NULL,
  outcome_variable = NULL,
  cases = NULL,
  case_label = 1,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  count_NA = FALSE
) {

  analysis_type <- "Undefined"
  if (is.null(cases)) {
    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

      is_char <- try(
        is.character(outcome_variable), silent = TRUE
      )

      if (class(is_char) == "try-error") {
        # If not a character, must have been quoted
        outcome_variable_enquo <- rlang::enquo(outcome_variable)
        outcome_variable_quoname <- try(
          eval(rlang::as_name(outcome_variable_enquo)), silent = TRUE
        )
        if (class(outcome_variable_quoname) != "try-error") {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          outcome_variable <- outcome_variable_quoname
        } else {
          stop("Could not parse outcome_variable")
        }
      }

      if (length(outcome_variable) == 1) {
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"

        estimate <- list()
        for (ocv in outcome_variable) {
          estimate[[ocv]] <- estimate_proportion(
            data = data,
            outcome_variable = ocv,
            outcome_variable_name = ocv,
            case_label = case_label,
            conf_level = conf_level,
            count_NA = count_NA
          )
        }

        estimate <- esci_estimate_consolidate(estimate)
        class(estimate) <- "esci_estimate"

        return(estimate)



      }

    }

  } else {
    analysis_type = "summary"

  }



  estimate <- list()

  estimate$overview <- overview_nominal(
    data = data,
    outcome_variable = outcome_variable,
    cases = cases,
    outcome_variable_levels = outcome_variable_levels,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    count_NA = count_NA
  )

  if (is.null(case_label)) {
    case_label = 1
  }

  if (is.character(case_label)) {
    if (case_label %in% estimate$overview$outcome_variable_level) {
      estimate$es_proportion <- estimate$overview[
        estimate$overview$outcome_variable_level == case_label,
      ]
      if (nrow(estimate$es_proportion) != 1) {
        stop("case_label did not match 1 level from outcome_variable -- improve this message")
      }
    } else {
      stop ("case_label not found in outcome_variable levels -- improve this message")
    }
  } else {
    if (case_label > nrow(estimate$overview)) {
      stop("case_label exceeds number of levels in outcome_variable -- improve this message")
    }
    estimate$es_proportion <- estimate$overview[case_label, ]
    if (nrow(estimate$es_proportion) != 1) {
      stop("case_label did not match 1 level from outcome_variable -- improve this message")
    }
  }

  # Update es_proportion table for effect_size format  ----------------
  colnames(estimate$es_proportion) <- c(
    "outcome_variable_name",
    "effect",
    "cases_temp",
    "n_temp",
    "effect_size",
    "LL",
    "UL",
    "SE",
    "effect_size_adjusted",
    "ta_LL",
    "ta_UL"
  )

  estimate$es_proportion$case_label <- paste(
    "P_",
    estimate$es_proportion$effect
  )

  estimate$es_proportion <- estimate$es_proportion[
    , c(1, ncol(estimate$es_proportion), 2:(ncol(estimate$es_proportion)-1))
  ]


  estimate$es_proportion$cases <- estimate$es_proportion$cases_temp
  estimate$es_proportion$n <- estimate$es_proportion$n_temp
  estimate$es_proportion$cases_temp <- NULL
  estimate$es_proportion$n_temp <- NULL



  # Set properties --- this will need to be updated -------------
  estimate$properties <- list(
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = NULL,
    conf_level = conf_level,
    data_type = analysis_type,
    data_source = if (is.null(data)) NULL else deparse(substitute(data))
  )

  class(estimate) <- "esci_estimate"

  return(estimate)

}

