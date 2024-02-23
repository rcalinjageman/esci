#' Estimates for a repeated-measures study with two measures of a categorical
#' variable
#'
#' @description
#' \loadmathjax
#' `estimate_pdiff_paired` is suitable for a simple paired design
#' with a categorical outcome variable.  It provides estimates and CIs for the
#' population proportion difference between the repeated measures.  You can
#' pass raw data or summary data.
#'
#'
#' @details
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_pdiff()] and you can test hypotheses with
#' [esci::test_pdiff()].
#'
#' The estimated proportion differences are from [statpsych::ci.prop.ps()].
#'
#'
#' @param data For raw data - a datavframe or tibble
#' @param comparison_measure For raw data - The comparison measure, a factor.  Can
#'  be the column name of a data frame of a vector.
#' @param reference_measure For raw data - The reference measure, a factor.  Can
#'  be the column name of a data frame of a vector.
#' @param cases_consistent Count of *cases* in measure 1 that *are* also cases at
#'   measure 2; measure 1 = 0, measure 2 = 0; cell 0_0
#' @param cases_inconsistent Count of *cases* in measure 1 that are *not* cases at
#'   measure 2; measure 1 = 0, measure 2 = 1; cell 0_1
#' @param not_cases_consistent Count of *not cases* in measure 1 that *are* also
#'   not cases at measure 2; measure 1 = 1, measure 2 = 1, cell 1_1
#' @param not_cases_inconsistent Count of *not cases* in measure 1 that are *not*
#'   cases at measure 2; measure 1 = 1, measure 2 = 0, cell 1_0
#' @param case_label An optional numeric or character label for the
#'   case level.
#' @param not_case_label An optional numeric or character label for the
#'   not case level.
#' @param comparison_measure_name For summary data - An optional character
#'  label for the comparison measure.  Defaults to 'Comparison measure'
#' @param reference_measure_name For summary data - An optional character
#'  label for the reference measure.  Defaults to 'Reference measure'
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param count_NA Logical to count NAs (TRUE) in total N or not (FALSE)
#'
#'
#' @return Returns object of class esci_estimate
#'
#'
#' @export
estimate_pdiff_paired <- function(
  data = NULL,
  comparison_measure = NULL,
  reference_measure = NULL,
  cases_consistent = NULL,
  cases_inconsistent = NULL,
  not_cases_consistent = NULL,
  not_cases_inconsistent = NULL,
  case_label = 1,
  not_case_label = NULL,
  comparison_measure_name = "Comparison measure",
  reference_measure_name = "Reference measure",
  conf_level = 0.95,
  count_NA = FALSE
) {

  analysis_type <- "Undefined"

  # Check to see if summary data has been passed
  if (
    !is.null(cases_consistent) |
    !is.null(cases_inconsistent) |
    !is.null(not_cases_consistent) |
    !is.null(not_cases_inconsistent)
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
    if(!is.null(cases_consistent))  stop(
      "You have passed raw data,
      so don't pass the 'cases_consistent' parameter used for summary data.")
    if(!is.null(cases_inconsistent))  stop(
      "You have passed raw data,
      so don't pass the 'cases_inconsistent' parameter used for summary data.")
    if(!is.null(not_cases_consistent))  stop(
      "You have passed raw data,
      so don't pass the 'not_cases_consistent' parameter used for summary data.")
    if(!is.null(not_cases_inconsistent))  stop(
      "You have passed raw data,
      so don't pass the 'not_cases_inconsistent' parameter used for summary data.")

    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

      # Check comparison_measure -- if it is an unquoted column name
      #  turn it into a string and store back to grouping_variable
      is_char <- try(
        is.character(comparison_measure), silent = TRUE
      )
      if (is(is_char, "try-error")) {
        # If not a character, must have been quoted
        comparison_measure_enquo <- rlang::enquo(comparison_measure)
        comparison_measure_quoname <- try(
          eval(rlang::as_name(comparison_measure_enquo)), silent = TRUE
        )
        if (!is(comparison_measure_quoname, "try-error")) {
          # This only succeeds if comparison_measure was passed unquoted
          # Reset comparison_measure to be fully quoted
          comparison_measure <- comparison_measure_quoname
        } else {
          stop("Could not parse comparison_measure")
        }
      }


      # Check reference_measure -- if it is an unquoted column name
      #  turn it into a string and store back to grouping_variable
      is_char <- try(
        is.character(reference_measure), silent = TRUE
      )
      if (is(is_char, "try-error")) {
        # If not a character, must have been quoted
        reference_measure_enquo <- rlang::enquo(reference_measure)
        reference_measure_quoname <- try(
          eval(rlang::as_name(reference_measure_enquo)), silent = TRUE
        )
        if (!is(reference_measure_quoname, "try-error")) {
          # This only succeeds if reference_measure was passed unquoted
          # Reset reference_measure to be fully quoted
          reference_measure <- reference_measure_quoname
        } else {
          stop("Could not parse reference_measure")
        }
      }

      if (length(comparison_measure) == 1) {
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"
      }

    }


  }

  if(analysis_type == "data.frame") {
    return(
      estimate_pdiff_paired.data.frame(
        data = data,
        comparison_measure = comparison_measure,
        reference_measure = reference_measure,
        case_label = case_label,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  } else if (analysis_type == "jamovi") {
    stop("Not implemented yet")
  } else if (analysis_type == "summary") {
    return(
      estimate_pdiff_paired.summary(
        cases_consistent = cases_consistent,
        cases_inconsistent = cases_inconsistent,
        not_cases_consistent = not_cases_consistent,
        not_cases_inconsistent = not_cases_inconsistent,
        case_label = case_label,
        not_case_label = not_case_label,
        comparison_measure_name = comparison_measure_name,
        reference_measure_name = reference_measure_name,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {

    if (is.null(comparison_measure_name) | comparison_measure_name == "Comparison measure") {
      comparison_measure_name <- deparse(substitute(comparison_measure))
    }

    if (is.null(reference_measure_name) | reference_measure_name == "Reference measure") {
      reference_measure_name <- deparse(substitute(reference_measure))
    }

    return(
      estimate_pdiff_paired.vector(
        comparison_measure = comparison_measure,
        reference_measure = reference_measure,
        case_label = case_label,
        comparison_measure_name = comparison_measure_name,
        reference_measure_name = reference_measure_name,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  }


  stop("Something went wrong dispatching this function")

}


estimate_pdiff_paired.summary <- function(
  cases_consistent = NULL,
  cases_inconsistent = NULL,
  not_cases_consistent = NULL,
  not_cases_inconsistent = NULL,
  case_label = "Affected",
  not_case_label = "Not affected",
  comparison_measure_name = "Comparison measure",
  reference_measure_name = "Reference measure",
  conf_level = 0.95
) {

  # Input checks ----------------------------

  # Check case counts
  esci_assert_type(cases_consistent, "is.numeric")
  esci_assert_type(cases_consistent, "is.whole.number")
  esci_assert_range(cases_consistent, lower = 0, lower_inclusive = TRUE)
  esci_assert_type(cases_inconsistent, "is.numeric")
  esci_assert_type(cases_inconsistent, "is.whole.number")
  esci_assert_range(cases_inconsistent, lower = 0, lower_inclusive = TRUE)
  esci_assert_type(not_cases_consistent, "is.numeric")
  esci_assert_type(not_cases_consistent, "is.whole.number")
  esci_assert_range(not_cases_consistent, lower = 0, lower_inclusive = TRUE)
  esci_assert_type(not_cases_inconsistent, "is.numeric")
  esci_assert_type(not_cases_inconsistent, "is.whole.number")
  esci_assert_range(not_cases_inconsistent, lower = 0, lower_inclusive = TRUE)

  # Check labels
  if (is.null(case_label) | is.numeric(case_label)) {
    case_label <- "Affected"
  }
  if (is.null(not_case_label)) {
    not_case_label <- paste("Not ", case_label, sep = "")
  }

  # Initialize -----------------------------
  n <- sum(
    cases_consistent,
    cases_inconsistent,
    not_cases_consistent,
    not_cases_inconsistent
  )
  comparison_cases = sum(
    cases_consistent,
    not_cases_inconsistent
  )
  reference_cases = sum(
    cases_consistent,
    cases_inconsistent
  )

  # Analysis --------------------------
  # analyze as a two-group design
  estimate <- estimate_pdiff_two(
    comparison_cases = comparison_cases,
    comparison_n = n,
    reference_cases = reference_cases,
    reference_n = n,
    case_label = case_label,
    not_case_label = not_case_label,
    grouping_variable_levels = c(
      reference_measure_name,
      comparison_measure_name
    ),
    conf_level = conf_level
  )

  # Replace estimated difference based on paired design

  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  if (statpsych_version > 130) {
    pdiff_row <- as.data.frame(
      statpsych::ci.prop.ps(
        alpha = 1 - conf_level,
        f00 = cases_consistent,
        f01 = cases_inconsistent,
        f10 = not_cases_inconsistent,
        f11 = not_cases_consistent
      )
    )

  } else {

    pdiff_row <- as.data.frame(
      statpsych::ci.prop.ps(
        alpha = 1 - conf_level,
        f00 = not_cases_consistent,
        f01 = not_cases_inconsistent,
        f10 = cases_inconsistent,
        f11 = cases_consistent
      )
    )

  }




  estimate$es_proportion_difference[3, c("LL", "UL", "SE", "effect_size_adjusted")] <-
    pdiff_row[ , c("LL", "UL", "SE", "Estimate")]

  type_store <- estimate$es_proportion_difference$type
  cancel_columns <- c(
    "type", "outcome_variable_name", "grouping_variable_name"
  )
  estimate$es_proportion_difference[ , cancel_columns] <- NULL
  estimate$es_proportion_difference <- cbind(
    type = type_store,
    comparison_measure_name = comparison_measure_name,
    reference_measure_name = reference_measure_name,
    estimate$es_proportion_difference
  )

  estimate$overview <- rbind(
    overview_nominal.summary(
      cases = c(reference_cases, n-reference_cases),
      outcome_variable_levels = c(case_label, not_case_label),
      outcome_variable_name = reference_measure_name,
      conf_level = conf_level
    ),
    overview_nominal.summary(
      cases = c(comparison_cases, n-comparison_cases),
      outcome_variable_levels = c(case_label, not_case_label),
      outcome_variable_name = comparison_measure_name,
      conf_level = conf_level
    )
  )


  # output prep -----------------------------------------
  estimate$es_odds_ratio <- NULL
  estimate$es_phi <- NULL


  return(estimate)

}


estimate_pdiff_paired.vector <- function(
  comparison_measure,
  reference_measure,
  case_label = 1,
  comparison_measure_name = "Comparison measure",
  reference_measure_name = "Reference measure",
  conf_level = 0.95,
  count_NA = FALSE
) {


  # Input checks -------------------------------------


  # Initialize -----------------------


  # Analysis --------------------------
  mydf <- data.frame(
    "comparison_measure" = comparison_measure,
    "reference_measure" = reference_measure
  )
  colnames(mydf) <- c(comparison_measure_name, reference_measure_name)

  estimate <- estimate_pdiff_paired.data.frame(
    data = mydf,
    comparison_measure = comparison_measure_name,
    reference_measure = reference_measure_name,
    case_label = case_label,
    conf_level = conf_level,
    count_NA = count_NA
  )

  # Output prep -------------------------
  # Update estimate properties
  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL


  return(estimate)
}


estimate_pdiff_paired.data.frame <- function(
  data,
  comparison_measure,
  reference_measure,
  case_label = 1,
  conf_level = 0.95,
  count_NA = count_NA
) {

  # Input checks------------------

  # data frame
  esci_assert_type(data, "is.data.frame")

  # reference_measure
  esci_assert_valid_column_name(data, reference_measure)
  esci_assert_column_type(data, reference_measure, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    reference_measure,
    lower = 2,
    na.rm = TRUE
  )
  # comparison_measure
  esci_assert_valid_column_name(data, comparison_measure)
  esci_assert_column_type(data, comparison_measure, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    comparison_measure,
    lower = 2,
    na.rm = TRUE
  )
  # levels
  if (!identical(levels(data[[comparison_measure]]), levels(data[[reference_measure]]))) {
    msg <- glue::glue("
For this analysis comparison_measure and reference_measure must have the exact same levels.
Instead, comparison_measure has {length(levels(data[[comparison_measure]]))},
  which are {paste0(levels(data[[comparison_measure]]), collapse = ', ')} and
  reference_measure has {length(levels(data[[reference_measure]]))},
  which are {paste0(levels(data[[reference_measure]]), collapse = ', ')}
    ")
    stop(msg)
  }
  comparison_measure_levels <- levels(data[[comparison_measure]])
  esci_assert_vector_valid_length(
    vector = comparison_measure_levels,
    lower = 1,
    lower_inclusive = TRUE
  )


  # Case label
  if (is.numeric(case_label)){
    if (case_label > length(levels(data[[reference_measure]])) | case_label < 1) {
      msg <- glue::glue("
The case_label is {case_label}.
The number of levels in the data[[reference_measure]] is {length(levels(data[[reference_measure]]))}.
case_label must be between 1 and {length(levels(data[[reference_measure]]))}.
    ")
      stop(msg)
    }
    case_label <- levels(data[[reference_measure]])[case_label]
  } else {
    if (!case_label %in% levels(data[[reference_measure]])) {
      msg <- glue::glue("
The case_label is {case_label}.
The levels in the outcome variable are {paste(levels(data[[reference_measure]]), collapse = ',')}.
case_label must be either a numeric or a valid level from data[[reference_measure]].
    ")
      stop()
    }

  }

  # Handle NA
  if (count_NA & nrow(data) != nrow(data[complete.cases(data), ])){

    # Determine missing level
    missing_level <- "Missing"
    while (missing_level %in% comparison_measure_levels) {
      missing_level <- paste(missing_level, "*", sep = "")
    }

    # Add missing level
    levels(data[[comparison_measure]]) <- c(levels(data[[comparison_measure]]), missing_level)
    levels(data[[reference_measure]]) <- c(levels(data[[reference_measure]]), missing_level)

    # Replace NA
    data[is.na(comparison_measure), comparison_measure] <- missing_level
    data[is.na(reference_measure), reference_measure] <- missing_level

  }

  missing <- nrow(data) - nrow(data[complete.cases(data), ])
  data <- data[complete.cases(data), ]

  # Count cases
  my_cases <- data[data[[reference_measure]] == case_label, ]
  my_not_cases <- data[data[[reference_measure]] != case_label, ]

  cases_consistent <- nrow(my_cases[my_cases[[comparison_measure]] == case_label, ])
  cases_inconsistent <- nrow(my_cases) - cases_consistent

  not_cases_consistent <- nrow(my_not_cases[my_not_cases[[comparison_measure]] != case_label, ])
  not_cases_inconsistent <- nrow(my_not_cases) - not_cases_consistent


  estimate <- estimate_pdiff_paired.summary(
    cases_consistent = cases_consistent,
    cases_inconsistent = cases_inconsistent,
    not_cases_consistent = not_cases_consistent,
    not_cases_inconsistent = not_cases_inconsistent,
    case_label = case_label,
    not_case_label = paste("Not", case_label, sep = " "),
    comparison_measure_name = comparison_measure,
    reference_measure_name = reference_measure,
    conf_level = conf_level
  )

  # Replace overview
  estimate$overview <- rbind(
    overview_nominal.data.frame(
      data = data,
      outcome_variable = reference_measure,
      conf_level = conf_level,
      count_NA = FALSE
    ),
    overview_nominal.data.frame(
      data = data,
      outcome_variable = comparison_measure,
      conf_level = conf_level,
      count_NA = FALSE
    )
  )


  if (missing > 0) {
    estimate$overview_propertes <- list()
    invalid <- missing
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


  # Update estimate properties
  estimate$properties$data_type <- "data.frame"
  #estimate$properties$data_source <- deparse(substitute(data))

  return(estimate)

}

