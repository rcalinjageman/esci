#' Estimates for a multi-group study with a categorical outcome
#' variable
#'
#' @description Returns object
#' `estimate_pdiff_ind_contrast` is suitable for a multi-group design
#' (between subjects) with a categorical outcome variable.  It accepts
#' a user-defined set of contrast weights that allows estimation of any
#' 1-df contrast.  It can express estimates as a difference in proportions
#' and as an odds ratio (2-group designs only).  You can pass raw data or
#' summary data.
#'
#'
#' @details
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can test hypotheses with
#' [esci::test_mdiff()].
#'
#' The estimated proportion differences are from [statpsych::ci.lc.prop.bs()].
#'
#' The estimated odds ratios (if returned) are from [statpsych::ci.oddsratio()].
#'
#'
#' @param data For raw data - a data frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable which is a factor, or a vector that is a factor
#' @param grouping_variable For raw data - The column name of the grouping
#'   variable which is a factor, or a vector that is a factor
#' @param cases For summary data - A numeric vector of 2 or more event counts,
#'   each an integer >= 0
#' @param ns For summary data - A numeric vector of sample sizes, same length
#'   as counts, each an integer >= corresponding event count
#' @param contrast A vector of group weights, same length as number of groups.
#' @param case_label An optional numeric or character label
#'   For summary data, used as the label and defaults to 'Affected'.  For raw
#'   data, used to specify the level used for the proportion.
#' @param grouping_variable_levels For summary data - An optional vector of
#'   group labels, same length as cases
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param grouping_variable_name Optional friendly name for the grouping
#'   variable.  Defaults to 'My grouping variable' or the grouping variable
#'   column name if a data.frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param count_NA Logical to count NAs (TRUE) in total N or not (FALSE)
#'
#'
#' @return Returns object of class esci_estimate
#' - **es_proportion_difference**
#'     - *type* -
#'     - *outcome_variable_name* -
#'     - *case_label* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *LL* -
#'     - *UL* -
#'     - *SE* -
#'     - *effect_size_adjusted* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **es_odds_ratio**
#'     - *outcome_variable_name* -
#'     - *case_label* -
#'     - *grouping_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *SE* -
#'     - *LL* -
#'     - *UL* -
#'     - *ta_LL* -
#'     - *ta_UL* -
#' - **overview**
#'     - *grouping_variable_name* -
#'     - *grouping_variable_level* -
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
#' - **es_phi**
#'     - *grouping_variable_name* -
#'     - *outcome_variable_name* -
#'     - *effect* -
#'     - *effect_size* -
#'     - *SE* -
#'     - *LL* -
#'     - *UL* -
#'
#'
#' @examples
#' # From raw data
#' data("data_campus_involvement")
#'
#' estimate_from_raw <- esci::estimate_pdiff_ind_contrast(
#'   esci::data_campus_involvement,
#'   CommuterStatus,
#'   Gender,
#'   contrast = c("Male" = -1, "Female" = 1)
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
#' estimate_from_summary <- esci::estimate_pdiff_ind_contrast(
#'   cases = c(78, 10),
#'   ns = c(252, 20),
#'   case_label = "egocentric",
#'   grouping_variable_levels = c("Original", "Replication"),
#'   contrast = c(-1, 1),
#'   conf_level = 0.95
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
estimate_pdiff_ind_contrast <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  cases = NULL,
  ns = NULL,
  contrast = NULL,
  case_label = 1,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  count_NA = FALSE
) {

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(cases)) {

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
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(cases))  stop(
      "You have passed raw data,
      so don't pass the 'means' parameter used for summary data.")
    if(!is.null(ns))  stop(
      "You have passed raw data,
      so don't pass the 'ns' parameter used for summary data.")
    if(!is.null(grouping_variable_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_levels' parameter used for summary data.")


    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

      # Check grouping_variable -- if it is an unquoted column name
      #  turn it into a string and store back to grouping_variable
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

  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if(analysis_type == "data.frame") {
    return(
      estimate_pdiff_ind_contrast.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        case_label = case_label,
        contrast = contrast,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  } else if (analysis_type == "jamovi") {
    return(
      estimate_pdiff_ind_contrast.jamovi(
        data = data,
        outcome_variables = outcome_variable,
        grouping_variable = grouping_variable,
        case_label = case_label,
        contrast = contrast,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )

  } else if (analysis_type == "summary") {
    return(
      estimate_pdiff_ind_contrast.summary(
        cases = cases,
        ns = ns,
        contrast = contrast,
        case_label = case_label,
        grouping_variable_levels = grouping_variable_levels,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {
    if (is.null(grouping_variable_name) | grouping_variable_name == "My grouping variable") {
      grouping_variable_name <-  deparse(substitute(grouping_variable))
    }
    if (outcome_variable_name == "My outcome variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }

    return(
      estimate_pdiff_ind_contrast.vector(
        grouping_variable = grouping_variable,
        outcome_variable = outcome_variable,
        contrast = contrast,
        case_label = case_label,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  }

  stop("Something went wrong dispatching this function")

}


# Handles construction of the effect_sizes and standardized_effect_sizes tables
estimate_pdiff_ind_contrast.base <- function(
  cases,
  ns,
  contrast,
  case_label,
  grouping_variable_levels,
  grouping_variable_name,
  outcome_variable_name,
  conf_level = 0.95
) {

  # To do
  # Check lengths of outcome and grouping variable names

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # grouping_variable_name - non-zero length character
  # outcome_variable_name - non-zero length character
  # conf_level should be a numeric value > 0 and <1
  # contrast should be a vector of numerics
  #   If named vector, names must match grouping_variable_levels
  #   Otherwise, vector must be same length as counts

  # Should already be checked


  # Check contrast
  cells <- length(cases)
  esci_assert_type(contrast, "is.numeric")
  if(is.null(names(contrast))) {
    row_report <- esci_assert_vector_valid_length(
      contrast,
      lower = cells,
      upper = cells,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.invalid = TRUE
    )
  } else {
    for (myname in names(contrast)) {
      if (!(myname %in% grouping_variable_levels)) {
        valid_names <- paste(grouping_variable_levels, collapse = ", ")
        passed_contrast <- paste(
          names(contrast),
          " = ",
          contrast,
          collapse = "; "
        )
        msg <- glue::glue("
Contrast includues invalid name, {myname};
Valid names are {valid_names}.
The contrast passed was: {passed_contrast}.
")
        stop (msg)
      }
    }
  }

  # Check variable names
  esci_assert_type(grouping_variable_name, "is.character")
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


  # Prep the contrast ---------------------
  names(cases) <- grouping_variable_levels
  weights <- esci_tool_contrast_fixed(contrast, cases)
  contrast_labels <- esci_tool_contrast_labels(weights)
  # contrast_labels <- paste(
  #   contrast_labels,
  #   paste("P_", case_label, sep = ""),
  #   sep = " "
  # )

  # We'll estimate the comparison subset, reference subset, and the difference
  contrasts <- list(
    comparison = weights,
    reference = weights,
    difference = weights
  )
  # Filter to create comparison and reference only subsets
  contrasts$comparison[which(contrasts$comparison < 0)] <- 0
  contrasts$reference[which(contrasts$reference > 0)] <- 0
  contrasts$reference <- abs(contrasts$reference)


  # Prepare esci_estimate object that will be returned-------------------------
  estimate <- list()
  estimate$properties <- list(
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    contrast = weights,
    conf_level = conf_level
  )
  estimate$es_proportion_difference_properties <- list(
    effect_size_name = "P_Diff",
    effect_size_name_html = "<i>P</i><sub>diff</sub>",
    effect_size_category = "difference",
    effect_size_precision = "proportion",
    conf_level = conf_level,
    error_distribution = "norm"
  )
  class(estimate) <- "esci_estimate"


  # Now dispatch the contrast ----------------------------------------------
  es_proportion_difference <- NULL

  for (mycontrast in contrasts) {
    res <- as.data.frame(
      statpsych::ci.lc.prop.bs(
        alpha = 1 - conf_level,
        f = cases,
        n = ns,
        v = mycontrast
      )
    )

    res_2a <- as.data.frame(
      statpsych::ci.lc.prop.bs(
        alpha = (1 - conf_level)*2,
        f = cases,
        n = ns,
        v = mycontrast
      )
    )

    res$ta_LL <- res_2a$LL
    res$ta_UL <- res_2a$UL

    res$effect_size_adjusted <- res$Estimate
    res$Estimate <- sum(mycontrast*(cases/ns))

    es_proportion_difference <- rbind(
      es_proportion_difference,
      res
    )

  } # end of loop through 3 effect sizes


  estimate$es_proportion_difference <- data.frame(matrix(NA, ncol=1, nrow=3))[-1]
  estimate$es_proportion_difference[ , c("effect_size", "LL", "UL", "SE", "effect_size_adjusted", "ta_LL", "ta_UL")] <-
    es_proportion_difference[ , c("Estimate", "LL", "UL", "SE", "effect_size_adjusted", "ta_LL", "ta_UL")]

  estimate$es_proportion_difference <- cbind(
    type = c("Comparison", "Reference", "Difference"),
    outcome_variable_name = outcome_variable_name,
    case_label = paste("P_", case_label, sep = ""),
    grouping_variable_name = grouping_variable_name,
    effect = contrast_labels,
    estimate$es_proportion_difference
  )

  for (frow in c(1, 2)) {
    if (estimate$es_proportion_difference$LL[frow] < 0) estimate$es_proportion_difference$LL[frow] <- 0
    if (estimate$es_proportion_difference$ta_LL[frow] < 0) estimate$es_proportion_difference$ta_LL[frow] <- 0
    if (estimate$es_proportion_difference$UL[frow] < 0) estimate$es_proportion_difference$UL[frow] <- 0
    if (estimate$es_proportion_difference$ta_UL[frow] < 0) estimate$es_proportion_difference$ta_UL[frow] <- 0

  }

  # Odds ratio?
  if (length(cases) < 20) {
    estimate$es_odds_ratio <- as.data.frame(
      statpsych::ci.oddsratio(
        alpha = 1 - conf_level,
        f00 = cases[1],
        f01 = ns[1] - cases[1],
        f10 = cases[2],
        f11 = ns[2] - cases[2]
      )
    )

    res_2a <- as.data.frame(
      statpsych::ci.oddsratio(
        alpha = (1 - conf_level)*2,
        f00 = cases[1],
        f01 = ns[1] - cases[1],
        f10 = cases[2],
        f11 = ns[2] - cases[2]
      )
    )

    estimate$es_odds_ratio$ta_LL <- res_2a$LL
    estimate$es_odds_ratio$ta_UL <- res_2a$UL

    estimate$es_odds_ratio <- cbind(
      outcome_variable_name = outcome_variable_name,
      case_label = paste("P_", case_label, sep = ""),
      grouping_variable_name = grouping_variable_name,
      effect = paste(contrast_labels[[1]], " / ", contrast_labels[[2]], sep = ""),
      effect_size = estimate$es_odds_ratio$Estimate,
      estimate$es_odds_ratio
    )
    estimate$es_odds_ratio$Estimate <- NULL
  }


  return(estimate)

}



estimate_pdiff_ind_contrast.summary <- function(
  cases,
  ns,
  contrast = NULL,
  case_label = "Affected",
  grouping_variable_levels = NULL,
  grouping_variable_name = "My grouping variable",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # cases - vector of numeric integer data >= 0 with >2 elements, no NAs
  # ns - vector of integers >= 1, of same length as cases, no NAs
  # grouping_variable_levels -
  #   if passed: vector of characters same length of cases
  #   if not passed: auto-generated (Group1, Group2, etc.) and warning issued

  # The base function will check:
  #  The validity of the contrast
  #  conf_level is >0 and <1
  #  grouping_variable_name - optional, non-zero length character
  #  outcome_variable_name - optional, non-zero length character


  warnings <- c(NULL)

  # Check means
  esci_assert_type(cases, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    cases,
    lower = 2,
    lower_inclusive = TRUE,
    na.invalid = TRUE
  )
  for (case_value in cases) {
    esci_assert_type(case_value, "is.numeric")
    esci_assert_type(case_value, "is.whole.number")
    esci_assert_range(
      case_value,
      lower = 0,
      lower_inclusive = TRUE
    )
  }
  cells <- row_report$total


  # Check ns
  esci_assert_type(ns, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    ns,
    lower = cells,
    upper = cells,
    lower_inclusive = TRUE,
    upper_inclusive = TRUE,
    na.invalid = TRUE
  )
  for (x in 1:length(ns)) {
    n_value <- ns[x]
    esci_assert_type(n_value, "is.numeric")
    esci_assert_type(n_value, "is.whole.number")
    esci_assert_range(
      n_value,
      lower = cases[x],
      lower_inclusive = TRUE
    )
  }

  # Set group labels
  if(!is.null(grouping_variable_levels)) {
    esci_assert_type(grouping_variable_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_levels,
      lower = cells,
      upper = cells,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_levels <- paste(
      "Group",
      seq(from = 1, to = cells, by = 1),
      sep = ""
    )
    glc <- paste(
      grouping_variable_levels,
      collapse = ", "
    )
    msg <- glue::glue("Group labels have been auto-generated: {glc}")
    warnings <- c(warnings, msg)
    warning(msg)
  }


  # Overview table-------------------------------------------------------------
  overview_table <- NULL
  for (x in 1:length(cases)) {
    overview_table <- rbind(
      overview_table,
      overview_nominal.summary(
        cases = c(cases[x], ns[x] - cases[x]),
        outcome_variable_levels = c(
          case_label,
          paste("Not ", case_label, sep = "")
        ),
        outcome_variable_name = outcome_variable_name,
        conf_level = conf_level
      )
    )
  }

  overview_table <- cbind(
    grouping_variable_name = grouping_variable_name,
    grouping_variable_level = rep(grouping_variable_levels, each = 2),
    overview_table
  )

  # Create a table of group means and CIs as well
  if (is.null(contrast)) {
    estimate <- list()
    class(estimate) <- "esci_estimate"
    estimate$properties <- list(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      contrast = NULL,
      conf_level = conf_level,
      data_type = "summary",
      data_source = NULL
    )

  } else {
    estimate <- estimate_pdiff_ind_contrast.base(
      cases = cases,
      ns = ns,
      contrast = contrast,
      case_label = case_label,
      grouping_variable_levels = grouping_variable_levels,
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      conf_level = conf_level
    )
  }

  estimate$overview <- overview_table

  estimate <- esci_build_chi_square(estimate)

  estimate$properties$data_type <- "summary"
  estimate$properties$data_source <- NULL
  estimate$warnings <- c(estimate$warnings, warnings)

  return(estimate)

}



estimate_pdiff_ind_contrast.vector <- function(
  grouping_variable,
  outcome_variable,
  contrast = NULL,
  case_label = 1,
  grouping_variable_name = "My grouping variable",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  count_NA = FALSE
) {

  # To do - improve checking on levels of grouping variable and

  # Input checks --------------------------------------------------------------

  # Check outcome variable
  esci_assert_type(outcome_variable, "is.factor")
  outcome_variable_report <- esci_assert_vector_valid_length(
    outcome_variable,
    lower = 2,
    lower_inclusive = FALSE)
  if (length(levels(as.factor(outcome_variable))) < 2) {
    stop("Not enough levels in outcome_variable")
  }

  # Check grouping variable
  esci_assert_type(grouping_variable, "is.factor")
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable,
    lower = 2,
    lower_inclusive = FALSE)
  if (length(levels(as.factor(grouping_variable))) < 2) {
    stop("Not enough levels in grouping_variable")
  }

  # Vectors are same length
  if(length(grouping_variable) != length(outcome_variable)) {
    # vectors not of same length!
    msg <- glue::glue("
The grouping_variable and outcome_variable are not the same length
The grouping_variable length is {length(grouping_variable)};
The outcome_variable length is {length(outcome_variable)}.
    ")
    stop(msg)
  }

  # Case label
  if (is.numeric(case_label)){
    if (case_label > length(levels(outcome_variable)) | case_label < 1) {
      msg <- glue::glue("
The case_label is {case_label}.
The number of levels in the outcome variable is {length(levels(outcome_variable))}.
case_label must be between 1 and {length(levels(outcome_variable))}.
    ")
      stop(msg)
    }
    case_label <- levels(outcome_variable)[case_label]
  } else {
    if (!case_label %in% levels(outcome_variable)) {
      msg <- glue::glue("
The case_label is {case_label}.
The levels in the outcome variable are {paste(levels(outcome_variable), collapse = ',')}.
case_label must be either a numeric or a valid level from outcome_variable.
    ")
      stop()
    }

  }


  # Do the analysis --------------------------------------------------
  # Create overview -- which will gracefully deal with missing and n=0 or n=1
  all_overview <- overview_nominal.vector(
    outcome_variable = outcome_variable,
    grouping_variable = grouping_variable,
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    count_NA = count_NA
  )

  message <- NULL
  message_html <- NULL
  if ("Missing" %in% all_overview$outcome_variable_level) {
    message <- if (count_NA)
      "Missing values were present; these were counted as part of the total sample size."
    else
      "Missing values were present; these were *not* counted as part of the total sample size."

    message_html <- if (count_NA)
      "Missing values were present; these were counted as part of the total sample size."
    else
      "Missing values were present; these were <b>not</b> counted as part of the total sample size."
  }

  # If no contrast, job is done
  if (is.null(contrast)) {
    estimate <- list()
    estimate$properties <- list(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      effect_size_category = "Simple",
      effect_size_name = "P",
      conf_level = conf_level,
      count_NA = count_NA
    )
    estimate$overview <- all_overview
    if (!is.null(message)) {
      estimate$overview_properties <- list()
      estimate$overview_properties$message <- message
      estimate$overview_properties$message_html <- message_html
    }
    class(estimate) <- "esci_estimate"
    return(estimate)
  }


  # From the overview function, get just the valid groups
  no_miss_overview <- all_overview[! "missing" %in% row.names(all_overview), ]

  just_case <- no_miss_overview[no_miss_overview$outcome_variable_level == case_label, ]

  overview <- no_miss_overview[just_case$n > 1, ]


  # Check the validity of the contrast ---------------------------------------
  ns <- just_case$n
  groups <- just_case$grouping_variable_level

  valid_groups <- groups[which(ns > 1)]
  invalid_groups <- groups[which(ns < 2)]
  invalid_ns <- ns[which(ns < 2)]

  if (is.null(names(contrast))) {
    if (length(valid_groups) != length(contrast)) {
      msg <- glue::glue("
Invalid contrast specified.
Contrast length is {length(contrast)}.
But number of valid groups is {length(valid_groups)}.
The contrast passed is: {paste(contrast, collapse = ', ')}.
The valid groups found are: {paste(valid_groups, collapse = ', ')}
Invalid groups are those with n < 2.
{length(invalid_groups)} invalid groups: {paste(invalid_groups, collapse = ', ')}
      ")
      stop(msg)
    }
  } else {
    if (prod(names(contrast) %in% valid_groups) != 1) {
      msg <- glue::glue("
Invalid contrast specified.
Contrast has named value that is not found in the valid groups.
The contrast passed is: {paste(names(contrast), '=', contrast, collapse = ', ')}.
The valid groups are: {paste(valid_groups, collapse = ', ')}
Invalid groups are those with n < 2.
{length(invalid_groups)} invalid groups: {paste(invalid_groups, collapse = ', ')}
      ")
      stop(msg)
    }
  }


  # Dispatch only valid groups to base function
  estimate <-estimate_pdiff_ind_contrast.base(
    cases = just_case$cases,
    ns = just_case$n,
    contrast = contrast,
    case_label = case_label,
    grouping_variable_levels = just_case$grouping_variable_level,
    grouping_variable_name = grouping_variable_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level
  )


  estimate$overview <- all_overview
  if (!is.null(message)) {
    estimate$overview_properties <- list()
    estimate$overview_properties$message <- message
    estimate$overview_properties$message_html <- message_html
  }
  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL
  estimate$es_proportion_difference_properties$message <- message
  estimate$es_proportion_difference_properties$message_html <- message_html


  # Raise warnings if needed ------------------------------------------------
  # Warning if all_overview has a row for missing grouping data
  if ("missing" %in% row.names(all_overview)) {
    n_miss <- all_overview[row.names(all_overview) == "missing", "n"]
    msg <-  glue::glue("
There are {n_miss} missing values in grouping variable {grouping_variable_name}.
Outcomes with missing grouping variables were **dropped** from the analysis
    ")
    estimate$warnings <- c(estimate$warnings, msg)
    warning(msg)
  }

  if(length(invalid_groups) > 0) {
    for (x in 1:length(invalid_groups)) {
      msg <-  glue::glue("
  The group {invalid_groups[[x]]} had an invalid sample size: {invalid_ns[[x]]}.
  This group was **dropped** from the analysis.
        ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)

    }
  }

  estimate <- esci_build_chi_square(estimate)


  return(estimate)
}



estimate_pdiff_ind_contrast.data.frame <- function(
  data,
  grouping_variable,
  outcome_variable,
  contrast = NULL,
  case_label = 1,
  conf_level = 0.95,
  count_NA = count_NA
) {


  # Input Checks -------------------------------------------------------------
  # This function expects:
  #   data to be a data frame
  #   grouping_variable to be a factor with more than 2 valid rows
  #   outcome_variable to be a numeric column in data, with more than 2 rows
  esci_assert_type(data, "is.data.frame")
  esci_assert_valid_column_name(data, grouping_variable)
  esci_assert_column_type(data, grouping_variable, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    grouping_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Validate this outcome variable
  esci_assert_valid_column_name(data, outcome_variable)
  esci_assert_column_type(data, outcome_variable, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Now pass along to the .vector version of this function
  estimate <- estimate_pdiff_ind_contrast.vector(
    grouping_variable = data[[grouping_variable]],
    outcome_variable = data[[outcome_variable]],
    contrast = contrast,
    case_label = case_label,
    grouping_variable_name = grouping_variable,
    outcome_variable_name = outcome_variable,
    conf_level = conf_level,
    count_NA = count_NA
  )

  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))

  return(estimate)

}


estimate_pdiff_ind_contrast.jamovi <- function(
  data,
  grouping_variable,
  outcome_variables,
  contrast = NULL,
  case_label = case_label,
  conf_level = 0.95,
  count_NA = count_NA
) {

  res <- list()

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.data-frame, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the .vector version of this function
    res[[outcome_variable]] <- estimate_pdiff_ind_contrast.data.frame(
      data = data,
      grouping_variable = grouping_variable,
      outcome_variable = outcome_variable,
      contrast = contrast,
      case_label = case_label,
      conf_level = conf_level,
      count_NA = count_NA
    )

  }

  res <- esci_estimate_consolidate(res)
  class(res) <- "esci_estimate"

  return(res)

}


esci_build_chi_square <- function(estimate) {

  o <- estimate$overview
  ctable <- NULL

  for (mylevel in unique(o$outcome_variable_level)) {
    ctable <- rbind(
      ctable,
      as.data.frame(t(o[o$outcome_variable_level == mylevel, "cases"]))
    )
  }
  colnames(ctable) <-  t(o[o$outcome_variable_level == mylevel, "grouping_variable_level"])
  rownames(ctable) <- unique(o$outcome_variable_level)

  estimate$properties$chi_square <- stats::chisq.test(ctable, correct = FALSE)

  if (all(dim(estimate$properties$chi_square$observed) == c(2,2))) {
    res <- statpsych::ci.phi(
      1 - estimate$properties$conf_level,
      estimate$properties$chi_square$observed[2, 1],
      estimate$properties$chi_square$observed[1, 1],
      estimate$properties$chi_square$observed[2, 2],
      estimate$properties$chi_square$observed[1, 2]
    )
    res <- as.data.frame(res)
    res <- cbind(
      data.frame(
        grouping_variable_name = estimate$overview$grouping_variable_name[[1]],
        outcome_variable_name = estimate$overview$outcome_variable_name[[1]],
        effect = paste(
          estimate$overview$grouping_variable_name[[1]],
          " and ",
          estimate$overview$outcome_variable_name[[1]],
          sep = ""
        ),
        effect_size = res$Estimate[[1]]
      ),
      res
    )
    res$Estimate <- NULL
    estimate$es_phi <- res
  }

  return(estimate)

}
