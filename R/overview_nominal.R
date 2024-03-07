#' Calculates descriptive statistics for a numerical variable
#'
#'
#' @description
#' This function calculated basic descriptive statistics for a categorical/
#' nominal variable. Inputs can be summary data, vectors, or a data frame.
#'
#'
#' @param data - for raw data, a data frame or tibble
#' @param outcome_variable - for raw data, either a vector containing factor
#'   data or the name of a data-frame column containing a factor
#' @param grouping_variable - for raw data, either NULL (default), or
#'   the vector of a factor or a data-frame column containing a factor
#' @param cases For summary data - A vector of 1 or more counts, integers>0
#' @param outcome_variable_levels For summary data - An optional vector of
#'   group labels, same length as cases.  If not passed, auto-generated.
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My Outcome Variable'.  Ignored if a data-frame is passed,
#'   this argument is ignored.
#' @param grouping_variable_name Optional friendly name for the grouping variable.
#'   Defaults to 'My Grouping Variable'.  Ignored for summary data and for
#'   data frames -- only used if vectors of data are passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param count_NA Logical to count NAs (TRUE) in total N or not (FALSE)
#'
#'
#' @return Returns a table of descriptive statistics
#' - **overview_nominal**
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
#'
#'
#' @examples
#' # example code
#' esci::overview_nominal(esci::data_latimier_3groups, "Group")
#'
#' @export
overview_nominal <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  cases = NULL,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  grouping_variable_name = "My Grouping Variable",
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
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(cases))  stop(
      "You have passed raw data,
      so don't pass the 'cases' parameter used for summary data.")
    if(!is.null(outcome_variable_levels))  stop(
      "You have passed raw data,
      so don't pass the 'outcome_variable_levels' parameter used for summary data.")

    if(is.null(data)) {
      analysis_type <- "vector"
    } else {
      # data parameter has been passed


      # outcome variable
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

      # grouping_variable
      is_char <- try(
        is.character(grouping_variable), silent = TRUE
      )
      if (is(is_char, "try-error")) {
        grouping_variable_enquo <- rlang::enquo(grouping_variable)
        grouping_variable_quoname <- try(
          eval(rlang::as_name(grouping_variable_enquo)), silent = TRUE
        )
        if (!is(grouping_variable_quoname, "try-error")) {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          grouping_variable <- grouping_variable_quoname
        } else {
          grouping_variable <- NULL
        }
      }

      # Ok, must have been string column names
      if (length(outcome_variable) == 1) {
        analysis_type <- "data.frame"
      } else {
        analysis_type <- "jamovi"
      }

    }  # end else where data is not null

  }


  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if(analysis_type == "data.frame") {
    return(
      overview_nominal.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  } else if (analysis_type == "jamovi") {
    return(
      overview_nominal.jamovi(
        data = data,
        outcome_variables = outcome_variable,
        grouping_variable = grouping_variable,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  } else if (analysis_type == "summary") {
    return(
      overview_nominal.summary(
        cases = cases,
        outcome_variable_levels = outcome_variable_levels,
        outcome_variable_name = outcome_variable_name,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {
    if (outcome_variable_name == "My Outcome Variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }
    if (outcome_variable_name == "My Grouping Variable") {
      grouping_variable_name <- deparse(substitute(grouping_variable))
    }
    return(
      overview_nominal.vector(
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )
  }

  stop("Something went wrong dispatching this function")

}



# Takes an overview table and fills in mean CI, df, se, and MoE
# If assume_equal_variance, uses a pooled sd and total df for estimating means
# Otherwise, just uses group sd and df for estimating each group mean
# Uses CI_mdiff_contrast_bs as the base function for calculating CIs
overview_nominal.base <- function(
  overview_table,
  conf_level = 0.95
) {

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # overview_table - has more than 1 row
  # conf_level should be a numeric value > 0 and <1

  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )


  # Prep --------------------------------------------
  n_cells <- nrow(overview_table)

  statpsych_row <- 1
  statpsych_cnames <- c("Estimate", "LL", "UL", "SE")
  esci_column_names <- c("P", "P_LL", "P_UL", "P_SE")


  # Analysis ----------------------------------------
  for (x in 1:n_cells) {
    if (overview_table$n[x] == 0 | is.na(overview_table$n[x])) {
      res <- data.frame(
        "Estimate" = NA,
        "LL" = NA,
        "UL" = NA,
        "SE" = NA
      )
      res_ta <- res
    } else {
      res <- as.data.frame(
        statpsych::ci.prop1(
          alpha = 1 - conf_level,
          f = overview_table$cases[x],
          n = overview_table$n[x]
        )
      )

      res_ta <- as.data.frame(
        statpsych::ci.prop1(
          alpha = (1 - conf_level)*2,
          f = overview_table$cases[x],
          n = overview_table$n[x]
        )
      )
    }

    effect_size_adjusted <- res$Estimate[1]
    res$Estimate[1] <- res$Estimate[2]
    res_ta$Estimate[1] <- res_ta$Estimate[2]

    overview_table[x, esci_column_names] <- res[statpsych_row, statpsych_cnames]
    overview_table[x, "P_adjusted"] <- effect_size_adjusted
    overview_table[x, "ta_LL"] <- res_ta[statpsych_row, "LL"]
    overview_table[x, "ta_UL"] <- res_ta[statpsych_row, "UL"]

  }

  return(overview_table)
}


# Produces an overview table from summary data
overview_nominal.summary <- function(
  cases,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  conf_level = 0.95
) {

  # Input checks      ---------------------------------------------------------
  # This function expects:
  # cases - vector of 2 or more positive integers, no NAs
  # outcome_variable_name -
  # outcome_variable_levels -
  #   if passed: vector of characters same length of cases
  #   if not passed: auto-generated (Group1, Group2, etc.) and warning issued

  # The base function will check:
  #  conf_level is >0 and <1

  # Check cases
  esci_assert_type(cases, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    cases,
    lower = 2,
    lower_inclusive = TRUE,
    na.invalid = TRUE
  )
  for (count in cases) {
    esci_assert_type(count, "is.whole.number")
    esci_assert_range(
      count,
      lower = 0,
      lower_inclusive = TRUE
    )
  }
  cells <- row_report$total


  # Check outcome_variable_name
  esci_assert_type(outcome_variable_name, "is.character")
  outcome_variable_names <- rep(outcome_variable_name, cells)

  # Check outcome_variable_levels
  if(!is.null(outcome_variable_levels)) {
    esci_assert_type(outcome_variable_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      outcome_variable_levels,
      lower = cells,
      upper = cells,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
    levels_passed <- TRUE
  } else {
    outcome_variable_levels <- paste("Level", c(1:cells), sep = "")
    levels_passed <- FALSE
  }


  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    outcome_variable_level = outcome_variable_levels,
    cases = cases,
    n = sum(cases),
    P = NA,
    P_LL = NA,
    P_UL = NA,
    P_SE = NA
  )


  return(
    overview_nominal.base(
      overview_table = overview_table,
      conf_level = conf_level
    )
  )
}


# Produces an overview table from vector data
overview_nominal.vector <- function(
  outcome_variable,
  grouping_variable = NULL,
  outcome_variable_name = NULL,
  grouping_variable_name = NULL,
  conf_level = 0.95,
  count_NA = FALSE
) {

  # Expectations:
  # * outcome_variable is a factor with at least 1 level and at least 1 row
  # * outcome_variable_name is a character, or will filled from outcome_variable
  # * if grouping_variable is passed, a factor with at least 1 level and 1 row
  # * if grouping_variable passed, grouping_variable_name should be a character
  #    or is filled with grouping_variable

  # Input checks ----------------------------
  esci_assert_type(
    outcome_variable,
    "is.factor"
  )
  esci_assert_vector_valid_length(
    outcome_variable,
    lower = 1,
    lower_inclusive = TRUE,
    na.rm = TRUE,
    na.invalid = FALSE
  )
  # Check outcome_variable_name
  if(is.null(outcome_variable_name)) {
    outcome_variable_name <- deparse(substitute(outcome_variable))
  } else {
    # Check that it is a character
    esci_assert_type(outcome_variable_name, "is.character")
  }

  if(!is.null(grouping_variable)) {
    esci_assert_type(
      grouping_variable,
      "is.factor"
    )
    esci_assert_vector_valid_length(
      grouping_variable,
      lower = 1,
      lower_inclusive = TRUE,
      na.rm = TRUE,
      na.invalid = FALSE
    )
    # Check outcome_variable_name
    if(is.null(grouping_variable_name)) {
      grouping_variable_name <- deparse(substitute(grouping_variable))
    } else {
      # Check that it is a character
      esci_assert_type(grouping_variable_name, "is.character")
    }
  }


  if (!is.null(grouping_variable)) {
    overview_table <- NULL

    gv_has_na <- FALSE

    if (anyNA(grouping_variable)) {
      gv_has_na <- TRUE
      missing_level <- "Missing"
      while (missing_level %in% levels(grouping_variable)) {
        missing_level <- paste(missing_level, "*", sep = "")
      }
      levels(grouping_variable) <- c(levels(grouping_variable), missing_level)
      grouping_variable[which(is.na(grouping_variable))] <- missing_level
    }

    for (mylevel in levels(grouping_variable)) {
      this_level <- overview_nominal.vector(
        outcome_variable = outcome_variable[which(grouping_variable == mylevel)],
        grouping_variable = NULL,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = NULL,
        conf_level = conf_level,
        count_NA = count_NA
      )

      this_level <- cbind(
        grouping_variable_name = grouping_variable_name,
        grouping_variable_level = mylevel,
        this_level
      )

      overview_table <- rbind(
        overview_table,
        this_level
      )

    }

    if (gv_has_na) {
      missing_rows <- which(overview_table$grouping_variable_level == missing_level)
      row.names(overview_table)[missing_rows] <- paste("missing", missing_rows, sep = " ")
    }


    return(overview_table)
  }


  # Setup   ---------------------
  groups <- addNA(levels(as.factor(outcome_variable)))
  outcome_variable_names <- rep(outcome_variable_name, length(groups)+1)


  # Build the overview table ---------------------------------------------------
  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    outcome_variable_level = c(as.character(groups), "Missing")
  )

  overview_table$cases <- aggregate(
    outcome_variable,
    by = list(addNA(outcome_variable)),
    drop = FALSE,
    FUN = length)[, 2]

  cases <- overview_table$cases
  cases[is.na(cases)] <- 0
  overview_table$cases <- cases

  na_count <- if (count_NA)
    0
  else
    overview_table$cases[nrow(overview_table)]

  overview_table$n <- sum(overview_table$cases,na.rm = TRUE) - na_count

  if (overview_table$cases[nrow(overview_table)] == 0) {
    overview_table <- head(overview_table, -1)
  } else {
    if (!count_NA) overview_table$n[nrow(overview_table)] <- NA
  }

  overview_table$P <- NA
  overview_table$P_LL <- NA
  overview_table$P_UL <- NA

  # Cleanup - Deal with invalid rows and missing data rows-----------------
  overview_table <- overview_nominal.base(
    overview_table = overview_table,
    conf_level = conf_level
  )

  return(overview_table)

}

# Overview from a data frame
overview_nominal.data.frame <- function(
  data,
  outcome_variable,
  grouping_variable = NULL,
  conf_level = 0.95,
  count_NA = FALSE
) {

  # Input Checks -------------------------------------------------------------
  # This function expects:
  #   data to be a data frame
  #   grouping_variable to be null a factor with more than 2 valid rows
  #   outcome_variable to be a factor, with more than 2 rows
  # data frame
  esci_assert_type(data, "is.data.frame")

  # outcome_variable
  esci_assert_valid_column_name(data, outcome_variable)
  esci_assert_column_type(data, outcome_variable, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 1,
    lower_inclusive = TRUE,
    na.rm = TRUE
  )

  # grouping_variable
  if(!is.null(grouping_variable)) {
    esci_assert_valid_column_name(data, grouping_variable)
    esci_assert_column_type(data, grouping_variable, "is.factor")
    esci_assert_column_has_valid_rows(
      data,
      grouping_variable,
      lower = 2,
      na.rm = TRUE
    )
  }

  # Now pass along to the .vector version of this function
  grouping_variable_vector <- if(is.null(grouping_variable))
    NULL
  else
    data[[grouping_variable]]

  grouping_variable_name <- if(is.null(grouping_variable))
    NULL
  else
    grouping_variable


  overview_table <- overview_nominal.vector(
    outcome_variable = data[[outcome_variable]],
    grouping_variable = grouping_variable_vector,
    outcome_variable_name = outcome_variable,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    count_NA = count_NA
  )


  return(overview_table)
}


# Overview from a data frame with a list of outcome variables
overview_nominal.jamovi <- function(
  data,
  outcome_variables,
  grouping_variable,
  conf_level = 0.95,
  count_NA = FALSE
) {

  res <- NULL

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.character, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the data_frame version
    res <- rbind(
      res,
      overview_nominal.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        conf_level = conf_level,
        count_NA = count_NA
      )
    )

  } # Finish cycle through variables

  return(res)
}

