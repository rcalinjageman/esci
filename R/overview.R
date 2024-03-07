#' Calculates descriptive statistics for a continuous variable
#'
#'
#' @description
#' This function calculates basic descriptive statistics for a numerical
#' variable.  It can calculate an overall summary, or broken down by
#' the levels of a grouping variable.  Inputs can be summary data,
#' vectors, or a data frame.
#'
#'
#' @details
#' If equal variance is not assumed, each group is treated independently.  In
#' that case, the estimated mean, CI, and SE is from [statpsych::ci.mean1()],
#' and the estimated median, CI, and SE is from [statpsych::ci.median1()].  If
#' equal variance is assumed, each group CI is calculated as with respect to all
#' group data, using [statpsych::ci.lc.mean.bs()] and
#' [statpsych::ci.lc.median.bs()]
#'
#'
#' @param data - for raw data, a data frame or tibble
#' @param outcome_variable - for raw data, either a vector containing numerical
#'   data or the name of a data-frame column containing a factor
#' @param grouping_variable - optional; for raw data either a vector containing
#'   a factor or the name of a data frame column containing a factor
#' @param means For summary data - A vector of 1 or more numerical means
#' @param sds For summary data - A vector of standard deviations, same length as
#'   means
#' @param ns For summary data - A vector of sample sizes, same length as means
#' @param grouping_variable_levels For summary data - An optional vector of
#'   group labels, same length as means.  If not passed, auto-generated.
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My Outcome Variable'.  Ignored if a data-frame is passed,
#'   this argument is ignored.
#' @param grouping_variable_name Optional friendly name for the grouping
#'   variable.  If a data frame is passed, this argument is ignored.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param assume_equal_variance Defaults to FALSE
#'
#'
#' @return Returns a table of descriptive statistics
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
#'
#'
#' @examples
#' # example code
#' esci::overview(data_latimier_3groups, "Test%", "Group")
#'
#'
#' @export
overview <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  grouping_variable_name = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {
  # I wanted this function to be flexible for different inputs
  # But I also found use_method to be klunky and hard to follow.
  # So this function is a self-rolled dispatcher--it inspects the
  #   arguments passed and then dispatches to handler functions.
  # I guess time will tell if this is a reasonable thing to have done.

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(means)) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(grouping_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")
    if(!is.null(outcome_variable)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(means))  stop(
      "You have passed raw data,
      so don't pass the 'means' parameter used for summary data.")
    if(!is.null(sds))  stop(
      "You have passed raw data,
      so don't pass the 'sds' parameter used for summary data.")
    if(!is.null(ns))  stop(
      "You have passed raw data,
      so don't pass the 'ns' parameter used for summary data.")
    if(!is.null(grouping_variable_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_levels' parameter used for summary data.")

    # Check grouping_variable -- if it is an unquoted column name
    #  turn it into a string and store back to grouping_variable
    is_column_name <- try(grouping_variable, silent = TRUE)
    if(is(is_column_name, "try-error")) {
      grouping_variable_enquo <- rlang::enquo(grouping_variable)
      grouping_variable_enquo_name <- try(
        eval(rlang::as_name(grouping_variable_enquo)), silent = TRUE
      )
      if (!is(grouping_variable_enquo_name, "try-error")) {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        grouping_variable <- grouping_variable_enquo_name
      }
    }

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
      # At this stage, we know that outcome_variable was not a tidy column name,
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
    if (!is.null(grouping_variable)) {
      grouping_variable <- grouping_variable
    }
    return(
      overview.data.frame(
        data = data,
        grouping_variable = grouping_variable,
        outcome_variable = outcome_variable,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )
  } else if (analysis_type == "jamovi") {
    return(
      overview.jamovi(
        data = data,
        grouping_variable = grouping_variable,
        outcome_variables = outcome_variable,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )
  } else if (analysis_type == "summary") {
    return(
      overview.summary(
        means = means,
        sds = sds,
        ns = ns,
        grouping_variable_levels = grouping_variable_levels,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )
  } else if (analysis_type == "vector") {
    if (is.null(grouping_variable_name)) {
      grouping_variable_name <-  deparse(substitute(grouping_variable))
    }
    if (outcome_variable_name == "My Outcome Variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }
    return(
      overview.vector(
        grouping_variable = grouping_variable,
        outcome_variable = outcome_variable,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )
  }

  stop("Something went wrong dispatching this function")

}



# Takes an overview table and fills in mean CI, df, se, and MoE
# If assume_equal_variance, uses a pooled sd and total df for estimating means
# Otherwise, just uses group sd and df for estimating each group mean
# Uses CI_mdiff_contrast_bs as the base function for calculating CIs
overview.base <- function(
  overview_table,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # overview_table - has more than 1 row
  # conf_level should be a numeric value > 0 and <1
  # assume_equal_variance should be a logical value, TRUE or FALSE

  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )

  # Check assume_equal_variance
  esci_assert_type(assume_equal_variance, "is.logical")


  # Prep --------------------------------------------
  n_means <- nrow(overview_table)
  contrasts <- matrix(data = 0, nrow = n_means, ncol = n_means)
  diag(contrasts) <- 1

  statpsych_row <- if (assume_equal_variance | n_means == 1) 1 else 2
  statpsych_cnames <- c("LL", "UL", "df", "SE")
  esci_column_names <- c("mean_LL", "mean_UL", "df", "mean_SE")
  statpsych_cnames_mdn <- c("LL", "UL")
  esci_column_names_mdn <- c("median_LL", "median_UL")


  # Analysis ----------------------------------------
  if (n_means == 1) {
    res <- as.data.frame(
        statpsych::ci.mean1(
        alpha = 1 - conf_level,
        m = overview_table$mean,
        sd = overview_table$sd,
        n = overview_table$n
      )
    )

    res$df <- overview_table$n - 1

    overview_table[1, esci_column_names] <- res[1, statpsych_cnames]

  } else {

    for (x in 1:n_means) {
      res <- statpsych::ci.lc.mean.bs(
        alpha = 1 - conf_level,
        m = overview_table$mean,
        s = overview_table$sd,
        n = overview_table$n,
        v = contrasts[x,]
      )

      overview_table[x, esci_column_names] <- res[statpsych_row, statpsych_cnames]

      if (!is.null(overview_table$median_SE)) {
        res_median <- statpsych::ci.lc.median.bs(
          alpha = 1 - conf_level,
          m = overview_table$median,
          s = overview_table$median_SE,
          v = contrasts[x, ]
        )
        overview_table[x, esci_column_names_mdn ] <- res_median[1, statpsych_cnames_mdn]
      }

    }

  }

  return(overview_table)
}


# Produces an overview table from summary data
overview.summary <- function(
  means,
  sds,
  ns,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  grouping_variable_name = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # Input checks      ---------------------------------------------------------
  # This function expects:
  # means - vector of numeric data with >2 elements, no NAs
  # sds  - vector of numeric data >0 of same length as means, no NAs
  # ns - vector of integers >= 2, of same length as means, no NAs
  # outcome_variable_name -
  # grouping_variable_levels -
  #   if passed: vector of characters same length of means
  #   if not passed: auto-generated (Group1, Group2, etc.) and warning issued

  # The base function will check:
  #  conf_level is >0 and <1
  #  assume_equal_variance is logical

  # Check means
  esci_assert_type(means, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    means,
    lower = 1,
    lower_inclusive = TRUE,
    na.invalid = TRUE
  )
  cells <- row_report$total


  # Check sds
  esci_assert_type(sds, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    sds,
    lower = cells,
    upper = cells,
    lower_inclusive = TRUE,
    upper_inclusive = TRUE,
    na.invalid = TRUE
  )
  for (sd_value in sds) {
    esci_assert_type(sd_value, "is.numeric")
    esci_assert_range(sd_value,
                      lower = 0,
                      lower_inclusive = TRUE)
  }

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
  for (n_value in ns) {
    esci_assert_type(n_value, "is.numeric")
    esci_assert_type(n_value, "is.whole.number")
    esci_assert_range(n_value,
                      lower = 2,
                      lower_inclusive = TRUE)
  }

  # Check outcome_variable_name
  esci_assert_type(outcome_variable_name, "is.character")
  outcome_variable_names <- rep(outcome_variable_name, length(means))

  # Check grouping_variable_name
  if (!is.null(grouping_variable_name)) {
    esci_assert_type(grouping_variable_name, "is.character")
  } else {
    grouping_variable_name = "My Grouping Variable"
  }
  grouping_variable_names <- rep(grouping_variable_name, length(means))

  # Check grouping_variable_levels
  if(!is.null(grouping_variable_levels)) {
    esci_assert_type(grouping_variable_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_levels,
      lower = length(means),
      upper = length(means),
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
    levels_passed <- TRUE
  } else {
    grouping_variable_levels <- rep("All", length(means))
    levels_passed <- FALSE
  }


  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    grouping_variable_name = grouping_variable_names,
    grouping_variable_level = grouping_variable_levels,
    mean = means,
    mean_LL = NA,
    mean_UL = NA,
    sd = sds,
    n = ns,
    df = NA,
    mean_SE = NA
  )

  if (!levels_passed) overview_table$grouping_variable_name <- NULL
  if (!levels_passed) overview_table$grouping_variable_level <- NULL

  return(
    overview.base(
      overview_table = overview_table,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )
  )
}


# Produces an overview table from vector data
overview.vector <- function(
  outcome_variable,
  grouping_variable = NULL,
  outcome_variable_name = NULL,
  grouping_variable_name = NULL,
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {


  # Input checks ----------------------------
  if(is.null(outcome_variable_name)) {
    outcome_variable_name <- deparse(substitute(outcome_variable))
  } else {
    # Check that it is a character
    esci_assert_type(outcome_variable_name, "is.character")
  }

  # If no grouping variable, just summarize whole vector
  if (is.null(grouping_variable)) {
    grouping_variable <- rep("All", length(outcome_variable))
    grouping_variable_name <- "My Grouping Variable"
    group_passed <- FALSE
  } else {
    if(is.null(grouping_variable_name)) {
      grouping_variable_name <- deparse(substitute(grouping_variable))
    } else {
      esci_assert_type(grouping_variable_name, "is.character")
    }
    group_passed <- TRUE
  }

  # Deal with NA values in grouping variable------------------------------------
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable,
    lower = 2,
    lower_inclusive = FALSE)
  grouping_variable_all <-grouping_variable_report$total
  grouping_variable_valid <- grouping_variable_report$valid
  grouping_variable_missing <- grouping_variable_report$missing


  # Setup   ---------------------
  groups <- levels(addNA(grouping_variable))
  outcome_variable_names <- rep(outcome_variable_name, length(groups))
  grouping_variable_names <- rep(grouping_variable_name, length(groups))


  # Build the overview table ---------------------------------------------------
  overview_table <- data.frame(
    outcome_variable_name = outcome_variable_names,
    grouping_variable_name = grouping_variable_names,
    grouping_variable_level = groups
  )

  overview_table[ , c("mean", "mean_SE_temp", "mean_LL", "mean_UL")] <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = wrap_ci_mean1,
    drop = FALSE,
    conf_level = conf_level,
    na.rm = TRUE
  )[, 2]

  overview_table[ , c("median", "median_SE_temp", "median_LL", "median_UL")] <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = wrap_ci_median1,
    drop = FALSE,
    conf_level = conf_level,
    na.rm = TRUE
  )[, 2]

  overview_table$sd <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = sd,
    drop = FALSE,
    na.rm = TRUE)[, 2]

  overview_table$min <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = min,
    drop = FALSE,
    na.rm = TRUE)[, 2]

  overview_table$max <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = max,
    drop = FALSE,
    na.rm = TRUE)[, 2]

  overview_table[ , c("q1", "q3")] <-aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    FUN = quantile,
    drop = FALSE,
    probs = c(0.25, 0.75),
    na.rm = TRUE)[, 2]

  overview_table$n <- aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    drop = FALSE,
    FUN = length)[, 2]

  overview_table$missing <-aggregate(
    outcome_variable,
    by = list(addNA(grouping_variable)),
    drop = FALSE,
    function(x) { sum(is.na(x)) })[, 2]

  overview_table$n <- overview_table$n - overview_table$missing

  overview_table$df <- NA

  overview_table$mean_SE <- overview_table$mean_SE_temp
  overview_table$mean_SE_temp <- NULL

  overview_table$median_SE <- overview_table$median_SE_temp
  overview_table$median_SE_temp <- NULL

  if (!group_passed) overview_table$grouping_variable_name <- NULL
  if (!group_passed) overview_table$grouping_variable_level <- NULL

  # Cleanup - Deal with invalid rows and missing data rows-----------------

  if (nrow(overview_table[is.na(overview_table$n), ]) > 0) {
    overview_table[is.na(overview_table$n), ]$n <- 0
  }

  if (nrow(overview_table[is.na(overview_table$missing), ]) > 0) {
    overview_table[is.na(overview_table$missing), ]$missing <- 0
  }

  na_level <- NULL
  if(overview_table[nrow(overview_table), "n"] == 0 & overview_table[nrow(overview_table), "missing"] == 0) {
      overview_table <- head(overview_table, -1)
  } else {
    na_level <- "Missing"
    while (na_level %in% overview_table$group) {
      na_level <- paste(na_level, "*", sep ="")
    }
    overview_table[nrow(overview_table), "grouping_variable_level"] <- na_level
    row.names(overview_table)[nrow(overview_table)] <- "missing"
  }

  overview_no_miss <- overview_table[row.names(overview_table) != 'missing', ]
  overview_valid <- overview_no_miss[overview_no_miss$n > 1, ]

  overview_valid <- overview.base(
    overview_table = overview_valid,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  overview_all <- rbind(
    overview_valid,
    overview_table[row.names(overview_table) != "missing" & overview_table$n < 2, ],
    overview_table[row.names(overview_table) == "missing", ]
  )

  return(overview_all)

}

# Overview from a data frame
overview.data.frame <- function(
  data,
  outcome_variable,
  grouping_variable = NULL,
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {

  # Input Checks -------------------------------------------------------------
  # This function expects:
  #   data to be a data frame
  #   grouping_variable to be null a factor with more than 2 valid rows
  #   outcome_variable to be a numeric column in data, with more than 2 rows
  esci_assert_type(data, "is.data.frame")

  if (!is.null(grouping_variable)) {
    esci_assert_valid_column_name(data, grouping_variable)
    esci_assert_column_type(data, grouping_variable, "is.factor")
    esci_assert_column_has_valid_rows(
      data,
      grouping_variable,
      lower = 2,
      na.rm = TRUE
    )
  }

  # Validate this outcome variable
  esci_assert_valid_column_name(data, outcome_variable)
  esci_assert_column_type(data, outcome_variable, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  if (!is.null(grouping_variable)) {
    grouping_variable_name <- grouping_variable
    grouping_variable <- data[[grouping_variable]]
  } else {
    grouping_variable_name <- NULL
  }

  # Now pass along to the .vector version of this function
  overview_table <- overview.vector(
    outcome_variable = data[[outcome_variable]],
    grouping_variable = grouping_variable,
    outcome_variable_name = outcome_variable,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  return(overview_table)
}


# Overview from a data frame with a list of outcome variables
overview.jamovi <- function(
  data,
  outcome_variables,
  grouping_variable = NULL,
  conf_level = 0.95,
  assume_equal_variance = TRUE
) {

  res <- NULL

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.character, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the data_frame version
    res <- rbind(
      res,
      overview.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable = grouping_variable,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )

  } # Finish cycle through variables

  return(res)
}


wrap_ci_median1 <- function(
  x,
  conf_level,
  na.rm = TRUE,
  drop = FALSE
) {

  if(length(na.omit(x)) < 2) return(c(median(x), NA, NA, NA))

  res <- statpsych::ci.median1(
    alpha = 1 - conf_level,
    y = if(na.rm) x[!is.na(x)] else x
  )

  return(res)
}


wrap_ci_mean1 <- function(
  x,
  conf_level,
  na.rm = TRUE,
  drop = FALSE
) {

  if(length(x) < 2) return(c(mean(x), NA, NA, NA))

  res <- statpsych::ci.mean1(
    alpha = 1 - conf_level,
    m = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    n = if(na.rm) length(x[!is.na(x)]) else length(x)
  )

  return(res)
}
