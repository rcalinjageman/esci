#' Estimates for a multi-group design with a continuous outcome
#' variable
#'
#' @description
#' \loadmathjax
#' `estimate_mdiff_ind_contrast` is suitable for a multi-group design
#' (between subjects) with a continuous outcome variable.  It accepts
#' a user-defined set of contrast weights that allows estimation of any
#' 1-df contrast.  It can express estimates as mean differences, standardized
#' mean differences (Cohen's d) or median differences (raw data only).  You can
#' pass raw data or summary data.
#'
#'
#' @details
#' Reach for this function in place of a one-way ANOVA.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can test hypotheses with
#' [esci::test_mdiff()].
#'
#' The estimated mean differences are from [statpsych::ci.lc.mean.bs()].
#'
#' The estimated SMDs are from [esci::CI_smd_ind_contrast()] which relies
#' on [statpsych::ci.lc.stdmean.bs()] unless there are only 2 groups.
#'
#' The estimated median differences are from [statpsych::ci.lc.median.bs()]
#'
#'
#' @param data For raw data - a data frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param grouping_variable For raw data - The column name of the grouping
#'   variable, or a vector of group names
#' @param means For summary data - A vector of 2 or more means
#' @param sds For summary data - A vector of standard deviations, same length as
#'   means
#' @param ns For summary data - A vector of sample sizes, same length as means
#' @param contrast A vector of group weights, same length as number of groups.
#' @param grouping_variable_levels For summary data - An optional vector of
#'   group labels, same length as means
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
#'
#'
#' @return Returnsobject of class esci_estimate
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
#' data("data_halagappa")
#'
#' estimate <- estimate_mdiff_ind_contrast(
#'   means = data_halagappa$Mean,
#'   sds = data_halagappa$SD,
#'   ns = data_halagappa$n,
#'   grouping_variable_levels = as.character(data_halagappa$Groups),
#'   assume_equal_variance = TRUE,
#'   contrast = c(
#'     "NFree10" = 1/3,
#'     "AFree10" = 1/3,
#'     "ADiet10" = -1/3,
#'     "NFree17" = -1/3,
#'     "AFree17" = 1/3,
#'     "ADiet17" = -1/3
#'   ),
#'   grouping_variable_name = "Diet",
#'   outcome_variable_name = "% time near target"
#' )
#' estimate
#'
#' \dontrun{
#' # To visualize the estimated mean difference
#' plot_mdiff(estimate, effect_size = "mean")
#' }
#'
#'
#' @export
estimate_mdiff_ind_contrast <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  contrast = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {

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
      so don't pass the 'outcome_variable' parameter used for raw data.")

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
      estimate_mdiff_ind_contrast.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable =grouping_variable,
        contrast = contrast,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
      )
    )
  } else if (analysis_type == "jamovi") {
    return(
      estimate_mdiff_ind_contrast.jamovi(
        data = data,
        outcome_variables = outcome_variable,
        grouping_variable = grouping_variable,
        contrast = contrast,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
      )
    )


  } else if (analysis_type == "summary") {
    return(
      estimate_mdiff_ind_contrast.summary(
        means = means,
        sds = sds,
        ns = ns,
        contrast = contrast,
        grouping_variable_levels = grouping_variable_levels,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
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
      estimate_mdiff_ind_contrast.vector(
        grouping_variable = grouping_variable,
        outcome_variable = outcome_variable,
        contrast = contrast,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_name = grouping_variable_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
        )
    )
  }

  stop("Something went wrong dispatching this function")

}


# Handles construction of the effect_sizes and standardized_effect_sizes tables
estimate_mdiff_ind_contrast.base <- function(
  overview_table,
  contrast,
  grouping_variable_levels,
  grouping_variable_name,
  outcome_variable_name,
  conf_level = 0.95,
  assume_equal_variance = FALSE
) {

  # To do
  # Check lengths of outcome and grouping variable names

  # Input checks -------------------------
  # This is the base function for generating an estimated contrast
  # It expects:
  # grouping_variable_name - non-zero length character
  # outcome_variable_name - non-zero length character
  # conf_level should be a numeric value > 0 and <1
  # assume_equal_variance should be a logical value, TRUE or FALSE
  # contrast shold be a vector of numerics
  #   If named vector, names must match grouping_variable_levels
  #   Otherwise, vector must be same length as means

  # Should already be checked
  # means, sds, and ns should already be checked to be numerics
  #  of same length without NAs and with all ns > 1
  # grouping_variable_levels should be already be checked to be a vector
  #  of characters without NAs of same length as means

  means <- overview_table$mean
  sds <- overview_table$sd
  ns <- overview_table$n


  # Check contrast
  cells <- length(means)
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

  # Check assume_equal_variance
  esci_assert_type(assume_equal_variance, "is.logical")


  # Prep the contrast ---------------------
  names(means) <- grouping_variable_levels
  weights <- esci_tool_contrast_fixed(contrast, means)
  contrast_labels <- esci_tool_contrast_labels(weights)

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
  estimate$es_mean_difference_properties <- list(
    effect_size_name = "M_Diff",
    effect_size_name_html = "<i>M</i><sub>diff</sub>",
    effect_size_category = "difference",
    effect_size_precision = "magnitude",
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance,
    error_distribution = "t_dist"
  )
  class(estimate) <- "esci_estimate"


  # Now dispatch the contrast ----------------------------------------------
  es_mean_difference <- NULL
  es_median_difference <- NULL

  for (mycontrast in contrasts) {
    res <- as.data.frame(
      statpsych::ci.lc.mean.bs(
        alpha = 1 - conf_level,
        m = means,
        sd = sds,
        n = ns,
        v = mycontrast
      )
    )

    res <- res[if (assume_equal_variance) 1 else 2, ]

    res_2a <- as.data.frame(
      statpsych::ci.lc.mean.bs(
        alpha = (1 - conf_level)*2,
        m = means,
        sd = sds,
        n = ns,
        v = mycontrast
      )
    )

    res$ta_LL <- res_2a[if (assume_equal_variance) 1 else 2, "LL"]
    res$ta_UL <- res_2a[if (assume_equal_variance) 1 else 2, "UL"]

    es_mean_difference <- rbind(
      es_mean_difference,
      res
    )

    if (!is.null(overview_table$median)) {
      mdn_res <- statpsych::ci.lc.median.bs(
            alpha = 1 - conf_level,
            m = overview_table$median,
            se = overview_table$median_SE,
            v = mycontrast
      )

      mdn_2a <- statpsych::ci.lc.median.bs(
        alpha = (1 - conf_level)*2,
        m = overview_table$median,
        se = overview_table$median_SE,
        v = mycontrast
      )

      mdn_df <- as.data.frame(mdn_res)

      mdn_df$ta_LL <- mdn_2a[1, "LL"]
      mdn_df$ta_UL <- mdn_2a[1, "UL"]


      es_median_difference <- rbind(
        es_median_difference,
        mdn_df
      )

    } # end of processing median

  } # end of loop through 3 effect sizes


  estimate$es_mean_difference <- data.frame(matrix(NA, ncol=1, nrow=3))[-1]
  estimate$es_mean_difference[ , c("effect_size", "LL", "UL", "SE", "df", "ta_LL", "ta_UL")] <-
    es_mean_difference[ , c("Estimate", "LL", "UL", "SE", "df", "ta_LL", "ta_UL")]

  estimate$es_mean_difference <- cbind(
    type = c("Comparison", "Reference", "Difference"),
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    effect = contrast_labels,
    estimate$es_mean_difference
  )

  # Median difference
  if (!is.null(es_median_difference)) {
    estimate$es_median_difference <- data.frame(matrix(NA, ncol=1, nrow=3))[-1]
    estimate$es_median_difference[ , c("effect_size", "LL", "UL", "SE", "ta_LL", "ta_UL")] <-
      es_median_difference[ , c("Estimate", "LL", "UL", "SE", "ta_LL", "ta_UL")]

    estimate$es_median_difference <- cbind(
      type = c("Comparison", "Reference", "Difference"),
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      effect = contrast_labels,
      estimate$es_median_difference
    )

    estimate$es_median_difference_properties <- list(
      effect_size_name = "Mdn_Diff",
      effect_size_name_html = "<i>Mdn</i><sub>diff</sub>",
      effect_size_category = "difference",
      effect_size_precision = "magnitude",
      conf_level = conf_level,
      error_distribution = "norm"
    )
  }


  # SMD ----------------------------------------------------------
  smd_result <- CI_smd_ind_contrast(
    means = means,
    sds = sds,
    ns = ns,
    contrast = weights,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance,
    correct_bias = TRUE
  )

  estimate$es_smd_properties <- smd_result$properties
  smd_result$properties <- NULL
  smd_result <- list(list(effect = contrast_labels[[3]]), smd_result)

  estimate$es_smd <- as.data.frame(smd_result)

  estimate$es_smd <- cbind(
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    estimate$es_smd
  )


  return(estimate)

}


estimate_mdiff_ind_contrast.summary <- function(
  means,
  sds,
  ns,
  grouping_variable_levels = NULL,
  grouping_variable_name = "My grouping variable",
  outcome_variable_name = "My outcome variable",
  contrast = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # means - vector of numeric data with >2 elements, no NAs
  # sds  - vector of numeric data of same length as means, no NAs
  # ns - vector of integers >= 2, of same length as means, no NAs
  # grouping_variable_levels -
  #   if passed: vector of characters same length of means
  #   if not passed: auto-generated (Group1, Group2, etc.) and warning issued

  # The base function will check:
  #  The validity of the contrast
  #  conf_level is >0 and <1
  #  assume_equal_variance is logical
  #  grouping_variable_name - optional, non-zero length character
  #  outcome_variable_name - optional, non-zero length character


  warnings <- c(NULL)

  # Check means
  esci_assert_type(means, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    means,
    lower = 2,
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
  overview <- overview.summary(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_levels = grouping_variable_levels,
    grouping_variable_name = grouping_variable_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
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
    estimate <- estimate_mdiff_ind_contrast.base(
      overview_table = overview,
      grouping_variable_levels = grouping_variable_levels,
      grouping_variable_name = grouping_variable_name,
      outcome_variable_name = outcome_variable_name,
      contrast = contrast,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )
  }

  estimate$overview <- overview

  estimate$properties$data_type <- "summary"
  estimate$properties$data_source <- NULL
  estimate$warnings <- c(estimate$warnings, warnings)

  return(estimate)

}


estimate_mdiff_ind_contrast.vector <- function(
  grouping_variable,
  outcome_variable,
  contrast = NULL,
  grouping_variable_name = "My grouping variable",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {

  # To do - improve checking on levels of grouping variable and
  #  rows per level of outcome variable

  # Input checks --------------------------------------------------------------
  # This function expects:
  #   grouping_variable to be a factor:
  #      with >= 2 valid levels
  #  outcome_variable to be a vector of numeric data:
  #      with > 2 valid rows
  #      of same length as x
  #  save_raw_data is a logical, TRUE or FALSE

  # Check grouping variable
  esci_assert_type(grouping_variable, "is.factor")
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable,
    lower = 2,
    lower_inclusive = FALSE)
  if (length(levels(as.factor(grouping_variable))) < 2) {
    stop("Not enough levels in grouping_variable")
  }

  # Check outcome variable
  esci_assert_type(outcome_variable, "is.numeric")
  if(length(grouping_variable) != length(outcome_variable)) {
    # vectors not of same length!
    msg <- glue::glue("
The grouping_variable and outcome_variable are not the same length
The grouping_variable length is {length(grouping_variable)};
The outcome_variable length is {length(outcome_variable)}.
    ")
    stop(msg)
  }

  # Check save_raw_data
  esci_assert_type(save_raw_data, "is.logical")


  # Do the analysis --------------------------------------------------
  # Create overview -- which will gracefully deal with missing and n=0 or n=1
  all_overview <- overview.vector(
    grouping_variable = grouping_variable,
    outcome_variable = outcome_variable,
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  # From the overview function, get just the valid groups
  no_miss_overview <- all_overview[row.names(all_overview) != "missing", ]
  overview <- no_miss_overview[no_miss_overview$n > 1, ]

  # If no contrast, job is done
  if (is.null(contrast)) {
    estimate <- list()
    estimate$properties <- list(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = grouping_variable_name,
      effect_size_category = "Simple",
      effect_size_name = "M",
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )
    estimate$overview <- all_overview
    class(estimate) <- "esci_estimate"

    # Store raw data -----------------------------------------------
    if (save_raw_data) {
      # Revise all NAs
      if (row.names(overview)[nrow(overview)] == "missing") {
        # na_level was generated in overview to be unique
        na_level <- overview[nrow(overview), "group"]
        levels(grouping_variable) <- c(levels(grouping_variable), na_level)
        grouping_variable[which(is.na(grouping_variable))] <- na_level
      }

      estimate$raw_data <- data.frame(
        grouping_variable = grouping_variable,
        outcome_variable = outcome_variable
      )
    }


    return(estimate)
  }


  # Check the validity of the contrast ---------------------------------------
  ns <- no_miss_overview$n
  groups <- no_miss_overview$grouping_variable_level

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
  estimate <-estimate_mdiff_ind_contrast.base(
    overview_table = overview,
    grouping_variable_levels = overview$grouping_variable_level,
    grouping_variable_name = grouping_variable_name,
    outcome_variable_name = outcome_variable_name,
    contrast = contrast,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )


  if(length(contrast) == 2) {
    comp_level <- names(contrast[contrast > 0])
    ref_level <- names(contrast[contrast < 0])

    if(length(comp_level) > 0 & length(ref_level) > 0) {
      vec_comparison <- outcome_variable[which(grouping_variable == comp_level)]
      vec_reference <- outcome_variable[which(grouping_variable == ref_level)]
      vec_comparison <- vec_comparison[!is.na(vec_comparison)]
      vec_reference <- vec_reference[!is.na(vec_reference)]

      if(length(vec_comparison) > 0 & length(vec_reference) > 0) {
        estimate$es_mean_ratio <- as.data.frame(
            statpsych::ci.ratio.mean2(
              alpha = 1 - conf_level,
              y1 = vec_comparison,
              y2 = vec_reference
            )
        )

        no_negs <- (all(vec_comparison[!is.na(vec_comparison)] >= 0) & all(vec_reference[!is.na(vec_reference)] >= 0))

        estimate$es_mean_ratio_properties <- list(
          message_html = paste(
            if (no_negs) "" else "WARNING!  Your data has negative values.  ",
            "This effect-size measure is appropriate only for true ratio scales where values < 0 are impossible.
            For more information on this effect size, see Bonett & Price (2020) doi: 10.3102/1076998620934125.",
            sep = ""
          )
        )

        if (!no_negs) {
          estimate$warnings <- c(
            estimate$warnings,
            "neg_values" = "The ratio between group effect size is appropriate only for true ratio scales where values < 0 are impossible.  Your data has negative values and therefore any ratio between groups is invalid and should not be interpreted."
          )
        }


        estimate$es_median_ratio <- as.data.frame(
          statpsych::ci.ratio.median2(
            alpha = 1 - conf_level,
            y1 = vec_comparison,
            y2 = vec_reference
          )
        )

        estimate$es_median_ratio_properties <- estimate$es_mean_ratio_properties


        estimate$es_mean_ratio <- estimate$es_mean_ratio[ , c(3, 4, 5, 1, 2)]
        colnames(estimate$es_mean_ratio) <- c("effect_size", "LL", "UL", "comparison_mean", "reference_mean")

        estimate$es_median_ratio <- estimate$es_median_ratio[ , c(3, 4, 5, 1, 2)]
        colnames(estimate$es_median_ratio) <- c("effect_size", "LL", "UL", "comparison_median", "reference_median")

        clabel <- paste(
          comp_level,
          "/",
          ref_level
        )

        estimate$es_mean_ratio <- cbind(
          outcome_variable_name = outcome_variable_name,
          grouping_variable_name = grouping_variable_name,
          effect = clabel,
          estimate$es_mean_ratio
        )

        estimate$es_median_ratio <- cbind(
          outcome_variable_name = outcome_variable_name,
          grouping_variable_name = grouping_variable_name,
          effect = clabel,
          estimate$es_median_ratio
        )

      }
    }
  }

  estimate$overview <- all_overview
  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL


  # Raise warnings if needed ------------------------------------------------
  # Warning if all_overview has a row for missing grouping data
  if ("missing" %in% row.names(all_overview)) {
    n_miss <- all_overview[row.names(all_overview) == "missing", "n"]
    n_rows <- all_overview[row.names(all_overview) == "missing", "missing"]
    if (n_miss > 0 & n_rows > 0) {
      msg <-  glue::glue("
There are {n_rows} rows missing the outcome variable and grouping variable and {n_miss} rows missing only the grouping variable ({grouping_variable_name}).
Outcomes with missing grouping variables were **dropped** from the analysis
    ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)
    }
    if (n_miss > 0 & n_rows == 0) {
      msg <-  glue::glue("
There are {n_miss} missing values in grouping variable {grouping_variable_name}.
Outcomes with missing grouping variables were **dropped** from the analysis
    ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)
    }
    if (n_rows > 0 & n_miss == 0) {
      msg <-  glue::glue("
There are {n_rows} rows that were missing values for both the grouping variable ({grouping_variable_name}) and the outcome variable.
    ")
      estimate$warnings <- c(estimate$warnings, msg)
      warning(msg)
    }
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

  # Store raw data -----------------------------------------------
  if (save_raw_data) {
    # Revise all NAs
    if (row.names(overview)[nrow(overview)] == "missing") {
      # na_level was generated in overview to be unique
      na_level <- overview[nrow(overview), "group"]
      levels(grouping_variable) <- c(levels(grouping_variable), na_level)
      grouping_variable[which(is.na(grouping_variable))] <- na_level
    }

    estimate$raw_data <- data.frame(
      grouping_variable = grouping_variable,
      outcome_variable = outcome_variable
    )
  }

  return(estimate)
}


estimate_mdiff_ind_contrast.data.frame <- function(
  data,
  grouping_variable,
  outcome_variable,
  contrast = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
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
  esci_assert_column_type(data, outcome_variable, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Now pass along to the .vector version of this function
  estimate <- estimate_mdiff_ind_contrast.vector(
    grouping_variable = data[[grouping_variable]],
    outcome_variable = data[[outcome_variable]],
    grouping_variable_name = grouping_variable,
    outcome_variable_name = outcome_variable,
    contrast = contrast,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance,
    save_raw_data = save_raw_data
  )

  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))

  return(estimate)

}


estimate_mdiff_ind_contrast.jamovi <- function(
  data,
  grouping_variable,
  outcome_variables,
  contrast = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
) {

  res <- list()

  # Cycle through the list of columns;
  #  for each call estimate_mean_one.data-frame, which handles 1 column
  for (outcome_variable in outcome_variables) {

    # Now pass along to the .vector version of this function
    res[[outcome_variable]] <- estimate_mdiff_ind_contrast.data.frame(
      data = data,
      grouping_variable = grouping_variable,
      outcome_variable = outcome_variable,
      contrast = contrast,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance,
      save_raw_data = save_raw_data
    )

  }

  res <- esci_estimate_consolidate(res)
  class(res) <- "esci_estimate"

  return(res)

}


## Tools for contrasts

# User can input contrast as simple vector as same length as means
#   e.g. contrast <- c(-1/2, -1/2, 0, 1)
# Or as a named vector, in which case, they need only name the means
#   involved in the contrast
#  e.g. contrast <- c("Group 1" = -1/2, "Group 2" = -1/2, "Group 4" = 1)
# This function takes either case and returns a contrast that has a
#   proper weight for each mean.
# For the named approach, any group not names gets a weight of 0
#
# IMPORTANT: The means vector must be named
esci_tool_contrast_fixed <- function(contrast, means) {

  # If names were not passed, set to be the same names as the means
  if (is.null(names(contrast)) & (length(contrast) == length(means))) {
    names(contrast) <- names(means)
  }

  # Now, we'll initialize a vector of weights, all 0, same names as means
  weights <- rep(0, times = length(means))
  names(weights) <- names(means)

  # For each value in contrast, assign it to weight by matching mean name
  for (myindex in 1:length(names(contrast))) {
    weights[[names(contrast)[[myindex]]]] <- contrast[[myindex]]
  }

  return(weights)

}


# Takes a vector of named weights and creates labels
# for the comparison group (all weights that are positive)
# and the reference group (all weights that are negative)
# and the difference (comparison - reference)
# Group names are separated by an and
# And when more than 1 group in a subset, wrapped in ()
esci_tool_contrast_labels <- function(contrast) {
  # Create a label for the contrast
  # Get and-separated list of group names for comparison and ref groups
  comparison_label <- paste0(
    names(contrast)[which(contrast > 0)],
    collapse = " and ")
  reference_label <- paste0(
    names(contrast)[which(contrast < 0)],
    collapse = " and ")
  # Wrap in () if more than 1 group combined
  if (length(which(contrast > 0)) > 1) {
    comparison_label <- paste("(", comparison_label, ")", sep = "")
  }
  if (length(which(contrast < 0)) > 1) {
    reference_label <- paste("(", reference_label, ")", sep = "")
  }
  contrast_label <- paste(comparison_label, " \U2012 ", reference_label, sep = "")

  return(c(comparison_label, reference_label, contrast_label))

}
