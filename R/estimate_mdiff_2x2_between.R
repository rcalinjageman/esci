#' Estimates for a 2x2 between-subjects design with a continuous outcome
#' variable
#'
#' @description
#' \loadmathjax
#' `estimate_mdiff_2x2_between` is suitable for a 2x2 between-subjects design
#' with a continuous outcome variable.  It estimates each main effect, the
#' simple effects for the first factor, and the interaction.  It can express
#' these estimates as mean differences, standardized mean differences (Cohen's
#' d), and as median differences (raw data only).  You can pass raw data or
#' or summary data (summary data does not return medians).
#'
#' @details
#' Reach for this function in place of a 2x2 between-subjects ANOVA.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can visualize the interaction
#' specifically with [esci::plot_interaction()].  You can test hypotheses
#' with [esci::test_mdiff()].
#'
#'
#' The estimated mean differences are from [statpsych::ci.2x2.mean.bs()].
#'
#' The estimated SMDs are from [statpsych::ci.2x2.stdmean.bs()].
#'
#' The estimated median differences are from [statpsych::ci.2x2.median.bs()]
#'
#'
#' @param data For raw data - a data frame or tibble
#' @param outcome_variable For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param grouping_variable_A For raw data - The column name of the grouping
#'   variable, or a vector of group names, only 2 levels allowed
#' @param grouping_variable_B For raw data - The column name of the grouping
#'   variable, or a vector of group names, only 2 levels allowed
#' @param means For summary data - A vector of 4 means: A1B1, A1B2, A2B1, A2B2
#' @param sds For summary data - A vector of 4 standard deviations, same order
#' @param ns For summary data - A vector of 4 sample sizes
#' @param grouping_variable_A_levels For summary data - An optional vector of
#'   2 group labels
#' @param grouping_variable_B_levels For summary data - An optional vector of
#'   2 group labels
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param grouping_variable_A_name Optional friendly name for the grouping
#'   variable.  Defaults to 'A' or the grouping variable
#'   column name if a data.frame is passed.
#' @param grouping_variable_B_name Optional friendly name for the grouping
#'   variable.  Defaults to 'A' or the grouping variable
#'   column name if a data.frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param assume_equal_variance Defaults to FALSE
#' @param save_raw_data For raw data; defaults to TRUE; set to FALSE to save
#'   memory by not returning raw data in estimate object
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
#'     - *effect_type* -
#'     - *effects_complex* -
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
#'     - *effect_type* -
#'     - *effects_complex* -
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
#'     - *effect_type* -
#'     - *effects_complex* -
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
#'     - *grouping_variable_A* -
#'     - *grouping_variable_B* -
#'
#'
#' @examples
#' # From summary data
#' means <- c(1.5, 1.14, 1.38, 2.22)
#' sds <- c(1.38, .96,1.5, 1.68)
#' ns <- c(26, 26, 25, 26)
#' grouping_variable_A_levels <- c("Evening", "Morning")
#' grouping_variable_B_levels <- c("Sleep", "No Sleep")
#'
#' estimates <- estimate_mdiff_2x2_between(
#'   means = means,
#'   sds = sds,
#'   ns = ns,
#'   grouping_variable_A_levels = grouping_variable_A_levels,
#'   grouping_variable_B_levels = grouping_variable_B_levels,
#'   grouping_variable_A_name = "Testing Time",
#'   grouping_variable_B_name = "Rest",
#'   outcome_variable_name = "False Memory Score",
#'   assume_equal_variance = TRUE
#' )
#'
#' \dontrun{
#' # To visualize the estimated mean difference
#' plot_mdiff(estimate, effect_size = "mean")
#' }
#'
#'
#' @export
estimate_mdiff_2x2_between <- function(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable_A = NULL,
  grouping_variable_B = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  grouping_variable_A_levels = NULL,
  grouping_variable_B_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_A_name = "A",
  grouping_variable_B_name = "A",
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
    if(!is.null(grouping_variable_A)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable_A' parameter used for raw data.")
    if(!is.null(grouping_variable_B)) stop(
      "You have passed summary statistics,
      so don't pass the 'grouping_variable_B' parameter used for raw data.")
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
    if(!is.null(grouping_variable_A_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_A_levels' parameter used for summary data.")
    if(!is.null(grouping_variable_B_levels))  stop(
      "You have passed raw data,
      so don't pass the 'grouping_variable_B_levels' parameter used for summary data.")

    # Check grouping_variable -- if it is an unquoted column name
    #  turn it into a string and store back to grouping_variable
    is_column_name <- try(grouping_variable_A, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      grouping_variable_A_enquo <- rlang::enquo(grouping_variable_A)
      grouping_variable_A_enquo_name <- try(
        eval(rlang::as_name(grouping_variable_A_enquo)), silent = TRUE
      )
      if (class(grouping_variable_A_enquo_name) != "try-error") {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        grouping_variable_A <- grouping_variable_A_enquo_name
      }
    }


    is_column_name <- try(grouping_variable_B, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      grouping_variable_B_enquo <- rlang::enquo(grouping_variable_B)
      grouping_variable_B_enquo_name <- try(
        eval(rlang::as_name(grouping_variable_B_enquo)), silent = TRUE
      )
      if (class(grouping_variable_B_enquo_name) != "try-error") {
        # This only succeeds if the columns were passed unquoted
        # So now replace grouping_variable with a quoted version
        grouping_variable_B <- grouping_variable_B_enquo_name
      }
    }

    # Now we have to figure out what type of raw data:
    #   could be tidy column names, string column names, or vectors
    # We check to see if we have a tidy column name by trying to evaluate it
    is_column_name <- try(outcome_variable, silent = TRUE)
    if(class(is_column_name) == "try-error") {
      # Column names have been passed, check if need to be quoted up
      outcome_variable_enquo <- rlang::enquo(outcome_variable)
      outcome_variable_quoname <- try(
        eval(rlang::as_name(outcome_variable_enquo)), silent = TRUE
      )
      if (class(outcome_variable_quoname) != "try-error") {
        # This only succeeds if outcome_variable was passed unquoted
        # Reset outcome_variable to be fully quoted
        outcome_variable <- outcome_variable_quoname
      }

      # Ready to be analyzed as a list of string column names
      analysis_type <- "data.frame"

    } else if (class(outcome_variable) == "numeric") {
      # At this stage, we know that y was not a tidy column name,
      #  so it should be either a vector of raw data (class = numeric)
      #  or a vector of column names passed as strings
      analysis_type <- "vector"
    } else if (class(outcome_variable) == "character") {
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
      estimate_mdiff_2x2_between.data.frame(
        data = data,
        outcome_variable = outcome_variable,
        grouping_variable_A = grouping_variable_A,
        grouping_variable_B = grouping_variable_B,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
      )
    )
  } else if (analysis_type == "summary") {
    return(
      estimate_mdiff_2x2_between.summary(
        means = means,
        sds = sds,
        ns = ns,
        grouping_variable_A_levels = grouping_variable_A_levels,
        grouping_variable_B_levels = grouping_variable_B_levels,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_A_name = grouping_variable_A_name,
        grouping_variable_B_name = grouping_variable_B_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance
      )
    )
  } else if (analysis_type == "vector") {
    if (is.null(grouping_variable_A_name) | grouping_variable_A_name == "A") {
      grouping_variable_A_name <-  deparse(substitute(grouping_variable_A))
    }
    if (is.null(grouping_variable_B_name) | grouping_variable_B_name == "B") {
      grouping_variable_B_name <-  deparse(substitute(grouping_variable_B))
    }
    if (outcome_variable_name == "My outcome variable") {
      outcome_variable_name <- deparse(substitute(outcome_variable))
    }

    return(
      estimate_mdiff_2x2_between.vector(
        grouping_variable_A = grouping_variable_A,
        grouping_variable_B = grouping_variable_B,
        outcome_variable = outcome_variable,
        outcome_variable_name = outcome_variable_name,
        grouping_variable_A_name = grouping_variable_A_name,
        grouping_variable_B_name = grouping_variable_B_name,
        conf_level = conf_level,
        assume_equal_variance = assume_equal_variance,
        save_raw_data = save_raw_data
        )
    )
  }

  stop("Something went wrong dispatching this function")

}


# Handles construction of the effect_sizes and standardized_effect_sizes tables
estimate_mdiff_2x2_between.base <- function(
  overview_table,
  grouping_variable_A_levels,
  grouping_variable_B_levels,
  grouping_variable_A_name,
  grouping_variable_B_name,
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


  # Check variable names
  esci_assert_type(grouping_variable_A_name, "is.character")
  esci_assert_type(grouping_variable_B_name, "is.character")
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
  all_contrasts <- list(
    main_effect_A = c(-1/2, -1/2, 1/2, 1/2),
    main_effect_B = c(-1/2, 1/2, -1/2, 1/2),
    simple_effect_B_at_A2 = c(0, 0, -1, 1),
    simple_effect_B_at_A1 = c(-1, 1, 0, 0),
    interaction = c(1, -1, -1, 1)
  )

  all_grouping_variable_names <- c(
    grouping_variable_A_name,
    grouping_variable_B_name,
    grouping_variable_B_name,
    grouping_variable_B_name,
    "Interaction"
  )

  all_contrast_labels <-list(
    c(grouping_variable_A_levels[[2]], grouping_variable_A_levels[[1]], "Difference"),
    c(grouping_variable_B_levels[[2]], grouping_variable_B_levels[[1]], "Difference"),
    c(grouping_variable_B_levels[[2]], grouping_variable_B_levels[[1]], "Difference"),
    c(grouping_variable_B_levels[[2]], grouping_variable_B_levels[[1]], "Difference"),
    c(
      paste("Simple effect of", grouping_variable_B_name, "at", grouping_variable_A_levels[[2]]),
      paste("Simple effect of", grouping_variable_B_name, "at", grouping_variable_A_levels[[1]]),
      paste("Difference of differences")
    )
  )

  effect_types <- c(
    paste("Main effect of ", grouping_variable_A_name, sep = ""),
    paste("Main effect of ", grouping_variable_B_name, sep = ""),
    paste("Simple effect of ", grouping_variable_B_name, " at ", grouping_variable_A_name, ": ", grouping_variable_A_levels[[2]], sep = ""),
    paste("Simple effect of ", grouping_variable_B_name, " at ",  grouping_variable_A_name, ": ", grouping_variable_A_levels[[1]], sep = ""),
    paste("Interaction of ", grouping_variable_A_name, " and ", grouping_variable_B_name, sep = "")
  )


  b_effects <-  c(
    grouping_variable_B_levels[[2]],
    grouping_variable_B_levels[[1]],
    paste(grouping_variable_B_levels[[2]], "\U2012", grouping_variable_B_levels[[1]])
  )

  effects_complex <- list(
    c(
      grouping_variable_A_levels[[2]],
      grouping_variable_A_levels[[1]],
      paste(grouping_variable_A_levels[[2]], "\U2012", grouping_variable_A_levels[[1]])
    ),
    b_effects,
    paste(
      paste(grouping_variable_A_levels[[2]], ":", sep = ""),
      b_effects
    ),
    paste(
      paste(grouping_variable_A_levels[[1]], ":", sep = ""),
      b_effects
    ),
    c(
      paste(grouping_variable_A_levels[[2]], ": ", b_effects[[3]], sep = ""),
      paste(grouping_variable_A_levels[[1]], ": ", b_effects[[3]], sep = ""),
      paste("Difference of differences")
    )
  )

  names(means) <- c(
    paste(grouping_variable_A_levels[1], grouping_variable_B_levels, sep = " - "),
    paste(grouping_variable_A_levels[2], grouping_variable_B_levels, sep = " - ")
  )


  all_estimates <- list()

  for (x in 1:5) {
    contrast <- all_contrasts[[x]]
    grouping_variable_name <- all_grouping_variable_names[[x]]
    weights <- esci_tool_contrast_fixed(contrast, means)
    contrast_labels <- all_contrast_labels[[x]]

    # We'll estimate the comparison subset, reference subset, and the difference
    if (names(all_contrasts)[[x]] == "interaction") {
      contrasts <- list(
        comparison = all_contrasts$simple_effect_B_at_A2,
        reference = all_contrasts$simple_effect_B_at_A1,
        difference = all_contrasts$interaction
      )
      names(contrasts$comparison) <- names(means)
      names(contrasts$reference) <- names(means)
      names(contrasts$difference) <- names(means)
    } else {
      contrasts <- list(
        comparison = weights,
        reference = weights,
        difference = weights
      )
      # Filter to create comparison and reference only subsets
      contrasts$comparison[which(contrasts$comparison < 0)] <- 0
      contrasts$reference[which(contrasts$reference > 0)] <- 0
      contrasts$reference <- abs(contrasts$reference)
    }


    # Prepare esci_estimate object that will be returned-------------------------
    estimate <- list()
    estimate$properties <- list(
      outcome_variable_name = outcome_variable_name,
      grouping_variable_name = paste(grouping_variable_A_name, grouping_variable_B_name, sep = " - "),
      grouping_variable_A_name = grouping_variable_A_name,
      grouping_variable_B_name = grouping_variable_B_name,
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
    estimate$es_mean_difference$effect_type <- effect_types[[x]]
    estimate$es_mean_difference$effects_complex <- effects_complex[[x]]

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

      estimate$es_median_difference$effect_type <- effect_types[[x]]
      estimate$es_median_difference$effects_complex <- effects_complex[[x]]

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

    estimate$es_smd$effect_type <- effect_types[[x]]
    estimate$es_smd$effects_complex <- effects_complex[[x]][[3]]

    all_estimates[[names(all_contrasts)[[x]]]] <- estimate

  }

  all_estimates <- esci_estimate_consolidate(all_estimates)
  class(all_estimates) <- "esci_estimate"


  return(all_estimates)

}


estimate_mdiff_2x2_between.summary <- function(
  means,
  sds,
  ns,
  grouping_variable_A_levels = NULL,
  grouping_variable_B_levels = NULL,
  grouping_variable_A_name = "A",
  grouping_variable_B_name = "B",
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # means - vector of numeric data with 4 elements, no NAs
  # sds  - vector of numeric data of same length as means, no NAs
  # ns - vector of integers >= 2, of same length as means, no NAs
  # grouping_variable_A_levels -
  #   if passed: vector of 2 characters
  #   if not passed: auto-generated (A1, A2) and warning issued
  # grouping_variable_B_levels -
  #   if passed: vector of 2 characters
  #   if not passed: auto-generated (B1, B2) and warning issued

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
    lower = 4,
    lower_inclusive = TRUE,
    upper = 4,
    upper_inclusive = TRUE,
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

  # Set A labels
  if(!is.null(grouping_variable_A_levels)) {
    esci_assert_type(grouping_variable_A_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_A_levels,
      lower = 2,
      upper = 2,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_A_levels <- c("A1", "A2")
    glc <- paste(
      grouping_variable_A_levels,
      collapse = ", "
    )
    msg <- glue::glue("Labels for grouping_variable_A have been auto-generated: {glc}")
    warnings <- c(warnings, msg)
    warning(msg)
  }


  # Set B labels
  if(!is.null(grouping_variable_B_levels)) {
    esci_assert_type(grouping_variable_B_levels, "is.character")
    row_report <- esci_assert_vector_valid_length(
      grouping_variable_B_levels,
      lower = 2,
      upper = 2,
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = FALSE
    )
  } else {
    grouping_variable_B_levels <- c("B1", "B2")
    glc <- paste(
      grouping_variable_B_levels,
      collapse = ", "
    )
    msg <- glue::glue("Labels for grouping_variable_A have been auto-generated: {glc}")
    warnings <- c(warnings, msg)
    warning(msg)
  }

  olevels <- c(
    paste(grouping_variable_A_levels[1], grouping_variable_B_levels, sep = " - "),
    paste(grouping_variable_A_levels[2], grouping_variable_B_levels, sep = " - ")
  )

  # Overview table-------------------------------------------------------------
  overview <- overview.summary(
    means = means,
    sds = sds,
    ns = ns,
    grouping_variable_levels = olevels,
    grouping_variable_name = grouping_variable_A_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  #
  # estimate <- estimate_mdiff_ind_contrast.base(
  #   overview_table = overview,
  #   grouping_variable_levels = grouping_variable_levels,
  #   grouping_variable_name = grouping_variable_name,
  #   outcome_variable_name = outcome_variable_name,
  #   contrast = contrast,
  #   conf_level = conf_level,
  #   assume_equal_variance = assume_equal_variance
  # )

  estimate <- estimate_mdiff_2x2_between.base(
    overview_table = overview,
    grouping_variable_A_levels = grouping_variable_A_levels,
    grouping_variable_B_levels = grouping_variable_B_levels,
    grouping_variable_A_name = grouping_variable_A_name,
    grouping_variable_B_name = grouping_variable_B_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  for (x in 1:length(estimate)) {
    if (class(estimate[[x]]) == "esci_estimate") {
      estimate[[x]]$overview <- overview
      estimate[[x]]$properties$data_type <- "summary"
      estimate[[x]]$properties$data_source <- NULL
    }
  }

  overview$grouping_variable_A_name <- grouping_variable_A_name
  overview$grouping_variable_B_name <- grouping_variable_B_name
  overview$grouping_variable_A_level <- c(
    rep(grouping_variable_A_levels[[1]], 2),
    rep(grouping_variable_A_levels[[2]], 2)
  )
  overview$grouping_variable_B_level <- c(
    grouping_variable_B_levels,
    grouping_variable_B_levels
  )

  estimate$overview <- overview
  estimate$warnings <- c(estimate$warnings, warnings)
  estimate$properties$data_type <- "summary"
  estimate$properties$data_source <- NULL

  return(estimate)

}


estimate_mdiff_2x2_between.vector <- function(
  grouping_variable_A,
  grouping_variable_B,
  outcome_variable,
  grouping_variable_A_name = "A",
  grouping_variable_B_name = "B",
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
  esci_assert_type(grouping_variable_A, "is.factor")
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable_A,
    lower = 2,
    lower_inclusive = FALSE)
  if (length(levels(as.factor(grouping_variable_A))) < 2) {
    stop("Not enough levels in grouping_variable_A")
  }

  # Check grouping variable
  esci_assert_type(grouping_variable_B, "is.factor")
  grouping_variable_report <- esci_assert_vector_valid_length(
    grouping_variable_B,
    lower = 2,
    lower_inclusive = FALSE)
  if (length(levels(as.factor(grouping_variable_B))) < 2) {
    stop("Not enough levels in grouping_variable_B")
  }


  # Check outcome variable
  esci_assert_type(outcome_variable, "is.numeric")
  if(length(grouping_variable_A) != length(outcome_variable)) {
    # vectors not of same length!
    msg <- glue::glue("
The grouping_variable_A and outcome_variable are not the same length
The grouping_variable_A length is {length(grouping_variable_A)};
The outcome_variable length is {length(outcome_variable)}.
    ")
    stop(msg)
  }
  if(length(grouping_variable_B) != length(outcome_variable)) {
    # vectors not of same length!
    msg <- glue::glue("
The grouping_variable_B and outcome_variable are not the same length
The grouping_variable_B length is {length(grouping_variable_A)};
The outcome_variable length is {length(outcome_variable)}.
    ")
    stop(msg)
  }


  # Check save_raw_data
  esci_assert_type(save_raw_data, "is.logical")


  # Do the analysis --------------------------------------------------
  # Create overview -- which will gracefully deal with missing and n=0 or n=1
  all_overview <- overview.vector(
    grouping_variable = paste(grouping_variable_A, grouping_variable_B, sep = " - "),
    outcome_variable = outcome_variable,
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = paste(grouping_variable_A_name, grouping_variable_B_name, sep = " - "),
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  mydata <- data.frame(
    ov = outcome_variable,
    g1 = grouping_variable_A,
    g2 = grouping_variable_B
  )

  colnames(mydata) <- c(
    outcome_variable_name,
    grouping_variable_A_name,
    grouping_variable_B_name
  )

  estimate <- estimate_mdiff_2x2_between.data.frame(
    data = mydata,
    grouping_variable_A = grouping_variable_A_name,
    grouping_variable_B = grouping_variable_B_name,
    outcome_variable = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance,
    save_raw_data = save_raw_data
  )

  return(estimate)
}


estimate_mdiff_2x2_between.data.frame <- function(
  data,
  grouping_variable_A,
  grouping_variable_B,
  outcome_variable,
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
  esci_assert_valid_column_name(data, grouping_variable_A)
  esci_assert_column_type(data, grouping_variable_A, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    grouping_variable_A,
    lower = 2,
    na.rm = TRUE
  )
  esci_assert_valid_column_name(data, grouping_variable_B)
  esci_assert_column_type(data, grouping_variable_B, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    grouping_variable_B,
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



  keeps <- c(
    grouping_variable_A,
    grouping_variable_B,
    outcome_variable
  )
  data <- data[ , keeps]

  # Check for NAs and too many levels
  invalids <- NULL

  vs_to_check <- c(grouping_variable_B, grouping_variable_A, outcome_variable)
  for (myv in vs_to_check) {
    if (sum(is.na(data[[myv]])) > 0) {
      invalids <- c(
        invalids,
        glue::glue("{myv} had {sum(is.na(data[[myv]]))} NA elements; these have been dropped.")
      )
    }

  }

  if (length(levels(data[[grouping_variable_A]])) > 2) {
    invalids <- c(
      invalids,
      glue::glue("Variable {grouping_variable_A} has more than 2 levels; only the first two levels were processed; the levels {paste0(levels(data[[grouping_variable_A]])[-(1:2)], collapse = ', ')} were dropped.")
    )
  }

  if (length(levels(data[[grouping_variable_B]])) > 2) {
    invalids <- c(
      invalids,
      glue::glue("Variable {grouping_variable_B} has more than 2 levels; only the first two levels were processed; the levels {paste0(levels(data[[grouping_variable_B]])[-(1:2)], collapse = ', ')} were dropped.")
    )
    data <- data[data[[grouping_variable_B]] %in% levels(data[[grouping_variable_B]])[1:2], ]

  }

  data <- data[complete.cases(data), ]
  data <- droplevels(data)

  a1 <- levels(data[[grouping_variable_A]])[[1]]
  a2 <- levels(data[[grouping_variable_A]])[[2]]
  b1 <- levels(data[[grouping_variable_B]])[[1]]
  b2 <- levels(data[[grouping_variable_B]])[[2]]

  da1 <- data[data[[grouping_variable_A]] == a1, ]
  da2 <- data[data[[grouping_variable_A]] == a2, ]

  esci_assert_column_has_valid_rows(
    da1,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  esci_assert_column_has_valid_rows(
    da2,
    outcome_variable,
    lower = 2,
    na.rm = TRUE
  )

  overview <- rbind(
    overview.data.frame(
      data = da1,
      outcome_variable = outcome_variable,
      grouping_variable = grouping_variable_B,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    ),
    overview.data.frame(
      data = da2,
      outcome_variable = outcome_variable,
      grouping_variable = grouping_variable_B,
      conf_level = conf_level,
      assume_equal_variance = assume_equal_variance
    )
  )

  overview_all <- overview.summary(
    means = overview$mean,
    sds = overview$sd,
    ns = overview$n,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )
  overview$mean_LL <- overview_all$mean_LL
  overview$mean_UL <- overview_all$mean_UL
  overview$df <- overview_all$df

  overview$grouping_variable_level <- c(
    paste(levels(data[[grouping_variable_A]])[[1]], levels(data[[grouping_variable_B]])[1:2], sep = " - "),
    paste(levels(data[[grouping_variable_A]])[[2]], levels(data[[grouping_variable_B]])[1:2], sep = " - ")
  )

  estimate <- estimate_mdiff_2x2_between.base(
    overview_table = overview,
    grouping_variable_A_levels = c(a1, a2),
    grouping_variable_B_levels = c(b1, b2),
    grouping_variable_A_name = grouping_variable_A,
    grouping_variable_B_name = grouping_variable_B,
    outcome_variable_name = outcome_variable,
    conf_level = conf_level,
    assume_equal_variance = assume_equal_variance
  )

  # Store raw data -----------------------------------------------
  if (save_raw_data) {
    # Revise all NAs
    raw_data <- data.frame(
      grouping_variable = as.factor(
        paste(
          data[[grouping_variable_A]],
          data[[grouping_variable_B]],
          sep = " - "
        )
      ),
      outcome_variable = data[[outcome_variable]],
      grouping_variable_A = data[[grouping_variable_A]],
      grouping_variable_B = data[[grouping_variable_B]]
    )
  }


  for (x in 1:length(estimate)) {
    if (class(estimate[[x]]) == "esci_estimate") {
        estimate[[x]]$overview <- overview
        estimate[[x]]$raw_data <- if (save_raw_data) raw_data else NULL
        estimate[[x]]$properties$data_type <- "data.frame"
        estimate[[x]]$properties$data_source <- deparse(substitute(data))
    }
  }


  overview$grouping_variable_A_name <- grouping_variable_A
  overview$grouping_variable_B_name <- grouping_variable_B
  overview$grouping_variable_A_level <- c(a1, a1, a2, a2)
  overview$grouping_variable_B_level <- c(b1, b2, b1, b2)

  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))
  estimate$overview <- overview

  if (!is.null(invalids)) {
    estimate$overview_properties$message <- paste0(
      invalids,
      collapse = "\n"
    )

    estimate$overview_properties$message_html <- gsub(
      "\n",
      "<BR>",
      estimate$overview_properties$message
    )

  }


  return(estimate)

}

