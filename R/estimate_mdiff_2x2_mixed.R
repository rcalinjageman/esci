#' Estimates for a 2x2 mixed factorial design with a continuous outcome
#' variable
#'
#' @description
#' \loadmathjax
#' `estimate_mdiff_2x2_mixed` is suitable for a 2x2 mixed-factorial design
#' with a continuous outcome variable.  It estimates each main effect, the
#' simple effects for the repeated-measures factor, and the interaction.
#' It can express these estimates as mean differences.
#' This function accepts raw data only.  Standardized mean differences are not
#' (yet) available; stay tuned.  Median differences are also not yet available.
#'
#' @details
#' Reach for this function in place of a 2x2 mixed-factorial ANOVA.
#'
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_mdiff()] and you can visualize the interaction
#' specifically with [esci::plot_interaction()].  You can test hypotheses
#' with [esci::test_mdiff()].
#'
#'
#' The estimated mean differences are from [statpsych::ci.2x2.mean.mixed()].
#'
#'
#' @param data For raw data - a dataframe or tibble
#' @param outcome_variable_level1 The column name of the outcome
#'   variable for level 1 of the repeated-measures factor
#' @param outcome_variable_level2 The column name of the outcome
#'   variable for level 2 of the repeated-measures factor
#' @param grouping_variable The column name of the grouping
#'   variable; only 2 levels allowed; must be a factor
#' @param outcome_variable_name Optional friendly name for the outcome variable.
#'   Defaults to 'My outcome variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param repeated_measures_name Optional friendly name for the repeated
#'   measures factor.  Defaults to 'Time'
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
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
#'     - *t* -
#'     - *p* -
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
#'     - *paired* -
#'
#'
#' @examples
#' # From raw data (summary data mode not available for this function)
#' example_data <- data.frame(
#'   pretest = c(
#'     19, 18, 19, 20, 17, 16, 16, 10, 12,  9, 13, 15
#'   ),
#'   posttest = c(
#'     18, 19, 20, 17, 20, 16, 19, 16, 16, 14, 16, 18
#'   ),
#'   condition = as.factor(
#'     c(
#'       rep("Control", times = 6),
#'       rep("Treated", times = 6)
#'     )
#'   )
#' )
#'
#' estimates <- esci::estimate_mdiff_2x2_mixed(
#'   data = example_data,
#'   outcome_variable_level1 = pretest,
#'   outcome_variable_level2 = posttest,
#'   grouping_variable = condition,
#'   repeated_measures_name = "Time"
#' )
#
#' \dontrun{
#' # To visualize the estimated mean difference for the interaction
#' plot_mdiff(estimate$interaction, effect_size = "mean")
#' }
#'
#'
#'
#' @export
estimate_mdiff_2x2_mixed <- function(
    data,
    outcome_variable_level1,
    outcome_variable_level2,
    grouping_variable,
    outcome_variable_name = "My outcome variable",
    repeated_measures_name = "Time",
    conf_level = .95,
    save_raw_data = TRUE
) {

  # Quoting
  # --------------------------------
  outcome_variable_level1_enquo <- rlang::enquo(outcome_variable_level1)
  outcome_variable_level1_enquo_name <- try(
    eval(rlang::as_name(outcome_variable_level1_enquo)), silent = TRUE
  )
  if (!is(outcome_variable_level1_enquo_name, "try-error")) {
    # This only succeeds if the columns were passed unquoted
    # So now replace outcome_variable_level1 with a quoted version
    outcome_variable_level1 <- outcome_variable_level1_enquo_name
  }


  outcome_variable_level2_enquo <- rlang::enquo(outcome_variable_level2)
  outcome_variable_level2_enquo_name <- try(
    eval(rlang::as_name(outcome_variable_level2_enquo)), silent = TRUE
  )
  if (!is(outcome_variable_level2_enquo_name, "try-error")) {
    # This only succeeds if the columns were passed unquoted
    # So now replace outcome_variable_level2 with a quoted version
    outcome_variable_level2 <- outcome_variable_level2_enquo_name
  }

  grouping_variable_enquo <- rlang::enquo(grouping_variable)
  grouping_variable_enquo_name <- try(
    eval(rlang::as_name(grouping_variable_enquo)), silent = TRUE
  )
  if (!is(grouping_variable_enquo_name, "try-error")) {
    # This only succeeds if the columns were passed unquoted
    # So now replace grouping_variable with a quoted version
    grouping_variable <- grouping_variable_enquo_name
  }


  # Input checks
  # --------------------------------
  # This function expects:
  #   data to be a data frame
  #   grouping_variable to be a factor with 2 levels more than 2 valid rows
  #   outcome_variable_level1 to be a numeric column in data, with more than 2 rows
  #   outcome_variable_level2 to be a numeric column of data > 2 rows
  esci_assert_type(data, "is.data.frame")
  esci_assert_valid_column_name(data, grouping_variable)
  esci_assert_column_type(data, grouping_variable, "is.factor")
  esci_assert_column_has_valid_rows(
    data,
    grouping_variable,
    lower = 2,
    na.rm = TRUE
  )

  # Validate this outcome variable, level 1
  esci_assert_valid_column_name(data, outcome_variable_level1)
  esci_assert_column_type(data, outcome_variable_level1, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable_level1,
    lower = 2,
    na.rm = TRUE
  )

  # Validate this outcome variable, level 2
  esci_assert_valid_column_name(data, outcome_variable_level2)
  esci_assert_column_type(data, outcome_variable_level2, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    outcome_variable_level2,
    lower = 2,
    na.rm = TRUE
  )

  if (is.null(outcome_variable_name)) {
    outcome_variable_name <- "My outcome variable"
  }
  if (!is.character(outcome_variable_name)) {
    outcome_variable_name <- "My outcome variable"
  }
  if (is.null(repeated_measures_name)) {
    repeated_measures_name <- "Time"
  }
  if (!is.character(repeated_measures_name)) {
    repeated_measures_name <- "Time"
  }


  # Check for NAs and too many levels
  invalids <- NULL

  vs_to_check <- c(outcome_variable_level1, outcome_variable_level2, grouping_variable)
  for (myv in vs_to_check) {
    if (sum(is.na(data[[myv]])) > 0) {
      invalids <- c(
        invalids,
        glue::glue("{myv} had {sum(is.na(data[[myv]]))} NA elements; these have been dropped.")
      )
    }

  }

  if (length(levels(data[[grouping_variable]])) > 2) {
    invalids <- c(
      invalids,
      glue::glue("Variable {grouping_variable} has more than 2 levels; only the first two levels were processed; the levels {paste0(levels(data[[grouping_variable]])[-(1:2)], collapse = ', ')} were dropped.")
    )
  }


  # Data prep
  # ---------------------------
  keeps <- c(
    outcome_variable_level1,
    outcome_variable_level2,
    grouping_variable
  )

  data <- data[ , keeps]
  data <- data[complete.cases(data), ]

  data <- data[data[[grouping_variable]] %in% levels(data[[grouping_variable]])[1:2], ]

  a1 <- levels(data[[grouping_variable]])[[1]]
  a2 <- levels(data[[grouping_variable]])[[2]]
  b1 <- outcome_variable_level1
  b2 <- outcome_variable_level2

  overview <- rbind(
    overview.data.frame(
      data = data,
      outcome_variable = outcome_variable_level1,
      grouping_variable = grouping_variable,
      conf_level = conf_level,
      assume_equal_variance = FALSE
    )[1:2, ],
    overview.data.frame(
      data = data,
      outcome_variable = outcome_variable_level2,
      grouping_variable = grouping_variable,
      conf_level = conf_level,
      assume_equal_variance = FALSE
    )[1:2, ]
  )

  overview <- overview[c(1, 3, 2, 4), ]


  overview$grouping_variable_level <- c(
    paste(a1, c(b1, b2), sep = " - "),
    paste(a2, c(b1, b2), sep = " - ")
  )

  estimate <- estimate_mdiff_2x2_between.base(
    overview_table = overview,
    grouping_variable_A_levels = c(a1, a2),
    grouping_variable_B_levels =c(b1, b2),
    grouping_variable_A_name = grouping_variable,
    grouping_variable_B_name = repeated_measures_name,
    outcome_variable_name = outcome_variable_name,
    conf_level = conf_level,
    assume_equal_variance = FALSE
  )


  sp <- as.data.frame(
    statpsych::ci.2x2.mean.mixed(
      alpha = 1 - conf_level,
      y11 = data[data[[grouping_variable]] == a2, b2],
      y12 = data[data[[grouping_variable]] == a2, b1],
      y21 = data[data[[grouping_variable]] == a1, b2],
      y22 = data[data[[grouping_variable]] == a1, b1]
    )
  )

  sp_ta <- as.data.frame(
   statpsych::ci.2x2.mean.mixed(
      alpha = (1 - conf_level)*2,
      y11 = data[data[[grouping_variable]] == a2, b2],
      y12 = data[data[[grouping_variable]] == a2, b1],
      y21 = data[data[[grouping_variable]] == a1, b2],
      y22 = data[data[[grouping_variable]] == a1, b1]
    )
  )

  sp$ta_LL <- sp_ta$LL
  sp$ta_UL <- sp_ta$UL

  esci_names <- c("effect_size", "LL", "UL", "ta_LL", "ta_UL", "SE", "df", "t", "p")
  statpsych_names <- c("Estimate", "LL", "UL", "ta_LL", "ta_UL", "SE", "df", "t", "p")

  tbl_fix <- c(
    "main_effect_A" = 3,
    "main_effect_B" = 2,
    "interaction" = 1,
    "simple_effect_B_at_A1" = 5,
    "simple_effect_B_at_A2" = 4
  )

  for (x in 1:length(tbl_fix)) {
    cestimate <- names(tbl_fix)[[x]]
    estimate[[cestimate]]$es_mean_difference$t <- NA
    estimate[[cestimate]]$es_mean_difference$p <- NA
    estimate[[cestimate]]$es_mean_difference[3, esci_names] <-
      sp[tbl_fix[[x]], statpsych_names]

  }

  estimate$interaction$es_mean_difference[1, esci_names] <-
    sp[4, statpsych_names]
  estimate$interaction$es_mean_difference[2, esci_names] <-
    sp[5, statpsych_names]


  ws_estimate <- esci::estimate_mdiff_paired(
    data = data,
    comparison_measure = outcome_variable_level2,
    reference_measure = outcome_variable_level1,
    conf_level = conf_level,
    save_raw_data = FALSE
  )

  esci_copy_columns <- c("LL", "UL", "ta_LL", "ta_UL", "SE", "df")
  estimate$main_effect_B$es_mean_difference[1, esci_copy_columns] <-
    ws_estimate$es_mean_difference[1, esci_copy_columns]
  estimate$main_effect_B$es_mean_difference[2, esci_copy_columns] <-
    ws_estimate$es_mean_difference[2, esci_copy_columns]


  data$esci_ov <- (data[[outcome_variable_level1]] + data[[outcome_variable_level2]])/2
  bs_estimate <- esci::estimate_mdiff_two(
    data = data,
    outcome_variable = "esci_ov",
    grouping_variable = grouping_variable,
    conf_level = conf_level,
    assume_equal_variance = FALSE,
    save_raw_data = FALSE
  )
  estimate$main_effect_A$es_mean_difference[1, esci_copy_columns] <-
    bs_estimate$es_mean_difference[1, esci_copy_columns]
  estimate$main_effect_A$es_mean_difference[2, esci_copy_columns] <-
    bs_estimate$es_mean_difference[2, esci_copy_columns]


  # Store raw data -----------------------------------------------
  if (save_raw_data) {
    myrows <- nrow(data)

    # Revise all NAs
    raw_data <- data.frame(
      grouping_variable = as.factor(
        c(
          paste(
            data[[grouping_variable]],
            outcome_variable_level1,
            sep = " - "
          ),
          paste(
            data[[grouping_variable]],
            outcome_variable_level2,
            sep = " - "
          )
        )
      ),
      outcome_variable = c(data[[outcome_variable_level1]], data[[outcome_variable_level2]]),
      grouping_variable_A = as.factor(
        c(
          data[[grouping_variable]],
          data[[grouping_variable]]
        )
      ),
      grouping_variable_B = as.factor(c(
        rep(outcome_variable_level1, myrows),
        rep(outcome_variable_level2, myrows)
      )),
      paired =  c(data[[outcome_variable_level2]], data[[outcome_variable_level1]])
    )

    levels(raw_data$grouping_variable_A) <- levels(data[[grouping_variable]])

    estimate$raw_data <- raw_data

  }


  overview$outcome_variable_name <- outcome_variable_name

  for (x in 1:length(estimate)) {
    if (is(estimate[[x]], "esci_estimate")) {
      estimate[[x]]$overview <- overview
      estimate[[x]]$raw_data <- if (save_raw_data) raw_data else NULL
      estimate[[x]]$properties$data_type <- "data.frame"
      estimate[[x]]$properties$data_source <- deparse(substitute(data))

      estimate[[x]]$es_median_difference <- NULL
      estimate[[x]]$es_median_difference_properties <- NULL
      # estimate[[x]]$es_smd <- NULL
      # estimate[[x]]$es_smd_properties <- NULL
    }
  }

  estimate$properties <- NULL
  estimate$es_mean_difference <- NULL
  estimate$es_mean_difference_properties <- NULL
  estimate$es_median_difference <- NULL
  estimate$es_median_difference_properties <- NULL
  estimate$es_smd <- NULL
  estimate$es_smd_properties <- NULL

  estimate <- esci_estimate_consolidate(estimate)
  class(estimate) <- "esci_estimate"


  overview$grouping_variable_A_name <- grouping_variable
  overview$grouping_variable_B_name <- repeated_measures_name
  overview$grouping_variable_A_level <- c(a1, a1, a2, a2)
  overview$grouping_variable_B_level <- c(b1, b2, b1, b2)

  estimate$properties$data_type <- "data.frame"
  estimate$properties$data_source <- deparse(substitute(data))
  estimate$overview <- overview

  if (!is.null(invalids)) {
    estimate$overview_properties$message <- paste0(
      invalids,
      "\nVariances are not assumed equal and so the CI was calculated seperately for each mean.",
      collapse = "\n"
    )

    estimate$overview_properties$message_html <- gsub(
      "\n",
      "<BR>",
      estimate$overview_properties$message
    )
  } else {
    estimate$overview_properties$message_html <- "Variances are not assumed equal and so the CI was calculated seperately for each mean."
  }


  estimate$es_mean_difference_properties$message_html <- "Variances are not assumed equal."



  return(estimate)

}
