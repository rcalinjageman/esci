#' Estimates for a 2x2 mixed factorial design with a continuous outcome
#' variable
#'
#' @description Returns object
#' `estimate_mdiff_2x2_mixed` is suitable for a 2x2 mixed-factorial design
#' with a continuous outcome variable.  It estimates each main effect, the
#' simple effects for the repeated-measures factor, and the interaction.
#' It can express these estimates as mean differences, median difference,
#' or standardized mean differences.  This function accepts raw data only.
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
#' # To visualize the estimated mean difference for the interaction
#' myplot <- esci::plot_mdiff(estimates$interaction, effect_size = "mean")
#'
#' # Line-plot of the interaction with fan effect representing each simple-effect CI
#' plot_interaction_line_CI <- esci::plot_interaction(
#'   estimates,
#'   show_CI = TRUE
#' )
#'
#' # To conduct a hypothesis test
#' res_htest_from_raw <- esci::test_mdiff(
#'   estimates$interaction,
#'   effect_size = "mean"
#'  )
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

  statpsych_version <- as.numeric(gsub("\\.", "", utils::packageVersion("statpsych")))

  if (statpsych_version >= 170) {

    sp <- as.data.frame(
      statpsych::ci.2x2.mean.mixed(
        alpha = 1 - conf_level,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

    sp_ta <- as.data.frame(
      statpsych::ci.2x2.mean.mixed(
        alpha = (1 - conf_level)*2,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

    mysmds <- as.data.frame(
      statpsych::ci.2x2.stdmean.mixed(
        alpha = (1 - conf_level),
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )


    median_sp <- as.data.frame(
      statpsych::ci.2x2.median.mixed(
        alpha = 1 - conf_level,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

    median_sp_ta <- as.data.frame(
      statpsych::ci.2x2.median.mixed(
        alpha = (1 - conf_level)*2,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

  } else {
    sp <- as.data.frame(
      fixed.ci.2x2.mean.mixed(
        alpha = 1 - conf_level,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

    sp_ta <- as.data.frame(
      fixed.ci.2x2.mean.mixed(
        alpha = (1 - conf_level)*2,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

    mysmds <- as.data.frame(
      fixed.ci.2x2.stdmean.mixed(
        alpha = (1 - conf_level),
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )


    median_sp <- as.data.frame(
      fixed.ci.2x2.median.mixed(
        alpha = 1 - conf_level,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

    median_sp_ta <- as.data.frame(
      fixed.ci.2x2.median.mixed(
        alpha = (1 - conf_level)*2,
        y11 = data[data[[grouping_variable]] == a2, b2],
        y12 = data[data[[grouping_variable]] == a1, b2],
        y21 = data[data[[grouping_variable]] == a2, b1],
        y22 = data[data[[grouping_variable]] == a1, b1]
      )
    )

   }



  sp$ta_LL <- sp_ta$LL
  sp$ta_UL <- sp_ta$UL

  median_sp$ta_LL <- median_sp_ta$LL
  median_sp$ta_UL <- median_sp_ta$UL

  esci_names <- c("effect_size", "LL", "UL", "ta_LL", "ta_UL", "SE", "df", "t", "p")
  statpsych_names <- c("Estimate", "LL", "UL", "ta_LL", "ta_UL", "SE", "df", "t", "p")

  d_esci_names <- c("effect_size", "LL", "UL", "SE", "d_biased")
  d_statpsych_names <- c("adj Estimate", "LL", "UL", "SE", "Estimate")

  mdn_esci_names <- c("effect_size", "LL", "UL", "ta_LL", "ta_UL", "SE")
  mdn_statpsych_names <- c("Estimate", "LL", "UL", "ta_LL", "ta_UL", "SE")

  tbl_fix <- c(
    "main_effect_A" = 3,
    "main_effect_B" = 2,
    "interaction" = 1,
    "simple_effect_B_at_A1" = 5,
    "simple_effect_B_at_A2" = 4
  )

  mdn_tbl_fix <- c(
    "main_effect_A" = 2,
    "main_effect_B" = 3,
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
    estimate[[cestimate]]$es_smd[1, d_esci_names] <- mysmds[tbl_fix[[x]], d_statpsych_names]
    estimate[[cestimate]]$es_median_difference[3, mdn_esci_names] <-
      median_sp[mdn_tbl_fix[[x]], mdn_statpsych_names]

  }

  estimate$interaction$es_mean_difference[1, esci_names] <-
    sp[4, statpsych_names]
  estimate$interaction$es_mean_difference[2, esci_names] <-
    sp[5, statpsych_names]


  estimate$interaction$es_median_difference[1, mdn_esci_names] <-
    median_sp[4, mdn_statpsych_names]
  estimate$interaction$es_median_difference[2, mdn_esci_names] <-
    median_sp[5, mdn_statpsych_names]

  ws_estimate <- esci::estimate_mdiff_paired(
    data = data,
    comparison_measure = outcome_variable_level2,
    reference_measure = outcome_variable_level1,
    conf_level = conf_level,
    save_raw_data = FALSE
  )

  esci_copy_columns <- c("LL", "UL", "ta_LL", "ta_UL", "SE", "df")
  # estimate$main_effect_B$es_mean_difference[1, esci_copy_columns] <-
  #   ws_estimate$es_mean_difference[1, esci_copy_columns]
  # estimate$main_effect_B$es_mean_difference[2, esci_copy_columns] <-
  #   ws_estimate$es_mean_difference[2, esci_copy_columns]


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

      # estimate[[x]]$es_median_difference <- NULL
      # estimate[[x]]$es_median_difference_properties <- NULL
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




fixed.ci.2x2.stdmean.mixed <- function(alpha, y11, y12, y21, y22) {
  if (length(y11) != length(y21)) {stop("length of y11 must equal length of y21")}
  if (length(y12) != length(y22)) {stop("length of y12 must equal length of y22")}
  z <- qnorm(1 - alpha/2)
  n1 <- length(y11)
  n2 <- length(y12)
  df1 <- n1 - 1
  df2 <- n2 - 1
  adj1 <- 1 - 3/(4*(df1 + df2) - 1)
  adj2 <- sqrt((n1 - 2)/df1)
  adj3 <- sqrt((n2 - 2)/df2)
  adj4 <- sqrt((n1 + n2 - 2)/(n1 + n2 - 1))
  diff1 <- y11 - y21
  diff2 <- y12 - y22
  ave1 <- (y11 + y21)/2
  ave2 <- (y12 + y22)/2
  vd1 <- var(diff1)
  vd2 <- var(diff2)
  va1 <- var(ave1)
  va2 <- var(ave2)
  sd1 <- sd(y11)
  sd2 <- sd(y12)
  sd3 <- sd(y21)
  sd4 <- sd(y22)
  cor1 <- cor(y11, y21)
  cor2 <- cor(y12, y22)
  s <- sqrt((sd1^2 + sd2^2 + sd3^2 + sd4^2)/4)
  v01 <- (sd1^4 + sd3^4 + 2*(cor1^2*sd1^2*sd3^2))/(32*s^4*df1)
  v02 <- (sd2^4 + sd4^4 + 2*(cor2^2*sd2^2*sd4^2))/(32*s^4*df2)
  v0 <- v01 + v02
  # AB
  est1 <- (mean(diff1) - mean(diff2))/s
  est1u <- adj1*est1
  v1 <- (vd1/df1 + vd2/df2)/(s^2)
  se1 <- sqrt(est1*v0/s^4 + v1)
  LL1 <- est1 - z*se1
  UL1 <- est1 + z*se1
  row1 <- c(est1, est1u, se1, LL1, UL1)
  # A
  est2 <- (mean(diff1) + mean(diff2))/(2*s)
  est2u <- adj4*est2
  v2 <- (vd1/df1 + vd2/df2)/(4*s^2)
  se2 <- sqrt(est2*v0/s^4 + v2)
  LL2 <- est2 - z*se2
  UL2 <- est2 + z*se2
  row2 <- c(est2, est2u, se2, LL2, UL2)
  # B
  est3 <- (mean(ave1) - mean(ave2))/s
  est3u <- adj1*est3
  v3 <- (va1/df1 + va2/df2)/(s^2)
  se3 <- sqrt(est3*v0/s^4 + v3)
  LL3 <- est3 - z*se3
  UL3 <- est3 + z*se3
  row3 <- c(est3, est3u, se3, LL3, UL3)
  # A at b1
  est4 <- mean(diff1)/s
  est4u <- adj2*est4
  v4 <- vd1/df1
  se4 <- sqrt(est4*v0/s^4 + v4/s^2)
  LL4 <- est4 - z*se4
  UL4 <- est4 + z*se4
  row4 <- c(est4, est4u, se4, LL4, UL4)
  # A at b2
  est5 <- mean(diff2)/s
  est5u <- adj3*est5
  v5 <- vd2/df2
  se5 <- sqrt(est5*v0/s^4 + v5/s^2)
  LL5 <- est5 - z*se5
  UL5 <- est5 + z*se5
  row5 <- c(est5, est5u, se5, LL5, UL5)
  # B at a1
  est6 <- (mean(y11) - mean(y12))/s
  est6u <- adj1*est6
  v6 <- var(y11)/df1 + var(y21)/df2
  se6 <- sqrt(est6*v0/s^4 + v6/s^2)
  LL6 <- est6 - z*se6
  UL6 <- est6 + z*se6
  row6 <- c(est6, est6u, se6, LL6, UL6)
  # B at a2
  est7 <- (mean(y21) - mean(y22))/s
  est7u <- adj1*est7
  v7 <- var(y12)/df1 + var(y22)/df2
  se7 <- sqrt(est7*v0/s^4 + v7/s^2)
  LL7 <- est7 - z*se7
  UL7 <- est7 + z*se7
  row7 <- c(est7, est7u, se7, LL7, UL7)
  out <- rbind(row1, row2, row3, row4, row5, row6, row7)
  rownames(out) <- c("AB:", "A:", "B:", "A at b1:", "A at b2:", "B at a1:", "B at a2:")
  colnames(out) = c("Estimate", "adj Estimate", "SE", "LL", "UL")
  return(out)
}


fixed.ci.2x2.median.mixed <- function(alpha, y11, y12, y21, y22) {
  if (length(y11) != length(y21)) {stop("length of y11 must equal length of y21")}
  if (length(y12) != length(y22)) {stop("length of y12 must equal length of y22")}
  z <- qnorm(1 - alpha/2)
  n1 <- length(y11)
  n2 <- length(y12)
  median11 <- median(y11)
  median12 <- median(y12)
  median21 <- median(y21)
  median22 <- median(y22)
  # Group 1
  a1 <- (y11 < median11)
  a2 <- (y21 < median21)
  a3 <- a1 + a2
  a4 <- sum(a3 == 2)
  a <- round(n1/2 - sqrt(n1))
  if (a < 1) {a = 1}
  p <- pbinom(a - 1, size = n1, prob = .5)
  z0 <- qnorm(1 - p)
  y11 <- sort(y11)
  y21 <- sort(y21)
  L1 <- y11[a]
  U1 <- y11[n1 - a + 1]
  se11 <- (U1 - L1)/(2*z0)
  L2 <- y21[a]
  U2 <- y21[n1 - a + 1]
  se21 <- (U2 - L2)/(2*z0)
  if (n1/2 == trunc(n1/2)) {
    p00 <- (sum(a4) + .25)/(n1 + 1)
  } else {
    p00 <- (sum(a4) + .25)/n1
  }
  cov1 <- (4*p00 - 1)*se11*se21
  # Group 2
  a1 <- (y12 < median12)
  a2 <- (y22 < median22)
  a3 <- a1 + a2
  a4 <- sum(a3 == 2)
  a <- round(n2/2 - sqrt(n2))
  if (a < 1) {a = 1}
  p <- pbinom(a - 1, size = n2, prob = .5)
  z0 <- qnorm(1 - p)
  y12 <- sort(y12)
  y22 <- sort(y22)
  L1 <- y12[a]
  U1 <- y12[n2 - a + 1]
  se12 <- (U1 - L1)/(2*z0)
  L2 <- y22[a]
  U2 <- y22[n2 - a + 1]
  se22 <- (U2 - L2)/(2*z0)
  if (n2/2 == trunc(n2/2)) {
    p00 <- (sum(a4) + .25)/(n2 + 1)
  } else {
    p00 <- (sum(a4) + .25)/n2
  }
  cov2 <- (4*p00 - 1)*se12*se22
  # AB
  est1 <- (median11 - median12) - (median21 - median22)
  se1 <- sqrt(se11^2 + se21^2 - 2*cov1 + se12^2 + se22^2 - 2*cov2)
  LL1 <- est1 - z*se1
  UL1 <- est1 + z*se1
  row1 <- c(est1, se1, LL1, UL1)
  # A
  est2 <- (median11 + median21)/2 - (median12 + median22)/2
  se2 <- se1/2
  LL2 <- est2 - z*se2
  UL2 <- est2 + z*se2
  row2 <- c(est2, se2, LL2, UL2)
  # B
  est3 <- (median11 + median12)/2 - (median21 + median22)/2
  se3 <- sqrt(se11^2 + se21^2 + 2*cov1 + se12^2 + se22^2 + 2*cov2)/2
  LL3 <- est3 - z*se3
  UL3 <- est3 + z*se3
  row3 <- c(est3, se3, LL3, UL3)
  # A at b1
  est4 <- median11 - median21
  se4 <- sqrt(se11^2 + se21^2 - 2*cov1)
  LL4 <- est4 - z*se4
  UL4 <- est4 + z*se4
  row4 <- c(est4, se4, LL4, UL4)
  # A at b2
  est5 <- median12 - median22
  se5 <- sqrt(se12^2 + se22^2 - 2*cov2)
  LL5 <- est5 - z*se5
  UL5 <- est5 + z*se5
  row5 <- c(est5, se5, LL5, UL5)
  #B at a1
  est6 <- median11 - median12
  se6 <- sqrt(se11^2 + se12^2)
  LL6 <- est6 - z*se6
  UL6 <- est6 + z*se6
  row6 <- c(est6, se6, LL6, UL6)
  #B at a2
  est7 <- median21 - median22
  se7 <- sqrt(se21^2 + se22^2)
  LL7 <- est7 - z*se7
  UL7 <- est7 + z*se7
  row7 <- c(est7, se7, LL7, UL7)
  out <- rbind(row1, row2, row3, row4, row5, row6, row7)
  rownames(out) <- c("AB:", "A:", "B:", "A at b1:", "A at b2:", "B at a1:", "B at a2:")
  colnames(out) = c("Estimate", "SE", "LL", "UL")
  return(out)
}



fixed.ci.2x2.mean.mixed <- function(alpha, y11, y12, y21, y22) {
  if (length(y11) != length(y21)) {stop("length of y11 must equal length of y21")}
  if (length(y12) != length(y22)) {stop("length of y12 must equal length of y22")}
  n1 <- length(y11)
  n2 <- length(y12)
  diff1 <- y11 - y21
  diff2 <- y12 - y22
  ave1 <- (y11 + y21)/2
  ave2 <- (y12 + y22)/2
  vd1 <- var(diff1)
  vd2 <- var(diff2)
  va1 <- var(ave1)
  va2 <- var(ave2)
  # AB
  est1 <- mean(diff1) - mean(diff2)
  se1 <- sqrt(vd1/n1 + vd2/n2)
  df1 <- (se1^4)/(vd1^2/(n1^3 - n1^2) + vd2^2/(n2^3 - n2^2))
  tcrit1 <- qt(1 - alpha/2, df1)
  t1 <- est1/se1
  p1 <- 2*(1 - pt(abs(t1), df1))
  LL1 <- est1 - tcrit1*se1
  UL1 <- est1 + tcrit1*se1
  row1 <- c(est1, se1, t1, df1, p1, LL1, UL1)
  # A
  est2 <- (mean(diff1) + mean(diff2))/2
  se2 <- sqrt(vd1/n1 + vd2/n2)/2
  df2 <- (se2^4)/(vd1^2/((n1^3 - n1^2)*16) + vd2^2/((n2^3 - n2^2)*16))
  tcrit2 <- qt(1 - alpha/2, df2)
  t2 <- est2/se2
  p2 <- 2*(1 - pt(abs(t2), df2))
  LL2 <- est2 - tcrit2*se2
  UL2 <- est2 + tcrit2*se2
  row2 <- c(est2, se2, t2, df2, p2, LL2, UL2)
  # B
  est3 <- mean(ave1) - mean(ave2)
  se3 <- sqrt(va1/n1 + va2/n2)
  df3 <- (se3^4)/(va1^2/(n1^3 - n1^2) + va2^2/(n2^3 - n2^2))
  tcrit3 <- qt(1 - alpha/2, df3)
  t3 <- est3/se3
  p3 <- 2*(1 - pt(abs(t3), df3))
  LL3 <- est3 - tcrit3*se3
  UL3 <- est3 + tcrit3*se3
  row3 <- c(est3, se3, t3, df3, p3, LL3, UL3)
  # A at b1
  est4 <- mean(diff1)
  se4 <- sqrt(vd1/n1)
  df4 <- n1 - 1
  tcrit4 <- qt(1 - alpha/2, df4)
  t4 <- est4/se4
  p4 <- 2*(1 - pt(abs(t4), df4))
  LL4 <- est4 - tcrit4*se4
  UL4 <- est4 + tcrit4*se4
  row4 <- c(est4, se4, t4, df4, p4, LL4, UL4)
  # A at b2
  est5 <- mean(diff2)
  se5 <- sqrt(vd2/n2)
  df5 <- n2 - 1
  tcrit5 <- qt(1 - alpha/2, df5)
  t5 <- est5/se5
  p5 <- 2*(1 - pt(abs(t5), df5))
  LL5 <- est5 - tcrit5*se5
  UL5 <- est5 + tcrit5*se5
  row5 <- c(est5, se5, t5, df5, p5, LL5, UL5)
  # B at a1
  est6 <- mean(y11) - mean(y12)
  se6 <- sqrt(var(y11)/n1 + var(y12)/n2)
  df6 <- (se6^4)/(var(y11)^2/(n1^3 - n1^2) + var(y12)^2/(n2^3 - n2^2))
  tcrit6 <- qt(1 - alpha/2, df6)
  t6 <- est6/se6
  p6 <- 2*(1 - pt(abs(t6), df6))
  LL6 <- est6 - tcrit6*se6
  UL6 <- est6 + tcrit6*se6
  row6 <- c(est6, se6, t6, df6, p6, LL6, UL6)
  # B at a2
  est7 <- mean(y21) - mean(y22)
  se7 <- sqrt(var(y21)/n1 + var(y22)/n2)
  df7 <- (se7^4)/(var(y21)^2/(n1^3 - n1^2) + var(y22)^2/(n2^3 - n2^2))
  tcrit7 <- qt(1 - alpha/2, df7)
  t7 <- est7/se7
  p7 <- 2*(1 - pt(abs(t7), df7))
  LL7 <- est7 - tcrit7*se7
  UL7 <- est7 + tcrit7*se7
  row7 <- c(est7, se7, t7, df7, p7, LL7, UL7)
  out <- rbind(row1, row2, row3, row4, row5, row6, row7)
  rownames(out) <- c("AB:", "A:", "B:", "A at b1:", "A at b2:", "B at a1:", "B at a2:")
  colnames(out) <- c("Estimate", "SE", "t", "df", "p", "LL", "UL")
  return(out)
}
