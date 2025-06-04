#' Estimate meta-analytic difference in proportions over multiple studies
#' with two independent groups and a categorical outcome variable.
#'
#'
#' @description
#' `meta_pdiff_two` is suitable for synthesizing across multiple two-group
#' studies with a categorical outcome variable.  It takes as input the
#' the number of cases/events in the comparison and reference groups as
#' well as the total number of samples in the comparison and reference groups.
#'
#'
#' @details
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_meta()].
#'
#' The meta-analytic effect size, confidence interval and heterogeneity
#' estimates all come from [metafor::rma()].
#'
#' The conversion of events into suitable effect sizes is handled by
#' [metafor::escalc()]
#'
#'
#' @param data A dataframe or tibble
#' @param comparison_cases A collection of case/event counts for the comparison
#'   groups, 1 per study, all integers >= 0
#' @param comparison_ns A collection of sample sizes for the comparison groups,
#'   1 per study, all integers > 2
#' @param reference_cases A collection of case/event counts for the reference
#'   groups, 1 per study, all integers >= 0
#' @param reference_ns A collection of sample sizes for the reference groups,
#'   1 per study, all integers > 2
#' @param labels An optional collection of study labels
#' @param moderator An optional factor to analyze as a categorical moderator,
#' must have k > 2 per groups
#' @param contrast An optional contrast to estimate between moderator levels;
#' express as a vector of contrast weights with 1 weight per moderator level.
#' @param effect_label Optional character giving a human-friendly name of
#' the effect being synthesized
#' @param reported_effect_size Character specifying effect size to return:
#'   Must be one of 'RD' (risk difference, default), 'RR' (log risk
#'   ratio), 'OR' (log odds ratio), 'AS' (arcsine square root transformed
#'   risk difference), or 'PETO' (log odds ratio estimated using Peto's
#'   method).  See [metafor::escalc()] for details.
#' @param random_effects TRUE for random effect model; FALSE for fixed effects
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#'
#' @inherit meta_any return
#'
#'
#' @examples
#' # Data set: Replications of power on egocentric behavior
#' esci_meta_pdiff_two <- data.frame(
#'   studies = c(
#'     "Online",
#'     "Original",
#'     "Online Pilot",
#'     "Exact replication"
#'   ),
#'   control_egocentric = c(
#'     33,
#'     4,
#'     4,
#'     7
#'   ),
#'   control_sample_size = c(
#'    101,
#'     33,
#'     10,
#'     53
#'   ),
#'   power_egocentric = c(
#'     48,
#'     8,
#'     4,
#'     11
#'   ),
#'   power_sample_size = c(
#'     105,
#'     24,
#'     12,
#'     56
#'   ),
#'   setting = as.factor(
#'     c(
#'       "Online",
#'      "In-Person",
#'       "Online",
#'       "In-Person"
#'     )
#'   )
#' )
#'
#' # Meta-analysis, risk difference as effect size
#' estimate <- esci::meta_pdiff_two(
#'   esci_meta_pdiff_two,
#'   power_egocentric,
#'   power_sample_size,
#'   control_egocentric,
#'   control_sample_size,
#'   studies,
#'   reported_effect_size = "RD"
#' )
#'
#' # Forest plot
#' myplot_forst <- esci::plot_meta(estimate)
#'
#'
#' # Add a categorical moderator (setting)
#' estimate_moderator <- esci::meta_pdiff_two(
#'   esci_meta_pdiff_two,
#'   power_egocentric,
#'   power_sample_size,
#'   control_egocentric,
#'   control_sample_size,
#'   studies,
#'   moderator = setting,
#'   reported_effect_size = "RD"
#' )
#'
#' # Forest plot
#' myplot_forest_moderator <- esci::plot_meta(estimate_moderator)
#'
#'
#' @export
meta_pdiff_two <- function(
  data,
  comparison_cases,
  comparison_ns,
  reference_cases,
  reference_ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  reported_effect_size = c("RD", "RR", "OR", "AS", "PETO"),
  random_effects = TRUE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  comparison_cases_enquo        <-  rlang::enquo(comparison_cases)
  comparison_cases_quoname      <-  rlang::quo_name(comparison_cases_enquo)

  reference_cases_enquo        <-  rlang::enquo(reference_cases)
  reference_cases_quoname      <-  rlang::quo_name(reference_cases_enquo)

  comparison_ns_enquo        <-  rlang::enquo(comparison_ns)
  comparison_ns_quoname      <-  rlang::quo_name(comparison_ns_enquo)

  reference_ns_enquo        <-  rlang::enquo(reference_ns)
  reference_ns_quoname      <-  rlang::quo_name(reference_ns_enquo)

  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  if (moderator_quoname == "NULL") moderator_quoname <- NULL

  labels_enquo        <-  rlang::enquo(labels)
  labels_quoname      <-  rlang::quo_name(labels_enquo)
  if (labels_quoname == "NULL") labels_quoname <- NULL

  warnings <- NULL

  comparison_N <- reference_N <- NULL

  # Input checks --------------------------------
  # * data must be a data frame
  #    all rows with an NA a parameter column will be dropped, warning issued
  # * the column cases must exist and be numeric, integer >= 0
  #    with > 1 row after NAs removed
  # * the column ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * reported_effect size shold be "RD", "RR", "OR", "AS", "PETO"
  # * conf_level must be a numeric >0 and < 1, checked in meta_any


  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")

  # comparison_cases
  esci_assert_valid_column_name(data, comparison_cases_quoname)
  esci_assert_column_type(data, comparison_cases_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_cases_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  if (!all(data[[comparison_cases_quoname]] >= 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some case values in {comparison_cases_quoname} are < 0.
These are rows {paste(which(data[[comparison_cases_quoname]] < 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[comparison_cases_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some case values in {comparison_cases_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[comparison_cases_quoname]])), collapse = ', ')}.
      ")
    )
  }

  # comparison_ns
  esci_assert_valid_column_name(data, comparison_ns_quoname)
  esci_assert_column_type(data, comparison_ns_quoname, "is.numeric")
  if (!all(data[[comparison_ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {comparison_ns_quoname} are 0 or less.
These are rows {paste(which(data[[comparison_ns_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[comparison_ns_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {comparison_ns_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[comparison_ns_quoname]])), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_ns_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # reference_cases
  esci_assert_valid_column_name(data, reference_cases_quoname)
  esci_assert_column_type(data, reference_cases_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_cases_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  if (!all(data[[reference_cases_quoname]] >= 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some case values in {reference_cases_quoname} are < 0.
These are rows {paste(which(data[[reference_cases_quoname]] < 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[reference_cases_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some case values in {reference_cases_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[reference_cases_quoname]])), collapse = ', ')}.
      ")
    )
  }

  # reference_ns
  esci_assert_valid_column_name(data, reference_ns_quoname)
  esci_assert_column_type(data, reference_ns_quoname, "is.numeric")
  if (!all(data[[reference_ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {reference_ns_quoname} are 0 or less.
These are rows {paste(which(data[[reference_ns_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[reference_ns_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {reference_ns_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[reference_ns_quoname]])), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_ns_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # labels
  if (is.null(labels_quoname)) {
    data$esci_label <- paste("Study", seq(1:nrow(data)))
    labels_quoname <- "esci_label"
  } else {
    esci_assert_valid_column_name(data, labels_quoname)
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    labels_quoname,
    lower = 1,
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # moderator
  moderator <- !is.null(moderator_quoname)
  if (moderator) {
    esci_assert_valid_column_name(data, moderator_quoname)
    row_report <- esci_assert_column_has_valid_rows(
      data,
      moderator_quoname,
      lower = 1,
    )
    if (row_report$missing > 0) {
      warnings <- c(warnings, row_report$warning)
      warning(row_report$warning)
      data <- data[-row_report$NA_rows, ]
    }
  }

  # Check options
  reported_effect_size <- match.arg(reported_effect_size)
  effect_size_name <- switch(
    reported_effect_size,
    "RD" = "risk difference",
    "RR" = "log risk ratio",
    "OR" = "log odds ratio",
    "AS" = "arcsine square-root transformed risk difference",
    "PETO" = "log odds ratio estimated with Peto's method"
  )

  # All other checks happen in meta_any:
  # * additional constraints on moderator
  # * contrast
  # * random_effects
  # * conf_level


  # Data prep------------------------------------------

  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    comparison_cases_quoname,
    comparison_ns_quoname,
    reference_cases_quoname,
    reference_ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of canonical column names
  numeric_cols <- c(
    "comparison_cases",
    "comparison_N",
    "reference_cases",
    "reference_N"
  )
  col_names <- c(
    "label",
    numeric_cols,
    if (moderator) "moderator"
  )

  # reduce data down to just needed columns with cannonical names
  data <- data[ , just_cols]
  colnames(data) <- col_names


  # Calculations -------------------------------------------------
  # Get yi and vi for raw scores
  es_data <- as.data.frame(
    t(
      apply(
        X = data[ , numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_prop2,
        conf_level = conf_level
      )
    )
  )
  better_es <- metafor::escalc(
    measure = reported_effect_size,
    data =  data,
    ai = comparison_cases,
    ci = reference_cases,
    n1i = comparison_N,
    n2i = reference_N
  )
  better_es_CIs <- metafor::summary.escalc(better_es)
  es_data$yi <- better_es$yi
  es_data$vi <- better_es$vi
  es_data$p <- better_es_CIs$pval

  if (reported_effect_size != "RD") {
    es_data$LL <- better_es_CIs$ci.lb
    es_data$UL <- better_es_CIs$ci.ub
  }


  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = effect_size_name,
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    contrast = contrast,
    random_effects = random_effects,
    conf_level = conf_level
  )

  # Clean up -----------------------------
  clear_cols <- c(
    "label",
    "moderator"
  )
  data[ , clear_cols] <- NULL
  data$p <- es_data$p
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)

  # Effect size labels
  # res$properties$effect_size_name <- "P_diff"
  # res$properties$effect_size_name_html <- "<i>P</i><sub>diff</sub>"
  # res$properties$effect_size_name_ggplot <- "*P*<sub>diff</sub>"

  # Effect size labels
  res$properties$effect_size_name <- switch(
    reported_effect_size,
    "RD" = "P_diff",
    "RR" = "log_risk_ratio",
    "OR" = "log_odds_ratio",
    "AS" = "arcsine(sqrt(P_diff))",
    "PETO" = "log_odds_ratio_Peto_method"
  )
  res$properties$effect_size_name_html <- switch(
    reported_effect_size,
    "RD" = "<i>P</i><sub>diff</sub>",
    "RR" = "ln(<i>RR</i>)",
    "OR" = "ln(<i>OR</i>)",
    "AS" = "1/2 * <i>d</i><sub><i>h</i></sub>",
    "PETO" = "ln(<i>OR</i>)<sub>Peto</sub>"
  )
  res$properties$effect_size_name_ggplot <- switch(
    reported_effect_size,
    "RD" = "*P*<sub>diff</sub>",
    "RR" = "Log Risk Ratio",
    "OR" = "Log Odds Ratio",
    "AS" = "Arcsine Square-Root Transformed Risk Difference",
    "PETO" = "Log Odds Ratio, Peto's method"
  )

  return(res)
}


