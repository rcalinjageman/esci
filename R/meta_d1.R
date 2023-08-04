#' Estimate ameta-analytic Cohen's d1 across multiple studies
#'
#' @description `meta_d1` is suitable for synthesizing across multiple
#' single-group studies with a continuous outcome variable, but where the
#' outcome is not measured on the same scale in all studies
#'
#' @details
#' Each study's effect size should be expressed as Cohen's
#' d1: (mean - reference) / sd.
#'
#' And the d1 values should all be corrected for bias.
#' The function [esci::CI_smd_one()] can assist with converting raw data from
#' each study to d1_unbiased.
#'
#' The meta-analytic effect size, confidence interval and heterogeneity
#' estimates all come from [metafor::rma()].
#'
#' The diamond ratio and its confidence interval come from
#' [esci::CI_diamond_ratio()].
#'
#'
#' @param data A data frame or tibble
#' @param ds Set of bias-adjusted cohen's d1 values, 1 for each study
#' @param ns Set of sample sizes, positive integers, 1 for each study
#' @param labels Optional set of labels, 1 for each study
#'
#' @param moderator Optional factor as a categorical moderator; should have k >
#'   2 per group
#' @param contrast Optional vector specifying a contrast between moderator
#'   levels
#' @param effect_label Optional character providing a human-friendly label for
#'   the effect
#' @param random_effects Boolean; TRUE for a random effects model; otherwise
#'   fixed effects
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#'
#' @inherit meta_any return
#'
#'
#' @export
meta_d1 <- function(
  data,
  ds,
  ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  random_effects = TRUE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  means_enquo        <-  rlang::enquo(ds)
  means_quoname      <-  rlang::quo_name(means_enquo)

  ns_enquo        <-  rlang::enquo(ns)
  ns_quoname      <-  rlang::quo_name(ns_enquo)

  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  if (moderator_quoname == "NULL") moderator_quoname <- NULL

  labels_enquo        <-  rlang::enquo(labels)
  labels_quoname      <-  rlang::quo_name(labels_enquo)
  if (labels_quoname == "NULL") labels_quoname <- NULL

  warnings <- NULL

  # Input checks --------------------------------
  # * data must be a data frame
  #    all rows with an NA a parameter column will be dropped, warning issued
  # * the column means must exist and be numeric,
  #    with > 1 row after NAs removed
  # * the column ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * random_effect must be a logical, TRUE or FALSE, checked in meta_any
  # * conf_level must be a numeric >0 and < 1, checked in meta_any

  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")

  # means
  esci_assert_valid_column_name(data, means_quoname)
  esci_assert_column_type(data, means_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    means_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # ns
  esci_assert_valid_column_name(data, ns_quoname)
  esci_assert_column_type(data, ns_quoname, "is.numeric")
  if (!all(data[[ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sample-size values in {ns_quoname} are 0 or less.
These are rows {paste(which(data[[ns_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[ns_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {ns_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[ns_quoname]])), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    ns_quoname,
    lower = 2,
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
    lower = 2,
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
      lower = 2,
    )
    if (row_report$missing > 0) {
      warnings <- c(warnings, row_report$warning)
      warning(row_report$warning)
      data <- data[-row_report$NA_rows, ]
    }
  }

  # Check options
  report_smd <- TRUE
  correct_bias <- FALSE


  # All other checks happen in meta_any
  # * additional constraints on moderator
  # * contrast
  # * effect_label
  # * random_effects
  # * conf_level


  # Data prep------------------------------------------
  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    means_quoname,
    ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "mean",
    "n"
  )
  col_names <- c(
    "label",
    numeric_cols,
    if (moderator) "moderator",
    "sd"
  )

  # reduce data down to just needed columns with canonical names
  data <- data[just_cols]
  data$sd <- 1
  sds_quoname <- "sd"
  colnames(data) <- col_names
  numeric_cols <- c(numeric_cols, sds_quoname)


  # Calculations -------------------------------------------------
  # Get yi and vi for raw scores
  es_data <- as.data.frame(
    t(
      apply(
        X = data[ , numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_stdmean1,
        reference_mean = 0,
        correct_bias = correct_bias,
        conf_level = conf_level
      )
    )
  )


  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = "d1",
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    random_effects = random_effects,
    conf_level = conf_level
  )

  data$label <- NULL
  data$moderator <- NULL
  data$p <- es_data$p
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)

  # Effect size labels
  res$properties$effect_size_name <- "d1_unbiased"
  res$properties$effect_size_name_html <- "<i>d</i><sub>1.unbiased</sub>"
  res$properties$effect_size_name_ggplot <- "*d*<sub>1.unbiased</sub>"


  return(res)
}


