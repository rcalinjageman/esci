#' Estimate meta-analytic d_s or d_avg across multiple two group studies.
#'
#' @description
#' `meta_smd_two` is suitable for synthesizing across multiple
#' two-group studies where the outcome variable is continuous but not all
#' studies are measured on the same scale.
#'
#' @details
#' Each study's effect size should be expressed as d_s or d_avg.  The function
#' [esci::estimate_mdiff_two()] can provide these effect sizes from raw data.
#'
#' The meta-analytic effect size, confidence interval and heterogeneity
#' estimates all come from [metafor::rma()].
#'
#' The diamond ratio and its confidence interval come from
#' [esci::CI_diamond_ratio()].
#'
#' @param data A data frame or tibble
#' @param smds Name of a column in data containing Cohen's d for each study
#' @param comparison_ns Name of a column in data containing the sample size for the comparison group in each study; each should be an integer > 0.
#' @param reference_ns Name of a column in data containing the sample size for the reference group in each study; each should be an integer > 0.
#' @param labels Name of a column in data containing a label for each study
#' @param moderator Optional name of a column in data containing a factor as a
#'   categorical moderator
#' @param contrast Optional vector specifying a contrast analysis for the
#'   categorical moderator.  Only define if a moderator is defined; vector
#'   length should match number of levels in the moderator
#' @param effect_label Optional human-friendly name for the effect being
#'   synthesized; defaults to 'My effect'
#' @param assume_equal_variance TRUE to assume equal variance, meaning effect sizes provided are d_s; FALSE to not assume equal variance, meaning effect sizes provided as d_avg
#' @param correct_bias TRUE to apply bias correction to effect sizes provided before conducting the meta-analysis
#' @param esci_vi TRUE to report the sampling variance based on Bonnett rather than metafor
#' @param random_effects Use TRUE to obtain a random effect meta-anlaysis
#'   (usually reccomended); FALSE for fixed effect.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#'
#' @inherit meta_any return
#'
#'
#' @export
meta_smd_two <- function(
  data,
  smds,
  comparison_ns,
  reference_ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  assume_equal_variance = FALSE,
  correct_bias = TRUE,
  esci_vi = FALSE,
  random_effects = TRUE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  smds_enquo        <-  rlang::enquo(smds)
  smds_quoname      <-  rlang::quo_name(smds_enquo)

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

  # Input checks --------------------------------
  # * data must be a data frame
  #    all rows with an NA a parameter column will be dropped, warning issued
  # * the column smds must exist and be numeric,
  #    with > 1 row after NAs removed
  # * the column comparison_ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column reference_ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * random_effects must be a logical, TRUE or FALSE, checked in meta_any
  # * assume_equal_variance must be logical
  # * correct_bias must be logical
  # * esci_vi must be logical
  # * conf_level must be a numeric >0 and < 1, checked in meta_any


  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")

  # smds
  esci_assert_valid_column_name(data, smds_quoname)
  esci_assert_column_type(data, smds_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    smds_quoname,
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
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
    lower = 2,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # comparison_ns
  esci_assert_valid_column_name(data, comparison_ns_quoname)
  esci_assert_column_type(data, comparison_ns_quoname, "is.numeric")
  if (!all(data[[comparison_ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sample-size values in {comparison_ns_quoname} are 0 or less.
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
  esci_assert_type(assume_equal_variance, "is.logical")
  esci_assert_type(correct_bias, "is.logical")
  esci_assert_type(esci_vi, "is.logical")

  # All other checks happen in meta_any:
  # * additional constraints on moderator
  # * contrast
  # * random_effects
  # * conf_level


  # Data prep------------------------------------------

  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    smds_quoname,
    reference_ns_quoname,
    comparison_ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "smd",
    "reference_n",
    "comparison_n"
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
  data$comparison_sd <- rep(1, times = nrow(data))
  data$reference_sd <- rep(1, times = nrow(data))
  data$reference_mean <- rep(0, times = nrow(data))
  data$comparison_mean <- data$smd
  smd_numeric_cols <- c(
    "reference_mean",
    "reference_sd",
    "reference_n",
    "comparison_mean",
    "comparison_sd",
    "comparison_n"
  )

  # Get yi and vi for raw scores
  es_data <- as.data.frame(
    t(
      apply(
        X = data[ , smd_numeric_cols],
        MARGIN = 1,
        FUN = apply_ci_stdmean_two,
        assume_equal_variance = assume_equal_variance,
        correct_bias = correct_bias,
        conf_level = conf_level
      )
    )
  )

  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = !!if (esci_vi) "vi_alt" else "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = if (correct_bias) "SMD_corrected" else "SMD",
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    contrast = contrast,
    random_effects = random_effects,
    conf_level = conf_level
  )

  # Clean up -----------------------------
  clear_cols <- c(
    "label",
    "moderator",
    "comparison_sd",
    "reference_sd",
    "reference_mean",
    "comparison_mean"
  )
  data[ , clear_cols] <- NULL
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)

  return(res)
}


