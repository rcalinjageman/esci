#' Estimate meta-analytic difference in magnitude between two ind. groups
#'
#' @description
#' `meta_mdiff_two` returns
#'
#'
#' @param data A dataframe or tibble
#' @param comparison_means comparison
#' @param comparison_sds comparison
#' @param comparison_n comparison
#' @param reference_means reference
#' @param reference_sds reference
#' @param reference_ns reference
#' @param r optional correlation between measures for w-s studies, NA otherwise
#' @param labels labels
#' @param moderator mod
#' @param contrast contrast
#' @param effect_label el
#' @param reported_effect_size smd
#' @param random_effects re
#' @param assume_equal_variance aev
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @export
meta_mdiff_two <- function(
  data,
  comparison_means,
  comparison_sds,
  comparison_ns,
  reference_means,
  reference_sds,
  reference_ns,
  r = NULL,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  reported_effect_size = c("mean_difference", "smd_unbiased", "smd"),
  assume_equal_variance = FALSE,
  random_effects = TRUE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  comparison_means_enquo        <-  rlang::enquo(comparison_means)
  comparison_means_quoname      <-  rlang::quo_name(comparison_means_enquo)

  reference_means_enquo        <-  rlang::enquo(reference_means)
  reference_means_quoname      <-  rlang::quo_name(reference_means_enquo)

  comparison_sds_enquo        <-  rlang::enquo(comparison_sds)
  comparison_sds_quoname      <-  rlang::quo_name(comparison_sds_enquo)

  reference_sds_enquo        <-  rlang::enquo(reference_sds)
  reference_sds_quoname      <-  rlang::quo_name(reference_sds_enquo)

  comparison_ns_enquo        <-  rlang::enquo(comparison_ns)
  comparison_ns_quoname      <-  rlang::quo_name(comparison_ns_enquo)

  reference_ns_enquo        <-  rlang::enquo(reference_ns)
  reference_ns_quoname      <-  rlang::quo_name(reference_ns_enquo)

  r_enquo        <-  rlang::enquo(r)
  r_quoname      <-  rlang::quo_name(r_enquo)
  if (r_quoname == "NULL") r_quoname <- NULL

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
  # * the column comparison_means must exist and be numeric,
  #    with > 1 row after NAs removed
  # * the column reference_means must exist and be numeric,
  #    with > 1 row after NAs removed
  # * the column comparison_sds must exist and be numeric > 0
  #    with > 1 row after NAs removed
  # * the column reference_sds must exist and be numeric > 0
  #    with > 1 row after NAs removed
  # * the column comparison_ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column reference_ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column r is optional but if passed must be numeric fro -1 to 1 or NA
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * reported_effect_size must be mean_difference, smd_unbiased, or smd
  # * random_effect must be a logical, TRUE or FALSE, checked in meta_any
  # * assume_equal_variance must be logical
  # * conf_level must be a numeric >0 and < 1, checked in meta_any

  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")

  # reference_means
  esci_assert_valid_column_name(data, reference_means_quoname)
  esci_assert_column_type(data, reference_means_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_means_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # comparison_means
  esci_assert_valid_column_name(data, comparison_means_quoname)
  esci_assert_column_type(data, comparison_means_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_means_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # reference_sds
  esci_assert_valid_column_name(data, reference_sds_quoname)
  esci_assert_column_type(data, reference_sds_quoname, "is.numeric")
  if (!all(data[[reference_sds_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sd values in {reference_sds_quoname} are 0 or less.
These are rows {paste(which(data[[reference_sds_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    reference_sds_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # comparison_sds
  esci_assert_valid_column_name(data, comparison_sds_quoname)
  esci_assert_column_type(data, comparison_sds_quoname, "is.numeric")
  if (!all(data[[comparison_sds_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sd values in {comparison_sds_quoname} are 0 or less.
These are rows {paste(which(data[[comparison_sds_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    comparison_sds_quoname,
    lower = 1,
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
    lower = 1,
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
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }


  # r
  if (!is.null(r_quoname)) {
    esci_assert_valid_column_name(data, r_quoname)
    esci_assert_column_type(data, r_quoname, "is.numeric")
    if (!all(data[[r_quoname]] >= -1, na.rm = TRUE)) {
      stop(
        glue::glue("
Some r values in {r_quoname} are < -1.
These are rows {paste(which(data[[r_quoname]] < -1), collapse = ', ')}.
      ")
      )
    }

    if (!all(data[[r_quoname]] <= 1, na.rm = TRUE)) {
      stop(
        glue::glue("
Some r values in {r_quoname} are > 1.
These are rows {paste(which(data[[r_quoname]] > 1), collapse = ', ')}.
      ")
      )
    }

    check_n <- data[!is.na(data[[r_quoname]]), ]
    if (!all(check_n[[reference_ns_quoname]] == check_n[[comparison_ns_quoname]])) {
      stop(
        glue::glue("
Some studies are passed with r but with n for reference and comparison not set the same.
The rows with r but mismatching n are:
{paste(check_n[which(check_n[[reference_ns_quoname]] != check_n[[comparison_ns_quoname]]), ], collapse = ', ')}.
      ")
      )

    }
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
  esci_assert_type(assume_equal_variance, "is.logical")

  report_smd <- reported_effect_size != "mean_difference"
  correct_bias <- reported_effect_size == "smd_unbiased"

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
    reference_means_quoname,
    reference_sds_quoname,
    reference_ns_quoname,
    comparison_means_quoname,
    comparison_sds_quoname,
    comparison_ns_quoname,
    if (!is.null(r_quoname)) r_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "reference_mean",
    "reference_sd",
    "reference_n",
    "comparison_mean",
    "comparison_sd",
    "comparison_n",
    if (!is.null(r_quoname)) "r"
  )
  col_names <- c(
    "label",
    numeric_cols,
    if (moderator) "moderator"
  )

  # reduce data down to just needed columns with canonical names
  data <- data[ , just_cols]
  colnames(data) <- col_names


  # Calculations -------------------------------------------------
  # Get yi and vi for raw scores
  if (!report_smd) {
    es_data <- as.data.frame(
      t(
        apply(
          X = data[ , numeric_cols],
          MARGIN = 1,
          FUN = apply_ci_mdiff,
          assume_equal_variance = assume_equal_variance,
          conf_level = conf_level
        )
      )
    )
  } else {
    es_data <- as.data.frame(
      t(
        apply(
          X = data[ , numeric_cols],
          MARGIN = 1,
          FUN = apply_ci_stdmean_two,
          assume_equal_variance = assume_equal_variance,
          correct_bias = correct_bias,
          conf_level = conf_level
        )
      )
    )

  }


  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    contrast = contrast,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = reported_effect_size,
    moderator_variable_name = if (moderator) moderator_quoname else "My moderator",
    random_effects = random_effects,
    conf_level = conf_level
  )

  data$label <- NULL
  data$moderator <- NULL
  data$df <- es_data$df
  data$p <- es_data$p
  if (is.null(r_quoname)) data$r <- NA

  es_data_include <- c("LL", "UL")


  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)


  # Effect size labels


  res$properties$effect_size_name <- switch(
    reported_effect_size,
    "mean_difference" = "Mean_diff",
    "smd" = if(assume_equal_variance) "d_s.biased" else "d_avg.biased",
    "smd_unbiased" = if(assume_equal_variance)"d_s" else "d_avg"
  )
  res$properties$effect_size_name_html <- switch(
    reported_effect_size,
    "mean_difference" = "Mean<sub>diff</sub>",
    "smd" = if(assume_equal_variance) "<i>d</i><sub>s.biased</sub>" else "<i>d</i><sub>avg.biased</sub>",
    "smd_unbiased" = if(assume_equal_variance) "<i>d</i><sub>s</sub>" else "<i>d</i><sub>avg</sub>"
  )
  res$properties$effect_size_name_ggplot <- switch(
    reported_effect_size,
    "mean_difference" = "*M*<sub>diff</sub>",
    "smd_unbiased" = if(assume_equal_variance) "*d*<sub>s</sub>" else "*d*<sub>avg</sub>",
    "smd" = if(assume_equal_variance) "*d*<sub>s.biased</sub>" else "*d*<sub>avg.biased</sub>"
  )


  return(res)
}


