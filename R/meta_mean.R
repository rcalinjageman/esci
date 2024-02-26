#' Estimate a meta-analytic mean across multiple single-group studies.
#'
#'
#' @description
#' `meta_mean` is suitable for synthesizing across multiple single-group studies
#' with a continuous outcome variable when all studies are measured on the
#' same scale.
#'
#'
#' @details The meta-analytic effect size, confidence interval and heterogeneity
#' estimates all come from [metafor::rma()].
#'
#' The diamond ratio and its confidence interval come from
#' [esci::CI_diamond_ratio()].
#'
#' If reported_effect_size is smd_unbiased or smd the conversion to d1
#' is handled by [esci::CI_smd_one()].
#'
#'
#' @param data A dataframe or tibble
#' @param means A collection of study means, 1 per study
#' @param sds A collection of study standard deviations, 1 per study, all >0
#' @param ns A collection of sample sizes, 1 per study, all integers > 2
#'
#' @param labels An optional collection of study labels
#' @param moderator An optional factor to analyze as a categorical moderator,
#' must have k > 2 per groups
#' @param contrast An optional contrast to estimate between moderator levels;
#' express as a vector of contrast weights with 1 weight per moderator level.
#' @param effect_label Optional character giving a human-friendly name of
#' the effect being synthesized
#' @param reference_mean Optional reference mean, defaults to 0
#' @param reported_effect_size Character specifying effect size to return; Must
#' be one of 'mean_difference', 'smd_unbiased' (to return an unbiased Cohen's
#' d1) or 'smd' (to return Cohen's d1 without correction for bias)
#' @param random_effects TRUE for random effect model; FALSE for fixed effects
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#'
#' @inherit meta_any return
#'
#'
#' @examples
#' # example code
#'   original_7 <- data.frame(
#' study_name = c(
#'   "Aden (1993)"	,
#'   "Buggs (1995)"	,
#'   "Crazed (1999)"	,
#'   "Dudley (2003)"	,
#'   "Evers (2005)"	,
#'   "Fox (2009)",
#'   "Mine (2011)"
#' ),
#' rt_mean = c(
#'   454	,
#'   317	,
#'   430	,
#'   525	,
#'   479	,
#'   387,
#'   531
#' ),
#' rt_sd = c(
#'   142	,
#'   158	,
#'   137	,
#'   260	,
#'   144	,
#'   165,
#'   233
#' ),
#' rt_n = c(
#'   24	,
#'   7	,
#'   20	,
#'   8	,
#'  14	,
#'  13,
#'   18
#' ),
#' subset = as.factor(
#'   c(
#'     "90s",
#'    "90s",
#'     "90s",
#'     "00s",
#'     "00s",
#'     "00s",
#'     "00s"
#'   )
#' ),
#' d1_unbiased = c(
#'   3.091587,
#'   1.742751,
#'   3.012857,
#'   1.793487,
#'   3.130074,
#'   2.195209,
#'   2.17667
#' )
#' )
#'
#'
#' # Fixed effect, 95% CI
#' estimate <- esci::meta_mean(
#'   original_7,
#'   rt_mean,
#'   rt_sd,
#'   rt_n,
#'   study_name,
#'   random_effects = FALSE
#' )
#'
#' # Random effects, categorical moderator, report cohen's d1
#'   estimate <- esci::meta_mean(
#'     original_7,
#'     rt_mean,
#'     rt_sd,
#'     rt_n,
#'     study_name,
#'     subset,
#'     reported_effect_size = "smd_unbiased"
#'     )
#'
#' \dontrun{
#' # Forest plot
#' esci::plot_meta(estimate)
#' }
#'
#' @export
meta_mean <- function(
  data,
  means,
  sds,
  ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  reference_mean = 0,
  reported_effect_size = c("mean_difference", "smd_unbiased", "smd"),
  random_effects = TRUE,
  conf_level = .95
)  {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  means_enquo        <-  rlang::enquo(means)
  means_quoname      <-  rlang::quo_name(means_enquo)

  sds_enquo        <-  rlang::enquo(sds)
  sds_quoname      <-  rlang::quo_name(sds_enquo)

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
  # * the column sds must exist and be numeric > 0
  #    with > 1 row after NAs removed
  # * the column ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * reported_effect_size must be mean_difference, smd_unbiased, or smd
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
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }

  # sds
  esci_assert_valid_column_name(data, sds_quoname)
  esci_assert_column_type(data, sds_quoname, "is.numeric")
  if (!all(data[[sds_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some sd values in {sds_quoname} are 0 or less.
These are rows {paste(which(data[[sds_quoname]] <= 0), collapse = ', ')}.
      ")
    )
  }
  row_report <- esci_assert_column_has_valid_rows(
    data,
    sds_quoname,
    lower = 1,
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
  esci_assert_type(reference_mean, "is.numeric")
  reported_effect_size <- match.arg(reported_effect_size)
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
    means_quoname,
    sds_quoname,
    ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "mean",
    "sd",
    "n"
  )
  col_names <- c(
    "label",
    numeric_cols,
    if (moderator) "moderator"
  )

  # reduce data down to just needed columns with canonical names
  data <- data[just_cols]
  colnames(data) <- col_names


  # Calculations -------------------------------------------------
  # Get yi and vi for raw scores
  if (!report_smd) {
    es_data <- as.data.frame(
      t(
        apply(
          X = data[ , numeric_cols],
          MARGIN = 1,
          FUN = apply_ci_mean1,
          reference_mean = reference_mean,
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
          FUN = apply_ci_stdmean1,
          reference_mean = reference_mean,
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
    labels = "label",
    effect_label = effect_label,
    effect_size_name = reported_effect_size,
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
  res$properties$effect_size_name <- switch(
    reported_effect_size,
    "mean_difference" = "Mean",
    "smd" = "d1_biased",
    "smd_unbiased" = "d1_unbiased"
  )
  res$properties$effect_size_name_html <- switch(
    reported_effect_size,
    "mean_difference" = "Mean",
    "smd" = "<i>d</i><sub>1.biased</sub>",
    "smd_unbiased" = "<i>d</i><sub>1.unbiased</sub>"
  )
  res$properties$effect_size_name_ggplot <- switch(
    reported_effect_size,
    "mean_difference" = "*M*",
    "smd" = "*d*<sub>1.biased</sub>",
    "smd_unbiased" = "*d*<sub>1.unbiased</sub>"
  )

  return(res)
}


