#' Estimate a meta-analytic proportion of outcomes over multiple studies with
#' a categorical outcome variable.
#'
#'
#' @description
#' `meta_proportion` is suitable for synthesizing across multiple studies with
#' a categorical outcome variable.  It takes as input the number of cases/events
#' and the number of samples in each study.
#'
#'
#' @details
#' Once you generate an estimate with this function, you can visualize
#' it with [esci::plot_meta()].
#'
#' The meta-analytic effect size, confidence interval and heterogeneity
#' estimates all come from [metafor::rma()].
#'
#'
#'
#' @param data A dataframe or tibble
#' @param cases A collection of cases/event counts, 1 per study, all integers,
#'   all > 0
#' @param ns A collection of sample sizes, 1 per study, all integers > 2
#' @param labels An optional collection of study labels
#' @param moderator An optional factor to analyze as a categorical moderator,
#' must have k > 2 per groups
#' @param contrast An optional contrast to estimate between moderator levels;
#' express as a vector of contrast weights with 1 weight per moderator level.
#' @param effect_label Optional character giving a human-friendly name of
#' the effect being synthesized
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
#' estimate <- esci::meta_proportion(
#'   esci_meta_pdiff_two,
#'   power_egocentric,
#'   power_sample_size,
#'   studies
#' )
#'
#' \dontrun{
#' # Forest plot
#' esci::plot_meta(estimate)
#' }
#'
#'
#' # Meta-analysis, risk difference as effect size, moderator (setting)
#' estimate <- esci::meta_proportion(
#'   esci_meta_pdiff_two,
#'   power_egocentric,
#'   power_sample_size,
#'   studies,
#'   moderator = setting
#' )
#'
#' \dontrun{
#' # Forest plot
#' esci::plot_meta(estimate)
#' }
#'
#'
#'
#' @export
meta_proportion <- function(
  data,
  cases,
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
  cases_enquo        <-  rlang::enquo(cases)
  cases_quoname      <-  rlang::quo_name(cases_enquo)

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
  # * the column cases must exist and be numeric, integer >= 0
  #    with > 1 row after NAs removed
  # * the column ns must exist and be numeric integers > 0
  #    with > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional; checks happen in meta_any
  # * contrast should only be passed in moderator is defined; checks in meta_any
  # * effect_label should be a character, checked in meta_any
  # * conf_level must be a numeric >0 and < 1, checked in meta_any


  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")

  # cases
  esci_assert_valid_column_name(data, cases_quoname)
  esci_assert_column_type(data, cases_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    cases_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  if (!all(data[[cases_quoname]] >= 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some case values in {cases_quoname} are < 0.
These are rows {paste(which(data[[cases_quoname]] < 0), collapse = ', ')}.
      ")
    )
  }
  if (!all(is.whole.number(data[[cases_quoname]]), na.rm = TRUE)) {
    stop(
      glue::glue("
Some case values in {cases_quoname} are not integers.
These are rows {paste(which(!is.whole.number(data[[cases_quoname]])), collapse = ', ')}.
      ")
    )
  }

  # ns
  esci_assert_valid_column_name(data, ns_quoname)
  esci_assert_column_type(data, ns_quoname, "is.numeric")
  if (!all(data[[ns_quoname]] > 0, na.rm = TRUE)) {
    stop(
      glue::glue("
Some n values in {ns_quoname} are 0 or less.
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

  tdata <- data[ , c(cases_quoname, ns_quoname)]
  tdata <- tdata[complete.cases(tdata), ]
  if (!all(tdata[[ns_quoname]] >= tdata[[cases_quoname]])) {
    stop(
      glue::glue("
  Some sample sizes in {ns_quoname} are smaller than case counts in {cases_quoname}.
  These are rows {paste(which(tdata[[ns_quoname]] < tdata[[cases_quoname]]), collapse = ', ')}.
        ")
    )
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

  # All other checks happen in meta_any:
  # * additional constraints on moderator
  # * contrast
  # * random_effects
  # * conf_level


  # Data prep------------------------------------------

  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    cases_quoname,
    ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of canonical column names
  numeric_cols <- c(
    "cases",
    "N"
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
        FUN = apply_ci_prop1,
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
    effect_size_name = "P",
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
  data$P_adjusted <- es_data$P_adjusted
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)


  # Effect size labels
  res$properties$effect_size_name <- "P"
  res$properties$effect_size_name_html <- "<i>P</i>"
  res$properties$effect_size_name_ggplot <- "*P*"

  return(res)
}


