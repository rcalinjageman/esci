#' Estimate meta-analytic Pearson's r across multiple studies with two
#' continuous outcome variables.
#'
#'
#' @description
#' `meta_r` is suitable for synthesizing across multiple studies that have
#' measured a linear correlation (Pearson's r) from two continuous variables.
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
#' @param data A dataframe or tibble
#' @param rs A collection of Pearson's r values, 1 per study, all between
#'   -1 and 1, inclusive
#' @param ns A collection of study sample sizes, all integers > 2
#' @param labels labels
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
#' @inherit meta_any return
#'
#'
#' @examples
#' # Data: See Introduction to the New Statistics, first edition
#' esci_single_r <- data.frame(
#'   studies = c(
#'     'Violin, viola'	,
#'     'Strings'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'All'	,
#'     'Piano'	,
#'     'Piano'	,
#'     'Band'	,
#'     'Music majors'	,
#'     'Music majors'	,
#'     'All'
#'   ),
#'   rvalues = c(
#'     .67,
#'     .51,
#'     .4,
#'     .46,
#'     .47,
#'     .228,
#'     -.224,
#'     .104,
#'     .322,
#'     .231,
#'     .67,
#'     .41,
#'     .34,
#'     .31,
#'     .54,
#'     .583
#'   ),
#'   sample_size = c(
#'     109,
#'     55,
#'     19,
#'     30,
#'     19,
#'     52,
#'     24,
#'     52,
#'     16,
#'     97,
#'     57,
#'     107,
#'     178,
#'     64,
#'     19,
#'     135
#'   ),
#'   subsets = as.factor(
#'     c(
#'       'Strings'	,
#'       'Strings'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Piano'	,
#'       'Strings'	,
#'       'Strings'	,
#'       'Strings'	,
#'       'Strings'
#'     )
#'  )
#' )
#'
#' # Meta-analysis, random effects, no moderator
#' estimate <- esci::meta_r(
#'   esci_single_r,
#'   rvalues,
#'   sample_size,
#'   studies,
#'   random_effects = TRUE
#' )
#'
#' # Forest plot
#' myplot_forest <- esci::plot_meta(estimate)
#'
#'
#' # Meta-analysis, random effects, moderator (subsets)
#' estimate_moderator <- esci::meta_r(
#'   esci_single_r,
#'   rvalues,
#'   sample_size,
#'   studies,
#'   subsets,
#'   random_effects = TRUE
#' )
#'
#' # Forest plot
#' myplot_forest_moderator <- esci::plot_meta(estimate_moderator)
#'
#'
#' @export
meta_r <- function(
  data,
  rs,
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
  rs_enquo        <-  rlang::enquo(rs)
  rs_quoname      <-  rlang::quo_name(rs_enquo)

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
  # * the column r must exist and be numeric, >= -1 and <= -1
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

  # rs
  esci_assert_valid_column_name(data, rs_quoname)
  esci_assert_column_type(data, rs_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    rs_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  if (!all(data[[rs_quoname]] >= -1, na.rm = TRUE)) {
    stop(
      glue::glue("
Some r values in {rs_quoname} are < -1.
These are rows {paste(which(data[[rs_quoname]] < -1), collapse = ', ')}.
      ")
    )
  }
  if (!all(data[[rs_quoname]] <= 1, na.rm = TRUE)) {
    stop(
      glue::glue("
Some r values in {rs_quoname} are > 1.
These are rows {paste(which(data[[rs_quoname]] > 1), collapse = ', ')}.
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
    rs_quoname,
    ns_quoname,
    if (moderator) moderator_quoname
  )

  # vector of cannonical column names
  numeric_cols <- c(
    "r",
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
        FUN = apply_ci_cor,
        conf_level = conf_level
      )
    )
  )

  eso <- metafor::escalc(ri = data$r, ni = data$N, measure = "ZCOR")
  es_data$yi <- eso$yi
  es_data$vi <- eso$vi

  res <- meta_any(
    data = cbind(data, es_data),
    yi = "yi",
    vi = "vi",
    moderator = !!if (moderator) "moderator" else NULL,
    labels = "label",
    effect_label = effect_label,
    effect_size_name = "r",
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
  res$raw_data <- cbind(res$raw_data, es_data[ , c("LL", "UL")], data)
  res$warnings <- c(res$warnings, warnings)

  res$es_meta$z <- res$es_meta$effect_size
  res$es_meta$effect_size <- esci_z_to_r(res$es_meta$effect_size)
  res$es_meta$LL <- esci_z_to_r(res$es_meta$LL)
  res$es_meta$UL <- esci_z_to_r(res$es_meta$UL)
  res$es_meta$PI_LL <- esci_z_to_r(res$es_meta$PI_LL)
  res$es_meta$PI_UL <- esci_z_to_r(res$es_meta$PI_UL)
  res$es_meta$FE_effect_size <- esci_z_to_r(res$es_meta$FE_effect_size)
  res$es_meta$RE_effect_size <- esci_z_to_r(res$es_meta$RE_effect_size)

  res$raw_data$z <- res$raw_data$effect_size
  res$raw_data$effect_size <- esci_z_to_r(res$raw_data$effect_size)
  res$raw_data$df <- res$raw_data$N - 2
  res$raw_data$t <- (res$raw_data$r) / sqrt( (1 - res$raw_data$r^2) / (res$raw_data$df) )
  res$raw_data$p <- 2*stats::pt(q=abs(res$raw_data$t), df=22, lower.tail=FALSE)

  if (!is.null(res$es_meta_difference)) {
    res$es_meta_difference$z <- res$es_meta_difference$effect_size
    res$es_meta_difference$effect_size <- esci_z_to_r(res$es_meta_difference$effect_size)
    res$es_meta_difference$LL <- esci_z_to_r(res$es_meta_difference$LL)
    res$es_meta_difference$UL <- esci_z_to_r(res$es_meta_difference$UL)
    cor1 <- res$es_meta_difference$effect_size[1]
    cor2 <- res$es_meta_difference$effect_size[2]
    ll1 <- res$es_meta_difference$LL[1]
    ll2 <- res$es_meta_difference$LL[2]
    ul1 <- res$es_meta_difference$UL[1]
    ul2 <- res$es_meta_difference$UL[2]
    diff <- cor1 - cor2
    res$es_meta_difference$effect_size[3] <- diff
    res$es_meta_difference$LL[3] <- diff - sqrt((cor1 - ll1)^2 + (ul2 - cor2)^2)
    res$es_meta_difference$UL[3] <- diff + sqrt((ul1 - cor1)^2 + (cor2 - ll2)^2)

  }


  # Effect size labels
  res$properties$effect_size_name <- "r"
  res$properties$effect_size_name_html <- "<i>r</i>"
  res$properties$effect_size_name_ggplot <- "*r*"

  return(res)
}


esci_z_to_r <- function(z) {
  if (is.null(z)) return(NULL)
  return(
    (exp(2*z) - 1) / (exp(2*z) + 1)
  )
}

