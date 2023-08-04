#' Estimate any meta effect.
#'
#' @description `meta_any` is suitable for synthesizing any effect size across
#' multiple studies.  You must provide the effect size for each study and the
#' predicted sampling variance for each study.
#'
#'
#' @details The meta-analytic effect size, confidence interval and heterogeneity
#' estimates all come from [metafor::rma()].
#'
#' The diamond ratio and its confidence interval come from
#' [esci::CI_diamond_ratio()].
#'
#'
#' @param data A data frame or tibble with columns
#' @param yi Name a column in data containing the effect size for each study
#' @param vi Name of a column in data containing the expected sampling variance
#'   for each study
#'
#' @param labels Name of a column in data containing a label for each study
#' @param moderator Optional name of a column in data containing a factor as a
#'   categorical moderator
#' @param contrast Optional vector specifying a contrast analysis for the
#'   categorical moderator.  Only define if a moderator is defined; vector
#'   length should match number of levels in the moderator
#' @param effect_label Optional human-friendly name for the effect being
#'   synthesized; defaults to 'My effect'
#' @param effect_size_name Optional human-friendly name of the effect size being
#'   synthesized; defaults to 'Effect size'
#' @param moderator_variable_name Optional human-friendly name of the moderator,
#'   if defined; If not passed but a moderator is defined, will be set to the
#'   quoted name of the moderator column or 'My moderator'
#' @param random_effects Use TRUE to obtain a random effect meta-anlaysis
#'   (usually reccomended); FALSE for fixed effect.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#' @returns An esci-estimate object; a list of data frames and properties.
#' Returned tables include:
#' * es_meta - A data frame with one overall.  If a moderator was
#' defined, there is an additional row for each level of the moderator.
#'   * effect_label -  Study label
#'   * effect_size - Effect size
#'   * LL - Lower bound of conf_level% confidence interval
#'   * UL - Upper bound of conf_level% confidence interval
#'   * SE - Expected standard error
#'   * k - Number of studies
#'   * diamond_ratio - ratio of random to fixed effects meta-analtics effect sizes
#'   * diamond_ratio_LL - lower bound of conf_level% confidence interval for diamond ratio
#'   * diamond_ratio_UL - upper bound of conf_level% confidence interval for diamond ratio
#'   * I2 -  I2 measure of heterogeneity
#'   * I2_LL - Lower bound of conf_level% confidence interval for I2
#'   * I2_UL - upper bound of conf_level% confidence interval for I2
#'   * PI_LL - lower bound of conf_level% of prediction interval
#'   * PI_UL - upper bound of conf_level% of prediction interval
#'   * p - p value for the meta-analytic effect size, based on null of exactly 0
#'   * width - width of the effect-size confidence interval
#'   * FE_effect_size - effect size of the fixed-effects model (regardless of if fixed effects was selected
#'   * RE_effect_size - effect size of the random-effects model (regardless of if random effects was selected
#'   * FE_CI_width - width of the fixed-effects confidence interval, used to calculate diamond ratio
#'   * RE_CI_width - width of the fixed-effects confidence interval, used to calculate diamond ratio
#' * es_heterogeneity - A data frame of of heterogeneity values and
#' conf_level% CIs for the meta-analytic effect size.  If a moderator was defined
#' also reports heterogeneity estimates for each level of the moderator.
#'   * effect_label - study label
#'   * moderator_variable_name - if moderator passed, gives name of the moderator
#'   * moderator_variable_name - 'Overall' and each level of moderator, if passed
#'   * measure - Name of the measure of heterogeneity
#'   * LL - lower bound of conf_level% confidence interval
#'   * UL - upper bound of conf_level% confidence interval
#' * raw_data - A data from with one row for each study that was passed
#'   * label - study label
#'   * effect_size - effect size
#'   * weight - study weight in the meta analysis
#'   * sample_variance - expected level of sampling variation
#'   * SE - expected standard error
#'   * LL - lower bound of conf_level% confidence interval
#'   * UL - upper bound of conf_level% confidence interval
#'   * mean - used to calculate study p value; this is the d value entered for the study
#'   * n - study sample size
#'   * sd - use to calculate study p value; set to 1 for each study
#'   * p - p value for the study, based on null of exactly 0
#'
#' @export
meta_any <- function(
  data,
  yi,
  vi,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  effect_size_name = "Effect size",
  moderator_variable_name = "My moderator",
  random_effects = TRUE,
  conf_level = 0.95
) {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  yi_enquo        <-  rlang::enquo(yi)
  yi_quoname      <-  rlang::quo_name(yi_enquo)

  vi_enquo        <-  rlang::enquo(vi)
  vi_quoname      <-  rlang::quo_name(vi_enquo)

  yi_enquo        <-  rlang::enquo(yi)
  yi_quoname      <-  rlang::quo_name(yi_enquo)

  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  if (moderator_quoname == "NULL") moderator_quoname <- NULL

  labels_enquo        <-  rlang::enquo(labels)
  labels_quoname      <-  rlang::quo_name(labels_enquo)
  if (labels_quoname == "NULL") labels_quoname <- NULL

  warnings <- NULL


  # Input checks --------------------------------
  # * data must be a data frame
  #    all rows with an NA in yi or vi will be dropped, warning issued
  #    if labels passed, rows with NA labels will be dropped, warning issued
  #    if moderator defined, rows with NA modrator will be dropped, warning issued
  # * the column yi must exist and be numeric, > 1 row after NAs removed
  # * the column vi must exist and be numeric, > 1 row after NAs removed
  # * the column labels is optional, but if passed must exist and
  #    have > 1 row after NAs removed
  # * the column moderator is optional, but if passed must exist and be a factor
  #     must be > 2 levels in the factor
  #     must have > 2 cases per level after NAs removed
  # * contrast should only be passed in moderator is defined
  #    if passed, must be numeric vector of same length as levels(moderator)
  #    and must have no NA values
  # * effect_label should be a character
  # * effect_size_name should be a character
  # * moderator_variable_name should be a character, even if moderator not passed
  # * random_effect must be a logical, TRUE or FALSE
  # * conf_level must be a numeric >0 and < 1

  # Check that data is a data.frame
  esci_assert_type(data, "is.data.frame")
  # yi
  esci_assert_valid_column_name(data, yi_quoname)
  esci_assert_column_type(data, yi_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    yi_quoname,
    lower = 1,
    na.rm = TRUE
  )
  if (row_report$missing > 0) {
    warnings <- c(warnings, row_report$warning)
    warning(row_report$warning)
    data <- data[-row_report$NA_rows, ]
  }
  # vi
  esci_assert_valid_column_name(data, vi_quoname)
  esci_assert_column_type(data, vi_quoname, "is.numeric")
  row_report <- esci_assert_column_has_valid_rows(
    data,
    vi_quoname,
    lower = 1,
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
    esci_assert_column_type(data, moderator_quoname, "is.factor")
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
    mlevels <- levels(data[[moderator_quoname]])
    if (length(mlevels) < 2) stop(
      glue::glue("
Moderator must have 2 or more levels, but has {length(mlevels)}, which are {mlevels}.
      ")
    )

    level_counts <- aggregate(
      data[[moderator_quoname]],
      by = list(data[[moderator_quoname]]),
      drop = FALSE,
      FUN = length
    )

    bad_levels <- c(
      which(level_counts[, 2] < 2),
      which(is.na(level_counts[, 2]))
    )

    if (length(bad_levels) > 0) {
      bmess <- paste(level_counts[ , 1], level_counts[ , 2], collapse = ", ")
      stop(
        glue::glue("
Invalid moderator data.  Each moderator level must have 2 or more cases.
After dropping any NA rows, current data has:
{bmess}.
        ")
      )
    }

  }

  if (nrow(data) < 2) stop(
"Not enough valid data: < 2 rows remain after dropping rows with NA values."
  )

  if (!is.null(contrast)) {
    if (!moderator) stop (
      "You have passed a contrast, but not a moderator.  The contrast parameter
      only applies if a moderator is defined."
    )
    esci_assert_type(contrast, "is.numeric")
    esci_assert_vector_valid_length(
      contrast,
      lower = length(level_counts[ , 1]),
      upper = length(level_counts[ , 1]),
      lower_inclusive = TRUE,
      upper_inclusive = TRUE,
      na.rm = TRUE,
      na.invalid = TRUE
    )
  }

  # Check labels
  esci_assert_type(effect_label, "is.character")
  esci_assert_type(effect_size_name, "is.character")
  if (moderator) {
    if (is.null(moderator_variable_name) | moderator_variable_name == "My moderator") {
      moderator_variable_name <- moderator_quoname
    } else {
      esci_assert_type(moderator_variable_name, "is.character")
    }
  }

  # Check options
  esci_assert_type(random_effects, "is.logical")

  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )


  # Data prep------------------------------------------
  # vector of passed column names
  just_cols <- c(
    labels_quoname,
    yi_quoname,
    vi_quoname,
    if (moderator) moderator_quoname
  )

  # vector of canonical column names
  numeric_cols <- c(
    "yi",
    "vi"
  )
  col_names <- c(
    "label",
    numeric_cols,
    if (moderator) "moderator"
  )

  # reduce data down to just needed columns with canonical names
  data <- data[ , just_cols]
  colnames(data) <- col_names


  # Calculations --------------------------------------------------
  # We now have a prepped data frame with columns
  # label, yi, vi, (moderator).  Ready for analysis

  # Do both fixed and random effects overall meta-analaysis
  FE <- metafor::rma(
    data = data,
    yi = yi,
    vi = vi,
    method="FE",
    level = conf_level * 100
  )
  RE <- metafor::rma(
    data = data,
    yi = yi,
    vi = vi,
    method="DL",
    level = conf_level * 100
  )

  # Heterogeneity
  es_heterogeneity <- meta_to_heterogeneity(
    RE = RE,
    FE = FE,
    effect_label = effect_label,
    moderator_variable_name = moderator_variable_name,
    moderator_level = "Overall",
    vi = data$vi,
    conf_level = conf_level
  )

  # Calculate diamond ratio and handles for results
  dr_res <- CI_diamond_ratio(RE, FE, data$vi, conf_level = conf_level)
  diamond_ratio <- dr_res$diamond_ratio
  diamond_ratio_LL <- dr_res$LL
  diamond_ratio_UL <- dr_res$UL

  # Extract results table from each
  FEtbl <- meta_to_table(
    FE,
    fixed_effects = TRUE,
    dr_res = dr_res,
    effect_label = effect_label,
    moderator_variable_name = moderator_variable_name,
    moderator_level = "Overall",
    conf_level = conf_level
  )
  REtbl <- meta_to_table(
    RE,
    fixed_effects = FALSE,
    dr_res = dr_res,
    effect_label = effect_label,
    moderator_variable_name = moderator_variable_name,
    moderator_level = "Overall",
    conf_level = conf_level
  )

  # Deal with moderator
  if(moderator) {
    # With -1, we estimate with no intercept, so the parameters
    # reflect each moderator level effect on its own
    FEgroups <- metafor::rma(
      data = data,
      yi = yi,
      vi = vi,
      mods = ~ moderator -1,
      method="FE",
      level = conf_level * 100
    )
    REgroups <- metafor::rma(
      data = data,
      yi = yi,
      vi = vi,
      mods = ~ moderator -1,
      method="DL",
      level = conf_level * 100
    )

    # Save tables for each group-based analysis
    FEgtable <- meta_to_table(
      FEgroups,
      fixed_effects = TRUE,
      effect_label = effect_label,
      moderator_variable_name = moderator_variable_name,
      moderator_level = NULL,
      conf_level = conf_level
    )
    REgtable <- meta_to_table(
      REgroups,
      fixed_effects = FALSE,
      effect_label = effect_label,
      moderator_variable_name = moderator_variable_name,
      moderator_level = NULL,
      conf_level = conf_level
    )

    # Unfortunately, the group-based analyses don't give us I2
    # or the diamond ratio for each level, so we run an additional analyses
    # for each level and save I2 and its CI for each of these.
    x <- 0
    replabels <- NULL
    data$weight <- 0
    for(lev in levels(data$moderator)) {
      x <- x + 1
      REjustl <- metafor::rma(
        data = data[data$moderator == lev, ],
        yi = yi,
        vi = vi,
        method = "DL",
        level = conf_level * 100
      )
      FEjustl <- metafor::rma(
        data = data[data$moderator == lev, ],
        yi = yi,
        vi = vi,
        method = "FE",
        level = conf_level * 100
      )

      data[data$moderator == lev, ]$weight <-
        metafor::weights.rma.uni(if (random_effects) REjustl else FEjustl)

      REback <- REjustl
      FEback <- FEjustl
      REback$ci.ub <- REgtable[x, "UL"]
      FEback$ci.ub <- FEgtable[x, "UL"]
      REback$ci.lb <- REgtable[x, "LL"]
      FEback$ci.lb <- FEgtable[x, "LL"]

      dr_res <- CI_diamond_ratio(
        REback,
        FEback,
        data[data$moderator == lev, ]$vi,
        conf_level = conf_level
      )

      REjustltbl <- meta_to_table(
        REjustl,
        fixed_effects = FALSE,
        dr_res = dr_res,
        effect_label = effect_label,
        moderator_variable_name = moderator_variable_name,
        moderator_level = lev,
        conf_level = conf_level
      )

      FEjustltbl <- meta_to_table(
        FEjustl,
        fixed_effects = TRUE,
        dr_res = dr_res,
        effect_label = effect_label,
        moderator_variable_name = moderator_variable_name,
        moderator_level = lev,
        conf_level = conf_level
      )

      es_heterogeneity <- rbind(
        es_heterogeneity,
        meta_to_heterogeneity(
          RE = REback,
          FE = FEback,
          effect_label = effect_label,
          moderator_variable_name = moderator_variable_name,
          moderator_level = lev,
          vi = data[data$moderator == lev, ]$vi,
          conf_level = conf_level
        )
      )


      replabels <- c(replabels, lev)
      REgtable[x, "k"] <- REjustltbl$k
      FEgtable[x, "k"] <- REjustltbl$k
      REgtable[x, "I2"] <- REjustltbl$I2[1]
      REgtable[x, "I2_LL"] <- REjustltbl$I2_LL[1]
      REgtable[x, "I2_UL"] <- REjustltbl$I2_UL[1]
      REgtable[x, "diamond_ratio"] <- dr_res$diamond_ratio
      REgtable[x, "diamond_ratio_LL"] <- dr_res$LL
      REgtable[x, "diamond_ratio_UL"] <- dr_res$UL
      FEgtable[x, "diamond_ratio"] <- dr_res$diamond_ratio
      FEgtable[x, "diamond_ratio_LL"] <- dr_res$LL
      FEgtable[x, "diamond_ratio_UL"] <- dr_res$UL
      REgtable[x, "width"] <- REjustltbl$width
      FEgtable[x, "width"] <- FEjustltbl$width
    }
    REgtable$moderator_variable_level <- replabels
    FEgtable$moderator_variable_level <- replabels

    # Now we've prepped each subgroup, bind to overall results
    FEtbl <- rbind(
      FEtbl,
      FEgtable
    )
    REtbl <- rbind(
      REtbl,
      REgtable
    )


    # Now we do the actual contrast
    # Check contrast
    if (is.null(contrast)) {
      contrast = rep(x = 0, times = length(FEgroups$b))
      contrast[1] = -1
      contrast[2] = 1
    }

    if (is.null(names(contrast)) & (length(contrast) == length(replabels))) {
      names(contrast) <- replabels
    }

    # Build contrast set: comparison, reference, and difference
    contrasts <- list(
      Comparison = contrast,
      Reference = contrast,
      Difference = contrast
    )
    # Filter to create comparison and reference only subsets
    contrasts$Comparison[which(contrasts$Comparison < 0)] <- 0
    contrasts$Reference[which(contrasts$Reference > 0)] <- 0
    contrasts$Reference <- abs(contrasts$Reference)

    contrast_labels <- esci_tool_contrast_labels(contrast)

    # initialize results tables
    FE_contrast <- NULL
    RE_contrast <- NULL

    for (x in 1:length(contrasts)) {
      FE_anova <- summary(
        multcomp::glht(
          FEgroups,
          linfct = rbind(contrasts[[x]])
        ),
        test = multcomp::adjusted("none")
      )$test

      # FE_anova <- anova(FEgroups, X = contrasts[[x]])
      FE_contrast <- rbind(
        FE_contrast,
        anova_to_table(
          cres = FE_anova,
          type = names(contrasts[x]),
          effect_label = effect_label,
          moderator_variable_name = moderator_variable_name,
          moderator_level = contrast_labels[[x]],
          conf_level = conf_level
        )
      )
      RE_anova <- summary(
        multcomp::glht(
          REgroups,
          linfct = rbind(contrasts[[x]])
        ),
        test = multcomp::adjusted("none")
      )$test

      #RE_anova <- anova(REgroups, X = contrasts[[x]])
      RE_contrast <- rbind(
        RE_contrast,
        anova_to_table(
          cres = RE_anova,
          type = names(contrasts[x]),
          effect_label = effect_label,
          moderator_variable_name = moderator_variable_name,
          moderator_level = contrast_labels[[x]],
          conf_level = conf_level
        )
      )
    }
    row.names(FE_contrast) <- names(contrasts)
    row.names(RE_contrast) <- names(contrasts)

    data$weight <- metafor::weights.rma.uni(if (random_effects)  REgroups  else  FEgroups )

  } else {
    FEtbl[ , c("moderator_variable_name", "moderator_variable_level")] <- NULL
    REtbl[ , c("moderator_variable_name", "moderator_variable_level")] <- NULL
    data$weight <- metafor::weights.rma.uni(if (random_effects)  RE else FE)
  }


  # Cross-populate RE/FE
  cross_cols <- c(
    "FE_effect_size", "RE_effect_size",
    "FE_CI_width", "RE_CI_width"
  )
  FEtbl <- meta_FE_and_RE(FEtbl, REtbl)
  REtbl <- cbind(REtbl, FEtbl[ , cross_cols])

  # Select which meta-analysis to report
  if(random_effects) {
    es_meta <- REtbl
    es_meta_difference = if (moderator) RE_contrast else NULL
  }  else {
    es_meta <- FEtbl
    es_meta_difference = if (moderator) FE_contrast else NULL
    es_heterogeneity <- es_heterogeneity[es_heterogeneity$measure == "Diamond Ratio", ]
  }


  # Set properties
  properties <- list(
    conf_level = conf_level,
    data_type = "meta",
    effect_size_name = effect_size_name,
    contrast = contrast,
    effect_size_name = "effect_size",
    effect_size_name_html = "effect_size",
    effect_size_name_ggplot = "effect_size",
    model = if (random_effects) "random_effects" else "fixed_effects"
  )


  # Fix up data just a bit
  names(data)[names(data) == "yi"] <- "effect_size"
  data$sample_variance <- data$vi
  data$SE <- sqrt(data$vi)
  data$vi <- NULL


  res <- list(
    properties = properties,
    es_meta = es_meta,
    es_heterogeneity = es_heterogeneity[order(es_heterogeneity$measure), ],
    raw_data = data,
    warnings = warnings
  )

  if (moderator) res$es_meta_difference <- es_meta_difference

  class(res) <- "esci_estimate"

  return(res)
}



meta_to_table <- function(
  meta,
  fixed_effects = TRUE,
  dr_res = NULL,
  effect_label = NULL,
  moderator_variable_name,
  moderator_level = NULL,
  conf_level = 0.95
) {
  rowcount <- length(meta$b[, 1])

  if (rowcount > 1) {
    nmod <- diag(nrow = rowcount, ncol = rowcount)
    pi <- metafor::predict.rma(meta, newmods = nmod)
  } else {
    pi <- metafor::predict.rma(meta)
  }
  if (is.null(pi$pi.lb)) pi$pi.lb <- NA
  if (is.null(pi$pi.ub)) pi$pi.ub <- NA


  if(fixed_effects) {
    I2 <- rep(NA, rowcount)
    I2_LL <- rep(NA, rowcount)
    I2_UL <- rep(NA, rowcount)
  } else {
    hetCIs <- metafor::confint.rma.uni(meta, level = conf_level * 100)
    I2 <- rep(hetCIs$random["I^2(%)", "estimate"], rowcount)
    I2_LL <- rep(hetCIs$random["I^2(%)", "ci.lb"], rowcount)
    I2_UL <- rep(hetCIs$random["I^2(%)", "ci.ub"], rowcount)
  }

  if(is.null(moderator_level)) {
    moderator_level <- names(meta$b[ ,1])
  }

  if(is.null(dr_res)) {
    dr_res <- list(
      diamond_ratio = NA,
      LL = NA,
      UL = NA
    )
  }

  result_table <- data.frame(
    effect_label = effect_label,
    moderator_variable_name = moderator_variable_name,
    moderator_variable_level = moderator_level,
    effect_size = unname(meta$b[, 1]),
    LL = unname(meta$ci.lb),
    UL = unname(meta$ci.ub),
    SE = unname(meta$se),
    k = unname(meta$k),
    diamond_ratio = dr_res$diamond_ratio,
    diamond_ratio_LL = dr_res$LL,
    diamond_ratio_UL = dr_res$UL,
    I2 = I2,
    I2_LL = I2_LL,
    I2_UL = I2_UL,
    PI_LL = pi$pi.lb,
    PI_UL = pi$pi.ub,
    p = meta$pval,
    width = abs(unname(meta$ci.ub) - unname(meta$ci.lb))
  )

    return(result_table)
}


meta_FE_and_RE <- function(FEtable, REtable) {
  FEtable$FE_effect_size <- FEtable$effect_size
  FEtable$RE_effect_size <- REtable$effect_size

  FEtable$FE_CI_width <- abs(FEtable$UL - FEtable$LL)
  FEtable$RE_CI_width <- abs(REtable$UL - REtable$LL)

  # FEtable$FE_CI_width <- FEtable$width
  # FEtable$RE_CI_width <- REtable$width

  return(FEtable)

}

anova_to_table <- function(
  cres,
  type,
  effect_label,
  moderator_variable_name,
  moderator_level,
  conf_level
) {
  c_es <- cres$coefficients[[1]]
  c_se <- cres$sigma
  z_crit <- qnorm((1-conf_level)/2, lower.tail = FALSE)
  c_LL <- c_es - (c_se * z_crit)
  c_UL <- c_es + (c_se * z_crit)

  return(
    data.frame(
      type = type,
      effect_label = effect_label,
      moderator_variable_name = moderator_variable_name,
      moderator_level = moderator_level,
      effect_size = c_es,
      LL = c_LL,
      UL = c_UL,
      SE = c_se,
      p = cres$pvalues[[1]]
    )
  )
}

meta_to_heterogeneity <- function(
    RE,
    FE,
    effect_label = NULL,
    moderator_variable_name,
    moderator_level = NULL,
    vi,
    conf_level
) {


  RE_het <- as.data.frame(stats::confint(RE)$random)
  RE_het <- cbind(
    data.frame(measure = row.names(RE_het)),
    RE_het
  )
  colnames(RE_het) <- c("measure", "estimate", "LL", "UL")

  dr_res <- CI_diamond_ratio(RE, FE, vi, conf_level = conf_level)

  RE_het <- rbind(
    RE_het,
    data.frame(
      measure = "Diamond Ratio",
      estimate = dr_res$diamond_ratio,
      LL = dr_res$LL,
      UL = dr_res$UL
    )
  )

  if(is.null(moderator_level)) {
    moderator_level <- names(RE$b[ ,1])
  }

  effect_label <- rep(effect_label, times = nrow(RE_het))
  moderator_variable_name <- rep(moderator_variable_name, times = nrow(RE_het))
  moderator_level <- rep(moderator_level, times = nrow(RE_het))

  RE_het <- cbind(
    effect_label,
    moderator_variable_name,
    moderator_level,
    RE_het
  )

}

