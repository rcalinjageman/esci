wrapper_ci.stdmean1 <- function(
  comparison_mean,
  comparison_sd,
  comparison_n,
  reference_mean,
  effect_label,
  conf_level
) {

  # Result from statpsych -------------------------------
  res <- as.data.frame(
    statpsych::ci.stdmean1(
      alpha = 1 - conf_level,
      m = comparison_mean,
      sd = comparison_sd,
      n = comparison_n,
      h = reference_mean
    )
  )

  # Update for statpsych > 1.5
  biased <- res$Estimate
  if (!is.null(res$`adj Estimate`)) {
    biased <- res$`adj Estimate`
    res$Estimate <- res$`adj Estimate`
  }

  # Wrangling ----------------------------------------
  # Change order and names
  res <- res[ , c("Estimate", "LL", "UL", "SE")]
  colnames(res) <- c("effect_size", "LL", "UL", "SE_temp")

  # Add additional properties
  res$numerator <- comparison_mean - reference_mean
  res$denominator <- comparison_sd
  res$SE <- res$SE_temp
  res$SE_temp <- NULL
  res$d_biased <- biased

  # Add effect label
  res <- cbind(
    "effect" = effect_label,
    res
  )


  # Properties ------------------------------
  properties <- list(
    effect_size_name = "d_1",
    effect_size_name_html = "<i>d</i><sub>1.biased</sub>",
    denominator_name = "s_comparison",
    denominator_name_html = "<i>s</i><sub>comparison</sub>",
    bias_corrected = TRUE
  )

  properties$effect_size_category = "difference"
  properties$effect_size_precision = "magnitude"
  properties$conf_level = conf_level
  properties$error_distribution = "norm"


  properties$message <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name} because the standardizer used was {properties$denominator_name}. {properties$effect_size_name} {if (properties$bias_corrected) 'has' else 'has *not*'} been corrected for bias. Correction for bias can be important when df < 50.
    "
  )

  properties$message_html <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name_html}
because the standardizer used was {properties$denominator_name_html}.<br>
{properties$effect_size_name_html} {if (properties$bias_corrected) 'has' else 'has *not*'}
been corrected for bias.
Correction for bias can be important when <i>df</i> < 50.<br>
    "
  )




  smd <- list(
    es_smd = res,
    es_smd_properties = properties
  )

  return(smd)

}


wrapper_ci.stdmean.ps <- function(
  comparison_mean,
  comparison_sd,
  reference_mean,
  reference_sd,
  n,
  correlation,
  grouping_variable_levels,
  conf_level
) {

  effect_label <- paste(
    grouping_variable_levels[1],
    "-",
    grouping_variable_levels[2],
    sep = " "
  )

  es_smd <- as.data.frame(
    statpsych::ci.stdmean.ps(
      alpha = 1 - conf_level,
      m1 = comparison_mean,
      m2 = reference_mean,
      sd1 = comparison_sd,
      sd2 = reference_sd,
      cor = correlation,
      n = n
    )
  )[1, ]

  # Update for statpsych > 1.5
  biased <- es_smd$Estimate
  if (!is.null(es_smd$`adj Estimate`)) {
    biased <- es_smd$`adj Estimate`
    es_smd$Estimate <- es_smd$`adj Estimate`
  }

  rownames(es_smd) <- NULL
  es_smd <- es_smd[ , c("Estimate", "LL", "UL", "SE")]
  colnames(es_smd) <- c("effect_size", "LL", "UL", "SE_temp")

  # Add additional properties
  bias <- sqrt((n - 2)/(n-1))

  es_smd$numerator <- comparison_mean - reference_mean
  es_smd$denominator <- sqrt((comparison_sd^2 + reference_sd^2)/2)
  es_smd$SE <- es_smd$SE_temp
  es_smd$SE_temp <- NULL
  es_smd$d_biased <- biased
  es_smd$df <- n-1

  # Add effect label
  es_smd<- cbind(
    "comparison_measure_name" = grouping_variable_levels[1],
    "reference_measure_name" = grouping_variable_levels[2],
    "effect" = effect_label,
    es_smd
  )


  # Properties ------------------------------
  properties <- list(
    effect_size_name = "d_average",
    effect_size_name_html = "<i>d</i><sub>avg</sub>",
    denominator_name = "s_average",
    denominator_name_html = "<i>s</i><sub>avg</sub>",
    bias_corrected = TRUE
  )

  properties$effect_size_category = "difference"
  properties$effect_size_precision = "magnitude"
  properties$conf_level = conf_level
  properties$error_distribution = "norm"


  properties$message <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name} because the standardizer used was {properties$denominator_name}. {properties$effect_size_name} {if (properties$bias_corrected) 'has' else 'has *not*'} been corrected for bias. Correction for bias can be important when df < 50.
    "
  )

  properties$message_html <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name_html}
because the standardizer used was {properties$denominator_name_html}.<br>
{properties$effect_size_name_html} {if (properties$bias_corrected) 'has' else 'has *not*'}
been corrected for bias.
Correction for bias can be important when <i>df</i> < 50.<br>
    "
  )


  smd <- list(
    es_smd = es_smd,
    es_smd_properties = properties
  )


  return(smd)

}


wrapper_ci.mean.ps <- function(
  comparison_mean,
  comparison_sd,
  reference_mean,
  reference_sd,
  n,
  correlation,
  grouping_variable_levels,
  conf_level
) {

  effect_label <- paste(
    grouping_variable_levels[1],
    "\U2012",
    grouping_variable_levels[2],
    sep = " "
  )

  es_mean_difference <- as.data.frame(
    statpsych::ci.mean.ps(
      alpha = 1 - conf_level,
      m1 = comparison_mean,
      m2 = reference_mean,
      sd1 = comparison_sd,
      sd2 = reference_sd,
      cor = correlation,
      n = n
    )
  )

  ta_res <- as.data.frame(
    statpsych::ci.mean.ps(
      alpha = (1 - conf_level)*2,
      m1 = comparison_mean,
      m2 = reference_mean,
      sd1 = comparison_sd,
      sd2 = reference_sd,
      cor = correlation,
      n = n
    )
  )

  es_mean_difference <- es_mean_difference[ , c("Estimate", "LL", "UL", "SE", "df")]
  colnames(es_mean_difference) <- c("effect_size", "LL", "UL", "SE", "df")
  es_mean_difference$ta_LL <- ta_res$LL
  es_mean_difference$ta_UL <- ta_res$UL

  es_mean_difference <- cbind(
    "type" = "Difference",
    "comparison_measure_name" = grouping_variable_levels[1],
    "reference_measure_name" = grouping_variable_levels[2],
    "effect" = effect_label,
    es_mean_difference
  )

  comp_mean <- estimate_mdiff_ind_contrast(
    means = c(comparison_mean, reference_mean),
    sds = c(comparison_sd, reference_sd),
    ns = c(n, n),
    contrast = c(1, -1),
    conf_level = conf_level,
    assume_equal_variance = FALSE,
    grouping_variable_levels = grouping_variable_levels
  )

  type_store <- comp_mean$es_mean_difference$type
  comp_mean$es_mean_difference$type <- NULL
  comp_mean$es_mean_difference$outcome_variable_name <- NULL
  comp_mean$es_mean_difference$grouping_variable_name <- NULL
  comp_mean$es_mean_difference <- cbind(
    type = type_store,
    "comparison_measure_name" = grouping_variable_levels[1],
    "reference_measure_name" = grouping_variable_levels[2],
    comp_mean$es_mean_difference
  )


  es_mean_difference <- rbind(
    comp_mean$es_mean_difference[1, ],
    comp_mean$es_mean_difference[2, ],
    es_mean_difference
  )


  return(es_mean_difference)

}


wrapper_ci_ratio.2 <- function(
  mean_or_median,
  comparison_measure,
  reference_measure,
  grouping_variable_levels,
  outcome_variable_name,
  grouping_variable_name,
  conf_level
) {

  statpsych_mean <- c("Mean1/Mean2", "LL", "UL", "Mean1", "Mean2")
  statpsych_median <- c("Median/Median2", "LL", "UL", "Median1", "Median2")
  esci_mean <- c("effect_size", "LL", "UL", "comparison_mean", "reference_mean")
  esci_median <- c("effect_size", "LL", "UL", "comparison_median", "reference_median")

  if(mean_or_median == "mean") {
    es_ratio <- as.data.frame(
      statpsych::ci.ratio.mean2(
        alpha = 1 - conf_level,
        y1 = comparison_measure,
        y2 = reference_measure
      )
    )
    statpsych_cols <- c("Mean1/Mean2", "LL", "UL", "Mean1", "Mean2")
    esci_cols <- c("effect_size", "LL", "UL", "comparison_mean", "reference_mean")
  } else if (mean_or_median == "median") {
    es_ratio <- as.data.frame(
      statpsych::ci.ratio.median2(
        alpha = 1 - conf_level,
        y1 = comparison_measure,
        y2 = reference_measure
      )
    )
    statpsych_cols <- c("Median1/Median2", "LL", "UL", "Median1", "Median2")
    esci_cols <- c("effect_size", "LL", "UL", "comparison_median", "reference_median")
  } else {
    stop("mean_or_median parameter not correctly defined")
  }


  es_ratio <- es_ratio[ , statpsych_cols]
  colnames(es_ratio) <- esci_cols

  clabel <- paste(
    grouping_variable_levels[1],
    "/",
    grouping_variable_levels[2],
    sep = " "
  )

  es_ratio <- cbind(
    type = c("Ratio"),
    outcome_variable_name = outcome_variable_name,
    grouping_variable_name = grouping_variable_name,
    effect = clabel,
    es_ratio
  )

  return(es_ratio)

}


wrapper_ci_ratio.ps <- function(
  mean_or_median,
  comparison_measure,
  reference_measure,
  grouping_variable_levels,
  conf_level
) {


  if(mean_or_median == "mean") {
    es_ratio <- as.data.frame(
      statpsych::ci.ratio.mean.ps(
        alpha = 1 - conf_level,
        y1 = comparison_measure,
        y2 = reference_measure
      )
    )
    statpsych_cols <- c("Mean1/Mean2", "LL", "UL", "Mean1", "Mean2")
    esci_cols <- c("effect_size", "LL", "UL", "comparison_mean", "reference_mean")
  } else if (mean_or_median == "median") {
    es_ratio <- as.data.frame(
      statpsych::ci.ratio.median.ps(
        alpha = 1 - conf_level,
        y1 = comparison_measure,
        y2 = reference_measure
      )
    )
    statpsych_cols <- c("Median1/Median2", "LL", "UL", "Median1", "Median2")
    esci_cols <- c("effect_size", "LL", "UL", "comparison_median", "reference_median")
  } else {
    stop("mean_or_median parameter not correctly defined")
  }

  es_ratio <- es_ratio[ , statpsych_cols]
  colnames(es_ratio) <- esci_cols

  clabel <- paste(
    grouping_variable_levels[1],
    "/",
    grouping_variable_levels[2],
    sep = " "
  )

  es_ratio <- cbind(
    "comparison_measure_name" = grouping_variable_levels[1],
    "reference_measure_name" = grouping_variable_levels[2],
    effect = clabel,
    es_ratio
  )

  return(es_ratio)

}

wrapper_ci.median.ps <- function(
  comparison_measure,
  reference_measure,
  grouping_variable_levels,
  conf_level
) {

  res <- as.data.frame(
    statpsych::ci.median1(
      alpha = 1 - conf_level,
      y = comparison_measure
    )
  )

  res <- rbind(
    res,
    as.data.frame(
      statpsych::ci.median1(
        alpha = 1 - conf_level,
        y = reference_measure
      )
    )
  )

  colnames(res)[1] <- "effect_size"

  mdn_diff <- as.data.frame(
    statpsych::ci.median.ps(
      alpha = 1 - conf_level,
      y1 = comparison_measure,
      y2 = reference_measure
    )
  )[, c("Median1-Median2", "SE", "LL", "UL")]

  colnames(mdn_diff)[1] <- "effect_size"

  es_median_difference <- rbind(
    res,
    mdn_diff
  )

  es_median_difference <- es_median_difference[ , c("effect_size", "LL", "UL", "SE")]

  es_median_difference <- cbind(
    type = c("Comparison", "Reference", "Difference"),
    "comparison_measure_name" = grouping_variable_levels[1],
    "reference_measure_name" = grouping_variable_levels[2],
    effect = c(
      grouping_variable_levels,
      paste(
        grouping_variable_levels[1],
        "\U2012",
        grouping_variable_levels[2],
        sep = " "
      )
    ),
    es_median_difference
  )

  es_median_difference_properties <- list(
    effect_size_name = "Mdn_Diff",
    effect_size_name_html = "<i>Mdn</i><sub>diff</sub>",
    effect_size_category = "difference",
    effect_size_precision = "magnitude",
    conf_level = conf_level,
    error_distribution = "norm"
  )

  estimate <- list(
    es_median_difference = es_median_difference,
    es_median_difference_properties = es_median_difference_properties
  )

  return(estimate)

}


wrapper_ci.cor <- function(
  r,
  n,
  conf_level,
  x_variable_name,
  y_variable_name
) {

  res <- statpsych::ci.cor(
    alpha = 1 - conf_level,
    cor = r,
    s = 0,
    n = n
  )

  res_ta <- statpsych::ci.cor(
    alpha = (1 - conf_level)*2,
    cor = r,
    s = 0,
    n = n
  )

  res <- as.data.frame(res)
  res_ta <- as.data.frame(res_ta)

  colnames(res)[1] <- "effect_size"
  colnames(res)[2] <- "SE_temp"
  res$SE <- res$SE_temp
  res$SE_temp <- NULL
  res$n = n
  res$df <- n - 2
  res$ta_LL <- res_ta$LL
  res$ta_UL <- res_ta$UL

  res <- cbind(
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name,
    effect = paste(
      x_variable_name,
      " and ",
      y_variable_name,
      sep = ""
    ),
    res
  )

  return(res)

}


wrapper_ci.cor2 <- function(
  comparison_r,
  comparison_n,
  reference_r,
  reference_n,
  conf_level,
  grouping_variable_levels,
  x_variable_name,
  y_variable_name,
  grouping_variable_name
) {

  res_comparison <- wrapper_ci.cor(
    r = comparison_r,
    n = comparison_n,
    conf_level = conf_level,
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name
  )

  res_reference <- wrapper_ci.cor(
    r = reference_r,
    n = reference_n,
    conf_level = conf_level,
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name
  )

  res_difference <- as.data.frame(
    statpsych::ci.cor2(
      alpha = 1 - conf_level,
      cor1 = comparison_r,
      cor2 = reference_r,
      n1 = comparison_n,
      n2 = reference_n
    )
  )

  res_difference_ta <- as.data.frame(
    statpsych::ci.cor2(
      alpha = (1 - conf_level)*2,
      cor1 = comparison_r,
      cor2 = reference_r,
      n1 = comparison_n,
      n2 = reference_n
    )
  )





  colnames(res_difference)[1] <- "effect_size"
  res_difference$SE <- NA
  res_difference$n <- NA
  res_difference$df <- NA
  res_difference <- cbind(
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name,
    effect = "Difference",
    res_difference
  )
  res_difference$ta_LL <- res_difference_ta$LL
  res_difference$ta_UL <- res_difference_ta$UL

  res <- rbind(
    res_comparison,
    res_reference,
    res_difference
  )

  res$effect <- c(
    grouping_variable_levels[[2]],
    grouping_variable_levels[[1]],
    paste(
      grouping_variable_levels[[2]],
      " - ",
      grouping_variable_levels[[1]],
      sep = " "
    )
  )

  res <- cbind(
    type = c("Comparison", "Reference", "Difference"),
    grouping_variable_name = grouping_variable_name,
    grouping_variable_level = res$effect,
    res
  )

  res$rz <- esci_trans_r_to_z(res$effect_size)
  res$rz[3] <- abs(res$rz[1] - res$rz[2])
  res$sem <- 0
  res$sem[1] <-  1 / sqrt(comparison_n - 3)
  res$sem[2] <-  1 / sqrt(reference_n - 3)
  res$sem[3] <- sqrt ( (1/(comparison_n -3)) + (1/(reference_n-3)) )

  res$z <- res$rz / res$sem
  res$p <- 2 * pnorm(q=res$z, lower.tail=FALSE)

  return(res)

}
