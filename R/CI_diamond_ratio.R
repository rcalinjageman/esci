#' Estimate the diamond ratio for a meta-analtyic effect, a measure of
#' heterogeneity
#'
#'
#' @description `CI_diamond_ratio` returns the diamond ratio and CI for a
#'   meta-analtyic effect, the ratio of the random-effects CI width to the
#'   fixed-effects CI width.  The diamond ratio is a measure of effect-size
#'   heteogeneity.
#'
#'
#' @details Calculation of the CI is based on code provided by Maxwell Cairns
#' (see Cairns et al., 2022).  Specifically, this function implements what
#' Cairns et al (2022) called the bWT-DL approach, which had the best peformance
#' for a diamond ratio CI.
#'
#'
#' @param RE metafor object with random effects result
#' @param FE metafor object with fixed effects result
#' @param vi vector of effect size variances
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#'
#'
#' @return Returns a list with 3 properties:
#' * diamond_ratio
#' * LL - lower limit of the conf_level% CI
#' * UL - upper limit of the conf_level% CI
#'
#'
#' @source Cairns, Maxwell, Geoff Cumming, Robert Calin‐Jageman, and Luke A.
#'   Prendergast. “The Diamond Ratio: A Visual Indicator of the Extent of
#'   Heterogeneity in Meta‐analysis.” *British Journal of Mathematical and
#'   Statistical Psychology* 75, no. 2 (May 2022): 201–19.
#'   https://doi.org/10.1111/bmsp.12258.
#'
#' @export
CI_diamond_ratio <- function(RE, FE, vi, conf_level = 0.95) {
  # Calculates the confidence interval on a diamond_ratio
  # Pass in a random effects and fixed effect meta-analysis and the study effect size variances
  # Obtain a list with the diamond_ratio and its CI
  # Adapted from code provided by Maxwell Cairns -- see Maxwell Cairns, Geoff Cumming, Luke A. Prendergast, forthcoming
  # Implements the bWT-DL approach

  diamond_ratio <- abs(RE$ci.ub - RE$ci.lb) / abs(FE$ci.ub - FE$ci.lb)

  #CI on the diamond ratio
  log.ratio <- log(diamond_ratio)
  sds <- sqrt(vi)
  w.star <- 1/(sds^2 + RE$tau2)

  log.var <- ((RE$se.tau2)^2)/4*1/(sum(w.star))^2*(sum(1/(sds^2 + RE$tau2)^2))^2
  log.sd <- sqrt(log.var)
  bias <- 1/2*(RE$se.tau2)^2*(1/2/sum(w.star)^2 - 1/sum(w.star)*sum(1/(sds^2 + RE$tau2)^3))

  #Bias corrected CIs
  zcrit <- qnorm((1-conf_level)/2, lower.tail = FALSE)
  LL <- max(exp(log.ratio - bias - zcrit*log.sd), 1)
  UL <- max(exp(log.ratio - bias + zcrit*log.sd), 1)

  res <- list(
    diamond_ratio = diamond_ratio,
    LL = LL,
    UL = UL
  )

  return(res)
}
