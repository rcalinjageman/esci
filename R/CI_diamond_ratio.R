#' Estimate the diamond ratio for a meta-analytic effect, a measure of
#' heterogeneity
#'
#'
#' @description `CI_diamond_ratio` returns the diamond ratio and CI for a
#'   meta-analytic effect, the ratio of the random-effects CI width to the
#'   fixed-effects CI width.  The diamond ratio is a measure of effect-size
#'   heterogeneity.
#'
#'
#' @details Calculation of the CI is based on code provided by Maxwell Cairns
#'   (see Cairns et al., 2022).  Specifically, this function implements what
#'   Cairns et al (2022) called the Sub-Q approach, which
#'   provides the best CI coverage in simulations.  For comparison, this
#'   function also returns the CI produced by the bWT-DL approach (which
#'   generally has worse performance).
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
#' * LL - lower limit of the conf_level% CI, Sub-Q approach
#' * UL - upper limit of the conf_level% CI, Sub-Q approach
#' * LL_bWT_DL  - lower limit of the conf_level% CI, bWT-DL approach
#' * UL_bWT_DL  - upper limit of the conf_level% CI, bWT-DL approach
#'
#'
#' @source Cairns, Maxwell, Geoff Cumming, Robert Calin‐Jageman, and Luke A.
#'   Prendergast. “The Diamond Ratio: A Visual Indicator of the Extent of
#'   Heterogeneity in Meta‐analysis.” *British Journal of Mathematical and
#'   Statistical Psychology* 75, no. 2 (May 2022): 201–19.
#'   https://doi.org/10.1111/bmsp.12258.
#'
#'
#' @examples
# Data set from Cairns et al., 2022, Figure 1
#' mydata <- esci::data_mccabemichael_brain
#'
#' # Use esci to obtain effect sizes and sample variances, storing only raw_data
#' mydata <- esci::meta_mdiff_two(
#'   data = mydata,
#'   comparison_means = "M Brain",
#'   comparison_ns = "n Brain",
#'   comparison_sds = "s Brain",
#'   reference_means = "M No Brain",
#'   reference_ns = "n No Brain",
#'   reference_sds = "s No Brain",
#'   random_effects = FALSE
#' )$raw_data
#'
#' # Conduct fixed effects meta-analysis
#' FE <- metafor::rma(
#'   data = mydata,
#'   yi = effect_size,
#'   vi = sample_variance,
#'   method="FE"
#' )
#' # Conduct random effect meta-analysis
#' RE <- metafor::rma(
#'   data = mydata,
#'   yi = effect_size,
#'   vi = sample_variance,
#'   method="DL"
#' )
#'
#' # Get the diamond ratio
#' res <- esci::CI_diamond_ratio(
#'   RE = RE,
#'   FE = FE,
#'   vi = mydata$sample_variance
#' )
#'
#' @export
CI_diamond_ratio <- function(RE, FE, vi, conf_level = 0.95) {
  # Adapted from code provided by Maxwell Cairns -- see Maxwell Cairns, Geoff Cumming, Luke A. Prendergast, forthcoming
  # Implements the bWT-DL approach

  diamond_ratio <- abs(RE$ci.ub - RE$ci.lb) / abs(FE$ci.ub - FE$ci.lb)

  #Sub-Q CIs
  V <- vi
  w <- 1/V
  res1 <- confint(RE)
  Sub_Q.lb <- max(sqrt(sum(w)/sum(1/(V + res1$random[1,2]))), 1)
  Sub_Q.ub <- max(sqrt(sum(w)/sum(1/(V + res1$random[1,3]))), 1)


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
    LL = Sub_Q.lb,
    UL = Sub_Q.ub,
    LL_bWT_DL = LL,
    UL_bWT_DL = UL
  )

  return(res)
}
