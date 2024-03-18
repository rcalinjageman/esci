#' Estimate standardized mean difference (Cohen's d) for an independent groups
#' contrast
#'
#'
#' @description `CI_smd_ind_contrast` returns the point estimate
#'   and confidence interval for a standardized mean difference (smd aka Cohen's
#' *d* aka Hedges *g*). A standardized mean difference is a difference in means standardized
#'   to a standard deviation:
#'   d = psi/s
#'
#'
#' @param means A vector of 2 or more means
#' @param sds A vector of standard deviations, same length as means
#' @param ns A vector of sample sizes, same length as means
#' @param contrast A vector of group weights, same length as means
#'
#' @param conf_level The confidence level for the confidence interval, in
#'   decimal form.  Defaults to 0.95.
#' @param assume_equal_variance Defaults to FALSE
#' @param correct_bias Defaults to TRUE; attempts to correct the slight upward
#'   bias in d derived from a sample.  As of 8/9/2023 - Bias correction
#'   has been added for more than 2 groups when equal variance is not
#'   assumed, based on recent updates to statpsych
#'
#'
#' @return Returns a list with these named elements:
#' * effect_size - the point estimate from the sample
#' * lower - lower bound of the CI
#' * upper - upper bound of the CI
#' * numerator - the numerator for Cohen's d_biased;
#'   the mean difference in the contrast
#' * denominator - the denominator for Cohen's d_biased;
#'   if equal variance is assumed this is sd_pooled, otherwise sd_avg
#' * df - the degrees of freedom used for correction and CI calculation
#' * se - the standard error of the estimate; **warning** not totally
#'   sure about this yet
#' * moe - margin of error; 1/2 length of the CI
#' * d_biased - Cohen's d without correction applied
#' * properties - a list of properties for the result
#'
#'   Properties
#' * effect_size_name - if equal variance assumed d_s, otherwise d_avg
#' * effect_size_name_html - html representation of d_name
#' * denominator_name - if equal variance assumed sd_pooled otherwise sd_avg
#' * denominator_name_html - html representation of denominator name
#' * bias_corrected - TRUE/FALSE if bias correction was applied
#' * message - a message explaining denominator and correction status
#' * message_html - html representation of message
#'
#' @section Details:
#' ## It's a bit complicated ##
#' A standardized mean difference turns out to be complicated.
#'
#' First, it has many names:
#' * standardized mean difference (smd)
#' * Cohen's *d*
#' * When bias in a sample d has been corrected, also called Hedge's *g*
#'
#'
#' Second, the choice of the standardizer requires thought:
#' * sd_pooled - used when assuming all groups have exact same variance
#' * sd_avg - does not require assumption of equal variance
#' * other possibilities, too, but not dealt with in this function
#'
#'
#' The choice of standardizer is important, so it's noted in the subscript:
#' * d_s -- assumes equal variance, standardized to sd_pooled
#' * d_avg - does not assume equal variance, standardized to sd_avg
#'
#'
#' A third complication is the issue of bias: d estimated from a sample has a
#' slight upward bias at smaller sample sizes.  With total sample size > 30,
#' this slight bias becomes fairly negligible (kind of like the small upward
#' bias in a sample standard deviation).
#'
#' This bias can be corrected when equal variance is assumed or when the
#' design of the study is simple (2 groups).  For complex designs (>2 groups)
#' without the assumption of equal variance, there is now also an
#' approximate approach to correcting bias from Bonett.
#'
#' Corrections for bias produce a long-run reduction in average bias.
#' Corrections for bias are approximate.
#'
#'
#' ## How are d and its CI calculated? ##
#'
#' ### When equal variance is assumed ###
#' When equal variance is assumed, the standardized mean difference is
#' d_s, defined in Kline, p. 196:
#'
#'  d_s = psi / sd_s
#'
#'
#' where psi is defined in Kline, equation 7.8:
#'
#' psi = sum(contrasts*means)
#'
#'
#' and where sd_pooled is defined in Kline, equation 3.11
#' sqrt(sum(variances*dfs) / sum(dfs))
#'
#'
#' The CI for d_s is derived from lambda-prime transformation from Lecoutre,
#' 2007 with code adapted from Cousineau & Goulet-Pelletier, 2020. Kelley,
#' 2007 explains the general approach for linear contrasts.
#'
#' This approach to generating the CI is 'exact', meaning coverage should be
#' as desired *if* all assumptions are met (ha!).
#'
#' Correction of upward bias can be applied.
#'
#'
#' ### When equal variance is not assumed ###
#' When equal variance is not
#' assumed, the standardized mean difference is d_avg, defined in Bonett,
#' equation 6:
#'
#' d_avg = psi / sd_avg
#'
#' Where sd_avg is the square root of the average of the group variances, as
#' given in Bonett, explanation of equation 6:
#'
#' sqrt(mean(variances))
#'
#'
#' #### If only 2 groups ####
#' * The CI is derived from lambda-prime transformation
#'   using df and se from Huynh, 1989 -- see especially Delacre et al., 2021
#' * This is also an 'exact' approach, and correction can be applied
#'
#'
#' #### If more than 2 groups ####
#' * CI is approximated using the approach from Bonett, 2008
#' * An approximate correction developed by Bonett is used
#'
#'
#' @references
#' * Bonett D. G. (2023). statpsych: Statistical Methods for Psychologists. R package version 1.4.0.
#' [https://dgbonett.github.io/statpsych](https://dgbonett.github.io/statpsych/)
#' * Bonett, D. G. (2018).  R code posted to personal website (now removed).
#' Formally at https://people.ucsc.edu/~dgbonett/psyc204.html
#' * Bonett, D. G. (2008). Confidence Intervals for Standardized Linear
#' Contrasts of Means. *Psychological Methods*, 13(2), 99–109.
#' \doi{doi:10.1037/1082-989X.13.2.99}
#' * Cousineau & Goulet-Pelletier (2020)
#' [https://osf.io/preprints/psyarxiv/s2597](https://osf.io/preprints/psyarxiv/s2597)
#' * Delacre et al., 2021,
#' [https://osf.io/preprints/psyarxiv/tu6mp/](https://osf.io/preprints/psyarxiv/tu6mp/)
#' * Huynh, C.-L. (1989). A unified approach to the estimation of effect size
#' in meta-analysis. NBER Working Paper Series, 58(58), 99–104.
#' * Kelley, K. (2007). Confidence intervals for standardized effect sizes:
#' Theory, application, and implementation.
#' *Journal of Statistical Software, 20(8)*, 1–24.
#' \doi{doi:10.18637/jss.v020.i08}
#' * Lecoutre, B. (2007). Another Look at the Confidence Intervals for the
#' Noncentral T Distribution. Journal of Modern Applied Statistical Methods,
#' 6(1), 107–116. \doi{doi:10.22237/jmasm/1177992600}
#'
#'
#' @seealso
#' * \code{\link{estimate_mdiff_ind_contrast}} for friendly version that also
#' returns raw score effect sizes for this design
#'
#'
#' @examples
#' # Example from Kline, 2013
#' #  Data in Table 3.4
#' #  Worked out in Chapter 7
#' #  See p. 202, non-central approach
#' # With equal variance assumed and no correction, should give:
#' #   d_s = -0.8528028 [-2.121155, 0.4482578]
#'
#' res <- esci::CI_smd_ind_contrast(
#'   means = c(13, 11, 15),
#'   sds = c(2.738613, 2.236068, 2.000000),
#'   ns = c(5, 5, 5),
#'   contrast = contrast <- c(1, 0, -1),
#'   conf_level = 0.95,
#'   assume_equal_variance = TRUE,
#'   correct_bias = FALSE
#' )
#'
#'
#' # Example from [statpsych::ci.lc.stdmean.bs()] should give:
#' # Estimate        SE        LL         UL
#' # Unweighted standardizer: -1.273964 0.3692800 -2.025039 -0.5774878
#' # Weighted standardizer:   -1.273964 0.3514511 -1.990095 -0.6124317
#' # Group 1 standardizer:    -1.273810 0.4849842 -2.343781 -0.4426775
#'
#'
#' res <- esci::CI_smd_ind_contrast(
#'   means = c(33.5, 37.9, 38.0, 44.1),
#'   sds = c(3.84, 3.84, 3.65, 4.98),
#'   ns = c(10,10,10,10),
#'   contrast = c(.5, .5, -.5, -.5),
#'   conf_level = 0.95,
#'   assume_equal_variance = FALSE,
#'   correct_bias = TRUE
#' )
#'
#'
#'
#' @export
CI_smd_ind_contrast <- function(
  means,
  sds,
  ns,
  contrast,
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  correct_bias = TRUE
) {

  # n_groups - number of different groups passed
  n_groups <- length(means)

  # At moment, can only correct d if equal variance or simple comparison
  #  (no correction apparent for unequal variance with 3 or more groups)
  simple_comparison <- (n_groups == 2)
  do_correct <- correct_bias

  # degrees of freedom for each mean
  dfs <- ns-1

  # pooled and average standard deviations
  # sd_pooled from Kline, 2013, equation 3.11
  # sd_avg from Bonett, 2008, explanation of equation 6
  variances <- sds^2
  sd_pooled <- sqrt(sum(variances*dfs) / sum(dfs))
  sd_avg <- sqrt(mean(variances))

  # df - Huynh-type if unequal variance and simple comparison
  #      or simple sum of n-1 for all other cases
  df <- if (simple_comparison & !assume_equal_variance)
    df <- prod(ns-1)*sum(variances)^2 / sum((ns-1)*rev(variances^2))
  else
    df <- sum(dfs)

  # Correction factor
  # Cousineau & Goulet-Pelletier, 2020 from Hedges
  J <- 1
  if (do_correct) {
    if (simple_comparison | assume_equal_variance) {
      J <-  exp ( lgamma(df/2) - (log(sqrt(df/2)) + lgamma((df-1)/2) ) )
    } else {
      J <- 1 - 3/(4*df - 1)
    }
  }


  # Calculate d -----------------------------------------------------------
  numerator <- sum(means*contrast)

  denominator <- if(assume_equal_variance)
    sd_pooled
  else
    sd_avg

  d_biased <- numerator / denominator

  effect_size <- d_biased * J

  # Calculate CI --------------------------------------------------------
  if(assume_equal_variance | simple_comparison) {
    # In these cases, we can use the exact approach
    # Cousineau & Goulet-Pelletier, 2020; method of Lecoutre, 2007

    lambda <- if(assume_equal_variance)
      # Kelley, 2007, equation 60
      effect_size / sqrt(sum(contrast^2/ns))
    else
      # Huynh, 1989 via Delacre et al., 2021,
      (sqrt(prod(ns)) * numerator) / sqrt(sum((ns)*rev(variances)))

    back_from_lambda <- if(assume_equal_variance)
      # Kelley, 2007
      sqrt(sum(contrast^2/ns))
    else
      # Huynh, 1989 via Delacre et al., 2021,
      sqrt( (2* sum((ns)*rev(variances))) / (prod(ns)*sum(variances)) )

    lambda_low <- sadists::qlambdap(1/2-conf_level/2, df = df, t = lambda )
    lambda_high <- sadists::qlambdap(1/2+conf_level/2, df = df, t = lambda )

    if (assume_equal_variance) {
      # SE when equal variance from p. 111 of Lecoutre, 2007
      #  thanks to Cousineau personal communication
      k <- sqrt(2/df)* exp((lgamma((df+1)/2)) - (lgamma((df)/2)))
      lambda_se <- sqrt(1 + lambda^2 * (1-k^2))
      se <- lambda_se * back_from_lambda
    } else {
      # SE when not equal variance but simple contrast
      # From Delacre et al., Table 2, 2021 via Huynh
      cf <- sqrt(2/df)* exp((lgamma((df+1)/2)) - (lgamma((df)/2)))
      mid_term <- (2 * sum(variances/ns)  / sum(variances))
      se <- sqrt(df/(df-2) * mid_term + effect_size^2 * (df/(df-2)-cf^2))
      if (is.na(se)) {
        esci_abort_calculation_error(
          "Standard error calculation (se) ",
          "returned a value of NA; a CI cannot be obtained."
        )
      }
    }

    lower <- lambda_low * back_from_lambda
    upper <- lambda_high * back_from_lambda
    moe <- (upper-lower)/2

  } else {
    # Bonett, 2008, equation 7
    # Code adapted first from his posted code, then from statpsych
    a1 <- d_biased^2/(n_groups^2*sd_avg^4)
    a2 <- sum((variances^2/(2*(dfs))))
    a3 <- sum((contrast^2*variances/(dfs)))/sd_avg^2
    se <- sqrt(a1*a2 + a3)

    # z multiplier
    multiplier <- qnorm(1 - ((1-conf_level)/2))

    # MoE and confidence interval
    moe <- se * multiplier
    lower <- d_biased - moe
    upper <- d_biased + moe
  }

  if(assume_equal_variance) {
    properties <- list(
      effect_size_name = "d_s",
      effect_size_name_html = if (do_correct)
        "<i>d</i><sub>s</sub>"
      else
        "<i>d</i><sub>s.biased</sub>",
      denominator_name = "s_p",
      denominator_name_html = "<i>s</i><sub>p</sub>",
      bias_corrected = do_correct
    )
  } else {
    properties <- list(
      effect_size_name = "d_avg",
      effect_size_name_html = if (do_correct)
        "<i>d</i><sub>avg</sub>"
      else
        "<i>d</i><sub>avg.biased</sub>",
      denominator_name = "s_avg",
      denominator_name_html = "<i>s</i><sub>avg</sub>",
      bias_corrected = do_correct
    )
  }

  properties$effect_size_category = "difference"
  properties$effect_size_precision = "magnitude"
  properties$conf_level = conf_level
  properties$assume_equal_variance = assume_equal_variance
  properties$error_distribution = "t_dist"


  see_biased <- if(do_correct)
    "See the rightmost column for the biased value."
  else
    ""

  properties$message <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name} because the standardizer used was {properties$denominator_name}. {properties$effect_size_name} {if (properties$bias_corrected) 'has' else 'has *not*'} been corrected for bias. Correction for bias can be important when df < 50.  {see_biased}
    "
  )

  properties$message_html <- glue::glue(
    "
This standardized mean difference is called {properties$effect_size_name_html}
because the standardizer used was {properties$denominator_name_html}.<br>
{properties$effect_size_name_html} {if (properties$bias_corrected) 'has' else 'has *not*'}
been corrected for bias.
Correction for bias can be important when <i>df</i> < 50.  {see_biased}<br>
    "
  )

  # Put in table form
  res <- list(
    effect_size = effect_size,
    LL = lower,
    UL = upper,
    numerator = numerator,
    denominator = denominator,
    SE = se,
    df = df,
    d_biased = d_biased,
    properties = properties
  )

  return(res)
}



CI_smd_ind_contrast.examples <- function() {


  # Example: 3 groups, equal variance assumed, no correction ------------------
  # Example from Kline, Table 3.4, worked out in Chapter 7, p. 202
  # With equal variance and no correction, Kline writes:
  #  d_s = -0.8528029 [-2.1213, 0.4483] using Steiger's NDC calculator, p. 202
  # and MBESS reports:
  #  d_s = -0.8528028 [-2.121155, 0.4482578]
  CI_smd_ind_contrast(
    means = c(13, 11, 15),
    sds = c(2.738613, 2.236068, 2.000000),
    ns = c(5, 5, 5),
    contrast = c(1, 0, -1),
    assume_equal_variance = TRUE,
    correct_bias = FALSE
  )

  # Compare to MBESS ci on a linear contrast
  # In this example, sd_pooled = sqrt(sum(sds^2*(ns-1)) / sum(ns-1)) = 2.345208
  # MBESS::ci.sc(
  #   means = c(13, 11, 15),
  #   s.anova = 2.345208,
  #   c.weights = c(1, 0, -1),
  #   n = c(5, 5, 5),
  #   N = 15
  # )


  # Example: 3 groups, equal variance not assumed, no correction -------------
  # Example from Kline, Table 3.4, worked out in Chapter 7, p. 202
  # When same data is input to Bonett's approach without equal var, obtain:
  #                                Estimate        SE        LL        UL
  # Equal Variances Not Assumed: -0.8528028 0.7451180 -2.313207 0.6076015
  # Equal Variances Assumed:     -0.8528028 0.6559749 -2.138490 0.4328843
  #
  # Note Bonett uses different approach for equal variance assumed, and
  #   no correction, so won't match this routine
  #
  CI_smd_ind_contrast(
    means = c(13, 11, 15),
    sds = c(2.738613, 2.236068, 2.000000),
    ns = c(5, 5, 5),
    contrast = c(1, 0, -1),
    assume_equal_variance = FALSE,
    correct_bias = FALSE
  )

  # Compare to Bonett's function, available online at:
  #  https://people.ucsc.edu/~dgbonett/psyc204.html
  # ci.lc.stdmean.bs(
  #   alpha = 0.05,
  #   m = c(13, 11, 15),
  #   sd = c(2.738613, 2.236068, 2.000000),
  #   n = c(5, 5, 5),
  #   c = c(1, 0, -1)
  # )


  # Example: 2 groups, equal variance assumed, correction -------------------
  # From Goulet-Pelletier & Cousineau, 2018, appendix.
  # x1 <- c(53, 68, 66, 69, 83, 91)
  # x2 <- c(49, 60, 67, 75, 78, 89)
  #
  # Appendix provides example with correction for MBESS, which provides:
  # d_s_corrected =  .1337921 [-1.002561  1.263567]
  #
  CI_smd_ind_contrast(
    means = c(71.66667, 69.66667),
    sds = c(13.44123, 14.13742),
    ns = c(6, 6),
    contrast = c(1, -1),
    assume_equal_variance = TRUE,
    correct_bias = TRUE
  )

  # Compare to MBESS
  # MBESS::ci.smd(
  #   ncp = MBESS::smd(
  #     Group.1 = c(53, 68, 66, 69, 83, 91),
  #     Group.2 = c(49, 60, 67, 75, 78, 89),
  #     Unbiased = TRUE
  #     ) * sqrt(6/2),
  #   n.1 = 6,
  #   n.2 = 6,
  #   conf.level = 0.95
  # )


  # Example: 2 groups, equal variance not assumed, corrected or not
  # From Goulet-Pelletier & Cousineau, 2018, appendix.
  # x1 <- c(53, 68, 66, 69, 83, 91)
  # x2 <- c(49, 60, 67, 75, 78, 89)
  #
  # Delacre's deffectsize, with not-equal variance and no correction gives:
  #   d_av_biased = 0.1449935 [-0.9919049  1.2747360]
  #   d_av_unbiased = 0.1337628 [-0.915075  1.175999]
  # but note that when unbiasing, Delacre unbiases the CI ends as well
  # The correction factor in this case is J = 0.922543
  # So if correction factor were not applied to CI ends, delacre would give:
  #  d_av_unbiased = 0.1337628 [-0.9919049  1.2747360] and this matches

  CI_smd_ind_contrast(
    means = c(71.66667, 69.66667),
    sds = c(13.44123, 14.13742),
    ns = c(6, 6),
    contrast = c(1, -1),
    assume_equal_variance = FALSE,
    correct_bias = FALSE
  )

  CI_smd_ind_contrast(
    means = c(71.66667, 69.66667),
    sds = c(13.44123, 14.13742),
    ns = c(6, 6),
    contrast = c(1, -1),
    assume_equal_variance = FALSE,
    correct_bias = TRUE
  )

  # deffectsize::cohen_CI(
  #   m1 = 71.66667,
  #   m2 = 69.66667,
  #   sd1 = 13.44123,
  #   sd2 = 14.13742,
  #   n1 = 6,
  #   n2 = 6,
  #   conf.level = 0.95,
  #   var.equal = FALSE,
  #   unbiased = FALSE
  # )
  #
  # deffectsize::cohen_CI(
  #   m1 = 71.66667,
  #   m2 = 69.66667,
  #   sd1 = 13.44123,
  #   sd2 = 14.13742,
  #   n1 = 6,
  #   n2 = 6,
  #   conf.level = 0.95,
  #   var.equal = TRUE,
  #   unbiased = TRUE
  # )
  #

  # Example - Should raise a warning if >2 groups, unequal var, and correction
  #  requested
  CI_smd_ind_contrast(
    means = c(13, 11, 15),
    sds = c(2.738613, 2.236068, 2.000000),
    ns = c(5, 5, 5),
    contrast = c(1, 0, -1),
    assume_equal_variance = FALSE,
    correct_bias = TRUE
  )

}

