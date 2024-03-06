#' Estimate standardized mean difference (Cohen's d1) for a single group
#'
#' @description
#' `CI_smd_one` STILL NEEDS WORK TO VERIFY APPROACH FOR SE and MoE
#'
#'
#' @param mean Mean for a single group for the outcome measure
#' @param sd Standard deviation, > 0
#' @param n Sample size, an integer > 2
#' @param reference_mean defaults to 0
#' @param conf_level The confidence level for the confidence interval, in
#'   decimal form.  Defaults to 0.95.
#' @param correct_bias Defaults to TRUE
#'
#' @return Returns a list with these named elements:
#' * effect_size - the point estimate from the sample
#' * lower - lower bound of the CI
#' * upper - upper bound of the CI
#' * numerator - the numerator for Cohen's d_biased;
#'    the mean difference in the contrast
#' * denominator - the denominator for Cohen's d_biased;
#'     if equal variance is assumed this is sd_pooled, otherwise sd_avg
#' * df - the degrees of freedom used for correction and CI calculation
#' * se - the standard error of the estimate; **warning** not totally
#'     sure about this yet
#' * moe - margin of error; 1/2 length of the CI
#' * d_biased - Cohen's d without correction applied
#' * properties - a list of properties for the result
#'
#' Properties
#' * effect_size_name - if equal variance assumed d_s, otherwise d_avg
#' * effect_size_name_html - html representation of d_name
#' * denominator_name - if equal variance assumed sd_pooled otherwise sd_avg
#' * denominator_name_html - html representation of denominator name
#' * bias_corrected - TRUE/FALSE if bias correction was applied
#' * message - a message explaining denominator and correction status
#' * message_html - html representation of message
#'
#'
#' @examples
#' # example code
#' esci::CI_smd_one(24.5, 3.65, 40, 20)
#'
#' @export
CI_smd_one <- function(
  mean,
  sd,
  n,
  reference_mean,
  conf_level = 0.95,
  correct_bias = TRUE
) {


  # degrees of freedom for each mean
  df <- n-1
  variance <- sd^2

  # Correction factor
  # Cousineau & Goulet-Pelletier, 2020 from Hedges
  J <- if(correct_bias)
    exp ( lgamma(df/2) - (log(sqrt(df/2)) + lgamma((df-1)/2) ) )
  else
    1

  # Calculate d -----------------------------------------------------------
  numerator <- mean - reference_mean
  denominator <- sd
  d_biased <- numerator / denominator
  effect_size <- d_biased * J

  # Calculate CI --------------------------------------------------------
  # We can use the exact approach
  # Cousineau & Goulet-Pelletier, 2020; method of Lecoutre, 2007

  lambda <- effect_size / sqrt(1/n)

  back_from_lambda <- sqrt(sum(1/n))

  lambda_low <- sadists::qlambdap(1/2-conf_level/2, df = df, t = lambda )
  lambda_high <- sadists::qlambdap(1/2+conf_level/2, df = df, t = lambda )

  # SE when equal variance from p. 111 of Lecoutre, 2007
  #  thanks to Cousineau personal communication
  k <- sqrt(2/df)* exp((lgamma((df+1)/2)) - (lgamma((df)/2)))
  lambda_se <- sqrt(1 + lambda^2 * (1-k^2))
  se <- lambda_se * back_from_lambda

  lower <- lambda_low * back_from_lambda
  upper <- lambda_high * back_from_lambda
  moe <- (upper-lower)/2

  properties <- list(
    effect_size_name = "d_1",
    effect_size_name_html = if (correct_bias)
        "<i>d</i><sub>1</sub>"
      else
        "<i>d</i><sub>1.biased</sub>",
    denominator_name = "s",
    denominator_name_html = "<i>s</i>",
    bias_corrected = correct_bias
  )

  properties$effect_size_category = "difference"
  properties$effect_size_precision = "magnitude"
  properties$conf_level = conf_level

  see_biased <- if(correct_bias)
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



