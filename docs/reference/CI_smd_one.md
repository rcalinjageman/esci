# Estimate standardized mean difference (Cohen's d1) for a single group

`CI_smd_one` STILL NEEDS WORK TO VERIFY APPROACH FOR SE and MoE

## Usage

``` r
CI_smd_one(mean, sd, n, reference_mean, conf_level = 0.95, correct_bias = TRUE)
```

## Arguments

- mean:

  Mean for a single group for the outcome measure

- sd:

  Standard deviation, \> 0

- n:

  Sample size, an integer \> 2

- reference_mean:

  defaults to 0

- conf_level:

  The confidence level for the confidence interval, in decimal form.
  Defaults to 0.95.

- correct_bias:

  Defaults to TRUE

## Value

Returns a list with these named elements:

- effect_size - the point estimate from the sample

- lower - lower bound of the CI

- upper - upper bound of the CI

- numerator - the numerator for Cohen's d_biased; the mean difference in
  the contrast

- denominator - the denominator for Cohen's d_biased; if equal variance
  is assumed this is sd_pooled, otherwise sd_avg

- df - the degrees of freedom used for correction and CI calculation

- se - the standard error of the estimate; **warning** not totally sure
  about this yet

- moe - margin of error; 1/2 length of the CI

- d_biased - Cohen's d without correction applied

- properties - a list of properties for the result

Properties

- effect_size_name - if equal variance assumed d_s, otherwise d_avg

- effect_size_name_html - html representation of d_name

- denominator_name - if equal variance assumed sd_pooled otherwise
  sd_avg

- denominator_name_html - html representation of denominator name

- bias_corrected - TRUE/FALSE if bias correction was applied

- message - a message explaining denominator and correction status

- message_html - html representation of message

## Examples

``` r
# example code
res <- esci::CI_smd_one(24.5, 3.65, 40, 20)
```
