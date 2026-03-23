# Estimate standardized mean difference (Cohen's d) for an independent groups contrast

`CI_smd_ind_contrast` returns the point estimate and confidence interval
for a standardized mean difference (smd aka Cohen's *d* aka Hedges *g*).
A standardized mean difference is a difference in means standardized to
a standard deviation: \\d = \frac{ \psi }{s}\\

## Usage

``` r
CI_smd_ind_contrast(
  means,
  sds,
  ns,
  contrast,
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  correct_bias = TRUE
)
```

## Arguments

- means:

  A vector of 2 or more means

- sds:

  A vector of standard deviations, same length as means

- ns:

  A vector of sample sizes, same length as means

- contrast:

  A vector of group weights, same length as means

- conf_level:

  The confidence level for the confidence interval, in decimal form.
  Defaults to 0.95.

- assume_equal_variance:

  Defaults to FALSE

- correct_bias:

  Defaults to TRUE; attempts to correct the slight upward bias in d
  derived from a sample. As of 8/9/2023 - Bias correction has been added
  for more than 2 groups when equal variance is not assumed, based on
  recent updates to statpsych

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

## Details

### It's a bit complicated

A standardized mean difference turns out to be complicated.

First, it has many names:

- standardized mean difference (smd)

- Cohen's *d*

- When bias in a sample d has been corrected, also called Hedge's *g*

Second, the choice of the standardizer requires thought:

- sd_pooled - used when assuming all groups have exact same variance

- sd_avg - does not require assumption of equal variance

- other possibilities, too, but not dealt with in this function

The choice of standardizer is important, so it's noted in the subscript:

- d_s – assumes equal variance, standardized to sd_pooled

- d_avg - does not assume equal variance, standardized to sd_avg

A third complication is the issue of bias: d estimated from a sample has
a slight upward bias at smaller sample sizes. With total sample size \>
30, this slight bias becomes fairly negligible (kind of like the small
upward bias in a sample standard deviation).

This bias can be corrected when equal variance is assumed or when the
design of the study is simple (2 groups). For complex designs (\>2
groups) without the assumption of equal variance, there is now also an
approximate approach to correcting bias from Bonett.

Corrections for bias produce a long-run reduction in average bias.
Corrections for bias are approximate.

### How are d and its CI calculated?

#### When equal variance is assumed

When equal variance is assumed, the standardized mean difference is d_s,
defined in Kline, p. 196:

\\ d_s = \frac{ \psi }{ sd\_{pooled} } \\

where psi is defined in Kline, equation 7.8:

\\ \psi =\sum\_{i=1}^{a}c_iM_i \\

and where sd_pooled is defined in Kline, equation 3.11 \\sd\_{pooled} =
{ \frac{ \sum\_{i=1}^{a} (n_i -1) s_i^2 } { \sum\_{i=1}^{a} (n_i-1) }
}\\

The CI for d_s is derived from lambda-prime transformation from
Lecoutre, 2007 with code adapted from Cousineau & Goulet-Pelletier,
2020. Kelley, 2007 explains the general approach for linear contrasts.

This approach to generating the CI is 'exact', meaning coverage should
be as desired *if* all assumptions are met (ha!).

Correction of upward bias can be applied.

#### When equal variance is not assumed

When equal variance is not assumed, the standardized mean difference is
d_avg, defined in Bonett, equation 6:

\\ d\_{avg} = \frac{ \psi }{ sd\_{avg} }\\

Where sd_avg is the square root of the average of the group variances,
as given in Bonett, explanation of equation 6:

\\sd\_{avg} = \sqrt{ \frac{ \sum\_{i=1}^{a} s_i^2 }{ a } }\\

##### If only 2 groups

- The CI is derived from lambda-prime transformation using df and se
  from Huynh, 1989 – see especially Delacre et al., 2021

- This is also an 'exact' approach, and correction can be applied

##### If more than 2 groups

- CI is approximated using the approach from Bonett, 2008

- An approximate correction developed by Bonett is used

## References

- Bonett D. G. (2023). statpsych: Statistical Methods for Psychologists.
  R package version 1.4.0.
  [https://dgbonett.github.io/statpsych](https://dgbonett.github.io/statpsych/)

- Bonett, D. G. (2018). R code posted to personal website (now removed).
  Formally at https://people.ucsc.edu/~dgbonett/psyc204.html

- Bonett, D. G. (2008). Confidence Intervals for Standardized Linear
  Contrasts of Means. *Psychological Methods*, 13(2), 99–109.
  [doi:10.1037/1082-989X.13.2.99](https://doi.org/10.1037/1082-989X.13.2.99)

- Cousineau & Goulet-Pelletier (2020) A review of five techniques to
  derive confidence intervals with a special attention to the Cohen’s dp
  in the between-group design.
  [doi:10.31234/osf.io/s2597](https://doi.org/10.31234/osf.io/s2597)

- Delacre M, Lakens D, Ley C, Liu L, Leys C (2021) Why Hedges gs based
  on the non-pooled standard deviation should be reported with Welch’s
  t-test.
  [doi:10.31234/osf.io/tu6mp](https://doi.org/10.31234/osf.io/tu6mp)

- Huynh, C.-L. (1989). A unified approach to the estimation of effect
  size in meta-analysis. NBER Working Paper Series, 58(58), 99–104.

- Kelley, K. (2007). Confidence intervals for standardized effect sizes:
  Theory, application, and implementation. *Journal of Statistical
  Software, 20(8)*, 1–24.
  [doi:10.18637/jss.v020.i08](https://doi.org/10.18637/jss.v020.i08)

- Lecoutre, B. (2007). Another Look at the Confidence Intervals for the
  Noncentral T Distribution. Journal of Modern Applied Statistical
  Methods, 6(1), 107–116.
  [doi:10.22237/jmasm/1177992600](https://doi.org/10.22237/jmasm/1177992600)

## See also

- [`estimate_mdiff_ind_contrast`](https://rcalinjageman.github.io/esci/reference/estimate_mdiff_ind_contrast.md)
  for friendly version that also returns raw score effect sizes for this
  design

## Examples

``` r
# Example from Kline, 2013
#  Data in Table 3.4
#  Worked out in Chapter 7
#  See p. 202, non-central approach
# With equal variance assumed and no correction, should give:
#   d_s = -0.8528028 [-2.121155, 0.4482578]

res <- esci::CI_smd_ind_contrast(
  means = c(13, 11, 15),
  sds = c(2.738613, 2.236068, 2.000000),
  ns = c(5, 5, 5),
  contrast = contrast <- c(1, 0, -1),
  conf_level = 0.95,
  assume_equal_variance = TRUE,
  correct_bias = FALSE
)


# Example from [statpsych::ci.lc.stdmean.bs()] should give:
# Estimate        SE        LL         UL
# Unweighted standardizer: -1.273964 0.3692800 -2.025039 -0.5774878
# Weighted standardizer:   -1.273964 0.3514511 -1.990095 -0.6124317
# Group 1 standardizer:    -1.273810 0.4849842 -2.343781 -0.4426775


res <- esci::CI_smd_ind_contrast(
  means = c(33.5, 37.9, 38.0, 44.1),
  sds = c(3.84, 3.84, 3.65, 4.98),
  ns = c(10,10,10,10),
  contrast = c(.5, .5, -.5, -.5),
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  correct_bias = TRUE
)


```
