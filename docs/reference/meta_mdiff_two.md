# Estimate meta-analytic difference in means across multiple two-group studies.

`meta_mdiff_two` is suitable for synthesizing across multiple two-group
studies (paired or independent) with a continuous outcome measure. It
takes in raw data from each study. If all studies used the same
measurement scale, a meta-analytic raw-score difference can be returned.
If studies used different scales, a standardized mean difference can be
returned. Studies can be all paired, all independent, or a mix. Equal
variance can be assumed, or not. If standardized mean difference is the
output, it is d_s when equal variance is assumed and d_avg when equal
variance is not assumed.

## Usage

``` r
meta_mdiff_two(
  data,
  comparison_means,
  comparison_sds,
  comparison_ns,
  reference_means,
  reference_sds,
  reference_ns,
  r = NULL,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  reported_effect_size = c("mean_difference", "smd_unbiased", "smd"),
  assume_equal_variance = FALSE,
  random_effects = TRUE,
  method = c("DL", "REML", "PM"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame or tibble

- comparison_means:

  Set of comparison_group means, 1 per study

- comparison_sds:

  Set of comparison_group standard deviations, 1 per study, all \> 0

- comparison_ns:

  Set of comparison_group sample sizes, positive integers, 1 for each
  study

- reference_means:

  Set of reference_group means, 1 per study

- reference_sds:

  Set of comparison_group standard deviations, 1 per study, all \> 0

- reference_ns:

  Set of reference_group sample sizes, positive integers, 1 for each
  study

- r:

  Optional correlation between measures for w-s studies, NA otherwise

- labels:

  An optional collection of study labels

- moderator:

  An optional factor to analyze as a categorical moderator, must have k
  \> 2 per groups

- contrast:

  An optional contrast to estimate between moderator levels; express as
  a vector of contrast weights with 1 weight per moderator level.

- effect_label:

  Optional character giving a human-friendly name of the effect being
  synthesized

- reported_effect_size:

  Character specifying effect size to return: Must be one of
  'mean_difference', 'smd_unbiased' (to return an unbiased Cohen's d_s
  or d_avg) or 'smd' (to return d_s or d_avg without correction for
  bias). Defaults to mean_difference.

- assume_equal_variance:

  Defaults to FALSE

- random_effects:

  TRUE for random effect model; FALSE for fixed effects

- method:

  If not fixed effects, this controls the approach. Defaults to 'DL',
  other options are REML and PM

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

## Value

An esci-estimate object; a list of data frames and properties. Returned
tables include:

- **es_meta** - A data frame of meta-analytic effect sizes. If a
  moderator was defined, there is an additional row for each level of
  the moderator.

  - *effect_label* - Study label

  - *effect_size* - Effect size

  - *LL* - Lower bound of conf_level% confidence interval

  - *UL* - Upper bound of conf_level% confidence interval

  - *SE* - Expected standard error

  - *k* - Number of studies

  - *diamond_ratio* - ratio of random to fixed effects meta-analytic
    effect sizes

  - *diamond_ratio_LL* - lower bound of conf_level% confidence interval
    for diamond ratio

  - *diamond_ratio_UL* - upper bound of conf_level% confidence interval
    for diamond ratio

  - *I2* - I2 measure of heterogeneity

  - *I2_LL* - Lower bound of conf_level% confidence interval for I2

  - *I2_UL* - upper bound of conf_level% confidence interval for I2

  - *PI_LL* - lower bound of conf_level% of prediction interval

  - *PI_UL* - upper bound of conf_level% of prediction interval

  - *p* - p value for the meta-analytic effect size, based on null of
    exactly 0

  - \*width - width of the effect-size confidence interval

  - *FE_effect_size* - effect size of the fixed-effects model
    (regardless of if fixed effects was selected

  - *RE_effect_size* - effect size of the random-effects model
    (regardless of if random effects was selected

  - *FE_CI_width* - width of the fixed-effects confidence interval, used
    to calculate diamond ratio

  - *RE_CI_width* - width of the fixed-effects confidence interval, used
    to calculate diamond ratio

- **es_heterogeneity** - A data frame of of heterogeneity values and
  conf_level% CIs for the meta-analytic effect size. If a moderator was
  defined also reports heterogeneity estimates for each level of the
  moderator.

  - *effect_label* - study label

  - *moderator_variable_name* - if moderator passed, gives name of the
    moderator

  - *moderator_level* - 'Overall' and each level of moderator, if passed

  - *measure* - Name of the measure of heterogeneity

  - *estimate* - Value of the heterogeneity estimate

  - *LL* - lower bound of conf_level% confidence interval

  - *UL* - upper bound of conf_level% confidence interval

- **raw_data** - A data from with one row for each study that was passed

  - *label* - study label

  - *effect_size* - effect size

  - *weight* - study weight in the meta analysis

  - *sample_variance* - expected level of sampling variation

  - *SE* - expected standard error

  - *LL* - lower bound of conf_level% confidence interval

  - *UL* - upper bound of conf_level% confidence interval

  - *mean* - used to calculate study p value; this is the d value
    entered for the study

  - *sd* - use to calculate study p value; set to 1 for each study

  - *n* - study sample size

  - *p* - p value for the study, based on null of exactly 0

## Details

Once you generate an estimate with this function, you can visualize it
with
[`plot_meta()`](https://rcalinjageman.github.io/esci/reference/plot_meta.md).

The meta-analytic effect size, confidence interval and heterogeneity
estimates all come from
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

The diamond ratio and its confidence interval come from
[`CI_diamond_ratio()`](https://rcalinjageman.github.io/esci/reference/CI_diamond_ratio.md).

If reported_effect_size is smd_unbiased or smd the conversion to Cohen's
d is handled by
[`CI_smd_ind_contrast()`](https://rcalinjageman.github.io/esci/reference/CI_smd_ind_contrast.md).

## Examples

``` r
# Data set -- see Introduction to the New Statistics, 2nd edition
data("data_mccabemichael_brain")

# Meta-analysis: random effects, no moderator
estimate <- esci::meta_mdiff_two(
  data = esci::data_mccabemichael_brain,
  comparison_means = "M Brain",
  comparison_sds = "s Brain",
  comparison_ns = "n Brain",
  reference_means = "M No Brain",
  reference_sds = "s No Brain",
  reference_ns = "n No Brain",
  labels = "Study name",
  effect_label = "Brain Photo Rating - No Brain Photo Rating",
  assume_equal_variance = TRUE,
  random_effects = TRUE
)
# Forest plot
myplot_forest <- esci::plot_meta(estimate)


# Meta-analysis: random effects, moderator
estimate_moderator <- esci::meta_mdiff_two(
  data = esci::data_mccabemichael_brain,
  comparison_means = "M Brain",
  comparison_sds = "s Brain",
  comparison_ns = "n Brain",
  reference_means = "M No Brain",
  reference_sds = "s No Brain",
  reference_ns = "n No Brain",
  labels = "Study name",
  moderator = "Research group",
  effect_label = "Brain Photo Rating - No Brain Photo Rating",
  assume_equal_variance = TRUE,
  random_effects = TRUE
)
# Forest plot
myplot_forest_moderator <- esci::plot_meta(estimate_moderator)
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> Scale for x is already present.
#> Adding another scale for x, which will replace the existing scale.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.


# Meta-analysis: random effects, moderator, output d_s
estimate_moderator_d <- esci::meta_mdiff_two(
  data = esci::data_mccabemichael_brain,
  comparison_means = "M Brain",
  comparison_sds = "s Brain",
  comparison_ns = "n Brain",
  reference_means = "M No Brain",
  reference_sds = "s No Brain",
  reference_ns = "n No Brain",
  labels = "Study name",
  moderator = "Research group",
  effect_label = "Brain Photo Rating - No Brain Photo Rating",
  assume_equal_variance = TRUE,
  random_effects = TRUE
)
# Forest plot
myplot_forest_moderator_d <- esci::plot_meta(estimate_moderator_d)
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> Scale for x is already present.
#> Adding another scale for x, which will replace the existing scale.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> `height` was translated to `width`.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.

```
