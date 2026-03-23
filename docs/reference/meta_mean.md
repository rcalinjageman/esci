# Estimate a meta-analytic mean across multiple single-group studies.

`meta_mean` is suitable for synthesizing across multiple single-group
studies with a continuous outcome variable when all studies are measured
on the same scale.

## Usage

``` r
meta_mean(
  data,
  means,
  sds,
  ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  reference_mean = 0,
  reported_effect_size = c("mean_difference", "smd_unbiased", "smd"),
  random_effects = TRUE,
  method = c("DL", "REML", "PM"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A dataframe or tibble

- means:

  A collection of study means, 1 per study

- sds:

  A collection of study standard deviations, 1 per study, all \>0

- ns:

  A collection of sample sizes, 1 per study, all integers \> 2

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

- reference_mean:

  Optional reference mean, defaults to 0

- reported_effect_size:

  Character specifying effect size to return; Must be one of
  'mean_difference', 'smd_unbiased' (to return an unbiased Cohen's d1)
  or 'smd' (to return Cohen's d1 without correction for bias)

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

The meta-analytic effect size, confidence interval and heterogeneity
estimates all come from
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

The diamond ratio and its confidence interval come from
[`CI_diamond_ratio()`](https://rcalinjageman.github.io/esci/reference/CI_diamond_ratio.md).

If reported_effect_size is smd_unbiased or smd the conversion to d1 is
handled by
[`CI_smd_one()`](https://rcalinjageman.github.io/esci/reference/CI_smd_one.md).

## Examples

``` r
# Data set -- see Introduction to the New Statistics, 2nd edition
data("data_mccabemichael_brain")

# Fixed effect, 95% CI
estimate <- esci::meta_mean(
  data = esci::data_mccabemichael_brain,
  means = "M Brain",
  sds = "s Brain",
  ns = "n Brain",
  labels = "Study name",
  random_effects = FALSE
)

myplot_forest <- esci::plot_meta(estimate)


# Add a moderator, report cohen's d1
estimate_moderator_d <- esci::meta_mean(
  data = esci::data_mccabemichael_brain,
  means = "M Brain",
  sds = "s Brain",
  ns = "n Brain",
  labels = "Study name",
  moderator = "Research group",
  reported_effect_size = "smd_unbiased",
  random_effects = FALSE
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
