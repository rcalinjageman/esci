# Estimate any meta effect.

`meta_any` is suitable for synthesizing any effect size across multiple
studies. You must provide the effect size for each study and the
predicted sampling variance for each study.

## Usage

``` r
meta_any(
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
  method = c("DL", "REML", "PM"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame or tibble with columns

- yi:

  Name a column in data containing the effect size for each study

- vi:

  Name of a column in data containing the expected sampling variance for
  each study

- labels:

  Name of a column in data containing a label for each study

- moderator:

  Optional name of a column in data containing a factor as a categorical
  moderator

- contrast:

  Optional vector specifying a contrast analysis for the categorical
  moderator. Only define if a moderator is defined; vector length should
  match number of levels in the moderator

- effect_label:

  Optional human-friendly name for the effect being synthesized;
  defaults to 'My effect'

- effect_size_name:

  Optional human-friendly name of the effect size being synthesized;
  defaults to 'Effect size'

- moderator_variable_name:

  Optional human-friendly name of the moderator, if defined; If not
  passed but a moderator is defined, will be set to the quoted name of
  the moderator column or 'My moderator'

- random_effects:

  Use TRUE to obtain a random effect meta-analysis (usually
  recommended); FALSE for fixed effect.

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

\#' Once you generate an estimate with this function, you can visualize
it with
[`plot_meta()`](https://rcalinjageman.github.io/esci/reference/plot_meta.md).

The meta-analytic effect size, confidence interval and heterogeneity
estimates all come from
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

The diamond ratio and its confidence interval come from
[`CI_diamond_ratio()`](https://rcalinjageman.github.io/esci/reference/CI_diamond_ratio.md).

## Examples

``` r
#' # Data set -- see Introduction to the New Statistics, 2nd edition
data("data_mccabemichael_brain")

# Fixed effect, 95% CI
esizes <- esci::meta_mean(
  data = esci::data_mccabemichael_brain,
  means = "M Brain",
  sds = "s Brain",
  ns = "n Brain",
  labels = "Study name",
  random_effects = FALSE
)$raw_data

estimate <- esci::meta_any(
  data = esizes,
  yi = effect_size,
  vi = sample_variance,
  labels = label,
  effect_size_name = "Mean",
  random_effects = FALSE
)

myplot_forest <- esci::plot_meta(estimate)


```
