# Estimate meta-analytic Pearson's r across multiple studies with two continuous outcome variables.

`meta_r` is suitable for synthesizing across multiple studies that have
measured a linear correlation (Pearson's r) from two continuous
variables.

## Usage

``` r
meta_r(
  data,
  rs,
  ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  random_effects = TRUE,
  method = c("DL", "REML", "PM"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A dataframe or tibble

- rs:

  A collection of Pearson's r values, 1 per study, all between -1 and 1,
  inclusive

- ns:

  A collection of study sample sizes, all integers \> 2

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

## Examples

``` r
# Data: See Introduction to the New Statistics, first edition
esci_single_r <- data.frame(
  studies = c(
    'Violin, viola'  ,
    'Strings'  ,
    'Piano'  ,
    'Piano'  ,
    'Piano'  ,
    'Piano'  ,
    'Piano'  ,
    'Piano'  ,
    'Piano'  ,
    'All'  ,
    'Piano'  ,
    'Piano'  ,
    'Band'  ,
    'Music majors'  ,
    'Music majors'  ,
    'All'
  ),
  rvalues = c(
    .67,
    .51,
    .4,
    .46,
    .47,
    .228,
    -.224,
    .104,
    .322,
    .231,
    .67,
    .41,
    .34,
    .31,
    .54,
    .583
  ),
  sample_size = c(
    109,
    55,
    19,
    30,
    19,
    52,
    24,
    52,
    16,
    97,
    57,
    107,
    178,
    64,
    19,
    135
  ),
  subsets = as.factor(
    c(
      'Strings'  ,
      'Strings'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Piano'  ,
      'Strings'  ,
      'Strings'  ,
      'Strings'  ,
      'Strings'
    )
 )
)

# Meta-analysis, random effects, no moderator
estimate <- esci::meta_r(
  esci_single_r,
  rvalues,
  sample_size,
  studies,
  random_effects = TRUE
)

# Forest plot
myplot_forest <- esci::plot_meta(estimate)


# Meta-analysis, random effects, moderator (subsets)
estimate_moderator <- esci::meta_r(
  esci_single_r,
  rvalues,
  sample_size,
  studies,
  subsets,
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

```
