# Estimate meta-analytic standardized mean difference across multiple two group studies (all paired, all independent, or a mix).

`meta_d2` is suitable for synthesizing across multiple two-group studies
(paired or independent) with a continuous outcome measure but where not
all studies are measured on the same scale, and instead the magnitude of
difference for each study is expressed as d_s or d_avg.

## Usage

``` r
meta_d2(
  data,
  ds,
  comparison_ns,
  reference_ns,
  r = NULL,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  assume_equal_variance = FALSE,
  random_effects = TRUE,
  method = c("DL", "REML", "PM"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame or tibble

- ds:

  Set of bias-adjusted cohen's d_s or d_avg values, 1 for each study

- comparison_ns:

  Set of comparison_group sample sizes, positive integers, 1 for each
  study

- reference_ns:

  Set of reference_groups sample sizes, positive integers, 1 for each
  study

- r:

  optional correlation between measures for w-s studies, NA otherwise

- labels:

  Optional set of labels, 1 for each study

- moderator:

  Optional factor as a categorical moderator; should have k \> 2 per
  group

- contrast:

  Optional vector specifying a contrast between moderator levels

- effect_label:

  Optional character providing a human-friendly label for the effect

- assume_equal_variance:

  Defaults to FALSE

- random_effects:

  Boolean; TRUE for a random effects model; otherwise fixed effects

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

Each study's effect size should be expressed as: Cohen's d_s:
(comparison_mean - reference_mean) / sd_pooled or Cohen\_'s d_avg:
(comparison_mean - reference_mean) / sd_avg

To enter d_s, set assume_equal_variance to TRUE To enter d_avg, set
assume_equal_variance to FALSE

And the d values should all be corrected for bias. The function
[`CI_smd_ind_contrast()`](https://rcalinjageman.github.io/esci/reference/CI_smd_ind_contrast.md)
can assist with converting raw data from each study to d_s or d_avg with
bias correction. It also has more details on calculation of these forms
of d and their CIs.

The meta-analytic effect size, confidence interval and heterogeneity
estimates all come from
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

The diamond ratio and its confidence interval come from
[`CI_diamond_ratio()`](https://rcalinjageman.github.io/esci/reference/CI_diamond_ratio.md).

## Examples

``` r
# Data set -- see Introduction to the New Statistics, 1st edition
data("data_damischrcj")

# Meta-analysis, random effects, assuming equal variance, no moderator
estimate <- esci::meta_d2(
  data = esci::data_damischrcj,
  ds = "Cohen's d unbiased",
  comparison_ns = "n Control",
  reference_ns = "n Lucky",
  labels = Study,
  assume_equal_variance = TRUE,
  random_effects = TRUE
)

# Forest plot
myplot_forest <- esci::plot_meta(estimate)


# Add a categorical moderator
estimate_moderator <- esci::meta_d2(
  data = esci::data_damischrcj,
  ds = "Cohen's d unbiased",
  comparison_ns = "n Control",
  reference_ns = "n Lucky",
  labels = "Study",
  moderator = "Research Group",
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
```
