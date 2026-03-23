# Estimate meta-analytic d_s or d_avg across multiple two group studies.

`meta_smd_two` is suitable for synthesizing across multiple two-group
studies where the outcome variable is continuous but not all studies are
measured on the same scale.

## Usage

``` r
meta_smd_two(
  data,
  smds,
  comparison_ns,
  reference_ns,
  labels = NULL,
  moderator = NULL,
  contrast = NULL,
  effect_label = "My effect",
  assume_equal_variance = FALSE,
  correct_bias = TRUE,
  esci_vi = FALSE,
  random_effects = TRUE,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame or tibble

- smds:

  Name of a column in data containing Cohen's d for each study

- comparison_ns:

  Name of a column in data containing the sample size for the comparison
  group in each study; each should be an integer \> 0.

- reference_ns:

  Name of a column in data containing the sample size for the reference
  group in each study; each should be an integer \> 0.

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

- assume_equal_variance:

  TRUE to assume equal variance, meaning effect sizes provided are d_s;
  FALSE to not assume equal variance, meaning effect sizes provided as
  d_avg

- correct_bias:

  TRUE to apply bias correction to effect sizes provided before
  conducting the meta-analysis

- esci_vi:

  TRUE to report the sampling variance based on Bonnett rather than
  metafor

- random_effects:

  Use TRUE to obtain a random effect meta-anlaysis (usually
  reccomended); FALSE for fixed effect.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

## Value

An esci-estimate object; a list of data frames and properties. Returned
tables include:

- es_meta - A data frame with one overall. If a moderator was defined,
  there is an additional row for each level of the moderator.

  - effect_label - Study label

  - effect_size - Effect size

  - LL - Lower bound of conf_level% confidence interval

  - UL - Upper bound of conf_level% confidence interval

  - SE - Expected standard error

  - k - Number of studies

  - diamond_ratio - ratio of random to fixed effects meta-analtics
    effect sizes

  - diamond_ratio_LL - lower bound of conf_level% confidence interval
    for diamond ratio

  - diamond_ratio_UL - upper bound of conf_level% confidence interval
    for diamond ratio

  - I2 - I2 measure of heterogeneity

  - I2_LL - Lower bound of conf_level% confidence interval for I2

  - I2_UL - upper bound of conf_level% confidence interval for I2

  - PI_LL - lower bound of conf_level% of prediction interval

  - PI_UL - upper bound of conf_level% of prediction interval

  - p - p value for the meta-analytic effect size, based on null of
    exactly 0

  - width - width of the effect-size confidence interval

  - FE_effect_size - effect size of the fixed-effects model (regardless
    of if fixed effects was selected

  - RE_effect_size - effect size of the random-effects model (regardless
    of if random effects was selected

  - FE_CI_width - width of the fixed-effects confidence interval, used
    to calculate diamond ratio

  - RE_CI_width - width of the fixed-effects confidence interval, used
    to calculate diamond ratio

- es_heterogeneity - A data frame of of heterogeneity values and
  conf_level% CIs for the meta-analytic effect size. If a moderator was
  defined also reports heterogeneity estimates for each level of the
  moderator.

  - effect_label - study label

  - moderator_variable_name - if moderator passed, gives name of the
    moderator

  - moderator_variable_name - 'Overall' and each level of moderator, if
    passed

  - measure - Name of the measure of heterogeneity

  - LL - lower bound of conf_level% confidence interval

  - UL - upper bound of conf_level% confidence interval

- raw_data - A data from with one row for each study that was passed

  - label - study label

  - effect_size - effect size

  - weight - study weight in the meta analysis

  - sample_variance - expected level of sampling variation

  - SE - expected standard error

  - LL - lower bound of conf_level% confidence interval

  - UL - upper bound of conf_level% confidence interval

  - mean - used to calculate study p value; this is the d value entered
    for the study

  - n - study sample size

  - sd - use to calculate study p value; set to 1 for each study

  - p - p value for the study, based on null of exactly 0

## Details

Each study's effect size should be expressed as d_s or d_avg. The
function
[`estimate_mdiff_two()`](https://rcalinjageman.github.io/esci/reference/estimate_mdiff_two.md)
can provide these effect sizes from raw data.

The meta-analytic effect size, confidence interval and heterogeneity
estimates all come from
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

The diamond ratio and its confidence interval come from
[`CI_diamond_ratio()`](https://rcalinjageman.github.io/esci/reference/CI_diamond_ratio.md).
