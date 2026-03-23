# Estimate a meta-analytic Cohen's d1 across multiple studies

`meta_d1` is suitable for synthesizing across multiple single-group
studies with a continuous outcome variable, but where the outcome is not
measured on the same scale in all studies

## Usage

``` r
meta_d1(
  data,
  ds,
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

  A data frame or tibble

- ds:

  Set of bias-adjusted cohen's d1 values, 1 for each study

- ns:

  Set of sample sizes, positive integers, 1 for each study

- labels:

  Optional set of labels, 1 for each study

- moderator:

  Optional factor as a categorical moderator; should have k \> 2 per
  group

- contrast:

  Optional vector specifying a contrast between moderator levels

- effect_label:

  Optional character providing a human-friendly label for the effect

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

Each study's effect size should be expressed as Cohen's d1: (mean -
reference) / sd.

And the d1 values should all be corrected for bias. The function
[`CI_smd_one()`](https://rcalinjageman.github.io/esci/reference/CI_smd_one.md)
can assist with converting raw data from each study to d1_unbiased.

The meta-analytic effect size, confidence interval and heterogeneity
estimates all come from
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).

The diamond ratio and its confidence interval come from
[`CI_diamond_ratio()`](https://rcalinjageman.github.io/esci/reference/CI_diamond_ratio.md).

## Examples

``` r
# example code
  original_7 <- data.frame(
study_name = c(
  "Aden (1993)"  ,
  "Buggs (1995)"  ,
  "Crazed (1999)"  ,
  "Dudley (2003)"  ,
  "Evers (2005)"  ,
  "Fox (2009)",
  "Mine (2011)"
),
rt_mean = c(
  454  ,
  317  ,
  430  ,
  525  ,
  479  ,
  387,
  531
),
rt_sd = c(
  142  ,
  158  ,
  137  ,
  260  ,
  144  ,
  165,
  233
),
rt_n = c(
  24  ,
  7  ,
  20  ,
  8  ,
 14  ,
 13,
  18
),
subset = as.factor(
  c(
    "90s",
   "90s",
    "90s",
    "00s",
    "00s",
    "00s",
    "00s"
  )
),
d1_unbiased = c(
  3.091587,
  1.742751,
  3.012857,
  1.793487,
  3.130074,
  2.195209,
  2.17667
)
)


# Fixed effect, 95% CI
estimate <- esci::meta_d1(
  original_7,
  d1_unbiased,
  rt_n,
  study_name,
  random_effects = FALSE
)

# Forest plot
myplot_forest <- esci::plot_meta(estimate)


# Add a moderator
estimate_moderator <- esci::meta_d1(
  data = original_7,
  ds = d1_unbiased,
  ns = rt_n,
  moderator = subset,
  labels = study_name,
  random_effects = FALSE
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
