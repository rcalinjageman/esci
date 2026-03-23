# Estimates for a multi-group design with a continuous outcome variable

Returns object `estimate_mdiff_ind_contrast` is suitable for a
multi-group design (between subjects) with a continuous outcome
variable. It accepts a user-defined set of contrast weights that allows
estimation of any 1-df contrast. It can express estimates as mean
differences, standardized mean differences (Cohen's d) or median
differences (raw data only). You can pass raw data or summary data.

## Usage

``` r
estimate_mdiff_ind_contrast(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  contrast = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - a data frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- grouping_variable:

  For raw data - The column name of the grouping variable, or a vector
  of group names

- means:

  For summary data - A vector of 2 or more means

- sds:

  For summary data - A vector of standard deviations, same length as
  means

- ns:

  For summary data - A vector of sample sizes, same length as means

- contrast:

  A vector of group weights, same length as number of groups.

- grouping_variable_levels:

  For summary data - An optional vector of group labels, same length as
  means

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  outcome variable' or the outcome variable column name if a data frame
  is passed.

- grouping_variable_name:

  Optional friendly name for the grouping variable. Defaults to 'My
  grouping variable' or the grouping variable column name if a
  data.frame is passed.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

- assume_equal_variance:

  Defaults to FALSE

- save_raw_data:

  For raw data; defaults to TRUE; set to FALSE to save memory by not
  returning raw data in estimate object

## Value

Returns object of class esci_estimate

- **es_mean_difference**

  - *type* -

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

- **es_median_difference**

  - *type* -

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *ta_LL* -

  - *ta_UL* -

- **es_smd**

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *numerator* -

  - *denominator* -

  - *SE* -

  - *df* -

  - *d_biased* -

- **overview**

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *grouping_variable_level* -

  - *mean* -

  - *mean_LL* -

  - *mean_UL* -

  - *median* -

  - *median_LL* -

  - *median_UL* -

  - *sd* -

  - *min* -

  - *max* -

  - *q1* -

  - *q3* -

  - *n* -

  - *missing* -

  - *df* -

  - *mean_SE* -

  - *median_SE* -

- **raw_data**

  - *grouping_variable* -

  - *outcome_variable* -

## Details

Reach for this function in place of a one-way ANOVA.

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.lc.mean.bs()`](https://dgbonett.github.io/statpsych/reference/ci.lc.mean.bs.html).

The estimated SMDs are from
[`CI_smd_ind_contrast()`](https://rcalinjageman.github.io/esci/reference/CI_smd_ind_contrast.md)
which relies on
[`statpsych::ci.lc.stdmean.bs()`](https://dgbonett.github.io/statpsych/reference/ci.lc.stdmean.bs.html)
unless there are only 2 groups.

The estimated median differences are from
[`statpsych::ci.lc.median.bs()`](https://dgbonett.github.io/statpsych/reference/ci.lc.median.bs.html)

## Examples

``` r
# From raw data
data("data_rattanmotivation")

estimate_from_raw <- esci::estimate_mdiff_ind_contrast(
  esci::data_rattanmotivation,
  Motivation,
  Group,
  contrast = c("Challenge" = 1, "Control" = -1/2, "Comfort" = -1/2)
)

# To visualize the estimate
myplot_from_raw <- esci::plot_mdiff(
  estimate_from_raw,
  effect_size = "median"
)
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_mdiff(
  estimate_from_raw,
  effect_size = "median"
)


# From summary data
data("data_halagappa")

estimate_from_summary <- estimate_mdiff_ind_contrast(
  means = data_halagappa$Mean,
  sds = data_halagappa$SD,
  ns = data_halagappa$n,
  grouping_variable_levels = as.character(data_halagappa$Groups),
  assume_equal_variance = TRUE,
  contrast = c(
    "NFree10" = 1/3,
    "AFree10" = 1/3,
    "ADiet10" = -1/3,
    "NFree17" = -1/3,
    "AFree17" = 1/3,
    "ADiet17" = -1/3
  ),
  grouping_variable_name = "Diet",
  outcome_variable_name = "% time near target"
)

# To visualize the estimate
myplot <- esci::plot_mdiff(estimate_from_summary, effect_size = "mean")
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: longer object length is not a multiple of shorter object length
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_mdiff(
  estimate_from_summary,
  effect_size = "mean"
)
```
