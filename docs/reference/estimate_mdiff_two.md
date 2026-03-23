# Estimates for a two-group study with a continuous outcome variable

Returns object `estimate_mdiff_two` is suitable for a simple two-group
design with a continuous outcome variable. It provides estimates and CIs
for the population mean difference between the repeated measures, the
standardized mean difference (SMD; Cohen's d) between the repeated
measures, and the median difference between the repeated measures (raw
data only). You can pass raw data or summary data.

## Usage

``` r
estimate_mdiff_two(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  comparison_n = NULL,
  reference_mean = NULL,
  reference_sd = NULL,
  reference_n = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE,
  switch_comparison_order = FALSE
)
```

## Arguments

- data:

  For raw data - a data.frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- grouping_variable:

  For raw data - The column name of the grouping variable, or a vector
  of group names

- comparison_mean:

  For summary data, a numeric

- comparison_sd:

  For summary data, numeric \> 0

- comparison_n:

  For summary data, a numeric integer \> 0

- reference_mean:

  For summary data, a numeric

- reference_sd:

  For summary data, numeric \> 0

- reference_n:

  For summary data, a numeric integer \> 0

- grouping_variable_levels:

  For summary data - An optional vector of 2 group labels

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

- switch_comparison_order:

  Defaults to FALSE

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

- **es_mean_ratio**

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *comparison_mean* -

  - *reference_mean* -

- **es_median_ratio**

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *comparison_median* -

  - *reference_median* -

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

Reach for this function in place of an independent-samples *t*-test.

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.mean2()`](https://dgbonett.github.io/statpsych/reference/ci.mean2.html).

The estimated SMDs are from
[`CI_smd_ind_contrast()`](https://rcalinjageman.github.io/esci/reference/CI_smd_ind_contrast.md).

The estimated median differences are from
[`statpsych::ci.median2()`](https://dgbonett.github.io/statpsych/reference/ci.median2.html).

## Examples

``` r
# From raw data
data("data_penlaptop1")

estimate_from_raw <- esci::estimate_mdiff_two(
  data = data_penlaptop1,
  outcome_variable = transcription,
  grouping_variable = condition,
  switch_comparison_order = TRUE,
  assume_equal_variance = TRUE
)

# To visualize the estimated median difference (raw data only)
myplot_from_raw <- esci::plot_mdiff(
  estimate_from_raw,
  effect_size = "median"
)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_mdiff(
  estimate_from_raw,
  effect_size = "median",
  rope = c(-2, 2)
)


# From summary data
estimate_from_summary <- esci::estimate_mdiff_two(
  comparison_mean = 12.09,
  comparison_sd = 5.52,
  comparison_n = 103,
  reference_mean = 6.88,
  reference_sd = 4.22,
  reference_n = 48,
  grouping_variable_levels = c("Ref-Laptop", "Comp-Pen"),
  outcome_variable_name = "% Transcription",
  grouping_variable_name = "Note-taking type",
  assume_equal_variance = TRUE
)

# To visualize the estimated mean difference
myplot <- esci::plot_mdiff(
  estimate_from_summary,
  effect_size = "mean"
)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_summary <- esci::test_mdiff(
  estimate_from_summary,
  effect_size = "mean",
  rope = c(-2, 2)
)
```
