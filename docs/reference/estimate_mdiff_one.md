# Estimates for a single-group design with a continuous outcome variable compared to a reference or population value

Returns object `estimate_mdiff_one` is suitable for a single-group
design with a continuous outcome variable that is compared to a
reference or population value. It can express estimates as mean
differences, standardized mean differences (Cohen's d) or median
differences (raw data only). You can pass raw data or summary data.

## Usage

``` r
estimate_mdiff_one(
  data = NULL,
  outcome_variable = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  comparison_n = NULL,
  reference_mean = 0,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - a data frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- comparison_mean:

  For summary data, a numeric

- comparison_sd:

  For summary data, numeric \> 0

- comparison_n:

  For summary data, a numeric integer \> 0

- reference_mean:

  Reference value, defaults to 0

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  outcome variable' or the outcome variable column name if a data frame
  is passed.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

- save_raw_data:

  For raw data; defaults to TRUE; set to FALSE to save memory by not
  returning raw data in estimate object

## Value

Returns object of class esci_estimate

- **overview**

  - *outcome_variable_name* -

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

- **es_mean**

  - *outcome_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

- **es_median**

  - *outcome_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

- **raw_data**

  - *grouping_variable* -

  - *outcome_variable* -

- **es_mean_difference**

  - *outcome_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

  - *type* -

- **es_median_difference**

  - *outcome_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

  - *type* -

- **es_smd**

  - *outcome_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *numerator* -

  - *denominator* -

  - *SE* -

  - *df* -

  - *d_biased* -

## Details

Reach for this function in place of a *z*-test or one-sample *t*-test.

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.mean()`](https://dgbonett.github.io/statpsych/reference/ci.mean.html)
(named ci.mean1 in of statpsych \< 1.6).

The estimated SMDs are from
[`CI_smd_one()`](https://rcalinjageman.github.io/esci/reference/CI_smd_one.md).

The estimated median differences are from
[`statpsych::ci.median()`](https://dgbonett.github.io/statpsych/reference/ci.median.html)
(named ci.median1 in statpsych \< 1.6)

## Examples

``` r
# From raw data
data("data_penlaptop1")
estimate_from_raw <- esci::estimate_mdiff_one(
  data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
  outcome_variable = transcription,
  reference_mean = 10
)

# To visualize the mean difference estimate
myplot_from_raw <- esci::plot_mdiff(estimate_from_raw, effect_size = "mean")
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_mdiff(
  estimate_from_raw,
  effect_size = "mean",
  rope = c(-2, 2)
)


# From summary data
mymean <- 12.09
mysd <- 5.52
myn <- 103

estimate_from_summary <- esci::estimate_mdiff_one(
  comparison_mean = mymean,
  comparison_sd = mysd,
  comparison_n = myn,
  reference_mean = 12
)

# To visualize the estimate
myplot_from_sumary <- esci::plot_mdiff(
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
