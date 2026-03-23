# Estimates for a repeated-measures study with two measures of a continuous variable

Returns object `estimate_mdiff_paired` is suitable for a simple paired
design with a continuous outcome variable. It provides estimates and CIs
for the population mean difference between the repeated measures, the
standardized mean difference (SMD; Cohen's d) between the repeated
measures, and the median difference between the repeated measures (raw
data only). You can pass raw data or summary data.

## Usage

``` r
estimate_mdiff_paired(
  data = NULL,
  comparison_measure = NULL,
  reference_measure = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  reference_mean = NULL,
  reference_sd = NULL,
  n = NULL,
  correlation = NULL,
  comparison_measure_name = "Comparison measure",
  reference_measure_name = "Reference measure",
  conf_level = 0.95,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - a data frame or tibble

- comparison_measure:

  For raw data - The column name of comparison measure of the outcome
  variable, or a vector of numeric data

- reference_measure:

  For raw data - The column name of the reference measure of the outcome
  variable, or a vector of numeric data

- comparison_mean:

  For summary data, a numeric

- comparison_sd:

  For summary data, numeric \> 0

- reference_mean:

  For summary data, a numeric

- reference_sd:

  For summary data, numeric \> 0

- n:

  For summary data, a numeric integer \> 0

- correlation:

  For summary data, correlation between measures, a numeric that is \>
  -1 and \< 1

- comparison_measure_name:

  For summary data - An optional character label for the comparison
  measure. Defaults to 'Comparison measure'

- reference_measure_name:

  For summary data - An optional character label for the reference
  measure. Defaults to 'Reference measure'

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

- **es_mean_difference**

  - *type* -

  - *comparison_measure_name* -

  - *reference_measure_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

- **es_smd**

  - *comparison_measure_name* -

  - *reference_measure_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *numerator* -

  - *denominator* -

  - *SE* -

  - *d_biased* -

  - *df* -

- **es_r**

  - *x_variable_name* -

  - *y_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *n* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

- **es_median_difference**

  - *type* -

  - *comparison_measure_name* -

  - *reference_measure_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *ta_LL* -

  - *ta_UL* -

- **es_mean_ratio**

  - *comparison_measure_name* -

  - *reference_measure_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *comparison_mean* -

  - *reference_mean* -

- **es_median_ratio**

  - *comparison_measure_name* -

  - *reference_measure_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *comparison_median* -

  - *reference_median* -

- **raw_data**

  - *comparison_measure* -

  - *reference_measure* -

## Details

Reach for this function in place of a paired-samples *t*-test.

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.mean.ps()`](https://dgbonett.github.io/statpsych/reference/ci.mean.ps.html).

The estimated SMDs are from
[`CI_smd_ind_contrast()`](https://rcalinjageman.github.io/esci/reference/CI_smd_ind_contrast.md).

The estimated median differences are from
[`statpsych::ci.median.ps()`](https://dgbonett.github.io/statpsych/reference/ci.median.ps.html).

## Examples

``` r
# From raw data
data("data_thomason_1")

estimate_from_raw <- esci::estimate_mdiff_paired(
  data = esci::data_thomason_1,
  comparison_measure = Posttest,
  reference_measure = Pretest
)

# To visualize the estimated median difference (raw data only)
myplot_from_raw <- esci::plot_mdiff(estimate_from_raw, effect_size = "median")
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
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


sd1 <- 4.28
sd2 <- 3.4
sdiff <- 2.13

cor <- (sd1^2 + sd2^2 - sdiff^2) / (2*sd1*sd2)

estimate_from_summary <- esci::estimate_mdiff_paired(
  comparison_mean = 14.25,
  comparison_sd = 4.28,
  reference_mean = 12.88,
  reference_sd = 3.4,
  n = 16,
  correlation = 0.87072223749,
  comparison_measure_name = "After",
  reference_measure_name = "Before"
)

# To visualize the estimated mean difference
myplot_from_summary <- esci::plot_mdiff(
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
