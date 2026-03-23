# Estimates for a continuous variable with no grouping (single-group design)

`estimate_magnitude` is suitable for a single group design with a
continuous outcome variable. It estimates the population mean and
population median (raw data only) with confidence intervals. You can
pass raw data or summary data.

## Usage

``` r
estimate_magnitude(
  data = NULL,
  outcome_variable = NULL,
  mean = NULL,
  sd = NULL,
  n = NULL,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - A data frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- mean:

  For summary data - A numeric representing the mean of the outcome
  variable

- sd:

  For summary data - A numeric \> 0, standard deviation of the outcome
  variable

- n:

  For summary data - An integer \> 0, sample size of the outcome
  variable

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

Returns an object of class esci_estimate

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

## Details

Reach for this function in place of a one-sample *t*-test or *z*-test.

Once you generate an estimate with this function, you can visualize it
with
[`plot_magnitude()`](https://rcalinjageman.github.io/esci/reference/plot_magnitude.md).

If you want to compare your sample to a known value or reference, then
use
[`estimate_mdiff_one()`](https://rcalinjageman.github.io/esci/reference/estimate_mdiff_one.md).

The estimated mean is from
[statpsych::ci.mean](https://dgbonett.github.io/statpsych/reference/ci.mean.html)
(named ci.mean1 in statpsych \< 1.6).

The estimated median is from
[`statpsych::ci.median()`](https://dgbonett.github.io/statpsych/reference/ci.median.html)
(named ci.median1 in of statpsych \< 1.6)

## Examples

``` r
# From raw data
data("data_penlaptop1")

estimate_from_raw <- esci::estimate_magnitude(
  data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
    outcome_variable = transcription
)

# To visualize the estimate
myplot_from_raw <- esci::plot_magnitude(
  estimate_from_raw,
  effect_size = "median"
)


# From summary data
mymean <- 24.5
mysd <- 3.65
myn <- 40

estimate_from_summary <- esci::estimate_magnitude(
  mean = mymean,
  sd = mysd,
  n = myn
)

# To visualize the estimate
myplot_from_summary <- esci::plot_magnitude(
  estimate_from_summary,
  effect_size = "mean"
)

```
