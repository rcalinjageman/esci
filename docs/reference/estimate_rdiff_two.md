# Estimates the difference in correlation for a design with two groups and two continuous outcome variables

Returns object `estimate_rdiff_two` is suitable for a simple two-group
design with two continuous outcome variables where you want to estimate
the difference in the strength of the relationship between the two
groups. It estimate the linear correlation (Pearson's r) for each group
and the difference in r, along with confidence intervals. You can pass
raw data or summary data.

Returns effect sizes appropriate for estimating the linear relationship
between two quantitative variables

## Usage

``` r
estimate_rdiff_two(
  data = NULL,
  x = NULL,
  y = NULL,
  grouping_variable = NULL,
  comparison_r = NULL,
  comparison_n = NULL,
  reference_r = NULL,
  reference_n = NULL,
  grouping_variable_levels = NULL,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - a dataframe or tibble

- x:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- y:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- grouping_variable:

  For raw data, a vector that is a factor or the name of a factor column
  from data

- comparison_r:

  For summary data, a pearson's r correlation coefficient

- comparison_n:

  For summary data - An integer \> 0

- reference_r:

  For summary data, a pearson's r correlation coefficient

- reference_n:

  For summary data - An integer \> 0

- grouping_variable_levels:

  For summary data - An optional vector of 2 group labels

- x_variable_name:

  Optional friendly name for the x variable. Defaults to 'My x variable'
  or the outcome variable column name if a data frame is passed.

- y_variable_name:

  Optional friendly name for the y variable. Defaults to 'My y variable'
  or the outcome variable column name if a data frame is passed.

- grouping_variable_name:

  Optional friendly name for the grouping variable. Defaults to 'My
  grouping variable' or the grouping variable column name if a
  data.frame is passed.

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

- **es_r_difference**

  - *type* -

  - *grouping_variable_name* -

  - *grouping_variable_level* -

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

  - *rz* -

  - *sem* -

  - *z* -

  - *p* -

- **es_r**

  - *grouping_variable_name* -

  - *grouping_variable_level* -

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

- **raw_data**

  - *x* -

  - *y* -

  - *grouping_variable* -

## Details

Once you generate an estimate with this function, you can visualize it
with
[`plot_rdiff()`](https://rcalinjageman.github.io/esci/reference/plot_rdiff.md)
and you can test hypotheses with
[`test_rdiff()`](https://rcalinjageman.github.io/esci/reference/test_rdiff.md).
In addition, you can use
[`plot_scatter()`](https://rcalinjageman.github.io/esci/reference/plot_scatter.md)
to visualize the raw data.

The estimated single-group r values are from
[`statpsych::ci.cor()`](https://dgbonett.github.io/statpsych/reference/ci.cor.html).

The difference in r values is from
[`statpsych::ci.cor2()`](https://dgbonett.github.io/statpsych/reference/ci.cor2.html).

## Examples

``` r
# From raw data
data("data_campus_involvement")

estimate_from_raw <- esci::estimate_rdiff_two(
  esci::data_campus_involvement,
  GPA,
  SWB,
  Gender
)

# To visualize the difference in r
myplot_from_raw <- esci::plot_rdiff(estimate_from_raw)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To visualize the data (scatterplot) by group
myplot_scatter <- esci::plot_scatter(estimate_from_raw)

# To evaluate a hypothesis (by default: point null of exaclty 0):
res_htest_from_raw <- esci::test_rdiff(
  estimate_from_raw
)


# From summary data
estimate <- esci::estimate_rdiff_two(
  comparison_r = .53,
  comparison_n = 45,
  reference_r = .41,
  reference_n = 59,
  grouping_variable_levels = c("Females", "Males"),
  x_variable_name = "Satisfaction with life",
  y_variable_name = "Body satisfaction",
  grouping_variable_name = "Gender",
  conf_level = .95
)

myplot_from_summary <- esci::plot_rdiff(estimate)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To evaluate a hypothesis (interval null from -0.1 to 0.1):
res_htest_from_summary <- esci::test_rdiff(
  estimate,
  rope = c(-0.1, 0.1)
)

```
