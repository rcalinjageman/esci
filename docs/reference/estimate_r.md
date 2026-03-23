# Estimates the linear correlation (Pearson's r) between two continuous variables

`estimate_r` is suitable for a design with two continuous variables. It
estimates the linear correlation between two variables (Pearson's r)
with a confidence interval. You can pass raw data or summary data.

## Usage

``` r
estimate_r(
  data = NULL,
  x = NULL,
  y = NULL,
  r = NULL,
  n = NULL,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  conf_level = 0.95,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - A data frame or tibble

- x:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- y:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- r:

  For summary data - A pearson's r correlation coefficient

- n:

  For summary data - Sample size, an integer \> 0

- x_variable_name:

  Optional friendly name for the x variable. Defaults to 'My x variable'
  or the outcome variable column name if a data frame is passed.

- y_variable_name:

  Optional friendly name for the y variable. Defaults to 'My y variable'
  or the outcome variable column name if a data frame is passed.

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

- **regression**

  - *component* -

  - *values* -

  - *LL* -

  - *UL* -

- **raw_data**

  - *x* -

  - *y* -

  - *fit* -

  - *lwr* -

  - *upr* -

## Details

Reach for this function to conduct simple linear correlation or simple
linear regression.

Once you generate an estimate with this function, you can visualize it
with
[`plot_correlation()`](https://rcalinjageman.github.io/esci/reference/plot_correlation.md)
and you can test hypotheses with
[`test_correlation()`](https://rcalinjageman.github.io/esci/reference/test_correlation.md).
In addition, you can use
[`plot_scatter()`](https://rcalinjageman.github.io/esci/reference/plot_scatter.md)
to visualize the raw data and to conduct a regression analysis that r
returns predicted Y' values from a given X value.

The estimated correlation is from
[`statpsych::ci.cor()`](https://dgbonett.github.io/statpsych/reference/ci.cor.html),
which uses the Fisher r-to-z approach.

## Examples

``` r
# From raw data
data("data_thomason_1")

estimate_from_raw <- esci::estimate_r(
  esci::data_thomason_1,
  Pretest,
  Posttest
)

# To visualize the value of r
myplot_correlation <- esci::plot_correlation(estimate_from_raw)

# To visualize the data (scatterplot) and use regression to obtain Y' from X
myplot_scatter_from_raw <- esci::plot_scatter(estimate_from_raw, predict_from_x = 10)
#> Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the esci package.
#>   Please report the issue at <https://github.com/rcalinjageman/esci/issues/>.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 12 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.

# To evaluate a hypothesis (interval null from -0.1 to 0.1):
res_htest_from_raw <- esci::test_correlation(
  estimate_from_raw,
  rope = c(-0.1, 0.1)
)


# From summary data
estimate_from_summary <- esci::estimate_r(r = 0.536, n = 50)

# To visualize the value of r
myplot_correlation_from_summary <- esci::plot_correlation(estimate_from_summary)

# To evaluate a hypothesis (interval null from -0.1 to 0.1):
res_htest_from_summary <- esci::test_correlation(
  estimate_from_summary,
  rope = c(-0.1, 0.1)
)

```
