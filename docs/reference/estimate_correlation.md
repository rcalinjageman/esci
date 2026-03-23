# Estimates the linear correlation (Pearson's r) between two continuous variables

`estimate_correlation` is suitable for a design with two continuous
variables. It estimates the linear correlation between two variables
(Pearson's r). You can pass raw data or summary data.

## Usage

``` r
estimate_correlation(
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

## Details

Once you generate an estimate with this function, you can visualize it
with
[`plot_correlation()`](https://rcalinjageman.github.io/esci/reference/plot_correlation.md)
and you can test hypotheses with
[`test_correlation()`](https://rcalinjageman.github.io/esci/reference/test_correlation.md).
In addition, you can use
[`plot_scatter()`](https://rcalinjageman.github.io/esci/reference/plot_scatter.md)
to visualize the raw data and to conduct a regression analysis that
returns predicted Y' values from a given X value.

The estimated correlation is from
[`statpsych::ci.cor()`](https://rdrr.io/pkg/statpsych/man/ci.cor.html),
which uses the Fisher r-to-z approach.
