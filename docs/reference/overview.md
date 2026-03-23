# Calculates descriptive statistics for a continuous variable

This function calculates basic descriptive statistics for a numerical
variable. It can calculate an overall summary, or broken down by the
levels of a grouping variable. Inputs can be summary data, vectors, or a
data frame.

## Usage

``` r
overview(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My Outcome Variable",
  grouping_variable_name = NULL,
  conf_level = 0.95,
  assume_equal_variance = FALSE
)
```

## Arguments

- data:

  - for raw data, a data frame or tibble

- outcome_variable:

  - for raw data, either a vector containing numerical data or the name
    of a data-frame column containing a factor

- grouping_variable:

  - optional; for raw data either a vector containing a factor or the
    name of a data frame column containing a factor

- means:

  For summary data - A vector of 1 or more numerical means

- sds:

  For summary data - A vector of standard deviations, same length as
  means

- ns:

  For summary data - A vector of sample sizes, same length as means

- grouping_variable_levels:

  For summary data - An optional vector of group labels, same length as
  means. If not passed, auto-generated.

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  Outcome Variable'. Ignored if a data-frame is passed, this argument is
  ignored.

- grouping_variable_name:

  Optional friendly name for the grouping variable. If a data frame is
  passed, this argument is ignored.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

- assume_equal_variance:

  Defaults to FALSE

## Value

Returns a table of descriptive statistics

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

## Details

If equal variance is not assumed, each group is treated independently.
In that case, the estimated mean, CI, and SE is from
[`statpsych::ci.mean()`](https://dgbonett.github.io/statpsych/reference/ci.mean.html),
and the estimated median, CI, and SE is from
[`statpsych::ci.median()`](https://dgbonett.github.io/statpsych/reference/ci.median.html).
If equal variance is assumed, each group CI is calculated as with
respect to all group data, using
[`statpsych::ci.lc.mean.bs()`](https://dgbonett.github.io/statpsych/reference/ci.lc.mean.bs.html)
and
[`statpsych::ci.lc.median.bs()`](https://dgbonett.github.io/statpsych/reference/ci.lc.median.bs.html)

## Examples

``` r
# example code
esci::overview(data_latimier_3groups, "Test%", "Group")
#>   outcome_variable_name grouping_variable_name grouping_variable_level     mean
#> 1                 Test%                  Group                  Reread 37.29323
#> 2                 Test%                  Group                    Quiz 42.75689
#> 3                 Test%                  Group                 Prequiz 37.14286
#>    mean_LL  mean_UL   median median_LL median_UL       sd      min      max
#> 1 34.17500 40.41146 33.33333  28.79785  37.86881 15.30716  9.52381 76.19048
#> 2 38.96548 46.54830 42.85714  36.05392  49.66036 18.61177 14.28571 95.23810
#> 3 33.88967 40.39604 33.33333  31.06559  35.60107 15.96965  9.52381 90.47619
#>         q1       q3  n missing df  mean_SE median_SE
#> 1 28.57143 47.61905 95       0 94 1.570482  2.314063
#> 2 28.57143 57.14286 95       0 94 1.909527  3.471095
#> 3 23.80952 42.85714 95       0 94 1.638451  1.157033

```
