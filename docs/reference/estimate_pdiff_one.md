# Estimates for a single-group design with a categorical outcome variable compared to a reference or population value.

Returns object `estimate_pdiff_one` is suitable for a single-group
design (between subjects) with a categorical outcome variable. It
calculates the effect sizes with respect to a reference or population
proportion (default value of 0). It returns the estimated difference
between the in proportion from the reference/population value. You can
pass raw data or summary data.

## Usage

``` r
estimate_pdiff_one(
  data = NULL,
  outcome_variable = NULL,
  comparison_cases = NULL,
  comparison_n = NULL,
  reference_p = 0,
  case_label = 1,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  count_NA = FALSE
)
```

## Arguments

- data:

  For raw data - a dataframe or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, which must be
  a factor, or a vector that is a factor

- comparison_cases:

  For summary data, a numeric integer \> 0

- comparison_n:

  For summary data, a numeric integer \>= count

- reference_p:

  Reference proportion, numeric \>=0 and \<=1

- case_label:

  An optional numeric or character label for the count level.

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  outcome variable' or the outcome variable column name if a data frame
  is passed.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

- count_NA:

  Logical to count NAs (TRUE) in total N or not (FALSE)

## Value

Returns an object of class esci_estimate

- **overview**

  - *outcome_variable_name* -

  - *outcome_variable_level* -

  - *cases* -

  - *n* -

  - *P* -

  - *P_LL* -

  - *P_UL* -

  - *P_SE* -

  - *P_adjusted* -

  - *ta_LL* -

  - *ta_UL* -

- **es_proportion_difference**

  - *outcome_variable_name* -

  - *case_label* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *effect_size_adjusted* -

  - *ta_LL* -

  - *ta_UL* -

  - *cases* -

  - *n* -

  - *type* -

## Details

Once you generate an estimate with this function, you can visualize it
with
[`plot_pdiff()`](https://rcalinjageman.github.io/esci/reference/plot_pdiff.md)
and you can test hypotheses with
[`test_pdiff()`](https://rcalinjageman.github.io/esci/reference/test_pdiff.md).

The estimated proportion differences are from
[`statpsych::ci.prop()`](https://dgbonett.github.io/statpsych/reference/ci.prop.html)
(named ci.prop in statpsych \< 1.6).

## Examples

``` r
# From raw data
data("data_campus_involvement")

estimate_from_raw <- esci::estimate_pdiff_one(
  esci::data_campus_involvement,
  CommuterStatus,
  reference_p = 0.50
)

# To visualize the estimate
myplot_from_raw <- esci::plot_pdiff(estimate_from_raw)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_pdiff(estimate_from_raw)


# From summary data
estimate_from_summary  <- esci::estimate_pdiff_one(
  comparison_cases = 8,
  comparison_n = 22,
  reference_p = 0.5
)

# To visualize the estimate
myplot_from_summary <- esci::plot_pdiff(estimate_from_summary)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_summary <- esci::test_pdiff(estimate_from_summary)

```
