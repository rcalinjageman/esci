# Estimates for a categorical variable with no grouping (single-group design)

`estimate_proportion` is suitable for a single group design with a
categorical outcome variable. It estimates the population proportion for
the frequency of each level of the outcome variable, with confidence
intervals. You can pass raw data or summary data.

## Usage

``` r
estimate_proportion(
  data = NULL,
  outcome_variable = NULL,
  cases = NULL,
  case_label = 1,
  outcome_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  count_NA = FALSE
)
```

## Arguments

- data:

  For raw data - a data frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, which must be
  a factor, or a vector that is a factor

- cases:

  For summary data - A vector of cases

- case_label:

  A numeric or string indicating which level of the factor to estimate.
  Defaults to 1, meaning first level is analyzed

- outcome_variable_levels:

  For summary data - optional vector of 2 characters indicating name of
  the count level and name of the not count level. Defaults to
  "Affected" and "Not Affected"

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

- **es_proportion**

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

## Details

Once you generate an estimate with this function, you can visualize it
with
[`plot_proportion()`](https://rcalinjageman.github.io/esci/reference/plot_proportion.md).

If you want to compare your estimate to a known value or reference, then
use
[`estimate_pdiff_one()`](https://rcalinjageman.github.io/esci/reference/estimate_pdiff_one.md).

The estimated proportions are from
[`statpsych::ci.prop()`](https://dgbonett.github.io/statpsych/reference/ci.prop.html)
(named ci.prop1 in statpsych \< 1.6).

## Examples

``` r
# From raw data
data("data_campus_involvement")

estimate_from_raw <- esci::estimate_proportion(
  esci::data_campus_involvement,
  CommuterStatus
)


# To visualize the estimate
myplot_from_raw <- esci::plot_proportion(estimate_from_raw)


# From summary data
estimate_from_summary <- esci::estimate_proportion(
  cases = c(8, 22-8),
  outcome_variable_levels = c("Affected", "Not Affected")
)

# To visualize the estimate
myplot_from_summary<- esci::plot_proportion(estimate_from_summary)
```
