# Estimates for a two-group study with a categorical outcome variable

Returns object `estimate_pdiff_two` is suitable for a simple two-group
design with a categorical outcome variable. It provides estimates and
CIs for the difference in proportions between the two groups, the odds
ratio, and phi. You can pass raw data or summary data.

## Usage

``` r
estimate_pdiff_two(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  comparison_cases = NULL,
  comparison_n = NULL,
  reference_cases = NULL,
  reference_n = NULL,
  case_label = 1,
  not_case_label = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  count_NA = FALSE
)
```

## Arguments

- data:

  For raw data - a data frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable which is a
  factor, or a vector that is a factor

- grouping_variable:

  For raw data - The column name of the grouping variable which is a
  factor, or a vector that is a factor

- comparison_cases:

  For summary data, a numeric integer \>= 0

- comparison_n:

  For summary data, a numeric integer \>= comparison_events

- reference_cases:

  For summary data, a numeric integer \>= 0

- reference_n:

  For summary data, a numeric integer \>= reference_events

- case_label:

  An optional numeric or character label for the case level.

- not_case_label:

  An optional numeric or character label for the not case level.

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

- count_NA:

  Logical to count NAs (TRUE) in total N or not (FALSE)

## Value

Returns object of class esci_estimate

- **es_proportion_difference**

  - *type* -

  - *outcome_variable_name* -

  - *case_label* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *effect_size_adjusted* -

  - *ta_LL* -

  - *ta_UL* -

- **es_odds_ratio**

  - *outcome_variable_name* -

  - *case_label* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *SE* -

  - *LL* -

  - *UL* -

  - *ta_LL* -

  - *ta_UL* -

- **overview**

  - *grouping_variable_name* -

  - *grouping_variable_level* -

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

- **es_phi**

  - *grouping_variable_name* -

  - *outcome_variable_name* -

  - *effect* -

  - *effect_size* -

  - *SE* -

  - *LL* -

  - *UL* -

## Details

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.prop2()`](https://dgbonett.github.io/statpsych/reference/ci.prop2.html).

The estimated odds ratio is from
[`statpsych::ci.oddsratio()`](https://dgbonett.github.io/statpsych/reference/ci.oddsratio.html).

The estimated correlation (phi) is from
[`statpsych::ci.phi()`](https://dgbonett.github.io/statpsych/reference/ci.phi.html).

## Examples

``` r
data("data_campus_involvement")

estimate_from_raw <- esci::estimate_pdiff_two(
  esci::data_campus_involvement,
  CommuterStatus,
  Gender
)

# To visualize the estimate
myplot_from_raw <- esci::plot_pdiff(estimate_from_raw)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_pdiff(estimate_from_raw)


# From summary_data
estimate_from_summary <- esci::estimate_pdiff_two(
  comparison_cases = 10,
  comparison_n = 20,
  reference_cases = 78,
  reference_n = 252,
  grouping_variable_levels = c("Original", "Replication"),
  conf_level = 0.95
)

# To visualize the estimate
myplot_from_summary <- esci::plot_pdiff(estimate_from_summary)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

#' # To conduct a hypothesis test
res_htest_from_summary <- esci::test_pdiff(estimate_from_summary)

```
