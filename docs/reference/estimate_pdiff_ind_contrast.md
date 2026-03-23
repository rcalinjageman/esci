# Estimates for a multi-group study with a categorical outcome variable

Returns object `estimate_pdiff_ind_contrast` is suitable for a
multi-group design (between subjects) with a categorical outcome
variable. It accepts a user-defined set of contrast weights that allows
estimation of any 1-df contrast. It can express estimates as a
difference in proportions and as an odds ratio (2-group designs only).
You can pass raw data or summary data.

## Usage

``` r
estimate_pdiff_ind_contrast(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  cases = NULL,
  ns = NULL,
  contrast = NULL,
  case_label = 1,
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

- cases:

  For summary data - A numeric vector of 2 or more event counts, each an
  integer \>= 0

- ns:

  For summary data - A numeric vector of sample sizes, same length as
  counts, each an integer \>= corresponding event count

- contrast:

  A vector of group weights, same length as number of groups.

- case_label:

  An optional numeric or character label For summary data, used as the
  label and defaults to 'Affected'. For raw data, used to specify the
  level used for the proportion.

- grouping_variable_levels:

  For summary data - An optional vector of group labels, same length as
  cases

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

The estimated proportion differences are from
[`statpsych::ci.lc.prop.bs()`](https://dgbonett.github.io/statpsych/reference/ci.lc.prop.bs.html).

The estimated odds ratios (if returned) are from
[`statpsych::ci.oddsratio()`](https://dgbonett.github.io/statpsych/reference/ci.oddsratio.html).

## Examples

``` r
# From raw data
data("data_campus_involvement")

estimate_from_raw <- esci::estimate_pdiff_ind_contrast(
  esci::data_campus_involvement,
  CommuterStatus,
  Gender,
  contrast = c("Male" = -1, "Female" = 1)
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
estimate_from_summary <- esci::estimate_pdiff_ind_contrast(
  cases = c(78, 10),
  ns = c(252, 20),
  case_label = "egocentric",
  grouping_variable_levels = c("Original", "Replication"),
  contrast = c(-1, 1),
  conf_level = 0.95
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
