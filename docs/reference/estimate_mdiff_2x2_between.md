# Estimates for a 2x2 between-subjects design with a continuous outcome variable

Returns object `estimate_mdiff_2x2_between` is suitable for a 2x2
between-subjects design with a continuous outcome variable. It estimates
each main effect, the simple effects for the first factor, and the
interaction. It can express these estimates as mean differences,
standardized mean differences (Cohen's d), and as median differences
(raw data only). You can pass raw data or or summary data (summary data
does not return medians).

## Usage

``` r
estimate_mdiff_2x2_between(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable_A = NULL,
  grouping_variable_B = NULL,
  means = NULL,
  sds = NULL,
  ns = NULL,
  grouping_variable_A_levels = NULL,
  grouping_variable_B_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_A_name = "A",
  grouping_variable_B_name = "A",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - a data frame or tibble

- outcome_variable:

  For raw data - The column name of the outcome variable, or a vector of
  numeric data

- grouping_variable_A:

  For raw data - The column name of the grouping variable, or a vector
  of group names, only 2 levels allowed

- grouping_variable_B:

  For raw data - The column name of the grouping variable, or a vector
  of group names, only 2 levels allowed

- means:

  For summary data - A vector of 4 means: A1B1, A1B2, A2B1, A2B2

- sds:

  For summary data - A vector of 4 standard deviations, same order

- ns:

  For summary data - A vector of 4 sample sizes

- grouping_variable_A_levels:

  For summary data - An optional vector of 2 group labels

- grouping_variable_B_levels:

  For summary data - An optional vector of 2 group labels

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  outcome variable' or the outcome variable column name if a data frame
  is passed.

- grouping_variable_A_name:

  Optional friendly name for the grouping variable. Defaults to 'A' or
  the grouping variable column name if a data.frame is passed.

- grouping_variable_B_name:

  Optional friendly name for the grouping variable. Defaults to 'A' or
  the grouping variable column name if a data.frame is passed.

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

- assume_equal_variance:

  Defaults to FALSE

- save_raw_data:

  For raw data; defaults to TRUE; set to FALSE to save memory by not
  returning raw data in estimate object

## Value

Returns object of class esci_estimate

- **es_mean_difference**

  - *type* -

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *df* -

  - *ta_LL* -

  - *ta_UL* -

  - *effect_type* -

  - *effects_complex* -

- **es_median_difference**

  - *type* -

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *SE* -

  - *ta_LL* -

  - *ta_UL* -

  - *effect_type* -

  - *effects_complex* -

- **es_smd**

  - *outcome_variable_name* -

  - *grouping_variable_name* -

  - *effect* -

  - *effect_size* -

  - *LL* -

  - *UL* -

  - *numerator* -

  - *denominator* -

  - *SE* -

  - *df* -

  - *d_biased* -

  - *effect_type* -

  - *effects_complex* -

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

- **raw_data**

  - *grouping_variable* -

  - *outcome_variable* -

  - *grouping_variable_A* -

  - *grouping_variable_B* -

## Details

Reach for this function in place of a 2x2 between-subjects ANOVA.

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can visualize the interaction specifically with
[`plot_interaction()`](https://rcalinjageman.github.io/esci/reference/plot_interaction.md).
You can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.2x2.mean.bs()`](https://dgbonett.github.io/statpsych/reference/ci.2x2.mean.bs.html).

The estimated SMDs are from
[`statpsych::ci.2x2.stdmean.bs()`](https://dgbonett.github.io/statpsych/reference/ci.2x2.stdmean.bs.html).

The estimated median differences are from
[`statpsych::ci.2x2.median.bs()`](https://dgbonett.github.io/statpsych/reference/ci.2x2.median.bs.html)

## Examples

``` r
data("data_videogameaggression")

estimates_from_raw <- esci::estimate_mdiff_2x2_between(
  esci::data_videogameaggression,
  Agression,
  Violence,
  Difficulty
)

# To visualize the estimated mean difference for the interaction
myplot_from_raw <- esci::plot_mdiff(
  estimates_from_raw$interaction,
  effect_size = "median"
)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the esci package.
#>   Please report the issue at <https://github.com/rcalinjageman/esci/issues/>.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To conduct a hypothesis test on the mean difference
res_htest_from_raw <- esci::test_mdiff(
  estimates_from_raw$interaction,
  effect_size = "median"
)


# From summary data
means <- c(1.5, 1.14, 1.38, 2.22)
sds <- c(1.38, .96,1.5, 1.68)
ns <- c(26, 26, 25, 26)
grouping_variable_A_levels <- c("Evening", "Morning")
grouping_variable_B_levels <- c("Sleep", "No Sleep")

estimates_from_summary <- esci::estimate_mdiff_2x2_between(
  means = means,
  sds = sds,
  ns = ns,
  grouping_variable_A_levels = grouping_variable_A_levels,
  grouping_variable_B_levels = grouping_variable_B_levels,
  grouping_variable_A_name = "Testing Time",
  grouping_variable_B_name = "Rest",
  outcome_variable_name = "False Memory Score",
  assume_equal_variance = TRUE
)

# To visualize the estimated mean difference for the interaction
plot_mdiff_interaction <- esci::plot_mdiff(
  estimates_from_summary$interaction,
  effect_size = "mean"
)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# To visualize the interaction as a line plot
plot_interaction_line <- esci::plot_interaction(estimates_from_summary)

# Same but with fan effect representing each simple-effect CI
plot_interaction_line_CI <- esci::plot_interaction(
  estimates_from_summary,
  show_CI = TRUE
)

# To conduct a hypothesis test on the mean difference
res_htest_from_raw <- esci::test_mdiff(
  estimates_from_summary$interaction,
  effect_size = "mean"
)

```
