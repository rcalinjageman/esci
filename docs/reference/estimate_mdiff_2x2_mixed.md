# Estimates for a 2x2 mixed factorial design with a continuous outcome variable

Returns object `estimate_mdiff_2x2_mixed` is suitable for a 2x2
mixed-factorial design with a continuous outcome variable. It estimates
each main effect, the simple effects for the repeated-measures factor,
and the interaction. It can express these estimates as mean differences,
median difference, or standardized mean differences. This function
accepts raw data only.

## Usage

``` r
estimate_mdiff_2x2_mixed(
  data,
  outcome_variable_level1,
  outcome_variable_level2,
  grouping_variable,
  outcome_variable_name = "My outcome variable",
  repeated_measures_name = "Time",
  conf_level = 0.95,
  save_raw_data = TRUE
)
```

## Arguments

- data:

  For raw data - a dataframe or tibble

- outcome_variable_level1:

  The column name of the outcome variable for level 1 of the
  repeated-measures factor

- outcome_variable_level2:

  The column name of the outcome variable for level 2 of the
  repeated-measures factor

- grouping_variable:

  The column name of the grouping variable; only 2 levels allowed; must
  be a factor

- outcome_variable_name:

  Optional friendly name for the outcome variable. Defaults to 'My
  outcome variable' or the outcome variable column name if a data frame
  is passed.

- repeated_measures_name:

  Optional friendly name for the repeated measures factor. Defaults to
  'Time'

- conf_level:

  The confidence level for the confidence interval. Given in decimal
  form. Defaults to 0.95.

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

  - *t* -

  - *p* -

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

  - *paired* -

## Details

Reach for this function in place of a 2x2 mixed-factorial ANOVA.

Once you generate an estimate with this function, you can visualize it
with
[`plot_mdiff()`](https://rcalinjageman.github.io/esci/reference/plot_mdiff.md)
and you can visualize the interaction specifically with
[`plot_interaction()`](https://rcalinjageman.github.io/esci/reference/plot_interaction.md).
You can test hypotheses with
[`test_mdiff()`](https://rcalinjageman.github.io/esci/reference/test_mdiff.md).

The estimated mean differences are from
[`statpsych::ci.2x2.mean.mixed()`](https://dgbonett.github.io/statpsych/reference/ci.2x2.mean.mixed.html).

## Examples

``` r
# From raw data (summary data mode not available for this function)
example_data <- data.frame(
  pretest = c(
    19, 18, 19, 20, 17, 16, 16, 10, 12,  9, 13, 15
  ),
  posttest = c(
    18, 19, 20, 17, 20, 16, 19, 16, 16, 14, 16, 18
  ),
  condition = as.factor(
    c(
      rep("Control", times = 6),
      rep("Treated", times = 6)
    )
  )
)

estimates <- esci::estimate_mdiff_2x2_mixed(
  data = example_data,
  outcome_variable_level1 = pretest,
  outcome_variable_level2 = posttest,
  grouping_variable = condition,
  repeated_measures_name = "Time"
)
# To visualize the estimated mean difference for the interaction
myplot <- esci::plot_mdiff(estimates$interaction, effect_size = "mean")
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Using alpha for a discrete variable is not advised.

# Line-plot of the interaction with fan effect representing each simple-effect CI
plot_interaction_line_CI <- esci::plot_interaction(
  estimates,
  show_CI = TRUE
)

# To conduct a hypothesis test
res_htest_from_raw <- esci::test_mdiff(
  estimates$interaction,
  effect_size = "mean"
 )
```
