# Plot the interaction from a 2x2 design

`plot_interaction` helps visualize the interaction from a 2x2 design. It
plots the 2 simple effects for the first factor and can also help
visualize the CIs on those simple effects. It is the comparison between
those simple effects that represents an interaction (the difference in
the difference). You can pass esci-estimate objects generated
[`estimate_mdiff_2x2_between()`](https://rcalinjageman.github.io/esci/reference/estimate_mdiff_2x2_between.md)
or
[`estimate_mdiff_2x2_mixed()`](https://rcalinjageman.github.io/esci/reference/estimate_mdiff_2x2_mixed.md).
This function returns a ggplot2 object.

## Usage

``` r
plot_interaction(
  estimate,
  effect_size = c("mean", "median"),
  show_CI = FALSE,
  ggtheme = NULL,
  line_count = 100,
  line_alpha = 0.02
)
```

## Arguments

- estimate:

  A esci_estimate object with raw data an es_mdiff_2x2\_ function

- effect_size:

  Optional; one of 'mean' or 'median' to determine the measure of
  central tendency plotted. Note that median is only available if the
  estimate was generated from raw data. Defaults to 'mean'

- show_CI:

  Optional logical; set to TRUE to visualize the confidence intervals on
  each simple effect; defaults to FALSE

- ggtheme:

  Optional ggplot2 theme object to specify the visual style of the plot.
  Defaults to
  [`ggplot2::theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

- line_count:

  Optional integer \> 0 to specify the number of lines used to visualize
  the simple-effect confidence intervals; defaults to 100

- line_alpha:

  Optional numeric between 0 and 1 to specify the alpha (transparency)
  of the confidence interval lines; defaults to 0.02

## Value

Returns a ggplot object

## Details

This function was developed primarily for student use within jamovi when
learning along with the text book Introduction to the New Statistics,
2nd edition (Cumming & Calin-Jageman, 2024).

Expect breaking changes as this function is improved for general use.
Work still do be done includes:

- Revise to avoid deprecated ggplot features

- Revise for consistent ability to control aesthetics and consistent
  layer names

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
