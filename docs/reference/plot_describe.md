# Plot a histogram or dotplot of an estimated magnitude with raw data

`plot_describe` Takes an estimate produced from
[estimate_magnitude](https://rcalinjageman.github.io/esci/reference/estimate_magnitude.md)
and produces a dotplot or histogram. It can mark various descriptive
statistics on the plot, including mean, median, sd, quartiles, and z
lines. If a percentile is passed, it color-codes data based on if it is
above or below that percentile.

## Usage

``` r
plot_describe(
  estimate,
  type = c("histogram", "dotplot"),
  mark_mean = FALSE,
  mark_median = FALSE,
  mark_sd = FALSE,
  mark_quartiles = FALSE,
  mark_z_lines = FALSE,
  mark_percentile = NULL,
  histogram_bins = 12,
  ylim = c(0, NA),
  ybreaks = NULL,
  xlim = c(NA, NA),
  xbreaks = NULL,
  fill_regular = "#008DF9",
  fill_highlighted = "#E20134",
  color = "black",
  marker_size = 5,
  ggtheme = NULL
)
```

## Arguments

- estimate:

  A esci_estimate object with raw data an es_mean

- type:

  histogram or dotplot

- mark_mean:

  should mean be marked?

- mark_median:

  should median be marked?

- mark_sd:

  should mean be marked?

- mark_quartiles:

  should mean be marked?

- mark_z_lines:

  should z lines be marked?

- mark_percentile:

  a percentile (0 to 1) to be marked

- histogram_bins:

  number of bins if a histogram

- ylim:

  2-length numeric vector

- ybreaks:

  numeric \>= 1

- xlim:

  2-length numeric vector

- xbreaks:

  numeric \>= 1

- fill_regular:

  color for

- fill_highlighted:

  color for

- color:

  outline color

- marker_size:

  Size of markers

- ggtheme:

  theme to apply, if any

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
# example code
# Generate an estimate on a single continuous variable
estimate <- esci::estimate_magnitude(esci::data_latimier_3groups, `Test%`)

# Now describe the result, with a histogram
myplot_hist <- plot_describe(estimate)

# Same, but as a dotplot and mark the mean
myplot_dots <- plot_describe(estimate, type = "dotplot", mark_mean = TRUE)
#> Warning: Ignoring unknown aesthetics: z


```
