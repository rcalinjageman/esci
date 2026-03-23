# Describe

Describe

## Usage

``` r
jamovidescribe(
  data,
  outcome_variable,
  show_details = FALSE,
  mark_mean = FALSE,
  mark_median = FALSE,
  mark_sd = FALSE,
  mark_quartiles = FALSE,
  mark_z_lines = FALSE,
  mark_percentile = "0",
  histogram_bins = "12",
  es_plot_width = "500",
  es_plot_height = "400",
  ymin = "auto",
  ymax = "auto",
  breaks = "auto",
  xmin = "auto",
  xmax = "auto",
  xbreaks = "auto",
  ylab = "auto",
  xlab = "auto",
  axis.text.y = "14",
  axis.title.y = "15",
  axis.text.x = "14",
  axis.title.x = "15",
  fill_regular = "#008DF9",
  fill_highlighted = "#E20134",
  color = "black"
)
```

## Arguments

- data:

  .

- outcome_variable:

  .

- show_details:

  .

- mark_mean:

  .

- mark_median:

  .

- mark_sd:

  .

- mark_quartiles:

  .

- mark_z_lines:

  .

- mark_percentile:

  .

- histogram_bins:

  .

- es_plot_width:

  .

- es_plot_height:

  .

- ymin:

  .

- ymax:

  .

- breaks:

  .

- xmin:

  .

- xmax:

  .

- xbreaks:

  .

- ylab:

  .

- xlab:

  .

- axis.text.y:

  .

- axis.title.y:

  .

- axis.text.x:

  .

- axis.title.x:

  .

- fill_regular:

  .

- fill_highlighted:

  .

- color:

  .

## Value

A results object containing:

|                                  |     |     |     |     |                |
|----------------------------------|-----|-----|-----|-----|----------------|
| `results$debug`                  |     |     |     |     | a preformatted |
| `results$help`                   |     |     |     |     | a html         |
| `results$overview`               |     |     |     |     | a table        |
| `results$describe_plot_warnings` |     |     |     |     | a html         |
| `results$describe_plot`          |     |     |     |     | an image       |
| `results$describe_dotplot`       |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
