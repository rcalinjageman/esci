# Means and Medians: Single Group

Means and Medians: Single Group

## Usage

``` r
jamovimagnitude(
  switch = "from_raw",
  data,
  outcome_variable,
  mean = "",
  sd = "",
  n = "",
  outcome_variable_name = "Outcome variable",
  conf_level = 95,
  effect_size = "mean",
  show_details = FALSE,
  show_calculations = FALSE,
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "300",
  es_plot_height = "400",
  ymin = "auto",
  ymax = "auto",
  breaks = "auto",
  ylab = "auto",
  xlab = "auto",
  axis.text.y = "14",
  axis.title.y = "15",
  axis.text.x = "14",
  axis.title.x = "15",
  error_layout = "halfeye",
  error_scale = "0.20",
  error_nudge = "0.3",
  data_layout = "random",
  data_spread = "0.25",
  shape_raw = "circle filled",
  shape_summary = "circle filled",
  color_raw = "#008DF9",
  color_summary = "#008DF9",
  fill_raw = "NA",
  fill_summary = "#008DF9",
  size_raw = "2",
  size_summary = "4",
  alpha_raw = "1",
  alpha_summary = "1",
  linetype_summary = "solid",
  color_interval = "black",
  size_interval = "3",
  alpha_interval = "1",
  alpha_error = "1",
  fill_error = "gray75"
)
```

## Arguments

- switch:

  .

- data:

  .

- outcome_variable:

  .

- mean:

  .

- sd:

  .

- n:

  .

- outcome_variable_name:

  .

- conf_level:

  .

- effect_size:

  .

- show_details:

  .

- show_calculations:

  .

- evaluate_hypotheses:

  .

- null_value:

  .

- null_boundary:

  .

- alpha:

  .

- null_color:

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

- error_layout:

  .

- error_scale:

  .

- error_nudge:

  .

- data_layout:

  .

- data_spread:

  .

- shape_raw:

  .

- shape_summary:

  .

- color_raw:

  .

- color_summary:

  .

- fill_raw:

  .

- fill_summary:

  .

- size_raw:

  .

- size_summary:

  .

- alpha_raw:

  .

- alpha_summary:

  .

- linetype_summary:

  .

- color_interval:

  .

- size_interval:

  .

- alpha_interval:

  .

- alpha_error:

  .

- fill_error:

  .

## Value

A results object containing:

|                                   |     |     |     |     |                |
|-----------------------------------|-----|-----|-----|-----|----------------|
| `results$debug`                   |     |     |     |     | a preformatted |
| `results$help`                    |     |     |     |     | a html         |
| `results$overview`                |     |     |     |     | a table        |
| `results$es_smd`                  |     |     |     |     | a table        |
| `results$point_null`              |     |     |     |     | a table        |
| `results$interval_null`           |     |     |     |     | a table        |
| `results$magnitude_plot_warnings` |     |     |     |     | a html         |
| `results$magnitude_plot`          |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
