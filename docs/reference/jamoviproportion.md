# Proportions: Single Group

Proportions: Single Group

## Usage

``` r
jamoviproportion(
  switch = "from_raw",
  data,
  outcome_variable,
  cases = " ",
  not_cases = " ",
  case_label = "Affected",
  not_case_label = "Not Affected",
  outcome_variable_name = "My outcome variable",
  count_NA = FALSE,
  conf_level = 95,
  show_details = FALSE,
  plot_possible = FALSE,
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "300",
  es_plot_height = "400",
  ymin = "0",
  ymax = "1",
  breaks = "auto",
  ylab = "auto",
  xlab = "auto",
  axis.text.y = "14",
  axis.title.y = "15",
  axis.text.x = "14",
  axis.title.x = "15",
  error_layout = "none",
  shape_summary = "circle filled",
  color_summary = "#008DF9",
  fill_summary = "#008DF9",
  size_summary = "4",
  alpha_summary = "1",
  linetype_summary = "solid"
)
```

## Arguments

- switch:

  .

- data:

  .

- outcome_variable:

  .

- cases:

  .

- not_cases:

  .

- case_label:

  .

- not_case_label:

  .

- outcome_variable_name:

  .

- count_NA:

  .

- conf_level:

  .

- show_details:

  .

- plot_possible:

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

- shape_summary:

  .

- color_summary:

  .

- fill_summary:

  .

- size_summary:

  .

- alpha_summary:

  .

- linetype_summary:

  .

## Value

A results object containing:

|                                   |     |     |     |     |                    |
|-----------------------------------|-----|-----|-----|-----|--------------------|
| `results$debug`                   |     |     |     |     | a preformatted     |
| `results$help`                    |     |     |     |     | a html             |
| `results$overview`                |     |     |     |     | a table            |
| `results$point_null`              |     |     |     |     | a table            |
| `results$interval_null`           |     |     |     |     | a table            |
| `results$magnitude_plot_warnings` |     |     |     |     | a html             |
| `results$estimation_plots`        |     |     |     |     | an array of images |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
