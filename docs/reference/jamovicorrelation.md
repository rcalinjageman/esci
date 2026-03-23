# Correlations: Single Group

Correlations: Single Group

## Usage

``` r
jamovicorrelation(
  switch = "from_raw",
  data,
  x,
  y,
  r = " ",
  n = " ",
  x_variable_name = "X variable",
  y_variable_name = "Y variable",
  conf_level = 95,
  show_details = FALSE,
  do_regression = FALSE,
  show_line = FALSE,
  show_line_CI = FALSE,
  show_residuals = FALSE,
  show_PI = FALSE,
  show_mean_lines = FALSE,
  show_r = FALSE,
  plot_as_z = FALSE,
  predict_from_x = " ",
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "300",
  es_plot_height = "400",
  sp_plot_width = "650",
  sp_plot_height = "650",
  ymin = "-1",
  ymax = "1",
  ybreaks = "auto",
  ylab = "auto",
  xlab = "auto",
  axis.text.y = "14",
  axis.title.y = "15",
  axis.text.x = "14",
  axis.title.x = "15",
  error_layout = "none",
  sp_ymin = "auto",
  sp_ymax = "auto",
  sp_ybreaks = "auto",
  sp_xmin = "auto",
  sp_xmax = "auto",
  sp_xbreaks = "auto",
  sp_ylab = "auto",
  sp_xlab = "auto",
  sp_axis.text.y = "14",
  sp_axis.title.y = "15",
  sp_axis.text.x = "14",
  sp_axis.title.x = "15",
  shape_summary = "circle filled",
  color_summary = "#008DF9",
  fill_summary = "#008DF9",
  size_summary = "4",
  alpha_summary = "1",
  linetype_summary = "solid",
  color_interval = "black",
  size_interval = "3",
  alpha_interval = "1",
  alpha_error = "1",
  fill_error = "gray75",
  sp_shape_raw_reference = "circle filled",
  sp_color_raw_reference = "black",
  sp_fill_raw_reference = "#008DF9",
  sp_size_raw_reference = "3",
  sp_alpha_raw_reference = ".25",
  sp_linetype_summary_reference = "solid",
  sp_color_summary_reference = "#008DF9",
  sp_size_summary_reference = "3",
  sp_alpha_summary_reference = ".25",
  sp_linetype_PI_reference = "dotted",
  sp_color_PI_reference = "#E20134",
  sp_size_PI_reference = "2",
  sp_alpha_PI_reference = "1",
  sp_linetype_residual_reference = "solid",
  sp_color_residual_reference = "#E20134",
  sp_size_residual_reference = "1",
  sp_alpha_residual_reference = "1",
  sp_prediction_label = "5",
  sp_prediction_color = "#E20134",
  sp_linetype_CI = "solid",
  sp_color_CI = "#008DF9",
  sp_size_CI = "4",
  sp_alpha_CI = "1",
  sp_linetype_ref = "dotted",
  sp_color_ref = "gray60",
  sp_size_ref = "1",
  sp_alpha_ref = "1",
  sp_linetype_PI = "solid",
  sp_color_PI = "#E20134",
  sp_size_PI = "2",
  sp_alpha_PI = "1"
)
```

## Arguments

- switch:

  .

- data:

  .

- x:

  .

- y:

  .

- r:

  .

- n:

  .

- x_variable_name:

  .

- y_variable_name:

  .

- conf_level:

  .

- show_details:

  .

- do_regression:

  .

- show_line:

  .

- show_line_CI:

  .

- show_residuals:

  .

- show_PI:

  .

- show_mean_lines:

  .

- show_r:

  .

- plot_as_z:

  .

- predict_from_x:

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

- sp_plot_width:

  .

- sp_plot_height:

  .

- ymin:

  .

- ymax:

  .

- ybreaks:

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

- sp_ymin:

  .

- sp_ymax:

  .

- sp_ybreaks:

  .

- sp_xmin:

  .

- sp_xmax:

  .

- sp_xbreaks:

  .

- sp_ylab:

  .

- sp_xlab:

  .

- sp_axis.text.y:

  .

- sp_axis.title.y:

  .

- sp_axis.text.x:

  .

- sp_axis.title.x:

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

- sp_shape_raw_reference:

  .

- sp_color_raw_reference:

  .

- sp_fill_raw_reference:

  .

- sp_size_raw_reference:

  .

- sp_alpha_raw_reference:

  .

- sp_linetype_summary_reference:

  .

- sp_color_summary_reference:

  .

- sp_size_summary_reference:

  .

- sp_alpha_summary_reference:

  .

- sp_linetype_PI_reference:

  .

- sp_color_PI_reference:

  .

- sp_size_PI_reference:

  .

- sp_alpha_PI_reference:

  .

- sp_linetype_residual_reference:

  .

- sp_color_residual_reference:

  .

- sp_size_residual_reference:

  .

- sp_alpha_residual_reference:

  .

- sp_prediction_label:

  .

- sp_prediction_color:

  .

- sp_linetype_CI:

  .

- sp_color_CI:

  .

- sp_size_CI:

  .

- sp_alpha_CI:

  .

- sp_linetype_ref:

  .

- sp_color_ref:

  .

- sp_size_ref:

  .

- sp_alpha_ref:

  .

- sp_linetype_PI:

  .

- sp_color_PI:

  .

- sp_size_PI:

  .

- sp_alpha_PI:

  .

## Value

A results object containing:

|                                    |     |     |     |     |                |
|------------------------------------|-----|-----|-----|-----|----------------|
| `results$debug`                    |     |     |     |     | a preformatted |
| `results$help`                     |     |     |     |     | a html         |
| `results$overview`                 |     |     |     |     | a table        |
| `results$es_r`                     |     |     |     |     | a table        |
| `results$regression`               |     |     |     |     | a table        |
| `results$point_null`               |     |     |     |     | a table        |
| `results$interval_null`            |     |     |     |     | a table        |
| `results$scatter_plot_warnings`    |     |     |     |     | a html         |
| `results$scatter_plots`            |     |     |     |     | an image       |
| `results$estimation_plot_warnings` |     |     |     |     | a html         |
| `results$estimation_plots`         |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
