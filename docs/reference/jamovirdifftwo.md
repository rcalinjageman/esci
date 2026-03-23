# Correlations: Two Groups

Correlations: Two Groups

## Usage

``` r
jamovirdifftwo(
  switch = "from_raw",
  data,
  x,
  y,
  grouping_variable,
  comparison_r = " ",
  comparison_n = " ",
  reference_r = " ",
  reference_n = " ",
  x_variable_name = "X variable",
  y_variable_name = "Y variable",
  comparison_level_name = "Comparison level",
  reference_level_name = "Reference level",
  grouping_variable_name = "Grouping variable",
  conf_level = 95,
  show_details = FALSE,
  show_line = TRUE,
  show_line_CI = TRUE,
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "600",
  es_plot_height = "400",
  sp_plot_width = "650",
  sp_plot_height = "650",
  ymin = "auto",
  ymax = "auto",
  ybreaks = "auto",
  ylab = "auto",
  xlab = "auto",
  axis.text.y = "14",
  axis.title.y = "15",
  axis.text.x = "14",
  axis.title.x = "15",
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
  difference_axis_breaks = "auto",
  shape_summary_reference = "circle filled",
  shape_summary_comparison = "circle filled",
  shape_summary_difference = "triangle filled",
  color_summary_reference = "#008DF9",
  color_summary_comparison = "#009F81",
  color_summary_difference = "black",
  fill_summary_reference = "#008DF9",
  fill_summary_comparison = "#009F81",
  fill_summary_difference = "black",
  size_summary_reference = "4",
  size_summary_comparison = "4",
  size_summary_difference = "4",
  alpha_summary_reference = "1",
  alpha_summary_comparison = "1",
  alpha_summary_difference = "1",
  linetype_summary_reference = "solid",
  linetype_summary_comparison = "solid",
  linetype_summary_difference = "solid",
  sp_shape_raw_reference = "circle filled",
  sp_shape_raw_comparison = "circle filled",
  sp_shape_raw_unused = "circle filled",
  sp_color_raw_reference = "black",
  sp_color_raw_comparison = "black",
  sp_color_raw_unused = "black",
  sp_fill_raw_reference = "#008DF9",
  sp_fill_raw_comparison = "#009F81",
  sp_fill_raw_unused = "NA",
  sp_size_raw_reference = "3",
  sp_size_raw_comparison = "3",
  sp_size_raw_unused = "2",
  sp_alpha_raw_reference = ".25",
  sp_alpha_raw_comparison = ".25",
  sp_alpha_raw_unused = ".25",
  sp_linetype_summary_reference = "solid",
  sp_linetype_summary_comparison = "solid",
  sp_color_summary_reference = "#008DF9",
  sp_color_summary_comparison = "#009F81",
  sp_size_summary_reference = "2",
  sp_size_summary_comparison = "2",
  sp_alpha_summary_reference = ".25",
  sp_alpha_summary_comparison = ".25"
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

- grouping_variable:

  .

- comparison_r:

  .

- comparison_n:

  .

- reference_r:

  .

- reference_n:

  .

- x_variable_name:

  .

- y_variable_name:

  .

- comparison_level_name:

  .

- reference_level_name:

  .

- grouping_variable_name:

  .

- conf_level:

  .

- show_details:

  .

- show_line:

  .

- show_line_CI:

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

- difference_axis_breaks:

  .

- shape_summary_reference:

  .

- shape_summary_comparison:

  .

- shape_summary_difference:

  .

- color_summary_reference:

  .

- color_summary_comparison:

  .

- color_summary_difference:

  .

- fill_summary_reference:

  .

- fill_summary_comparison:

  .

- fill_summary_difference:

  .

- size_summary_reference:

  .

- size_summary_comparison:

  .

- size_summary_difference:

  .

- alpha_summary_reference:

  .

- alpha_summary_comparison:

  .

- alpha_summary_difference:

  .

- linetype_summary_reference:

  .

- linetype_summary_comparison:

  .

- linetype_summary_difference:

  .

- sp_shape_raw_reference:

  .

- sp_shape_raw_comparison:

  .

- sp_shape_raw_unused:

  .

- sp_color_raw_reference:

  .

- sp_color_raw_comparison:

  .

- sp_color_raw_unused:

  .

- sp_fill_raw_reference:

  .

- sp_fill_raw_comparison:

  .

- sp_fill_raw_unused:

  .

- sp_size_raw_reference:

  .

- sp_size_raw_comparison:

  .

- sp_size_raw_unused:

  .

- sp_alpha_raw_reference:

  .

- sp_alpha_raw_comparison:

  .

- sp_alpha_raw_unused:

  .

- sp_linetype_summary_reference:

  .

- sp_linetype_summary_comparison:

  .

- sp_color_summary_reference:

  .

- sp_color_summary_comparison:

  .

- sp_size_summary_reference:

  .

- sp_size_summary_comparison:

  .

- sp_alpha_summary_reference:

  .

- sp_alpha_summary_comparison:

  .

## Value

A results object containing:

|                                    |     |     |     |     |                |
|------------------------------------|-----|-----|-----|-----|----------------|
| `results$debug`                    |     |     |     |     | a preformatted |
| `results$help`                     |     |     |     |     | a html         |
| `results$overview`                 |     |     |     |     | a table        |
| `results$es_r`                     |     |     |     |     | a table        |
| `results$es_r_difference`          |     |     |     |     | a table        |
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
