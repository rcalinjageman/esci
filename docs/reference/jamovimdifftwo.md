# Means and Medians: Two Groups

Means and Medians: Two Groups

## Usage

``` r
jamovimdifftwo(
  switch = "from_raw",
  data,
  outcome_variable,
  grouping_variable,
  reference_level_name = "Reference group",
  reference_mean = " ",
  reference_sd = " ",
  reference_n = " ",
  comparison_level_name = "Comparison group",
  comparison_mean = " ",
  comparison_sd = " ",
  comparison_n = " ",
  outcome_variable_name = "Outcome variable",
  grouping_variable_name = "Grouping variable",
  conf_level = 95,
  assume_equal_variance = TRUE,
  effect_size = "mean_difference",
  show_ratio = FALSE,
  switch_comparison_order = FALSE,
  show_details = FALSE,
  show_calculations = FALSE,
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  rope_units = "raw",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "600",
  es_plot_height = "400",
  ymin = "auto",
  ymax = "auto",
  ybreaks = "auto",
  ylab = "auto",
  xlab = "auto",
  axis.text.y = "14",
  axis.title.y = "15",
  axis.text.x = "14",
  axis.title.x = "15",
  simple_contrast_labels = TRUE,
  error_layout = "halfeye",
  error_scale = "0.25",
  error_nudge = "0.5",
  data_layout = "random",
  data_spread = "0.20",
  difference_axis_units = "raw",
  difference_axis_breaks = "auto",
  shape_raw_reference = "circle filled",
  shape_raw_comparison = "circle filled",
  shape_summary_reference = "circle filled",
  shape_summary_comparison = "circle filled",
  shape_summary_difference = "triangle filled",
  color_raw_reference = "#008DF9",
  color_raw_comparison = "#009F81",
  color_summary_reference = "#008DF9",
  color_summary_comparison = "#009F81",
  color_summary_difference = "black",
  fill_raw_reference = "NA",
  fill_raw_comparison = "NA",
  fill_summary_reference = "#008DF9",
  fill_summary_comparison = "#009F81",
  fill_summary_difference = "black",
  size_raw_reference = "2",
  size_raw_comparison = "2",
  size_summary_reference = "4",
  size_summary_comparison = "4",
  size_summary_difference = "4",
  alpha_raw_reference = "1",
  alpha_raw_comparison = "1",
  alpha_summary_reference = "1",
  alpha_summary_comparison = "1",
  alpha_summary_difference = "1",
  linetype_summary_reference = "solid",
  linetype_summary_comparison = "solid",
  linetype_summary_difference = "solid",
  color_interval_reference = "black",
  color_interval_comparison = "black",
  color_interval_difference = "black",
  size_interval_reference = "3",
  size_interval_comparison = "3",
  size_interval_difference = "3",
  alpha_interval_reference = "1",
  alpha_interval_comparison = "1",
  alpha_interval_difference = "1",
  alpha_error_reference = "1",
  alpha_error_comparison = "1",
  alpha_error_difference = "1",
  fill_error_reference = "gray75",
  fill_error_comparison = "gray75",
  fill_error_difference = "gray75"
)
```

## Arguments

- switch:

  .

- data:

  .

- outcome_variable:

  .

- grouping_variable:

  .

- reference_level_name:

  .

- reference_mean:

  .

- reference_sd:

  .

- reference_n:

  .

- comparison_level_name:

  .

- comparison_mean:

  .

- comparison_sd:

  .

- comparison_n:

  .

- outcome_variable_name:

  .

- grouping_variable_name:

  .

- conf_level:

  .

- assume_equal_variance:

  .

- effect_size:

  .

- show_ratio:

  .

- switch_comparison_order:

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

- rope_units:

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

- simple_contrast_labels:

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

- difference_axis_units:

  .

- difference_axis_breaks:

  .

- shape_raw_reference:

  .

- shape_raw_comparison:

  .

- shape_summary_reference:

  .

- shape_summary_comparison:

  .

- shape_summary_difference:

  .

- color_raw_reference:

  .

- color_raw_comparison:

  .

- color_summary_reference:

  .

- color_summary_comparison:

  .

- color_summary_difference:

  .

- fill_raw_reference:

  .

- fill_raw_comparison:

  .

- fill_summary_reference:

  .

- fill_summary_comparison:

  .

- fill_summary_difference:

  .

- size_raw_reference:

  .

- size_raw_comparison:

  .

- size_summary_reference:

  .

- size_summary_comparison:

  .

- size_summary_difference:

  .

- alpha_raw_reference:

  .

- alpha_raw_comparison:

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

- color_interval_reference:

  .

- color_interval_comparison:

  .

- color_interval_difference:

  .

- size_interval_reference:

  .

- size_interval_comparison:

  .

- size_interval_difference:

  .

- alpha_interval_reference:

  .

- alpha_interval_comparison:

  .

- alpha_interval_difference:

  .

- alpha_error_reference:

  .

- alpha_error_comparison:

  .

- alpha_error_difference:

  .

- fill_error_reference:

  .

- fill_error_comparison:

  .

- fill_error_difference:

  .

## Value

A results object containing:

|                                    |     |     |     |     |                    |
|------------------------------------|-----|-----|-----|-----|--------------------|
| `results$debug`                    |     |     |     |     | a preformatted     |
| `results$help`                     |     |     |     |     | a html             |
| `results$overview`                 |     |     |     |     | a table            |
| `results$es_mean_difference`       |     |     |     |     | a table            |
| `results$es_smd`                   |     |     |     |     | a table            |
| `results$es_mean_ratio`            |     |     |     |     | a table            |
| `results$es_median_difference`     |     |     |     |     | a table            |
| `results$es_median_ratio`          |     |     |     |     | a table            |
| `results$point_null`               |     |     |     |     | a table            |
| `results$interval_null`            |     |     |     |     | a table            |
| `results$estimation_plot_warnings` |     |     |     |     | a html             |
| `results$estimation_plots`         |     |     |     |     | an array of images |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
