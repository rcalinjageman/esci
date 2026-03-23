# Means and Medians: 2x2 Factorial

Means and Medians: 2x2 Factorial

## Usage

``` r
jamovimdiff2x2(
  design = "fully_between",
  switch = "from_raw",
  data,
  outcome_variable,
  grouping_variable_A,
  grouping_variable_B,
  outcome_variable_level1,
  outcome_variable_level2,
  outcome_variable_name_bs = "My outcome variable",
  grouping_variable,
  repeated_measures_name = "Time",
  outcome_variable_name = "My outcome variable",
  A1_label = "A1 level",
  A2_label = "A2 level",
  B1_label = "B1 level",
  B2_label = "B2 level",
  A_label = "Variable A",
  B_label = "Variable B",
  A1B1_mean = " ",
  A1B1_sd = " ",
  A1B1_n = " ",
  A1B2_mean = " ",
  A1B2_sd = " ",
  A1B2_n = " ",
  A2B1_mean = " ",
  A2B1_sd = " ",
  A2B1_n = " ",
  A2B2_mean = " ",
  A2B2_sd = " ",
  A2B2_n = " ",
  conf_level = 95,
  effect_size = "mean_difference",
  assume_equal_variance = TRUE,
  show_details = FALSE,
  show_interaction_plot = FALSE,
  show_CI = FALSE,
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  rope_units = "raw",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "700",
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
  simple_contrast_labels = FALSE,
  error_layout = "halfeye",
  error_scale = "0.25",
  error_nudge = "0.5",
  data_layout = "random",
  data_spread = "0.20",
  difference_axis_units = "raw",
  difference_axis_breaks = "auto",
  shape_raw_reference = "circle filled",
  shape_raw_comparison = "circle filled",
  shape_raw_unused = "circle filled",
  shape_summary_reference = "circle filled",
  shape_summary_comparison = "circle filled",
  shape_summary_unused = "circle filled",
  shape_summary_difference = "triangle filled",
  color_raw_reference = "#008DF9",
  color_raw_comparison = "#009F81",
  color_raw_unused = "gray65",
  color_summary_reference = "#008DF9",
  color_summary_comparison = "#009F81",
  color_summary_unused = "gray65",
  color_summary_difference = "black",
  fill_raw_reference = "NA",
  fill_raw_comparison = "NA",
  fill_raw_unused = "NA",
  fill_summary_reference = "#008DF9",
  fill_summary_comparison = "#009F81",
  fill_summary_unused = "gray65",
  fill_summary_difference = "black",
  size_raw_reference = "2",
  size_raw_comparison = "2",
  size_raw_unused = "1",
  size_summary_reference = "4",
  size_summary_comparison = "4",
  size_summary_unused = "4",
  size_summary_difference = "4",
  alpha_raw_reference = "1",
  alpha_raw_comparison = "1",
  alpha_raw_unused = "1",
  alpha_summary_reference = "1",
  alpha_summary_comparison = "1",
  alpha_summary_unused = "1",
  alpha_summary_difference = "1",
  linetype_summary_reference = "solid",
  linetype_summary_comparison = "solid",
  linetype_summary_unused = "solid",
  linetype_summary_difference = "solid",
  color_interval_reference = "black",
  color_interval_comparison = "black",
  color_interval_unused = "gray65",
  color_interval_difference = "black",
  size_interval_reference = "3",
  size_interval_comparison = "3",
  size_interval_unused = "3",
  size_interval_difference = "3",
  alpha_interval_reference = "1",
  alpha_interval_comparison = "1",
  alpha_interval_unused = "1",
  alpha_interval_difference = "1",
  alpha_error_reference = "1",
  alpha_error_comparison = "1",
  alpha_error_unused = "1",
  alpha_error_difference = "1",
  fill_error_reference = "gray75",
  fill_error_comparison = "gray75",
  fill_error_unused = "gray75",
  fill_error_difference = "gray75"
)
```

## Arguments

- design:

  .

- switch:

  .

- data:

  .

- outcome_variable:

  .

- grouping_variable_A:

  .

- grouping_variable_B:

  .

- outcome_variable_level1:

  .

- outcome_variable_level2:

  .

- outcome_variable_name_bs:

  .

- grouping_variable:

  .

- repeated_measures_name:

  .

- outcome_variable_name:

  .

- A1_label:

  .

- A2_label:

  .

- B1_label:

  .

- B2_label:

  .

- A_label:

  .

- B_label:

  .

- A1B1_mean:

  .

- A1B1_sd:

  .

- A1B1_n:

  .

- A1B2_mean:

  .

- A1B2_sd:

  .

- A1B2_n:

  .

- A2B1_mean:

  .

- A2B1_sd:

  .

- A2B1_n:

  .

- A2B2_mean:

  .

- A2B2_sd:

  .

- A2B2_n:

  .

- conf_level:

  .

- effect_size:

  .

- assume_equal_variance:

  .

- show_details:

  .

- show_interaction_plot:

  .

- show_CI:

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

- shape_raw_unused:

  .

- shape_summary_reference:

  .

- shape_summary_comparison:

  .

- shape_summary_unused:

  .

- shape_summary_difference:

  .

- color_raw_reference:

  .

- color_raw_comparison:

  .

- color_raw_unused:

  .

- color_summary_reference:

  .

- color_summary_comparison:

  .

- color_summary_unused:

  .

- color_summary_difference:

  .

- fill_raw_reference:

  .

- fill_raw_comparison:

  .

- fill_raw_unused:

  .

- fill_summary_reference:

  .

- fill_summary_comparison:

  .

- fill_summary_unused:

  .

- fill_summary_difference:

  .

- size_raw_reference:

  .

- size_raw_comparison:

  .

- size_raw_unused:

  .

- size_summary_reference:

  .

- size_summary_comparison:

  .

- size_summary_unused:

  .

- size_summary_difference:

  .

- alpha_raw_reference:

  .

- alpha_raw_comparison:

  .

- alpha_raw_unused:

  .

- alpha_summary_reference:

  .

- alpha_summary_comparison:

  .

- alpha_summary_unused:

  .

- alpha_summary_difference:

  .

- linetype_summary_reference:

  .

- linetype_summary_comparison:

  .

- linetype_summary_unused:

  .

- linetype_summary_difference:

  .

- color_interval_reference:

  .

- color_interval_comparison:

  .

- color_interval_unused:

  .

- color_interval_difference:

  .

- size_interval_reference:

  .

- size_interval_comparison:

  .

- size_interval_unused:

  .

- size_interval_difference:

  .

- alpha_interval_reference:

  .

- alpha_interval_comparison:

  .

- alpha_interval_unused:

  .

- alpha_interval_difference:

  .

- alpha_error_reference:

  .

- alpha_error_comparison:

  .

- alpha_error_unused:

  .

- alpha_error_difference:

  .

- fill_error_reference:

  .

- fill_error_comparison:

  .

- fill_error_unused:

  .

- fill_error_difference:

  .

## Value

A results object containing:

|                                    |     |     |     |     |                |
|------------------------------------|-----|-----|-----|-----|----------------|
| `results$analysis_type`            |     |     |     |     | a html         |
| `results$debug`                    |     |     |     |     | a preformatted |
| `results$help`                     |     |     |     |     | a html         |
| `results$overview`                 |     |     |     |     | a table        |
| `results$es_median_difference`     |     |     |     |     | a table        |
| `results$es_mean_difference`       |     |     |     |     | a table        |
| `results$es_smd`                   |     |     |     |     | a table        |
| `results$point_null`               |     |     |     |     | a table        |
| `results$interval_null`            |     |     |     |     | a table        |
| `results$estimation_plot_warnings` |     |     |     |     | a html         |
| `results$main_effect_A`            |     |     |     |     | an image       |
| `results$main_effect_B`            |     |     |     |     | an image       |
| `results$interaction`              |     |     |     |     | an image       |
| `results$interaction_plot`         |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
