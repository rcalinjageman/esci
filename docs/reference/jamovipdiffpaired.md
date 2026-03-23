# Proportions: Paired

Proportions: Paired

## Usage

``` r
jamovipdiffpaired(
  switch = "from_raw",
  data,
  reference_measure,
  comparison_measure,
  cases_consistent = " ",
  cases_inconsistent = " ",
  not_cases_consistent = " ",
  not_cases_inconsistent = " ",
  case_label = "Sick",
  not_case_label = "Well",
  comparison_measure_name = "Post-test",
  reference_measure_name = "Pre-test",
  conf_level = 95,
  show_details = FALSE,
  evaluate_hypotheses = FALSE,
  null_value = "0",
  null_boundary = "0",
  alpha = 0.05,
  null_color = "#A40122",
  es_plot_width = "400",
  es_plot_height = "450",
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
  error_layout = "none",
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
  linetype_summary_difference = "solid"
)
```

## Arguments

- switch:

  .

- data:

  .

- reference_measure:

  .

- comparison_measure:

  .

- cases_consistent:

  .

- cases_inconsistent:

  .

- not_cases_consistent:

  .

- not_cases_inconsistent:

  .

- case_label:

  .

- not_case_label:

  .

- comparison_measure_name:

  .

- reference_measure_name:

  .

- conf_level:

  .

- show_details:

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

## Value

A results object containing:

|                                    |     |     |     |     |                |
|------------------------------------|-----|-----|-----|-----|----------------|
| `results$debug`                    |     |     |     |     | a preformatted |
| `results$help`                     |     |     |     |     | a html         |
| `results$overview`                 |     |     |     |     | a table        |
| `results$es_proportion_difference` |     |     |     |     | a table        |
| `results$point_null`               |     |     |     |     | a table        |
| `results$interval_null`            |     |     |     |     | a table        |
| `results$estimation_plot_warnings` |     |     |     |     | a html         |
| `results$estimation_plots`         |     |     |     |     | an image       |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$overview$asDF`

`as.data.frame(results$overview)`
