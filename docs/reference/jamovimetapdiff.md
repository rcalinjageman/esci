# Meta-Analysis: Difference in Proportions

Meta-Analysis: Difference in Proportions

## Usage

``` r
jamovimetapdiff(
  data,
  reference_cases,
  reference_ns,
  comparison_cases,
  comparison_ns,
  labels,
  moderator,
  effect_label = "My effect",
  reported_effect_size = "RD",
  conf_level = 95,
  random_effects = "random_effects",
  method = "DL",
  include_PIs = FALSE,
  show_details = FALSE,
  es_plot_width = "600",
  es_plot_height = "750",
  size_base = "2",
  size_multiplier = "3",
  axis.text.y = "14",
  report_CIs = FALSE,
  meta_diamond_height = ".25",
  xlab = "auto",
  xmin = "auto",
  xmax = "auto",
  xbreaks = "auto",
  mark_zero = FALSE,
  axis.text.x = "14",
  axis.title.x = "15",
  dlab = "auto",
  dmin = "auto",
  dmax = "auto",
  dbreaks = "auto",
  shape_raw_reference = "square filled",
  shape_raw_comparison = "square filled",
  shape_summary_difference = "triangle filled",
  shape_raw_unused = "square filled",
  color_raw_reference = "#008DF9",
  color_raw_comparison = "#009F81",
  color_raw_unused = "gray65",
  color_summary_reference = "#008DF9",
  color_summary_comparison = "#009F81",
  color_summary_unused = "gray75",
  color_summary_difference = "black",
  color_summary_overall = "black",
  fill_raw_reference = "#008DF9",
  fill_raw_comparison = "#009F81",
  fill_raw_unused = "gray65",
  fill_summary_reference = "#008DF9",
  fill_summary_comparison = "#009F81",
  fill_summary_unused = "gray75",
  fill_summary_difference = "black",
  fill_summary_overall = "black",
  alpha_raw_reference = "1",
  alpha_raw_comparison = "1",
  alpha_raw_unused = "1",
  alpha_summary_reference = "1",
  alpha_summary_comparison = "1",
  alpha_summary_unused = "1",
  alpha_summary_difference = "1",
  alpha_summary_overall = "1",
  linetype_raw_reference = "solid",
  linetype_raw_comparison = "solid",
  linetype_summary_difference = "solid",
  linetype_raw_unused = "solid",
  color_interval_reference = "black",
  color_interval_comparison = "black",
  color_interval_difference = "black",
  color_interval_unused = "black",
  size_interval_reference = ".50",
  size_interval_comparison = ".50",
  size_interval_difference = ".50",
  size_interval_unused = ".50",
  alpha_interval_reference = "1",
  alpha_interval_comparison = "1",
  alpha_interval_difference = "1",
  alpha_interval_unused = "1"
)
```

## Arguments

- data:

  .

- reference_cases:

  .

- reference_ns:

  .

- comparison_cases:

  .

- comparison_ns:

  .

- labels:

  .

- moderator:

  .

- effect_label:

  .

- reported_effect_size:

  .

- conf_level:

  .

- random_effects:

  .

- method:

  .

- include_PIs:

  .

- show_details:

  .

- es_plot_width:

  .

- es_plot_height:

  .

- size_base:

  .

- size_multiplier:

  .

- axis.text.y:

  .

- report_CIs:

  .

- meta_diamond_height:

  .

- xlab:

  .

- xmin:

  .

- xmax:

  .

- xbreaks:

  .

- mark_zero:

  .

- axis.text.x:

  .

- axis.title.x:

  .

- dlab:

  .

- dmin:

  .

- dmax:

  .

- dbreaks:

  .

- shape_raw_reference:

  .

- shape_raw_comparison:

  .

- shape_summary_difference:

  .

- shape_raw_unused:

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

- color_summary_overall:

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

- fill_summary_overall:

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

- alpha_summary_overall:

  .

- linetype_raw_reference:

  .

- linetype_raw_comparison:

  .

- linetype_summary_difference:

  .

- linetype_raw_unused:

  .

- color_interval_reference:

  .

- color_interval_comparison:

  .

- color_interval_difference:

  .

- color_interval_unused:

  .

- size_interval_reference:

  .

- size_interval_comparison:

  .

- size_interval_difference:

  .

- size_interval_unused:

  .

- alpha_interval_reference:

  .

- alpha_interval_comparison:

  .

- alpha_interval_difference:

  .

- alpha_interval_unused:

  .

## Value

A results object containing:

|                                    |     |     |     |     |          |
|------------------------------------|-----|-----|-----|-----|----------|
| `results$debug`                    |     |     |     |     | a html   |
| `results$help`                     |     |     |     |     | a html   |
| `results$raw_data`                 |     |     |     |     | a table  |
| `results$es_meta`                  |     |     |     |     | a table  |
| `results$es_heterogeneity`         |     |     |     |     | a table  |
| `results$es_meta_difference`       |     |     |     |     | a table  |
| `results$estimation_plot_warnings` |     |     |     |     | a html   |
| `results$estimation_plots`         |     |     |     |     | an image |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$raw_data$asDF`

`as.data.frame(results$raw_data)`
