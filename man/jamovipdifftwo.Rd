% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamovipdifftwo.h.R
\name{jamovipdifftwo}
\alias{jamovipdifftwo}
\title{Proportions: Two Groups}
\usage{
jamovipdifftwo(
  switch = "from_raw",
  data,
  outcome_variable,
  grouping_variable,
  comparison_cases = " ",
  comparison_not_cases = " ",
  reference_cases = " ",
  reference_not_cases = " ",
  case_label = "Sick",
  not_case_label = "Well",
  grouping_variable_level1 = "Treated",
  grouping_variable_level2 = "Control",
  outcome_variable_name = "Outcome variable",
  grouping_variable_name = "Grouping variable",
  count_NA = FALSE,
  show_ratio = FALSE,
  show_chi_square = FALSE,
  chi_table_option = "both",
  show_phi = FALSE,
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
}
\arguments{
\item{switch}{.}

\item{data}{.}

\item{outcome_variable}{.}

\item{grouping_variable}{.}

\item{comparison_cases}{.}

\item{comparison_not_cases}{.}

\item{reference_cases}{.}

\item{reference_not_cases}{.}

\item{case_label}{.}

\item{not_case_label}{.}

\item{grouping_variable_level1}{.}

\item{grouping_variable_level2}{.}

\item{outcome_variable_name}{.}

\item{grouping_variable_name}{.}

\item{count_NA}{.}

\item{show_ratio}{.}

\item{show_chi_square}{.}

\item{chi_table_option}{.}

\item{show_phi}{.}

\item{conf_level}{.}

\item{show_details}{.}

\item{evaluate_hypotheses}{.}

\item{null_value}{.}

\item{null_boundary}{.}

\item{alpha}{.}

\item{null_color}{.}

\item{es_plot_width}{.}

\item{es_plot_height}{.}

\item{ymin}{.}

\item{ymax}{.}

\item{ybreaks}{.}

\item{ylab}{.}

\item{xlab}{.}

\item{axis.text.y}{.}

\item{axis.title.y}{.}

\item{axis.text.x}{.}

\item{axis.title.x}{.}

\item{simple_contrast_labels}{.}

\item{error_layout}{.}

\item{difference_axis_breaks}{.}

\item{shape_summary_reference}{.}

\item{shape_summary_comparison}{.}

\item{shape_summary_difference}{.}

\item{color_summary_reference}{.}

\item{color_summary_comparison}{.}

\item{color_summary_difference}{.}

\item{fill_summary_reference}{.}

\item{fill_summary_comparison}{.}

\item{fill_summary_difference}{.}

\item{size_summary_reference}{.}

\item{size_summary_comparison}{.}

\item{size_summary_difference}{.}

\item{alpha_summary_reference}{.}

\item{alpha_summary_comparison}{.}

\item{alpha_summary_difference}{.}

\item{linetype_summary_reference}{.}

\item{linetype_summary_comparison}{.}

\item{linetype_summary_difference}{.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$debug} \tab \tab \tab \tab \tab a preformatted \cr
\code{results$help} \tab \tab \tab \tab \tab a html \cr
\code{results$overview} \tab \tab \tab \tab \tab a table \cr
\code{results$es_proportion_difference} \tab \tab \tab \tab \tab a table \cr
\code{results$es_odds_ratio} \tab \tab \tab \tab \tab a table \cr
\code{results$es_phi} \tab \tab \tab \tab \tab a table \cr
\code{results$contingency_table} \tab \tab \tab \tab \tab a table \cr
\code{results$point_null} \tab \tab \tab \tab \tab a table \cr
\code{results$interval_null} \tab \tab \tab \tab \tab a table \cr
\code{results$estimation_plot_warnings} \tab \tab \tab \tab \tab a html \cr
\code{results$estimation_plots} \tab \tab \tab \tab \tab an array of images \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$overview$asDF}

\code{as.data.frame(results$overview)}
}
\description{
Proportions: Two Groups
}
