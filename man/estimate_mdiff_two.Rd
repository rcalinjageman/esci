% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_mdiff_two.R
\name{estimate_mdiff_two}
\alias{estimate_mdiff_two}
\title{Estimate magnitude of difference between two independent groups.}
\usage{
estimate_mdiff_two(
  data = NULL,
  outcome_variable = NULL,
  grouping_variable = NULL,
  comparison_mean = NULL,
  comparison_sd = NULL,
  comparison_n = NULL,
  reference_mean = NULL,
  reference_sd = NULL,
  reference_n = NULL,
  grouping_variable_levels = NULL,
  outcome_variable_name = "My outcome variable",
  grouping_variable_name = "My grouping variable",
  conf_level = 0.95,
  assume_equal_variance = FALSE,
  save_raw_data = TRUE,
  switch_comparison_order = FALSE
)
}
\arguments{
\item{data}{For raw data - a dataframe or tibble}

\item{outcome_variable}{For raw data - The column name of the outcome
variable, or a vector of numeric data}

\item{grouping_variable}{For raw data - The column name of the grouping
variable, or a vector of group names}

\item{comparison_mean}{For summary data, a numeric}

\item{comparison_sd}{For summary data, numeric > 0}

\item{comparison_n}{For summary data, a numeric integer > 0}

\item{reference_mean}{For summary data, a numeric}

\item{reference_sd}{For summary data, numeric > 0}

\item{reference_n}{For summary data, a numeric integer > 0}

\item{grouping_variable_levels}{For summary data - An optional vector of
2 group labels}

\item{outcome_variable_name}{Optional friendly name for the outcome variable.
Defaults to 'My outcome variable' or the outcome variable column name if a
data frame is passed.}

\item{grouping_variable_name}{Optional friendly name for the grouping
variable.  Defaults to 'My grouping variable' or the grouping variable
column name if a data.frame is passed.}

\item{conf_level}{The confidence level for the confidence interval.  Given in
decimal form.  Defaults to 0.95.}

\item{assume_equal_variance}{Defaults to FALSE}

\item{save_raw_data}{For raw data; defaults to TRUE; set to FALSE to save
memory by not returning raw data in estimate object}

\item{switch_comparison_order}{Defaults to FALSE}
}
\value{
Returnsobject of class esci_estimate
}
\description{
\loadmathjax
\code{estimate_mdiff_two} returns effect sizes estimating the magnitude of
difference between two groups along a quantitative variable.
}
\examples{
# From Raw Data ------------------------------------
# Just pass in the data source, grouping column, and outcome column.
# You can pass these in by position, skipping the labels:

# Note... not sure if PlantGrowth dataset meets assumptions for this analysis
estimate_mdiff_two(
 datasets::PlantGrowth[PlantGrowth$group != 'trt2', ],
 weight,
 group
)

}