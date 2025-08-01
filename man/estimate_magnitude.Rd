% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_magnitude.R
\name{estimate_magnitude}
\alias{estimate_magnitude}
\title{Estimates for a continuous variable with no grouping (single-group design)}
\usage{
estimate_magnitude(
  data = NULL,
  outcome_variable = NULL,
  mean = NULL,
  sd = NULL,
  n = NULL,
  outcome_variable_name = "My outcome variable",
  conf_level = 0.95,
  save_raw_data = TRUE
)
}
\arguments{
\item{data}{For raw data - A data frame or tibble}

\item{outcome_variable}{For raw data - The column name of the outcome
variable, or a vector of numeric data}

\item{mean}{For summary data - A numeric representing the mean of the outcome
variable}

\item{sd}{For summary data - A numeric > 0, standard deviation of the outcome
variable}

\item{n}{For summary data - An integer > 0, sample size of the outcome
variable}

\item{outcome_variable_name}{Optional friendly name for the outcome variable.
Defaults to 'My outcome variable' or the outcome variable column name if a
data frame is passed.}

\item{conf_level}{The confidence level for the confidence interval.  Given in
decimal form.  Defaults to 0.95.}

\item{save_raw_data}{For raw data; defaults to TRUE; set to FALSE to save
memory by not returning raw data in estimate object}
}
\value{
Returns an object of class esci_estimate
\itemize{
\item \strong{overview}
\itemize{
\item \emph{outcome_variable_name} -
\item \emph{mean} -
\item \emph{mean_LL} -
\item \emph{mean_UL} -
\item \emph{median} -
\item \emph{median_LL} -
\item \emph{median_UL} -
\item \emph{sd} -
\item \emph{min} -
\item \emph{max} -
\item \emph{q1} -
\item \emph{q3} -
\item \emph{n} -
\item \emph{missing} -
\item \emph{df} -
\item \emph{mean_SE} -
\item \emph{median_SE} -
}
\item \strong{es_mean}
\itemize{
\item \emph{outcome_variable_name} -
\item \emph{effect} -
\item \emph{effect_size} -
\item \emph{LL} -
\item \emph{UL} -
\item \emph{SE} -
\item \emph{df} -
\item \emph{ta_LL} -
\item \emph{ta_UL} -
}
\item \strong{es_median}
\itemize{
\item \emph{outcome_variable_name} -
\item \emph{effect} -
\item \emph{effect_size} -
\item \emph{LL} -
\item \emph{UL} -
\item \emph{SE} -
\item \emph{df} -
\item \emph{ta_LL} -
\item \emph{ta_UL} -
}
\item \strong{raw_data}
\itemize{
\item \emph{grouping_variable} -
\item \emph{outcome_variable} -
}
}
}
\description{
\code{estimate_magnitude} is suitable for a single group design with a
continuous outcome variable.  It estimates the population mean and
population median (raw data only) with confidence intervals.  You can
pass raw data or summary data.
}
\details{
Reach for this function in place of a one-sample \emph{t}-test or \emph{z}-test.

Once you generate an estimate with this function, you can visualize
it with \code{\link[=plot_magnitude]{plot_magnitude()}}.

If you want to compare your sample to a known value or reference, then
use \code{\link[=estimate_mdiff_one]{estimate_mdiff_one()}}.

The estimated mean is from \link[statpsych:ci.mean]{statpsych::ci.mean} (named ci.mean1 in
statpsych < 1.6).

The estimated median is from \code{\link[statpsych:ci.median]{statpsych::ci.median()}} (named ci.median1
in of statpsych < 1.6)
}
\examples{
# From raw data
data("data_penlaptop1")

estimate_from_raw <- esci::estimate_magnitude(
  data = data_penlaptop1[data_penlaptop1$condition == "Pen", ],
    outcome_variable = transcription
)

# To visualize the estimate
myplot_from_raw <- esci::plot_magnitude(
  estimate_from_raw,
  effect_size = "median"
)


# From summary data
mymean <- 24.5
mysd <- 3.65
myn <- 40

estimate_from_summary <- esci::estimate_magnitude(
  mean = mymean,
  sd = mysd,
  n = myn
)

# To visualize the estimate
myplot_from_summary <- esci::plot_magnitude(
  estimate_from_summary,
  effect_size = "mean"
)


}
