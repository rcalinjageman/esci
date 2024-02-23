################################################################################
# 12-color palette for deuteranopia color blindness
# 18:05:21 Tue 19 May 2020
# Martin Krzywinski martink@bcgsc.ca
# Methods and details: http://mkweb.bcgsc.ca/colorblind
# Color names: http://mkweb.bcgsc.ca/colornames
################################################################################
#' @importFrom glue glue
#' @importFrom methods is
#' @importFrom rlang enquo
#' @importFrom rlang as_name
#' @importFrom rlang abort
#' @importFrom statpsych ci.lc.mean.bs
#' @importFrom statpsych ci.lc.median.bs
#' @importFrom statpsych ci.lc.prop.bs
#' @importFrom statpsych ci.mean1
#' @importFrom statpsych ci.median1
#' @importFrom statpsych ci.oddsratio
#' @importFrom statpsych ci.phi
#' @importFrom statpsych ci.prop1
#' @importFrom statpsych ci.ratio.mean2
#' @importFrom statpsych ci.ratio.mean.ps
#' @importFrom statpsych ci.ratio.median2
#' @importFrom statpsych ci.ratio.median.ps
#' @importFrom statpsych ci.stdmean1
#' @importFrom statpsych ci.stdmean.ps
#' @importFrom stats aggregate
#' @importFrom stats anova
#' @importFrom stats complete.cases
#' @importFrom stats confint
#' @importFrom stats cor
#' @importFrom stats line
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats qnorm
#' @importFrom stats predict
#' @importFrom stats pt
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils packageVersion
#' @importFrom grid grobName
#' @import ggplot2
#' @import ggdist
NULL
#> NULL

esci_solve_imports <- function() {
  ggbeeswarm::geom_beeswarm
  ggbeeswarm::geom_quasirandom
  Rdpack::get_usage
  mathjaxr::preview_rd
  R6::is.R6
  utils::packageVersion
}
