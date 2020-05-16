estimateStandardizedMeanDifference <- function(m1, m2, s1, s2, n1, n2 = NA, r = NA, paired = FALSE, conf.level = .95, var.equal = TRUE) {
  # This function calculates standardized effect size with CI for comparing two means
  # It was adapted from:
  # Goulet-Pelletier, J.-C., & Cousineau, D. (2018). A review of effect sizes and their condence intervals, Part I: The Cohen’s d family, 14(4). https://doi.org/10.20982/tqmp.14.4.p242
  # http://www.tqmp.org/RegularArticles/vol14-4/p242/p242.R
  #
  
  # Validate inputs ---------------------------
  # Confidence level should be in range > 0.5 and <1 (technically could be <.5, but why?)
  if (conf.level < 0.50 | conf.level >= 1) {
    err_string <- stringr::str_interp(
      "`conf.level` must be between 0.50 and 1, not ${conf.level}"
    )
    stop(err_string)
  }
  # Make sure group standard deviations are not 0
  if (s1 == 0 | s2 == 0) {
    err_string <- stringr::str_interp(
      "Group standard deviations cannot be 0.  Currently you passed s1 = ${s1} and s2 = ${s2}"
    )
    stop(err_string)
  }
  # For paired designs, make sure r is passed an in range.  
  if(paired) {
    if(is.na(r)) {
      err_string <- stringr::str_interp(
        "For paired designs, must pass r, the correlation between the paired measures.  
        Currently you passed r = NA or didn't pass r.  
        If you have only sddiff, you can obtain r with this formula:  
        r = (Sdiff - S2x - S2y) / (2 * Sx * Sy) where
        Sdiff is sd of the diff scores, s2x is var of x, s2y is var of y, sx is standard deviation of x and sy is standard deviation of y."
      )
      stop(err_string)
    }
    if(r < -1 | r >1) {
      err_string <- stringr::str_interp(
        "For paired designs, must pass r between -1 and 1.  
        Currently you passed r = ${r}.  
        If you have only sddiff, you can obtain r with this formula:  
        r = (Sdiff - S2x - S2y) / (2 * Sx * Sy) where
        Sdiff is sd of the diff scores, s2x is var of x, s2y is var of y, sx is standard deviation of x and sy is standard deviation of y."
      )
      stop(err_string)
    }
    # Also, check if n2 was passed.  Not needed for paired designs, but if it was passed, should be same as n1
    if(!is.na(n2)) {
      if(n1 != n2) {
        err_string <- stringr::str_interp(
          "For paired designs, either don't pass n2 or make sure it has the same value as n1.  
        Currently you passed n1 = ${n1} and n2 = ${n2}"  
        )
        stop(err_string)
        
      }
    }
  }

  #This is a kludge to prevent an error when the means are exactly equal
  # Need to find a better way to work with this
  if (m1 == m2) {
    while(m1 == m2) {
      m1 <- m1 * (1+ 1e-12)
    }
  }
  
  # Calculations ---------------------------
  #Set d_name and de_denom name based on design.  Also, for paired, set n2 = to n1
  if(paired) {
    n2 <- n1
    d_denom_name <- "SDavg"
    d_name <- "d_avg"
    d_sub <- "avg"
  } else {
    if(var.equal) {
    d_denom_name <- "SDpooled"
    d_name <- "d_unbiased"
    d_sub <- "unbiased"
    } else {
      d_name <- "Not returned because var.equal set to true"
      d_denom_name <- ""
      d_sub <- ""
    }
  }
  
  # Calculate denominator for d.  This is sd pooled for ind. designs.  For paired, it is sdavg which can use same formulat when n1=n2
  d_denom <- sqrt( (n1-1) *s1^2 + (n2-1)*s2^2) / sqrt(n1+n2-2)
  
  ntilde <- stolen.harmonic.mean(c(n1,n2))
  # Compute biased Cohen's d (equation 1) 
  cohend <- abs((m1-m2) / d_denom)
  
  # Compute unbiased Hedges' g (equations 2a and 3)
  eta     <- n1 + n2 - 2
  # Use the lgamma function, and update to what Goulet-Pelletier & cousineau used; works with larger inputs 
  J <- exp ( lgamma(eta/2) - (log(sqrt(eta/2)) + lgamma((eta-1)/2) ) )
  hedgesg <-  cohend * J
  
  # Compute noncentrality parameter (equation 5a or 5b depending on the design)
  if(paired) {
    lambda <- hedgesg * sqrt( ntilde/(2 * (1-r)) )
  } else {
    lambda <- hedgesg * sqrt( ntilde/2)
  }
  
  # Confidence interval of the hedges g (equations 6 and 7)
  tlow <- qt(1/2 - conf.level/2, df = eta, ncp = lambda )
  thigh <- qt(1/2 + conf.level/2, df = eta, ncp = lambda )
  if(m1 == m2) {
    dlow <- (tlow*(n1+n2)) / (sqrt(eta) * sqrt(n1*n2))
    dhigh <- (thigh*(n1+n2)) / (sqrt(eta) * sqrt(n1*n2))
  } else {
    dlow <- tlow / lambda * hedgesg 
    dhigh <- thigh / lambda * hedgesg 
    
  }
  
  # The function provided by Goulet-Pelletier & Cousineau works with +mdiff.
  # Now we fix the signs and directions for negative differences
  if ((m1-m2) < 0) {
    hedgesg <- hedgesg * -1
    tdlow <- dlow
    dlow <- dhigh * -1
    dhigh <- tdlow * -1
  }

  # Currently, don't have a solution for unequal variances.  Something to work on.
  if(!var.equal) {
    hedgesg <- NA
    dlow <- NA
    dhigh <- NA
    cohend.denom <- NA
    formated_d <- NA
  }
  
  
  # Gather output to return ---------------------------
  # Create a formatted version of the SMD and CI
  formatted_d <- stringr::str_interp(
    "${d_name} = $[.2f]{hedgesg} ${conf.level*100}% CI [$[.2f]{dlow}, $[.2f]{dhigh}]"
  )
  
  html_d <- stringr::str_interp(
    "<i>d</i><sub>${d_sub}</sub> = $[.2f]{hedgesg} ${conf.level*100}% CI [$[.2f]{dlow}, $[.2f]{dhigh}]</br>"
  )
  if(d_sub == "") { html_d <- ""}
  
  # Create a report
  summary <- ""
  if(var.equal) {
    #summary <- paste(summary, "In standardized terms: ", formatted_d, "\n")
    summary <- paste(summary, "Note that the standardized effect size is", d_name, "\nbecause the denominator used was", d_denom_name, "\nwhich had a value of",  format(d_denom, digits = 3), "\n")
    summary <- paste(summary, "The standardized effect size has been corrected for bias.\nThe bias-corrected version of Cohen's d is sometimes also (confusingly) called Hedges' g.\n")
  } else {
    summary <- paste(summary, "Standardized mean difference was not returned because equal variance was not set to TRUE.\n")
  }
  
  html_summary <- gsub("\n", "</br>", summary)
  
  summary <- paste("In standardized terms: ", formatted_d, "\n", summary)
  
  report <- list()
  report$smd <- list(
    title = "\n---Standardized mean difference---\n",
    summary = summary
  )

  
  # Create a list with all outputs  
  out <- list(
    paired = paired,
    conf.level = conf.level,
    var.equal = var.equal,
    cohend = hedgesg,
    cohend.low = dlow,
    cohend.high = dhigh,
    cohend.denom = d_denom,
    cohend.denom_name = d_denom_name,
    cohend.name = d_name,
    formatted_d = formatted_d,
    html_d = html_d,
    html_summary = html_summary,
    r = r,
    type = "SMDOnly",
    report = report
  )
  # Give the list the class estimate for printing purposes
  class(out) <- "estimate"
  return(out)
}
