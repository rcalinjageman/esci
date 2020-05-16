#This function was stolen from the psych package.
# I renamed it to avoid having any name collision
# Should make it a private function

"stolen.harmonic.mean" <- function(x,na.rm=TRUE,zero=TRUE) {if(!zero) {x[x==0] <- NA}
  if (is.null(nrow(x))) {1/mean(1/x,na.rm=na.rm) } else {
    1/(apply(1/x,2,mean,na.rm=na.rm))} }


print.estimate <- function(estimate) {
  #print for estimate
  
  if (!is.null(estimate$summary_data)) { 
    cat("\n---Estimate---\n")
    print(estimate$summary_data) 
  }
  cat(reportEstimate(estimate, section = "All"))
}


reportEstimate <- function(estimate, section=c("All"), print.title = TRUE) {
  #This function prints objects of class estimate
  
  summary <- ""

  for (i in names(estimate$report)) {
    if(i %in% section | "All" %in% section)
      if(print.title) {
        summary <- paste(summary, estimate$report[[i]]$title, estimate$report[[i]]$summary)
      } else {
        summary <- paste(summary, estimate$report[[i]]$summary)
      }
  }
  
  return(summary)
}


#' Estimate the difference between two means
#' @param Bob Something or another
#' 
#' Some other thing
estimateMeanDifference <- function(x, ...) {
  UseMethod("estimateMeanDifference")
}


estimateMeanDifference.numeric <- function(m1, m2, s1, s2, n1, n2 = NA, r = NA, labels = c("Group 1", "Group 2"), paired = FALSE, var.equal = TRUE, conf.level=0.95) {
  #FUnction to compare means from two groups using summary data only
  
  # Validate inputs ---------------------------
  # Check labels
  if (is.null(labels)) {
    err_string <- stringr::str_interp(
      "`labels` must either be omitted or not null"
    )
    stop(err_string)
  } else {
    if(length(labels) < 2) {
      err_string <- stringr::str_interp(
        "`labels` must be a vector with two elements.  Currently class is ${class(labels)} of length ${length(labels)}"
      )
      stop(err_string)
    } else {
      if(labels[1] == labels[2]) {
        err_string <- stringr::str_interp(
          "labels must have elemtns 1 and 2 be distinct.  Currently elemnt 1 is ${labels[1]} and element 2 is also ${labels[2]}."
        )
        labels <- c("Group 1", "Group 2")
        warning(err_string)
      }
    }
  }
  # Check conf.level is >=.50 and < 1.  Could allow down to near 0, but why bother?
  if (conf.level < 0.50 | conf.level >= 1) {
    err_string <- stringr::str_interp(
      "`conf.level` must be between 0.50 and 1, not ${conf.level}"
    )
    stop(err_string)
  }
  # Check sds are not 0
  if (s1 == 0 | s2 == 0) {
    err_string <- stringr::str_interp(
      "Group standard deviations cannot be 0.  Currently you passed s1 = ${s1} and s2 = ${s2}"
    )
    stop(err_string)
  }
  # Check ns > 2
  if (n1 < 3 | (!paired & n2 < 2)) {
    err_string <- stringr::str_interp(
      "Sample sizes must be 2 or more per group.  Currently you passed n1 = ${n1} and n2 = ${n2}"
    )
    stop(err_string)
  }
  
  #This is a kludge to prevent an error when the means are exactly equal
  # Need to find a better way to work with this
  if (m1 == 0 & m2 == 0) {
    while(m1 == 0) {
      m1 <- m1 + (0+ 1e-16)
    }
  }
  
  # For paired, make sure r is passed and within range of -1 to 1
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
    # ALso, for paired n2 does not need to be sent, but if it is, it must be equal to n1 (otherwise user is confused)
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
  
  
  # Calculations ---------------------------
  # FOr paired, we will set n2 to n1, enabling use of same equations as for unpaired for sdpooled
  if(paired) {
    n2 <- n1
  }
  
  # Calculate alpha
  alpha <- 1-conf.level
  
  # Put input data into a data frame
  summary_data <- data.frame(iv = labels, n = c(n1, n2), m = c(m1, m2), s = c(s1, s2))   
  
  #Calculate df and sem for each group
  summary_data$df <- summary_data$n - 1
  summary_data$sem <- summary_data$s / sqrt(summary_data$n)
  
  # Calculate mdiff and pooledsd
  mdiff <- m1-m2  
  pooledsd <- sqrt( (n1-1) *s1^2 + (n2-1)*s2^2) / sqrt(n1+n2-2)
  
  # Calc standard deviation, standard error, and degrees of freedom for the difference
  if(paired) {
    # Paired design.  sdiff calculated based on sx, sy, and r; df is n1-1, se is sdiff/sqrt(n1)
    sdiff <- sqrt(s1^2 + s2^2 - 2*r*s1*s2)
    dfdiff <- n1 -1
    ndiff <- n1
    sediff <- sdiff / sqrt(ndiff)
    sdstore <- sdiff
  } else {
    #Non paired designs. Set r and sdiff to NA, calculate others based on if equal variance or not
    sdiff <- pooledsd
    ndiff <- n1+n2
    r <- NA
    sdiff <- NA
    if(var.equal) {
      #Equal variance
      dfdiff <- n1+n2-2
      sediff <- pooledsd * sqrt(1/n1 + 1/n2)
    } else {
      #Variance not equal
      sediff <- sqrt(summary_data$sem[1]^2 + summary_data$sem[2]^2)
      dfdiff <- sediff^4/(summary_data$sem[1]^4/(n1-1) + summary_data$sem[2]^4/(n2-1))
    }
    sdstore <- pooledsd
  }
  
  # Add basic info about the difference into the summary_data dataframe
  summary_data <- rbind(summary_data, data.frame(iv = "Difference", n = ndiff, m = mdiff, s=sdstore, df=dfdiff, sem = sediff))  
  
  # Now that we have a table of basic info that has rows for each group and the difference, apply common calcs
  # Calculate moe, cilow and cihigh
  summary_data$moe <- summary_data$sem * abs(qt(1-alpha/2,summary_data$df[3]))
  summary_data$cilow <- summary_data$m - summary_data$moe
  summary_data$cihigh <- summary_data$m + summary_data$mo
  
  # Obtain the standardized mean difference and CI
  if(paired) {
    smd <- estimateStandardizedMeanDifference(m1 = m1, m2 = m2, s1 = s1, s2 = s2, n1 = n1, r = r, paired = paired)
  } else {
    smd <- estimateStandardizedMeanDifference(m1 = m1, m2 = m2, s1 = s1, s2 = s2, n1 = n1, n2 = n2, paired = paired, var.equal = var.equal)
  }
  
  # Now the actual t test ad p value (blech)
  t <- mdiff / summary_data$sem[3]
  p <- 2*pt(-abs(t),df=summary_data$df[3])
  
  # Now we generate the sampling error distribution.
  # This is a bit odd--we're sampling from the t distribution and scaling based on the sem of the difference and then anchoring to the reference group
  # THis lets us use the density of this data to plot the sampling error distribution using the GeomFlatViolin code I stole from DaBestR
  diffdata <-  data.frame(t = rt(10000, summary_data$df[3]))
  diffdata$dv <-  (summary_data$sem[3] * diffdata$t)
  diffdata$iv <-  "Difference"
  
  # Prep Ouput ---------------------------
  # Set factor level order for summary_data
  summary_data$iv <- factor(summary_data$iv, levels = c(labels, "Difference"))
  
  # Make formatted verion of mdiff
  formatted_mdiff <- stringr::str_interp(
    "Mdiff = $[.3f]{mdiff} ${conf.level*100}% CI [$[.3f]{summary_data$cilow[3]}, $[.3f]{summary_data$cihigh[3]}]"
  )
  
  #Make a report
  report <- smd$report
  report$DecisionMaking <- list(
    title = "\n---Decisions Making---\n",
    summary = paste("t(", dfdiff, ") = ", t, ", p = ", p, "\n")
  )
 
  #CI Level 
  summary_notes <- paste("CIs are at the", conf.level*100, "% level.\n")
  #Explanation of paired or not, equal variance or not
  if(paired) {
    summary_notes <- paste(summary_notes, "This comparison was made on paired data.\n")
    summary_notes <- paste(summary_notes, "Correlation between paired measures =", format(r, digits=3), "\n")
    summary_notes <- paste(summary_notes, "s in the row for the difference is the standard deviation of the difference scores")
  } else {
    summary_notes <- paste(summary_notes, "This comparison was made on unpaired data.\n")
    if(var.equal) {
      summary_notes <- paste(summary_notes, "Equal variance was assumed.\n") 
    } else {
      summary_notes <- paste(summary_notes, "Equal variance was not assumeed.  No standardized mean difference (Cohen d) was returned.\nCurrently a CI on the standardized mean difference requires equal variances.\n")
    }
    summary_notes <- paste(summary_notes, "s in the row for the difference is the pooled standard deviation")
  }
  
  report$Notes <- list(
    title = "\n---Notes---\n",
    summary = summary_notes
  )
  
  
  
  #Store plot_info
  plot_info <- list(
    plotiv = "iv",
    plotdv = "m",
    plot_data = summary_data,
    diffdata = diffdata
  )
  

  # Gather output in a list
  out <- list(
    paired = paired,
    var.equal = var.equal,
    conf.level = conf.level,
    level1 = labels[1],
    level2 = labels[2],
    m1 = m1,
    m2 = m2,
    s1 = s1,
    s2 = s2,
    n1 = n1,
    n2 = n2,
    sdiff = sdiff,
    r = r,
    t = t,
    df = dfdiff,
    p = p,
    pooledsd = pooledsd,
    mdiff = mdiff,
    ci.low = summary_data$cilow[3],
    ci.high = summary_data$cihigh[3],
    formatted_mdiff = formatted_mdiff,
    summary_data = summary_data,
    cohend = smd$cohend,
    cohend.low = smd$cohend.low,
    cohend.high = smd$cohend.high,
    cohend.denom = smd$cohend.denom,
    cohend.denom_name = smd$cohend.denom_name,
    cohend.name = smd$cohend.name,
    html_d = smd$html_d,
    html_summary = smd$html_summary,
    formatted_d = smd$formatted_d,
    plot_info = plot_info,
    report = report,
    type = "fromSummary"
  )
  # Set class as estimate for printing purposes
  class(out) <- "estimate"
  return(out)
}


estimateMeanDifference.default <- function(data, x, y, paired = FALSE, var.equal = TRUE, conf.level=0.95, reference.group = 2, standardizer = "PooledSD") {
  # Standardizer = ReferenceGroup to normalize cohen's d to reference group s, otherwise normalized to pooledsd
  # Need to implement SMD for unequal variance--but still haven't found a good technique
  # Implement formula interface?  Needed?
  # Implement long data format for paired?  
  
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  x_enquo        <-  rlang::enquo(x)
  x_quoname      <-  rlang::quo_name(x_enquo)
  
  y_enquo        <-  rlang::enquo(y)
  y_quoname      <-  rlang::quo_name(y_enquo)
  
  # Initialize r to NA so it is returned as NA for non-paired designs.  WIll be calculated for paired
  r = NA
  
  
  # Validate inputs ---------------------------
  # Check that reference.group is in range (1 or 2)
  if (reference.group == 1 | reference.group == 2) { } else {
    err_string <- stringr::str_interp(
      "`reference.group` must be 1 or 2 (to indicate group 1 or group 2 is the control), not ${reference.group}"
    )
    stop(err_string)
  }
  # check CI.
  if (conf.level < 0.50 | conf.level >= 1) {
    err_string <- stringr::str_interp(
      "`conf.level` must be between 0.50 and 1, not ${conf.level}"
    )
    stop(err_string)
  }
  # Check that var.equal is not set with paired
  if(paired & !var.equal) {
    err_string <- stringr::str_interp(
      "You have requested analysis of paired data and yet var.equal to false.  This combination doesn't make sense.  Please change one or the other."
    )
    warning(err_string)
  }
  # Check data is a dataframe
  if(!is.data.frame(data)) {
    err_string <- stringr::str_interp(
      "`data` must be a data frame, not ${class(data)}"
    )
    stop(err_string)
  }
  #Check data has more than 2 rows
  if(nrow(data)<3) {
    err_string <- stringr::str_interp(
      "`data` must have more than 2 rows, not ${nrow(data)}"
    )
    stop(err_string)
  }
  #Check that x column exists
  if(x_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${x_quoname}"
    )
    stop(err_string)
  }
  #Check that y column exists
  if(y_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${y_quoname}"
    )
    stop(err_string)
  }
  # Check if dv is numeric
  if(!is.numeric(data[[y_quoname]])) {
    err_string <- stringr::str_interp(
      "y (${y_quoname}) must be numeric, not ${class(data[[y_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }
  
  # For paired must in wide format (two numeric columns)
  if(paired) {
    #has to be wide format, so be sure x and y are both numeric
    if(!is.numeric(data[[x_quoname]])) {
      err_string <- stringr::str_interp(
        "You specified paired data, so both 'x' and 'y' must be numeric (wide-format). Currently you passed: 'x' as ${class(data[[x_quoname]])} and 'y' as ${class(data[[y_quoname]])}.  Try making a numeric colum with as.numeric or, if your paired data is in long format, try tidyr:spread to convert to wide."
      )
      stop(err_string)
    }
    if(!is.numeric(data[[y_quoname]])) {
      err_string <- stringr::str_interp(
        "You specified paired data, so both 'x' and 'y' must be numeric (wide-format).  Currently you passed: 'x' as ${class(data[[x_quoname]])} and 'y' as ${class(data[[y_quoname]])}.  Try making a numeric colum with as.numeric or, if your paired data is in long format, try tidyr:spread to convert to wide."
      )
      stop(err_string)
    }
  } 
  
  # Finally, for unpaired and unpaired-wide format make sure grouping variable is a factor
  if (!paired) {
    if(!is.factor(data[[x_quoname]])) {
      err_string <- stringr::str_interp(
        "'x' (${x_quoname}) must be a factor for unpaired data, not ${class(data[[x_quoname]])}.  Try making a grouping column with as.factor"
      )
      stop(err_string)
    }
    # Check if iv has at least two levels
    if(length(levels(data[[x_quoname]])) < 2) {
      err_string <- stringr::str_interp(
        "x (${x_quoname}) must have at least 2 levels for unpaired data, not ${length(levels(data[[x_quoname]]))}."
      )
      stop(err_string)
    }
  }
  
  
  # Data cleanup ---------------------------
  # Make duplicate copies that can be addressed using $ notation..cause I like it?
  data$iv <- data[[x_quoname]]
  data$dv <- data[[y_quoname]]
  
  # Reduce down to only the iv and dv columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("iv", "dv")
  data <- data[keeps]  
  
  # Now remove NAs from data
  data <- data[!is.na(data$dv), ]
  data <- data[!is.na(data$iv), ]
  
  # Next, if unpaired data, trim the data down to just two levels
  if(!paired & length(levels(data$iv)) > 2) {
    data <- data[data$iv == levels(data$iv)[1] | data$iv == levels(data$iv)[2], ] 
  }
  
  # Deal with paired data, converting it to long with difference scores added in, and storing pearson's r
  if(paired) {
    #Store r; intialized to NA above, now set for only paired data
    r <- cor(data$dv, data$iv)
    #Calculate difference scores according to reference group passed in
    if(reference.group == 2) {
      data$diff <- data$iv - data$dv
    } else {
      data$diff <- data$dv - data$iv
    }
    # Now build a long-format dataframe with the difference scores
    ldata <- data.frame(iv = as.factor(rep(x_quoname, nrow(data))), dv = data$iv, id=c(1:nrow(data)))
    ldata <- rbind(ldata, data.frame(iv = as.factor(rep(y_quoname, nrow(data))), dv = data$dv, id=c(1:nrow(data))))
    ldata <- rbind(ldata, data.frame(iv = as.factor(rep("Difference", nrow(data))), dv = data$diff, id=c(1:nrow(data))))
    data <- ldata
  }
  
  # Now set level order and add "Difference" level for graphing
  if(reference.group == 2) {
    level1 <- levels(data$iv)[1]
    level2 <- levels(data$iv)[2]
  } else {
    level1 <- levels(data$iv)[2]
    level2 <- levels(data$iv)[1]
  }
  data$iv <- factor(data$iv, levels = c(level1, level2, "Difference")) 
  
  
  # Finally, split by group and last check ---------------------------
  # Now split by group
  group1 <- data[data$iv == level1, ]
  group2 <- data[data$iv == level2, ]
  
  #One more check--be sure at least 2 valid cases in each group
  if(length(group1$dv) <2 | length(group2$dv) <2 ) {
    err_string <- stringr::str_interp(
      "Must have N > 2 for both groups.
      Currently for ${level1} you have ${length(group1$dv)}
      and for ${level2} you have ${length(group2$dv)}"
    )
    stop(err_string)
  }
  
  
  # Calculations ---------------------------
  # Pass off to the summary-data version of this function
  out<-estimateMeanDifference.numeric(m1 = mean(group1$dv), m2 = mean(group2$dv),
                               s1 = sd(group1$dv), s2 = sd(group2$dv),
                               n1 = length(group1$dv), n2 = length(group2$dv),
                               r = r,
                               paired = paired, conf.level = conf.level, var.equal = var.equal,
                               labels = c(level1, level2)
  )
  
  # Gather output ---------------------------

  
  # Set estimate type and add in raw data, then return
  out$type <- "fromRaw"
  out$raw_data <- data
  out$x <- x_quoname
  out$y <- y_quoname
  class(out) <- "estimate"
  
  return(out)
}


estimateMean <- function(data, x, conf.level=0.95, na.rm = FALSE) {
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  x_enquo        <-  rlang::enquo(x)
  x_quoname      <-  rlang::quo_name(x_enquo)

  # Validate inputs ---------------------------
  # check CI.
  if (conf.level < 0.50 | conf.level >= 1) {
    err_string <- stringr::str_interp(
      "`conf.level` must be between 0.50 and 1, not ${conf.level}"
    )
    stop(err_string)
  }
  # Check data is a dataframe
  if(!is.data.frame(data)) {
    err_string <- stringr::str_interp(
      "`data` must be a data frame, not ${class(data)}"
    )
    stop(err_string)
  }
  #Check data has more than 2 rows
  if(nrow(data)<3) {
    err_string <- stringr::str_interp(
      "`data` must have more than 2 rows, not ${nrow(data)}"
    )
    stop(err_string)
  }
  #Check that x column exists
  if(x_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${x_quoname}"
    )
    stop(err_string)
  }
  # Check if x column is numeric
  if(!is.numeric(data[[x_quoname]])) {
    err_string <- stringr::str_interp(
      "x (${x_quoname}) must be numeric, not ${class(data[[x_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }

  
  # Data cleanup ---------------------------
  # Make duplicate copies that can be addressed using $ notation..cause I like it?
  data$dv <- data[[x_quoname]]


  # Reduce down to only the iv and dv columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("dv")
  data <- data[keeps]  
  data$iv <- x_quoname
  data$included <- TRUE
  
  # Track NAs
  na_count <- nrow(data) - nrow(na.omit(data))
  
  # Now remove NAs from data
  if (na.rm) {
    data <- data[!is.na(data$dv), ]
    
  }

  m <- mean(data$dv, na.rm = na.rm)
  n <- nrow(data)
  s <- sd(data$dv, na.rm = na.rm)
  qs <- quantile(data$dv, na.rm = na.rm) 
  moe <- s / sqrt(n) * qt(1 - (1-conf.level)/2, n-1)
  ci.low <- m - moe
  ci.high <- m + moe
  formatted_mean <- stringr::str_interp(
    "mean = $[.2f]{m} ${conf.level*100}% CI [$[.2f]{ci.low}, $[.2f]{ci.high}]"
  )
    
  summary_data <- data.frame(
      Variable = x_quoname, 
      m = m, 
      conf.level = conf.level,
      ci.low = ci.low, 
      ci.high = ci.high, 
      s = s, 
      N = n, 
      na_count = na_count, 
      median = qs[3], 
      q1 = qs[2], 
      q3 = qs[4]
      )
  
  error_data <-  data.frame(t = rt(10000, n-1))
  error_data$dv <-  s/sqrt(n) * error_data$t
  error_data$iv <-  x_quoname
  
  plot_info <- list(
    plotiv = x_quoname,
    plotdv = "m",
    raw_data = data,
    plot_data = summary_data,
    error_data = error_data
  )
    
  res <- list(
    m = m,
    conf.level = conf.level,
    ci.low = ci.low,
    ci.high = ci.high,
    formatted_mean = formatted_mean,
    s = s,
    n = n,
    df = n -1,
    na_count = na_count,
    median = qs[3],
    q1 = qs[2],
    q3 = qs[4],
    summary_data = summary_data,
    plot_info = plot_info,
    type = "fromRaw"
  )
  class(res) <- "estimate"
  return(res)
}