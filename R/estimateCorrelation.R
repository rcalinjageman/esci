# These functions were taken from the psych package, https://rdrr.io/cran/psych/src/R/fisherz.R
# THey have been renamed to avoid naming conflicts
"stolen.fisherz" <-
  function(rho)  {0.5*log((1+rho)/(1-rho)) }   #converts r to z  

"stolen.fisherz2r" <-
  function(z) {(exp(2*z)-1)/(1+exp(2*z)) }   #converts back again


estimateCorrelationDifference.default <- function(data, x, y, group, conf.level = 0.95) {
  
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  x_enquo        <-  rlang::enquo(x)
  x_quoname      <-  rlang::quo_name(x_enquo)
  
  y_enquo        <-  rlang::enquo(y)
  y_quoname      <-  rlang::quo_name(y_enquo)
  
  group_enquo        <-  rlang::enquo(group)
  group_quoname      <-  rlang::quo_name(group_enquo)
  
  
  
  # Validate inputs
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
  #Check that y column exists
  if(y_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${y_quoname}"
    )
    stop(err_string)
  }
  #Check that group column exists
  if(group_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a grouping column that exists, not ${group_quoname}"
    )
    stop(err_string)
  }
  # Check if y is numeric
  if(!is.numeric(data[[y_quoname]])) {
    err_string <- stringr::str_interp(
      "y (${y_quoname}) must be numeric, not ${class(data[[y_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }
  # Check if x is numeric
  if(!is.numeric(data[[x_quoname]])) {
    err_string <- stringr::str_interp(
      "x (${x_quoname}) must be numeric, not ${class(data[[x_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }
  
  # Data cleanup ---------------------------
  # Make duplicate copies that can be addressed using $ notation..cause I like it?
  data$x <- data[[x_quoname]]
  data$y <- data[[y_quoname]]
  data$group <- data[[group_quoname]]
  
  # Reduce down to only the x,y,group columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("x", "y", "group")
  data <- data[keeps]  

  # Now remove NAs from data
  na_count <- nrow(data)
  data <- data[!is.na(data$x), ]
  data <- data[!is.na(data$y), ]
  data <- data[!is.na(data$group), ]
  na_count <- na_count - nrow(data)
    
  ##########################
  # Breakdown by groups
  rs <- c(NULL)
  cilows <- c(NULL)
  cihighs <- c(NULL)
  ns <- c(NULL)
  group_labels <- c(NULL)
  labels <- c(NULL)
  this_index <- 0
  
  for (this_group in levels(data$group)) {
    group_only <- data[data$group == this_group, ]
    if (nrow(group_only) > 0) {
      this_index <- this_index + 1
      estimate <- estimateCorrelation.default(data = group_only, x = x, y = y, conf.level = conf.level)
      cilows[this_index] <- estimate$ci.low
      cihighs[this_index] <- estimate$ci.high
      rs[this_index] <- estimate$r
      ns[this_index] <- estimate$n
      group_labels[this_index] <- this_group
      labels[this_index] <- paste(x_quoname, " - ", y_quoname, sep="")
    }
  }
  
  this_index <- 0
  for (this_group in levels(data$group)) {
    this_index <- this_index + 1
    levels(data$group)[this_index] <- paste(levels(data$group)[this_index], ": r = ", formatC(rs[this_index], digits=2, format = "f"), sep="")
  }

  allrs <- data.frame(group = group_labels,
                      variables = labels,
                      r = rs,
                      ci.low = cilows,
                      ci.high = cihighs,
                      n = ns)
    
  estimate <- estimateCorrelationDifference.numeric(r1 = rs[1], n1 = ns[1], r2 = rs[2], n2 = ns[2], group.labels = c(group_labels[1], group_labels[2]), variable.labels = c(x_quoname, y_quoname), conf.level = conf.level)  
  
  estimate$allrs <- allrs
  estimate$plot_info$raw_data <- data
  estimate$plot_info$raw_data$iv <- estimate$plot_info$raw_data$x
  estimate$plot_info$raw_data$dv <- estimate$plot_info$raw_data$y
  estimate$n <- nrow(data)
  estimate$plot_info$y_name <- y_quoname
  estimate$plot_info$x_name <- x_quoname
  estimate$plot_info$group_name <- group_quoname
  estimate$na_count <- na_count
  estimate$type <- "fromRaw"
  
  return(estimate)  
}


estimateCorrelationDifference.numeric <- function(r1, n1, r2, n2, group.labels = c("Group 1", "Group 2"), variable.labels = c("Variable 1", "Variable 2"), conf.level = 0.95) {
  ##########################
  # Validate inputs
  
  # Check that r1 is passed, is numeric and is between -1 and 1
  if (is.null(r1) | !is.numeric(r1) | r1 < -1 | r1 > 1) {
    err_string <- stringr::str_interp(
      "`Must pass r1 as a valid correlation coefficient (numeric value between 0 and 1)."
    )
    stop(err_string)
  }
  
  # Check that r2 is passed, is numeric and is between -1 and 1
  if (is.null(r2) | !is.numeric(r2) | r2 < -1 | r2 > 1) {
    err_string <- stringr::str_interp(
      "`Must pass r2 as a valid correlation coefficient (numeric value between 0 and 1)."
    )
    stop(err_string)
  }
  
  # Check that n1 is passed, is numeric and is an integer > 0
  if (is.null(n1) | !is.numeric(n1) | n1 < 1 | n1%%1 !=0) {
    err_string <- stringr::str_interp(
      "`Must pass n1 as a sample size: an integer value of 1 or greater.)"
    )
    stop(err_string)
  }
  
  # Check that n2 is passed, is numeric and is an integer > 0
  if (is.null(n2) | !is.numeric(n2) | n2 < 1 | n2%%1 !=0) {
    err_string <- stringr::str_interp(
      "`Must pass n1 as a sample size: an integer value of 1 or greater.)"
    )
    stop(err_string)
  }
  
  if (is.null(group.labels[1]) | nchar(group.labels[1]) == 0) {
    group.labels[1] <- "Group 1"
  }
  
  if (is.null(group.labels[2]) | nchar(group.labels[2]) == 0) {
    group.labels[2] <- "Group 2"
  }
  
  if (is.null(variable.labels[1]) | nchar(variable.labels[1]) == 0) {
    variable.labels[1] <- "Variable 1"
  }
  
  if (is.null(variable.labels[2]) | nchar(variable.labels[2]) == 0) {
    variable.labels[2] <- "Variable 2"
  }
  
  # Get estimates for r1 and r2
  r1 <- estimateCorrelation.numeric(r = r1, n = n1, conf.level = conf.level, labels = variable.labels)
  r2 <- estimateCorrelation.numeric(r = r2, n = n2, conf.level = conf.level, labels = variable.labels)

  # Relabel these estimates with group labels for both summary data and error plot data
  r1$summary_data$variables <- paste(group.labels[1], r1$summary_data$variables[1], sep = ": ")
  r2$summary_data$variables <- paste(group.labels[2], r2$summary_data$variables[1], sep = ": ")
  r1$plot_info$summary_data$variables <- paste(r1$plot_info$summary_data$variables[1], group.labels[1], sep = "\n")
  r2$plot_info$summary_data$variables <- paste(r2$plot_info$summary_data$variables[1], group.labels[2], sep = "\n")
  r1$plot_info$error_data$iv <- r1$plot_info$summary_data$variables[1]
  r2$plot_info$error_data$iv <- r2$plot_info$summary_data$variables[1]
  
  # Generate error data
  zr1 <- stolen.fisherz(r1$r)
  zr2 <- stolen.fisherz(r2$r)
  zdiff <- abs(zr2 - zr1)
  sediff <- sqrt(1/(n1-3) + 1/(n2-3))
  zdiff <- zdiff/sediff

  error_data <- data.frame(z = rnorm(10000, mean = 0, sd = sediff))
  error_data$error <- stolen.fisherz2r(error_data$z) + r2$r
  error_data$iv <- "Difference"

  # Calculate rdiff and its CI
  rdiff <- r2$r - r1$r
  ci.high <- rdiff + sqrt((r2$ci.high - r2$r)^2 + (r1$r - r1$ci.low)^2)
  ci.low <- rdiff - sqrt((r2$r - r2$ci.low)^2 + (r1$ci.high - r1$r)^2)

  # Make formatted output
  formatted_r <- stringr::str_interp(
    "r.${group.labels[2]} - r.${group.labels[1]} = $[.2f]{rdiff} ${conf.level*100}% CI [$[.2f]{ci.low}, $[.2f]{ci.high}]"
  )

  # Create summary_data table and set level order for making a plot  
  summary_data <- rbind(r2$summary_data, 
                        r1$summary_data, 
                        data.frame(variables = c("Difference"),
                                   r = rdiff,
                                   conf.level = conf.level,
                                   ci.low = ci.low,
                                   ci.high = ci.high,
                                   n = NA
                                   )
                        )
  
  plot_data <- rbind(r2$plot_info$summary_data, 
                     r1$plot_info$summary_data, 
                        data.frame(variables = c("Difference"),
                                   r = rdiff,
                                   conf.level = conf.level,
                                   ci.low = ci.low,
                                   ci.high = ci.high,
                                   n = NA
                        )
  )
  
  plot_data$variables <- factor(plot_data$variables, levels = c(r1$plot_info$summary_data$variables, r2$plot_info$summary_data$variables, "Difference"))

  # Gather error data
  error_data <- rbind(r2$plot_info$error_data, r1$plot_info$error_data, error_data)

  # Gather plot_info   
  plot_info <- list(
    summary_data = plot_data,
    error_data = error_data
  )

  ## Offset plot data for floting axis
  plot_info$summary_data$r[3] <- plot_info$summary_data$r[3] + r1$r
  plot_info$summary_data$ci.low[3] <- plot_info$summary_data$ci.low[3] + r1$r
  plot_info$summary_data$ci.high[3] <- plot_info$summary_data$ci.high[3] + r1$r
  
   res <- list(
     r = rdiff,
     n = NA,
     conf.level = conf.level,
     ci.low = ci.low,
     ci.high = ci.high,
     formatted_r = formatted_r,
     summary_data = summary_data,
     plot_info = plot_info,
     r1 = r1,
     r2 = r2
   )
   
   class(res) <- "estimate"
  
   return(res)  
}


#' Some other thing
estimateCorrelation <- function(x, ...) {
  UseMethod("estimateCorrelation")
}


estimateCorrelation.numeric <- function(r, n, labels = c("Variable 1", "Variable 2"), conf.level=0.95) {
  #FUnction to estimate correlation from summary data only
  
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
          "labels must have elements 1 and 2 be distinct.  Currently elemnt 1 is ${labels[1]} and element 2 is also ${labels[2]}."
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
  # Check r is valid
  if (is.null(r)) {
    err_string <- stringr::str_interp(
      "'r' must be passed to this function; currently not passed or passed as null."
    )
    stop(err_string)
  }
  if (r > 1) {
    err_string <- stringr::str_interp(
      "'r' must be between -1 and 1 not ${r}."
    )
    stop(err_string)
  }
  # Check that n is valid
  if (is.null(n)) {
    err_string <- stringr::str_interp(
      "'n' must be passed to this function; currently not passed or passed as null."
    )
    stop(err_string)
  }
  if (n<2) {
    err_string <- stringr::str_interp(
      "'n' must be > 2, not ${n}."
    )
    stop(err_string)
  }
  
  # Calculations ---------------------------
  relationship_name <- paste(labels[1], "-", labels[2])
  z <- stolen.fisherz(r)
  se <- (1/sqrt(n-3) )
  moe <- se * qnorm(1 - (1-conf.level)/2)
  zlow <- z - moe 
  zhigh <- z + moe 
  ci.low <- stolen.fisherz2r(zlow)
  ci.high <- stolen.fisherz2r(zhigh)

  # Prep output --------------------------
  # Summary data table  
  summary_data <- data.frame(variables = relationship_name, r = r, conf.level = conf.level, ci.low = ci.low, ci.high = ci.high, n = n)

  # Make formatted verion of 
  formatted_r <- stringr::str_interp(
    "r = $[.2f]{r} ${conf.level*100}% CI [$[.2f]{ci.low}, $[.2f]{ci.high}]"
  )
  
  # Now make the errordata
  error_data <- data.frame(z = rnorm(10000, mean = z, sd = se)) 
  error_data$error <- stolen.fisherz2r(error_data$z)
  error_data$iv <- relationship_name
  
  plot_info <- list(
    iv_name = relationship_name,
    x_name = labels[1],
    y_name = labels[2],
    dv_name = "r",
    summary_data = summary_data,
    error_data = error_data
  )
  
  res <- list(
    r = r,
    n = n,
    conf.level = conf.level,
    ci.low = ci.low,
    ci.high = ci.high,
    formatted_r = formatted_r,
    na_count = 0,
    xname = labels[1],
    yname = labels[2],
    summary_data = summary_data,
    plot_info = plot_info,
    type = "fromSummary"
  ) 
  
  class(res) <- "estimate"
  
  return(res)
}


estimateCorrelation.default <- function(data, x, y, conf.level=0.95) {
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  x_enquo        <-  rlang::enquo(x)
  x_quoname      <-  rlang::quo_name(x_enquo)
  
  y_enquo        <-  rlang::enquo(y)
  y_quoname      <-  rlang::quo_name(y_enquo)
  

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
  #Check that y column exists
  if(y_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${y_quoname}"
    )
    stop(err_string)
  }
  # Check if y is numeric
  if(!is.numeric(data[[y_quoname]])) {
    err_string <- stringr::str_interp(
      "y (${y_quoname}) must be numeric, not ${class(data[[y_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }
  # Check if x is numeric
  if(!is.numeric(data[[x_quoname]])) {
    err_string <- stringr::str_interp(
      "x (${x_quoname}) must be numeric, not ${class(data[[x_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }
  
  
  # Data cleanup ---------------------------
  # Make duplicate copies that can be addressed using $ notation..cause I like it?
  data$iv <- data[[x_quoname]]
  data$dv <- data[[y_quoname]]
  
  # Reduce down to only the iv and dv columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("iv", "dv")
  data <- data[keeps]  
  
  # Now remove NAs from data
  na_count <- nrow(data) - nrow(na.omit(data))
  data <- na.omit(data)
  
  n <- nrow(data)
  r <- cor(data$iv, data$dv)
  lbf <- lm(dv ~ iv, data = data)
  b <- coefficients(lbf)[2]
  a <- coefficients(lbf)[1]
  pinterval <- predict(lbf, interval = "prediction", level = conf.level)
  data <- cbind(data, pinterval)
  regression_equation <- paste("Y' =", formatC(a, format = "fg", digits = 4), "+", formatC(b, format="fg", digits=4), "*X")
#  regression_equation <- stringr::str_interp(
#    "Y' = $[f]{a} + $[f]{b}*X"
#  )
  
  # Calculations ---------------------------
  # Pass off to the summary-data version of this function
  out <- estimateCorrelation.numeric(r = r, 
                                   n = nrow(data), 
                                   labels = c(x_quoname, y_quoname),
                                   conf.level = conf.level
  )
                                      
  
  # Gather output ---------------------------
  out$a <- a
  out$b <- b
  out$lm <- lbf
  out$regression_equation <- regression_equation
  out$plot_info$raw_data <- data
  out$na_count <- na_count
  out$type <- "fromRaw"
  
  return(out)
  
}


  

  