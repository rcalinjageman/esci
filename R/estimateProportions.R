#' Estimate a proportion
#' 
#' \code{estimateProportion} estimates a proportion along with a confidence interval for a categorical variable.
#' The function can accept a column of categorical outcomes (a factor).  It will calculate the proportion of 
#' outcomes of a specified level (cases) out of all valid outcomes (n): P = cases/N.  It will then calculate
#' the confidence interval on that proportion.
#' 
#' This function can also accept summary data, where the user pases the number of cases and sample size directly.  
#' 
#' @param cases The number of observations of the specific outcome
#' @param n The total number of observations
#' @param caselabels Optional argument to define labels for outcomes.  Default is "Affected" and "Not Affected".
#' @param conf.level Confidence level for the confidence interval.  Default is 0.95.  Must be >0.5 and <1.
#' 
#' @examples
#' estimateProportion(cases = 10, n = 100)
#' estimateProportion(cases = 10, n = 100, conf.level = 0.90)
#' estimateProportion(cases = 10, n = 100, caselabels = c("Employed", "Unemployed"))


#' Estimate the difference between two means
#' @param Bob Something or another
#' 
#' Some other thing
estimateProportion <- function(x, ...) {
  UseMethod("estimateProportion")
}

estimateProportionDifference <- function(x, ...) {
  UseMethod("estimateProportionDifference")
}


estimateProportion.numeric = function(cases, n, caselabels = c("Affected", "Not Affected"), conf.level=0.95) {
  #Function to calculate CI on single proportion from summary data only
  
  # Validate inputs ---------------------------
  # Check case labels
  if (is.null(caselabels)) {
    err_string <- stringr::str_interp(
      "`caselabels` must either be omitted or not null"
    )
    stop(err_string)
  } else {
    if(length(caselabels) < 2) {
      err_string <- stringr::str_interp(
        "`caselabels` must be a vector with two elements.  Currently class is ${class(caselabels)} of length ${length(caselabels)}"
      )
      stop(err_string)
    } else {
      if(caselabels[1] == caselabels[2]) {
        err_string <- stringr::str_interp(
          "caselabels must have 2 distinct elements.  Currently element 1 is ${caselabels[1]} and element 2 is also ${caselabels[2]}."
        )
        caselabels <- c("Affected", "Not Affected")
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
  
  # Check cases
  if(is.null(cases)) {
    err_string <- stringr::str_interp(
      "`cases` must be an integer number, not NULL"
    )
    stop(err_string)
  }
  if(cases != as.integer(cases)) {
    err_string <- stringr::str_interp(
      "`cases` must be an integer number, not ${cases}"
    )
    stop(err_string)
  }
  
  #check N
  if(is.null(n) | is.na(n)) {
    err_string <- stringr::str_interp(
      "`n` must be an integer number, not NULL"
    )
    stop(err_string)
  }
  if(! is.numeric(n)) {
    err_string <- stringr::str_interp(
      "`n` must be numeric not ${class(n)}"
    )
    stop(err_string)
  }
  if(n != as.integer(n)) {
    err_string <- stringr::str_interp(
      "`n` must be an integer number, not ${n}"
    )
    stop(err_string)
  }
  if(n <1) {
    err_string <- stringr::str_interp(
      "`n` must be > 0"
    )
    stop(err_string)
  }
  
  #Check that cases<N
  if(cases>n) {
    err_string <- stringr::str_interp(
      "`cases` must be less than n.  Currently cases is ${cases} and n is ${n}."
    )
    stop(err_string)
  }
  
  
  P= cases/n
  q = 1-P
  z = qnorm(1-(1-conf.level)/2)
  
  A = 2*cases+z^2
  B = z*sqrt(z^2+(4*cases*q))
  C = 2*(n+z^2)
  
  ci.low = (A-B) / C
  ci.high = (A+B) / C
  
  
  summary_data <- data.frame(
    cases = cases,
    unaffected = n-cases,
    total = n,
    P = P,
    P.ci.low = ci.low,
    P.ci.high = ci.high
  )
  
  
  names(summary_data)[1] <- caselabels[1]
  names(summary_data)[2] <- caselabels[2]
  names(summary_data)[4] <- paste("P.", caselabels[1], sep="")
  
  #Now generate data for cat's eye plot
  diffdata <-  data.frame(z = rnorm(10000), sign = rep(1, 10000))
  diffdata[diffdata$z < 0, ]$sign = -1
  diffdata$z = abs(diffdata$z)
  diffdata$A <- 2*cases+diffdata$z^2
  diffdata$B <- diffdata$z*sqrt(diffdata$z^2+(4*cases*q))
  diffdata$C <- 2*(n+diffdata$z^2)
  diffdata$low <- (diffdata$A + diffdata$B) / diffdata$C
  diffdata$high <- (diffdata$A + diffdata$B) / diffdata$C
  diffdata$B <- diffdata$B * diffdata$sign
  diffdata$error <- (diffdata$A + diffdata$B) / diffdata$C
  
  
  
  #Store plot_info
  plot_info <- list(
    plotiv = "Variable",
    plotdv = paste("P.", caselabels[1], sep=""),
    plot_data = summary_data,
    diffdata = diffdata
  )
  
  
  res = list(
    summary_data = summary_data,
    P = P, 
    ci.low = ci.low, 
    ci.high = ci.high,
    plot_info = plot_info)
  class(res) <- "estimate"
  return(res)
}


estimateProportion.default <- function(data, x, conf.level=0.95, case.level = 1, na.rm = FALSE) {
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
  # Check data has more than 2 rows
  if(nrow(data)<3) {
    err_string <- stringr::str_interp(
      "`data` must have more than 2 rows, not ${nrow(data)}"
    )
    stop(err_string)
  }
  # Check that x column exists
  if(x_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${x_quoname}"
    )
    stop(err_string)
  }
  # Check that x is a factor
  if(!is.factor(data[[x_quoname]])) {
    err_string <- stringr::str_interp(
      "'x' (${x_quoname}) must be a factor, not ${class(data[[x_quoname]])}.  Try recasting this column with as.factor"
    )
    stop(err_string)
  }
  # Check if iv has at least two levels
  if(length(levels(data[[x_quoname]])) < 2) {
    err_string <- stringr::str_interp(
      "x (${x_quoname}) must have at least 2 levels, not ${length(levels(data[[x_quoname]]))}."
    )
    stop(err_string)
  }
  # Deal with characters passed in as case.level
  if(class(case.level) == "character") {
    i <- 1
    while (i <= length(levels(data[[x_quoname]]))) {
      if (levels(data[[x_quoname]])[[i]] == case.level) {
        case.level <- i
      }
      i <- i + 1
    }
    if(class(case.level) == "character") {
      err_string <- stringr::str_interp(
        "case.level is ${case.level} but this level is not in levels of x (${x_quoname}), which hase levels ${levels(data[[x_quoname]])}."
      )
      stop(err_string)
    }
  }
  # Make sure case.level is not > number of levels
  if(case.level > length(levels(data[[x_quoname]])) ) {
    err_string <- stringr::str_interp(
      "case.level is set to ${case.level} but must be less than or equal to the number of levels in ${x_quoname}, which is ${length(levels(data[[x_quoname]]))}."
    )
    strop(err_string)
  }
  
  # Data cleanup ---------------------------
  # Make duplicate copies that can be addressed using $ notation..cause I like it?
  data$iv <- data[[x_quoname]]
  
  # Reduce down to only the needed column
  keeps <- c("iv")
  data <- data[keeps]  
  
  #Count NAs
  na_count <- nrow(data) - nrow(na.omit(data))
  
  
  # Now remove NAs from data
  if (na.rm) {
    data <- na.omit(data)
  }
  
  n <- nrow(data)
  
  all_result <- data.frame(level = NULL, cases = NULL, total = NULL, P = NULL, ci.low = NULL, ci.high = NULL)
  for (i in levels(data$iv)) {
    cases <- length(na.omit(data[data$iv == i, ]))
    level1 <- i
    level2 <- paste("not", i)
    tout <- estimateProportion(cases = cases, n = n, caselabels = c(level1, level2), conf.level = conf.level)
    all_result <- rbind(all_result, data.frame(level = i, cases = cases, total = n, P = tout$P, ci.low = tout$ci.low, ci.high = tout$ci.high))
  }

  cases <- length(data[data$iv == levels(data$iv)[case.level], ])
  level1 <- levels(data$iv)[[case.level]]
  if (length(levels(data$iv)) == 2) {
    if (case.level == 1) {
      level2 <- levels(data$iv)[[2]]
    } else {
      level2 <- levels(data$iv)[[1]]
    }
  } else {
    level2 <- paste("not", levels(data$iv)[[case.level]])
  }
  caselabels = c(level1, level2)
  
  # Calculations ---------------------------
  # Pass off to the summary-data version of this function
  out <- estimateProportion(cases = cases, n = n, caselabels = caselabels, conf.level = conf.level)
  # Save variable name into plotinfo
  out$plot_info$plotiv <- x_quoname 
  
  #Kludge - fix this
  out$all_result <- all_result
  
  
  #Report on any NAs
  report <- list()
  
  if (na_count > 0) {
    summary_notes <- paste("Missing responses = ", na_count, ".\n")
    if (na.rm) {
      summary_notes <- paste(summary_notes, "The total N did *not* include these missing responses (na.rm was TRUE)\n")
    } else {
      summary_notes <- paste(summary_notes, "The total N *did* include these missing responses (na.rm was FALSE)\n")
    }
  } else {
    summary_notes <- "There were no missing responses.\n"
  }

  report$Notes <- list(
    title = "\n---Notes---\n",
    summary = summary_notes
  )
  
  out$report <- report
  
  return(out)
}

estimateProportionDifference.numeric <- function(cases1, n1, cases2, n2, caselabels = c("Affected", "Not Affected"), grouplabels = c("Group 1", "Group 2"), conf.level=0.95) {
  #Function to compare proportions from two groups using summary data only
  
  # Validate inputs ---------------------------
  #check Ns
  if(is.null(n1) | is.na(n1)) {
    err_string <- stringr::str_interp(
      "`n1` must be an integer number, not NULL"
    )
    stop(err_string)
  }
  if(! is.numeric(n1)) {
    err_string <- stringr::str_interp(
      "`n1` must be numeric not ${class(n1)}"
    )
    stop(err_string)
  }
  if(n1 != as.integer(n1)) {
    err_string <- stringr::str_interp(
      "`n1` must be an integer number, not ${n1}"
    )
    stop(err_string)
  }
  if(n1 <1) {
    err_string <- stringr::str_interp(
      "`n1` must be > 0"
    )
    stop(err_string)
  }
  
  #Check that cases<N
  if(cases1>n1) {
    err_string <- stringr::str_interp(
      "`cases1` must be less than n1.  Currently cases is ${cases1} and n is ${n1}."
    )
    stop(err_string)
  }
  
  if(is.null(n2) | is.na(n2)) {
    err_string <- stringr::str_interp(
      "`n2` must be an integer number, not NULL or NA"
    )
    stop(err_string)
  }
  if(! is.numeric(n2)) {
    err_string <- stringr::str_interp(
      "`n2` must be numeric not ${class(n2)}"
    )
    stop(err_string)
  }
  if(n2 != as.integer(n2)) {
    err_string <- stringr::str_interp(
      "`n2` must be an integer number, not ${n2}"
    )
    stop(err_string)
  }
  if(n2 <1) {
    err_string <- stringr::str_interp(
      "`n2` must be > 0"
    )
    stop(err_string)
  }
  
  #Check that cases<N
  if(cases2>n2) {
    err_string <- stringr::str_interp(
      "`cases2` must be less than n2.  Currently cases is ${cases2} and n is ${n2}."
    )
    stop(err_string)
  }
  
  
  # Check labels
  if (is.null(grouplabels)) {
    err_string <- stringr::str_interp(
      "`grouplabels` must either be omitted or not null"
    )
    stop(err_string)
  } else {
    if(length(grouplabels) < 2) {
      err_string <- stringr::str_interp(
        "`grouplabels` must be a vector with two elements.  Currently class is ${class(grouplabels)} of length ${length(grouplabels)}"
      )
      stop(err_string)
    } else {
      if(grouplabels[1] == grouplabels[2]) {
        err_string <- stringr::str_interp(
          "grouplabels must have elemnts 1 and 2 be distinct.  Currently element 1 is ${grouplabels[1]} and element 2 is also ${grouplabels[2]}."
        )
        grouplabels <- c("Group 1", "Group 2")
        warning(err_string)
      }
    }
  }
  
  # Check case labels
  if (is.null(caselabels)) {
    err_string <- stringr::str_interp(
      "`caselabels` must either be omitted or not null"
    )
    stop(err_string)
  } else {
    if(length(caselabels) < 2) {
      err_string <- stringr::str_interp(
        "`caselabels` must be a vector with two elements.  Currently class is ${class(caselabels)} of length ${length(caselabels)}"
      )
      stop(err_string)
    } else {
      if(caselabels[1] == caselabels[2]) {
        err_string <- stringr::str_interp(
          "caselabels must have elemtns 1 and 2 be distinct.  Currently element 1 is ${caselabels[1]} and element 2 is also ${caselabels[2]}."
        )
        caselabels <- c("Affected", "Not Affected")
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

  
  # Calculations ---------------------------
  P1 = cases1/n1
  P2 = cases2/n2
  q1 = 1-P1
  q2 = 1-P2
  
  P1_res = estimateProportion(cases1, n1, conf.level = conf.level)
  P2_res = estimateProportion(cases2, n2, conf.level = conf.level)
  
  P1.ci.low = P1_res$ci.low
  P1.ci.high = P1_res$ci.high
  
  P2.ci.low = P2_res$ci.low
  P2.ci.high = P2_res$ci.high
  
  
  Pdiff = P1 - P2
  
  ci.low = Pdiff - sqrt( (P1-P1.ci.low)^2 + (P2.ci.high - P2)^2  )
  ci.high = Pdiff + sqrt( (P2 - P2.ci.low)^2 + (P1.ci.high - P1)^2 )
  

  
  # Prep Ouput ---------------------------
  summary_data <- rbind(P1_res$summary_data, P2_res$summary_data)
  summary_data[nrow(summary_data) + 1,] = list(NA, NA, NA, Pdiff, ci.low, ci.high)
  summary_data <- cbind(data.frame(Group = c(grouplabels, "Difference")), summary_data)
  
  names(summary_data)[2] <- caselabels[1]
  names(summary_data)[3] <- caselabels[2]
  names(summary_data)[5] <- paste("P.", caselabels[1], sep="")
  
  # Make formatted verion of mdiff
  formatted_pdiff <- stringr::str_interp(
    "Pdiff = $[.3f]{Pdiff} ${conf.level*100}% CI [$[.3f]{ci.low}, $[.3f]{ci.high}]"
  )

  # Create error data for cats' eye plot
  diffdata <- data.frame(Group = rep("Difference", 10000))
  diffdata$low <- Pdiff - sqrt( (P1 - P1_res$plot_info$diffdata$low)^2 + (P2_res$plot_info$diffdata$high - P2)^2 )
  diffdata$high <- Pdiff + sqrt( (P2 - P2_res$plot_info$diffdata$low)^2 + (P1_res$plot_info$diffdata$high - P1)^2  ) 
  diffdata <- rbind(diffdata, data.frame(Group = diffdata$Group, low = diffdata$high, high = diffdata$low))
  diffdata$differror <- diffdata$low
  
  #Prep plot info
  
  plot_info <- list(
    plotiv = "group",
    plotdv = names(summary_data)[5],
    plot_data = summary_data,
    diffdata = diffdata
  )
  
  # Gather output in a list
  out <- list(
    conf.level = conf.level,
    na.rm = FALSE,
    na_count = 0,
    summary_data = summary_data,
    level1 = grouplabels[1],
    level2 = grouplabels[2],
    cases1 = cases1,
    cases2 = cases2,
    n1 = n1,
    n2 = n2,
    P1 = P1,
    P1.ci.low = P1.ci.low,
    P1.ci.high = P1.ci.high,
    P2 = P2,
    P2.ci.low = P2.ci.low,
    P2.ci.high = P2.ci.high,
    Pdiff = Pdiff,
    ci.low = ci.low,
    ci.high = ci.high,
    formatted_pdiff = formatted_pdiff,
    plot_info = plot_info,
    type = "fromSummary"
  )
  
  # Set class as estimate for printing purposes
  class(out) <- "estimate"
  return(out)
}

estimateProportionDifference.default <- function(data, x, y, conf.level=0.95, case.level = 1, group.level = 1, na.rm = FALSE) {
  
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
  # Check data has more than 2 rows
  if(nrow(data)<3) {
    err_string <- stringr::str_interp(
      "`data` must have more than 2 rows, not ${nrow(data)}"
    )
    stop(err_string)
  }
  # Check that x column exists
  if(x_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${x_quoname}"
    )
    stop(err_string)
  }
  # Check that y column exists
  if(y_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${y_quoname}"
    )
    stop(err_string)
  }
  
  # Check that x is a factor
  if(!is.factor(data[[x_quoname]])) {
    err_string <- stringr::str_interp(
      "'x' (${x_quoname}) must be a factor, not ${class(data[[x_quoname]])}.  Try recasting as.factor"
    )
    stop(err_string)
  }
  # Check if x has at least two levels
  if(length(levels(data[[x_quoname]])) < 2) {
    err_string <- stringr::str_interp(
      "x (${x_quoname}) must have at least 2 levels, not ${length(levels(data[[x_quoname]]))}."
    )
    stop(err_string)
  }
  # Check that y is a factor
  if(!is.factor(data[[y_quoname]])) {
    err_string <- stringr::str_interp(
      "'y' (${y_quoname}) must be a factor, not ${class(data[[y_quoname]])}.  Try re-casting with as.factor"
    )
    stop(err_string)
  }
  # Check if y has at least two levels
  if(length(levels(data[[y_quoname]])) < 2) {
    err_string <- stringr::str_interp(
      "y (${y_quoname}) must have at least 2 levels, not ${length(levels(data[[y_quoname]]))}."
    )
    stop(err_string)
  }
  
  #Deal with characters passed in as case.level
  if(class(case.level) == "character") {
    i <- 1
    while (i <= length(levels(data[[y_quoname]]))) {
      if (levels(data[[y_quoname]])[[i]] == case.level) {
        case.level <- i
      }
      i <- i + 1
    }
    if(class(case.level) == "character") {
      err_string <- stringr::str_interp(
        "case.level is ${case.level} but this level is not in levels of y (${y_quoname}), which hase levels ${levels(data[[y_quoname]])}."
      )
      stop(err_string)
    }
  }

  #Deal with characters passed in as group.level
  if(class(group.level) == "character") {
    i <- 1
    while (i <= length(levels(data[[x_quoname]]))) {
      if (levels(data[[x_quoname]])[[i]] == group.level) {
        group.level <- i
      }
      i <- i + 1
    }
    if(class(group.level) == "character") {
      err_string <- stringr::str_interp(
        "group.level is ${group.level} but this level is not in levels of x (${x_quoname}), which hase levels ${levels(data[[x_quoname]])}."
      )
      stop(err_string)
    }
  }
  
  
    
  # Check if group.lvel is not > than x levels 
  if(group.level > length(levels(data[[x_quoname]]))) {
    err_string <- stringr::str_interp(
      "group.level is ${group.level} but must be equal or less than the number of levels in x (${x_quoname}), which is ${length(levels(data[[x_quoname]]))}."
    )
    stop(err_string)
  }
  # Check if case.evel < y levels 
  if(case.level > length(levels(data[[y_quoname]]))) {
    err_string <- stringr::str_interp(
      "case.level is ${case.level} but must less than the levels in y (${y_quoname}) which is ${length(levels(data[[y_quoname]]))}."
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
  
  na_count <- nrow(data) - nrow(na.omit(data))
  na_group <- nrow(data) - nrow(na.omit(data["iv"]))
  na_case <- na_count - na_group
  
  # Now remove NAs from data
  if (na.rm) {
    data <- na.omit(data)
  }
  
  
  # Now set group names
  level1 = levels(data$iv)[group.level]
  if (length(levels(data$iv)) == 2 & na_group == 0) {
    if(group.level == 2) {
      level2 = levels(data$iv)[1]
    } else {
      level2 = levels(data$iv)[2]
    }
  } else {
    level2 = paste("not", levels(data$iv)[group.level])
  }
  
  # Finally, split by group and last check ---------------------------
  
  # Now split by group
  group1 <- data[data$iv == level1, ]
  group2 <- data[data$iv != level1, ]
  
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
  n2 = nrow(group2)
  n1 = nrow(data) - n2
  cases1 = nrow(group1[group1$dv == levels(group1$dv)[case.level], ])
  cases2 = nrow(group2[group2$dv == levels(group2$dv)[case.level], ])

  caselabel1 <- levels(group1$dv)[case.level]
  if(length(levels(group1$dv)) == 2 & na_case == 0) {
    if(case.level == 1) {
      caselabel2 <- levels(group1$dv)[2]
    } else {
      caselabel2 <- levels(group1$dv)[1]
    }
  } else {
    caselabel2 <- paste("not", levels(group1$dv)[case.level])
  }
  
  caselabels <- c(caselabel1, caselabel2)
  
  # Pass off to the summary-data version of this function
  out <- estimateProportionDifference.numeric(cases1 = cases1, cases2 = cases2, n1 = n1, n2 = n2, grouplabels = c(level1, level2), caselabels = caselabels, conf.level = conf.level)
  
  #Last bits
  # Save variable name into plotinfo
  out$plot_info$plotiv <- x_quoname 
  
  #Report on any NAs
  report <- list()
  
  if (na_count > 0) {
    summary_notes <- paste("There were", na_group, "missing responses in the grouping variable (x) and", na_case, "missing responses in the outcome variable (y).\n")
    if (na.rm) {
      summary_notes <- paste(summary_notes, "Totals do *not* include these missing responses (na.rm was TRUE)\n")
    } else {
      summary_notes <- paste(summary_notes, "Totals *do* include these missing responses (na.rm was FALSE)\n")
    }
  } else {
    summary_notes <- "There were no missing responses.\n"
  }
  
  report$Notes <- list(
    title = "\n---Notes---\n",
    summary = summary_notes
  )
  out$na_count <- na_count
  out$na.rm <- na.rm
  out$report <- report
  
  return(out)
}



testthis <- function() {
  mydata <- data.frame(gender = c(rep("male", 100), rep("female", 100), rep("no response", 50)))
  mydata$diagnosis <- as.factor(c(rep("depressed", 10), rep("normal", 90), rep("depressed", 30), rep("normal", 70), rep("normal", 30), rep("depressed", 10), rep("depressed", 10)))
  estimate <- estimateProportionDifference(mydata, gender, diagnosis, group.level = "female")  
  plotEstimatedProportionDifference(estimate)
}
