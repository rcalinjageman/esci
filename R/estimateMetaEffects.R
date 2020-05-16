
scaleFUN <- function(x) sprintf("%.2f", x)

estimateOverallRaw <- function(data, m1, m2, s1, s2, n1, n2, label = NULL, moderator = NULL, random.effects = TRUE, report.cohens.d = FALSE, conf.level = 0.95) {

  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  m1_enquo        <-  rlang::enquo(m1)
  m1_quoname      <-  rlang::quo_name(m1_enquo)
  
  m2_enquo        <-  rlang::enquo(m2)
  m2_quoname      <-  rlang::quo_name(m2_enquo)
  
  s1_enquo        <-  rlang::enquo(s1)
  s1_quoname      <-  rlang::quo_name(s1_enquo)
  
  s2_enquo        <-  rlang::enquo(s2)
  s2_quoname      <-  rlang::quo_name(s2_enquo)
  
  n1_enquo        <-  rlang::enquo(n1)
  n1_quoname      <-  rlang::quo_name(n1_enquo)
  
  n2_enquo        <-  rlang::enquo(n2)
  n2_quoname      <-  rlang::quo_name(n2_enquo)
  
  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  
  label_enquo        <-  rlang::enquo(label)
  label_quoname      <-  rlang::quo_name(label_enquo)
  
  moderator <- FALSE
  if(!is.null(data[[moderator_quoname]])) {
    data$moderator         <- data[[moderator_quoname]]    
    moderator <- TRUE
  }
  
  if(!is.null(data[[label_quoname]])) {
    data$label <- as.character(data[[label_quoname]])
  } else {
    data$label <- paste("Study", seq(1:nrow(data)))
  }
  
  
  # Kludgey - but make column names to pass along to escalc
  data$m1 <- data[[m1_quoname]]
  data$m2 <- data[[m2_quoname]]
  data$s1 <- data[[s1_quoname]]
  data$s2 <- data[[s2_quoname]]
  data$n1 <- data[[n1_quoname]]
  data$n2 <- data[[n2_quoname]]
  
  # Reduce down to only the iv and dv columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("label", "m1", "s1", "n1", "m2", "s2", "n2")
  if(!is.null(data$moderator)) {keeps <- c(keeps, "moderator")}
  data <- data[keeps]  
  

  # Use escalc to obtain yi and vi for each study in raw scores
  if (report.cohens.d) {
    measure = "SMD"
    effect.size.name = "Cohen's d"
  } else {
    measure = "MD"    
    effect.size.name = "Mdiff"
  }
  data <- metafor::escalc(measure = measure, data = data, m1i = m2, m2i = m1, sd1i = s2, sd2i = s1, n1i = n2, n2i = n1)
  
  
  # Now store CI upper and lower bounds
  b <- summary(data)
  data$ci.low <- b$ci.lb
  data$ci.high <-b$ci.ub
  

  res <- estimateMetaEffect(data = data, moderator = moderator, effect.size.name = effect.size.name, random.effects = random.effects, conf.level = conf.level)
  
  return(res)
}

estimateOverallSMD <- function(data, d, n1, n2, label = NULL, moderator = NULL, random.effects = TRUE, conf.level = 0.95, correct.for.bias = TRUE) {
  
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  d_enquo        <-  rlang::enquo(d)
  d_quoname      <-  rlang::quo_name(d_enquo)

  n1_enquo        <-  rlang::enquo(n1)
  n1_quoname      <-  rlang::quo_name(n1_enquo)
  
  n2_enquo        <-  rlang::enquo(n2)
  n2_quoname      <-  rlang::quo_name(n2_enquo)
  
  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  
  label_enquo        <-  rlang::enquo(label)
  label_quoname      <-  rlang::quo_name(label_enquo)
  
  moderator <- FALSE
  if(!is.null(data[[moderator_quoname]])) {
    data$moderator         <- data[[moderator_quoname]]    
    moderator <- TRUE
  }
  
  if(!is.null(data[[label_quoname]])) {
    data$label <- as.character(data[[label_quoname]])
  } else {
    data$label <- paste("Study", seq(1:nrow(data)))
  }
  
  
  # Kludgey - but make column names to pass along to escalc
  data$d <- data[[d_quoname]]
  data$n1 <- data[[n1_quoname]]
  data$n2 <- data[[n2_quoname]]
  
  # Reduce down to only the iv and dv columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("label", "d", "n1", "n2")
  if(!is.null(data$moderator)) {keeps <- c(keeps, "moderator")}
  data <- data[keeps]  

  effect.size.name = "Cohen's d"
  
  
  # Correct d for bias
  if (correct.for.bias) {
    data$yi <- (1 - 3/(4*(data$n1+data$n2-2) - 1)) * data$d
  } else {
    data$yi <- data$d
  }
  
  # Calculate vi
  data$vi <- 1/data$n1+ 1/data$n2 + data$yi^2/(2*(data$n1+data$n2))
  
  class(data) <- c("escalc", "data.frame")
  
  # Now store CI upper and lower bounds
  b <- summary(data)
  data$ci.low <- b$ci.lb
  data$ci.high <-b$ci.ub
  
  
  res <- estimateMetaEffect(data = data, moderator = moderator, effect.size.name = effect.size.name, random.effects = random.effects, conf.level = conf.level)
  
  
  return(res)
}


estimateOverallCorrelation <- function(data, rvalues, ns, label = NULL, moderator = NULL, random.effects = TRUE, conf.level = 0.95) {
  
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  rvalues_enquo        <-  rlang::enquo(rvalues)
  rvalues_quoname      <-  rlang::quo_name(rvalues_enquo)
  
  ns_enquo        <-  rlang::enquo(ns)
  ns_quoname      <-  rlang::quo_name(ns_enquo)
  
  moderator_enquo        <-  rlang::enquo(moderator)
  moderator_quoname      <-  rlang::quo_name(moderator_enquo)
  
  
  label_enquo        <-  rlang::enquo(label)
  label_quoname      <-  rlang::quo_name(label_enquo)
  
  moderator <- FALSE
  if(!is.null(data[[moderator_quoname]])) {
    data$moderator         <- data[[moderator_quoname]]
    moderator <- TRUE
  }
  
  if(!is.null(data[[label_quoname]])) {
    data$label <- as.character(data[[label_quoname]])
  } else {
    data$label <- paste("Study", seq(1:nrow(data)))
  }
  
  
  # Kludgey - but make column names to pass along to escalc
  data$r <- data[[rvalues_quoname]]
  data$n <- data[[ns_quoname]]


  # Reduce down to only the needed columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("label", "r", "n")
  if(!is.null(data$moderator)) {keeps <- c(keeps, "moderator")}
  data <- data[keeps]  
  
  
  # Use escalc to obtain yi and vi for each study in raw scores
  measure = "COR"
  effect.size.name = "Pearson's r"
  data <- metafor::escalc(measure = measure, data = data, ri = r, ni = n)
  
  
  # Now store CI upper and lower bounds
  b <- summary(data)
  data$ci.low <- b$ci.lb
  data$ci.high <-b$ci.ub
  
  
  res <- estimateMetaEffect(data = data, moderator = moderator, effect.size.name = effect.size.name, random.effects = random.effects, conf.level = conf.level)

  
  return(res)
}


estimateMetaEffect <- function(data, moderator = FALSE, effect.size.name = "MDiff", random.effects = TRUE, conf.level = 0.95) {
  # We've passed in a prepared data table with columns:
    # Label, ci.low, ci.high, yi, vi, moderator and any other columns relevant to that meta-analysis

  # Now do both fixed and random effects overall meta-analaysis and store in tables  
  FE <- metafor::rma(data = data, yi = yi, vi = vi, method="FE")
  RE <- metafor::rma(data = data, yi = yi, vi = vi, method="DL")
  
  # Calculate diamond.ratio
  dr.res <- dr.CI(RE, FE, data$vi, conf.level = conf.level)
  
  diamond.ratio <- dr.res$diamond.ratio
  dr.low <- dr.res$dr.low
  dr.high <- dr.res$dr.high
  

  # Extract results table from each    
  FEtbl <- meta_to_table(FE, fixed.effects = TRUE, dr.res, label = "Overall")
  REtbl <- meta_to_table(RE, fixed.effects = FALSE, dr.res, label = "Overall")
  
  # Ok - this gets a bit complicated
  if(moderator) {
    # WIth -1, we estimate with no intercept, so the parameters reflect each moderator level effect on its own
    FEgroups <- metafor::rma(data = data, yi = yi, vi = vi, mods = ~ moderator -1, method="FE")
    REgroups <- metafor::rma(data = data, yi = yi, vi = vi, mods = ~ moderator -1, method="DL")
    # With out subtracting one, we get model params the reflec the difference from the first level to each next level
    FEdiffs <- metafor::rma(data = data, yi = yi, vi = vi, mods = ~ moderator, method="FE")
    REdiffs <- metafor::rma(data = data, yi = yi, vi = vi, mods = ~ moderator, method="DL")
  
    
    
    # Save tables for each group-based analysis  
    FEgtable <- meta_to_table(FEgroups, fixed.effects = TRUE)
    REgtable <- meta_to_table(REgroups, fixed.effects = FALSE)
    
    # Getting really crazy now--the group-based analyses don't give us I2 for each level, so we run an additional RE analysis for each level
    #   and save I2 and its CI for each of these.
    x <- 0
    replabels <- NULL
    for(lev in levels(data$moderator)) {
      x <- x + 1
      REjustl <- metafor::rma(data = data[data$moderator == lev, ], yi = yi, vi = vi, method = "DL")
      FEjustl <- metafor::rma(data = data[data$moderator == lev, ], yi = yi, vi = vi, method = "FE")
      dr.res <- dr.CI(REjustl, FEjustl, data[data$moderator == lev, ]$vi, conf.level = conf.level)
      REjustltbl <- meta_to_table(REjustl, fixed.effects = FALSE, dr.res)
      replabels <- c(replabels, lev)
      REgtable[x, "I2"] <- REjustltbl$I2[1]
      REgtable[x, "I2.low"] <- REjustltbl$I2.low[1]
      REgtable[x, "I2.high"] <- REjustltbl$I2.high[1]
      REgtable[x, "diamond.ratio"] <- dr.res$diamond.ratio
      REgtable[x, "dr.low"] <- dr.res$dr.low
      REgtable[x, "dr.high"] <- dr.res$dr.high
      FEgtable[x, "diamond.ratio"] <- dr.res$diamond.ratio
      FEgtable[x, "dr.low"] <- dr.res$dr.low
      FEgtable[x, "dr.high"] <- dr.res$dr.high
    }
    REgtable$label <- NULL
    REgtable <- cbind(data.frame(label = replabels), REgtable)


    # Now we extract the results from the difference approaches and save only the last row of params
    FEdiffstbl <- meta_to_table(FEdiffs, fixed.effects = TRUE, label = "Difference")
    REdiffstbl <- meta_to_table(REdiffs, fixed.effects = FALSE, label = "Difference")

    FEtbl <- rbind(FEtbl, FEgtable, FEdiffstbl[nrow(FEdiffstbl), ])
    REtbl <- rbind(REtbl, REgtable, REdiffstbl[nrow(REdiffstbl), ])
    
  }
  
  
  # Select which meta-analysis to report
  if(random.effects) {
    result_table <- REtbl
    report_from <- RE
  }  else {
    result_table <- FEtbl
    report_from <- FE
  }
  
  
  if(moderator) {
    # The diamond ratio of the difference estimate doesn't really make sense; clear it
    result_table[nrow(result_table), "I2"] <- NA
    result_table[nrow(result_table), "I2.low"] <- NA
    result_table[nrow(result_table), "I2.high"] <- NA
  }
  
  # Store away results to report as list properties
  effect.size <- result_table$effect.size[1]
  ci.low <- result_table$ci.low[1]
  ci.high <- result_table$ci.high[1]
  p.value <- result_table$p.value[1]
  I2 <- result_table$I2[1]
  I2.low <- result_table$I2.low[1]
  I2.high <- result_table$I2.high[1]
  
  # Fix up data just a bit
  data$sample_variance <- data$vi
  data$vi <- NULL
  data$weights <- (metafor::weights.rma.uni(report_from))
  names(data)[names(data) == "yi"] <- "effect.size"

  res <- list(
    result_table = result_table,
    effect.size = effect.size,
    effect.size.name = effect.size.name,
    ci.low = ci.low,
    ci.high = ci.high,
    diamond.ratio = diamond.ratio,
    dr.low = dr.low,
    dr.high = dr.high,
    I2 = I2,
    I2.low = I2.low,
    I2.high = I2.high,
    p.value = p.value,
    fixed_effect = FE,
    random_effect = RE,
    data = data
  )
  
  return(res)
}


plotMetaEffect <- function(estimate, xlims = c(NULL, NULL), dr.explain = FALSE) {
  # Store copies of the data  
  if(!is.null(estimate$data$moderator)) {dr.explain <- FALSE}
  
  plot_data <- estimate$data
  summary_data <- estimate$result_table
  if(dr.explain) {
    summary_data <- rbind(meta_to_table(estimate$random_effect, fixed.effects = FALSE, label = c("Random Effect")),
                          meta_to_table(estimate$fixed_effect, fixed.effects = TRUE, label = c("Fixed Effect"))
                          )
  }
  
  summary_data$label <- as.character(summary_data$label)
  
  
  if(is.null(xlims) & estimate$effect.size.name == "Pearson's r") {
    xlims <- c(-1, 1)
  }
  
  # Sort out if we are plotting a difference axis
  plotting.difference.axis <- FALSE
  if (!is.null(plot_data$moderator)) {
    if (nrow(summary_data) == 4) {
      # Yes, there is a moderator with exactly 2 levels, so plot the difference
      plotting.difference.axis <- TRUE
    } else {
      # There is a moderator, but more than 2 levels, so take out the difference estimate as it doesn't mean anything
      summary_data <- summary_data[1:nrow(summary_data)-1, ]
    }
  }
  

  #######################
  # y-axis stuff
  # Get number of studies and number of summaries
  studies <- nrow(plot_data)
  summaries <- nrow(summary_data)
  

  endpoint <- 1
  if (plotting.difference.axis) {
    endpoint <- -1  
  }

  # Create y axis data to order studies and summaries
  plot_data$y <- seq(from = studies+summaries+1, to = summaries+2, by = -1)
  summary_data$y <- seq(from = summaries, to = 1, by = -1)
  if(plotting.difference.axis) {
    summary_data[nrow(summary_data), "y"] <- endpoint
  }
  
  # Make ylabel data
  yseq <- seq(from = studies+summaries+1, to = endpoint, by = -1)
  ylabels <- c(plot_data$label, " ", summary_data$label)
  if(plotting.difference.axis) {
    ylabels[length(ylabels)] <- " "
    endlabel <- paste("Difference\n", summary_data[nrow(summary_data)-1, "label"], " - ", summary_data[nrow(summary_data)-2, "label"], sep="")
    ylabels <- c(ylabels, " ", endlabel)
  }

  #######################
  # effect size polygons
  # each polygon has 4 points
  # left point is ci.low at y, right point is ci.high at y
  # middle points are effect.size at y-0.5 and y+0.5
  # Could probably make the height scalable
  # Cycle through each effect size estimate and create add the 4 xy cords to the vector
  # And also add a grouping variable that shows which points belong together
  xs <- NULL
  ys <- NULL
  gs <- NULL
  ms <- NULL
  for (row in 1:nrow(summary_data)) {
    if(summary_data[row, "label"] != "Difference") {
      xs <- c(xs, summary_data[row, "ci.low"], summary_data[row, "effect.size"], summary_data[row, "ci.high"], summary_data[row, "effect.size"])
      ys <- c(ys, summary_data[row, "y"], summary_data[row, "y"]-0.4, summary_data[row, "y"], summary_data[row, "y"] + 0.4)
      gs <- c(gs, rep(paste("P", row, sep = ""), 4))
      ms <- c(ms, rep(summary_data[row, "label"], 4))
    }
  }
  
  poly_data <- data.frame(x = xs, y = ys, group = gs, moderator = ms)
  

  ###################### 
  # Now build up plot
  # Basic plot
  myplot <- ggplot(data = plot_data, aes(y = y, x = effect.size))
  # Add points for effect size for each study, error bars for CIs, and polygons for summaries
  if (is.null(plot_data$moderator)) {
    # No moderator variable so don't define it as a color
    myplot <- myplot + geom_point(aes(size = weights), shape = 22, fill="black")
    myplot <- myplot + geom_errorbarh(aes(xmin = ci.low, xmax = ci.high), height = 0)
    myplot <- myplot + geom_polygon(data = poly_data, aes(x = x, y = y, group = group))
  } else {
    # Moderator variable so define it as a color
    myplot <- myplot + geom_point(aes(fill = moderator, size = weights), shape = 22)
    myplot <- myplot + geom_errorbarh(aes(xmin = ci.low, xmax = ci.high, color = moderator), height = 0)
    myplot <- myplot + geom_polygon(data = poly_data, aes(x = x, y = y, group = group, color = moderator, fill = moderator))
  }
  # Plot overall effect size again in black
  myplot <- myplot + geom_polygon(data = poly_data[poly_data$moderator == "Overall", ], aes(x = x, y = y, group = group), color = "Black", fill = "Black")
  
  
  ###############
  # Difference axis 
  if (plotting.difference.axis) {
    # We will offset the difference relative to the reference group
    offset <- summary_data[2, "effect.size"]
    summary_data[nrow(summary_data), "effect.size"] <- summary_data[nrow(summary_data), "effect.size"] + offset
    summary_data[nrow(summary_data), "ci.low"] <- summary_data[nrow(summary_data), "ci.low"] + offset
    summary_data[nrow(summary_data), "ci.high"] <- summary_data[nrow(summary_data), "ci.high"] + offset
    
    # Plot the difference
    myplot <- myplot + geom_point(data = summary_data[summary_data$label == "Difference", ], aes(x = effect.size, y = y), shape = 17, size= 3)
    myplot <- myplot + geom_errorbarh(data = summary_data[summary_data$label == "Difference", ], aes(xmin = ci.low, xmax = ci.high), height = 0)
    
    #Calculate boundaries for secondary axis
    astart <- min(0, summary_data$ci.low-offset)
    aend <-  max(0, summary_data$ci.high-offset)
    goby <- (aend-astart)/6
    astart <- 0 - (ceiling((0-astart)/goby)*goby)
    saxisBreaks <- seq(from = astart, to = aend, by = goby)
    
    
    #Set the secondary axis
    myplot <- myplot + scale_x_continuous(position = "top", limits = xlims, sec.axis = sec_axis(trans = ~.-offset, breaks = saxisBreaks, labels = scaleFUN))
    myplot <- myplot + geom_segment(linetype = "solid", color = "black", aes(x = astart+offset, xend = aend+offset, y = -2, yend = -2))
    
    #Draw lines connecting estimates to secondary axis
    myplot <- myplot + geom_segment(linetype = "dashed", color = "black", aes(x = summary_data[3, "effect.size"], xend = summary_data[3, "effect.size"], y = summary_data[3, "y"], yend = summary_data[4, "y"]))
    myplot <- myplot + geom_segment(linetype = "dashed", color = "black", aes(x = summary_data[2, "effect.size"], xend = summary_data[2, "effect.size"], y = summary_data[2, "y"], yend = -2))
  } else {
    myplot <- myplot + scale_x_continuous(position = "top", limits = xlims)
  }
  
  
  #################
  # Finishing up
  # Label x and y axis  
  myplot <- myplot + ylab("Study") + xlab(estimate$effect.size.name)
  myplot <- myplot + scale_y_continuous(expand = c(0, 0), breaks = yseq, labels = ylabels)
  
  
    
  # Reference line
  refstart <- max(plot_data$y)*1.05
  refend <- summary_data[nrow(summary_data), "y"]
  if (plotting.difference.axis) {
    refend <- refend + 2.5
  }
  myplot <- myplot + geom_segment(color = "black", linetype = "solid", aes(x = 0, xend = 0, y = refstart, yend = refend))
    
  # Apply classic theme
  myplot <- myplot + theme_classic()
  
  # Clean up axis lines and hide legends
  myplot <- myplot + theme(axis.line.y.left = element_blank())
  myplot <- myplot + theme(axis.ticks.y.left = element_blank())
  myplot <- myplot + theme(axis.line.x.bottom = element_blank())
  
  myplot <- myplot + theme(legend.position = "none")

  return(myplot)  
  
}


meta_to_table <- function(meta, fixed.effects = TRUE, dr.res = NULL, label = NULL) {
  rowcount <- length(meta$b[, 1])
  
  if(fixed.effects) {
    I2 <- rep(NA, rowcount)
    I2.low <- rep(NA, rowcount)
    I2.high <- rep(NA, rowcount)
  } else {
    hetCIs <- metafor::confint.rma.uni(meta)
    I2 <- rep(hetCIs$random["I^2(%)", "estimate"], rowcount)
    I2.low <- rep(hetCIs$random["I^2(%)", "ci.lb"], rowcount)
    I2.high <- rep(hetCIs$random["I^2(%)", "ci.ub"], rowcount)
  }
  
  if(is.null(label)) {
    label <- names(meta$b[ ,1])
  } else {
    label <- rep(label, rowcount)
  }

  
  if(is.null(dr.res)) {
    dr.res <- list(
      diamond.ratio = NA,
      dr.low = NA,
      dr.high = NA
    )
  }
   
  result_table <- data.frame(label = label, 
                             effect.size = unname(meta$b[, 1]),
                             ci.low = unname(meta$ci.lb),
                             ci.high = unname(meta$ci.ub),
                             p.value = unname(meta$pval),
                             diamond.ratio = dr.res$diamond.ratio,
                             dr.low = dr.res$dr.low,
                             dr.high = dr.res$dr.high,
                             I2 = I2,
                             I2.low = I2.low,
                             I2.high = I2.high
                            )
  return(result_table)                       
}


dr.CI <- function(RE, FE, vi, conf.level = 0.95) {
  # Calculates the confidence interval on a diamond.ratio
  # Pass in a random effects and fixed effect meta-analysis and the study effect size variances
  # Obtain a list with the diamond.ratio and its CI
  # Adapted from code provided by Maxwell Cairns -- see Maxwell Cairns, Geoff Cumming, Luke A. Prendergast, forthcoming
  # Implements the bWT-DL approach
  
  diamond.ratio <- abs(RE$ci.ub - RE$ci.lb) / abs(FE$ci.ub - FE$ci.lb) 
  
  #CI on the diamond ratio
  log.ratio <- log(diamond.ratio)
  sds <- sqrt(vi)
  w.star <- 1/(sds^2 + RE$tau2)
  
  log.var <- ((RE$se.tau2)^2)/4*1/(sum(w.star))^2*(sum(1/(sds^2 + RE$tau2)^2))^2
  log.sd <- sqrt(log.var)
  bias <- 1/2*(RE$se.tau2)^2*(1/2/sum(w.star)^2 - 1/sum(w.star)*sum(1/(sds^2 + RE$tau2)^3))
  
  #Bias corrected CIs
  zcrit <- qnorm((1-conf.level)/2, lower.tail = FALSE)
  dr.low <- max(exp(log.ratio - bias - zcrit*log.sd), 1)
  dr.high <- max(exp(log.ratio - bias + zcrit*log.sd), 1)

  res <- list(
    diamond.ratio = diamond.ratio,
    dr.low = dr.low,
    dr.high = dr.high
  )
  
  return(res)
}

##########################
# Testing

# testd <- data.frame(controlM = c(rnorm(n = 15, mean = 10, sd = 1), rnorm(n = 15, mean = 10, sd = 1)),
#                     expM = c(rnorm(n = 15, mean = 11, sd = 1), rnorm(n = 15, mean = 10, sd = 1)),
#                     controlS = abs(rnorm(n = 30, mean = 1, sd = 0.25)),
#                     expS = abs(rnorm(n = 30, mean = 1, sd = 0.25)),
#                     controlN = abs(round(rnorm(n=30, mean = 25, sd = 5))),
#                     expN = abs(round(rnorm(n=30, mean = 25, sd = 5))),
#                     mod = c(rep("Group A", 15), rep("Group B", 15))
# )
# 
# esci_test <- data.frame(nbM = c(2.89, 2.69, 2.90, 2.62, 2.96, 2.93, 2.86, 2.50, 2.41, 2.54),
#                         nbS = c(0.79, 0.55, 0.58, 0.54, 0.36, 0.60, 0.59, 0.84, 0.78, 0.66),
#                         nbN = c(28, 26, 98, 42, 24, 184, 274, 58, 34, 99),
#                         bM = c(3.12, 3.00, 2.86, 2.85, 3.07, 2.89, 2.91, 2.60, 2.74, 2.72),
#                         bS = c(0.65, 0.54, 0.61, 0.57, 0.55, 0.60, 0.52, 0.83, 0.51, 0.68),
#                         bN = c(26, 28, 99, 33, 21, 184, 255, 55, 34, 95),
#                         mod = c("Simple", "Critique", "Simple","Simple","Simple","Simple","Simple","Critique","Critique","Critique")
#                         )
# 
# 
# estimate <- estimateOverallRaw(data = esci_test, m1 = nbM, m2 = bM, s1 = nbS, s2 = bS, n1 = nbN, n2 = bN, moderator = mod, random.effects = TRUE, report.cohens.d = FALSE, conf.level = 0.95)
# myplot <- plotMetaEffect(estimate)
# myplot


# testcor <- data.frame(rvalues = rnorm(n = 15, mean = 0, sd = .1),
#                    Ns = abs(round(rnorm(n=15, mean = 25, sd = 5))),
#                    mod = c(rep("Group A", 5), rep("Group B", 5), rep("Group C", 5))
# )

# testcohend <- data.frame(d = rnorm(n = 15, mean = 0, sd = 1),
#                     n1 = abs(round(rnorm(n=15, mean = 25, sd = 5))),
#                     n2 = abs(round(rnorm(n=15, mean = 25, sd = 5))),
#                     mod = c(rep("Group A", 5), rep("Group B", 5), rep("Group C", 5))
# )


# 
# 
# estimate <- estimateOverallRaw(data = testd, m1 = controlM, m2 = expM, s1 = controlS, s2 = expS, n1 = controlN, n2 = expN, moderator = mod, random.effects = TRUE, report.cohens.d = FALSE, conf.level = 0.95)
# estimate$diamond.ratio
# estimate <- estimateOverallRaw(data = testd, m1 = controlM, m2 = expM, s1 = controlS, s2 = expS, n1 = controlN, n2 = expN, report.cohens.d = TRUE, conf.level = 0.95)
# estimate <- estimateOverallCorrelation(data = testcor, rvalues = rvalues, ns = Ns, moderator = mod)

# estimate <- estimateOverallSMD(data = testcohend, d = d, n1 = n1, n2 = n2)
# estimate <- estimateOverallSMD(data = testcohend, d = d, n1 = n1, n2 = n2, moderator = mod)
# myplot <- plotMetaEffect(estimate)
# myplot


