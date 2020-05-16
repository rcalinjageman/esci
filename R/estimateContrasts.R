scaleFUN <- function(x) sprintf("%.2f", x)

estimateContrasts <- function(x, ...) {
  UseMethod("estimateContrasts")
}


estimateContrasts.numeric = function(means = c(NULL), 
                               sds = c(NULL), 
                               ns = c(NULL), 
                               contrasts = list(NULL),
                               labels = c(NULL),
                               clabels = list(NULL),
                               conf.level = .95) {

  #################
  ## Checks
    if (is.null(means) | is.null(sds) | is.null(ns) | is.null(contrasts) ) {
      err_string <- stringr::str_interp(
        "Must pass vector of means, vector of sds, vector of Ns, and vector of contrasts.  One or more is currently null."
      )
      stop(err_string)
    }
    
    if (length(means) <1) {
      err_string <- stringr::str_interp(
        "Must pass vector of means.  Currently, length of means vector is 0"
      )
      stop(err_string)
    }
    
    if (length(sds) != length(means)) {
      err_string <- stringr::str_interp(
        "Number of standard deviations must be same as number of means.  Passed ${length(means)} means but ${length(sds)} standard deviations."
      )
      stop(err_string)
    }
    
    if (length(ns) != length(means)) {
      err_string <- stringr::str_interp(
        "Number of ns must be same as number of means.  Passed ${length(means)} means but ${length(ns)} ns."
      )
      stop(err_string)
    }
  
    if (length(labels) != 0) {
      if (length(labels) != length(means)) {
        err_string <- stringr::str_interp(
          "Labels are optional, but if passed the number of labels must be same as number of means.  Passed ${length(means)} means but ${length(labels)} labels."
        )
        stop(err_string)
      }
    } else {
      labels <- paste("Group", c(1:length(means)))
    }
  
    if (length(contrasts) <1) {
      err_string <- stringr::str_interp(
      "Must pass vector of contrasts.  Currently, length of contrsts vector is 0"
      )
      stop(err_string)
    }
  
  contrast_count <- 0
    for (tcontrast in contrasts) {
      contrast_count <- contrast_count + 1
      if(length(tcontrast) != length(means)) {
        err_string <- stringr::str_interp(
          "Each contrast must have the same number of values as the number of means.  Contrast ${contrast_count} has length ${length(tconstrast)}, wherease there are ${length(means)} means."
        )
      }
      
    }
  

  ######################
  ## Global calculations
    # ngroups
    ngroups <- length(means)
  
    # degress of freedom
    dfs <- ns - 1
    df <- sum(dfs)
    
    # pooled sd
    vars <- sds^2
    sp <- sqrt(sum(vars*dfs) / df)
    
    # t crit
    tcrit <- qt(1-((1 - conf.level)/2), df)
  
    
   ################
   #Create a table of group means and CIs as well
    
    means_table <- data.frame(label = NULL, m = NULL, moe = NULL, ci.low = NULL, ci.high = NULL, ci.formatted = NULL, conf.level = NULL, s = NULL, n = NULL)
    error_table <- data.frame(t = NULL, plot_labels = NULL, m = NULL, x = NULL, contrast_number = NULL, contrast_column = NULL, label = NULL)
                                
    my_index <- 0
    for (x in means) {
      my_index <- my_index + 1
      moe <- tcrit*sp*(1/sqrt(ns[my_index]))
      ci.low <- means[my_index] - moe
      ci.high <- means[my_index] + moe
      ci.formatted <- paste("[", trimws(formatC(ci.low, format = "fg", digits = 3)), ", ", trimws(formatC(ci.high, format = "fg", digits = 3)), "]")
      
      means_table <- rbind(means_table, data.frame(
        label = labels[my_index],
        m = means[my_index],
        moe = moe,
        ci.low = ci.low,
        ci.high = ci.high,
        ci.formatted = ci.formatted,
        conf.level = conf.level,
        s = sds[my_index],
        n = ns[my_index]
      )
      )
      
      #Now create error distribution for this mean
      edata <- data.frame(t = rt(10000, ns[my_index]-1))
      edata$plot_labels <- labels[my_index]
      edata$m <- edata$t * sp*(1/sqrt(ns[my_index]))
      edata$m <- edata$m + means[my_index]
      edata$x <- my_index
      edata$contrast_number <- 0
      edata$contrast_column <- "Unused"
      edata$label <- "Mean"
      
      #Add error distribution for this mean to the table of error data
      error_table <- rbind(error_table, edata)

    }   
    
    
  #######################
  #Process each contrast
    
    contrast_count <- 0
    result_table <- data.frame(contrast_number = NULL, label = NULL, m = NULL, moe = NULL, ci.low = NULL, ci.high = NULL, ci.formatted = NULL, conf.level = NULL, p.value = NULL)
    for (contrast in contrasts) {
      
      contrast_count <- contrast_count + 1

      #Make label
      if (length(labels) != 0) {
        g1 <- "("
        g2 <- "("
        myindex <- 0
        for (x in contrast) {
          myindex <- myindex + 1
          if(x > 0) {
            g1 <- paste(g1, trimws(labels[myindex]), " and ", sep="")
          } 
          if(x < 0) {
            g2 <- paste(g2, trimws(labels[myindex]), " and ", sep="")
          }
        }
        if (stringr::str_length(g1) > 0) {
          g1 <- substr(g1, 1, stringr::str_length(g1)-5)
          g1 <- paste(g1, ")", sep="")
        }
        if (stringr::str_length(g2) > 0) {
          g2 <- substr(g2, 1, stringr::str_length(g2)-5)
          g2 <- paste(g2, ")", sep="")
        }
        contrast_label <- paste(g1, " vs. ", g2, sep = "")
      } else {
        contrast_label <- paste(formatC(contrast, digits=2, format="f"), collapse = ", ")
        g1 <- "G1"
        g2 <- "G2"
      }
      
      
      if(!is.null(clabels)) {
        if(contrast_count <= length(clabels)) {
          if(!is.null(clabels[[contrast_count]])) {
            if(length(clabels[[contrast_count]]) == 3) {
              g1 <- clabels[[contrast_count]][1]
              g2 <- clabels[[contrast_count]][2]
              contrast_label <- clabels[[contrast_count]][3]
            }
          }
        }
      }

      # Sample-size component
      con_squared <- contrast^2
      sample_size_component <- sqrt( sum( con_squared / ns) )
      
      #moe
      moe <- tcrit*sp*sample_size_component
      
      #mean difference
      mdiff <- sum(means * contrast)
      
      #conf.int
      ci.low <- mdiff - moe
      ci.high <- mdiff + moe
      ci.formatted <- paste("[", trimws(formatC(ci.low, format = "fg", digits = 3)), ", ", trimws(formatC(ci.high, format = "fg", digits = 3)), "]")
      
      #contrast group data
      m_pos <- mean(means[which(contrast > 0)])
      m_neg <- mean(means[which(contrast < 0)])

      n_pos <- sum(ns[which(contrast > 0)])
      n_neg <- sum(ns[which(contrast < 0)])
      
      moe_pos <- tcrit*sp*(1/sqrt(n_pos))
      moe_neg <- tcrit*sp*(1/sqrt(n_neg))
      
      pos_ci.low <- m_pos - moe_pos
      pos_ci.high <- m_pos + moe_pos
      neg_ci.low <- m_neg - moe_neg
      neg_ci.high <- m_neg + moe_neg
      
      pos_ci.formatted <- paste("[", trimws(formatC(pos_ci.low, format = "fg", digits = 3)), ", ", trimws(formatC(pos_ci.high, format = "fg", digits = 3)), "]")
      neg_ci.formatted <- paste("[", trimws(formatC(neg_ci.low, format = "fg", digits = 3)), ", ", trimws(formatC(neg_ci.high, format = "fg", digits = 3)), "]")      
      
      sediff <- sp * sample_size_component
      tvalue <- mdiff / sediff
      pvalue <- 2*pt(-abs(tvalue),df=df)
    
      ####################
      # Error data
      
      g1e <- rt(2000, n_pos-1)
      g2e <- rt(2000, n_neg-1)
      diffe <- rt(2000, df)
      g1e <- g1e* sp*(1/sqrt(n_pos))
      g2e <- g2e* sp*(1/sqrt(n_neg))
      diffe <- diffe * sp* sample_size_component
      g1e <- g1e + m_pos
      g2e <- g2e + m_neg
      diffe <- diffe + m_pos
      
      error_table <- rbind(error_table, data.frame(t = rep(0, 6000),
                                                   plot_labels = c(rep(g1, 2000), rep(g2, 2000), rep(contrast_label, 2000)),
                                                   m = c(g1e, g2e, diffe),
                                                   x = c(rep(nrow(means_table)+3, 2000), rep(nrow(means_table)+2, 2000), rep(nrow(means_table)+4, 2000)),
                                                   contrast_number = rep(contrast_count, 6000),
                                                   contrast_column = rep("Unused", 6000),
                                                   label = c(rep("G1M", 2000), rep("G2M", 2000), rep("Difference", 2000))
                                                   ))
      
      
      
      ########################
      #store contrast result in result_table
      result_table <- rbind(result_table, data.frame(
        contrast_number = contrast_count,
        label = g1,
        m = m_pos,
        moe = moe_pos,
        ci.low = pos_ci.low,
        ci.high = pos_ci.high,
        ci.formatted = pos_ci.formatted,
        conf.level = conf.level,
        p.value = NA
        )
      )
      
      result_table <- rbind(result_table, data.frame(
        contrast_number = contrast_count,
        label = g2,
        m = m_neg,
        moe = moe_neg,
        ci.low = neg_ci.low,
        ci.high = neg_ci.high,
        ci.formatted = neg_ci.formatted,
        conf.level = conf.level,
        p.value = NA
      )
      )
      
      result_table <- rbind(result_table, data.frame(
          contrast_number = contrast_count,
          label = contrast_label,
          m = mdiff,
          moe = moe,
          ci.low = ci.low,
          ci.high = ci.high,
          ci.formatted = ci.formatted,
          conf.level = conf.level,
          p.value = pvalue
          )
      )
      
      
      
    }  

    
    #####################
    # Prep plot data

    # convert means table to plot_table for graphing
    plot_table <- means_table
    plot_table$s <- NULL
    plot_table$n <- NULL
    plot_table$p.value <- NA
    plot_table <- cbind(data.frame(contrast_number = rep(0,nrow(plot_table))), plot_table)
    
    # Then add in results
    plot_table <- rbind(plot_table, result_table)

    # Now make a contrast column for each contrast
    contrast_count <- 0
    for (contrast in contrasts) {
      contrast_count <- contrast_count + 1
      contrast_column <- paste("contrast", contrast_count, collapse = "")
      cgroupvector <- contrast
      cgroupvector[which(contrast == 0)] = "Unused"
      cgroupvector[which(contrast > 0)] = "G1"
      cgroupvector[which(contrast < 0)] = "G2"
      cgroupvector <- c(cgroupvector, rep(c("G1M", "G2M", "Difference"), length(contrasts)))
      plot_table[, contrast_column] <- cgroupvector
    }
    
    # Labels
    plot_table$plot_labels <- c(labels, rep(c("G1", "G2", "Difference"), length(contrasts)))
    
    # Finally, means locations
    ngroups <- nrow(means_table)
    g1place <- ngroups+3
    g2place <- ngroups+2
    diffplace <- ngroups +4
    plot_table$x <- c(c(1:ngroups), rep(c(g1place,g2place,diffplace), length(contrasts)))
    
    res <- list(
      contrast_table = result_table,
      means_table = means_table,
      plot_table = plot_table,
      error_table = error_table,
      pooledsd = sp,
      contrasts = contrasts
    )
    
    return(res)
      
}

estimateContrasts.default <- function(data, x, y, contrasts = list(NULL), conf.level = 0.95) {
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  x_enquo        <-  rlang::enquo(x)
  x_quoname      <-  rlang::quo_name(x_enquo)
  
  y_enquo        <-  rlang::enquo(y)
  y_quoname      <-  rlang::quo_name(y_enquo)
  
  # Validate inputs ---------------------------
  # Must have a list of contrasts
  if (length(contrasts) <1) {
    err_string <- stringr::str_interp(
      "Must pass vector of contrasts.  Currently, length of contrsts vector is 0"
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

  data$x <- 0
  
  #Now get summary data by group
  means <- c(NULL)
  sds <- c(NULL)
  ns <- c(NULL)
  labels <- c(NULL)
  this_index <- 0
  
  for (this_group in levels(data$iv)) {
    group_only <- data[data$iv == this_group, ]
    if (nrow(group_only) > 0) {
      this_index <- this_index + 1
      data[data$iv == this_group, ]$x <- this_index-0.5
      means[this_index] <- mean(group_only$dv)
      sds[this_index] <- sd(group_only$dv)
      ns[this_index] <- nrow(group_only)
      labels[this_index] <- this_group
    }
  }

  ### Convert contrasts
  my_index <- 0
  for (contrast in contrasts) {
    my_index <- my_index + 1
    if (is.numeric(contrast)) {
      # Contrast passed numerically - probably dangerous given lack of control over factor level order
      if (length(contrast) != length(means)) {
        err_string <- stringr::str_interp(
          "Numeric contrasts must be same length as number of valid groups.  Contrast ${my_index} has ${length(contrast)} elements but only ${length(means)} valid group means were found."
        )
        stop(err_string)
      }
    } else {
      #Contrast passed by group name   
      negcon <- contrast[grep("^[-].*", contrast)]
      poscon <- contrast[! contrast %in% negcon]
      negcon <- sub(".", "", negcon)
      
      # double-check length of contrast passed
      if(length(negcon) + length(poscon) > length(means)) {
        error_string <- stringr::str_interp(
          "You passed a contrast that has more elements than valid means.  Contrast ${my_index} has ${length(contrast)} elements but only ${length(means)} valid group means were found."
        )
        stop(error_string)
      }
      
      # double-check no overlap
      if (all(! poscon %in% negcon)) {
      } else {
        error_string <- stringr::str_interp(
          "You passed a contrast that has the same condition on both sides of the contrast.  Contrast ${my_index} compares ${poscon} to ${negcon}."
        )
        stop(error_string)
      }
      
      # double-check no levels passed that don't exist
      if (! all(poscon %in% labels)) {
        error_string <- stringr::str_interp(
          "You named a condition in one of your contrasts that doesn't exist or have a valid mean.  Contrast ${my_index} has conditions ${poscon} but valid means were only found for ${labels}."
        )
        stop(error_string)
      }
      if (! all(negcon %in% labels)) {
        error_string <- stringr::str_interp(
          "You named a condition in one of your contrasts that doesn't exist or have a valid mean.  Contrast ${my_index} has conditions ${negcon} but valid means were only found for ${labels}."
        )
        stop(error_string)
      }
      
      posvalue <- 1/length(poscon)
      negvalue <- -1/length(negcon)
      newcontrast <- labels

      newcontrast[! newcontrast %in% c(poscon, negcon)] <- 0
      newcontrast[newcontrast %in% poscon] <- posvalue
      newcontrast[newcontrast %in% negcon] <- negvalue
      
      contrasts[[my_index]] <- as.numeric(newcontrast)

    }
  }

  contrast_count <- 0
  for (contrast in contrasts) {
  contrast_count <- contrast_count + 1
   contrast_column <- paste("contrast", contrast_count)
   data[data$iv %in% levels(data$iv)[which(contrast > 0)], contrast_column] <- "G1"
   data[data$iv %in% levels(data$iv)[which(contrast < 0)], contrast_column] <- "G2"
   data[data$iv %in% levels(data$iv)[which(contrast == 0)], contrast_column] <- "Unused"
  }

  ### Now pass along to summary data version
  res <- estimateContrasts.numeric(means = means,sds = sds, ns = ns, labels = labels, contrasts = contrasts, conf.level = conf.level)

  res$raw_data <- data
    
  ### Prepare for return
  return(res)  
    

}


plotContrast <- function(estimate, contrast_number = NULL, contrast_colors = c("blue", "green"), show.mean.error = TRUE, show.group.error = TRUE, show.raw.data = TRUE, ylab = "Dependent Variable") {
  ##########
  # Input validation
  
  # Set plotting_contrast to false
  plotting_contrast <- FALSE
  plotting_interaction <- FALSE

  # Check to see if a valid contrast numbber has been passed
  if(!is.null(contrast_number)) {
    if(contrast_number > 0) {
      if(!is.null(estimate$contrast_table)) {
        if(nrow(estimate$contrast_table) > 0) {
          plotting_contrast <- TRUE
          contrast_column_name <- paste("contrast", contrast_number, collapse="")
        }
      }
    }
  }
  
  # Check colors passed - if 2 are not passed, default back to blue and green
  if(length(contrast_colors) != 2) {
    contrast_colors <- c("blue", "green")
  }
  g1_color <- contrast_colors[1]
  g2_color <- contrast_colors[2]

  
  
  #####################
  # Wrangle data
  
  # s_data stores the plot_table for manipulation
  s_data <- estimate$plot_table
  e_data <- estimate$error_table
  r_data <- estimate$raw_data
  if(plotting_contrast) {
      # narrow s_data down; drop the contrasts not of interest for this plot
      s_data <- s_data[s_data$contrast_number == 0 | s_data$contrast_number == contrast_number, ]   
      e_data <- e_data[e_data$contrast_number == 0 | e_data$contrast_number == contrast_number, ]
      if(s_data[nrow(s_data), ]$plot_labels == "Interaction") { 
        plotting_interaction  <- TRUE
        interaction_magnitude <- s_data[5, ]$m 
      }
      # get the offset mean, which is the mean used as the 'reference' for estimating the mean difference
      offset_mean <- s_data[nrow(s_data)-1,"m"]
      if(plotting_interaction) {
        offset_mean <- s_data[4, "m"] - s_data[5, "m"]
      }
      
      # now adjust the difference estimate to plot at the same place, which will be 0 on the difference axis
      s_data[nrow(s_data), "m"] <- s_data[nrow(s_data), "m"] + offset_mean
      s_data[nrow(s_data), "ci.low"] <- s_data[nrow(s_data), "ci.low"] + offset_mean
      s_data[nrow(s_data), "ci.high"] <- s_data[nrow(s_data), "ci.high"] + offset_mean
      # store the column that will encode the details onthe contrast
      s_data$contrast_column <- s_data[ , contrast_column_name]
      if(!is.null(r_data)) {
        r_data$contrast_column <- r_data[ , contrast_column_name]
      }
      # get the min and max for each grouping to make the grouping lines
      g1min <- min(s_data[s_data$contrast_column == "G1", ]$m)
      g2min <- min(s_data[s_data$contrast_column == "G2", ]$m)
      g1max <- max(s_data[s_data$contrast_column == "G1", ]$m)
      g2max <- max(s_data[s_data$contrast_column == "G2", ]$m)

      if(!plotting_interaction) {
        levels(s_data$label)[match(s_data[nrow(s_data)-2, "label"],levels(s_data$label))] <- paste("\n\n", s_data[nrow(s_data)-2, "label"], sep="")
        levels(s_data$label)[match(s_data[nrow(s_data), "label"],levels(s_data$label))] <- paste("\n\n\n\n", s_data[nrow(s_data), "label"], sep="")
      }
  } else {
      #make up a contrast column and drop everthing but the individual group means
      s_data$contrast_column <- "Means"
      s_data <- s_data[s_data$contrast_number == 0, ]
      e_data <- e_data[e_data$contrast_number == 0, ]
      if(!is.null(r_data)) {
        r_data$contrast_column <- "G1"
      }
      
  }
  
  xending <- nrow(s_data)+2.5
  if (plotting_interaction) {
    xending <- nrow(s_data)+2
  }
  
  if (!show.mean.error) {
    m_data <- e_data[e_data$contrast_number != 0, ]
    e_data <- m_data
  }
  
  if (!show.group.error) {
    e_data <- e_data[e_data$label != "G1M", ]
    e_data <- e_data[e_data$label != "G2M", ]
  
  }
  
  #####################
  # Build up the plot
  
  # Basic plot
  myplot <- ggplot(data = s_data, aes(x=x, y=m, fill=contrast_column, color =contrast_column, shape = contrast_column, size = contrast_column))
  # Set x continuious scale to be like an discrete scale, with 1 break/group and the break labels set to each group name
  myplot <- myplot + scale_x_continuous(limits = c(0, xending), name = NULL, breaks = s_data$x, labels = s_data$label, expand = c(0,0))
  # If we are plotting a contrast, set fill, color, shape, and size to custom settings
  if (plotting_contrast) {
    myplot <- myplot + scale_fill_manual(values = c(G1 = g1_color, G2 = g2_color, Unused = "Gray", Else = "Blue", G1M = g1_color, G2M = g2_color, Difference = "Black"))
    myplot <- myplot + scale_color_manual(values = c(G1 = "Gray", G2 = "Gray", Unused = "Gray", Else = "Blue", G1M = g1_color, G2M = g2_color, Difference = "Black"))
    myplot <- myplot + scale_shape_manual(values = c(G1 = 21, G2 = 21, Unused = 21, Else = 21, G1M = 22, G2M = 22, Difference=24))
    myplot <- myplot + scale_size_manual(values = c(G1 = 3, G2 = 3, Unused = 2, Else = 2, G1M = 3, G2M = 3, Difference = 3.5))
  }
  
  # Error bars
  myplot <- myplot + geom_errorbar(data=s_data, aes(ymin=s_data$ci.low, ymax = s_data$ci.high),size=1, width = 0)
  # Cats eye
  myplot <- myplot + geom_cats_eye(data=e_data, aes(x=x, y=m, group=x), width=.5, adjust=3, size=0, alpha=.5)
  # Raw data
  if(!is.null(r_data) & show.raw.data) {
    myplot <- myplot + geom_beeswarm(data = r_data, aes(x = x, y = dv, group=x), size = 0.5)
  }
  
  
  ###############
  # Mark contrast
  if(plotting_contrast & !plotting_interaction) {
    #get the contrast means  
    g1mean <- s_data[nrow(s_data)-2, "m"]
    g2mean <- s_data[nrow(s_data)-1, "m"]
    
    #get x placement of the two contrast groups
    g1end <- s_data[nrow(s_data)-2,"x"]
    g2end <- s_data[nrow(s_data)-1,"x"]

    #if there is only 1 group in a contrast, draw a line staight to it, otherwise a grouping line from min to max at -0.5
    if (g1max != g1min) { 
      g1end <- g1end - 0.5
      myplot <- myplot + geom_segment(color = g1_color, linetype = "solid", alpha = 0.5, size=1, aes(y = g1min, yend = g1max, x = g1end, xend = g1end))
      myplot <- myplot + geom_segment(color = g1_color, linetype = "solid", alpha = 0.5, size=1, aes(y = g1mean, yend = g1mean, x=g1end, xend = g1end+0.5))
    }
    if (g2max != g2min) { 
      g2end <- g2end - 0.5
      myplot <- myplot + geom_segment(color = g2_color, linetype = "solid", alpha = 0.5, size=1, aes(y = g2min, yend = g2max, x = g2end, xend = g2end))
      myplot <- myplot + geom_segment(color = g2_color, linetype = "solid", alpha = 0.5, size=1, aes(y = g2mean, yend = g2mean, x = g2end, xend = g2end+0.5))
    }

    #Draw lines from each mean in a contrast over to its respective contrast mean    
    myplot <- myplot + geom_segment(data = s_data[s_data$contrast_column == "G1", ], color = g1_color, linetype = "solid", alpha = 0.5, size = 1, aes(y = m, yend = m, x = x, xend = g1end))
    myplot <- myplot + geom_segment(data = s_data[s_data$contrast_column == "G2", ], color = g2_color, linetype = "solid", alpha = 0.5, size = 1, aes(y = m, yend = m, x = x, xend = g2end))
    


    #draw dotted lines from contrast group means over to difference axis    
    myplot <- myplot + geom_segment(linetype = "dotted", size = 1, color = "black", aes(y=g1mean, yend=g1mean, x=ceiling(g1end), xend=nrow(s_data)+2.5))
    myplot <- myplot + geom_segment(linetype = "dotted", size = 1, color = "black", aes(y=g2mean, yend=g2mean, x=ceiling(g2end), xend=nrow(s_data)+2.5))
    
  }
  
  if (plotting_interaction) {
    g1mean <- s_data[4, "m"] 
    g2mean <- s_data[4, "m"] - interaction_magnitude
    
    # Plot each simple effect
    myplot <- myplot + geom_segment(color="purple", linetype="solid", size = 1, aes(x=1.5, xend = 1.5, y=s_data[1, ]$m, yend = s_data[2, ]$m))
    myplot <- myplot + geom_segment(color="purple", linetype="solid", size = 1, aes(x=3.5, xend = 3.5, y=s_data[4, ]$m, yend = s_data[3, ]$m))
    myplot <- myplot + geom_point(color = "purple", fill="purple", size = 2, shape = 23, aes(x = 1.5, y = s_data[2, ]$m))
    myplot <- myplot + geom_point(color = "purple", fill="purple", size = 2, shape = 23, aes(x = 3.5, y = s_data[4, ]$m))
    
    #Then lines contrasting each simple effects    
    myplot <- myplot + geom_segment(color="purple", linetype="dotted", size = 1, aes(x=1.5, xend = 3.5, y=s_data[1, ]$m, yend = s_data[3, ]$m))
    myplot <- myplot + geom_segment(color="purple", linetype="dotted", size = 1, aes(x=1.5, xend = 3.5, y=s_data[2, ]$m, yend = s_data[4, ]$m-interaction_magnitude))
    
    #Then lines over to the floating axis
    myplot <- myplot + geom_segment(color="purple", linetype="dotted", size=1, aes(x=3.5, xend = xending, y=s_data[4, ]$m, yend=s_data[4, ]$m))
    myplot <- myplot + geom_segment(color="purple", linetype="dotted", size=1, aes(x=3.5, xend = xending, y = s_data[4, ]$m-interaction_magnitude, yend = s_data[4, ]$m-interaction_magnitude))
    
  }
 
  #########################
  #Difference axis 
  if (plotting_interaction | plotting_contrast) {

    errorLower <- s_data$ci.low[nrow(s_data)]
    errorUpper <- s_data$ci.high[nrow(s_data)]
    
    if (errorUpper > g2mean) {
      saxisEnd <- ceiling((errorUpper-g2mean)/estimate$pooledsd)
      if (saxisEnd < 1) saxisEnd = 1
    } else {
      saxisEnd <- 0
    }
    
    if (errorLower < g2mean) {
      saxisStart <- floor((errorLower - g2mean)/estimate$pooledsd)
      if (saxisStart > -1) saxisStart = -1
    } else {
      saxisStart <- 0
    }
    
    saxisBreaks <- seq(saxisStart, saxisEnd, by=0.5)
    saxisBreaks <- saxisBreaks * estimate$pooledsd
    
    saxisStart <- g2mean + saxisBreaks[1]
    saxisEnd <- g2mean + saxisBreaks[length(saxisBreaks)]
    
    myplot <- myplot + scale_y_continuous(expand = expand_scale(mult = c(.15, .15)), sec.axis = sec_axis(trans = ~.-g2mean, breaks = saxisBreaks, labels = scaleFUN))
    
    # Now make the secondary axis floating
    
   
    myplot <- myplot + geom_segment(color="black", linetype="solid", aes(x=xending ,xend=xending ,y=saxisStart, yend=saxisEnd), size=1)
    
  }
  
  
  #Finally, points on top
  myplot <- myplot + geom_point()
  
  #And some theme stuff
  myplot <- myplot + ylab(ylab)
  myplot <- myplot + theme_classic()
  myplot <- myplot + theme(legend.position = "none")
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  myplot <- myplot + theme(axis.line.y.right = element_blank())


  return(myplot)

}


# 
#  means <- c(37.5, 31.9, 41.2, 33.4, 29.9, 38.3)
#  sds <- c(10, 13.5, 14.8, 10, 8.7, 10)
#  ns <- c(19, 19, 19, 19, 19, 19)
#  labels <- c("NFree10", "AFree10", "ADiet10", "NFree17", "AFree17", "ADiet17")
#  conf.level <- 0.95
#  contrast1 <- c(1, -1/3, -1/3, -1/3, 0, 0)
#  contrast2 <- c(1, -1, 0, 0, 0, 0)
#  contrasts <- list(contrast1, contrast2)
#  estimate <- estimateContrasts.numeric(means, sds, ns, contrasts, labels, conf.level = 0.95)
# 
# 
#  myplot <- plotContrast(estimate, contrast_number = 1, contrast_colors = c("red", "purple"), show.mean.error = FALSE)
#  myplot
# 
# myplot <- plotContrast(estimate, contrast_colors = c("red", "gray"))
# myplot
# 

# myplot <- plotContrast(estimate, contrast_number = 1, contrast_colors = c("red", "purple"), show.mean.error = FALSE)
# myplot
# 
# myplot <- plotContrast(estimate, contrast_number = 2)
# myplot
# 

# myplot
# myplot <- plotContrast(estimate, contrast_number = 2)
# myplot
# myplot <- plotContrast(estimate)
# myplot
# 
# 

# g1 <- "setosa, virginica"
# g2 <- "versicolor"
# 
# g1s <- strsplit(g1, ",")
# g1s <- trimws(g1s[[1]], which = "both")
# g2s <- strsplit(g2, ",")
# g2s <- trimws(g2s[[1]], which = "both")
# 
# 
# contrast1 <- c(g1s, paste("-", g2s, sep=""))
# contrast2 <- c("setosa", "virginica", "-versicolor")
# 
# mycontrasts <- list(contrast1, contrast2)
# 
# estimate <- estimateContrasts(iris, Species, Petal.Width, mycontrasts)

# contrast1 <- c("setosa", "-virginica")
# contrast2 <- c(1, -0.5, -0.5)
# contrast3 <- c("setosa", "-virginica", "-versicolor")
# contrast4 <- c("virginica", "-setosa", "-versicolor")
# contrasts <- list(contrast1, contrast2, contrast3, contrast4)
# estimate <- estimateContrasts.default(iris, Species, Petal.Width, contrasts)
# estimate
#  myplot <- plotContrast(estimate, contrast_number = 1)
#  myplot
#  
 
# contrast1 <- c("d 0.5", "-d 1", "-d 2")
# mytooth <- datasets::ToothGrowth
# mytooth$dose <- as.factor(paste("d", mytooth$dose))
# contrasts <- list(contrast1)
# estimate <- estimateContrasts.default(mytooth, dose, len, contrasts)
# myplot <- plotContrast(estimate, contrast_number = 1, show.rawdata = FALSE)
# myplot
# 
# 
# 
# contrast1 <- c("-ctrl", "trt1")
# contrast2 <- c("-ctrl", "trt2")
# contrast3 <- c("-ctrl", "trt1", "trt2")
# estimate <- estimateContrasts.default(PlantGrowth, group, weight, contrast = list(contrast1, contrast2, contrast3))
# estimate


