scaleFUN <- function(x) sprintf("%.2f", x)

plotEstimatedDifference <- function(estimate, ylims = NULL, ylab = NULL, xlab = NULL, rope = c(NA, NA), grouplabels = c("auto", "auto"), fills = c("white", "gray75", "gray75"), bcex = 1, bsize = 2, alpha = 0.3) {

  # Check inputs ---------------------------
  # Needs to be of class estimate
  if (class(estimate) != "estimate") {
    err_string <- stringr::str_interp(
      "`estimate` must be class estimate, not ${class(estimate)}"
    )
    stop(err_string)
  }

  
  #Alpha
  if (alpha < 0) {
    warning_string <- stringr::str_interp("'alpha' ignored because it is less than 0.  You passed alpha = ${alpha}.")
    warning(warning_string)
    alpha = 0.3
  }
  if (alpha > 1) {
    warning_string <- stringr::str_interp("'alpha' ignored because it is greater than 1.  You passed alpha = ${alpha}.")
    warning(warning_string)
    alpha = 1
  }
  
  # Check ylims
  # Currently, ggbeeswarm 0.6 fails when passed in NA, so we must either not use ylim or pass in two valid numbers
  if(!is.null(ylims)) {
    if(is.vector(ylims)) {
      if (length(ylims) != 2) { 
        warning_string <- stringr::str_interp("'ylims' ignored because not a vector of length 2, instead length of ylims = ${length(ylims)}.")
        warning(warning_string)
        ylims <- NULL
      } else {
        if(ylims[1] == "auto" | is.na(ylims[1]) | ylims[2] == "auto" | is.na(ylims[2])) {
          ylims <- NULL
        } else {
          ylims[1] <- as.numeric(ylims[1])
          ylims[2] <- as.numeric(ylims[2])
          if(ylims[2] <= ylims[1]) {
            warning_string <- stringr::str_interp("'ylims' ignored because ymax is < ymin: ymax = ${ylims[2]}, ymin = ${ylims[1]}.")
            warning(warning_string)
            ylims <- NULL
          }
        }
      }
    } else {
      err_string <- stringr::str_interp(
        "'ylims' must be a vector, not ${class(ylims)}"
      )
      stop(err_string)
    }
  }
  
  #Group labels
  if (!is.null(grouplabels)) {
    if(is.vector(grouplabels)) {
      if(length(grouplabels) != 2){
        warning_string <- stringr::str_interp("'grouplabels' ignored because not a vector of length 2, instead length of grouplabels = ${length(grouplabels)}.")
        warning(warning_string)
        grouplabels = c("auto", "auto")
      }
    } else {
      err_string <- stringr::str_interp(
        "'grouplabels' must be a vector, not ${class(grouplabels)}"
      )
      stop(err_string)
    }
  }
  
  #ylab
  if (!is.null(ylab)) {
    if(!is.character(ylab)) {
      ylab <- NULL
      warning_string <- stringr::str_interp("'ylab' ignored because not a character string, instead ylab class is ${class(ylab)}.")
      warning(warning_string)
    }
  }
  
  #xlab
  if (!is.null(xlab)) {
    if(!is.character(xlab)) {
      xlab <- NULL
      warning_string <- stringr::str_interp("'xlab' ignored because not a character string, instead xlab class is ${class(xlab)}.")
      warning(warning_string)
    }
  }
  
  #fills
  if(is.vector(fills)) {
    if (length(fills) != 3) { 
      fills <- c("white", "gray75", "gray75")
      warning_string <- stringr::str_interp("'fills' ignored because not a vector of 3 colors, instead length of fills = ${length(fills)}.")
      warning(warning_string)
    }
  } else {
    err_string <- stringr::str_interp(
        "'fills' must be a vector, not ${class(fills)}"
      )
      stop(err_string)
  }
  
  #rope
  printrope <- FALSE
  if(is.null(rope)) {
    err_string <- stringr::str_interp(
      "'rope' must be a vector with 2 elements or not passed at all."
    )
    stop(err_string)
  } else {
    if (length(rope) < 2) {
      err_string <- stringr::str_interp(
        "'rope' must be a vector with two elements, currently class is ${class(rope)} with length ${length(rope)}"
      )
      stop(err_string)
    } else {
      if(is.na(rope[1]) | is.na(rope[2])) {
        printrope <- FALSE
      } else {
        printrope <- TRUE
      }
    }
  }
  
  if(estimate$paired) {fills[2] <- fills[1]}
  

  # Prep for building graph ---------------------------
  # First, we are going to reverse the order of the summary data.  
  # Because graphically, it looks better to have the reference group on the left and the contrast group in the right
  s_data <- estimate$plot_info$plot_data
  s_data$iv <- factor(s_data$iv, levels = c(estimate$level2, estimate$level1, "Difference"))

  # Now we need to do some transforms so that the difference data is plotted relative to the contrast group
  # Set difference to plot at reference grooup mean and set difference CI relative to there as well
  s_data$m[3] <- s_data$m[1]    
  s_data$cilow[3] <- s_data$m[1] - s_data$moe[3]
  s_data$cihigh[3] <-  s_data$m[1] + s_data$moe[3]
  errorLower <- s_data$cilow[3]
  errorUpper <- s_data$cihigh[3]
  
  # If paired, we need to anchor the difference scores to the reference group
  if(estimate$paired & estimate$type == "fromRaw") {
    estimate$raw_data[estimate$raw_data$iv == "Difference", ]$dv <- estimate$raw_data[estimate$raw_data$iv == "Difference", ]$dv + s_data$m[2] 
  }
  
  # Now we generate the sampling error distribution.
  # This is a bit odd--we're sampling from the t distribution and scaling based on the sem of the difference and then anchoring to the reference group
  # THis lets us use the density of this data to plot the sampling error distribution using the GeomFlatViolin code I stole from DaBestR
  diffdata <-  estimate$plot_info$diffdata
  # before anchoring to the reference group mean, store the min and max values to use for the secondary axis
  diffdata$dv <- diffdata$dv + s_data$m[1]
  diffdata$iv <-  "Difference"

  # Now we need to set the labels for the secondary axis
  # We want to mark in pooledsd units by 1/2s, so each tick represents cohen's d change of 0.5
  # We want the axis to span from the reference group up to the end of the error distribution
  # And if the error distribution is very narrow, we want at least +/- 1 pooledsd to be shown
  if (errorUpper > s_data$m[2]) {
    saxisEnd <- ceiling((errorUpper-s_data$m[2])/estimate$pooledsd)
    if (saxisEnd < 1) saxisEnd = 1
  } else {
    saxisEnd <- 0
  }
  
  if (errorLower < s_data$m[2]) {
    saxisStart <- floor((errorLower - s_data$m[2])/estimate$pooledsd)
    if (saxisStart > -1) saxisStart = -1
  } else {
    saxisStart <- 0
  }

  # Now we know how many pooledsd units are between the reference group and the end of the expected error distribution
  # So we make a sequence counting between these and multiple by pooledsd to scale up the tick labels
  saxisBreaks <- seq(saxisStart, saxisEnd, by=0.5)
  saxisBreaks <- saxisBreaks * estimate$pooledsd
  
  # Finaly, store the start and end point relative to the main axis so we can make the secondary axis floating  
  saxisStart <- s_data$m[2] + saxisBreaks[1]
  saxisEnd <- s_data$m[2] + saxisBreaks[length(saxisBreaks)]

  # This defines how much we nudge the mean and cis to accomodate the beeswarm for when raw data is available
  if(estimate$type == "fromRaw") {
    if(estimate$paired) {
      nudge <-c(.25, -0.25, .35)
      ref1 <- 0.75
      ref2 <- 2.25
    } else {
      nudge <-c(0.35, 0.35, 0)
      ref1 <- 1.35
      ref2 <- 2.35
    }
  } else {
    nudge <-c(0,0,0)
    ref1 <- 1
    ref2 <- 2
  }

  # Now set the labels for the groups
  if (grouplabels[1] == "auto") {
    grouplabels[1] <- paste(estimate$level2, "\nN = ", s_data$n[2])
  }
  if (grouplabels[2] == "auto") {
    grouplabels[2] <- paste(estimate$level1, "\nN = ", s_data$n[1])
  }
  grouplabels[3] <- "Difference"

  #Labels
  if(is.null(ylab)) {
    ylab <- estimate$y 
  } else {
    if (ylab == "auto") {
      ylab = "Dependent Variable"
    }
  }
  if(is.null(xlab)) { 
    xlab <- paste(estimate$x, "\n", estimate$formatted_mdiff) 
  } else {
    if (xlab == "auto") {
      xlab <- paste("Group Variable\n", estimate$formatted_mdiff)
    }
  }
  # Generate plot ---------------------------
  # Basic plot
  myplot <- ggplot(data=s_data, aes(x=iv, y=m, color=iv, shape=iv, fill=iv))
  # Set secondary axis
  myplot <- myplot + scale_y_continuous(limits = ylims, sec.axis = sec_axis(trans = ~.-s_data$m[2], breaks = saxisBreaks, labels = scaleFUN))
  
  # Style options
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  myplot <- myplot + scale_color_manual(values = c("black", "black", "black"))  #Outlines
  myplot <- myplot + scale_fill_manual(values = fills)  #Fills
  myplot <- myplot + scale_shape_manual(values=c(21, 21, 17)) #shapes
  myplot <- myplot + theme(legend.position="none")
  myplot <- myplot + theme( axis.line = element_line(size = 1, linetype = "solid"))

  # Print rope if passed
  if (printrope) {
    myplot <- myplot + geom_rect(aes(xmin = 3, xmax = 3.69, ymin = s_data$m[2] + rope[1], ymax = s_data$m[2] + rope[2]), alpha = .10)
  }
  
  # Now build up plot -- CIs first, then raw data, then error distribution
  myplot <- myplot + geom_errorbar(data=s_data, aes(ymin=s_data$cilow, ymax = s_data$cihigh), size=1, width = 0, position=position_nudge(x=nudge))
  if(estimate$type == "fromRaw") {
    if(estimate$paired) {
      myplot <- myplot + geom_line(data=estimate$raw_data[estimate$raw_data$iv != "Difference", ], alpha = alpha, aes(x=iv, y=dv, group=id))
      myplot <- myplot + geom_beeswarm(data=estimate$raw_data[estimate$raw_data$iv == "Difference", ], aes(x=iv, y=dv), cex=bcex, size=bsize, fill="white", shape=2)
    } else {
      myplot <- myplot + geom_beeswarm(data=estimate$raw_data, aes(x=iv, y=dv, fill=iv), cex=bcex, size=bsize)
    }
  } else {
    if(estimate$paired) {
      myplot <- myplot + geom_line(data=s_data[s_data$iv != "Difference", ], aes(x=iv, y=m))
    }
  }
  
  if (estimate$paired) {
    myplot <- myplot + geom_segment(color = "black", linetype = "solid", size=1, aes(x=ref1, xend=ref2, y=s_data$m[2], yend=s_data$m[1]))
  }

  myplot <- myplot + geom_cats_eye(data=diffdata, aes(x=iv, y=dv), width=.5,adjust=3,size=0, alpha=.5, position=position_nudge(x=c(nudge[3])))

  # Now reference lines
  myplot <- myplot + geom_segment(color = "black", linetype = "dashed", aes(x=ref1, xend=3.7, y = s_data$m[2], yend=s_data$m[2]))
  myplot <- myplot + geom_segment(color = "black", linetype = "dotted", aes(x=ref2, xend=3.7, y = s_data$m[1], yend=s_data$m[1]))
 
  # Now make the secondary axis floating
  myplot <- myplot + theme( axis.line.y.right = element_blank())
  myplot <- myplot + geom_segment(color="black", linetype="solid", aes(x=3.7,xend=3.7,y=saxisStart, yend=saxisEnd), size=1)
  
  # Now means--they go last so they'll show up on top
  myplot <- myplot + geom_point(data=s_data, size = bsize+1, stroke=1, position= position_nudge(x=nudge))

  # Finally, the labels
  myplot <- myplot + scale_x_discrete(labels = grouplabels)
  #if(estimate$type == "fromRaw") {
    myplot <- myplot + ylab(ylab) + xlab(xlab)
  #}

  return(myplot)
}
