

plotEstimatedProportion <- function(estimate, ylims = c(0, 1), ylab = NULL, xlab = NULL, rope = c(NA, NA)) {
  
  # Check inputs ---------------------------
  # Needs to be of class estimate
  if (class(estimate) != "estimate") {
    err_string <- stringr::str_interp(
      "`estimate` must be class estimate, not ${class(estimate)}"
    )
    stop(err_string)
  }
  
  
  # Check ylims
  if(!is.null(ylims)) {
    if(is.vector(ylims)) {
      if (length(ylims) != 2) { 
        warning_string <- stringr::str_interp("'ylims' ignored because not a vector of length 2, instead length of ylims = ${length(ylims)}.")
        warning(warning_string)
        ylims <- c(0,1)
      } else {
        if(ylims[1] == "auto" | is.na(ylims[1]) | ylims[2] == "auto" | is.na(ylims[2])) {
          ylims <- c(0,1)
        } else {
          ylims[1] <- as.numeric(ylims[1])
          ylims[2] <- as.numeric(ylims[2])
          if(ylims[2] <= ylims[1]) {
            warning_string <- stringr::str_interp("'ylims' ignored because ymax is < ymin: ymax = ${ylims[2]}, ymin = ${ylims[1]}.")
            warning(warning_string)
            ylims <- c(0,1)
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
  
  #ylab
  if (!is.null(ylab)) {
    if(!is.character(ylab)) {
      ylab <- estimate$plot_info$plotdv
      warning_string <- stringr::str_interp("'ylab' ignored because not a character string, instead ylab class is ${class(ylab)}.")
      warning(warning_string)
    }
  } else {
    ylab <- estimate$plot_info$plotdv
  }
  
  #xlab
  if (!is.null(xlab)) {
    if(!is.character(xlab)) {
      xlab <- estimate$plot_info$plotdv
      warning_string <- stringr::str_interp("'xlab' ignored because not a character string, instead xlab class is ${class(xlab)}.")
      warning(warning_string)
    }
  } else {
    xlab <- estimate$plot_info$plotiv
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
        if(rope[1] < 0 | rope[1] > 1 | rope[2] < 0 | rope[2] > 1) {
          warning_string <- stringr::str_interp(
            "Warning: Ignoring rope parameter.  For plotting a proportion 'rope' must be a vector with values between 0 and 1.  Currently rope is set with ${rope[1]} and ${rope[2]}"
          )
          warning(warning_string)
         printrope <- FALSE
        }
        printrope <- TRUE
      }
    }
  }
  
  s <- estimate$summary_data
  s$dv <- s[[estimate$plot_info$plotdv]]
  s$iv <- 1
  e <- estimate$plot_info$diffdata
  e$iv <- 1
  
  myplot <- ggplot(data=s, aes(x = iv, y=dv)) + ylab(ylab) + xlab(xlab)
  myplot <- myplot + scale_x_continuous(limits = c(0.8, 1.3))
  myplot <- myplot + scale_y_continuous(limits = ylims)
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(axis.text.x=element_blank(), axis.ticks.x = element_blank())
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  if (printrope) {
    myplot <- myplot + geom_rect(aes(xmin = 0.8, xmax = 1.3, ymin = rope[1], ymax = rope[2]), alpha = .10)
  }
  myplot <- myplot + geom_point()
  myplot <- myplot + geom_errorbar(aes(ymin=P.ci.low, ymax = P.ci.high), size=1, width = 0)
  myplot <- myplot +geom_cats_eye(data=e, aes(x=iv, y=error), width=.2,adjust=3,size=0, alpha=.5)
  return(myplot)  
}



plotEstimatedProportionDifference <- function(estimate, ylims = c(0,1), ylab = NULL, xlab = NULL, rope = c(NA, NA), grouplabels = c("auto", "auto"), fills = c("white", "gray75", "gray75"), bsize = 2) {
  
  # Check inputs ---------------------------
  # Needs to be of class estimate
  if (class(estimate) != "estimate") {
    err_string <- stringr::str_interp(
      "`estimate` must be class estimate, not ${class(estimate)}"
    )
    stop(err_string)
  }
  
  
  # Check ylims
  # Currently, ggbeeswarm 0.6 fails when passed in NA, so we must either not use ylim or pass in two valid numbers
  if(!is.null(ylims)) {
    if(is.vector(ylims)) {
      if (length(ylims) != 2) { 
        warning_string <- stringr::str_interp("'ylims' ignored because not a vector of length 2, instead length of ylims = ${length(ylims)}.")
        warning(warning_string)
        ylims <- c(0,1)
      } else {
        if(ylims[1] == "auto" | is.na(ylims[1]) | ylims[2] == "auto" | is.na(ylims[2])) {
          ylims <- c(0,1)
        } else {
          ylims[1] <- as.numeric(ylims[1])
          ylims[2] <- as.numeric(ylims[2])
          if(ylims[2] <= ylims[1] | ylims[1] <0 | ylims[2] < 0 | ylims[1] > 1 | ylims[2] > 1) {
            warning_string <- stringr::str_interp("'ylims' ignored because ymax is < ymin not within range of 0-1: ymax = ${ylims[2]}, ymin = ${ylims[1]}.")
            warning(warning_string)
            ylims <- c(0,1)
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
  } else {
    xlab = "auto"
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
        if(rope[1] < 0 | rope[1] > 1 | rope[2] < 0 | rope[2] > 1) {
          warning_string <- stringr::str_interp(
            "Warning: Ignoring rope parameter.  For plotting a proportion 'rope' must be a vector with values between 0 and 1.  Currently rope is set with ${rope[1]} and ${rope[2]}"
          )
          warning(warning_string)
          printrope <- FALSE
        }
        printrope <- TRUE
      }
    }
  }
  
  #First, make copies of the summary data and error data info
  s <- estimate$plot_info$plot_data
  s$dv <- s[[estimate$plot_info$plotdv]]
  e <- estimate$plot_info$diffdata
  
  s$Group <- factor(s$Group, levels = c(estimate$level2, estimate$level1, "Difference"))
  
  # Now we need to do some transforms so that the difference data is plotted relative to the contrast group
  # Set difference to plot at reference grooup mean and set difference CI relative to there as well
  s[3, "dv"] <- s[1, "dv"]    
  s[3, "P.ci.low"] <- s[1, "dv"] - (s[3, estimate$plot_info$plotdv] - s[3,"P.ci.low" ])
  s[3, "P.ci.high"] <- s[1, "dv"] + (s[3,"P.ci.high" ] - s[3, estimate$plot_info$plotdv])
  anchor <- as.numeric(s[1, "dv"])
  reference <- as.numeric(s[2, "dv"])
  
  #Now adjust all error data relative to the reference, too
  e$differror <- e$differror + (anchor - estimate$Pdiff)
  
  # Now we need to set the labels for the secondary axis
  # We want to mark in pooledsd units by 1/2s, so each tick represents cohen's d change of 0.5
  # We want the axis to span from the reference group up to the end of the error distribution
  # And if the error distribution is very narrow, we want at least +/- 1 pooledsd to be shown
  saxisStart <- estimate$ci.low
  saxisEnd <- estimate$ci.high
  if (0 < saxisStart) {saxisStart <- 0}
  if (0 > saxisEnd) {saxisEnd <- 0}
  
  saxisBreaks <- seq(saxisStart, saxisEnd, by=0.1)
  
  nudge <-c(0,0,0)
  ref1 <- 1
  ref2 <- 2
  
  # Now set the labels for the groups
  if (estimate$na_count > 0) { 
    ast = "*"
  } else {
    ast = ""
  }
  if (grouplabels[1] == "auto") {
    grouplabels[1] <- paste(estimate$level2, "\nN = ", estimate$n1, ast, sep="")
  }
  if (grouplabels[2] == "auto") {
    grouplabels[2] <- paste(estimate$level1, "\nN = ", estimate$n2, ast, sep="")
  }
  grouplabels[3] <- "Difference"
  
  #Labels
  if(is.null(ylab)) {
    ylab <- estimate$plot_info$plotdv
  } else {
    if (ylab == "auto") {
      ylab = "Outcome Variable"
    }
  }
  
  if (xlab == "auto") { 
    xlab <- paste(estimate$plot_info$plotiv, "\n", estimate$formatted_pdiff) 
  }
  
  
  # Generate plot ---------------------------
  # Basic plot
  myplot <- ggplot(data=s, aes(x=Group, y=dv, color=Group, shape=Group, fill=Group))
  # Set secondary axis
  myplot <- myplot + scale_y_continuous(limits = ylims, sec.axis = sec_axis(trans = ~.-reference, breaks = saxisBreaks, labels = scaleFUN))
  
  # Style options
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  myplot <- myplot + scale_color_manual(values = c("black", "black", "black"))  #Outlines
  myplot <- myplot + scale_fill_manual(values = fills)  #Fills
  myplot <- myplot + scale_shape_manual(values=c(19, 19, 17)) #shapes
  myplot <- myplot + theme(legend.position="none")
  myplot <- myplot + theme( axis.line = element_line(size = 1, linetype = "solid"))
  
  # Print rope if passed
  if (printrope) {
    myplot <- myplot + geom_rect(aes(xmin = 3, xmax = 3.69, ymin = s$dv[2] + rope[1], ymax = s$dv[2] + rope[2]), alpha = .10)
  }
  
  # Now build up plot -- CIs first, then raw data, then error distribution
  myplot <- myplot + geom_errorbar(data=s, aes(ymin=P.ci.low, ymax = P.ci.high), size=1, width = 0, position=position_nudge(x=nudge))
  #myplot <- myplot + geom_cats_eye(data=e, aes(x=Group, y=differror), width=.5,adjust=3,size=0, alpha=.5, position=position_nudge(x=c(nudge[3])))
  
  # Now reference lines
  myplot <- myplot + geom_segment(color = "black", linetype = "dashed", aes(x=ref1, xend=3.7, y = s$dv[2], yend= s$dv[2]))
  myplot <- myplot + geom_segment(color = "black", linetype = "dotted", aes(x=ref2, xend=3.7, y = s$dv[1], yend= s$dv[1]))
  
  # Now make the secondary axis floating
  myplot <- myplot + theme( axis.line.y.right = element_blank())
  myplot <- myplot + geom_segment(color="black", linetype="solid", aes(x=3.7,xend=3.7,y=saxisStart+reference, yend=saxisEnd+reference), size=1)
  
  # Now means--they go last so they'll show up on top
  myplot <- myplot + geom_point(data=s, size = bsize+1, stroke=1, position= position_nudge(x=nudge))
  
  # Finally, the labels
  myplot <- myplot + scale_x_discrete(labels = grouplabels)
  #if(estimate$type == "fromRaw") {
  myplot <- myplot + ylab(ylab) + xlab(xlab)
  #}
  
  return(myplot)

}