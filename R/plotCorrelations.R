plotEstimatedCorrelation <- function(estimate, ylims = c(-1,1), ylab = NULL, xlab = NULL, fills = c("white"), size = 3) {
  
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
    if (length(fills) != 1) { 
      fills <- c("white")
      warning_string <- stringr::str_interp("'fills' ignored because not a vector of 1 color, instead length of fills = ${length(fills)}.")
      warning(warning_string)
    }
  } else {
    err_string <- stringr::str_interp(
      "'fills' must be a vector, not ${class(fills)}"
    )
    stop(err_string)
  }
  
  
  # Prep for building graph ---------------------------
  # First, make copies of the summary and error data
  s_data <- estimate$plot_info$summary_data
  diffdata <-  estimate$plot_info$error_data

  # Next, set labels
  if(is.null(ylab)) {
    ylab <- estimate$plot_info$dv_name
  } 
  if(ylab == "auto") {
    ylab <- estimate$plot_info$dv_name
  }
  astrik <- ""
  if (estimate$na_count > 0) {astrik <- "*"}
  if(is.null(xlab)) { 
    xlab <- paste(estimate$formatted_r, "\nN = ", estimate$n, astrik, sep="")
  }
  if(xlab == "auto") {
    xlab <- paste(estimate$formatted_r, "\nN = ", estimate$n, astrik, sep="")
  }
  
  # Generate plot ---------------------------
  # Basic plot
  myplot <- ggplot(data=s_data, aes(x=variables, y=r))
  
  # Style options
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  myplot <- myplot + theme(legend.position="none")
  myplot <- myplot + theme( axis.line = element_line(size = 1, linetype = "solid"))
  
  if (!is.null(ylims)) {
    myplot <- myplot + scale_y_continuous(limits = ylims)
  }
  
  myplot <- myplot + geom_hline(yintercept = 0)
  
  # Now build up plot -- CIs first, then raw data, then error distribution
  myplot <- myplot + geom_errorbar(data=s_data, aes(ymin=ci.low, ymax = ci.high), size=1, width = 0)
  myplot <- myplot + geom_cats_eye(data=diffdata, aes(x=iv, y=error), width=.25,adjust=3,size=0, alpha=.5)
  # Now means--they go last so they'll show up on top
  myplot <- myplot + geom_point(data=s_data, size = size, stroke=1, shape = 21, fill = fills[1])
  myplot <- myplot + ylab(ylab) + xlab(xlab)
  
  
  return(myplot)
  
}

plotScatterPlot <- function(estimate, ylims = NULL, xlims = NULL, ylab = NULL, xlab = NULL, fills = c("white"), size = 3, show.line = FALSE, show.meanCI = FALSE, show.PI = FALSE, predictx = NULL) {
  # Check inputs ---------------------------
  # Needs to be of class estimate
  if (class(estimate) != "estimate") {
    err_string <- stringr::str_interp(
      "`estimate` must be class estimate, not ${class(estimate)}"
    )
    stop(err_string)
  }
  
  # Check raw data available
  if (estimate$type != "fromRaw" | is.null(estimate$plot_info$raw_data) | nrow(estimate$plot_info$raw_data) == 0) {
    err_string <- stringr::str_interp(
      "This plot only works for estimates with raw data available."
    )
    stop(err_string)
  }
  
  plotting.groups <- !is.null(estimate$plot_info$group_name)

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
    if (length(fills) != 1) { 
      fills <- c("white")
      warning_string <- stringr::str_interp("'fills' ignored because not a vector of 1 color, instead length of fills = ${length(fills)}.")
      warning(warning_string)
    }
  } else {
    err_string <- stringr::str_interp(
      "'fills' must be a vector, not ${class(fills)}"
    )
    stop(err_string)
  }
  
  # predictx
  if(!is.null(predictx)) {
    if(!is.numeric(predictx)) {
      predictx <- NULL
      warning_string <- stringr::str_interp("'predictx' ignored because it is not numeric, instead it is = ${class(predictx)}.")
      warning(warning_string)
    } else {
      if(predictx > max(estimate$plot_info$raw_data$iv)) {
        predictx <- NULL
        warning_string <- stringr::str_interp("'predictx' ignored because it is out of range.  predictx = ${predictx}, but highest x value is ${max(estimate$plot_info$raw_data$iv)}.")
        warning(warning_string)
      } else {
        if(predictx < min(estimate$plot_info$raw_data$iv)) {
         predictx <- NULL
         warning_string <- stringr::str_interp("'predictx' ignored because it is out of range.  predictx = ${predictx}, but lowest x value is ${min(estimate$plot_info$raw_data$iv)}.")
          warning(warning_string)
        }
      }
    }
  }
  
  
  # Prep for building graph ---------------------------
  # First, make copies of the summary and error data
  s_data <- estimate$plot_info$raw_data
  
  
  # Next, set labels
  if(is.null(ylab)) {
    ylab <- estimate$plot_info$y_name
  } 
  if(ylab == "auto") {
    ylab <- estimate$plot_info$y_name
  }
  astrik <- ""
  if (estimate$na_count > 0) {astrik <- "*"}
  if(is.null(xlab)) { 
    xlab <- paste(estimate$plot_info$x_name, "\n\n", estimate$formatted_r, "\nN = ", estimate$n, astrik, sep="")
    if(show.line) {xlab <- paste(xlab, "\n", estimate$regression_equation, sep="") }
  }
  if(xlab == "auto") {
    xlab <- paste(estimate$plot_info$x_name, "\n\n", estimate$formatted_r, "\nN = ", estimate$n, astrik, "\n", estimate$regression_equation, sep="")
    if(show.line) {xlab <- paste(xlab, "\n", estimate$regression_equation, sep="") }
  }
  
  
  # Build the plot
  if (plotting.groups) {
    myplot <- ggplot(data = s_data, aes(x = iv, y = dv, shape=group, fill = group, color = group))
  } else {
    myplot <- ggplot(data = s_data, aes(x = iv, y = dv))
  }
  
  # Style options
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  if (!plotting.groups) {
    myplot <- myplot + theme(legend.position="none")
  } else {
    myplot <- myplot + scale_fill_brewer(type = "qual")
    myplot <- myplot + scale_color_brewer(type = "qual")
    myplot <- myplot + guides(color=guide_legend(title=estimate$plot_info$group_name))
    myplot <- myplot + guides(shape=guide_legend(title=estimate$plot_info$group_name))
    myplot <- myplot + guides(fill=guide_legend(title=estimate$plot_info$group_name))
  }
  myplot <- myplot + theme( axis.line = element_line(size = 1, linetype = "solid"))
  
  if (!is.null(ylims)) {
    myplot <- myplot + scale_y_continuous(limits = ylims)
  } else {
    yrange <- max(s_data$dv) - min(s_data$dv)
    ylow <- min(s_data$dv) - 0.2*yrange
    yhigh <- max(s_data$dv) + 0.2*yrange
    ylims <- c(ylow, yhigh)
    myplot <- myplot + scale_y_continuous(limits = ylims)
  }
  if (!is.null(xlims)) {
    myplot <- myplot + scale_x_continuous(limits = xlims)
  } else {
    xrange <- max(s_data$iv) - min(s_data$iv)
    xlow <- min(s_data$iv) - 0.2*xrange
    xhigh <- max(s_data$iv) + 0.2*xrange
    xlims <- c(xlow, xhigh)
    myplot <- myplot + scale_x_continuous(limits = xlims)
  }
  
  if (show.line) {myplot <- myplot + geom_smooth(method='lm', se = show.meanCI)}
  
  if (show.PI) {
    myplot <- myplot + geom_line(aes(y=lwr), color = "red", linetype = "dashed")
    myplot <- myplot + geom_line(aes(y=upr), color = "red", linetype = "dashed")
  }
  
  if(plotting.groups) {
    myplot <- myplot + geom_point(size = size)
  } else {
    myplot <- myplot + geom_point(size = size, stroke=1, shape = 21, fill = fills[1])
  }
  
  if(!is.null(predictx)) {
    ypr <- predictx*estimate$b+estimate$a
    yplabel <- paste("Y' = ", format(ypr, digits = 2))
#    xhalf <- predictx - (predictx - min(s_data$iv) ) /2
#    yhalf <- ypr + ypr*.10
    myplot <- myplot + annotate("text", x = min(s_data$iv) - 0.1*xrange, y = ypr, label = yplabel, color = "red")
    xlabel <- paste("X =", predictx)
#    xhalf <- predictx + predictx*.10
    yhalf <- min(s_data$dv) - (0.1* (max(s_data$dv) - min(s_data$dv)) ) 
#    yhalf <- ypr - (ypr - min(s_data$dv) )/2
    myplot <- myplot + annotate("text", x = predictx, y = yhalf, label = xlabel, color = "red")
    pi <- predict(estimate$lm, interval = "prediction", newdata = data.frame(iv = predictx), level = estimate$conf.level)
    xlab <- paste(xlab, "\nAt X =", predictx, ": Y' =", format(ypr, digits = 2), ", ", format(estimate$conf.level*100, digits=0), "% PI[", format(pi[1, "lwr"], digits=2), ",", format(pi[1, "upr"], digits=2), "]")
    myplot <- myplot + geom_segment(alpha = 0.1, size = 1, color = "red", aes(x = predictx, xend = predictx, y=pi[1, "lwr"], yend = pi[1, "upr"]))    
    myplot <- myplot + geom_segment(linetype = "dotted", aes(x = predictx, xend = predictx, y = min(s_data$dv), yend = predictx*estimate$b+estimate$a))
    myplot <- myplot + geom_segment(linetype = "dotted", aes(x = min(s_data$iv), xend = predictx, y = predictx*estimate$b+estimate$a, yend = predictx*estimate$b+estimate$a))
    myplot <- myplot + annotate("point", x = predictx, y = ypr, colour = "red", shape = 23, size=size+1, fill="white")
  }
  
  myplot <- myplot + ylab(ylab) + xlab(xlab)
  

  return(myplot)
}



plotCorrelationDifference <- function(estimate, show.cat.eye = TRUE) {
  
  # General plot setup
  myplot <- ggplot(data = estimate$plot_info$summary_data, aes(x = variables, y = r))
  myplot <- myplot + scale_shape_manual(values=c(19, 19, 17))   # dot, dot, triangle
  
  # Some theme stuff
  myplot <- myplot + theme_classic()
  myplot <- myplot + theme(legend.position="none")
  myplot <- myplot + theme( axis.line = element_line(size = 1, linetype = "solid"))
  
  # Points, error bars, and cat's eye
  myplot <- myplot + geom_point(aes(shape = variables), size = 2)
  myplot <- myplot + geom_errorbar(aes(ymin = ci.low, ymax = ci.high), size=1, width = 0)
  if (show.cat.eye) {
    myplot <- myplot + geom_cats_eye(data=estimate$plot_info$error_data, aes(x=iv, y=error), width=.25,adjust=3,size=0, alpha=.5)
  }
  
  #########################
  #Difference axis 
  # Figure out the span of the floating axis
  saxisStart <- estimate$ci.low
  saxisEnd <- estimate$ci.high
  if (0 < saxisStart) {saxisStart <- 0}
  if (0 > saxisEnd) {saxisEnd <- 0}
  
  # Adjust starting point to be a multiple of 0.1 so that 0 will be a tick mark
  tozero <- ceiling(abs(saxisStart - 0)/.1)
  saxisStart <- -0.1 * tozero
  
  # Make the saxis braks and set the y scale
  saxisBreaks <- seq(saxisStart, saxisEnd, by=0.1)
  myplot <- myplot + scale_y_continuous(limits = c(-1.1,1.1), sec.axis = sec_axis(trans = ~.-estimate$r1$r, breaks = saxisBreaks, labels = scaleFUN))
  
  # Now make the secondary axis floating
  myplot <- myplot + theme( axis.line.y.right = element_blank())
  myplot <- myplot + geom_segment(color="black", linetype="solid", aes(x=3.7,xend=3.7,y=saxisStart+estimate$r1$r, yend=saxisEnd+estimate$r1$r), size=1)
  
  # Mark from the r values to the floating axis
  myplot <- myplot + geom_segment(color="black", linetype="dotted", size = 1, aes(x=1, xend = 3.7, y=estimate$r1$r, yend = estimate$r1$r))
  myplot <- myplot + geom_segment(color="black", linetype="dotted", size = 1, aes(x=2, xend = 3, y=estimate$r2$r, yend = estimate$r2$r))
  myplot <- myplot + geom_segment(color="black", linetype="solid", size = 1, aes(x=3, xend = 3.7, y=estimate$r2$r, yend = estimate$r2$r))
  
  
  
  # Other stuff: reference line at 0, report formatted r in x label
  myplot <- myplot + geom_hline(yintercept = 0)
  myplot <- myplot + xlab(estimate$formatted_r)
  
  return(myplot)
  
}  


#estimate <- estimateCorrelationDifference.default(data = iris, x = Sepal.Length, y = Sepal.Width, group = Species)

#estimate <- estimateCorrelationDifference.numeric(r1 = 0.41, n1 = 59, r2 = -0.41, n2 = 47)
#myplot <- plotScatterPlot(estimate, show.line = TRUE)
#myplot