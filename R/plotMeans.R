plotDistribution <- function(estimate, type = "Dotplot", ylims = NULL, xlims = NULL, ylab = NULL, xlab = NULL, bins = 12, size = 0.5, marker = NULL, show.mean = FALSE, show.median = FALSE, show.zlines = FALSE, show.s = FALSE, color.regular = "gray", color.highlighted = "red") {
  # This function is really crazy now -- need to rewrite
  # The use of ggforce to create a proper dotplot was taken from https://stackoverflow.com/questions/53697235/ggplot-dotplot-what-is-the-proper-use-of-geom-dotplot
  #   From the answer by Tjebo
  # The goal is to replicate the functionaility of "Describe" tab in ESCI as closely as possible
  # This will work for now, but some serious re-factoring is needed things more sensible

  # Check type
  if (is.null(type)) {type <- "Dotplot"}
  
  # If no x or y labels are passed, set them to their default values  
  if(is.null(ylab)) {
    ylab <- "Count"
  }
  if(is.null(xlab)) {
    xlab <- estimate$plot_info$plotiv
  }
  
  if (bins > length(unique(estimate$plot_info$raw_data$dv))) {
    bins <- length(unique(estimate$plot_info$raw_data$dv))
  }

 # Set the fills.  
 # One problem is if the marker is above or below range only 1 level of the include factor
 # If above range, that's ok, everything is highlighted.  But if below the range it also makes everything highlighted
 # So this sets up the two-color for highlighting only if the marker is above the min of data
 fills <- c(color.regular, color.regular)
 if(is.null(marker)) {
    estimate$plot_info$raw_data$included <- TRUE
  } else {
    estimate$plot_info$raw_data$included <- estimate$plot_info$raw_data$dv > marker
    if (nrow(estimate$plot_info$raw_data[estimate$plot_info$raw_data$included == FALSE, ]) > 0) {
     fills <- c(color.highlighted, color.regular)
    }
  }

 # Had to do these calculations for the histogram myself
 # Because in ggplot if you just set bin number it uses the whole x axis, and so if you expand that axis at all, results are terrible
 # So we get the min and max of the data, make a bind width, and then define the bin breaks using the seq function
 dmin <- min(estimate$plot_info$raw_data$dv)
 dmax <- max(estimate$plot_info$raw_data$dv)
 bin_width <- (dmax - dmin)/bins
 hbreaks <- seq(dmin, dmax, by=bin_width)
 
 pt_width <- bin_width / 2.2 # so that they don't touch horizontally
 pt_height <- bin_width / 2 # 2 so that they will touch vertically
 
 if (is.null(xlims) | length(xlims) != 2) {
   xlims <- c(dmin, dmax)
 }
 
 # Now we make the basic plot
 myplot <- ggplot(data = estimate$plot_info$raw_data, aes(x = dv))  
 if (type == "Histogram") {
   myplot <- myplot + geom_histogram(breaks = hbreaks, col = "black", aes(fill = included))
   myplot <- myplot + ggplot2::coord_cartesian(clip = "off")
   top <- max(ggplot_build(myplot)$data[[1]]$count)

 } else {
   
   

   
   d_max <- max(estimate$plot_info$raw_data$dv)
   d_min <- min(estimate$plot_info$raw_data$dv)
   d_range <- d_max - d_min
   breaks <- d_range/bin_width
   
   count_data <- hist(estimate$plot_info$raw_data$dv, breaks = breaks, plot = FALSE)
   count_data$breaks <- head(count_data$breaks, -1)
   
   xs <- NULL
   ys <- NULL
   for (c_break in 1:length(count_data$breaks)) {
     if(count_data$counts[c_break] != 0) {
       xs <- c(xs, rep(count_data$breaks[c_break], count_data$counts[c_break]))
       ys <- c(ys, seq(1:count_data$counts[c_break]))
     }
   }
   ys <- ys - 0.5
   count_data <- data.frame(x = xs, y = ys)
   top <- max(count_data$y)
   
   count_data$included <- TRUE
   if(!is.null(marker)) { count_data$included <- count_data$x > marker }
   myplot <- ggplot(count_data) +
     ggforce::geom_ellipse(aes(
       x0 = x,
       y0 = y,
       fill = included,
       a = pt_width / bin_width,
       b = pt_height / bin_width,
       angle = 0
     )) + 
     coord_equal((1 / pt_height) * pt_width, clip = "off")

 }
 

 bigtop <- ceiling((top+3)/10)*10
 if (is.null(ylims) | length(ylims) != 2) {
   ylims <- c(0, bigtop)
 }
 
 
 if (show.mean) {
  # estimate$plot_info$plot_data$y = -1
  m_data <- data.frame(x = mean(estimate$m), 
                       y = 0-((pt_height / bin_width)*1.5), 
                       sides = 3, 
                       r = (pt_height / bin_width)*1.5,
                       angle = 0
                       )

  if (type == "Histogram") {
    myplot <- myplot + geom_point(fill = "green", shape = 24, size = 6, alpha = 0.5,
                                x = estimate$m, y = 0-0.5)
  } else {
  myplot <- myplot + ggforce::geom_regon(fill = "green", 
                                         data = m_data,
                                         alpha = 0.5,
                                         aes(x0 =  x, 
                                             y0 = y,
                                             sides = sides, 
                                             r = r,
                                             angle = angle)
                                         )
  }
  # myplot <- myplot + geom_point(fill = "green", shape = 24, size = 6, x = mean(estimate$plot_info$raw_data$dv), y  = -1)
  # myplot <- myplot +   annotate("segment", x=mean(estimate$plot_info$raw_data$dv), y=-1, xend=mean(estimate$plot_info$raw_data$dv), yend=0,
  #                               col="green", arrow=arrow(length=unit(0.3, "cm")))
  # myplot <- myplot + geom_segment(color = "black", linetype = "dashed", aes(x = estimate$m, xend = estimate$m, y = 0, yend = top))
  # myplot <- myplot + geom_segment(color = "black", linetype = "solid", aes(x = estimate$ci.low, xend = estimate$ci.high, y = 0.6, yend = 0.6))

 }
 if (show.median) {
  estimate$plot_info$plot_data$y = top+1
  #myplot <- myplot + geom_segment(color = "black", linetype = "dashed", aes(x = estimate$median, xend = estimate$median, y = 0, yend = top))
  myplot <- myplot + geom_point(data = estimate$plot_info$plot_data, fill = "pink", shape = 23, size = 5, aes(x = median, y = y))
 }
 if (show.s) {
   sdf <- data.frame(s = c(estimate$m - estimate$s, estimate$m + estimate$s))
   sdf$y <- top+2
   myplot <- myplot + geom_segment(color = "black", linetype = "solid", aes(x = estimate$m - estimate$s, xend = estimate$m + estimate$s, y = top+2, yend = top+2))
   myplot <- myplot + geom_point(data = sdf, fill = "blue", shape=25, size = 5, aes(x = s, y = y))
 }
 
 if (show.zlines) {
   for (i in -3:3) {
     cz <- estimate$m + (estimate$s * i)
     myplot <- myplot + geom_segment(color = "black", linetype = "dotted", x = cz, xend = cz, y = 0, yend = top+3)
     # myplot <- myplot + geom_vline(color = "black", linetype = "dotted", xintercept = cz)
   }
   if (xlims[1] > estimate$m + (estimate$s * -3)) xlims[1] <- estimate$m + (estimate$s * -3)
   if (xlims[2] < estimate$m + (estimate$s * 3)) xlims[2] <- estimate$m + (estimate$s * 3)
   zdata <- data.frame(z = c(-3:3))
   zdata$xdata <- estimate$m + (zdata$z * estimate$s)
   zdata$ydata <- bigtop-2
   if (type != "Histogram") { zdata$ydata <- 0}
   zdata$zlabel <- "z ="
   zdata$zlabel <- paste(zdata$zlabel, zdata$z)
   myplot <- myplot + geom_text(data = zdata, y = top+4, aes(x=xdata, label=zlabel))
 }


  myplot <- myplot +  scale_y_continuous(expand = c(0,0))
 
  
 # if (type == "Histogram") {
 # } else {
 #   myplot <- myplot + scale_y_continuous(expand = c(0,0))
 #   myplot <- myplot + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
 # }
 
  # if (!is.null(xlims)) {
  #   myplot <- myplot + scale_x_continuous(limits = xlims)
  # }
  
  if (type == "Histogram") {
    myplot <- myplot + coord_cartesian(ylim = ylims, xlim = xlims, clip = "off")
  } else {
    myplot <- myplot  + coord_equal((1 / pt_height) * pt_width, xlim = xlims, ylim = ylims, clip = "off")
  }
  
  myplot <- myplot + scale_fill_manual(values = fills)
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(legend.position="none")
  myplot <- myplot + labs(x = xlab, y = ylab)
  
  # if (show.mean) {
  #   myplot <- myplot + theme(axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  # }
  # if (type != "Histogram") {
  #   myplot <- myplot + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
  # }  
  
  return(myplot)
}  

plotEstimatedMean <- function(estimate, ylims = NULL, ylab = NULL, xlab = NULL, fills = c("white", "gray75"), bcex = 1, bsize = 2) {
  
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
    if (length(fills) != 2) { 
      fills <- c("white", "gray75")
      warning_string <- stringr::str_interp("'fills' ignored because not a vector of 2 colors, instead length of fills = ${length(fills)}.")
      warning(warning_string)
    }
  } else {
    err_string <- stringr::str_interp(
      "'fills' must be a vector, not ${class(fills)}"
    )
    stop(err_string)
  }
  

  # Prep for building graph ---------------------------
  # First, make copies of the summary, raw, and error data
  s_data <- estimate$plot_info$plot_data
  diffdata <-  estimate$plot_info$error_data
  diffdata$dv <- diffdata$dv + estimate$m
  rdata <- estimate$plot_info$raw_data

  # Next, set labels
  if(is.null(ylab)) {
    ylab <- estimate$plot_info$plotiv 
  } 
  if(ylab == "auto") {
    ylab <- estimate$plot_info$plotiv
  }
  astrik <- ""
  if (estimate$na_count > 0) {astrik <- "*"}
  if(is.null(xlab)) { 
    xlab <- paste(estimate$formatted_mean, "\nN = ", estimate$n, astrik, sep="")  
  }
  if(xlab == "auto") {
    xlab <- paste(estimate$formatted_mean, "\nN = ", estimate$n, astrik, sep="")  
  }
  
  # Generate plot ---------------------------
  # Basic plot
  myplot <- ggplot(data=s_data, aes(x=Variable, y=m))
  
  # Style options
  myplot <- myplot + theme_classic()  # Classic theme
  myplot <- myplot + theme(plot.margin=unit(c(0, 1, 0, 1),"in"))
  myplot <- myplot + theme(legend.position="none")
  myplot <- myplot + theme( axis.line = element_line(size = 1, linetype = "solid"))
  
  if (!is.null(ylims)) {
    myplot <- myplot + scale_y_continuous(limits = ylims)
  }
  
  # Now build up plot -- CIs first, then raw data, then error distribution
  myplot <- myplot + geom_errorbar(data=s_data, aes(ymin=ci.low, ymax = ci.high), size=1, width = 0, position=position_nudge(x=.25))
  myplot <- myplot + geom_beeswarm(data=rdata, aes(x=iv, y=dv), fill = fills[1], shape = 21, cex=bcex, size=bsize)
  myplot <- myplot + geom_cats_eye(data=diffdata, aes(x=iv, y=dv), width=.25,adjust=3,size=0, alpha=.5, position=position_nudge(x=c(.25)))
  # Now means--they go last so they'll show up on top
  myplot <- myplot + geom_point(data=s_data, size = bsize+1, stroke=1, shape = 21, fill = fills[2], position= position_nudge(x=.25))
  myplot <- myplot + ylab(ylab) + xlab(xlab)
  
  
  return(myplot)
  
}
