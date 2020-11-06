
# This file is a generated template, your changes will not be overwritten

jmvDescriptivesClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvDescriptivesClass",
    inherit = jmvDescriptivesBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
            # Set flag to see if we will actually run the analysis
            run.analysis <- TRUE
            estimate <- "b"
            class(estimate) <- "try-error"
            
            err_string <- 
                "This analysis will help you explore the distribution of a numeric variable.  
To run the analysis, specify a numeric measure.  

You can use the analysis options to mark different percentiles in the distribution,
and to mark the mean, median, z-lines, or standard deviation.

"
            
            if(is.null(self$options$measure1)) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "* Waiting for you to define the variable to analyze
")
            }
            
            if(run.analysis) {
                estimate <- estimateMean(
                    data = self$data, 
                    !!self$options$measure1, 
                    conf.level = 0.95, 
                    na.rm = TRUE
                )
            }
            
            if (class(estimate) == "try-error" & run.analysis) {
                run.analysis <- FALSE
                emssg <- estimate[1]
                if (regexpr("\n", emssg) < nchar(emssg)) {
                    emssg <- substr(emssg, regexpr("\n", emssg)+3, nchar(emssg))
                    emssg <- paste("
ERROR:
", emssg)
                    err_string <- paste(err_string, emssg)
                }
            }
            
            
            ####################
            # Report the results
            
            if(!run.analysis) {
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            } else {
                
                table <- self$results$descriptives
                
                table$setRow(rowNo=1, values=list(
                    var=estimate$plot_info$plotiv,
                    m=estimate$m,
                    median = estimate$median,
                    s1 = "",
                    s = estimate$s,
                    min = min(estimate$plot_info$raw_data$dv),
                    max = max(estimate$plot_info$raw_data$dv),
                    q1 = estimate$q1,
                    q3 = estimate$q3,
                    s2 = "",
                    N = estimate$n,
                    NA_count = estimate$na_count
                ))
                
                image <- self$results$distribution
                image$setState(estimate)
                
            }

        },
        .plot=function(image, ...) {
            if (is.null(image$state))
                return(FALSE)
            
            estimate <- image$state
            
            if(self$options$plottype == "dotplot") {
                plottype <- "Dotplot" 
            } else {
                plottype <- "Histogram"
            }
            
            xlab <- jmvSanitizeOption(self$options$xlab)
            ylab <- jmvSanitizeOption(self$options$ylab)
            ymin <- jmvSanitizeOption(self$options$ymin, type.numeric = TRUE)
            ymax <- jmvSanitizeOption(self$options$ymax, type.numeric = TRUE)
            xmin <- jmvSanitizeOption(self$options$xmin, type.numeric = TRUE)
            xmax <- jmvSanitizeOption(self$options$xmax, type.numeric = TRUE)
            
            ylims <- c(ymin, ymax)
            xlims <- c(xmin, xmax)
            
            marker <- jmvSanitizeOption(self$options$marker, type.numeric = TRUE, vrange = c(0,100))
            if(!is.null(marker)) {
                marker <- quantile(estimate$plot_info$raw_data$dv, marker/100)
            }
            bins <- jmvSanitizeOption(self$options$bins, 12, type.numeric = TRUE)
            
            color.regular <- jmvSanitizeOption(self$options$color.regular, "gray")
            color.highlighted <- jmvSanitizeOption(self$options$color.highlighted, "red")
            
            
            
            plot <- plotDistribution(estimate,
                                     type = plottype,
                                     bins = bins,
                                     ylims = ylims,
                                     xlims = xlims,
                                     ylab = ylab,
                                     xlab = xlab,
                                     size = size,
                                     marker = marker, 
                                     show.mean = self$options$show.mean, 
                                     show.median = self$options$show.median,
                                     show.zlines = self$options$show.zlines,
                                     show.s = self$options$show.s,
                                     color.regular = color.regular,
                                     color.highlighted = color.highlighted
                                    )
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)