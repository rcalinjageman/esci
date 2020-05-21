
# This file is a generated template, your changes will not be overwritten

jmvEstimateCorrelationClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvEstimateCorrelationClass",
    inherit = jmvEstimateCorrelationBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            
            ##################
            # Run the analysis (from raw or summary)
 
            # Set flag to see if we will actually run the analysis
            run.analysis <- TRUE
            estimate <- "b"
            class(estimate) <- "try-error"
            
                       
            if (self$options$switch == "fromraw") {
                err_string <- 
"This analysis will explore the linear relationship between two numeric variables.  
To run the analysis enter a variable to predict (Y), which should be numeric.
Also enter a predictor variable (X), which should be numeric and measured in 
the same set of observations.

The analysis will calculate the linear correlation coefficient (r) with a CI.
In addition, you'll have a scatterplot.
Click 'Show regression line' to obtain the regression line and equation.
Click 'Show CI on regression line' to see the uncertainty in the regression line.
Click 'Show Prediction Intervals' to see uncertainty in prediction.

Finally, enter an X value to obtain a predicted Y value and prediction interval.
Be sure the X value you enter is within the range of the X values used for the analysis.

"
                
                if(is.null(self$options$dv)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the variable to predict (Y).
")
                }
                
                if(is.null(self$options$iv)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the predictor variable (X).
")
                }
                

                if(run.analysis) {                
                    estimate <- try(estimateCorrelation.default(data = self$data, 
                                                            !!self$options$iv, 
                                                            !!self$options$dv, 
                                                            conf.level = self$options$conf.level/100)
                                    )
                }
            } else {
                err_string <- 
"This analysis will calculate confidence interval on a linear relationship.  
Enter a correlation coefficient (r)
and also enter the sample size (N).

The analysis will provide the confidence interval on r.  

"
                
                
                estimate <- try(estimateCorrelation.numeric(r = self$options$r,
                                                        n = self$options$n,
                                                        conf.level = self$options$conf.level/100)
                                )
            }
            
            ###################
            # Check to see if the analysis ran
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
            
            self$results$text$setVisible(!run.analysis)
            self$results$scatter_plot$setVisible(self$options$switch == "fromraw" & run.analysis)
            self$results$summary_table$setVisible(run.analysis)
            self$results$correlation_plot$setVisible(run.analysis)
            
            if(!run.analysis) {
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            } else {
            
                table <- self$results$summary_table
                table$addColumn(name = "r", title = "Pearson's r", type = 'number')
                table$addColumn(name = "ci.low", 
                                title = "ci.low", 
                                type = 'number', 
                                superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") 
                                )
                table$addColumn(name = "ci.high", 
                                title = "ci.high", 
                                type = 'number', 
                                superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") 
                                )
                table$addColumn(name = "n", title = "N", type = 'integer')
    
                table$setRow(rowNo=1, values=list(
                    var=as.character(estimate$summary_data[1,1]),
                    r = estimate$summary_data[1,2],
                    ci.low = estimate$summary_data[1,4],
                    ci.high = estimate$summary_data[1,5],
                    n = estimate$summary_data[1,6]
                ))
                
                if(self$options$show.line) {
                    table$setNote(key="Notes", note = estimate$regression_equation)
                }
                            
                image <- self$results$scatter_plot
                image$setState(estimate)   
                
                image <- self$results$correlation_plot
                image$setState(estimate)   
            }
        },
        .plot=function(image, ...) {
            estimate <- image$state
            if (is.null(image$state))
                return(FALSE)

            xlab <- jmvSanitizeOption(self$options$xlab)
            ylab <- jmvSanitizeOption(self$options$ylab)
            ymin <- jmvSanitizeOption(self$options$ymin, type.numeric = TRUE)
            ymax <- jmvSanitizeOption(self$options$ymax, type.numeric = TRUE)
            xmin <- jmvSanitizeOption(self$options$xmin, type.numeric = TRUE)
            xmax <- jmvSanitizeOption(self$options$xmax, type.numeric = TRUE)
            
            ylims <- c(ymin, ymax)
            xlims <- c(xmin, xmax)
            
            predictx <- NULL
            if(!is.null(self$options$predictx)) {
                if(nchar(self$options$predictx) > 0) {
                    if(self$options$predictx != "none") {
                        if(!is.na(as.numeric(self$options$predictx))) {
                            predictx <- as.numeric(self$options$predictx)
                        }
                    }
                }
            }
            
            if(image$name == "scatter_plot") {
                if (self$options$switch == "fromraw") {
                plot <- plotScatterPlot(estimate,
                                        ylims = ylims,
                                        xlims = xlims,
                                        xlab = xlab,
                                        ylab = ylab,
                                        size = self$options$size,
                                        show.line = self$options$show.line,
                                        show.meanCI = self$options$show.meanCI,
                                        show.PI = self$options$show.PI,
                                        predictx = predictx, 
                )
                } else {
                    return(FALSE)
                }
            } else {
                plot <- plotEstimatedCorrelation(estimate)
            }
            print(jmvClearPlotBackground(plot))

            TRUE
        })
)
