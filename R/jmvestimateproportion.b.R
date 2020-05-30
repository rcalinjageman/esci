
# This file is a generated template, your changes will not be overwritten

jmvEstimateProportionClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvEstimateProportionClass",
    inherit = jmvEstimateProportionBase,
    private = list(
        .init = function() {
            table <- self$results$summary_table
            table$getColumn("ci.low")$setSuperTitle(paste(self$options$conf.level, "% CI"))
            table$getColumn("ci.high")$setSuperTitle(paste(self$options$conf.level, "% CI"))
            
        },
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            # Set flag to see if we will actually run the analysis
            run.analysis <- TRUE
            estimate <- "b"
            class(estimate) <- "try-error"
            
            
            ##################
            # Run the analysis (from raw or summary)
            
            if (self$options$switch == "fromraw") {
                err_string <- 
"This analysis will estimate proportions of different levels for a nominal variable.
To run the analysis you will specify 'Variable', which should be nominal.  
The analysis will return the proportion and CI for each level of Variable.

By default the first level of the variable will be plotted in the proportion plot.
You can use the 'Case Level' option to define by name or index the level that should 
be plotted in the proportion plot.

"
                
                case.level <- 1
                if (is.null(self$options$case.level)) {
                    
                } else {
                    if (nchar(self$options$case.level) >0) {
                        if (!is.na(as.numeric(self$options$case.level))) {
                            case.level <- as.numeric(self$options$case.level)
                        } else {
                            case.level <- self$options$case.level 
                        }
                    }
                }
            
                if(is.null(self$options$measure1)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define variable (which should be nominal).
")
                }
                
                if(run.analysis) {
                    estimate <- try(estimateProportion.default(data = self$data, 
                                                           !!self$options$measure1, 
                                                           case.level = case.level, 
                                                           conf.level = self$options$conf.level/100, 
                                                           na.rm = (self$options$na.rm == "remove")
                                                           )
                                    )
                }
            } else {

                err_string <- 
"This analysis will estimate a proportion from summary data.
You will enter the number of cases and the total sample size (N).

For example, if there were 22 first-year students in a sample of 100 students
you would enter 22 for cases and 100 for N to estimate the proportion of first-year
students in the population.

It can make results easier to understand to provide a case label and a 'not case' label.
In the example above the case label would be 'first-year student' and 
the not case label would be 'returning student'.

"
                         
                
                caselabel1 <- "Affected"
                if (is.null(self$options$caselabel1)) {
                    
                } else {
                    if (nchar(self$options$caselabel1) >0) {
                        caselabel1 <- self$options$caselabel1 
                    }
                }
                
                caselabel2 <- "Not Affected"
                if (is.null(self$options$caselabel2)) {
                    
                } else {
                    if (nchar(self$options$caselabel2) >0) {
                        caselabel2 <- self$options$caselabel2 
                    }
                }
                
                estimate <- try(estimateProportion.numeric(cases = self$options$cases, 
                                                       n = self$options$n, 
                                                       caselabels = c(caselabel1, caselabel2), 
                                                       conf.level = self$options$conf.level/100
                                ))
                
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
           
            
            ####################
            # Report the results

            if(!run.analysis) {
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            } else {                
                table <- self$results$summary_table

                if (self$options$switch == "fromraw") {
                    counter <- 1
                    for(i in 1:nrow(estimate$all_result)) {
                        table$addRow(rowKey=counter, values=list(
                            var=as.character(estimate$all_result[i, "level"]),
                            cases = estimate$all_result[i, "cases"],
                            n = estimate$all_result[i, "total"],
                            P = estimate$all_result[i, "P"],
                            ci.low = estimate$all_result[i, "ci.low"],
                            ci.high = estimate$all_result[i, "ci.high"]
                        ))
                        counter <- counter + 1
                    }
                    table$setNote(key="Notes", note = gsub("\n", "", reportEstimate(estimate, section = c("Notes"), print.title = FALSE)))
                } else {
                    table$addRow(rowKey=1, values=list(
                        var=estimate$plot_info$plotiv,
                        cases = estimate$summary_data[1,1],
                        ncases = estimate$summary_data[1,2],
                        n = estimate$summary_data[1,3],
                        P = estimate$summary_data[1,4],
                        ci.low = estimate$summary_data[1,5],
                        ci.high = estimate$summary_data[1,6]
                    ))
                }
                  
                #self$results$text$setVisible(TRUE)
                #self$results$text$setContent(estimate$all_result)
                    
                image <- self$results$proportion_plot
                image$setState(estimate)     
                    
                image <- self$results$bar_plot
                image$setState(estimate)   
            }
        },
        .plot=function(image, ...) {
            if (is.null(image$state))
                return(FALSE)
            
            estimate <- image$state
            
            xlab <- NULL
            ylab <- NULL
                
            if (is.null(self$options$xlab)) {
            } else {
                if(nchar(self$options$xlab) > 0 & self$options$xlab != "auto") {
                    xlab <- self$options$xlab                        
                }
            }
                
            if (is.null(self$options$ylab)) {
            } else {
                if(nchar(self$options$ylab) > 0 & self$options$ylab != "auto") {
                    ylab <- self$options$ylab                        
                }
            }
                
                
            rope <- c(NA, NA)
            ropeBottom <- NA
            ropeTop <- NA
            if (is.null(self$options$ropeBottom)) {
            } else {
                if(!is.na(as.numeric(self$options$ropeBottom))) {
                    ropeBottom <- as.numeric(self$options$ropeBottom)
                }
            }
            if (is.null(self$options$ropeTop)) {
            } else {
                if(!is.na(as.numeric(self$options$ropeTop))) {
                    ropeTop <- as.numeric(self$options$ropeTop)
                }
            }
                if (!is.na(ropeBottom) & !is.na(ropeTop)) { rope <- c(ropeBottom, ropeTop)}
                
            plot <- plotEstimatedProportion(estimate,
                                         ylab = ylab,
                                         xlab = xlab,
                                         rope = rope
                )
            print(jmvClearPlotBackground(plot))
            TRUE
        },
        .bplot=function(image, ...) {
            if (is.null(image$state) | self$options$switch == "fromsummary")
                return(FALSE)
            
            estimate <- image$state
            
            if (class(estimate) == "estimate") {
                plot <- barplot(estimate$all_result$cases, names=estimate$all_result$level)
                print(jmvClearPlotBackground(plot))
            }
            TRUE
        }
        )
)
