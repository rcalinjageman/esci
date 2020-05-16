
# This file is a generated template, your changes will not be overwritten

jmvEstimateProportionDifferenceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvEstimateProportionDifferenceClass",
    inherit = jmvEstimateProportionDifferenceBase,
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
"This analysis will estimate the difference between two proportions.
The outcome variable should be a nominal variable with two levels.
The category variable should be a nominal variable measured
in the same participants and also with two levels.

For example, outcome could be 'status: depressed or not depressed'
and category could be 'employment: night shift or regular shift'
and the analysis will compare the proportion depressed in the night shift vs. regular shift.

By default the first level of the outcome variable will be used and compared
against all other levels.  Use 'Case level' to manually select the level for analysis.

Similarly, the first level of the category variable will be used and compared
against all other levels of that variable.  Use 'Classification level' to manually 
select the cateogry level for analysis. 

"
                
                if(is.null(self$options$measure1)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the outcome variable.
")
                }
                

                if(is.null(self$options$measure2)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the category variable.
")
                }
                
                                
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
                
                category.level <- 1
                if (is.null(self$options$category.level)) {
                } else {
                    if (nchar(self$options$category.level) >0) {
                        if (!is.na(as.numeric(self$options$category.level))) {
                            category.level <- as.numeric(self$options$category.level)
                        } else {
                            category.level <- self$options$category.level 
                        }
                    }
                }
                
                if(run.analysis) {
                
                    estimate <- try(estimateProportionDifference.default(data = self$data,
                                                                     !!self$options$measure2,
                                                                     !!self$options$measure1,
                                                                     case.level = case.level,
                                                                     group.level = category.level,
                                                                     conf.level = self$options$conf.level,
                                                                     na.rm = self$options$na.rm
                                                                    ))
                }                
            } else {                
                
                err_string <- 
"This analysis will estimate the difference between two proportions from summary data.
For group 1, define the group label (Label 1), number of cases (Cases 1) and sample size (N1)
For group 2, define the group label (Label 2), number of cases (Cases 2) and sample size (N).
It is also helpul to define the 'Case label' and the 'Not case label'

For example, you might compare depression status among night-shift and day-shift workers.
Group 1 would be labelled 'day shift', and you would enter number depressed (Cases 1) 
out of total day-shift workers (N1).
Group 2 would be labelled 'night shift' and you would enter number depressed (Cases 2)
out fo the total night-shift workers (N2).
Additionally, it would then be helpful to define the 'Case label' as 'Depressed' and the
'Not case label' as 'Not depressed'.

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
                
                grouplabel1 <- "Group 1"
                if (is.null(self$options$grouplabel1)) {
                    
                } else {
                    if (nchar(self$options$grouplabel1) >0) {
                        grouplabel1 <- self$options$grouplabel1 
                    }
                }
                
                grouplabel2 <- "Group 2"
                if (is.null(self$options$grouplabel2)) {
                    
                } else {
                    if (nchar(self$options$grouplabel2) >0) {
                        grouplabel2 <- self$options$grouplabel2 
                    }
                }
                
                estimate <- try(estimateProportionDifference.numeric(cases1 = self$options$cases1, 
                                                                 n1 = self$options$n1, 
                                                                 cases2 = self$options$cases2,
                                                                 n2 = self$options$n2,
                                                                 caselabels = c(caselabel1, caselabel2),
                                                                 grouplabels = c(grouplabel1, grouplabel2),
                                                                 conf.level = self$options$conf.level
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
        
            self$results$text$setVisible(!run.analysis)
            self$results$summary_table$setVisible(run.analysis)
            self$results$proportion_plot$setVisible(run.analysis)
            
            if(!run.analysis) {
                self$results$text$setContent(err_string)
            } else {
                table <- self$results$summary_table
                table$addColumn(name = "cases", title = names(estimate$summary_data)[2], type = 'integer')
                table$addColumn(name = "ncases", title = names(estimate$summary_data)[3], type = 'integer')
                table$addColumn(name = "n", title = "N", type = 'integer')
                table$addColumn(name = "P", title = estimate$plot_info$plotdv, type = 'number')
                table$addColumn(name = "ci.low", 
                                title = "P.ci.low", 
                                type = 'number', 
                                superTitle = paste(format(self$options$conf.level * 100, digits = 0), "% CI") 
                                )
                table$addColumn(name = "ci.high", 
                                title = "P.ci.high", 
                                type = 'number', 
                                superTitle = paste(format(self$options$conf.level * 100, digits = 0), "% CI") 
                                )
                
                table$setRow(rowNo=1, values=list(
                    var=as.character(estimate$summary_data[1,1]),
                    cases = estimate$summary_data[1,2],
                    ncases = estimate$summary_data[1,3],
                    n = estimate$summary_data[1,4],
                    P = estimate$summary_data[1,5],
                    ci.low = estimate$summary_data[1,6],
                    ci.high = estimate$summary_data[1,7]
                ))
                
                table$setRow(rowNo=2, values=list(
                    var=as.character(estimate$summary_data[2,1]),
                    cases = estimate$summary_data[2,2],
                    ncases = estimate$summary_data[2,3],
                    n = estimate$summary_data[2,4],
                    P = estimate$summary_data[2,5],
                    ci.low = estimate$summary_data[2,6],
                    ci.high = estimate$summary_data[2,7]
                ))
    
                table$setRow(rowNo=3, values=list(
                    var=as.character(estimate$summary_data[3,1]),
                    cases = NA,
                    ncases = NA,
                    n = NA,
                    P = estimate$summary_data[3,5],
                    ci.low = estimate$summary_data[3,6],
                    ci.high = estimate$summary_data[3,7]
                ))
                
                
                table$setNote(key="Notes", note = reportEstimate(estimate, section = c("Notes"), print.title = FALSE))
                
                image <- self$results$proportion_plot
                image$setState(estimate)         
            }
        },
        .plot=function(image, ...) {
            estimate <- image$state
            if (is.null(image$state))
                return(FALSE)
            
            
            if (class(estimate) == "estimate") {
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
                
                plot <- plotEstimatedProportionDifference(estimate,
                                                ylab = ylab,
                                                xlab = xlab,
                                                rope = rope
                )
                print(plot)
            }
            TRUE
        })
)
