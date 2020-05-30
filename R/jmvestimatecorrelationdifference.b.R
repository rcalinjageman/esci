
# This file is a generated template, your changes will not be overwritten

jmvEstimateCorrelationDifferenceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvEstimateCorrelationDifferenceClass",
    inherit = jmvEstimateCorrelationDifferenceBase,
    private = list(
        .init = function() {
            tables <- list(self$results$result_table, self$results$allrs)
            for(table in tables) {
                table$getColumn("ci.low")$setSuperTitle(paste(self$options$conf.level, "% CI"))
                table$getColumn("ci.high")$setSuperTitle(paste(self$options$conf.level, "% CI"))
            }
            
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
"This analysis will estimate the difference in correlation across two independent groups.
To run the analysis you will specify the variables to correlate (Y and X); both should be numeric.
You will also specify a categorical moderator measured in the same participants; this should be nominal.

The analysis will calculate the difference between correlations across the first
two levels of the cateogrical moderator.  If the categorical moderator has more than 
2 levels, r for each additional level will also be reported.


"
                
                if(is.null(self$options$iv)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the predictor variable (X).
")
                }

                
                if(is.null(self$options$dv)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the variable to be predicted (Y).
")
                }
                
                if(is.null(self$options$group)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the categorical moderator.
")
                }
                
                if(run.analysis) {
                    estimate <- try(estimateCorrelationDifference.default(data = self$data, 
                                                                      x = !!self$options$iv, 
                                                                      y = !!self$options$dv, 
                                                                      group = !!self$options$group,
                                                                      conf.level = self$options$conf.level/100
                    ))
                }
            } else {
                
                err_string <-
"This analysis will use summary data to estimate the difference in correlation across
two independent groups.
For group 1, specify the name of the group (Label 1), the correlation observed in that 
group (r1),and the sample size for that group (N1).
For group 2, specificy the name of the group (Label 2), the correlation observed in that
group (r2), and the sample size for that group (N2).

It can also help to give labels for the variables being correlated (Variable 1 Label 
and Variable 2 Label)

For example, one might compare have a report of the correlation between body image
and life satisfaction broken down by self-reported gender.  From this, you would want
to estimate how different these correlations are.  In this case, under group 1 you 
might specify the label 'Identify as male' and enter the correlation and sample-size for
the group that identifies as male.  Group 2 might then be labelled 'Identify as female'
and you would enter r2 and N2 as the correlation and sample-size for those that
identify as famale.  Under Variable 1 Label and Variable 2 Label you would then enter
'Body image' and 'Life satisfiaction'.  Entering the variable labels is optional but
can help make the results easier to understand.

"
                
                estimate <- try(estimateCorrelationDifference.numeric(r1 = self$options$r1,
                                                                  n1 = self$options$n1,
                                                                  r2 = self$options$r2,
                                                                  n2 = self$options$n2,
                                                                  group.labels = c(self$options$grouplabel1, self$options$grouplabel2),
                                                                  variable.labels = c(self$options$varlabel1, self$options$varlabel2),
                                                                  conf.level = self$options$conf.level/100
                ))
                
            }
            
            ###################
            # Check to see if the analysis ran
            if (class(estimate) == "try-error" & run.analysis) {
                emssg <- estimate[1]
                run.analysis <- FALSE
                if (regexpr("\n", emssg) < nchar(emssg)) {
                    emssg <- substr(emssg, regexpr("\n", emssg)+3, nchar(emssg))
                    emssg <- paste("
ERROR:
", emssg)
                    err_string <- paste(err_string, emssg)
                }
            }
            

            if(!run.analysis) {
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            } else {
                
                                
                # Store the estimate for the plots
                image <- self$results$scatter_plot
                image$setState(estimate)
                image <- self$results$correlation_plot
                image$setState(estimate)
                    
                # Fill comparison result table - set columns and fill
                table <- self$results$result_table

                reporttable <- nrow(estimate$summary_data)
                for(x in 1:reporttable) {
                    table$setRow(x, values = list(
                                variables = as.character(estimate$summary_data[x, "variables"]),
                                r = estimate$summary_data[x, "r"],
                                ci.low = estimate$summary_data[x, "ci.low"],
                                ci.high = estimate$summary_data[x, "ci.high"],
                                n = estimate$summary_data[x, "n"]
                    ))
                }
                    
                if(self$options$switch == "fromraw") {    
                    table <- self$results$allrs                    
                            
                    reporttable <- nrow(estimate$allrs)
                    for(x in 1:reporttable) {
                        table$addRow(x, values = list(
                                    group = as.character(estimate$allrs[x, "group"]),
                                    variables = as.character(estimate$allrs[x, "variables"]),
                                    r = estimate$allrs[x, "r"],
                                    ci.low = estimate$allrs[x, "ci.low"],
                                    ci.high = estimate$allrs[x, "ci.high"],
                                    n = estimate$allrs[x, "n"]
                        ))
                    }
                }
                                    
            }
        },
        .plotSP=function(image, ...) {  # <-- the plot function
            if (is.null(image$state) | self$options$switch != "fromraw")
                return(FALSE)
    
            estimate <- image$state
            plot <- plotScatterPlot(estimate, show.line = TRUE)                
            print(jmvClearPlotBackground(plot))
            TRUE
        },
        .plotCP=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            
            estimate <- image$state
            plot <- plotCorrelationDifference(estimate, show.cat.eye = FALSE)
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
