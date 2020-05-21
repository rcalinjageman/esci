
# This file is a generated template, your changes will not be overwritten

jmvEstimateIndMeanDifferenceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvEstimateIndMeanDifferenceClass",
    inherit = jmvEstimateIndMeanDifferenceBase,
    private = list(
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
"This analysis will estimate the mean difference between two independent groups.
To run the analysis you will specify a dependent variable and a grouping variable.
The dependent variable is the outcome and should be on a numeric scale.
The grouping variable defines the groups and should be nominal with only 2 groups.  

This analysis will only compare two groups; additional groups will be ignored.
If you have more than 2 groups consider using 'Estimate - Ind. Groups Contrasts'
to analyze your data.

"
                
                # Analysis is from raw data            
                if(self$options$reference.group) { 
                    reference.group = 2
                } else { 
                    reference.group = 1
                }
                
                if(is.null(self$options$y)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the dependent variable.
")
                }
                
                
                if(is.null(self$options$x)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define the grouping variable.
")
                }
                
                if(run.analysis) {
                    estimate <- try(estimateMeanDifference(self$data, 
                                                       !!self$options$x, 
                                                       !!self$options$y, 
                                                       reference.group = reference.group, 
                                                       paired=FALSE, 
                                                       var.equal = self$options$var.equal, 
                                                       conf.level = self$options$conf.level/100
                                                        ))
                }                    
            } else {
                # Analysis is from summary data
                
                err_string <- 
"This analysis will estimate the mean difference between two independent groups from summary data.
To run the analysis you will the means of both groups (m1 and m2),
the standard deviations for both groups (s1 and s2, both must be positive),
and the sample sizes for both groups (n1 and n2, both must be positive integers).

Under graph options you can provide labels for the groups, which can be helpful for interpreting
output (Group 1 label and Group 2 label).

"
                
                if (is.null(self$options$g1lab)) {g1label <= "Reference Group"} else {g1label <- self$options$g1lab}
                if (is.null(self$options$g2lab)) {g2label <= "Comparison Group"} else {g2label <- self$options$g2lab}
                if (g1label == g2label) {
                    g1label <= "Reference Group"
                    g2label <= "Comparison Group"
                }
                estimate <-  try(estimateMeanDifference(m1 = self$options$m2, m2 = self$options$m1, 
                                                        s1 = self$options$s2, s2 = self$options$s1, 
                                                        n1 = self$options$n2, n2=self$options$n1, 
                                                        paired=FALSE, var.equal = self$options$var.equal, 
                                                        conf.level = self$options$conf.level/100,
                                                        labels = c(g2label, g1label)
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
            
            # Set visibility of results based on if we are going to run the analysis
            self$results$text$setVisible(!run.analysis)
            self$results$ctmTable$setVisible(run.analysis)
            self$results$notes$setVisible(run.analysis)
            self$results$smd$setVisible(run.analysis)
            self$results$decisionMakingText$setVisible(run.analysis)
            self$results$decisionMaking$setVisible(run.analysis)
            self$results$plot$setVisible(run.analysis)
            
            
            if(!run.analysis) {
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            } else {
            
                ####################
                # Report the results
                
                # Analysis ran so set report sections visible
                self$results$text$setVisible(FALSE)            
                self$results$ctmTable$setVisible(TRUE)
                self$results$notes$setVisible(TRUE)
                self$results$smd$setVisible(TRUE)
                self$results$plot$setVisible(TRUE)
                self$results$decisionMakingText$setVisible(TRUE)
                self$results$decisionMaking$setVisible(TRUE)
                
                # Report smd and notes
                self$results$smd$setTitle("Standardized Mean Difference")
                self$results$notes$setTitle("Notes")
                self$results$smd$setContent(paste("<h2>Standardized Mean Difference</h2>", estimate$html_d, estimate$html_summary))
                self$results$notes$setContent(paste("<h2>Notes</h2>", gsub("\n", "</br>", reportEstimate(estimate, "Notes", print.title = FALSE))))
                
                # Fill decision-making table
                table <- self$results$decisionMaking
                table$setRow(rowNo=1, values=list(
                    t=estimate$t,
                    df=estimate$df,
                    p = estimate$p
                ))
                
                # Fill main result table
                table <- self$results$ctmTable
                for (i in 1:3) {
                    table$setRow(rowNo=i, values=list(
                        Condition=paste(estimate$summary_data$iv[i]),
                        m=estimate$summary_data$m[i],
                        CI_low = estimate$summary_data$cilow[i],
                        CI_high = estimate$summary_data$cihigh[i],
                        s=estimate$summary_data$s[i],
                        n=estimate$summary_data$n[i]
                    ))
                }
                
                # Save for image state
                image <- self$results$plot
                image$setState(estimate)
            }
        },
        .plot=function(image, ...) {
            if (is.null(image$state))
                return(FALSE)
            
            estimate <- image$state
            ylims <- NULL
            if(self$options$ymin !="auto" & self$options$ymax != "auto") {
                ylims<-c(as.numeric(self$options$ymin), as.numeric(self$options$ymax))
            }
            rope <- c(NA,NA)
            if(self$options$ropeBottom !="none" & self$options$ropeTop != "none") {
                rope<-c(as.numeric(self$options$ropeBottom), as.numeric(self$options$ropeTop))
            }
            fills <- c(self$options$g1color, self$options$g2color, "gray75")
            
            mylabels <- c(self$options$g1lab, self$options$g2lab)
            if(self$options$switch == "fromraw" & self$options$reference.group) {
                mylabels <- c(self$options$g2lab, self$options$g1lab)
            }
            
            plot <- plotEstimatedDifference(estimate, 
                                            rope = rope, 
                                            ylims = ylims, 
                                            ylab = self$options$ylab, 
                                            xlab = self$options$xlab, 
                                            fills=fills, 
                                            grouplabels = mylabels, 
                                            bsize = self$options$bsize)
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
