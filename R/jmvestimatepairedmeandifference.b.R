
# This file is a generated template, your changes will not be overwritten

jmvEstimatePairedMeanDifferenceClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvEstimatePairedMeanDifferenceClass",
    inherit = jmvEstimatePairedMeanDifferenceBase,
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
"This analysis will estimate the mean difference between two paired measures.
To run the analysis you will specify measure 1 and measure 2.  
Measure 1 should be a numeric variable.
Measure 2 should also be a numeric variable and should be measured
from the same set of participants/observations as measure 1.  

"
                if(is.null(self$options$measure1)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define measure 1.
")
                }
                
                
                if(is.null(self$options$measure2)) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "* Waiting for you to define measure 2.
")
                }
                
                
                # Analysis is from raw data            
                if(self$options$reference.group) { 
                    reference.group = 2
                } else { 
                    reference.group = 1
                }
                
                if(run.analysis) {
                    estimate <- try(estimate <- estimateMeanDifference(self$data, 
                                                                       !!self$options$measure1, 
                                                                       !!self$options$measure2, 
                                                                       reference.group = reference.group, 
                                                                       paired=TRUE, 
                                                                       var.equal = self$options$var.equal, 
                                                                       conf.level = self$options$conf.level/100
                                                                       )
                                    )
                }                
            } else {
                
                err_string <- 
"This analysis will estimate the mean difference between two paired measures from summary data.
To run the analysis you will specify the mean and standard deviation for measure 1 (m1 and s1)
and the mean and standard deviation for measure 2 (m2 and s2)
and the sample size (N) and the correlaiton between measures (r).  
(if you don't have the correlation between measures you might be able to recover it from data or test statistics)

Under graph options you can provide labels for measure 1 and measure 2, which can be helpful for interpreting
output (Measure 1 label and Measure 2 label).

"
 
                # Analysis is from summary data
                if (is.null(self$options$g1lab)) {g1label <= "Reference Group"} else {g1label <- self$options$g1lab}
                if (is.null(self$options$g2lab)) {g2label <= "Comparison Group"} else {g2label <- self$options$g2lab}
                if (g1label == g2label) {
                    g1label <= "Reference Group"
                    g2label <= "Comparison Group"
                }
                
                estimate <-  try(estimate <- estimateMeanDifference(m1 = self$options$m2, m2 = self$options$m1, 
                                                                    s1 = self$options$s2, s2 = self$options$s1, 
                                                                    n1 = self$options$n1, 
                                                                    r = self$options$r,
                                                                    paired=TRUE, var.equal = self$options$var.equal, 
                                                                    conf.level = self$options$conf.level/100,
                                                                    labels = c(g2label, g1label)
                                                                    )
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
            
            ####################
            # Report the results
            
            # Analysis ran so set report sections visible
            self$results$text$setVisible(!run.analysis)            
            self$results$ctmTable$setVisible(run.analysis)
            self$results$notes$setVisible(run.analysis)
            self$results$smd$setVisible(run.analysis)
            self$results$plot$setVisible(run.analysis)
            self$results$decisionMakingText$setVisible(run.analysis)
            self$results$decisionMaking$setVisible(run.analysis)
            
            if(!run.analysis) {
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            } else {
                # Report smd and notes
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
                
                table$addColumn(name = "m", title = "M", type = 'number')
                table$addColumn(name = "CI_low", 
                                title = "Lower", 
                                type = 'number', 
                                superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") 
                )
                table$addColumn(name = "CI_high", 
                                title = "Upper", 
                                type = 'number', 
                                superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") 
                )
                table$addColumn(name = "s", title = "s", type = 'number')
                table$addColumn(name = "n", title = "N", type = 'integer')
                
                
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
            fills <- c(self$options$g1color, self$options$g1color, "gray75")
            
            mylabels <- c(self$options$g2lab, self$options$g1lab)
            if(self$options$switch == "fromraw" & !self$options$reference.group) {
                mylabels <- c(self$options$g1lab, self$options$g2lab)
            }
            
            plot <- plotEstimatedDifference(estimate, 
                                            ylims = ylims, 
                                            rope=rope, 
                                            alpha = self$options$alpha, 
                                            ylab = self$options$ylab, 
                                            xlab = self$options$xlab,  
                                            grouplabels = mylabels, 
                                            fills=fills, 
                                            bcex = self$options$bcex, 
                                            bsize = self$options$bsize
                                            )
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
