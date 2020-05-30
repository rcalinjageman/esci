
# This file is a generated template, your changes will not be overwritten

jmvComplexTwoByTwoClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvComplexTwoByTwoClass",
    inherit = jmvComplexTwoByTwoBase,
    private = list(
        .init = function() {
            tables <- list(self$results$means_table, self$results$contrast_table, self$results$interaction_table)
            for (table in tables) {
                table$getColumn("ci.low")$setSuperTitle(paste(self$options$conf.level, "% CI"))
                table$getColumn("ci.high")$setSuperTitle(paste(self$options$conf.level, "% CI"))
                table$getColumn("moe")$setTitle(paste(self$options$conf.level, "% MoE"))
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
            
            err_string <- 
                "This analysis is for a 2x2 fully between-subjects design.
Select a numeric variable for the dependent variable.
Then select two nominal variables as the two independent variables.
Remember each dependent variable should have only 2 levels.

"
            
            
            if(is.null(self$options$dep)) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "* Waiting for you to define the independent variable.
")
            }
            
            if(is.null(self$options$group1)) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "* Waiting for you to define independent variable 1.
")
            }
            
            if(is.null(self$options$group2)) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "* Waiting for you to define independent variable 2.
")
            }
            
            
            estimate <- try(estimateComplex_2x2.default(data = self$data, 
                                                        dv = !!self$options$dep, 
                                                        iv1 = !!self$options$group1, 
                                                        iv2 = !!self$options$group2, 
                                                        conf.level = self$options$conf.level/100
            )
            )
            
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
            

            if(run.analysis) {
        
                # Store result object for use with the graph
                image <- self$results$ME1_plot
                image$setState(estimate)
                image <- self$results$ME2_plot
                image$setState(estimate) 
                image <- self$results$Int_plot
                image$setState(estimate) 
                
                # Set each row (need to write a generic function for filling in jamovi tables)
                table <- self$results$means_table
                for(x in 1:nrow(estimate$means_table)) {
                    table$setRow(x, values = list(
                        label = as.character(estimate$means_table[x, "label"]),
                        m = estimate$means_table[x, "m"],
                        moe = estimate$means_table[x, "moe"],
                        ci.low = estimate$means_table[x, "ci.low"],
                        ci.high = estimate$means_table[x, "ci.high"],
                        s = estimate$means_table[x, "s"],
                        n = estimate$means_table[x, "n"]
                    ))
                }
                
                
                corder <- c("1", "2", "3", "B", "4", "5", "6")
                if(self$options$MEorInt == "fInt") {
                    corder <- c("10", "11", "12", "B", "13", "14", "15", "B", "7", "8", "9")
                    table <- self$results$interaction_table
                } else {
                    table <- self$results$contrast_table
                }
                
                
                for(x in 1:length(corder)) {
                    if(corder[x] == "B") {
                        table$setRow(rowNo = x, values=NULL)
                    } else {
                        table$setRow(x, values = list(
                            label = as.character(estimate$contrast_table[as.numeric(corder[x]), "label"]),
                            m = estimate$contrast_table[as.numeric(corder[x]), "m"],
                            moe = estimate$contrast_table[as.numeric(corder[x]), "moe"],
                            ci.low = estimate$contrast_table[as.numeric(corder[x]), "ci.low"],
                            ci.high = estimate$contrast_table[as.numeric(corder[x]), "ci.high"],
                            pvalue = estimate$contrast_table[as.numeric(corder[x]), "p.value"]
                        ))
                    }
                }
                
            } else {
                # We didn't run the analysis.  Let's set the error text
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
        },
        .ME1plot=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            
            if(self$options$MEorInt == "fME") {
                estimate <- image$state
                
                ylab = "Dependent Variable"
                if(!is.null(self$options$ylab)) {
                    ylab <- self$options$ylab
                }
                
                plot <- plotContrast(estimate, contrast_number = 1, show.mean.error = self$options$show.mean.error, show.raw.data = self$options$show.raw.data, ylab = ylab)
                print(jmvClearPlotBackground(plot))
                TRUE
            } else {
                FALSE
            }
            
        },
        .ME2plot=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            
            if(self$options$MEorInt == "fME") {
                estimate <- image$state
                
                ylab = "Dependent Variable"
                if(!is.null(self$options$ylab)) {
                    ylab <- self$options$ylab
                }
                
                plot <- plotContrast(estimate, contrast_number = 2, show.mean.error = self$options$show.mean.error, show.raw.data = self$options$show.raw.data, ylab = ylab)
                print(jmvClearPlotBackground(plot))
                TRUE
            } else {
                FALSE
            }   
            
            
        },
        .Intplot=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            if(self$options$MEorInt != "fME") {
                estimate <- image$state
                
                ylab = "Dependent Variable"
                if(!is.null(self$options$ylab)) {
                    ylab <- self$options$ylab
                }
                
                plot <- plotContrast(estimate, contrast_number = 3, show.mean.error = self$options$show.mean.error, show.raw.data = self$options$show.raw.data, ylab = ylab)
                print(jmvClearPlotBackground(plot))
                TRUE
            } else {
                FALSE
            }
            
        })
)
