
# This file is a generated template, your changes will not be overwritten

jmvComplexTwoByTwoClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvComplexTwoByTwoClass",
    inherit = jmvComplexTwoByTwoBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            # Set flag to see if we will actually run the analysis
            run.analysis <- TRUE
            err_string <- "This analysis is for a 2x2 fully between-subjects design.\nSelect a numeric variable for the dependent variable two independent variables.\nRemember each dependent variable should have only 2 levels."
            
            
            if(is.null(self$options$dep) |
               is.null(self$options$group1) |
               is.null(self$options$group2)) {
                run.analysis <- FALSE
            }
            
            # Set visibility of results based on if we are going to run the analysis
            self$results$text$setVisible(!run.analysis)
            self$results$contrast_table$setVisible(run.analysis)
            self$results$means_table$setVisible(run.analysis)
            self$results$ME1_plot$setVisible(run.analysis & self$options$MEorInt == "fME" )
            self$results$ME2_plot$setVisible(run.analysis & self$options$MEorInt == "fME" )
            self$results$Int_plot$setVisible(run.analysis & self$options$MEorInt == "fInt" )
            
            
            if(run.analysis) {
                estimate <- estimateComplex_2x2.default(data = self$data, dv = !!self$options$dep, iv1 = !!self$options$group1, iv2 = !!self$options$group2, conf.level = self$options$conf.level/100)
                
                # Store result opbject for use with the graph
                image <- self$results$ME1_plot
                image$setState(estimate)
                image <- self$results$ME2_plot
                image$setState(estimate) 
                image <- self$results$Int_plot
                image$setState(estimate) 
                
                
                # Setup the means_table, adding a column for each column we will report
                table <- self$results$means_table
                table$addColumn(name = "m", title = "M", type = 'number')
                table$addColumn(name = "moe", title = paste(format(self$options$conf.level, digits = 0), "% MoE"), type = 'number')
                table$addColumn(name = "ci.low", title = "Lower", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "ci.high", title = "Upper", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "s", title = "s", type = 'number')
                table$addColumn(name = "n", title = "N", type = 'integer')
                
                # Set each row (need to write a generic function for filling in jamovi tables)
                for(x in 1:nrow(estimate$means_table)) {
                    table$addRow(x, values = list(
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
                }
                
                # Now the same process with the contrast table--add columns and rows, then fill in table
                table <- self$results$contrast_table
                table$addColumn(name = "m", title = "M", type = 'number')
                table$addColumn(name = "moe", title = paste(format(self$options$conf.level, digits = 0), "% MoE"), type = 'number')
                table$addColumn(name = "ci.low", title = "Lower", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "ci.high", title = "Upper", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "pvalue", title = "p", type = 'number')
                
                for(x in 1:length(corder)) {
                    if(corder[x] == "B") {
                        table$addRow(paste(corder[x], x, sep=""))
                    } else {
                        table$addRow(paste(corder[x], x, sep=""), values = list(
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
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
        },
        .ME1plot=function(image, ...) {  # <-- the plot function
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
