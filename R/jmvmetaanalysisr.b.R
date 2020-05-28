
# This file is a generated template, your changes will not be overwritten

jmvMetaAnalysisRClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvMetaAnalysisRClass",
    inherit = jmvMetaAnalysisRBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
            
            #Set flag for running analysis and instruction string.
            run.analysis <- TRUE
            err_string <- "For this analysis, setup a data with at least 3 columns: study labels, Pearsons r, and sample sizes.\nYou can also setup a column that represents a categorical (nominal) moderator that varies between studies.\nBe sure to have no blank rows in your data.\n"
            
            # Check if all needed info has been set
            if(is.null(self$options$labels) | 
               is.null(self$options$r) | 
               is.null(self$options$n) ) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "Waiting for you to define variables containing study labels labels, Pearsons r, and sample sizes.", sep="\n")
            }            
            
            
            # Set visibility of results based on if we are going to run the analysis
            self$results$text$setVisible(!run.analysis)
            self$results$result_table$setVisible(run.analysis)
            self$results$study_table$setVisible(run.analysis & self$options$show.study.table)
            self$results$forest_plot$setVisible(run.analysis)
            
            
            if(run.analysis) {
                estimate <- estimateOverallCorrelation(data = self$data, 
                                                       rvalues = !!self$options$r, 
                                                       ns = !!self$options$n,
                                                       label = !!self$options$labels,
                                                       moderator = !!self$options$moderator,
                                                       random.effects = (self$options$REorFE == "RE"),
                                                       conf.level = self$options$conf.level/100)
                
                # Store the estimate for the plot
                image <- self$results$forest_plot
                image$setState(estimate)
                
                # self$results$text$setVisible(TRUE)
                # self$results$text$setContent(estimate)
                
                eslabel <- estimate$effect.size.name
                
                table <- self$results$result_table
                table$addColumn(name = "effect.size", title = eslabel, type = 'number')
                table$addColumn(name = "ci.low", title = "Lower", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "ci.high", title = "Upper", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "p.value", title = "p", type = 'number')
                table$addColumn(name = "diamond.ratio", title = "Diamond Ratio", type = 'number')
                table$addColumn(name = "dr.low", title = "Lower", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI"))
                table$addColumn(name = "dr.high", title = "Upper", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI"))
                # table$addColumn(name = "I2", title = "I^2", type = 'number')
                # table$addColumn(name = "I2.low", title = "I^2.ci.low", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                # table$addColumn(name = "I2.high", title = "I^2.ci.high", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                
                reporttable <- nrow(estimate$result_table)
                if(!is.null(self$options$moderator) & reporttable > 4) { reporttable <- reporttable-1}
                
                for(x in 1:reporttable) {
                    table$addRow(x, values = list(
                        label = as.character(estimate$result_table[x, "label"]),
                        effect.size = estimate$result_table[x, "effect.size"],
                        ci.low = estimate$result_table[x, "ci.low"],
                        ci.high = estimate$result_table[x, "ci.high"],
                        p.value = estimate$result_table[x, "p.value"],
                        diamond.ratio = estimate$result_table[x, "diamond.ratio"],
                        dr.low = estimate$result_table[x, "dr.low"],
                        dr.high = estimate$result_table[x, "dr.high"]
                    ))}
                

                # I2 = estimate$result_table[x, "I2"],
                # I2.low = estimate$result_table[x, "I2.low"],
                # I2.high = estimate$result_table[x, "I2.high"]
                
                                
                if(self$options$show.study.table) {
                    table <- self$results$study_table
                    table$addColumn(name = "effect.size", title = eslabel, type = 'number')
                    table$addColumn(name = "ci.low", title = "Lower", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                    table$addColumn(name = "ci.high", title = "Upper", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                    table$addColumn(name = "N", title = "N", type = 'number')
                    
                    for(x in 1:nrow(estimate$data)) {
                        table$addRow(x, values = list(
                            label = as.character(estimate$data[x, "label"]),
                            effect.size = estimate$data[x, "effect.size"],
                            ci.low = estimate$data[x, "ci.low"],
                            ci.high = estimate$data[x, "ci.high"],
                            N = estimate$data[x, "n"]
                        ))}
                }
                
                
            } else {
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
        },
        .plot=function(image, ...) {  # <-- the plot function
            estimate <- image$state
            
            plot <- plotMetaEffect(estimate, xlims = c(-1, 1))
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
