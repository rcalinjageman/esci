
# This file is a generated template, your changes will not be overwritten

jmvMetaAnalysisRawClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvMetaAnalysisRawClass",
    inherit = jmvMetaAnalysisRawBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            #Set flag for running analysis and instruction string.
            run.analysis <- TRUE
            err_string <- "For this analysis, setup a data with at least 7 columns: study labels, group 1 means, group 1 standard deviations, group 1 sample sizes, group 2 means, group 2 standard deviations, and group 2 sample sizes.\nYou can also setup a column that represents a categorical (nominal) moderator that varies between studies.\nBe sure to have no blank rows in your data.\n"
            
            # Check if all needed info has been set
            if(is.null(self$options$labels) | 
               is.null(self$options$m1) | 
               is.null(self$options$s1) | 
               is.null(self$options$n1) |
               is.null(self$options$m2) | 
               is.null(self$options$s2) | 
               is.null(self$options$n2) ) {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, "Waiting for you to define variables containing study labels labels and means, sds, and ns for each group.", sep="\n")
            }            
            
            
            # Set visibility of results based on if we are going to run the analysis
            self$results$text$setVisible(!run.analysis)
            self$results$result_table$setVisible(run.analysis)
            self$results$study_table$setVisible(run.analysis & self$options$show.study.table)
            self$results$forest_plot$setVisible(run.analysis)
            
            
            if(run.analysis) {
                estimate <- estimateOverallRaw(data = self$data, 
                                               m1 = !!self$options$m1, 
                                               m2 = !!self$options$m2,
                                               s1 = !!self$options$s1,
                                               s2 = !!self$options$s2,
                                               n1 = !!self$options$n1,
                                               n2 = !!self$options$n2,
                                               label = !!self$options$labels,
                                               moderator = !!self$options$moderator,
                                               random.effects = (self$options$REorFE == "RE"),
                                               report.cohens.d = self$options$report.cohens.d,
                                               conf.level = self$options$conf.level/100)
                
                # Store the estimate for the plot
                image <- self$results$forest_plot
                image$setState(estimate)
                
                # self$results$text$setVisible(TRUE)
                # self$results$text$setContent(estimate$result_table)
                
                eslabel <- "M2-M1"
                if(self$options$report.cohens.d) {
                    eslabel <- "Cohen's d (unbiased)"
                } else {
                    if(length(self$options$g1label)>0 & length(self$options$g2label) >0) {
                        eslabel <- paste(self$options$g2label, "-", self$options$g1label)
                    }
                }
                
                g1label <- "G1"
                g2label <- "G2"
                if(length(self$options$g1label)>0) { g1label <- self$options$g1label}
                if(length(self$options$g2label)>0) { g2label <- self$options$g2label}
                g1labels <-c("M", "SD", "N")
                g2labels <-c("M", "SD", "N")
                
                table <- self$results$result_table
                table$addColumn(name = "effect.size", title = eslabel, type = 'number')
                table$addColumn(name = "ci.low", title = "ci.low", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "ci.high", title = "ci.high", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                table$addColumn(name = "p.value", title = "p.value", type = 'number')
                table$addColumn(name = "diamond.ratio", title = "Diamond Ratio", type = 'number')
                table$addColumn(name = "dr.low", title = "dr.low", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI"))
                table$addColumn(name = "dr.high", title = "dr.high", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI"))
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
                # 
                
                if(self$options$show.study.table) {
                    table <- self$results$study_table
                    table$addColumn(name = "m1", title = g1labels[1], superTitle = g1label, type = 'number')
                    table$addColumn(name = "s1", title = g1labels[2], superTitle = g1label, type = 'number')
                    table$addColumn(name = "n1", title = g1labels[3], superTitle = g1label, type = 'number')
                    table$addColumn(name = "spacer", title = " ", type = 'text')
                    table$addColumn(name = "m2", title = g2labels[1], superTitle = g2label, type = 'number')
                    table$addColumn(name = "s2", title = g2labels[2], superTitle = g2label, type = 'number')
                    table$addColumn(name = "n2", title = g2labels[3], superTitle = g2label, type = 'number')
                    table$addColumn(name = "effect.size", title = eslabel, type = 'number')
                    table$addColumn(name = "ci.low", title = "ci.low", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                    table$addColumn(name = "ci.high", title = "ci.high", type = 'number', superTitle = paste(format(self$options$conf.level, digits = 0), "% CI") )
                    
                    for(x in 1:nrow(estimate$data)) {
                        table$addRow(x, values = list(
                            label = as.character(estimate$data[x, "label"]),
                            m1 = estimate$data[x, "m1"],
                            s1 = estimate$data[x, "s1"],
                            n1 = estimate$data[x, "n1"],
                            m2 = estimate$data[x, "m2"],
                            s2 = estimate$data[x, "s2"],
                            n2 = estimate$data[x, "n2"],
                            effect.size = estimate$data[x, "effect.size"],
                            ci.low = estimate$data[x, "ci.low"],
                            ci.high = estimate$data[x, "ci.high"]                        ))}
                }
                

            } else {
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
        },
        .plot=function(image, ...) {  # <-- the plot function
            estimate <- image$state
            
            plot <- plotMetaEffect(estimate, xlims = c(NULL, NULL), dr.explain = self$options$explainDR)
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
