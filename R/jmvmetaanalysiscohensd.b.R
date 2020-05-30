
# This file is a generated template, your changes will not be overwritten

jmvMetaAnalysisCohensdClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvMetaAnalysisCohensdClass",
    inherit = jmvMetaAnalysisCohensdBase,
    private = list(
        .init = function() {
            tables <- list(self$results$result_table, self$results$study_table)
            for(table in tables) {
                table$getColumn("ci.low")$setSuperTitle(paste(self$options$conf.level, "% CI"))
                table$getColumn("ci.high")$setSuperTitle(paste(self$options$conf.level, "% CI"))
            }
            self$results$result_table$getColumn("dr.low")$setSuperTitle(paste(self$options$conf.level, "% CI"))
            self$results$result_table$getColumn("dr.high")$setSuperTitle(paste(self$options$conf.level, "% CI"))
            
        },
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            #Set flag for running analysis and instruction string.
            run.analysis <- TRUE
            err_string <- "For this analysis, setup a data with at least 4 columns: study labels, cohen's d, group 1 sample sizes, and group 2 sample sizes.\nYou can also setup a column that represents a categorical (nominal) moderator that varies between studies.\nBe sure to have no blank rows in your data.\n"
            
            # Check if all needed info has been set
            if(is.null(self$options$labels) | 
               is.null(self$options$d) | 
               is.null(self$options$n1) |
               is.null(self$options$n2) ) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "Waiting for you to define variables containing study labels labels, cohen's d, and ns for each group.", sep="\n")
            }            
            
            
            if(run.analysis) {
                estimate <- estimateOverallSMD(data = self$data, 
                                               d = !!self$options$d, 
                                               n1 = !!self$options$n1,
                                               n2 = !!self$options$n2,
                                               label = !!self$options$labels,
                                               moderator = !!self$options$moderator,
                                               random.effects = (self$options$REorFE == "RE"),
                                               conf.level = self$options$conf.level/100,
                                               correct.for.bias = self$options$correct.for.bias)
                
                # Store the estimate for the plot
                image <- self$results$forest_plot
                image$setState(estimate)
                

                eslabel <- estimate$effect.size.name

                table <- self$results$result_table
                table$getColumn("effect.size")$setTitle(eslabel)

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
                

                if(self$options$show.study.table) {
                    table <- self$results$study_table
                    table$getColumn("effect.size")$setTitle(eslabel)
                    for(x in 1:nrow(estimate$data)) {
                        table$addRow(x, values = list(
                            label = as.character(estimate$data[x, "label"]),
                            effect.size = estimate$data[x, "effect.size"],
                            ci.low = estimate$data[x, "ci.low"],
                            ci.high = estimate$data[x, "ci.high"],
                            n1 = estimate$data[x, "n1"],
                            n2 = estimate$data[x, "n2"]
                            ))}
                }
                
                
            } else {
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
            
        },
        .plot=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            
            estimate <- image$state
            
            plot <- plotMetaEffect(estimate, xlims = c(NULL, NULL))
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
