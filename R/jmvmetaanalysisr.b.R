
# This file is a generated template, your changes will not be overwritten

jmvMetaAnalysisRClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvMetaAnalysisRClass",
    inherit = jmvMetaAnalysisRBase,
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
            err_string <- "For this analysis, setup a data with at least 3 columns: study labels, Pearsons r, and sample sizes.\nYou can also setup a column that represents a categorical (nominal) moderator that varies between studies.\nBe sure to have no blank rows in your data.\n"
            
            # Check if all needed info has been set
            if(is.null(self$options$labels) | 
               is.null(self$options$r) | 
               is.null(self$options$n) ) {
                run.analysis <- FALSE
                err_string <- paste(err_string, "Waiting for you to define variables containing study labels labels, Pearsons r, and sample sizes.", sep="\n")
            }            
            
            
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
                image$setState(TRUE)
                

                eslabel <- estimate$effect.size.name
                
                table <- self$results$result_table

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
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
        },
        .plot=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            
            estimate <- estimateOverallCorrelation(data = self$data, 
                                                   rvalues = !!self$options$r, 
                                                   ns = !!self$options$n,
                                                   label = !!self$options$labels,
                                                   moderator = !!self$options$moderator,
                                                   random.effects = (self$options$REorFE == "RE"),
                                                   conf.level = self$options$conf.level/100)
            
            plot <- plotMetaEffect(estimate, xlims = c(-1, 1))
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
