
# This file is a generated template, your changes will not be overwritten

jmvMetaAnalysisRawClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvMetaAnalysisRawClass",
    inherit = jmvMetaAnalysisRawBase,
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
                

                eslabel <- "M2-M1"
                if(self$options$report.cohens.d) {
                    eslabel <- "Cohen's d (unbiased)"
                } else {
                    if(length(self$options$g1label)>0 & length(self$options$g2label) >0) {
                        if(nchar(self$options$g1label)>0 & nchar(self$options$g2label)>0) {
                        eslabel <- paste(self$options$g2label, "-", self$options$g1label)
                        }
                    }
                }
                
                g1label <- "G1"
                g2label <- "G2"
                if(length(self$options$g1label)>0) { g1label <- self$options$g1label}
                if(length(self$options$g2label)>0) { g2label <- self$options$g2label}
                g1labels <-c("M", "SD", "N")
                g2labels <-c("M", "SD", "N")
                
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
                    table$getColumn("m1")$setTitle(g1labels[1])
                    table$getColumn("s1")$setTitle(g1labels[2])
                    table$getColumn("n1")$setTitle(g1labels[3])
                    table$getColumn("m1")$setTitle(g2labels[1])
                    table$getColumn("s1")$setTitle(g2labels[2])
                    table$getColumn("n1")$setTitle(g2labels[3])
                    
                    table$getColumn("m1")$setSuperTitle(g1label)
                    table$getColumn("s1")$setSuperTitle(g1label)
                    table$getColumn("n1")$setSuperTitle(g1label)
                    table$getColumn("m2")$setSuperTitle(g2label)
                    table$getColumn("s2")$setSuperTitle(g2label)
                    table$getColumn("n2")$setSuperTitle(g2label)
                    
                    table$getColumn("effect.size")$setTitle(eslabel)
                    
                    
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
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
            
        },
        .plot=function(image, ...) {  # <-- the plot function
            if (is.null(image$state))
                return(FALSE)
            estimate <- image$state
            
            plot <- plotMetaEffect(estimate, xlims = c(NULL, NULL), dr.explain = self$options$explainDR)
            print(jmvClearPlotBackground(plot))
            TRUE
        })
)
