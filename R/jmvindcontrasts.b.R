
# This file is a generated template, your changes will not be overwritten

jmvIndContrastsClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "jmvIndContrastsClass",
    inherit = jmvIndContrastsBase,
    private = list(
        .init = function() {
            tables <- list(self$results$means_table, self$results$contrast_table)
            for(table in tables) {
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
        
        clabels <- length(self$options$comparison_labels) > 0
        rlabels <- length(self$options$ref_labels) > 0
        if(clabels) {
            clabels <- clabels & nchar(self$options$comparison_labels) > 0
        }
        if(rlabels) {
            rlabels <- rlabels & nchar(self$options$ref_labels)> 0
        }
        
        
        if (self$options$switch == "fromraw") {
            err_string <- "Select a numeric variable for the dependent variable and a nominal variable for the grouping variable.\nIn the reference and comparison group textboxes you will write out a comma-separated list of the group names that you want to compare."
            
            
            # Check to see if all needed options have been set
            if (is.null(self$options$dep) | is.null(self$options$group) | !clabels | !rlabels) {
                run.analysis <- FALSE
                if(is.null(self$options$group)) {
                    err_string <- paste(err_string,"\nWaiting for you to define dependent variable, grouping variable, reference labels, and comparison labels.", sep="\n")
                }
            }
        
            if(!is.null(self$options$group)) {
                if(rlabels) {  
                    # Verify list of reference groups
                    # Split by comma, then trim ws whil also reducing the list returned by split to a vector
                    refgs <- strsplit(as.character(self$options$ref_labels), ",")
                    refgs <- trimws(refgs[[1]], which = "both")
                    # Now cycle through each item in the list to check it is a valid factor within the grouping variable
                    for (tlevel in refgs) {
                        if (!tlevel %in% levels(as.factor(self$data[, self$options$group]))) {
                            run.analysis <- FALSE
                            err_string <- stringr::str_interp(
                                "Reference group error: The level ${tlevel} does not exist in ${self$options$group}, which has levels ${levels(as.factor(self$data[, self$options$group]))}."
                            )
                        }
                    }
                } else {
                        err_string <- paste(err_string, stringr::str_interp(
                            "\nNow define the reference groups.  Type a list of levels from ${self$options$group} to be considered the reference group.  This variable has levels: ${levels(as.factor(self$data[, self$options$group]))}."
                        ))
                }
                
                if(clabels) {
                     # Verify list of comparison groups, same as above
                    compgs <- strsplit(as.character(self$options$comparison_labels), ",")
                    compgs <- trimws(compgs[[1]], which = "both")
                    for (tlevel in compgs) {
                        if (!tlevel %in% levels(as.factor(self$data[, self$options$group]))) {
                            run.analysis <- FALSE
                            err_string <- stringr::str_interp(
                                "Comparison group error: The level ${tlevel} does not exist in ${self$options$group}, which has levels ${levels(as.factor(self$data[, self$options$group]))}."
                            )
                        }
                    }
                } else {
                        err_string <- paste(err_string, stringr::str_interp(
                            "\nNow define the comparison groups.  Type a list of levels from ${self$options$group} to be considered the reference group.  This variable has levels: ${levels(as.factor(self$data[, self$options$group]))}."
                        ))
                }
            }
            
            if(run.analysis) {            
                contrast1 <- c(compgs, paste("-", refgs, sep=""))
                mycontrasts <- list(contrast1)
                estimate <- try(estimateContrasts.default(data = self$data, 
                                                          x = !!self$options$group, 
                                                          y = !!self$options$dep, 
                                                          contrasts = mycontrasts, 
                                                          conf.level = self$options$conf.level/100
                                                          ))
            }
        } else {
            err_string <- "For this analysis, setup a data with 5 columns: group labels, means, standard deviations, and sample sizes.\nBe sure to have no blank rows.\nIn the reference and comparison group textboxes you will write out a comma-separated list of the group names that you want to compare.\n"
            
            # Check if all needed info has been set
            if(is.null(self$options$labels) | 
               is.null(self$options$means) | 
               is.null(self$options$sds) | 
               is.null(self$options$ns) |
               !clabels |
               !rlabels ) {
                run.analysis <- FALSE
                
                if(is.null(self$options$labels)) {
                    err_string <- paste(err_string, "Waiting for you to define variables containing group labels, means, standard deviations, and sample sizes.", sep="\n")
                }
            }
            
            # Check that reference labels are within the list of groups names            
            if(!is.null(self$options$labels)) {
                if(rlabels) {
                    # Split the comma-separated list, then trim ws while reducing the list to a vector
                    refgs <- strsplit(as.character(self$options$ref_labels), ",")
                    refgs <- trimws(refgs[[1]], which = "both")
                    # Cycle through each item on the list to check it is in the list of group names
                    for (tlevel in refgs) {
                        if (!tlevel %in% self$data[, self$options$labels]) {
                            run.analysis <- FALSE
                            err_string <- stringr::str_interp(
                                "Reference group error: The level ${tlevel} does not exist in ${self$options$labels}, which has levels ${self$data[, self$options$labels]}."
                            )
                        }
                    }
                } else {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, stringr::str_interp(
                        "\nNow define the reference groups.  Type a list of levels from ${self$options$labels} to be considered the reference group.  This variable has levels: ${self$data[, self$options$labels]}."
                    ))
                }
                
                # Now check the comparison list, same as for the refernece list (clearly need to write a function for this)
                if(clabels) {
                    compgs <- strsplit(as.character(self$options$comparison_labels), ",")
                    compgs <- trimws(compgs[[1]], which = "both")
                    for (tlevel in compgs) {
                        if (!tlevel %in% self$data[, self$options$labels]) {
                            run.analysis <- FALSE
                            err_string <- stringr::str_interp(
                                "Comparison group error: The level ${tlevel} does not exist in ${self$options$labels}, which has levels ${self$data[, self$options$labels]}."
                            )
                        }
                    }
                } else {
                    run.analysis <- FALSE
                    err_string <- paste(err_string, stringr::str_interp(
                        "\nNow define the comparison groups.  Type a list of levels from ${self$options$labels} to be considered the reference group.  This variable has levels: ${self$data[, self$options$labels]}."
                    ))
                }
            }
        
            
            if(run.analysis) {
                # We are running the analysis
                
                # Create a numeric version of the contrast
                # First, determine the value that will be used for the pos and negative groups
                posvalue <- 1/length(compgs)
                negvalue <- -1/length(refgs)
                # Then, get a list of the group names
                newcontrast <- as.vector(self$data[[self$options$labels]])
                
                # Now replace the group names with 0, negvalue, or pos value based on the contrast lists passed
                newcontrast[! newcontrast %in% c(compgs, refgs)] <- 0
                newcontrast[newcontrast %in% compgs] <- posvalue
                newcontrast[newcontrast %in% refgs] <- negvalue
                # Finally, convert to numeric and store
                contrast1 <- as.numeric(newcontrast)
                mycontrasts <- list(contrast1)
                
                # Run the analysis
                estimate <- try(estimateContrasts.numeric(means = self$data[[self$options$means]],
                                                      sds = self$data[[self$options$sds]],
                                                      ns = self$data[[self$options$ns]],
                                                      contrasts = mycontrasts,
                                                      labels = self$data[[self$options$labels]],
                                                      conf.level = self$options$conf.level/100))
            }
            
        }
        

        if(run.analysis & class(estimate) != "try-error") {

            # Store result pbject for use with the graph
            image <- self$results$contrast_plot
            image$setState(mycontrasts) 
            
            # # Add needed rows to means table (got to be a better way to do this)
            # for(x in 1:(nrow(estimate$means_table)-1)) {
            #     table$addRow(x+1)
            # }
            
            # Setup the means_table, adding a column for each column we will report
            table <- self$results$means_table
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
            
            # Now the same process with the contrast table--add columns and rows, then fill in table
            table <- self$results$contrast_table
            for(x in 1:nrow(estimate$contrast_table)) {
                table$setRow(rowNo = x, values = list(
                    label = as.character(estimate$contrast_table[x, "label"]),
                    m = estimate$contrast_table[x, "m"],
                    moe = estimate$contrast_table[x, "moe"],
                    ci.low = estimate$contrast_table[x, "ci.low"],
                    ci.high = estimate$contrast_table[x, "ci.high"],
                    pvalue = estimate$contrast_table[x, "p.value"]
                ))}
            
            } else {
                # We didn't run the analysis.  Let's set the error text
                self$results$text$setVisible(TRUE)
                err_string <- paste("<h2>Instructions</h2>", err_string, "</p><hr></p>")
                self$results$text$setContent(gsub("\n", "</br>", err_string))
            }
        
        
        
        },
        .plot=function(image, ...) {  # <-- the plot function
            
            if (is.null(image$state))
                return(FALSE)

            if (self$options$switch == "fromraw") {
                estimate <- estimateContrasts.default(data = self$data, 
                                                          x = !!self$options$group, 
                                                          y = !!self$options$dep, 
                                                          contrasts = image$state, 
                                                          conf.level = self$options$conf.level/100
                )
            } else {
                estimate <- estimateContrasts.numeric(means = self$data[[self$options$means]],
                                                          sds = self$data[[self$options$sds]],
                                                          ns = self$data[[self$options$ns]],
                                                          contrasts = image$state,
                                                          labels = self$data[[self$options$labels]],
                                                          conf.level = self$options$conf.level/100)
            }
            
            contrast_colors = c("blue", "green")
            
            if(!is.null(self$options$contrast.colors)) {
                cc <- strsplit(as.character(self$options$contrast.colors), ",")
                contrast_colors <- trimws(cc[[1]], which = "both")
            }
            
            ylab = "Dependent Variable"
            if(!is.null(self$options$ylab)) {
                ylab <- self$options$ylab
            }
            
            plot <- plotContrast(estimate, contrast_number = 1, contrast_colors = contrast_colors, show.mean.error = self$options$show.mean.error, show.raw.data = self$options$show.raw.data, ylab = ylab)
            print(jmvClearPlotBackground(plot))
            TRUE
        },

     )
)
