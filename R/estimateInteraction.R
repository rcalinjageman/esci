estimateComplex_2x2 <- function(x, ...) {
  UseMethod("estimateComplex_2x2")
}



estimateComplex_2x2.numeric <- function(means = c(NULL), 
                                sds = c(NULL), 
                                ns = c(NULL), 
                                alabels = c("A1", "A2"),
                                blabels = c("B1", "B2"),
                                aname = "A",
                                bname = "B",
                                conf.level = .95) {



  
  # Setup 5 common contrasts for a 2x2 design    
  contrast1 <- c(-1/2, -1/2, 1/2, 1/2)
  contrast2 <- c(-1/2, 1/2, -1/2, 1/2)
  contrast3 <- c(1, -1, -1, 1)
  contrast4 <- c(-1, 1, 0, 0)
  contrast5 <- c(0,0,-1, 1)
  myconstrasts <- list(contrast1, contrast2, contrast3, contrast4, contrast5)

  
  # Check if variable labels have been passed, otherwise set to generic A*B labels
  if(is.null(alabels)) {
    alabels <- c("A1", "A2")
  }
  
  if(is.null(blabels)) {
    blabels <- c("B1", "B2")
  }
  if(is.null(aname)) {
    aname <- "A"
  }
  if(is.null(bname)) {
    bname <- "B"
  }
  
  # Make cross labels for each cell
  labels <- paste(rep(alabels, each = length(blabels)), blabels, sep = "\n")
  celllabels <- paste(rep(alabels, each = length(blabels)), blabels, sep = ".")
  
  
  # Now make labels for each of the main contrasts
  clabel1 <- c(alabels[2], alabels[1], paste("Main effect\n of ", aname, ":\n(", alabels[2], " - ", alabels[1], ")", sep = ""))
  clabel2 <- c(blabels[2], blabels[1], paste("Main effect\n of ", bname, ":\n(", blabels[2], " - ", blabels[1], ")", sep = ""))
  clabel3 <- c("G2", "G1",   paste("Interaction\n of ", aname, " and ", bname, ":\n(",
                                   celllabels[4], " - ", celllabels[3], ") - (",
                                   celllabels[2], " - ", celllabels[1], ")",
                                   sep = ""
                                    )
              )
  clabel4 <- c(labels[2], labels[1], paste("Simple effect:\n(", blabels[2], " - ", blabels[1], ")\n at ", alabels[1], sep=""))
  clabel5 <- c(labels[4], labels[3], paste("Simple effect:\n(", blabels[2], " - ", blabels[1], ")\n at ", alabels[2], sep=""))
  clabels <- list(clabel1, clabel2, clabel3, clabel4, clabel5)
  
  # Use estimate contrasts to obtain estimate for each contrast
  estimate <- estimateContrasts.numeric(means, sds, ns, myconstrasts, labels, clabels, conf.level = conf.level)
  
  
  # Stitch together the interaction contrast--we will copy each simple effect in
  estimate$contrast_table[7, ] <- estimate$contrast_table[15, ]
  estimate$contrast_table[8, ] <- estimate$contrast_table[12, ]
  estimate$contrast_table[7, "contrast_number"] <- 3
  estimate$contrast_table[8, "contrast_number"] <- 3
  

  # Fix the labels in the contrast table to remove line breaks
  levels(estimate$contrast_table$label) <- gsub("\n", "", levels(estimate$contrast_table$label))
  levels(estimate$means_table$label) <- gsub("\n", "", levels(estimate$means_table$label))
  
  
  # Fix the interaction plot
  estimate$plot_table <- estimate$plot_table[!(estimate$plot_table$contrast_number == 3 & estimate$plot_table$plot_labels != "Difference"), ]
  estimate$plot_table[estimate$plot_table$contrast_number == 3, ]$plot_labels <- "Interaction"
  estimate$plot_table[estimate$plot_table$contrast_number == 3, ]$x <- 5.5
  
  estimate$error_table <- estimate$error_table[!(estimate$error_table$contrast_number == 3 & estimate$error_table$label != "Difference"), ]
  estimate$error_table[estimate$error_table$contrast_number == 3, ]$label <- "Interaction"
  estimate$error_table[estimate$error_table$contrast_number == 3, ]$x <- 5.5

  #Fix the incorrect offset of the error table
  wrong_offset <- mean(means[which(contrast3 > 0)])
  estimate$error_table[estimate$error_table$contrast_number == 3, ]$m <- 
      estimate$error_table[estimate$error_table$contrast_number == 3, ]$m -
      wrong_offset +
      means[4]
    

  
  return(estimate)

}


estimateComplex_2x2.default <- function(data, dv, iv1, iv2,
                                        conf.level = .95) {
  # Initialization ---------------------------
  # Create quosures and quonames.
  # Stolen directly from dabestr
  dv_enquo        <-  rlang::enquo(dv)
  dv_quoname      <-  rlang::quo_name(dv_enquo)
  
  iv1_enquo        <-  rlang::enquo(iv1)
  iv1_quoname      <-  rlang::quo_name(iv1_enquo)

  iv2_enquo        <-  rlang::enquo(iv2)
  iv2_quoname      <-  rlang::quo_name(iv2_enquo)
  
    
  # Validate inputs ---------------------------
  # check CI.
  if (conf.level < 0.50 | conf.level >= 1) {
    err_string <- stringr::str_interp(
      "`conf.level` must be between 0.50 and 1, not ${conf.level}"
    )
    stop(err_string)
  }
  
  # Check data is a dataframe
  if(!is.data.frame(data)) {
    err_string <- stringr::str_interp(
      "`data` must be a data frame, not ${class(data)}"
    )
    stop(err_string)
  }
  #Check data has more than 8 rows
  if(nrow(data)<8) {
    err_string <- stringr::str_interp(
      "`data` must have more than 7 rows, not ${nrow(data)}"
    )
    stop(err_string)
  }
  #Check that dv column exists
  if(dv_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${dv_quoname}"
    )
    stop(err_string)
  }
  #Check that iv1 column exists
  if(iv1_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${iv1_quoname}"
    )
    stop(err_string)
  }
  #Check that iv1 column exists
  if(iv2_quoname %in% colnames(data)) {
  } else {
    err_string <- stringr::str_interp(
      "Must pass a column name that exists, not ${iv2_quoname}"
    )
    stop(err_string)
  }
  # Check if dv is numeric
  if(!is.numeric(data[[dv_quoname]])) {
    err_string <- stringr::str_interp(
      "dv (${dv_quoname}) must be numeric, not ${class(data[[dv_quoname]])}.  Try making a numeric colum with as.numeric"
    )
    stop(err_string)
  }
  
  
  # Data cleanup ---------------------------
  # Make duplicate copies that can be addressed using $ notation..cause I like it?
  data$iv1 <- data[[iv1_quoname]]
  data$iv2 <- data[[iv2_quoname]]
  data$dv <- data[[dv_quoname]]
  
  # Reduce down to only the iv and dv columns.  Since we're passing this data back, best to limit its size, I think
  keeps <- c("iv1", "iv2", "dv")
  data <- data[keeps]  
  
  # Now remove NAs from data
  data <- data[!is.na(data$dv), ]
  data <- data[!is.na(data$iv1), ]
  data <- data[!is.na(data$iv2), ]
  
  data$x <- 0
  
  #Now get summary data by group
  means <- c(NULL)
  sds <- c(NULL)
  ns <- c(NULL)
  labels <- c(NULL)
  this_index <- 0
  
  for (this_group in levels(data$iv1)) {
      for(that_group in levels(data$iv2)) {
          group_only <- data[data$iv1 == this_group & data$iv2 == that_group, ]
          if (nrow(group_only) > 0) {
            this_index <- this_index + 1
            data[data$iv1 == this_group & data$iv2 == that_group, ]$x <- this_index-0.5
            means[this_index] <- mean(group_only$dv)
            sds[this_index] <- sd(group_only$dv)
            ns[this_index] <- nrow(group_only)
            labels[this_index] <- this_group
          }
      }
  }
  
  ### Now pass along to summary data version
  res <- estimateComplex_2x2.numeric(means = means,sds = sds, ns = ns, 
                                     alabels = levels(data$iv1),
                                     blabels = levels(data$iv2),
                                     aname = iv1_quoname,
                                     bname = iv2_quoname, 
                                     conf.level = conf.level
                                     )
  
  data$cell_labels <- as.factor(paste(data$iv1, data$iv2, sep="."))
  
  contrast_count <- 0
  for (contrast in res$contrasts) {
    contrast_count <- contrast_count + 1
    contrast_column <- paste("contrast", contrast_count)
    data[data$cell_labels %in% levels(data$cell_labels)[which(contrast > 0)], contrast_column] <- "G1"
    data[data$cell_labels %in% levels(data$cell_labels)[which(contrast < 0)], contrast_column] <- "G2"
    data[data$cell_labels %in% levels(data$cell_labels)[which(contrast == 0)], contrast_column] <- "Unused"
  }
  
  
  res$raw_data <- data
  
  ### Prepare for return
  return(res)  
  
  
}

## Raw data test
# testd <- data.frame(duration = c(rep("morning", 100), rep("evening", 100)),
#                     activity = c(rep("Sleep", 50), rep("Wake", 50), rep("Sleep", 50), rep("Wake", 50)),
#                     memory = c(rnorm(n = 50, mean= 1.5, sd = 1.38), rnorm(n = 50, mean= 1.14, sd = 0.96), rnorm(n = 50, mean= 1.38, sd = 1.5), rnorm(n = 50, mean= 2.22, sd = 1.68))
#                     )
# 
# estimate <- estimateComplex_2x2(testd, memory, duration, activity)
# myplot <- plotContrast(estimate, contrast_number = 3)
# myplot

### 

# # Temp assignments for debugging
# means <- c(1.5, 1.14, 1.38, 2.22)
# sds <- c(1.38, 0.96, 1.5, 1.68)
# ns <- c(26, 26, 25, 26)
# alabels <- c("Evening", "Morning")
# blabels <- c("Sleep", "No Sleep")
# aname <- "Time"
# bname <- "Activity"
# conf.level = .95
# 
# estimate <- estimateComplex_2x2(means, sds, ns, alabels, blabels, aname, bname)
# 
# myplot <- plotContrast(estimate, contrast_number = 2, ylab = "Memory Score")
# myplot
