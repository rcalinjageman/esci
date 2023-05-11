# Check for class estimate
# This function checks if x is an esci_estimate

is.estimate <- function(x)  {
  is(x, "esci_estimate")
}


#' Print an esci_estimate
#'
#' Pretties up the printing of a complex esci_estimate object.
#'
#' @param x - object to print; must be of class esci_estimate
#' @param ... S3 signature for generic plot function.
#' @param verbose - optional logical print all details; defaults to false
#'
#' @exportS3Method print esci_estimate
print.esci_estimate <- function(x, ..., verbose = FALSE) {

  estimate <- x

  # Summary
  summary_text <- if(estimate$properties$data_type == "Summary")
    "Analysis of summary data:\n"
  else
    "Analysis of raw data:\n"
  if(!is.null(estimate$properties$data_source)) {
    summary_text <- paste(
      summary_text,
      "Data frame = ",
      estimate$properties$data_source,
      "\n",
      sep = ""
    )
  }
  if(!is.null(estimate$properties$outcome_variable_name)) {
    summary_text <- paste(
      summary_text,
      "Outcome variable(s) = ",
      paste(estimate$properties$outcome_variable_name, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  if(!is.null(estimate$properties$grouping_variable_name)) {
    summary_text <- paste(
      summary_text,
      "Grouping variable(s) = ",
      estimate$properties$grouping_variable_name,
      "\n",
      sep = ""
    )
  }

  cat(summary_text)

  if(!is.null(estimate$overview)) {
    cat("\n---Overview---\n")
    print(estimate$overview)
  }

  for (tbl in names(estimate)) {
    if ("data.frame" %in% class(estimate[[tbl]]) & tbl != "overview" & tbl != "raw_data") {
      cat(paste("\n--", tbl, "--\n"))
      print(estimate[[tbl]])

      # Print any message in that table's properties
      tbl_properties <- paste(tbl, "_properties", sep = "")
      if(!is.null(estimate[[tbl_properties]])) {
        if(!is.null(estimate[[tbl_properties]]$message)) {
          print(estimate[[tbl_properties]]$message)
        }
      }

    }
  }


  # Note about CI width
  ci_message <- paste(
    "\n\nNote: LL and UL are lower and upper boundaries of confidence intervals with ",
    estimate$properties$conf_level*100,
    "% expected coverage.",
    sep = ""
  )

  cat(ci_message)


  # Warnings
  if (!is.null(estimate$warnings)) {
    cat("\nWarnings:\n")
    cat(paste0("* ", estimate$warnings, "\n", sep = ""))
  }
}




# This function rolls up an esci_estimate
# It takes a list of esci_estimates, and consolidates their
# properties and data tables so that they are accessible from the
# top level of the object
esci_estimate_consolidate <- function(estimate_list) {

  # Cycle through the list
  for (estimate in estimate_list) {
    # Check if the current list item is an estimate
    if (class(estimate) == "esci_estimate") {

      # Handle warnings, consolidating with outcome variable name
      if(length(estimate$warnings) > 0 ) {
        estimate_list$warnings <- c(
          estimate_list$warnings,
          paste(
            estimate$properties$outcome_variable_name,
            ": ",
            estimate$warnings,
            sep = ""
          )
        )
      }

      # Merge properties
      if (is.null(estimate_list$properties)) {
        estimate_list$properties <- estimate$properties
      } else {
        estimate_list$properties$outcome_variable_name <-
          c(estimate_list$properties$outcome_variable_name,
            estimate$properties$outcome_variable_name)
      }


      # Merge each table in the current estimate, pulling properties up too
      for(mytbl in names(estimate)) {
        if (is.data.frame(estimate[[mytbl]])) {
          estimate_list[[mytbl]] <- rbind(
            estimate_list[[mytbl]],
            estimate[[mytbl]]
          )
        } else {
          if (endsWith(mytbl, "_properties")) {
            if(is.null(estimate_list[[mytbl]])) {
              estimate_list[[mytbl]] <- estimate[[mytbl]]
            }
          }
        }
      }  # End cycling through tables
    } # End if testing if current list item is an estimate
  } # End cycling through the list that was passed

  return(estimate_list)
}
