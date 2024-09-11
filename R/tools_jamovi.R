jamovi_estimate_filler <- function(self, estimate, expand = FALSE) {

  for (e_table in names(estimate)) {
    if(is.data.frame(estimate[[e_table]])) {
      j_table <- self$results[[e_table]]
      if (!is.null(j_table)) {
        jamovi_table_filler(
          j_table,
          estimate[[e_table]],
          expand = TRUE
        )

        eprop <- paste(e_table, "_properties", sep = "")
        if (!is.null(estimate[[eprop]])) {

          if (!is.null(estimate[[eprop]]$denominator_name_html)) {
            j_table$getColumn("denominator")$setTitle(
              estimate[[eprop]]$denominator_name_html
            )
          }

          if (!is.null(estimate[[eprop]]$effect_size_name_html) & e_table == "es_smd") {
            j_table$getColumn("effect_size")$setTitle(
              estimate[[eprop]]$effect_size_name_html
            )

            to_replace <- '<sub>.biased</sub>'
            if (grepl("biased", estimate[[eprop]]$effect_size_name_html))
              to_replace <- ""

            j_table$getColumn("d_biased")$setTitle(
              paste(
                estimate[[eprop]]$effect_size_name_html,
                to_replace,
                sep = ""
              )
            )
          }


          if (!is.null(estimate[[eprop]]$message_html)) {
            j_table$setNote(
              key = "rtable",
              note = estimate[[eprop]]$message_html,
              init = FALSE
            )
          }
        }


      }

    }
  }

}


# This function takes a jamovi table and data frame and fills the jamovi table
# with the data frame data.
#
# For this to work, the column names in the data frame must be exactly the same
# as defined in the results file for the jamovi table (can be different) order.
# Can have columns in the result_table that are not in the jamovi table
jamovi_table_filler <- function(jmv_table = NULL, result_table, expand = FALSE) {
  # Loop through rows in dataframe
  if (is.null(jmv_table)) return(FALSE)
  if (is.null(result_table)) return(FALSE)

  if (!is.null(result_table$df)) {
    result_table$df_i <- result_table$df
  }

  if (!is.null(result_table$effect_size)) {
    result_table$effect_size_smd <- result_table$effect_size
  }



  for (x in 1:nrow(result_table)) {
    # Initialize a named list
    row_list <- list()

    # Now fill the named list with the column/values from the data frame
    for(mycol in names(result_table)) {
      if (is.na(result_table[x, mycol])) {

      } else {
        row_list[mycol] = result_table[x, mycol]
      }
    }

    # Save this data to the jamovi table
    if_set <- try(jmv_table$setRow(rowNo = x, values = row_list), silent = TRUE)
    if (is(if_set, "try-error") & expand) {
      jmv_table$addRow(rowKey = x, values = row_list)
    }

  }


  if (!is.null(result_table$grouping_variable_A_name) & !is.null(result_table$grouping_variable_A_level)) {
    jmv_table$getColumn("grouping_variable_A_level")$setTitle(
      result_table$grouping_variable_A_name[[1]]
    )
  }

  if (!is.null(result_table$grouping_variable_B_name) & !is.null(result_table$grouping_variable_B_level)) {
    jmv_table$getColumn("grouping_variable_B_level")$setTitle(
      result_table$grouping_variable_B_name[[1]]
    )
  }

  if (!is.null(result_table$grouping_variable_name) & !is.null(result_table$grouping_variable_level)) {
    jmv_table$getColumn("grouping_variable_level")$setTitle(
      result_table$grouping_variable_name[[1]]
    )
  }

  effect <- NULL
  try(effect <- result_table$effect, silent = TRUE)
  if (!is.null(result_table$grouping_variable_name) & !is.null(effect)) {
    jmv_table$getColumn("effect")$setTitle(
      paste(result_table$grouping_variable_name[[1]], "<br>Effect")
    )
  }

  effect_plus <- NULL
  try(effect_plus <- result_table$effect_plus, silent = TRUE)
  if (!is.null(effect_plus)) {
    jmv_table$getColumn("effect_plus")$setTitle(
      paste(result_table$grouping_variable_name[[1]], "<br>Effect")
    )
  }

  if (!is.null(result_table$comparison_mean) & !is.null(result_table$effect)) {
    cmean <- strsplit(result_table$effect, " / ")
    jmv_table$getColumn("comparison_mean")$setTitle(
      paste("<i>M</i><sub>", cmean[[1]][1], "</sub>", sep = "")
    )
    jmv_table$getColumn("reference_mean")$setTitle(
      paste("<i>M</i><sub>", cmean[[1]][2], "</sub>", sep = "")
    )
    jmv_table$getColumn("effect_size")$setTitle(
      paste(
        "<i>M</i><sub>", cmean[[1]][1], "</sub>",
        " / ",
        "<i>M</i><sub>", cmean[[1]][2], "</sub>",
        sep = ""
      )
    )

  }

  if (!is.null(result_table$comparison_median) & !is.null(result_table$effect)) {
    cmean <- strsplit(result_table$effect, " / ")
    jmv_table$getColumn("comparison_median")$setTitle(
      paste("<i>Mdn</i><sub>", cmean[[1]][1], "</sub>", sep = "")
    )
    jmv_table$getColumn("reference_median")$setTitle(
      paste("<i>Mdn</i><sub>", cmean[[1]][2], "</sub>", sep = "")
    )
    jmv_table$getColumn("effect_size")$setTitle(
      paste(
        "<i>Mdn</i><sub>", cmean[[1]][1], "</sub>",
        " / ",
        "<i>Mdn</i><sub>", cmean[[1]][2], "</sub>",
        sep = ""
      )
    )
  }


  # Return the filled table
  return(TRUE)

}


# This helper function sets the lower, upper, and moe columns of a jamovi table
# based on the CI passed. Lower and upper are set with supertitles MoE is set by
# adjusting the name. All changes are wrapped in try statements, so if you pass
# a table without some of these columns, no errors will be thrown
jamovi_set_confidence <- function(jmv_table = NULL, CI) {

  if (is.null(jmv_table)) return(FALSE)

  CI_columns <- c(
    "mean_LL", "mean_UL", "median_LL", "median_UL",
    "LL", "UL",
    "P_LL", "P_UL",
    "I2_LL", "I2_UL",
    "diamond_ratio_LL", "diamond_ratio_UL"
  )

  PI_columns <- c(
    "PI_LL", "PI_UL"
  )


  for (column_name in CI_columns) {
    try(
      jmv_table$getColumn(column_name)$setSuperTitle(
        paste(CI, "% CI", sep = "")
      ), silent = TRUE
    )
  }

  for (column_name in PI_columns) {
    try(
      jmv_table$getColumn(column_name)$setSuperTitle(
        paste(CI, "% PI", sep = "")
      ), silent = TRUE
    )
  }


  return(TRUE)
}


# This helper function expands a jamovi table to the desired number of rows
#  and it also optionally creates groupings every breaks rows
jamovi_init_table <- function(jmv_table = NULL, desired_rows, breaks = NULL) {

  if (is.null(jmv_table)) return(FALSE)

  current_length <- length(jmv_table$rowKeys)
  if (current_length < desired_rows) {
    for (y in (current_length+1):desired_rows) {
      # Just a loop that adds rows
      jmv_table$addRow(rowKey = y)
    }
  }

  if (!is.null(breaks) & !is.null(jmv_table$rowKeys) & (length(jmv_table$rowKeys) > 0)) {
    # Create groups every breaks rows
    for (y in 1:length(jmv_table$rowKeys)) {
      if (y %% breaks == 1) {
        jmv_table$addFormat(rowNo = y, col = 1, jmvcore::Cell.BEGIN_GROUP)
      }
      if (y %% breaks == 0) {
        jmv_table$addFormat(rowNo = y, col = 1, jmvcore::Cell.END_GROUP)
      }
    }
  }
  return(TRUE)
}


jamovi_arg_builder <- function(
  args,
  arg_name,
  my_value = NULL,
  return_value = NULL,
  na_ok = FALSE,
  convert_to_number = FALSE,
  lower = NULL,
  upper = NULL,
  lower_inclusive = FALSE,
  upper_inclusive = FALSE,
  my_value_name = NULL
) {


  if(is.null(my_value_name)) {
    my_value_name <- deparse(substitute(my_value))
    my_value_name <- gsub("self\\$options\\$", "", my_value_name)
    my_value_name <- gsub("_", " ", my_value_name)
  }

  # Lots of ways a jamovi input can be invalid
  #   Check for null, na, trims length of 0, or one of several
  #   text strings that shouldn't be passed on
  if(is.null(my_value)) {
    reason <- glue::glue(
      "{my_value_name} was null; replaced with: {if(is.na(return_value)) 'auto' else return_value}"
    )
    args$warnings <- c(args$warnings, reason)
    args[[arg_name]] <- return_value
    return(args)
  }

  if(!na_ok & is.na(my_value)) {
    reason <- glue::glue(
      "{my_value_name} was NA/missing; replaced with: {if(is.na(return_value)) 'auto' else return_value}"
    )
    args$warnings <- c(args$warnings, reason)
    args[[arg_name]] <- return_value
    return(args)
  }

  if(length(trimws(as.character(my_value))) == 0) {
    reason <- glue::glue(
      "{my_value_name} was empty string (''); replaced with: {if(is.na(return_value)) 'auto' else return_value}"
    )
    args$warnings <- c(args$warnings, reason)
    args[[arg_name]] <- return_value
    return(args)
  }

  if(trimws(as.character(my_value)) %in% c("")) {
    reason <- glue::glue(
      "{my_value_name} was empty string (''); replaced with: {if(is.na(return_value)) 'auto' else return_value}"
    )
    args$warnings <- c(args$warnings, reason)
    args[[arg_name]] <- return_value
    return(args)
  }

  if(trimws(as.character(my_value)) %in% c("auto", "Auto", "AUTO")) {
    args[[arg_name]] <- return_value
    return(args)
  }

  if(trimws(as.character(my_value))
     %in%
     c("NaN", "Na", "NA", "None")
  ) {
    if(na_ok) {
      return(NA)
    } else {
      reason <- glue::glue(
        "{my_value_name} was NaN/Na/NA/None; replaced with: {if(is.na(return_value)) 'auto' else return_value}"
      )
      args$warnings <- c(args$warnings, reason)
      args[[arg_name]] <- return_value
      return(args)
    }
  }

  # Now, if specified, try to convert to a number
  fvalue <- if(convert_to_number) {
    as.numeric(my_value)
  } else {
    my_value
  }

  # If conversion didn't succeed, don't send the value back
  if (is.na(fvalue)) {
    if(na_ok) {
      reason <- glue::glue(
        "{my_value_name} conversion to number yielded Na/Missing;
        replaced with: {if(is.na(return_value)) 'auto' else return_value}"
      )
      args$warnings <- c(args$warnings, reason)
      args[[arg_name]] <- NA
      return(args)
    } else {
      reason <- glue::glue(
        "{my_value_name} conversion to number yielded Na/Missing;
        replaced with: {if(is.na(return_value)) 'auto' else return_value}"
      )
      args$warnings <- c(args$warnings, reason)
      args[[arg_name]] <- return_value
      return(args)
    }
  }

  # Check range of numeric parameter
  out_of_range <- NULL
  lower_symbol <- ifelse(lower_inclusive, ">=", ">")
  upper_symbol <- ifelse(upper_inclusive, "<=", "<")

  if(!is.null(lower)) {
    if(lower_inclusive) {
      if(fvalue < lower) out_of_range <- paste(lower_symbol, lower)
    } else {
      if(fvalue <= lower) out_of_range <- paste(lower_symbol, lower)
    }
  }

  if(!is.null(upper)) {
    if(upper_inclusive) {
      if(fvalue > upper) out_of_range <- paste(upper_symbol, upper)
    } else {
      if(fvalue >= upper) out_of_range <- paste(upper_symbol, upper)
    }
  }

  if(!is.null(out_of_range)) {
    reason <- glue::glue(
      "{my_value_name} is {fvalue} but must be {out_of_range};
        replaced with: {if(is.na(return_value)) 'auto' else return_value}"
    )
    args$warnings <- c(args$warnings, reason)
    args[[arg_name]] <- return_value
    return(args)
  }

  args[[arg_name]] <- fvalue
  return(args)

}


# This function sanitizes an input from jamovi
# If the input is null, length is 0, all spaces, or NA, it returns return value
# Otherwise it returns the input value, converted to as.numeric if specified
jamovi_sanitize <- function(
  my_value = NULL,
  return_value = NULL,
  na_ok = FALSE,
  convert_to_number = FALSE,
  lower = NULL,
  upper = NULL,
  lower_inclusive = FALSE,
  upper_inclusive = FALSE,
  my_value_name = NULL
) {

  if(is.null(my_value_name)) {
    my_value_name <- deparse(substitute(my_value))
    my_value_name <- gsub("self\\$options\\$", "", my_value_name)
    my_value_name <- gsub("_", " ", my_value_name)
  }

  # Lots of ways a jamovi input can be invalid
  #   Check for null, na, trims length of 0, or one of several
  #   text strings that shouldn't be passed on
  myreplace <- 'auto'
  try(myreplace <- return_value, silent = TRUE)

  if(is.null(my_value)) {
    myreplace <- 'auto'
    if (!is.null(return_value) & !is.na(return_value)) myreplace <- return_value

    reason <- glue::glue(
      "{my_value_name} was null; replaced with: {myreplace}"
    )
    if(!is.null(return_value)) names(return_value) <- reason
    return(return_value)
  }

  if(!na_ok & is.na(my_value)) {
    reason <- glue::glue(
      "{my_value_name} was NA/missing; replaced with: {myreplace}"
    )
    if(!is.null(return_value)) names(return_value) <- reason
    return(return_value)
  }

  if(length(trimws(as.character(my_value))) == 0) {
    reason <- glue::glue(
      "{my_value_name} was empty string (''); replaced with: {myreplace}"
    )
    if(!is.null(return_value)) names(return_value) <- reason
    return(return_value)
  }

  if(trimws(as.character(my_value)) %in% c("")) {
    reason <- glue::glue(
      "{my_value_name} was empty string (''); replaced with: {myreplace}"
    )
    if(!is.null(return_value)) names(return_value) <- reason
    return(return_value)
  }

  if(trimws(as.character(my_value)) %in% c("auto", "Auto", "AUTO")) {
    return(return_value)
  }

  if(trimws(as.character(my_value))
     %in%
     c("NaN", "Na", "NA", "None")
  ) {
    if(na_ok) {
      return(NA)
    } else {
      reason <- glue::glue(
        "{my_value_name} was NaN/Na/NA/None; replaced with: {myreplace}"
      )
      if(!is.null(return_value)) names(return_value) <- reason
      return(return_value)
    }
  }

  # Now, if specified, try to convert to a number
  fvalue <- if(convert_to_number) {
    as.numeric(my_value)
  } else {
    my_value
  }

  # If conversion didn't succeed, don't send the value back
  if (is.na(fvalue)) {
    if(na_ok) {
      return(NA)
    } else {
      reason <- glue::glue(
        "{my_value_name} conversion to number yielded Na/Missing;
        replaced with: {myreplace}"
      )
      if(!is.null(return_value)) names(return_value) <- reason
      return(return_value)
    }
  }

  # Check range of numeric parameter
  out_of_range <- NULL
  lower_symbol <- ifelse(lower_inclusive, ">=", ">")
  upper_symbol <- ifelse(upper_inclusive, "<=", "<")

  if(!is.null(lower)) {
    if(lower_inclusive) {
      if(fvalue < lower) out_of_range <- paste(lower_symbol, lower)
    } else {
      if(fvalue <= lower) out_of_range <- paste(lower_symbol, lower)
    }
  }

  if(!is.null(upper)) {
    if(upper_inclusive) {
      if(fvalue > upper) out_of_range <- paste(upper_symbol, upper)
    } else {
      if(fvalue >= upper) out_of_range <- paste(upper_symbol, upper)
    }
  }

  if(!is.null(out_of_range)) {
    reason <- glue::glue(
      "{my_value_name} is {fvalue} but must be {out_of_range};
        replaced with: {if(is.na(return_value)) 'auto' else return_value}"
    )
    if(!is.null(return_value)) names(return_value) <- reason
    return(return_value)
  }


  return(fvalue)

}

jamovi_required_numeric <- function(
  my_value = NULL,
  integer_required = FALSE,
  lower = NULL,
  upper = NULL,
  lower_inclusive = FALSE,
  upper_inclusive = FALSE,
  my_value_name = NULL
) {

  if(is.null(my_value_name)) {
    my_value_name <- deparse(substitute(my_value))
    my_value_name <- gsub("self\\$options\\$", "", my_value_name)
    my_value_name <- gsub("_", " ", my_value_name)
  }

  return_value <- NA
  names(return_value) <- my_value_name


  # Lots of ways a jamovi input can be invalid
  #   Check for null, na, trims length of 0, or one of several
  #   text strings that shouldn't be passed on
  if(is.null(my_value)) {
    return(return_value)
  }

  if(is.na(my_value)) {
    return(return_value)
  }

  if(length(trimws(as.character(my_value))) == 0) {
    return(return_value)
  }

  if(trimws(as.character(my_value)) %in% c("")) {
    return(return_value)
  }


  if(trimws(as.character(my_value))
     %in%
     c("NaN", "Na", "NA", "None")
  ) {
    return(return_value)
  }

  # Now, if specified, try to convert to a number
  fvalue <- as.numeric(my_value)

  # If conversion didn't succeed, don't send the value back
  if (is.na(fvalue)) {
    return(return_value)
  }

  # Check range of numeric parameter
  out_of_range <- NULL
  lower_symbol <- ifelse(lower_inclusive, ">=", ">")
  upper_symbol <- ifelse(upper_inclusive, "<=", "<")

  if(!is.null(lower)) {
    if(lower_inclusive) {
      if(fvalue < lower) out_of_range <- paste(lower_symbol, lower)
    } else {
      if(fvalue <= lower) out_of_range <- paste(lower_symbol, lower)
    }
  }

  if (integer_required & !is.whole.number(fvalue)) {
    reason <- glue::glue(
      "Error: {my_value_name} is {fvalue} but must be an integer"
    )
    return(reason)
  }

  if(!is.null(upper)) {
    if(upper_inclusive) {
      if(fvalue > upper) out_of_range <- paste(upper_symbol, upper)
    } else {
      if(fvalue >= upper) out_of_range <- paste(upper_symbol, upper)
    }
  }

  if(!is.null(out_of_range)) {
    reason <- glue::glue(
      "Error: {my_value_name} is {fvalue} but must be {out_of_range}"
    )
    return(reason)
  }

  return(fvalue)

}


# This helper function checks if a contrast is valid
jamovi_check_contrast <- function(
  labels,
  valid_levels,
  level_source,
  group_type,
  error_string = NULL,
  sequential = FALSE
) {

  run_analysis <- TRUE

  if(nchar(labels)>=1 & labels != ' ') {
    # Verify list of reference groups
    # Split by comma, then trim ws while also
    #  reducing the list returned by split to a vector
    refgs <- strsplit(
      as.character(labels), ","
    )
    refgs <- trimws(refgs[[1]], which = "both")


    # Now cycle through each item in the list to check it
    #   is a valid factor within the grouping variable

    for (tlevel in refgs) {
      if (!tlevel %in% valid_levels) {
        error_string <- paste(error_string, glue::glue(
          "<b>{group_type} error</b>:
The group {tlevel} does not exist in {level_source}.
Group labels in {level_source} are: {paste(valid_levels, collapse = ', ')}.
Use commas to separate labels.
"
        )
        )
        return(list(
          labels = NULL,
          run_analysis = FALSE,
          error_string = error_string
        )
        )
      }
    }
  } else {
    if (sequential) {
      error_string <- paste(error_string, glue::glue(
        "
<b>{group_type} subset</b>:
Do the same for this subset.  No group can belong to both subsets.
"
      ))
    } else {
      error_string <- paste(error_string, glue::glue(
        "
<b>{group_type} subset</b>:
Type one or more group labels, separated by commas,
to form the {group_type} subset.
Group labels in {level_source} are: {paste(valid_levels, collapse = ', ')}.
"
      ))
    }
    return(list(
      label = NULL,
      run_analysis = FALSE,
      error_string = error_string
    )
    )
  }


  return(list(
    label = refgs,
    run_analysis = TRUE,
    error_string = error_string
  )
  )
}


jamovi_create_contrast <- function(reference, comparison) {
  ref_n <- length(reference)
  comp_n <- length(comparison)
  ref_vector <- rep(-1/ref_n, times = ref_n)
  comp_vector <- rep(1/comp_n, times = comp_n)
  contrast <- c(ref_vector, comp_vector)
  names(contrast) <- c(reference, comparison)
  return(contrast)
}


jamovi_set_notes <- function(result_element) {
  notes <- result_element$state

  if(length(notes) > 0) {
    result_element$setContent(
      paste(
        "<div class='jmv-results-error-message' style='color:black'>",
        paste(
          "<li>",
          notes,
          "</li>",
          collapse = ""
        ),
        "</div>"
      )
    )
    result_element$setVisible(TRUE)
  } else {
    result_element$setContent("")
    result_element$setVisible(FALSE)
  }


}


jamovi_heterogeneity_to_html <- function(measures) {
  for (x in 1:length(measures)) {
    measures[[x]] <- switch(
      measures[[x]],
      'tau^2' = "<i>&Tau;</i><sup>2</sup>",
      'tau' = "<i>&Tau;</i>",
      'I^2(%)'  = "<i>I</i><sup>2</sup>(%)",
      'H^2' = "<i>H</i><sup>2</sup>",
      "Diamond Ratio" = "Diamond Ratio",
      "heterogneity"
    )
  }

  return(measures)

}

jamovi_contingency_table <- function(self, estimate) {
  # Create a contingency table for Chi Square

  # Setup based on options for chi_table_option
  print_observed <- switch(
    self$options$chi_table_option,
    "observed" = TRUE,
    "expected" = FALSE,
    "both" = TRUE
  )

  print_expected <- switch(
    self$options$chi_table_option,
    "observed" = FALSE,
    "expected" = TRUE,
    "both" = TRUE
  )

  buffer <- if (print_observed & print_expected) "<BR>" else NULL

  observed_prefix <- if (print_observed) NULL else NULL
  observed_suffix <- if (print_observed) NULL else NULL
  expected_prefix <- if (print_expected) "(<i>" else NULL
  expected_suffix <- if (print_expected) "</i>)" else NULL
  total_prefix <- "<center><B>"
  total_suffix <- "</B></center>"

  # Handls on the table and the observed and expected tables
  tbl <- self$results$contingency_table
  observed <- estimate$properties$chi_square$observed
  expected <- estimate$properties$chi_square$expected

  cdims <- dim(observed)
  crows <- cdims[[1]]
  ccolumns <- cdims[[2]]


  # First, create a column for each level of the grouping variable
  for(x in 1:ccolumns) {
      tbl$addColumn(
        name = colnames(observed)[[x]],
        title = colnames(observed)[[x]],
        superTitle = estimate$properties$grouping_variable_name,
        type = 'text',
        visible = TRUE
      )
  }

  # Add an extra column for totals
  tbl$addColumn(
    name = "esci_Totals",
    title = "Total",
    type = "text",
    visible = TRUE
  )

  # Now set each row
  for(x in 1:crows) {

    observed_values <- if (print_observed) format(observed[x, ], digits = 1) else NULL
    expected_values <- if (print_expected) format(expected[x, ], digits = 2) else NULL

    cell_values <- paste(
      "<center>",
      observed_prefix,
      observed_values,
      observed_suffix,
      buffer,
      expected_prefix,
      expected_values,
      expected_suffix,
      "</center>",
      sep = ""
    )

    cell_values <- c(
      row.names(observed)[[x]],
      cell_values,
      paste(
        total_prefix,
        sum(observed[x, ]),
        total_suffix,
        sep = ""
      )
    )

    names(cell_values) <- c(
      "outcome_variable_level",
      colnames(observed),
      "esci_Totals"
    )

    tbl$addRow(
      rowKey = x,
      values = cell_values
    )
  }

  # Add begin and ending group formats for main cells to mark totals
  tbl$addFormat(rowNo = 1, col = 1, jmvcore::Cell.BEGIN_GROUP)
  tbl$addFormat(rowNo = x-1, col = 1, jmvcore::Cell.END_GROUP)
  tbl$addFormat(rowNo = x, col = 1, jmvcore::Cell.BEGIN_GROUP)



  # Add the totals row
  total_values <- colSums(observed)
  total_values <- c(
    "Total",
    paste(
      total_prefix,
      total_values,
      total_suffix,
      sep = ""
    ),
    paste(
      total_prefix,
      sum(observed),
      total_suffix
    )
  )

  names(total_values) <- c(
    "outcome_variable_level",
    colnames(observed),
    "esci_Totals"
  )


  tbl$addRow(
    rowKey = x+1,
    values = total_values
  )


  #Set a note with the chi square results
  mynote <- glue::glue(
    "&#120536;<sup>2</sup>({format(estimate$properties$chi_square$parameter, digits = 1)}) = {format(estimate$properties$chi_square$statistic, digits = 2)}, <i>p</i> = {esci_pvalr(estimate$properties$chi_square$p.value)}.  Continuity correction has *not* been applied."
  )

  tbl$setNote(
    key = "1",
    note = mynote,
    init = FALSE
  )
  #
  # Finally, rename title for outcome variable
  tbl$getColumn("outcome_variable_level")$setTitle(estimate$properties$outcome_variable_name)




  return(TRUE)

}



esci_pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {

  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }

  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
        return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}
