# esci_error_handling
# These functions provide informative error-handling for esci.
# One key goal for esci is to be user-friendly--to provide rigorous input
# checks with informative feedback when these fail.
# The code bundled together here is meant to implement this goal
# while reducing redundancy in input validation


# Stolen from the documentation for is.integer
# This function checks if x is a whole number, within the range of tolerance set for that system
# x - the variable to be checked
# tol - optional setting for the tolerance for determining a the variable contains a whole number
# is.wholenumber is *not* included in base r--added here to make esci_arrert_type more complete
is.whole.number <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}


# Assertion function - check that a variable is of a specific type
# var - the variable being passed
# condition - must be a quoted name of an 'is' function in R (e.g. 'is.number' or 'is.factor')
# This checks if var meets the criterion for the is function passed
# If it fails, it raises an informative esci_abort_bad_argument error
# Note that 'is' functions can be added -- for example above I've added is.wholenumber, a function *not* in base R
esci_assert_type <- function(var, condition) {

  # Get the name of the variable passed-- will use this if we need to generate an abort message
  var_enquo <- rlang::enquo(var)
  var_quoname <- rlang::as_name(var_enquo)

  # Make an informative error based on the 'is.' function passed
  condition_explain <- gsub(pattern = "is.", replacement = "be a ", x = condition)

  # Now use do.call to call the is function with the variable passed-- if fail, generate an esci_abort_bad_argument
  if(!do.call(what = condition, args = list(var), quote = FALSE)) esci_abort_bad_argument(arg = var_quoname,
                                                                                          must = condition_explain,
                                                                                          not = var)
}

# Assertion function - check if a variable is one of a set list of valid parameter strings
# var - the variable to be checked
# parameter_list - a vector of valid paramter strings
# If the value of var is not in paramter_list, raises an informative error
esci_assert_parameter_value <- function(var, parameter_list) {
  # Get name of passed variable
  var_enquo <- rlang::enquo(var)
  var_quoname <- rlang::as_name(var_enquo)

  if(!var %in% parameter_list) esci_abort_bad_parameter_value(arg = var_enquo, must = parameter_list, not = var)
}


# Assertion function - check if a variable is within a range or throw an esci_abort_out_of_range_error
# var - the variable to be checked
# lower - optional lower bound
# upper - optional upper bound
# lower_inclusive - set TRUE to test var >= lower, default of FALSE requires var > lower
# upper_inclusive - set TRUE to test var < upper, default of FALSE requires var < upper
# No error is thrown if both upper and lower are NULL--this would represent a check which is always passed, but does not raise an error
esci_assert_range <- function(var, lower = NULL, upper = NULL, lower_inclusive = FALSE, upper_inclusive = FALSE) {
  # Get name of passed variable
  var_enquo <- rlang::enquo(var)
  var_quoname <- rlang::as_name(var_enquo)

  # Define test symbols for upper and lower
  lower_symbol <- ifelse(lower_inclusive, ">=", ">")
  upper_symbol <- ifelse(upper_inclusive, "<=", "<")

  # Set a flag for the range test
  out_of_range <- FALSE

  # If lower was passed, check if var is out of the lower range
  if(!is.null(lower)) {
    if(lower_inclusive) {
      if(var < lower) out_of_range <- TRUE
    } else {
      if(var <= lower) out_of_range <- TRUE
    }
  }

  # If upper was passed, check if var is out of the upper range
  if(!is.null(upper)) {
    if(upper_inclusive) {
      if(var > upper) out_of_range <- TRUE
    } else {
      if(var >= upper) out_of_range <- TRUE
    }
  }

  # Now raise an error if the variable was out of range
  if(out_of_range) esci_abort_out_of_range(arg = var_quoname,
                                           lower = lower,
                                           upper = upper,
                                           not = var,
                                           lower_symbol = lower_symbol,
                                           upper_symbol = upper_symbol
                                           )
}

# Assertion function - check if a column name is in a dataframe
# data - a data frame or tibble
# var - string encoding of column name
# checks if var is in colnames(data)--if not generates an informative esci_abort_invalid_column_name error
# can handle a case where there are no columns in data
# but note, this does not check to ensure data is, in fact, a data.frame (user esci_assert_type for that)
esci_assert_valid_column_name <- function(data, var) {
  # Get name of passed data frame
  data_enquo <- rlang::enquo(data)
  data_quoname <- rlang::as_name(data_enquo)


  # get a list of valid column names or NULL if no valid columns
  if(length(colnames(data)) == 0) {
    valid_columns <- NULL
  } else {
    valid_columns <- colnames(data)
  }

  # Now check for var in colnames(data), and raise error if not found
  if(!var %in% colnames(data)) esci_abort_invalid_column_name(data = data_quoname,
                                                             var = var,
                                                             valid_columns = valid_columns
                                                             )

}

# Assertion function - check if a column is of a specific type
# data - a data frame or tibble
# var - string encoding of column name
# condition - the name of an is. function that will be used to check the column type
# This checks if var meets the criterion for the is function passed
# If it fails, it raises an informative esci_abort_bad_argument error
# Note that 'is' functions can be added -- for example above I've added is.wholenumber, a function *not* in base R
esci_assert_column_type <- function(data, var, condition) {
  # Get name of passed data frame
  data_enquo <- rlang::enquo(data)
  data_quoname <- rlang::as_name(data_enquo)

  # Make an informative error based on the 'is.' function passed
  condition_explain <- gsub(pattern = "is.", replacement = "be a ", x = condition)

  arg <- glue::glue("{data_quoname}${var}")

  # Now use do.call to call the is function with the variable passed-- if fail, generate an esci_abort_bad_argument
  if(!do.call(what = condition, args = list(data[[var]]), quote = FALSE)) esci_abort_bad_argument(arg = var,
                                                                                          must = condition_explain,
                                                                                          not = data[[var]])
}


# Assertion function - check if a column has at least lower number of valid rows
# data - a data frame or tibble
# var - string encoding of column name
# lower - required number of rows
# lower_inclusive - set to TRUE to test nrow >= lower; defaults to FALSE, which tests nrow > lower
# na.rm - set to FALSE and NAs will not be removed before the test; defaults to TRUE, which removes NAs prior to counting rows
# Note this function assumes data is a valid data frame, var is a valid column of data, etc...
# If this function passes it returns a row_data list:
#   row_data$total gives total rows
#   row_data$valid gives valid rows (*regardless of how na.rm is passed... should this change?)
#   row_data$missing gives number of NA rows
esci_assert_column_has_valid_rows <- function(
  data,
  var,
  lower,
  lower_inclusive = FALSE,
  na.rm = TRUE
) {
  # Define test symbols for upper and lower
  lower_symbol <- ifelse(lower_inclusive, ">=", ">")

  # Get name of passed data frame
  data_enquo <- rlang::enquo(data)
  data_quoname <- rlang::as_name(data_enquo)

  # Set flag for the test
  not_enough_rows <- FALSE

  # Make the row_data list
  # Note the need to use drop = FALSE, or one-column dataframes will become vectors on filtering, making nrow go quitely bonkers
  row_data <- list()
  row_data$total <- nrow(data)
  row_data$NA_rows <- which(is.na(data[[var]]))
  row_data$missing <- length(row_data$NA_rows)
  row_data$valid <- row_data$total - row_data$missing
  if (row_data$missing > 0) {
    p_text <- paste(row_data$NA_rows, collapse = ',')
    row_data$warning <- glue::glue("
  Dropping rows due to missing values!
  NA values in variable {var} were found in rows {p_text}
  These rows have been dropped
    ")
  } else {
    row_data$warning <- NULL
  }

  # Set which count will be used as our standard
  check_rows <- ifelse(na.rm,
                       row_data$valid,
                       row_data$total)


  # Check if there are enough rows
  if (lower_inclusive) {
    if(lower > check_rows) not_enough_rows <- TRUE
  } else {
    if(lower >= check_rows) not_enough_rows <- TRUE
  }

  # If not enough rows, raise an error
  if(not_enough_rows) {
    esci_abort_invalid_column_data(data = data_quoname,
                            var = var,
                            lower = lower,
                            row_data = row_data,
                            na.rm = na.rm,
                            lower_symbol = lower_symbol
                             )
  }

  # Otherwise, return the row data
  return(row_data)
}

esci_assert_vector_valid_length <- function(
  vector,
  lower = NULL,
  upper = NULL,
  lower_inclusive = FALSE,
  upper_inclusive = FALSE,
  na.rm = TRUE,
  na.invalid = FALSE
) {

  # Define test symbols for upper and lower
  lower_symbol <- ifelse(lower_inclusive, ">=", ">")
  upper_symbol <- ifelse(upper_inclusive, "<=", "<")

  # Get name of passed vector
  vector_enquo <- rlang::enquo(vector)
  vector_quoname <- rlang::as_name(vector_enquo)

  # Set flag for the test
  not_enough_rows <- FALSE
  too_many_rows <- FALSE

  # Make the row_data list
  row_data <- list()
  row_data$total <- length(vector)
  row_data$valid <- length(vector[!is.na(vector)])
  row_data$missing <- row_data$total - row_data$valid

  if (na.invalid & row_data$missing > 0) {
    esci_abort_invalid_vector_missing(
      vector_name = vector_quoname,
      vector = vector,
      row_data = row_data
    )
  }

  # Set which count will be used as our standard
  check_rows <- ifelse(na.rm,
                       row_data$valid,
                       row_data$total)


  # Check if there are enough rows
  if (!is.null(lower)) {
    if (lower_inclusive) {
      if(lower > check_rows) not_enough_rows <- TRUE
    } else {
      if(lower >= check_rows) not_enough_rows <- TRUE
    }
  }

  if (!is.null(upper)) {
    if (upper_inclusive) {
      if(check_rows > upper) too_many_rows <- TRUE
    } else {
      if(check_rows >= upper) too_many_rows <- TRUE
    }
  }



  # If not enough rows, raise an error
  if(not_enough_rows) {
    esci_abort_invalid_vector_data(vector = vector_quoname,
                                   lower = lower,
                                   row_data = row_data,
                                   na.rm = na.rm,
                                   lower_symbol = lower_symbol
    )
  }
  if(too_many_rows) {
      esci_abort_invalid_vector_data(vector = vector_quoname,
                                     lower = upper,
                                     row_data = row_data,
                                     na.rm = na.rm,
                                     lower_symbol = upper_symbol
      )
  }

  # Otherwise, return the row data
  return(row_data)
}

# Informative abort function
# This function was stolen (and slightly modified) from Hadley Wickham's Advanced R Programming
#   https://adv-r.hadley.nz/conditions.html
# arg - the name of the variable, as a string
# must - the class the variable should be
# not - the variable itself--this will be used to tell the user what the variable's current class is
# In Wickham's original, it used typeof(not).  I switched it to class because it seemed more informative
#   (e.g. reporting a factor as a factor rather than an integer)
esci_abort_bad_argument <- function(arg, must, not = NULL) {
  # Build the error message
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- class(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  # Raise the error
  rlang::abort("error_bad_argument",
        message = msg,
        arg = arg,
        must = must,
        not = not
  )
}

# Informative abort function for when a variable is out of range
# Adapted from Wickham's abort_bad_argument function https://adv-r.hadley.nz/conditions.html
# This function generates an informative abort error when an argument is out of range
esci_abort_out_of_range <- function(arg, lower = NULL, upper = NULL, not = NULL, lower_symbol = ">", upper_symbol = "<") {
  # Build the error message
  and_text <- ""
  msg <- glue::glue("`{arg}` must be")

  if (!is.null(lower)) {
    msg <- glue::glue("{msg} {lower_symbol} {lower}")
    and_text <- " and"
  }

  if (!is.null(upper)) {
    msg <- glue::glue("{msg}{and_text} {upper_symbol} {upper}")
  }

  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not {not}.")
  }

  # Raise the error
  rlang::abort("error_out_of_range",
               message = msg,
               arg = arg,
               lower = lower,
               upper = upper,
               not = not
  )
}


esci_abort_bad_parameter_value <- function(arg, must, not) {
  # Build the error message
  msg <- glue::glue("`{arg}` must have a value in this list: {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not {not}.")
  }

  # Raise the error
  rlang::abort("error_invalid_parameter_value",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}

esci_abort_invalid_column_name <- function(data, var, valid_columns = NULL) {
  # Build the error message
  if (is.null(valid_columns)) {
    column_msg <- glue::glue("No valid columns were found in '{data}'.")
  } else {
    column_string <- glue::glue_collapse(valid_columns, sep = ", ", last = " and ")
    column_msg <- glue::glue("Valid column names for '{data}' are: {column_string}.")
  }

  msg <- glue::glue("`{var}` is not a valid column name in '{data}'.\n{column_msg}")

  # Raise the error
  rlang::abort("error_invalid_column_name",
               message = msg,
               data = data,
               var = var
  )
}

esci_abort_invalid_column_data <- function(data, var, lower, row_data, na.rm, lower_symbol = ">") {
  # Build error message
  row_report <- glue::glue("Currently, column '{var}' in '{data}' has {row_data$total} row(s),")
  if(na.rm) row_report <- glue::glue("{row_report} of which {row_data$missing} have missing values, leaving {row_data$valid} valid row(s).")

  namsg <- ifelse(na.rm, ", where the count of valid rows does *not* included missing/NA values", "")

  msg <- glue::glue("The `{var}` column in '{data}' must have {lower_symbol} {lower} valid rows{namsg}.")
  msg <- glue::glue("{msg}\n{row_report}")

  # Raise the error
  rlang::abort("error_invalid_column_data",
               message = msg,
               data = data,
               var = var,
               lower = lower,
               row_report = row_report,
               lower_symbol = lower_symbol
  )
}

esci_abort_invalid_vector_missing <- function(
  vector_name, vector, row_data
) {
  msg <- glue::glue("
The vector '{vector_name} has {row_data$missing} NA elements but cannot have any.
{vector_name} = c({paste(vector, collapse = ', ')})
")

  rlang::abort("error_invalid_vector_missingdata",
               message = msg,
               vector = vector,
               row_data = row_data
  )
}


esci_abort_invalid_vector_data <- function(vector, lower, row_data, na.rm, lower_symbol = ">") {
  # Build error message
  row_report <- glue::glue("Currently, the length of vector '{vector}' is {row_data$total} row(s)")
  if(na.rm) row_report <- glue::glue("{row_report}, and {row_data$missing} elements have missing values, leaving a valid length of {row_data$valid}.")

  namsg <- ifelse(na.rm, ", where the lenth of valid data does *not* included missing/NA values", "")

  msg <- glue::glue("The vector `{vector}` must have {lower_symbol} {lower} valid rows{namsg}.")
  msg <- glue::glue("{msg}\n{row_report}")

  # Raise the error
  rlang::abort("error_invalid_vector_data",
               message = msg,
               vector = vector,
               lower = lower,
               row_data = row_data,
               na.rm = na.rm
  )
}


esci_abort_calculation_error <- function(arg, problem) {
  # Build the error message
  msg <- glue::glue("`{arg}` {problem}")

  # Raise the error
  rlang::abort("error_out_of_range",
               message = msg,
               arg = arg,
               problem = problem
  )
}

