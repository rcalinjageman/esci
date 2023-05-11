#' Estimate a correlation.
#'
#' @description
#' Returns effect sizes appropriate for estimating the linear relationship
#' between two quantitative variables
#'
#' @param data For raw data - a dataframe or tibble
#' @param x For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param y For raw data - The column name of the outcome
#'   variable, or a vector of numeric data
#' @param r For summary data, a pearson's r correlation coefficient
#' @param n For summary data - An integer > 0
#' @param x_variable_name Optional friendly name for the x variable.
#'   Defaults to 'My x variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param y_variable_name Optional friendly name for the y variable.
#'   Defaults to 'My y variable' or the outcome variable column name if a
#'   data frame is passed.
#' @param conf_level The confidence level for the confidence interval.  Given in
#'   decimal form.  Defaults to 0.95.
#' @param save_raw_data For raw data; defaults to TRUE; set to FALSE to save
#'   memory by not returning raw data in estimate object
#'
#'
#' @return Returnsobject of class esci_estimate
#'
#'
#' @examples
#' # From Raw Data ------------------------------------
#' # Just pass in the data source, grouping column, and outcome column.
#' # You can pass these in by position, skipping the labels:
#'
#' # Note... not sure if PlantGrowth dataset meets assumptions for this analysis
#' estimate_correlation(
#'  datasets::anscombe,
#'  x1,
#'  y1
#' )
#'
#' @export
estimate_correlation <- function(
  data = NULL,
  x = NULL,
  y = NULL,
  r = NULL,
  n = NULL,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  analysis_type <- "Undefined"


  # Check to see if summary data has been passed
  if (!is.null(r)) {

    # Summary data is passed, so check to make sure raw data not included
    if(!is.null(data))  stop(
      "You have passed summary statistics,
      so don't pass the 'data' parameter used for raw data.")
    if(!is.null(x)) stop(
      "You have passed summary statistics,
      so don't pass the 'x' parameter used for raw data.")
    if(!is.null(y)) stop(
      "You have passed summary statistics,
      so don't pass the 'y' parameter used for raw data.")

    # Looks good, we can pass on to summary data
    analysis_type <- "summary"

  } else {
    # Raw data has been passed, first sure summary data is not passed
    if(!is.null(r))  stop(
      "You have passed raw data,
      so don't pass the 'r' parameter used for summary data.")
    if(!is.null(n))  stop(
      "You have passed raw data,
      so don't pass the 'n' parameter used for summary data.")


    if (is.null(data)) {
      analysis_type <- "vector"
    } else {

      # Check x_variable -- if it is an unquoted column name
      #  turn it into a string and store back to x
      is_char <- try(
        is.character(x), silent = TRUE
      )
      if (class(is_char) == "try-error") {
        # If not a character, must have been quoted
        x_enquo <- rlang::enquo(x)
        x_quoname <- try(
          eval(rlang::as_name(x_enquo)), silent = TRUE
        )
        if (class(x_quoname) != "try-error") {
          # This only succeeds if outcome_variable was passed unquoted
          # Reset outcome_variable to be fully quoted
          x <- x_quoname
          x_variable_name <- x
        } else {
          stop("Could not parse x")
        }
      }


      is_char <- try(
        is.character(y), silent = TRUE
      )
      if (class(is_char) == "try-error") {
        # If not a character, must have been quoted
        y_enquo <- rlang::enquo(y)
        y_quoname <- try(
          eval(rlang::as_name(y_enquo)), silent = TRUE
        )
        if (class(y_quoname) != "try-error") {
          # This only succeeds if y was passed unquoted
          # Reset y to be fully quoted
          y <- y_quoname
        } else {
          stop("Could not parse y")
        }
      }

      if (length(y) > 1 | length(x) > 1) {
        analysis_type <- "jamovi"
      } else {
        analysis_type <- "data.frame"
      }

    }


  }

  # At this point, we've figured out the type of data passed
  #  so we can dispatch

  # I put all the dispatches here, at the end, to make it easier to
  #   update in case the underlying function parameters change

  if(analysis_type == "jamovi") {
    return(
      estimate_correlation.jamovi(
        data = data,
        vars = c(x, y),
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )
    )
  } else if (analysis_type == "summary") {
    return(
      estimate_correlation.summary(
        r = r,
        n = n,
        x_variable_name = x_variable_name,
        y_variable_name = y_variable_name,
        conf_level = conf_level
      )
    )
  } else if (analysis_type == "vector") {
    if (is.null(x_variable_name) | x_variable_name == "My x variable") {
      x_variable_name <- deparse(substitute(x))
    }
    if (is.null(y_variable_name) | y_variable_name == "My y variable") {
      y_variable_name <- deparse(substitute(y))
    }
    return(
      estimate_correlation.vector(
        x = x,
        y = y,
        x_variable_name = x_variable_name,
        y_variable_name = y_variable_name,
        conf_level = conf_level,
        save_raw_data = save_raw_data
        )
    )
  } else if (analysis_type == "data.frame") {
    return(
      estimate_correlation.data.frame(
        data = data,
        x = x,
        y = y,
        conf_level = conf_level,
        save_raw_data = save_raw_data
      )
    )
  }

  stop("Something went wrong dispatching this function")

}



estimate_correlation.summary <- function(
  r,
  n,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  conf_level = 0.95
){


  # Input checks      ---------------------------------------------------------
  # This function expects:
  # r - numeric data between -1 and 1
  # ns - numeric integer > 1
  # conf_level is >0 and <1
  # x_variable_name - optional, non-zero length character
  # y_variable_name - optional, non-zero length character

  # Check r
  esci_assert_type(r, "is.numeric")
  esci_assert_range(
    r,
    lower = -1,
    lower_inclusive = TRUE,
    upper = 1,
    upper_inclusive = TRUE
  )
  # Check n
  esci_assert_type(n, "is.numeric")
  esci_assert_type(n, "is.whole.number")
  esci_assert_range(
    n,
    lower = 2,
    lower_inclusive = TRUE
  )
  # Check variable names
  # Check variable names
  esci_assert_type(x_variable_name, "is.character")
  esci_assert_type(y_variable_name, "is.character")
  # Check conf.level
  esci_assert_type(conf_level, "is.numeric")
  esci_assert_range(
    conf_level,
    lower = 0,
    upper = 1,
    lower_inclusive = FALSE,
    upper_inclusive = FALSE
  )


  # Do analysis ------------------------------------
  estimate <- list()
  class(estimate) <- "esci_estimate"
  estimate$es_r <- wrapper_ci.cor(
    r = r,
    n = n,
    conf_level = conf_level,
    x_variable_name = x_variable_name,
    y_variable_name = y_variable_name
  )

  estimate$properties <- list(
    data_type = "Summary",
    data_source = NULL,
    conf_level = conf_level
  )

  return(estimate)

}


estimate_correlation.vector <- function(
  x,
  y,
  x_variable_name = "My x variable",
  y_variable_name = "My y variable",
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  # Input checks --------------------------------------------------------------
  # This function expects:
  #  outcome_variable to be a vector of numeric data:
  #      with > 2 valid rows
  #  save_raw_data is a logical, TRUE or FALSE

  # Check x variable
  esci_assert_type(x, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    x,
    lower = 2,
    lower_inclusive = FALSE,
    na.invalid = TRUE
  )
  # Check y variable
  esci_assert_type(y, "is.numeric")
  row_report <- esci_assert_vector_valid_length(
    y,
    lower = 2,
    lower_inclusive = FALSE,
    na.invalid = TRUE
  )
  # Check that x and y are same length
  if (length(x) != length(y)) {
    stop("x and y variable must be same length")
  }

  mydata <- data.frame(
    x = x,
    y = y
  )
  colnames(mydata) <- c(
    x_variable_name,
    y_variable_name
  )

  estimate <- estimate_correlation.data.frame(
    data = mydata,
    x = x_variable_name,
    y = y_variable_name,
    conf_level = conf_level,
    save_raw_data = save_raw_data
  )


  estimate$properties$data_type <- "vector"
  estimate$properties$data_source <- NULL

  return(estimate)
}


estimate_correlation.data.frame <- function(
    data,
    x,
    y,
    conf_level = 0.95,
    save_raw_data = TRUE
) {

  # Input checks
  # This function expects:
  #   data to be a data frame
  #   outcome_variable to be a numeric column in data, with more than 2 rows
  esci_assert_type(data, "is.data.frame")

  # Validate each variable name
  esci_assert_valid_column_name(data, x)
  esci_assert_column_type(data, x, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    x,
    lower = 3,
    na.rm = TRUE
  )

  esci_assert_valid_column_name(data, y)
  esci_assert_column_type(data, y, "is.numeric")
  esci_assert_column_has_valid_rows(
    data,
    y,
    lower = 3,
    na.rm = TRUE
  )

  # Check save_raw_data
  esci_assert_type(save_raw_data, "is.logical")


  # Data prep ------------------------------------------------
  data_valid <- data[ , c(x, y)]
  colnames(data_valid) <- c("x", "y")
  na_count <- nrow(data_valid) - nrow(na.omit(data_valid))
  data_valid <- na.omit(data_valid)


  # Do the analysis ----------------------------------------------
  estimate <- list()
  class(estimate) <- "esci_estimate"
  estimate$properties <- list(
    data_type = "data.frame",
    data_source = deparse(substitute(data)),
    conf_level = conf_level
  )


  estimate$overview <- estimate_magnitude.jamovi(
    data = data,
    outcome_variables = c(x, y),
    conf_level = conf_level,
    save_raw_data = FALSE
  )$overview


  n <- nrow(data_valid)
  r <- cor(data_valid$x, data_valid$y)

  estimate$es_r <- estimate_correlation.summary(
    r = r,
    n = n,
    x_variable_name = x,
    y_variable_name = y,
    conf_level = conf_level
  )$es_r

  # Regression model
  lbf <- lm(y ~ x, data = data_valid)
  b <- coefficients(lbf)[2]
  a <- coefficients(lbf)[1]
  cis <- confint(lbf, level = conf_level)
  LL <- cis[, 1]
  UL <- cis[, 2]

  estimate$regression <- data.frame(
    component = c("Intercept (a)", "Slope (b)"),
    values = c(a, b),
    LL = LL,
    UL = UL
  )
  row.names(estimate$regression) <- NULL
  estimate$properties$lm <- lbf

  estimate$regression_properties <- list()
  estimate$regression_properties$message <- paste(
    "\U0176 = ",
    formatC(a, format = "fg", digits = 4),
    " + ",
    formatC(b, format="fg", digits=4),
    "*X",
    sep = ""
  )

  estimate$regression_properties$message_html <- paste(
    "<i>&#374;</i> = ",
    formatC(a, format = "fg", digits = 4),
    " + ",
    formatC(b, format="fg", digits=4),
    " * <i>X</i>",
    sep = ""
  )

  # Prediction intervals
  pinterval <- suppressWarnings(
    predict(
      lbf,
      interval = "prediction",
      level = conf_level
    )
  )
  data_valid <- cbind(data_valid, pinterval)


  if (na_count > 0) {
    estimate$overview_properties$message <-
      paste(
        "N_pairs = ",
        nrow(data_valid),
        ".  There were ",
        na_count,
        " rows with incomplete data.  All analyses are based only on the ",
        nrow(data_valid),
        " rows with complete data.",
        sep = ""
      )
    estimate$overview_properties$message_html <-
      paste(
        "<i>N</i><sub>pairs</sub> = ",
        nrow(data_valid),
        "<br>There were ",
        na_count,
        " rows with incomplete data.<br>All analyses are based only on the ",
        nrow(data_valid),
        " rows with complete data.",
        sep = ""
      )
  }


  if(save_raw_data) {
    estimate$raw_data <- data_valid
  }


  return(estimate)

}


estimate_correlation.jamovi <- function(
  data,
  vars,
  conf_level = 0.95,
  save_raw_data = TRUE
) {

  # Input checks
  # This function expects:
  #   data to be a data frame
  #   outcome_variable to be a numeric column in data, with more than 2 rows
  esci_assert_type(data, "is.data.frame")

  # Validate each variable name
  for (myvar in vars) {
    esci_assert_valid_column_name(data, myvar)
    esci_assert_column_type(data, myvar, "is.numeric")
    esci_assert_column_has_valid_rows(
      data,
      myvar,
      lower = 2,
      na.rm = TRUE
    )

  }

  # Check save_raw_data
  esci_assert_type(save_raw_data, "is.logical")


  # Do the analysis ----------------------------------------------
  estimate <- list()
  class(estimate) <- "esci_estimate"
  estimate$properties <- list(
    data_type = "data.frame",
    data_source = deparse(substitute(data)),
    conf_level = conf_level
  )

  estimate$overview <- estimate_magnitude.jamovi(
    data = data,
    outcome_variables = vars,
    conf_level = conf_level,
    save_raw_data = FALSE
  )$overview

  estimate$es_r <- NULL

  cor_matrix <- cor(data[, vars], use = "pairwise.complete.obs")
  n_matrix <- crossprod(!is.na(data[, vars]))
  ll_matrix <- n_matrix
  ul_matrix <- n_matrix
  completed <- n_matrix
  completed[] <- 0

  # Cycle through the list of columns;
  #  for each call estimate_magnitude.data-frame, which handles 1 column
  for (x in vars) {
    for (y in vars) {
      if (x != y) {
        this_r <- estimate_correlation.summary(
          r = cor_matrix[x, y],
          n = n_matrix[x, y],
          x_variable_name = x,
          y_variable_name = y,
          conf_level = conf_level
        )$es_r
        ll_matrix[x,y] <- this_r[1, "LL"]
        ul_matrix[x,y] <- this_r[1, "UL"]
        completed[x, y] <- 1
        if (completed[y, x] == 0) {
          estimate$es_r <- rbind(
            this_r,
            estimate$es_r
          )
        }
      }
    }
  }

  if (nrow(estimate$es_r) > 1) {
    estimate$es_r <- estimate$es_r[dim(estimate$es_r)[1]:1,]
  }

  cor_matrix[] <- paste(
    "r = ",
    format(cor_matrix, digits = 2),
    "\n",
    conf_level * 100,
    "% CI [",
    format(ll_matrix, digits = 2),
    ", ",
    format(ul_matrix, digits = 2),
    "]\n N = ",
    n_matrix,
    sep = ""
  )

  diag(cor_matrix) <- 1

  estimate$corelation_matrix <- cor_matrix


  if(save_raw_data) {
    estimate$raw_data <- data[ , c(vars)]
  }


  return(estimate)

}


