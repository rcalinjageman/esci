
# This file is a generated template, your changes will not be overwritten

jamovimdiffindcontrastClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiffindcontrastClass",
    inherit = jamovimdiffindcontrastBase,
    private = list(
        .init = function() {

          jamovi_mdiff_initialize(self, grouping_variable = TRUE)


        },
        .run = function() {

        from_raw <- (self$options$switch == "from_raw")

        estimate <- jamovi_mdiff_contrastindependent(
            self = self,
            outcome_variables = self$options$outcome_variable,
            save_raw_data = FALSE
        )

        # Print any notes that emerged from running the analysis
        jamovi_set_notes(self$results$help)

        # Check to see if the analysis ran
        #  If null, return
        #  If error, return the error
        if(is.null(estimate)) return(TRUE)
        if(is(estimate, "try-error")) stop(estimate[1])

        # Add in MoE
        estimate$es_mean_difference$moe <- (estimate$es_mean_difference$UL - estimate$es_mean_difference$LL)/2
        estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

        # Fill tables
        jamovi_estimate_filler(self, estimate, TRUE)


        # Deal with plots ----------------------------------------
        # Set up array of estimation plots
        keys <- if (from_raw)
          self$options$outcome_variable
        else
          jamovi_sanitize(
            self$options$outcome_variable_name,
            "My outcome variable",
            na_ok = FALSE
          )

        for (my_key in keys) {
          image <- self$results$estimation_plots$get(key=my_key)
          image$setState(my_key)
        }

        if (length(keys) > 1) {
          self$results$estimation_plots$setTitle(
            paste(
              self$results$estimation_plots$title,
              "s",
              sep = ""
            )
          )
        }


        },
        .estimation_plots = function(image, ggtheme, theme, ...) {

          if (is.null(image$state))
            return(FALSE)

          # Redo analysis
          estimate <- jamovi_mdiff_contrastindependent(
            self = self,
            outcome_variables = c(image$state),
            save_raw_data = TRUE
          )

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          # if (is.null(estimate$properties$contrast)) {
          #   return(TRUE)
          # }

          myplot <- jamovi_plot_mdiff(
            self,
            estimate,
            image,
            ggtheme,
            theme
          )

          print(myplot)
          TRUE

        })
)



jamovi_mdiff_contrastindependent <- function(
  self,
  outcome_variables = NULL,
  save_raw_data = FALSE
) {
  # This function will build the analysis and then return
  #   - the estimate (class esci_estimate)
  #   - an error (class try-error)
  #   - or NULL (representing analysis not ready)


  # Prelim -----------------------------------------------------
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)


  # Step 1 - Check if analysis basics are defined ---------------
  #  if not, return NULL
  if(from_raw) {
    if (
      is.null(self$options$grouping_variable) |
      is.null(outcome_variables) |
      length(outcome_variables) == 0
    ) return(NULL)
  } else {
    if(
      is.null(self$options$means) |
      is.null(self$options$sds) |
      is.null(self$options$ns) |
      is.null(self$options$grouping_variable_levels)
    ) return(NULL)
  }

  # Step 2: Check on the contrast --------------------------------
  clabels <- self$options$comparison_labels
  rlabels <- self$options$reference_labels


  if(from_raw) {
    level_source <- self$options$grouping_variable
    valid_levels <- levels(as.factor(self$data[, level_source]))
    multiplier <- length(self$options$outcome_variable)
  } else {
    level_source <- self$options$grouping_variable_levels
    valid_levels <- self$data[
      which(!is.na(self$data[, self$options$grouping_variable_levels])),
      level_source
    ]
    multiplier <- 1
  }


  # This function checks if the contrast is valid or not
  reference_result <- jamovi_check_contrast(
    labels = rlabels,
    valid_levels = valid_levels,
    level_source = level_source,
    group_type = "Reference",
  )

  # Same, but with comparison labels
  comparison_result <- jamovi_check_contrast(
    labels = clabels,
    valid_levels = valid_levels,
    level_source = level_source,
    group_type = "Comparison",
    sequential = !is.null(reference_result$error_string)
  )

  notes <- c(notes,
             reference_result$error_string,
             comparison_result$error_string
  )

  overlap <- reference_result$label %in% comparison_result$label
  if (length(reference_result$label[overlap]) != 0) {
    next_note <- glue::glue(
      "<b>Error</b>: Reference and comparison groups must be distinct, but
{reference_result$label[overlap]} has been entered in both"
    )
    notes <- c(notes, next_note)
  }


  contrast <- if(length(notes) > 0)
    NULL
  else
    jamovi_create_contrast(
      reference_result$label,
      comparison_result$label
    )

  # Step 3: Run analysis ------------------------------------------
  # Fill in analysis properties

  # If from summary:
  # get outcome and grouping variable names
  # and set notes if they have been replaced
  if(!from_raw) {
    outcome_variable_name <- jamovi_sanitize(
      self$options$outcome_variable_name,
      return_value = "My outcome variable",
      na_ok = FALSE
    )
    grouping_variable_name <- jamovi_sanitize(
      self$options$grouping_variable_name,
      return_value = "My grouping variable",
      na_ok = FALSE
    )
    notes <- c(
      notes,
      names(outcome_variable_name),
      names(grouping_variable_name)
    )
  }

  args <- list()
  conf_level <- jamovi_sanitize(
    my_value = self$options$conf_level,
    return_value = 95,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 75,
    lower_inclusive = FALSE,
    upper = 100,
    upper_inclusive = FALSE,
    my_value_name = "Confidence level"
  )
  notes <- c(notes, names(conf_level))
  args$conf_level <- conf_level/100
  args$assume_equal_variance <- self$options$assume_equal_variance
  args$contrast <- contrast

  # Set args for summary and raw data cases
  if (from_raw) {
    # Analysis from raw data
    args$data <- self$data
    args$grouping_variable <- self$options$grouping_variable
    args$outcome_variable <- outcome_variables
    for (x in 1:length(args$outcome_variable)) {

      if (is.null(levels(self$data[[args$outcome_variable[[x]]]]))) {
        args$data[[args$outcome_variable[[x]]]] <- as.numeric(args$data[[args$outcome_variable[[x]]]])
      } else {
        args$data[[args$outcome_variable[[x]]]] <- as.numeric(levels(self$data[[args$outcome_variable[[x]]]]))[self$data[[args$outcome_variable[[x]]]]]
        notes <- c(
          notes,
          paste(
            "Converted nominal variable ", args$outcome_variable[[x]], "to numeric; be sure this makes sense."
          )
        )
      }

    }
    call <- esci::estimate_mdiff_ind_contrast
  } else {
    # Analysis from summary data
    group_labels <- self$data[, self$options$grouping_variable_levels]
    valid_rows <- which(!is.na(group_labels))

    if(length(valid_rows) != length(group_labels)) {
      msg <- glue::glue("
There are {length(group_labels) - length(valid_rows)} empty values
in {self$options$grouping_variable_levels}.  Rows with empty group labels have been
**dropped** from the analysis
                    ")
      notes <- c(notes, msg)
    }

    args$means <- self$data[valid_rows, self$options$means]
    args$sds <- self$data[valid_rows, self$options$sds]
    args$ns <- self$data[valid_rows, self$options$ns]
    args$grouping_variable_levels <- as.character(group_labels[valid_rows])
    args$outcome_variable_name <- outcome_variable_name
    args$grouping_variable_name <- grouping_variable_name
    call <- esci::estimate_mdiff_ind_contrast
  }


  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  # For summary data, store in a list based on outcome_variable_name
  if (!is(estimate, "try-error")) {
    estimate <- jamovi_add_htest_mdiff(
      self = self,
      estimate = estimate
    )

    notes <- c(notes, estimate$warnings)
    self$results$help$setState(notes)
    if(!from_raw) {
      estimate_list <- list()
      key <- outcome_variable_name
      estimate_list[[key]] <- estimate
      class(estimate_list) <- "esci_estimate"
      estimate <- esci_estimate_consolidate(estimate_list)
    }
  }



  return(estimate)
}


