
# This file is a generated template, your changes will not be overwritten

jamovimetameanClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimetameanClass",
    inherit = jamovimetameanBase,
    private = list(
      .init = function() {

        jamovi_meta_initialize(
          self = self,
          has_switch = TRUE
        )

      },
      .run = function() {

        estimate <- jamovi_meta_mean(self)


        # Print any notes that emerged from running the analysis
        jamovi_set_notes(self$results$help)

        # Check to see if the analysis ran
        #  If null, return
        #  If error, return the error
        if(is.null(estimate)) return(TRUE)
        if(is(estimate, "try-error")) stop(estimate[1])

        # Fill tables
        jamovi_meta_run(
          estimate = estimate,
          self = self,
          has_reference = TRUE,
          has_switch = TRUE,
          has_aev = FALSE
        )



      },
      .estimation_plots = function(image, ggtheme, theme, ...) {
        # Redo analysis
        estimate <- jamovi_meta_mean(self)

        if(!is(estimate, "esci_estimate"))
          return(TRUE)

        myplot <- jamovi_meta_forest_plot(
          estimate = estimate,
          self = self,
          ggtheme = ggtheme,
          theme = theme
        )

        print(myplot)
        TRUE

      })
)


jamovi_meta_mean <- function(self) {

  # Prelim -----------------------------------------------------
  notes <- c(NULL)
  from_raw <- self$options$switch == "from_raw"

  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()

  if (from_raw) {
    if (
      is.null(self$options$means) |
      is.null(self$options$sds) |
      is.null(self$options$ns)
    ) return(NULL)
    call <- esci::meta_mean
  } else {
    if (
      is.null(self$options$ds) |
      is.null(self$options$dns)
    ) return(NULL)
    call <- esci::meta_d1
  }


  # Step 2: Get analysis properties-----------------------------
  args$effect_label <- jamovi_sanitize(
    self$options$effect_label,
    return_value = "My effect",
    na_ok = FALSE
  )

  args$conf_level <- jamovi_sanitize(
    my_value = self$options$conf_level,
    return_value = 95,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 75,
    lower_inclusive = FALSE,
    upper = 100,
    upper_inclusive = FALSE,
    my_value_name = "Confidence level"
  )/100


  if (from_raw) {
    if (length(trimws(as.character(self$options$reference_mean))) == 0) {
    } else {
      if (trimws(as.character(self$options$reference_mean)) %in% c("")) {

      } else {
        args$reference_mean <- jamovi_sanitize(
          self$options$reference_mean,
          return_value = 0,
          na_ok = FALSE,
          convert_to_number = TRUE
        )
      }
    }
  }

  for (element in args) {
    notes <- c(notes, names(element))
  }

  if (from_raw) {
    args$data <- self$data
    args$means <- self$options$means
    args$sds <- self$options$sds
    args$ns <- self$options$ns
    args$reported_effect_size <- self$options$reported_effect_size

    if (!is.null(self$options$moderator)) {
      args$moderator <- self$options$moderator
    }

    if (!is.null(self$options$labels)) {
      args$labels <- self$options$labels
    }
  } else {
    args$data <- self$data
    args$ds <- self$options$ds
    args$ns <- self$options$dns

    if (!is.null(self$options$dmoderator)) {
      args$moderator <- self$options$dmoderator
    }

    if (!is.null(self$options$dlabels)) {
      args$labels <- self$options$dlabels
    }

  }

  args$random_effects <- self$options$random_effects %in% c("random_effects", "compare")

  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))
  if(is(estimate, "try-error")) stop(estimate[1])

  estimate$raw_data$label <- as.character(estimate$raw_data$label)

  if (from_raw) {
    if (!is.null(self$options$moderator)) {
      estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
    }
  } else {
    if (!is.null(self$options$dmoderator)) {
      estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
    }
  }

  if (!is(estimate, "try-error")) {
    if (length(estimate$warnings) > 0) {
      notes <- c(notes, estimate$warnings)
    }
  }

  self$results$help$setState(notes)

  return(estimate)
}
