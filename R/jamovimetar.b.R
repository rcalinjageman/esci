
# This file is a generated template, your changes will not be overwritten

jamovimetarClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimetarClass",
    inherit = jamovimetarBase,
    private = list(
      .init = function() {

        jamovi_meta_initialize(
          self = self,
          has_switch = FALSE
        )


      },
      .run = function() {

        estimate <- jamovi_meta_r(self)


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
          has_reference = FALSE,
          has_switch = FALSE,
          has_aev = FALSE
        )

      },
      .estimation_plots = function(image, ggtheme, theme, ...) {
        # Redo analysis
        estimate <- jamovi_meta_r(self)

        if(!is(estimate, "esci_estimate"))
          return(TRUE)

        myplot <- jamovi_meta_forest_plot(
          estimate = estimate,
          self = self,
          ggtheme = ggtheme,
          theme = theme,
          has_switch = FALSE
        )

        print(myplot)
        TRUE

      })
)


jamovi_meta_r <- function(self) {

  # Prelim -----------------------------------------------------
  notes <- c(NULL)

  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()

  if (
    is.null(self$options$rs) |
    is.null(self$options$ns)
  ) return(NULL)


  # Step 2: Get analysis properties-----------------------------
  call <- esci::meta_r

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


  for (element in args) {
    notes <- c(notes, names(element))
  }


  if (!is.null(self$options$moderator)) {
    args$moderator <- self$options$moderator
  }

  if (!is.null(self$options$labels)) {
    args$labels <- self$options$labels
  }

  args$data <- self$data
  args$rs <- self$options$rs
  args$ns <- self$options$ns

  args$random_effects <- self$options$random_effects %in% c("random_effects", "compare")



  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))
  if(is(estimate, "try-error")) stop(estimate[1])

  estimate$raw_data$label <- as.character(estimate$raw_data$label)
  if (!is.null(self$options$moderator)) {
    estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
  }


  if (!is(estimate, "try-error")) {
    if (length(estimate$warnings) > 0) {
      notes <- c(notes, estimate$warnings)
    }
  }

  self$results$help$setState(notes)

  return(estimate)
}
