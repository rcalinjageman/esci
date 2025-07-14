
# This file is a generated template, your changes will not be overwritten

jamovimetapdiffClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimetapdiffClass",
    inherit = jamovimetapdiffBase,
    private = list(
        .init = function() {

          jamovi_meta_initialize(
            self = self,
            has_switch = FALSE
          )

          tbl_raw_data <- self$results$raw_data
          tbl_es_meta <- self$results$es_meta


          if (self$options$reported_effect_size == "RD") {
            mykey <- "<i>P</i><sub>diff</sub> is calculated as <i>P</i><sub>comparison</sub> - <i>P</i><sub>reference</sub>"

            # tbl_es_meta$setNote(
            #   key = "mykey",
            #   note = mykey
            # )

            tbl_raw_data$setNote(
              key = "mykey",
              note = mykey
            )

          }


        },
        .run = function() {

          estimate <- jamovi_meta_pdiff_two(self)


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
            has_aev = FALSE,
            has_effect_size_names = TRUE
          )


        },
        .estimation_plots = function(image, ggtheme, theme, ...) {
          # Redo analysis
          estimate <- jamovi_meta_pdiff_two(self)

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



jamovi_meta_pdiff_two <- function(self) {

  # Prelim -----------------------------------------------------
  notes <- c(NULL)

  # Step 1 - Check if analysis basics are defined ---------------
  args <- list()

  if (
    is.null(self$options$reference_cases) |
    is.null(self$options$reference_ns) |
    is.null(self$options$comparison_cases) |
    is.null(self$options$comparison_ns)
  ) return(NULL)


  # Step 2: Get analysis properties-----------------------------
  call <- esci::meta_pdiff_two

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

  args$reported_effect_size <- self$options$reported_effect_size


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
  args$reference_cases <- self$options$reference_cases
  args$reference_ns <- self$options$reference_ns
  args$comparison_cases <- self$options$comparison_cases
  args$comparison_ns <- self$options$comparison_ns

  args$random_effects <- self$options$random_effects %in% c("random_effects", "compare")

  # self$results$debug$setVisible(TRUE)
  # self$results$debug$setContent(args)

  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))
  if(is(estimate, "try-error")) stop(estimate[1])

  estimate$raw_data$label <- as.character(estimate$raw_data$label)
  if (!is.null(self$options$moderator)) {
    estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
  }

  estimate$raw_data$reference_P <- estimate$raw_data$reference_cases / estimate$raw_data$reference_N
  estimate$raw_data$comparison_P <- estimate$raw_data$comparison_cases / estimate$raw_data$comparison_N


  if (!is(estimate, "try-error")) {
    if (length(estimate$warnings) > 0) {
      notes <- c(notes, estimate$warnings)
    }
  }

  self$results$help$setState(notes)

  return(estimate)
}
