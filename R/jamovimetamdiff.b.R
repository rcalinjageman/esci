
# This file is a generated template, your changes will not be overwritten

jamovimetamdiffClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimetamdiffClass",
    inherit = jamovimetamdiffBase,
    private = list(
        .init = function() {

            jamovi_meta_initialize(
              self = self,
              has_switch = TRUE
            )

        },
        .run = function() {

            estimate <- jamovi_meta_mdiff_two(self)


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
              has_switch = TRUE,
              has_aev = TRUE
            )

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_meta_mdiff_two(self)


          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          myplot <- jamovi_meta_forest_plot(
            estimate = estimate,
            self = self,
            ggtheme = ggtheme,
            theme = theme
          )

          print(myplot)

          return(TRUE)

        })
)


jamovi_meta_mdiff_two <- function(self) {

    # Prelim -----------------------------------------------------
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()
    from_raw <- self$options$switch == "from_raw"

    if (from_raw) {
      if (
        is.null(self$options$reference_means) |
        is.null(self$options$reference_sds) |
        is.null(self$options$reference_ns) |
        is.null(self$options$comparison_means) |
        is.null(self$options$comparison_sds) |
        is.null(self$options$comparison_ns)
      ) return(NULL)
      call <- esci::meta_mdiff_two
    } else {
      if (
        is.null(self$options$d) |
        is.null(self$options$dreference_ns) |
        is.null(self$options$dcomparison_ns)
      ) return(NULL)
      call <- esci::meta_d2
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

    args$assume_equal_variance <- self$options$assume_equal_variance

    for (element in args) {
        notes <- c(notes, names(element))
    }



    if (from_raw) {
      args$data <- self$data
      args$reference_means <- self$options$reference_means
      args$reference_sds <- self$options$reference_sds
      args$reference_ns <- self$options$reference_ns
      args$comparison_means <- self$options$comparison_means
      args$comparison_sds <- self$options$comparison_sds
      args$comparison_ns <- self$options$comparison_ns
      args$reported_effect_size <- self$options$reported_effect_size

      if (!is.null(self$options$r)) {
        args$r <- self$options$r
      }


      if (!is.null(self$options$moderator)) {
        args$moderator <- self$options$moderator
      }

      if (!is.null(self$options$labels)) {
        args$labels <- self$options$labels
      }

    } else {

      args$data <- self$data
      args$ds <- self$options$d
      args$reference_ns <- self$options$dreference_ns
      args$comparison_ns <- self$options$dcomparison_ns

      if (!is.null(self$options$dr)) {
        args$r <- self$options$dr
      }


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

    if (!is(estimate, "try-error")) {
      if (length(estimate$warnings) > 0) {
        notes <- c(notes, estimate$warnings)
      }


    estimate$raw_data$label <- as.character(estimate$raw_data$label)
    estimate$raw_data$df_i <- estimate$raw_data$df

    if (from_raw) {
      if (!is.null(self$options$moderator)) {
        estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
      }

    } else {
      if (!is.null(self$options$dmoderator)) {
        estimate$raw_data$moderator <- as.character(estimate$raw_data$moderator)
      }
    }

    }

    self$results$help$setState(notes)

    return(estimate)
}
