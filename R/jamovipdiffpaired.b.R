
# This file is a generated template, your changes will not be overwritten

jamovipdiffpairedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovipdiffpairedClass",
    inherit = jamovipdiffpairedBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_proportion_difference <- self$results$es_proportion_difference
            # tbl_es_phi <- self$results$es_phi
            tbl_point_null <- NULL
            tbl_interval_null <- NULL

            # Prep output -------------------------------------------
            # Set CI and MoE columns to reflect confidence level
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

            jamovi_set_confidence(tbl_overview, conf_level)
            jamovi_set_confidence(tbl_es_proportion_difference, conf_level)
            # jamovi_set_confidence(tbl_es_phi, conf_level)

            #overview rows
            overview_rows <- 4
            if (from_raw) {
                overview_rows <- 0
                if (!is.null(self$options$reference_measure)) {
                    overview_rows <- length(levels(as.factor(self$data[, self$options$reference_measure])))
                }
                if (!is.null(self$options$comparison_measure)) {
                    overview_rows <- overview_rows + length(levels(as.factor(self$data[, self$options$reference_measure])))
                }

            }

            jamovi_init_table(tbl_overview, overview_rows)

            width <- jamovi_sanitize(
              my_value = self$options$es_plot_width,
              return_value = 400,
              convert_to_number = TRUE,
              lower = 10,
              lower_inclusive = TRUE,
              upper = 3000,
              upper_inclusive = TRUE
            )
            height <- jamovi_sanitize(
              my_value = self$options$es_plot_height,
              return_value = 450,
              convert_to_number = TRUE,
              lower = 10,
              lower_inclusive = TRUE,
              upper = 4000,
              upper_inclusive = TRUE
            )

            self$results$estimation_plots$setSize(width , height)

        },
        .run = function() {

            estimate <- jamovi_pdiff_paired(self)



            # Print any notes that emerged from running the analysis
            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Fill tables

            # estimate$es_proportion_difference$effect <- paste(
            #   "<i>P</i><sub>",
            #   estimate$es_proportion_difference$case_label,
            #   "</sub>: ",
            #   estimate$es_proportion_difference$effect,
            #   sep = ""
            # )

            # Revise P symbol
            estimate$es_proportion_difference <- jamovi_peffect_html(
              estimate$es_proportion_difference
            )

            jamovi_estimate_filler(self, estimate, TRUE)

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_pdiff_paired(
            self = self
          )

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          myplot <- jamovi_plot_pdiff(
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


jamovi_pdiff_paired <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$comparison_measure) |
            is.null(self$options$reference_measure)
        ) return(NULL)

      clevels <- levels(self$data[[self$options$comparison_measure]])
      rlevels <- levels(self$data[[self$options$reference_measure]])

      if (!identical(clevels, rlevels)) {
        note <- glue::glue("
For this analysis the Reference and Comparison measures must have the exact same levels.
Instead, {self$options$comparison_measure} has {length(clevels)}
  ({paste0(clevels, collapse = '; ')})
  and {self$options$reference_measure} has {length(rlevels)}
  ({paste0(rlevels, collapse = '; ')})
    ")

        self$results$help$setState(c(notes, note))
        return(NULL)
      }

    } else {
      case_label <- jamovi_sanitize(
        self$options$case_label,
        return_value = "Affected",
        na_ok = FALSE
      )
      not_case_label <- jamovi_sanitize(
        self$options$not_case_label,
        return_value = "Not affected",
        na_ok = FALSE
      )
      comparison_measure_name <- jamovi_sanitize(
        self$options$comparison_measure_name,
        return_value = "Comparison measure",
        na_ok = FALSE
      )
      reference_measure_name <- jamovi_sanitize(
        self$options$reference_measure_name,
        return_value = "Reference measure",
        na_ok = FALSE
      )


        args$cases_consistent <- jamovi_required_numeric(
            self$options$cases_consistent,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE,
            my_value_name = paste(
              "Observations of ",
              case_label,
              " at ",
              reference_measure_name,
              " that were also ",
              case_label,
              " at ",
              comparison_measure_name,
              sep = ""
            )
        )
        args$cases_inconsistent <- jamovi_required_numeric(
            self$options$cases_inconsistent,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = TRUE,
            my_value_name = paste(
              "Observations of ",
              case_label,
              " at ",
              reference_measure_name,
              " that became ",
              not_case_label,
              " at ",
              comparison_measure_name,
              sep = ""
            )
        )
        args$not_cases_consistent <- jamovi_required_numeric(
            self$options$not_cases_consistent,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE,
            my_value_name = paste(
              "Observations of ",
              not_case_label,
              " at ",
              reference_measure_name,
              " that were also ",
              not_case_label,
              " at ",
              comparison_measure_name,
              sep = ""
            )
        )
        args$not_cases_inconsistent <- jamovi_required_numeric(
            self$options$not_cases_inconsistent,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = TRUE,
            my_value_name = paste(
              "Observations of ",
              not_case_label,
              " at ",
              reference_measure_name,
              " that became ",
              case_label,
              " at ",
              comparison_measure_name,
              sep = ""
            )
        )

        unfilled <- names(args[which(is.na(args))])

        for (element in args) {
            if (is.character(element)) {
                notes <- c(notes, element)
            }
        }

        if (length(unfilled) > 0) {
          notes <- c(
            paste(
              "For summary data, please specify ",
              length(unfilled),
              " more counts (must be numeric).",
              sep = ""
            ),
            notes
          )
        } else {
          if (args$cases_consistent < 0 | args$cases_inconsistent < 0 | args$not_cases_consistent < 0 | args$not_cases_inconsistent < 0) {
            notes <- c(
              "Error: For summary data, all counts must be >= 0",
              notes
            )
          }

          if (args$cases_consistent + args$cases_inconsistent + args$not_cases_consistent + args$not_cases_inconsistent < 3) {
            notes <- c(
              paste(
                "Error: Sum of all counts must add up to > 2",
                sep = ""
              ),
              notes
            )
          }

        }

        if (length(notes) > 0) {
            self$results$help$setState(notes)
            return(NULL)
        }


    }


    # Step 2: Get analysis properties-----------------------------
    call <- esci::estimate_pdiff_paired

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
    args$conf_level <- unname(conf_level)/100
    notes <- c(notes, names(conf_level))

    if(from_raw) {
        args$data <- self$data
        args$reference_measure <- unname(self$options$reference_measure)
        args$comparison_measure <- unname(self$options$comparison_measure)
    } else {
        args$case_label <- jamovi_sanitize(
            self$options$case_label,
            return_value = "Affected",
            na_ok = FALSE
        )
        args$not_case_label <- jamovi_sanitize(
            self$options$not_case_label,
            return_value = "Not affected",
            na_ok = FALSE
        )
        args$comparison_measure_name <- jamovi_sanitize(
            self$options$comparison_measure_name,
            return_value = "Comparison measure",
            na_ok = FALSE
        )
        args$reference_measure_name <- jamovi_sanitize(
            self$options$reference_measure_name,
            return_value = "Reference measure",
            na_ok = FALSE
        )

        for (element in args) {
            notes <- c(notes, names(element))
        }

    }

    # b <- paste(names(args), args)
    # c <- NULL
    # for (e in args) {
    #     paste(c, class(e))
    # }
    # self$results$debug$setContent(paste(b, c, collapse = ", "))
    # return(NULL)

    # Do analysis, then post any notes that have emerged
    estimate <- try(do.call(what = call, args = args))

    estimate2 <- estimate_pdiff_paired(
      cases_consistent = 60,
      cases_inconsistent = 50,
      not_cases_inconsistent = 22,
      not_cases_consistent = 68,
      case_label = "Answered True",
      not_case_label = "Answered False",
      reference_measure_name = "9th grade",
      comparison_measure_name = "12th grade",
      conf_level = 0.95
    )

    # self$results$debug$setContent(c(args, estimate, estimate2))
    # self$results$debug$setVisible(TRUE)



    if (!is(estimate, "try-error")) {

      estimate <- jamovi_add_htest_pdiff(
        self = self,
        estimate = estimate
      )

      if (length(estimate$warnings) > 0) {
          notes <- c(notes, estimate$warnings)
      }
    }

    self$results$help$setState(notes)

    return(estimate)
}
