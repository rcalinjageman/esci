
# This file is a generated template, your changes will not be overwritten

jamovipdifftwoClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovipdifftwoClass",
    inherit = jamovipdifftwoBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_proportion_difference <- self$results$es_proportion_difference
            tbl_es_odds_ratio <- self$results$es_odds_ratio
            tbl_point_null <- NULL
            tbl_interval_null <- NULL
            tbl_es_phi <- NULL
            try(tbl_point_null <- self$results$point_null)
            try(tbl_interval_null <- self$results$interval_null)
            try(tbl_es_phi <- self$results$es_phi)

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
            jamovi_set_confidence(tbl_es_odds_ratio, conf_level)
            jamovi_set_confidence(tbl_es_phi, conf_level)


            # Outcomes: 1 if from summary, length of outcome_variables if raw
            outcome_count <- if(from_raw) {
                length(self$options$outcome_variable)
            } else {
                1
            }


            # For now, only 1 contrast can be specified
            contrast_count <- 1

            # How many levels?
            #  For raw, check grouping_variable
            #  For summary, check group_labels
            level_count <- 1
            if (from_raw & !is.null(self$options$grouping_variable)) {
                level_source <- self$options$grouping_variable
                level_count <- length(levels(as.factor(self$data[, level_source])))
            }

            #overview rows
            overview_rows <- 2 * level_count
            o <- NULL
            if (from_raw & !is.null(self$options$outcome_variable)) {
                for (myi in 1:length(self$options$outcome_variable)) {
                    current_outcome <- self$options$outcome_variable[[myi]]
                    o <- c(o, length(levels(as.factor(self$data[, current_outcome]))))
                }
                overview_rows <- sum(level_count * o)
            }

            # Rows needed for each table -------------------------------
            mdiff_rows <- contrast_count * outcome_count * 3
            smd_rows <- contrast_count * outcome_count

            jamovi_init_table(tbl_overview, overview_rows)
            jamovi_init_table(tbl_es_proportion_difference, mdiff_rows, breaks = 3)
            jamovi_init_table(tbl_es_odds_ratio, smd_rows)

            if (!is.null(tbl_point_null)) {
              jamovi_init_table(
                tbl_point_null,
                outcome_count
              )
            }
            if (!is.null(tbl_interval_null)) {
              jamovi_init_table(
                tbl_interval_null,
                outcome_count
              )
            }

            # Hide odds ratio table when more than 2 levels in the grouping variable
            tbl_es_odds_ratio$setVisible(self$options$show_ratio)


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


            keys <- if (from_raw)
              self$options$outcome_variable
            else
              jamovi_sanitize(
                self$options$outcome_variable_name,
                "My outcome variable",
                na_ok = FALSE
              )

            for (my_key in keys) {
              self$results$estimation_plots$addItem(key = my_key)
              image <- self$results$estimation_plots$get(my_key)
              image$setSize(width , height)
            }

        },
        .run = function() {
            from_raw <- (self$options$switch == "from_raw")


            estimate <- jamovi_pdiff_two(self)

            # Print any notes that emerged from running the analysis
            if (self$options$show_phi & is.null(estimate$es_phi)) {
              notes <- self$results$help$state
              notes <- c(
                notes,
                glue::glue("Correlation (<i>&#981;</i>) table not shown because analysis did not produce a 2x2 contingency table.  Instead there were {dim(estimate$properties$chi_square$observe)[[1]]} grouping variable levels and {dim(estimate$properties$chi_square$observe)[[2]]} outcome variable levels, so <i>&#981;</i> could not be calculated.")
              )
              self$results$help$setState(notes)
            }


            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Revise P symbol
            estimate$es_proportion_difference <- jamovi_peffect_html(
              estimate$es_proportion_difference
            )

            estimate$es_odds_ratio <- jamovi_peffect_html(
              estimate$es_odds_ratio
            )

            if(is.null(estimate$es_phi)) {
              self$results$es_phi$setVisible(FALSE)
            } else {
              self$results$es_phi$setVisible(self$options$show_phi)
            }


            if (is.null(estimate$properties$chi_square)) {
              self$results$contingency_table$setVisible(FALSE)
            } else {
              self$results$contingency_table$setVisible(self$options$show_chi_square)
              if (self$options$show_chi_square) {
                jamovi_contingency_table(self, estimate)
              }
            }


            # Fill tables
            jamovi_estimate_filler(self, estimate, TRUE)

            # Deal with plots
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
          estimate <- jamovi_pdiff_two(
            self = self,
            outcome_variable = c(image$state)
          )

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          if (is.null(estimate$properties$contrast)) {
            return(TRUE)
          }

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


jamovi_pdiff_two <- function(self, outcome_variable = NULL) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
      if (is.null(outcome_variable)) {
        if (
          is.null(self$options$outcome_variable) |
          length(self$options$outcome_variable) == 0
        ) return(NULL)
      }
      if (
        is.null(self$options$grouping_variable)
      ) return(NULL)
    } else {
      case_label <- jamovi_sanitize(
        self$options$case_label,
        return_value = "Affected",
        na_ok = FALSE,
        my_value_name = "Label for cases"
      )
      not_case_label <- jamovi_sanitize(
        self$options$not_case_label,
        return_value = "Not affected",
        na_ok = FALSE,
        my_value_name = "Label for not cases"
      )
      grouping_variable_level1 <- jamovi_sanitize(
        self$options$grouping_variable_level1,
        return_value = "Comparison Level",
        na_ok = FALSE,
        my_value_name = "Comparison group label"
      )
      grouping_variable_level2 <- jamovi_sanitize(
        self$options$grouping_variable_level2,
        return_value = "Reference Level",
        na_ok = FALSE,
        my_value_name = "Reference group label"
      )

      outcome_variable_name <- jamovi_sanitize(
        self$options$outcome_variable_name,
        return_value = "My outcome variable",
        na_ok = FALSE,
        my_value_name = "Outcome variable name"
      )
      grouping_variable_name <- jamovi_sanitize(
        self$options$grouping_variable_name,
        return_value = "My grouping variable",
        na_ok = FALSE,
        my_value_name = "Grouping variable name"
      )


        args$comparison_cases <- jamovi_required_numeric(
            self$options$comparison_cases,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE,
            my_value_name = paste(
              "Observations of ",
              case_label,
              " in the ",
              grouping_variable_level1,
              " group",
              sep = ""
            )
        )

        args$comparison_not_cases <- jamovi_required_numeric(
          self$options$comparison_not_cases,
          lower = 0,
          lower_inclusive = TRUE,
          integer_required = TRUE,
          my_value_name = paste(
            "Observations of ",
            not_case_label,
            " in the ",
            grouping_variable_level1,
            " group",
            sep = ""
          )
        )

        args$reference_cases <- jamovi_required_numeric(
            self$options$reference_cases,
            lower = 0,
            lower_inclusive = TRUE,
            integer_required = TRUE,
            my_value_name = paste(
              "Observations of ",
              case_label,
              " in the ",
              grouping_variable_level2,
              " group",
              sep = ""
            )
        )

        args$reference_not_cases <- jamovi_required_numeric(
          self$options$reference_not_cases,
          lower = 0,
          lower_inclusive = TRUE,
          integer_required = TRUE,
          my_value_name = paste(
            "Observations of ",
            not_case_label,
            " in the ",
            grouping_variable_level2,
            " group",
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
          if (args$comparison_cases < 0 | args$comparison_not_cases < 0 | args$reference_cases < 0 | args$reference_not_cases < 0) {
            notes <- c(
              "Error: For summary data, all counts must be >= 0",
              notes
            )
          }

          if (args$comparison_cases + args$comparison_not_cases < 3) {
            notes <- c(
              paste(
                "Error: Counts in the ",
                grouping_variable_level1,
                "group must add up to > 2",
                sep = ""
              ),
              notes
            )
          }

          if (args$reference_cases + args$reference_not_cases < 3) {
            notes <- c(
              paste(
                "Error: Counts in the ",
                grouping_variable_level2,
                "group must add up to > 2",
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

        args$comparison_n <- args$comparison_cases + args$comparison_not_cases
        args$comparison_not_cases <- NULL
        args$reference_n <- args$reference_cases + args$reference_not_cases
        args$reference_not_cases <- NULL
    }


    # Step 2: Get analysis properties-----------------------------
    call <- esci::estimate_pdiff_two

    args$count_NA <- self$options$count_NA
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

        if (is.null(outcome_variable)) {
          args$outcome_variable <- unname(self$options$outcome_variable)
        } else {
          args$outcome_variable <- unname(outcome_variable)
          args$outcome_variable_name <- outcome_variable
        }

        args$grouping_variable <- unname(self$options$grouping_variable)
    } else {
        args$case_label <- jamovi_sanitize(
            self$options$case_label,
            return_value = "Affected",
            na_ok = FALSE,
            my_value_name = "Label for cases"
        )
        args$not_case_label <- jamovi_sanitize(
            self$options$not_case_label,
            return_value = "Not affected",
            na_ok = FALSE,
            my_value_name = "Label for not cases"
        )
        args$grouping_variable_level1 <- jamovi_sanitize(
            self$options$grouping_variable_level1,
            return_value = "Comparison Level",
            na_ok = FALSE,
            my_value_name = "Comparison group label"
        )
        args$grouping_variable_level2 <- jamovi_sanitize(
            self$options$grouping_variable_level2,
            return_value = "Reference Level",
            na_ok = FALSE,
            my_value_name = "Reference group label"
        )

        args$outcome_variable_name <- jamovi_sanitize(
            self$options$outcome_variable_name,
            return_value = "My outcome variable",
            na_ok = FALSE,
            my_value_name = "Outcome variable name"
        )
        args$grouping_variable_name <- jamovi_sanitize(
            self$options$grouping_variable_name,
            return_value = "My grouping variable",
            na_ok = FALSE,
            my_value_name = "Grouping variable name"
        )

        for (element in args) {
            notes <- c(notes, names(element))
        }

        args$grouping_variable_levels <- c(
            args$grouping_variable_level2,
            args$grouping_variable_level1
        )

        args$grouping_variable_level1 <- NULL
        args$grouping_variable_level2 <- NULL

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
