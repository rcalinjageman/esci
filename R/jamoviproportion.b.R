
# This file is a generated template, your changes will not be overwritten

jamoviproportionClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamoviproportionClass",
    inherit = jamoviproportionBase,
    private = list(
        .init = function() {
          from_raw <- (self$options$switch == "from_raw")

          # Get a handle for each table
          tbl_overview <- self$results$overview

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


          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 300,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 2000,
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
          if (from_raw) {
            level_count <- jamovi_sanitize(
              my_value = length(self$options$outcome_variable),
              return_value = 1,
              convert_to_number = TRUE,
              lower = 1,
              lower_inclusive = TRUE
            )
          } else {
            level_count <- 1
          }


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
          estimate <- jamovi_proportion(self)

          # Print any notes that emerged from running the analysis
          jamovi_set_notes(self$results$help)

          # Check to see if the analysis ran
          #  If null, return
          #  If error, return the error
          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])

          # Fill tables
#
#           self$results$debug$setVisible(TRUE)
#           self$results$debug$setContent(estimate$point_null)

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


          # Do analysis
          estimate <- jamovi_proportion(
            self,
            outcome_variable = c(image$state)
          )
          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])

          # self$results$debug$setContent(estimate)
          # self$results$debug$setVisible(TRUE)
          # return(TRUE)

          # Basic plot
          divider <- 4
          notes <- NULL
          args <- list()
          args$estimate <- estimate

          args$plot_possible <- self$options$plot_possible

          #Hyothesis evaluation
          interval_null <- FALSE
          if (self$options$evaluate_hypotheses) {
            args <- jamovi_arg_builder(
              args,
              "null_boundary",
              my_value = self$options$null_boundary,
              return_value = 0,
              convert_to_number = TRUE,
              lower = 0,
              lower_inclusive = TRUE,
              upper = 1,
              upper_inclusive = TRUE,
              my_value_name = "Hypothesis Evaluation: <i>H</i><sub>0</sub> boundary (+/-)"
            )
            args <- jamovi_arg_builder(
              args,
              "point_null",
              my_value = self$options$null_value,
              return_value = 0,
              lower = 0,
              lower_inclusive = TRUE,
              upper = 1,
              upper_inclusive = TRUE,
              convert_to_number = TRUE,
              my_value_name = "Hypothesis Evaluation: <i>H</i><sub>0</sub> value"
            )

            multiplier <- 1

            args$rope <- c(
              args$point_null - (args$null_boundary * multiplier),
              args$point_null + (args$null_boundary * multiplier)
            )

            if (args$rope[[1]] != args$rope[[2]]) {
              interval_null <- TRUE
            }

            # notes <- c(
            #   notes,
            #   names(args$point_null),
            #   names(args$null_boundary)
            # )
            args$warnings <- NULL
            args$point_null <- NULL
            args$null_boundary <- NULL
          }

          args$ggtheme <- ggtheme[[1]]

          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 300,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 2000,
            upper_inclusive = TRUE,
            my_value_name = "Plot width"
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 400,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE,
            my_value_name = "Plot height"
          )


          # Store notes from basic plot
          notes <- c(
            notes,
            args$warnings,
            names(width),
            names(height)
          )
          args$warnings <- NULL

          # Do basic plot
          myplot <- do.call(
            what = plot_proportion,
            args = args
          )


          # Basic graph options --------------------
          # Axis font sizes
          axis.text.y <- jamovi_sanitize(
            my_value = self$options$axis.text.y,
            return_value = 14,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Y axis: Tick font size"
          )
          axis.title.y <- jamovi_sanitize(
            my_value = self$options$axis.title.y,
            return_value = 15,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Y axis: Label font size"
          )
          axis.text.x <- jamovi_sanitize(
            my_value = self$options$axis.text.x,
            return_value = 14,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "X axis: Tick font size"
          )
          axis.title.x <- jamovi_sanitize(
            my_value = self$options$axis.title.x,
            return_value = 15,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "X axis: Label font size"
          )


          myplot <- myplot + ggplot2::theme(
            axis.text.y = element_text(size = axis.text.y),
            axis.title.y = element_text(size = axis.title.y),
            axis.text.x = element_text(size = axis.text.x),
            axis.title.x = element_text(size = axis.title.x)
          )


          # Axis labels
          xlab <- jamovi_sanitize(
            my_value = self$options$xlab,
            return_value = NULL,
            na_ok = FALSE,
            my_value_name = "X axis: Title"
          )

          ylab <- jamovi_sanitize(
            my_value = self$options$ylab,
            return_value = NULL,
            na_ok = FALSE,
            my_value_name = "Y axis: Title"
          )


          if (!(self$options$xlab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::xlab(xlab)
          }
          if (!(self$options$ylab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::ylab(ylab)
          }


          # Axis breaks
          breaks <- jamovi_sanitize(
            my_value = self$options$breaks,
            return_value = 8,
            na_ok = FALSE,
            lower = 2,
            lower_inclusive = TRUE,
            upper = 200,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Y axis: Number of tick marks"
          )

          myplot <- myplot + ggplot2::scale_y_continuous(
            limits = ggplot2::layer_scales(myplot)$y$limits,
            n.breaks = breaks
          )


          #aesthetics
          myplot <- myplot + ggplot2::scale_shape_manual(
            values = c(
              "summary" = self$options$shape_summary
            )
          )

          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "summary" = self$options$color_summary
            ),
            aesthetics = c("color", "point_color")
          )

          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "summary" = self$options$fill_summary
            ),
            aesthetics = c("fill", "point_fill")
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("size", "point_size"),
            "point_size_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$size_summary)/divider
            ))
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("alpha", "point_alpha"),
            "point_alpha_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$alpha_summary)
            ))
          )

          myplot <- myplot + ggplot2::scale_linetype_manual(
            values = c(
              "summary" = self$options$linetype_summary
            )
          )


          if (self$options$evaluate_hypotheses) {
            myplot$layers[["null_line"]]$aes_params$colour <- self$options$null_color
            if (interval_null) {
              try(myplot$layers[["null_interval"]]$aes_params$fill <- self$options$null_color)
              try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_summary)/divider+1)
              try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_summary))
              try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_summary)
              try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$linetype_summary)
            }
          }


          rope_upper <- jamovi_sanitize(
            self$options$null_boundary,
            na_ok = FALSE,
            return_value = 0,
            lower = 0,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Hypothesis Evaluation: Null range (+/-)"
          )

          null_value <- jamovi_sanitize(
            self$options$null_value,
            na_ok = FALSE,
            return_value = 0,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Hypothesis Evaluation: Null value"
          )



          notes <- c(
            notes,
            names(axis.text.y),
            names(axis.title.y),
            names(axis.text.x),
            names(axis.title.x),
            names(xlab),
            names(ylab),
            names(breaks),
            names(rope_upper),
            names(null_value)
          )
          self$results$magnitude_plot_warnings$setState(notes)
          jamovi_set_notes(self$results$magnitude_plot_warnings)


          print(myplot)
          TRUE
        })
)

jamovi_proportion <- function(self, outcome_variable = NULL) {
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
  } else {

    case_label <- jamovi_sanitize(
      self$options$case_label,
      return_value = "Affected",
      na_ok = FALSE
    )

    args$comparison_cases <- jamovi_required_numeric(
      self$options$cases,
      lower = 0,
      lower_inclusive = TRUE,
      integer_required = TRUE,
      my_value_name = paste(
        "Observations of ",
        case_label,
        sep = ""
      )
    )

    args$not_cases <- jamovi_required_numeric(
      self$options$not_cases,
      lower = 0,
      lower_inclusive = TRUE,
      integer_required = TRUE,
      my_value_name = paste(
        "Observations of Not ",
        case_label,
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

      if (args$comparison_cases < 0) {
        notes <- c(
          paste(
            "Observations of ",
            case_label,
            " must be >= 0",
            sep = ""
          ),
          notes
        )
      }

      if (args$not_cases < 0) {
        notes <- c(
          paste(
            "Observations of Not ",
            case_label,
            " must be >= 0",
            sep = ""
          ),
          notes
        )
      }

      if (args$comparison_cases + args$not_cases < 3) {
        notes <- c(
          "Error: For summary data, total observations must add up to > 2",
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
  call <- esci::estimate_pdiff_one

  args$reference_p <- jamovi_sanitize(
    my_value = self$options$null_value,
    return_value = 0,
    convert_to_number = TRUE,
    lower = 0,
    lower_inclusive = TRUE,
    upper = 1,
    upper_inclusive = TRUE,
    my_value_name = "Hypothesis Evaluation: Null value"
  )
  #notes <- c(notes, names(args$reference_p))

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

  } else {
    args$comparison_n <- args$comparison_cases + args$not_cases
    args$not_cases <- NULL

    args$case_label <- jamovi_sanitize(
      self$options$case_label,
      return_value = "Affected",
      na_ok = FALSE
    )
    args$outcome_variable_name <- jamovi_sanitize(
      self$options$outcome_variable_name,
      return_value = "My outcome variable",
      na_ok = FALSE
    )
    for (element in args) {
      notes <- c(notes, names(element))
    }
  }



  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  if (!is(estimate, "try-error")) {
    estimate <- jamovi_add_htest_pdiff(
      self = self,
      estimate = estimate
    )


    # self$results$debug$setVisible(TRUE)
    # self$results$debug$setContent(estimate$point_null)

    if (length(estimate$warnings) > 0) {

      notes <- c(notes, estimate$warnings)

    }
  }

  self$results$help$setState(notes)

  return(estimate)
}
