
# This file is a generated template, your changes will not be overwritten

jamovidescribeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovidescribeClass",
    inherit = jamovidescribeBase,
    private = list(
        .init = function() {
            jamovi_set_confidence(self$results$overview, 95)

          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 500,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 2000,
            upper_inclusive = TRUE
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 400,
            convert_to_number = TRUE,
            lower = 176,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE
          )
          image <- self$results$describe_plot
          image$setSize(width, height)
          image <- self$results$describe_dotplot
          image$setSize(width, height*.66)

        },
        .run = function() {

            # First - are options needed for analysis defined?
            if (is.null(self$options$outcome_variable)) return(TRUE)

            notes <- c(NULL)


            # Yes, we're doing the analysis
            args <- list()

            args$data <- self$data
            args$outcome_variable <- self$options$outcome_variable

            if (is.null(levels(self$data[[args$outcome_variable]]))) {
              args$data[[args$outcome_variable]] <- as.numeric(args$data[[args$outcome_variable]])
            } else {
              args$data[[args$outcome_variable]] <- as.numeric(levels(self$data[[args$outcome_variable]]))[self$data[[args$outcome_variable]]]
              notes <- c(
                notes,
                paste(
                  "Converted nominal variable ",args$outcome_variable, "to numeric; be sure this makes sense."
                )
              )
            }

            #args$data[[args$outcome_variable]] <- as.numeric(args$data[[args$outcome_variable]])

            call <- esci::estimate_magnitude

            estimate <- try(do.call(what = call, args = args))

            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

            jamovi_estimate_filler(self, estimate, TRUE)

            self$results$help$setState(notes)
            jamovi_set_notes(self$results$help)

            image <- self$results$describe_plot
            image$setState("histogram")
            image <- self$results$describe_dotplot
            image$setState("dotplot")


        },
        .describe_plot = function(image, ggtheme, theme, ...) {

          # First - are options needed for analysis defined?
          if (is.null(self$options$outcome_variable)) return(TRUE)

          notes <- c(NULL)


          # Yes, we're doing the analysis
          args <- list()


          args$data <- self$data
          args$outcome_variable <- self$options$outcome_variable

          if (is.null(levels(self$data[[args$outcome_variable]]))) {
            args$data[[args$outcome_variable]] <- as.numeric(args$data[[args$outcome_variable]])
          } else {
            args$data[[args$outcome_variable]] <- as.numeric(levels(self$data[[args$outcome_variable]]))[self$data[[args$outcome_variable]]]

          }

#          args$data[[args$outcome_variable]] <- as.numeric(args$data[[args$outcome_variable]])
          call <- esci::estimate_magnitude

          estimate <- try(do.call(what = call, args = args))

          if(is.null(estimate)) return(TRUE)
          if(is(estimate, "try-error")) stop(estimate[1])


          args <- list()
          args$estimate <- estimate
          args$type <- image$state

          args$mark_mean <- self$options$mark_mean
          args$mark_median <- self$options$mark_median
          args$mark_sd <- self$options$mark_sd
          args$mark_quartiles <- self$options$mark_quartiles
          args$mark_z_lines <- self$options$mark_z_lines

          args <- jamovi_arg_builder(
            args,
            "mark_percentile",
            my_value = self$options$mark_percentile,
            return_value = 0,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 0,
            lower_inclusive = TRUE,
            upper = 100,
            upper_inclusive = TRUE,
            my_value_name = "Percentile"
          )
          if (!is.null(args$mark_percentile)) args$mark_percentile <- args$mark_percentile/100

          args <- jamovi_arg_builder(
            args,
            "histogram_bins",
            my_value = self$options$histogram_bins,
            return_value = 12,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 100,
            upper_inclusive = TRUE,
            my_value_name = "Histogram bins"
          )

            args <- jamovi_arg_builder(
              args,
              "ylim",
              my_value = self$options$ymin,
              return_value = NA,
              na_ok = TRUE,
              convert_to_number = TRUE,
              my_value_name = "Y axis: Axis minimum"
            )
            args <- jamovi_arg_builder(
              args,
              "ylim2",
              my_value = self$options$ymax,
              return_value = NA,
              na_ok = TRUE,
              convert_to_number = TRUE,
              my_value_name = "Y axis: Axis maximum"
            )

            if (!is.na(args$ylim) | !is.na(args$ylim2)) {
              args$warnings <- c(
                args$warnings,
                "Y-axis limits are applied to the histogram but have no effect on the dotplot."
              )
            }

            args$ylim <- c(args$ylim, args$ylim2)
            args$ylim2 <- NULL


            args <- jamovi_arg_builder(
              args,
              "ybreaks",
              self$options$breaks,
              return_value = 5,
              na_ok = FALSE,
              lower = 1,
              lower_inclusive = TRUE,
              upper = 50,
              upper_inclusive = TRUE,
              my_value_name = "Y axis: Number of tick marks",
              convert_to_number = TRUE
            )

          args <- jamovi_arg_builder(
            args,
            "xlim",
            my_value = self$options$xmin,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "X axis: Axis minimum"
          )
          args <- jamovi_arg_builder(
            args,
            "xlim2",
            my_value = self$options$xmax,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "X axis: Axis maximum"
          )
          args$xlim <- c(args$xlim, args$xlim2)
          args$xlim2 <- NULL

          args <- jamovi_arg_builder(
            args,
            "xbreaks",
            my_value = self$options$xbreaks,
            return_value = 5,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 50,
            upper_inclusive = TRUE,
            na_ok = FALSE,
            my_value_name = "X axis: Number of tick marks",
            convert_to_number = TRUE
          )

          args$fill_regular <- self$options$fill_regular
          args$fill_highlighted <- self$options$fill_highlighted
          args$color <- self$options$color
          # args$marker_size <- as.numeric(self$options$marker_size)

          args$ggtheme <- ggtheme[[1]]


          # Store notes from basic plot
          notes <- c(
            notes,
            args$warnings
          )
          args$warnings <- NULL

          if (image$state == "dotplot") {
            args$type <- "histogram"
            hplot <- do.call(
              what = plot_describe,
              args = args
            )
            args$type <- "dotplot"
            args$xlim <- ggplot_build(hplot)$layout$panel_params[[1]]$x.range
          }

          # Do basic plot
          myplot <- do.call(
            what = plot_describe,
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
            axis.text.y = ggtext::element_markdown(size = axis.text.y),
            axis.title.y = ggtext::element_markdown(size = axis.title.y),
            axis.text.x = ggtext::element_markdown(size = axis.text.x),
            axis.title.x = ggtext::element_markdown(size = axis.title.x)
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

          if (self$options$xlab != "auto") {
            myplot <- myplot + ggplot2::xlab(xlab)
          }
          if (self$options$ylab != "auto") {
            myplot <- myplot + ggplot2::ylab(ylab)
          }


          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 500,
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
            lower = 176,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE,
            my_value_name = "Plot height"
          )

        notes <- c(
          notes,
          names(axis.text.y),
          names(axis.title.y),
          names(axis.text.x),
          names(axis.title.x),
          names(width),
          names(height)
        )
        self$results$describe_plot_warnings$setState(notes)
        jamovi_set_notes(self$results$describe_plot_warnings)

        print(myplot)
        return(TRUE)

        })
)
