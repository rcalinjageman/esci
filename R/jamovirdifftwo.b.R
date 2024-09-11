
# This file is a generated template, your changes will not be overwritten

jamovirdifftwoClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovirdifftwoClass",
    inherit = jamovirdifftwoBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_r <- self$results$es_r
            tbl_es_r_difference <- self$results$es_r_difference

            tbl_overview$setVisible(from_raw)

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
            jamovi_set_confidence(tbl_es_r, conf_level)
            jamovi_set_confidence(tbl_es_r_difference, conf_level)


            width <- jamovi_sanitize(
              my_value = self$options$es_plot_width,
              return_value = 600,
              convert_to_number = TRUE,
              lower = 10,
              lower_inclusive = TRUE,
              upper = 3000,
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


            image <- self$results$estimation_plots
            image$setSize(width, height)


            width <- jamovi_sanitize(
              my_value = self$options$sp_plot_width,
              return_value = 650,
              convert_to_number = TRUE,
              lower = 10,
              lower_inclusive = TRUE,
              upper = 3000,
              upper_inclusive = TRUE
            )
            height <- jamovi_sanitize(
              my_value = self$options$sp_plot_height,
              return_value = 650,
              convert_to_number = TRUE,
              lower = 10,
              lower_inclusive = TRUE,
              upper = 4000,
              upper_inclusive = TRUE
            )

            image <- self$results$scatter_plots
            image$setSize(width, height)


        },
        .run = function() {

            estimate <- jamovi_rdiff_two(self)

            # Print any notes that emerged from running the analysis
            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) {
              stop(estimate[1])
              return(TRUE)
            }

            # Fill in MoE
            estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

            # Fill tables
            jamovi_estimate_filler(self, estimate, TRUE)

#
#             self$results$debug$setContent(estimate)
#             self$results$debug$setVisible(TRUE)

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {


          # Redo analysis
          estimate <- jamovi_rdiff_two(self)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          notes <- NULL

          #Hyothesis evaluation
          interval_null <- FALSE
          htest <- FALSE
          args <- list()
          graph_call <- "plot_rdiff"

          args$estimate <- estimate
          args$ggtheme <- ggtheme[[1]]

          # Axis breaks
          args <- jamovi_arg_builder(
            args,
            "difference_axis_breaks",
            self$options$difference_axis_breaks,
            return_value = 5,
            lower = 2,
            lower_inclusive = TRUE,
            upper = 50,
            upper_inclusive = TRUE,
            my_value_name = "Difference axis: Number of breaks",
            convert_to_number = TRUE
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
            lower = if(is.na(args$ylim)) NULL else args$ylim,
            lower_inclusive = FALSE,
            convert_to_number = TRUE,
            my_value_name = "Y axis: Axis maximum"
          )

          args$ylim <- c(args$ylim, args$ylim2)
          args$ylim2 <- NULL

          args <- jamovi_arg_builder(
            args,
            "ybreaks",
            self$options$ybreaks,
            return_value = 8,
            lower = 2,
            lower_inclusive = TRUE,
            upper = 50,
            upper_inclusive = TRUE,
            my_value_name = "Y axis: Number of tick marks",
            convert_to_number = TRUE
          )

          # Hypothesis test and rope
          try(htest <- self$options$evaluate_hypotheses, silent = TRUE)
          if (htest) {
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
              my_value_name = "Hypothesis Evaluation: Null range (+/-)"
            )

            args$rope <- c(
              0 - args$null_boundary,
              0 + args$null_boundary
            )

            if (args$rope[[1]] != args$rope[[2]]) {
              interval_null <- TRUE
            }

            notes <- c(
              notes,
              names(args$null_boundary),
              args$warnings
            )
            args$null_boundary <- NULL
            args$warnings <- NULL
          }


          notes <- c(
            notes,
            args$warnings
          )
          args$warnings <- NULL


          myplot <- do.call(
            what = graph_call,
            args = args
          )


          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 600,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 3000,
            upper_inclusive = TRUE,
            my_value_name = "Estimation plot width"
          )
          height <- jamovi_sanitize(
            my_value = self$options$es_plot_height,
            return_value = 400,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE,
            my_value_name = "Estimation plot height"
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

          if (!(self$options$xlab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::xlab(xlab)
          }
          if (!(self$options$ylab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::ylab(ylab)
          }



          shape_summary_unused <- "circle"
          color_summary_unused <- "black"
          fill_summary_unused <- "black"
          size_summary_unused <- 1
          alpha_summary_unused <- 1
          alpha_error_reference <- 1
          linetype_summary_unused <- "solid"
          linetype_summary_reference <- "solid"
          color_interval_unused <- "black"
          color_interval_reference <- "black"
          alpha_interval_unused <- 1
          alpha_interval_reference <- 1
          size_interval_unused <- 1
          size_interval_reference <- 1
          fill_error_unused <- "black"
          fill_error_reference <- "black"
          alpha_error_unused <- 1
          #
          #
          try(shape_summary_unused <- self$options$shape_summary_unused, silent = TRUE)
          try(color_summary_unused <- self$options$color_summary_unused, silent = TRUE)
          try(fill_summary_unused <- self$options$fill_summary_unused, silent = TRUE)
          try(size_summary_unused <- as.integer(self$options$size_summary_unused), silent = TRUE)
          try(alpha_summary_unused <- as.numeric(self$options$alpha_summary_unused), silent = TRUE)
          try(linetype_summary_unused <- self$options$linetype_summary_unused, silent = TRUE)
          try(linetype_summary_reference <- self$options$linetype_summary_reference, silent = TRUE)
          try(color_interval_unused <- self$options$color_interval_unused, silent = TRUE)
          try(color_interval_reference <- self$options$color_interval_reference, silent = TRUE)
          try(alpha_interval_unusued <- as.numeric(self$options$alpha_interval_unused), silent = TRUE)
          try(alpha_interval_reference <- as.numeric(self$options$alpha_interval_reference), silent = TRUE)
          try(size_interval_unused <- as.integer(self$options$size_interval_unused), silent = TRUE)
          try(size_interval_reference <- as.integer(self$options$size_interval_reference), silent = TRUE)
          try(fill_error_unused <- self$options$fill_error_unused, silent = TRUE)
          try(fill_error_reference <- self$options$fill_error_reference, silent = TRUE)
          try(alpha_error_reference <- self$options$alpha_error_reference, silent = TRUE)
          try(alpha_error_unused <- as.numeric(self$options$alpha_error_unused), silent = TRUE)
          #
          #
          # Aesthetics
          myplot <- myplot + ggplot2::scale_shape_manual(
            values = c(
              "Reference_summary" = self$options$shape_summary_reference,
              "Comparison_summary" = self$options$shape_summary_comparison,
              "Difference_summary" = self$options$shape_summary_difference,
              "Unused_summary" = shape_summary_unused
            )
          )
          #
          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "Reference_summary" = self$options$color_summary_reference,
              "Comparison_summary" = self$options$color_summary_comparison,
              "Difference_summary" = self$options$color_summary_difference,
              "Unused_summary" = color_summary_unused
            ),
            aesthetics = c("color", "point_color")
          )
          #
          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "Reference_summary" = self$options$fill_summary_reference,
              "Comparison_summary" = self$options$fill_summary_comparison,
              "Difference_summary" = self$options$fill_summary_difference,
              "Unused_summary" = fill_summary_unused
            ),
            aesthetics = c("fill", "point_fill")
          )
          #
          divider <- 4
          #
          myplot <- myplot + ggplot2::discrete_scale(
            c("size", "point_size"),
            "point_size_d",
            function(n) return(c(
              "Reference_summary" = as.integer(self$options$size_summary_reference)/divider,
              "Comparison_summary" = as.integer(self$options$size_summary_comparison)/divider,
              "Difference_summary" = as.integer(self$options$size_summary_difference)/divider,
              "Unused_summary" = size_summary_unused/divider
            ))
          )
          #
          myplot <- myplot + ggplot2::discrete_scale(
            c("alpha", "point_alpha"),
            "point_alpha_d",
            function(n) return(c(
              "Reference_summary" = as.numeric(self$options$alpha_summary_reference),
              "Comparison_summary" = as.numeric(self$options$alpha_summary_comparison),
              "Difference_summary" = as.numeric(self$options$alpha_summary_difference),
              "Unused_summary" = alpha_summary_unused
            ))
          )
          #
          # Error bars
          myplot <- myplot + ggplot2::scale_linetype_manual(
            values = c(
              "Reference_summary" = linetype_summary_reference,
              "Comparison_summary" = self$options$linetype_summary_comparison,
              "Difference_summary" = self$options$linetype_summary_difference,
              "Unused_summary" = linetype_summary_unused
            )
          )



          self$results$estimation_plot_warnings$setState(
            c(
              notes,
              names(xlab),
              names(ylab),
              names(axis.text.y),
              names(axis.title.y),
              names(axis.text.x),
              names(axis.title.x),
              names(width),
              names(height)
            )
          )
          jamovi_set_notes(self$results$estimation_plot_warnings)


          print(myplot)
          TRUE

        },
        .scatter_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_rdiff_two(self)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          if (is.null(estimate$raw_data)) return(TRUE)

          notes <- NULL

          myplot <- plot_scatter(
            estimate,
            show_line = self$options$show_line,
            show_line_CI = self$options$show_line_CI,
            ggtheme = ggtheme[[1]]
          )

          width <- jamovi_sanitize(
            my_value = self$options$sp_plot_width,
            return_value = 650,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 3000,
            upper_inclusive = TRUE,
            my_value_name = "Scatterplot width"
          )
          height <- jamovi_sanitize(
            my_value = self$options$sp_plot_height,
            return_value = 650,
            convert_to_number = TRUE,
            lower = 10,
            lower_inclusive = TRUE,
            upper = 4000,
            upper_inclusive = TRUE,
            my_value_name = "Scatterplot height"
          )

          # X and y limits
          xmin <- jamovi_sanitize(
            my_value = self$options$sp_xmin,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot X axis: Minimum"
          )

          xmax <- jamovi_sanitize(
            my_value = self$options$sp_xmax,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot X axis: Maximum"
          )

          xbreaks <- jamovi_sanitize(
            my_value = self$options$sp_xbreaks,
            return_value = 5,
            na_ok = FALSE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 50,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot X axis: Number of tick marks"
          )

          ymin <- jamovi_sanitize(
            my_value = self$options$sp_ymin,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Minimum"
          )

          ymax <- jamovi_sanitize(
            my_value = self$options$sp_ymax,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Maximum"
          )

          ybreaks <- jamovi_sanitize(
            my_value = self$options$sp_ybreaks,
            return_value = 5,
            na_ok = FALSE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 50,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Number of tick marks"
          )

          # Apply axis labels and scales
          myplot <- myplot + ggplot2::scale_x_continuous(
            limits = c(xmin, xmax),
            n.breaks = xbreaks
          )

          myplot <- myplot + ggplot2::scale_y_continuous(
            limits = c(ymin, ymax),
            n.breaks = ybreaks
          )

          # Axis font sizes
          axis.text.y <- jamovi_sanitize(
            my_value = self$options$sp_axis.text.y,
            return_value = 14,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Scatterplot Y axis: Tick font size"
          )
          axis.title.y <- jamovi_sanitize(
            my_value = self$options$sp_axis.title.y,
            return_value = 15,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Scatterplot Y axis: Label font size"
          )
          axis.text.x <- jamovi_sanitize(
            my_value = self$options$sp_axis.text.x,
            return_value = 14,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Scatterplot X axis: Tick font size"
          )
          axis.title.x <- jamovi_sanitize(
            my_value = self$options$sp_axis.title.x,
            return_value = 15,
            na_ok = FALSE,
            convert_to_number = TRUE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 97,
            my_value_name = "Scatterplot X axis: Label font size"
          )

          myplot <- myplot + ggplot2::theme(
            axis.text.y = ggtext::element_markdown(size = axis.text.y),
            axis.title.y = ggtext::element_markdown(size = axis.title.y),
            axis.text.x = ggtext::element_markdown(size = axis.text.x),
            axis.title.x = ggtext::element_markdown(size = axis.title.x),
            legend.title = ggtext::element_markdown(),
            legend.text = ggtext::element_markdown()
          )

          # Axis labels
          xlab <- jamovi_sanitize(
            my_value = self$options$sp_xlab,
            return_value = NULL,
            na_ok = FALSE,
            my_value_name = "Scatterplot X axis: Title"
          )

          ylab <- jamovi_sanitize(
            my_value = self$options$sp_ylab,
            return_value = NULL,
            na_ok = FALSE,
            my_value_name = "Scatterplot Y axis: Title"
          )

          if (!(self$options$sp_xlab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::xlab(xlab)
          }
          if (!(self$options$sp_ylab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::ylab(ylab)
          }


          # Aesthetics
          my_fills <- c(
            "Reference" =  self$options$sp_fill_raw_reference,
            "Comparison" = self$options$sp_fill_raw_comparison,
            "Unused" = self$options$sp_fill_raw_unused
          )
          my_shapes <- c(
            "Reference" =  self$options$sp_shape_raw_reference,
            "Comparison" = self$options$sp_shape_raw_comparison,
            "Unused" = self$options$sp_shape_raw_unused
          )
          my_colour <- c(
            "Reference" =  self$options$sp_color_raw_reference,
            "Comparison" = self$options$sp_color_raw_comparison,
            "Unused" = self$options$sp_color_raw_unused
          )
          my_alphas <- c(
            "Reference" =  as.numeric(self$options$sp_alpha_raw_reference),
            "Comparison" = as.numeric(self$options$sp_alpha_raw_comparison),
            "Unused" = as.numeric(self$options$sp_alpha_raw_unused)
          )
          my_sizes <- c(
            "Reference" =  as.numeric(self$options$sp_size_raw_reference),
            "Comparison" = as.numeric(self$options$sp_size_raw_comparison),
            "Unused" = as.numeric(self$options$sp_size_raw_unused)
          )

          my_labels <- myplot$esci_scale_labels
          scale_title <- estimate$es_r$grouping_variable_name[[1]]

          myplot <- myplot + ggplot2::scale_color_manual(values = my_colour, labels = my_labels, name = scale_title)
          myplot <- myplot + ggplot2::scale_fill_manual(values = my_fills, labels = my_labels, name = scale_title)
          myplot <- myplot + ggplot2::scale_shape_manual(values = my_shapes, labels = my_labels, name = scale_title)
          myplot <- myplot + ggplot2::scale_alpha_manual(values = my_alphas, labels = my_labels, name = scale_title)
          myplot <- myplot + ggplot2::scale_size_manual(values = my_sizes, labels = my_labels, name = scale_title)

          if (!is.null(myplot$layers$summary_Reference_line)) {
            myplot$layers$summary_Reference_line$aes_params$colour <- self$options$sp_color_summary_reference
            #myplot$layers$summary_Reference_line$aes_params$fill <- self$options$sp_color_summary_reference
            #myplot$layers$summary_Reference_line$aes_params$alpha <- as.numeric(self$options$sp_alpha_summary_reference)
            myplot$layers$summary_Reference_line$aes_params$linetype <- self$options$sp_linetype_summary_reference
            myplot$layers$summary_Reference_line$aes_params$size <- as.numeric(self$options$sp_size_summary_reference)/2
          }

          if (!is.null(myplot$layers$summary_Comparison_line)) {
            myplot$layers$summary_Comparison_line$aes_params$colour <- self$options$sp_color_summary_comparison
            #myplot$layers$summary_Comparison_line$aes_params$fill <- self$options$sp_color_summary_comparison
            #myplot$layers$summary_Comparison_line$aes_params$alpha <- as.numeric(self$options$sp_alpha_summary_comparison)
            myplot$layers$summary_Comparison_line$aes_params$linetype <- self$options$sp_linetype_summary_comparison
            myplot$layers$summary_Comparison_line$aes_params$size <- as.numeric(self$options$sp_size_summary_comparison)/2

          }


          if (!is.null(myplot$layers$summary_Reference_line_CI)) {
            #
            myplot$layers$summary_Reference_line_CI$aes_params$fill <- self$options$sp_color_summary_reference
            myplot$layers$summary_Reference_line_CI$aes_params$alpha <- as.numeric(self$options$sp_alpha_summary_reference)

            if (!self$options$show_line) {
              myplot$layers$summary_Reference_line$aes_params$colour <- NA
              myplot$layers$summary_Comparison_line$aes_params$colour <- NA
            }

           # myplot$layers$summary_Reference_line$aes_params$linetype <- self$options$sp_linetype_summary_reference
            #myplot$layers$summary_Reference_line$aes_params$size <- as.numeric(self$options$sp_size_summary_reference)/2
          }

          if (!is.null(myplot$layers$summary_Comparison_line_CI)) {
            #myplot$layers$summary_Comparison_line$aes_params$colour <- self$options$sp_color_summary_comparison
            myplot$layers$summary_Comparison_line_CI$aes_params$fill <- self$options$sp_color_summary_comparison
            myplot$layers$summary_Comparison_line_CI$aes_params$alpha <- as.numeric(self$options$sp_alpha_summary_comparison)

            if (!self$options$show_line) {
              myplot$layers$summary_Comparison_line$aes_params$colour <- NA
            }

            #myplot$layers$summary_Comparison_line$aes_params$linetype <- self$options$sp_linetype_summary_comparison
            #myplot$layers$summary_Comparison_line$aes_params$size <- as.numeric(self$options$sp_size_summary_comparison)/2

          }




          notes <- c(
            notes,
            names(width),
            names(height),
            names(axis.text.y),
            names(axis.text.x),
            names(axis.title.x),
            names(axis.title.y),
            names(xlab),
            names(xmin),
            names(xmax),
            names(xbreaks),
            names(ylab),
            names(ymin),
            names(ymax),
            names(ybreaks)
          )

          self$results$scatter_plot_warnings$setState(
            c(
              notes
            )
          )
          jamovi_set_notes(self$results$scatter_plot_warnings)

          print(myplot)
          TRUE

        })
)


jamovi_rdiff_two <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$x) |
            is.null(self$options$y) |
            is.null(self$options$grouping_variable)
        ) return(NULL)

    } else {
        args$reference_r <- jamovi_required_numeric(
          self$options$reference_r,
          lower = -1,
          lower_inclusive = TRUE,
          upper = 1,
          upper_inclusive = TRUE,
          my_value_name = "Reference <i>r</i>"
        )
        args$reference_n <- jamovi_required_numeric(
          self$options$reference_n,
          integer_required = TRUE,
          lower = 0,
          lower_inclusive = FALSE,
          my_value_name = "Reference <i>n</i>"
        )

        args$comparison_r <- jamovi_required_numeric(
            self$options$comparison_r,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE,
            my_value_name = "Comparison <i>r</i>"
        )
        args$comparison_n <- jamovi_required_numeric(
            self$options$comparison_n,
            integer_required = TRUE,
            lower = 0,
            lower_inclusive = FALSE,
            my_value_name = "Comparison <i>n</i>"
        )


        unfilled <- NULL
        for (element in args[which(is.na(args))]) {
          unfilled <- c(unfilled, names(element))
        }

#        unfilled <- names(args[which(is.na(args))])

        for (element in args) {
            if (is.character(element)) {
                notes <- c(notes, element)
            }
        }

        if (length(unfilled) > 0) {
            notes <- c(
                paste(
                    "For summary data, please specify: ",
                    paste0(unfilled, collapse = ", ")
                ),
                notes
            )
        }

        if (length(notes) > 0) {
            self$results$help$setState(notes)
            return(NULL)
        }


    }


    # Step 2: Get analysis properties-----------------------------
    call <- esci::estimate_rdiff_two

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
        args$x <- unname(self$options$x)
        args$y <- unname(self$options$y)
        args$grouping_variable <- unname(self$options$grouping_variable)
    } else {
        args$comparison_level_name <- jamovi_sanitize(
            self$options$comparison_level_name,
            return_value = "Comparison level",
            na_ok = FALSE
        )
        args$reference_level_name <- jamovi_sanitize(
            self$options$reference_level_name,
            return_value = "Reference level",
            na_ok = FALSE
        )
        args$x_variable_name <- jamovi_sanitize(
            self$options$x_variable_name,
            return_value = "X variable",
            na_ok = FALSE
        )
        args$y_variable_name <- jamovi_sanitize(
            self$options$y_variable_name,
            return_value = "Y variable",
            na_ok = FALSE
        )
        args$grouping_variable_name <- jamovi_sanitize(
            self$options$grouping_variable_name,
            return_value = "Grouping variable",
            na_ok = FALSE
        )


        for (element in args) {
            notes <- c(notes, names(element))
        }

        args$grouping_variable_levels <- c(
            args$reference_level_name,
            args$comparison_level_name
        )

        args$comparison_level_name <- NULL
        args$reference_level_name <- NULL

    }



    # Do analysis, then post any notes that have emerged
    estimate <- try(do.call(what = call, args = args))

    if (!is(estimate, "try-error")) {

      estimate <- jamovi_add_htest_rdiff(
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
