
# This file is a generated template, your changes will not be overwritten

jamovicorrelationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovicorrelationClass",
    inherit = jamovicorrelationBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")

            # Get a handle for each table
            tbl_overview <- self$results$overview
            tbl_es_r <- self$results$es_r
            tbl_regression <- self$results$regression

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
            jamovi_set_confidence(tbl_regression, conf_level)


            width <- jamovi_sanitize(
              my_value = self$options$es_plot_width,
              return_value = 300,
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

            estimate <- jamovi_correlation(self)

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

            if (!is.null(estimate$properties$lm)) {
              estimate$es_r$syx <- summary(estimate$properties$lm)$sigma
            }
            #
            # Fill in MoE
            estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2

            # switch regression table to html
            estimate$regression$component <- gsub(
              "(a)",
              "<i>a</i>",
              estimate$regression$component
            )
            estimate$regression$component <- gsub(
              "(b)",
              "<i>b</i>",
              estimate$regression$component
            )


            # Fill tables
            jamovi_estimate_filler(self, estimate, TRUE)

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {


          # Redo analysis
          estimate <- jamovi_correlation(self)

          if(is(estimate, "try-error")) return(TRUE)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          notes <- NULL

          #Hyothesis evaluation
          interval_null <- FALSE
          htest <- FALSE
          args <- list()
          graph_call <- "plot_correlation"

          args$estimate <- estimate
          args$ggtheme <- ggtheme[[1]]
          args$error_layout <- self$options$error_layout
          # args$error_scale <- self$options$error_scale


          # Hypothesis test and rope
          try(htest <- self$options$evaluate_hypotheses)
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

            args <- jamovi_arg_builder(
              args,
              "point_null",
              my_value = self$options$null_value,
              return_value = 0,
              lower = -1,
              lower_inclusive = TRUE,
              upper = 1,
              upper_inclusive = TRUE,
              convert_to_number = TRUE,
              my_value_name = "Hypothesis Evaluation: Null value"
            )

            multiplier <- 1
            # if (self$options$rope_units == "sd") {
            #   multiplier <- estimate$es_smd[[1, "denominator"]]
            # }

            args$rope <- c(
              args$point_null - (args$null_boundary * multiplier),
              args$point_null + (args$null_boundary * multiplier)
            )

            if (args$rope[[1]] != args$rope[[2]]) {
              interval_null <- TRUE
            }

            notes <- c(
              notes,
              names(args$point_null),
              names(args$null_boundary),
              args$warnings
            )
            args$point_null <- NULL
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
            return_value = 300,
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


          # Axis breaks
          ymin <- jamovi_sanitize(
            my_value = self$options$ymin,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Minimum"
          )

          ymax <- jamovi_sanitize(
            my_value = self$options$ymax,
            return_value = NA,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Maximum"
          )

          ybreaks <- jamovi_sanitize(
            my_value = self$options$ybreaks,
            return_value = 5,
            na_ok = FALSE,
            lower = 1,
            lower_inclusive = TRUE,
            upper = 50,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Number of tick marks"
          )

          # Slab
          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "summary" = self$options$fill_error
            ),
            aesthetics = "slab_fill"
          )
          myplot <- myplot + ggplot2::discrete_scale(
            "slab_alpha",
            "slab_alpha_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$alpha_error)
            ))
          )

          if (self$options$evaluate_hypotheses) {
            myplot$layers[["null_line"]]$aes_params$colour <- self$options$null_color
            if (interval_null) {
              try(myplot$layers[["null_interval"]]$aes_params$fill <- self$options$null_color)
              try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_interval)/divider+1)
              try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_interval))
              try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_interval)
              try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$linetype_summary)

            }
          }


          myplot <- myplot + ggplot2::scale_y_continuous(
            limits = c(ymin, ymax),
            n.breaks = ybreaks,
          )


          #aesthetics
          myplot <- myplot + ggplot2::scale_shape_manual(
            values = c(
              "raw" = "circle",
              "summary" = self$options$shape_summary
            )
          )

          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "raw" = "black",
              "summary" = self$options$color_summary
            ),
            aesthetics = c("color", "point_color")
          )

          myplot <- myplot + ggplot2::scale_fill_manual(
            values = c(
              "raw" = "black",
              "summary" = self$options$fill_summary
            ),
            aesthetics = c("fill", "point_fill")
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("size", "point_size"),
            "point_size_d",
            function(n) return(c(
              "raw" = 2,
              "summary" = as.numeric(self$options$size_summary)
            ))
          )

          myplot <- myplot + ggplot2::discrete_scale(
            c("alpha", "point_alpha"),
            "point_alpha_d",
            function(n) return(c(
              "raw" = 1,
              "summary" = as.numeric(self$options$alpha_summary)
            ))
          )

          myplot <- myplot + ggplot2::scale_linetype_manual(
            values = c(
              "summary" = self$options$linetype_summary
            )
          )

          myplot <- myplot + ggplot2::scale_color_manual(
            values = c(
              "summary" = self$options$color_interval
            ),
            aesthetics = "interval_color"
          )
          myplot <- myplot + ggplot2::discrete_scale(
            "interval_alpha",
            "interval_alpha_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$alpha_interval)
            ))
          )
          myplot <- myplot + ggplot2::discrete_scale(
            "interval_size",
            "interval_size_d",
            function(n) return(c(
              "summary" = as.numeric(self$options$size_interval)
            ))
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
              names(height),
              names(ymin),
              names(ymax),
              names(ybreaks)
            )
          )
          jamovi_set_notes(self$results$estimation_plot_warnings)


          print(myplot)
          TRUE

        },
        .scatter_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_correlation(self)

          if(is(estimate, "try-error")) return(TRUE)

          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          if (is.null(estimate$raw_data)) return(TRUE)

          notes <- NULL

          graph_call <- "plot_scatter"
          args <- list()
          args$estimate <- estimate
          args$ggtheme <- ggtheme[[1]]
          args$show_line <- self$options$show_line
          args$show_line_CI <- self$options$show_line_CI
          args$show_PI <- self$options$show_PI
          args$show_residuals <- self$options$show_residuals
          args$show_mean_lines <- self$options$show_mean_lines
          args$plot_as_z <- self$options$plot_as_z
          args$show_r <- self$options$show_r

          args <- jamovi_arg_builder(
            args = args,
            arg_name = "predict_from_x",
            my_value = self$options$predict_from_x,
            na_ok = FALSE,
            return_value = 'bad',
            convert_to_number = TRUE,
            my_value_name = "<i>X</i> value for prediction"
          )

          # self$results$debug$setContent(args$predict_from_x)
          # self$results$debug$setVisible(TRUE)

          if (args$predict_from_x == "bad") {
            args$predict_from_x <- NULL
            args$warnings <- NULL
          }

          myplot <- do.call(
            what = graph_call,
            args = args
          )



          if (self$options$show_r) {
            r_value <- estimate$es_r$effect_size[[1]]

            font_size <- jamovi_sanitize(
              my_value = self$options$sp_axis.title.x,
              return_value = 15,
              na_ok = FALSE,
              convert_to_number = TRUE,
              lower = 1,
              lower_inclusive = TRUE,
              upper = 97,
              my_value_name = "Scatterplot X axis: Label font size"
            )

            new_label <- paste(
              "<span style='font-size:",
              font_size,
              "pt'>*r* = ",
              format(r_value, digits = 2),
              "</span>",
              sep = ""
            )
            if (!is.null(myplot$layers$r_label)) {
              myplot$layers$r_label$mapping$label<- new_label
            }
          }


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
            return_value = myplot$esci_xmin,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot X axis: Minimum"
          )

          xmax <- jamovi_sanitize(
            my_value = self$options$sp_xmax,
            return_value = myplot$esci_xmax,
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
            return_value = myplot$esci_ymin,
            na_ok = TRUE,
            convert_to_number = TRUE,
            my_value_name = "Scatterplot Y axis: Minimum"
          )

          ymax <- jamovi_sanitize(
            my_value = self$options$sp_ymax,
            return_value = myplot$esci_ymax,
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
            n.breaks = xbreaks,
            expand = c(0, 0)
          )

          myplot <- myplot + ggplot2::scale_y_continuous(
            limits = c(ymin, ymax),
            n.breaks = ybreaks,
            expand = c(0, 0)
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
            my_value = self$options$sp_xlab ,
            return_value = myplot$esci_xlab,
            na_ok = FALSE,
            my_value_name = "Scatterplot X axis: Title"
          )

          ylab <- jamovi_sanitize(
            my_value = self$options$sp_ylab,
            return_value = myplot$esci_ylab,
            na_ok = FALSE,
            my_value_name = "Scatterplot Y axis: Title"
          )

          if (!(self$options$sp_xlab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::xlab(xlab)
          }
          if (!(self$options$sp_ylab %in% c("auto", "Auto", "AUTO"))) {
            myplot <- myplot + ggplot2::ylab(ylab)
          }


          myplot$layers$raw_Reference_point$aes_params$fill <- self$options$sp_fill_raw_reference
          myplot$layers$raw_Reference_point$aes_params$colour <- self$options$sp_color_raw_reference
          myplot$layers$raw_Reference_point$aes_params$size <- as.numeric(self$options$sp_size_raw_reference)
          myplot$layers$raw_Reference_point$aes_params$alpha <- as.numeric(self$options$sp_alpha_raw_reference)
          myplot$layers$raw_Reference_point$aes_params$shape <- self$options$sp_shape_raw_reference
          #

          if (!is.null(myplot$layers$summary_Reference_line) & self$options$show_line) {
            myplot$layers$summary_Reference_line$aes_params$colour <- self$options$sp_color_summary_reference
            #myplot$layers$summary_Reference_line$aes_params$fill <- self$options$sp_color_summary_reference
            #myplot$layers$summary_Reference_line$aes_params$alpha <- as.numeric(self$options$sp_alpha_summary_reference)
            myplot$layers$summary_Reference_line$aes_params$linetype <- self$options$sp_linetype_summary_reference
            myplot$layers$summary_Reference_line$aes_params$size <- as.numeric(self$options$sp_size_summary_reference)/2

          }


          if (!is.null(myplot$layers$summary_Reference_line_CI) & self$options$show_line_CI) {
            #myplot$layers$summary_Reference_line$aes_params$colour <- self$options$sp_color_summary_reference
            myplot$layers$summary_Reference_line_CI$aes_params$fill <- self$options$sp_color_summary_reference
            myplot$layers$summary_Reference_line_CI$aes_params$alpha <- as.numeric(self$options$sp_alpha_summary_reference)
            #myplot$layers$summary_Reference_line$aes_params$linetype <- self$options$sp_linetype_summary_reference
            #myplot$layers$summary_Reference_line$aes_params$size <- as.numeric(self$options$sp_size_summary_reference)/2
          }

          if (!is.null(myplot$layers$residuals)) {
            myplot$layers$residuals$aes_params$colour <- self$options$sp_color_residual_reference
            myplot$layers$residuals$aes_params$alpha <- as.numeric(self$options$sp_alpha_residual_reference)
            myplot$layers$residuals$aes_params$linetype <- self$options$sp_linetype_residual_reference
            myplot$layers$residuals$aes_params$size <- as.numeric(self$options$sp_size_residual_reference)/2
          }

          if (!is.null(myplot$layers$prediction_interval_upper)) {
            myplot$layers$prediction_interval_upper$aes_params$colour <- self$options$sp_color_PI_reference
            myplot$layers$prediction_interval_upper$aes_params$alpha <- as.numeric(self$options$sp_alpha_PI_reference)
            myplot$layers$prediction_interval_upper$aes_params$linetype <- self$options$sp_linetype_PI_reference
            myplot$layers$prediction_interval_upper$aes_params$size <- as.numeric(self$options$sp_size_PI_reference)/2
          }

          if (!is.null(myplot$layers$prediction_interval_lower)) {
            myplot$layers$prediction_interval_lower$aes_params$colour <- self$options$sp_color_PI_reference
            myplot$layers$prediction_interval_lower$aes_params$alpha <- as.numeric(self$options$sp_alpha_PI_reference)
            myplot$layers$prediction_interval_lower$aes_params$linetype <- self$options$sp_linetype_PI_reference
            myplot$layers$prediction_interval_lower$aes_params$size <- as.numeric(self$options$sp_size_PI_reference)/2
          }

          if (!is.null(myplot$layers$prediction_y_label)) {
            myplot$layers$prediction_y_label$aes_params$size <- as.numeric(self$options$sp_prediction_label)
            myplot$layers$prediction_y_label$aes_params$text.colour <- self$options$sp_prediction_color
          }

          if (!is.null(myplot$layers$prediction_x_label)) {
            myplot$layers$prediction_x_label$aes_params$size <- as.numeric(self$options$sp_prediction_label)
            myplot$layers$prediction_x_label$aes_params$text.colour <- self$options$sp_prediction_color
          }

          if (!is.null(myplot$layers$prediction_prediction_interval)) {
            myplot$layers$prediction_prediction_interval$aes_params$colour <- self$options$sp_color_PI
            myplot$layers$prediction_prediction_interval$aes_params$alpha <- as.numeric(self$options$sp_alpha_PI)/15
            myplot$layers$prediction_prediction_interval$aes_params$linetype <- self$options$sp_linetype_PI
            myplot$layers$prediction_prediction_interval$aes_params$size <- as.numeric(self$options$sp_size_PI)/2
          }

          if (!is.null(myplot$layers$prediction_confidence_interval)) {
            myplot$layers$prediction_confidence_interval$aes_params$colour <- self$options$sp_color_CI
            myplot$layers$prediction_confidence_interval$aes_params$alpha <- as.numeric(self$options$sp_alpha_CI)/15
            myplot$layers$prediction_confidence_interval$aes_params$linetype <- self$options$sp_linetype_CI
            myplot$layers$prediction_confidence_interval$aes_params$size <- as.numeric(self$options$sp_size_CI)/2
          }

          if (!is.null(myplot$layers$prediction_vertical_line)) {
            myplot$layers$prediction_vertical_line$aes_params$colour <- self$options$sp_color_ref
            myplot$layers$prediction_vertical_line$aes_params$alpha <- as.numeric(self$options$sp_alpha_ref)/15
            myplot$layers$prediction_vertical_line$aes_params$linetype <- self$options$sp_linetype_ref
            myplot$layers$prediction_vertical_line$aes_params$size <- as.numeric(self$options$sp_size_ref)/2
          }

          if (!is.null(myplot$layers$prediction_horizontal_line)) {
            myplot$layers$prediction_horizontal_line$aes_params$colour <- self$options$sp_color_ref
            myplot$layers$prediction_horizontal_line$aes_params$alpha <- as.numeric(self$options$sp_alpha_ref)/15
            myplot$layers$prediction_horizontal_line$aes_params$linetype <- self$options$sp_linetype_ref
            myplot$layers$prediction_horizontal_line$aes_params$size <- as.numeric(self$options$sp_size_ref)/2
          }


          notes <- c(
            notes,
            myplot$warnings,
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


jamovi_correlation <- function(self) {
    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()


    if(from_raw) {
        if (
            is.null(self$options$x) |
            is.null(self$options$y)
        ) return(NULL)

    } else {
        args$r <- jamovi_required_numeric(
            self$options$r,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE,
            my_value_name = "<i>r</i>"
        )
        args$n <- jamovi_required_numeric(
            self$options$n,
            integer_required = TRUE,
            lower = 4,
            lower_inclusive = TRUE,
            my_value_name = "<i>N</i>"
        )


        unfilled <- NULL
        for (element in args[which(is.na(args))]) {
          unfilled <- c(unfilled, names(element))
        }


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
    call <- esci::estimate_correlation

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
    } else {
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

        for (element in args) {
            notes <- c(notes, names(element))
        }

    }

    # Do analysis, then post any notes that have emerged
    estimate <- try(do.call(what = call, args = args))

    if (!is(estimate, "try-error")) {
        evaluate_h <- self$options$evaluate_hypotheses

        if(evaluate_h) {
          point_null <- jamovi_sanitize(
            self$options$null_value,
            na_ok = FALSE,
            return_value = 0,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE,
            convert_to_number = TRUE,
          )

          rope_upper <- jamovi_sanitize(
            self$options$null_boundary,
            na_ok = FALSE,
            return_value = 0,
            lower = 0,
            upper = 1,
            upper_inclusive = TRUE,
            lower_inclusive = TRUE,
            convert_to_number = TRUE
          )

          test_results <- test_correlation(
            estimate,
            rope = c(point_null - rope_upper, point_null + rope_upper),
            output_html = TRUE
          )

          estimate$point_null <- test_results$point_null
          estimate$interval_null <- test_results$interval_null

          if (!is.null(names(rope_upper))) {
            self$results$point_null$setVisible(TRUE)
            self$results$interval_null$setVisible(FALSE)
          }

        }


        if (length(estimate$warnings) > 0) {
            notes <- c(notes, estimate$warnings)
        }
    }

    self$results$help$setState(notes)

    return(estimate)
}
