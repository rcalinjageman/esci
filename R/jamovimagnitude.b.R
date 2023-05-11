
# This file is a generated template, your changes will not be overwritten

jamovimagnitudeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimagnitudeClass",
    inherit = jamovimagnitudeBase,
    private = list(
        .init = function() {
            from_raw <- (self$options$switch == "from_raw")
            try(tbl_overview <- self$results$overview)

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
            jamovi_set_confidence(self$results$es_smd, conf_level)


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
                return_value = 400,
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
            image <- self$results$magnitude_plot
            image$setSize(width * level_count, height)

        },
        .run = function() {

            estimate <- jamovi_magnitude(self, FALSE)


            # Print any notes that emerged from running the analysis
            jamovi_set_notes(self$results$help)

            # Check to see if the analysis ran
            #  If null, return
            #  If error, return the error
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # Fill tables
            alpha <- 1 - as.numeric(self$options$conf_level)/100
            estimate$overview$t_multiplier <- stats::qt(1-alpha/2, estimate$overview$df)
            estimate$overview$s_component <- estimate$overview$sd
            estimate$overview$n_component <- 1/sqrt(estimate$overview$n)

            # Add in MoE
            estimate$es_mean_difference$moe <- (estimate$es_mean_difference$UL - estimate$es_mean_difference$LL)/2
            estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2


            jamovi_estimate_filler(self, estimate, TRUE)

        },
        .magnitude_plot = function(image, ggtheme, theme, ...) {

            # Do analysis
            estimate <- jamovi_magnitude(self, TRUE)
            if(is.null(estimate)) return(TRUE)
            if(is(estimate, "try-error")) stop(estimate[1])

            # self$results$debug$setContent(estimate)
            # self$results$debug$setVisible(TRUE)
            # return(TRUE)

            divider <- 1
            if (self$options$effect_size == "median") divider <- 4

            # Basic plot
            notes <- NULL
            args <- list()
            args$estimate <- estimate
            args$data_layout <- self$options$data_layout
            args$effect_size = self$options$effect_size

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
                my_value_name = "Hypothesis Evaluation: Null range (+/-)"
              )
              args <- jamovi_arg_builder(
                args,
                "point_null",
                my_value = self$options$null_value,
                return_value = 0,
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
                names(args$null_boundary)
              )
              args$point_null <- NULL
              args$null_boundary <- NULL
            }

            args <- jamovi_arg_builder(
                args,
                "data_spread",
                my_value = self$options$data_spread,
                return_value = .25,
                convert_to_number = TRUE,
                lower = .01,
                lower_inclusive = TRUE,
                upper = 2,
                upper_inclusive = TRUE,
                my_value_name = "Data: Spread"
            )
            args$error_layout <- self$options$error_layout
            args <- jamovi_arg_builder(
                args,
                "error_scale",
                self$options$error_scale,
                return_value = 0.25,
                lower = 0,
                lower_inclusive = TRUE,
                upper = 5,
                upper_inclusive = TRUE,
                my_value_name = "Distributions: Width",
                convert_to_number = TRUE
            )
            args <- jamovi_arg_builder(
                args,
                "error_nudge",
                self$options$error_nudge,
                return_value = 0.4,
                lower = 0,
                lower_inclusive = TRUE,
                upper = 5,
                upper_inclusive = TRUE,
                my_value_name = "Distributions: Offset from data",
                convert_to_number = TRUE
            )
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
                what = plot_magnitude,
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
            ymin <- jamovi_sanitize(
                my_value = self$options$ymin,
                return_value = NA,
                na_ok = FALSE,
                convert_to_number = TRUE,
                my_value_name = "Y axis: Axis minimum"
            )
            ymax <- jamovi_sanitize(
                my_value = self$options$ymax,
                return_value = NA,
                na_ok = FALSE,
                convert_to_number = TRUE,
                lower = if(is.na(ymin)) NULL else ymin,
                lower_inclusive = FALSE,
                my_value_name = "Y axis: Axis maximum"
            )
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

            if (self$options$data_layout == "swarm") {
               if (is.na(ymin) & is.na(ymax)) {
                 myplot <- myplot + ggplot2::scale_y_continuous(
                   n.breaks = breaks
                 )
               }

              if (!is.na(ymin) & !is.na(ymax)) {
                myplot <- myplot + ggplot2::scale_y_continuous(
                  limits = c(ymin, ymax),
                  n.breaks = breaks
                )
              }

            } else {
              myplot <- myplot + ggplot2::scale_y_continuous(
                limits = c(ymin, ymax),
                n.breaks = breaks
              )
            }





            #aesthetics
            myplot <- myplot + ggplot2::scale_shape_manual(
                values = c(
                    "raw" = self$options$shape_raw,
                    "summary" = self$options$shape_summary
                )
            )

            myplot <- myplot + ggplot2::scale_color_manual(
                values = c(
                    "raw" = self$options$color_raw,
                    "summary" = self$options$color_summary
                ),
                aesthetics = c("color", "point_color")
            )

            myplot <- myplot + ggplot2::scale_fill_manual(
                values = c(
                    "raw" = self$options$fill_raw,
                    "summary" = self$options$fill_summary
                ),
                aesthetics = c("fill", "point_fill")
            )

            myplot <- myplot + ggplot2::discrete_scale(
                c("size", "point_size"),
                "point_size_d",
                function(n) return(c(
                    "raw" = as.numeric(self$options$size_raw),
                    "summary" = as.numeric(self$options$size_summary)/divider
                ))
            )

            myplot <- myplot + ggplot2::discrete_scale(
                c("alpha", "point_alpha"),
                "point_alpha_d",
                function(n) return(c(
                    "raw" = as.numeric(self$options$alpha_raw),
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
                    "summary" = as.numeric(self$options$size_interval)/divider
                ))
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

                if (self$options$effect_size == "median") {
                  try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_summary)
                  try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_summary)/divider*2)
                  try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_summary))
                  try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$linetype_summary)
                }

              }
            }


            notes <- c(
                notes,
                names(axis.text.y),
                names(axis.title.y),
                names(axis.text.x),
                names(axis.title.x),
                names(xlab),
                names(ylab),
                names(ymin),
                names(ymax),
                names(breaks)
            )
            self$results$magnitude_plot_warnings$setState(notes)
            jamovi_set_notes(self$results$magnitude_plot_warnings)


            print(myplot)
            TRUE
        })
)


jamovi_magnitude <- function(self, save_raw_data = FALSE) {

    # Prelim -----------------------------------------------------
    from_raw <- (self$options$switch == "from_raw")
    notes <- c(NULL)

    # Step 1 - Check if analysis basics are defined ---------------
    args <- list()

    if(from_raw) {
        if (
            is.null(self$options$outcome_variable)
        ) return(NULL)
    } else {
        args$comparison_mean <- jamovi_required_numeric(
            self$options$mean,
            my_value_name = "Mean (<i>M</i>)"
        )
        args$comparison_sd <- jamovi_required_numeric(
            self$options$sd,
            lower = 0,
            lower_inclusive = FALSE,
            my_value_name = "Standard deviation (<i>s</i>)"
        )
        args$comparison_n <- jamovi_required_numeric(
            self$options$n,
            integer_required = TRUE,
            lower = 2,
            lower_inclusive = TRUE,
            my_value_name = "Sample size (<i>N</i>)"
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
    call <- esci::estimate_mdiff_one

    args$reference_mean <- jamovi_sanitize(
      my_value = self$options$null_value,
      return_value = 0,
      convert_to_number = TRUE,
      my_value_name = "Null value"
    )

    args$save_raw_data <- save_raw_data
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
        args$outcome_variable <- unname(self$options$outcome_variable)
        for (x in 1:length(self$options$outcome_variable)) {
          args$data[[args$outcome_variable[[x]]]] <- as.numeric(args$data[[args$outcome_variable[[x]]]])
        }
    } else {
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
      estimate <- jamovi_add_htest_mdiff(
        self = self,
        estimate = estimate
      )

      estimate$es_smd$reference_value <- args$reference_mean
      estimate$es_smd$mean <- estimate$es_smd$numerator + args$reference_mean

      if (length(estimate$warnings) > 0) {
          notes <- c(notes, estimate$warnings)
      }
    }

    self$results$help$setState(notes)

    return(estimate)
}
