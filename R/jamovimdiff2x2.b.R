
# This file is a generated template, your changes will not be overwritten

jamovimdiff2x2Class <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiff2x2Class",
    inherit = jamovimdiff2x2Base,
    private = list(
      .init = function() {
        # Set some variables for convenience -----------------------
        #   Is analysis from summary data or raw?
        #   Are we evaluating a hypothesis?
        #   Is this a contrast?
        from_raw <- (self$options$switch == "from_raw")
        mixed <- (self$options$design == "mixed")

        # Get a handle for each table
        tbl_overview <- NULL
        tbl_es_mean_difference <- NULL
        tbl_es_smd <- NULL
        tbl_es_median_difference <- NULL
        tbl_hypothesis_evaluations <- NULL
        # tbl_htest_summary <- NULL
        assume_equal_variance <- NULL
        try(tbl_overview <- self$results$overview)
        try(tbl_es_mean_difference <- self$results$es_mean_difference)
        try(tbl_es_smd <- self$results$es_smd)
        try(tbl_es_median_difference <- self$results$es_median_difference)
        try(tbl_es_median_ratio <- self$results$es_median_ratio)
        try(assume_equal_variance <- self$options$assume_equal_variance)
        try(tbl_point_null <- self$results$point_null)
        try(tbl_interval_null <- self$results$interval_null)
        try(effect_size <- self$options$effect_size)

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


        if (!is.null(tbl_es_mean_difference) & !is.null(assume_equal_variance) & !mixed) {
          if (assume_equal_variance) {
            tbl_es_mean_difference$setNote(
              key = "overview_table",
              note = "Variances are assumed equal, so <i>s</i><sub>p</sub> was used to calculate each CI.",
              init = FALSE
            )
          } else {
            tbl_es_mean_difference$setNote(
              key = "overview_table",
              note = "Variances are not assumed equal, so the Welch method was used to calculate each CI on a difference.",
              init = FALSE
            )

          }
        } else {
          tbl_es_mean_difference$setNote(
            key = "es_mean_difference",
            note = NULL,
            init = TRUE
          )
        }


        jamovi_set_confidence(tbl_overview, conf_level)
        jamovi_set_confidence(tbl_es_mean_difference, conf_level)
        jamovi_set_confidence(tbl_es_smd, conf_level)
        jamovi_set_confidence(tbl_es_median_difference, conf_level)

        jamovi_init_table(tbl_overview, 4)
        jamovi_init_table(tbl_es_mean_difference, 15, breaks = 3)
        jamovi_init_table(tbl_es_smd, 5)
        jamovi_init_table(tbl_es_median_difference, 15, breaks = 3)
        jamovi_init_table(tbl_point_null, 1)
        jamovi_init_table(tbl_interval_null, 1)

        width <- jamovi_sanitize(
          my_value = self$options$es_plot_width,
          return_value = 700,
          convert_to_number = TRUE,
          lower = 10,
          lower_inclusive = TRUE,
          upper = 3000,
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

        image <- self$results$main_effect_A
        image$setState("Main Effect of A")
        image$setSize(width, height)

        image <- self$results$main_effect_B
        image$setState("Main Effect of B")
        image$setSize(width, height)

        image <- self$results$interaction
        image$setState("Interaction")
        image$setSize(width, height)

        image <- self$results$interaction_plot
        image$setSize(width, height)

        if (mixed) {
          self$results$setTitle(
            "Means and Medians: Mixed 2x2 Factorial (RCT)"
          )
          self$results$analysis_type$setContent(
            '
            <div class="jmv-editable-header had-focus" level="1">
            <h2 contenteditable="" spellcheck="false">
            Mixed Factorial (RCT)
            </h2>
            </div>
            '
          )
        } else {
          self$results$setTitle(
            'Means and Medians: Between-Subjects 2x2 Factorial'
          )
          self$results$analysis_type$setContent(
            '
            <div class="jmv-editable-header had-focus" level="1">
            <h2 contenteditable="" spellcheck="false">
            Fully Between-Subjects
            </h2>
            </div>
            '
          )
        }

      },
      .run = function() {

        from_raw <- (self$options$switch == "from_raw")
        mixed <- (self$options$design == "mixed")
        effect_size <- self$options$effect_size
        assume_equal_variance <- self$options$assume_equal_variance
        tbl_overview <- self$results$overview

        estimate <- jamovi_mdiff_2x2(
          self = self,
          save_raw_data = FALSE
        )

        # Print any notes that emerged from running the analysis
        jamovi_set_notes(self$results$help)

        # Check to see if the analysis ran
        #  If null, return
        #  If error, return the error
        if(is.null(estimate)) return(TRUE)
        if(is(estimate, "try-error")) stop(estimate[1])

        # Add in MoE
        estimate$es_mean_difference$moe <- (estimate$es_mean_difference$UL - estimate$es_mean_difference$LL)/2
        estimate$overview$moe <- (estimate$overview$mean_UL - estimate$overview$mean_LL)/2
        estimate$overview$s_pooled <- estimate$es_smd$denominator[[1]]
        estimate$es_mean_difference$s_component <- estimate$es_smd$denominator[[1]]

        estimate$es_mean_difference$effect_type <- paste(
          "<b>",
          estimate$es_mean_difference$effect_type,
          "</b>"
        )

        estimate$es_mean_difference$effects_complex[c(3, 6, 9, 12, 15)] <- paste(
          "<b>",
          estimate$es_mean_difference$effects_complex[c(3, 6, 9, 12, 15)],
          "</b>"
        )

        if (!is.null(estimate$es_median_difference)) {
          estimate$es_median_difference$effect_type <- paste(
            "<b>",
            estimate$es_median_difference$effect_type,
            "</b>"
          )

          estimate$es_median_difference$effects_complex[c(3, 6, 9, 12, 15)] <- paste(
            "<b>",
            estimate$es_median_difference$effects_complex[c(3, 6, 9, 12, 15)],
            "</b>"
          )

        }



        if (effect_size == "mean_difference" & !mixed) {
          mysep <- if (is.null(estimate$overview_properties$message_html)) NULL else "<BR>"
          if (!is.null(tbl_overview) & !is.null(assume_equal_variance)) {
            if (assume_equal_variance) {
              estimate$overview_properties$message_html <- paste(
                estimate$overview_properties$message_html,
                mysep,
                "Variances are assumed equal, so <i>s</i><sub>p</sub> was used to calculate each CI."
              )
            } else {
              estimate$overview_properties$message_html <- paste(
                estimate$overview_properties$message_html,
                mysep,
                "Variances are not assumed equal, and so the CI was calculated separately for each mean."
              )
            }
          }
        }



        # Fill tables
        jamovi_estimate_filler(self, estimate, TRUE)

      },
      .estimation_plot = function(image, ggtheme, theme, ...) {

        # Redo analysis
        estimate <- jamovi_mdiff_2x2(
          self = self,
          save_raw_data = TRUE
        )

        if (is.null(estimate)) return(TRUE)

        if(!is(estimate, "esci_estimate"))
          return(TRUE)


        which_plot <- switch(
          image$state,
          "Main Effect of A" = "main_effect_A",
          "Main Effect of B" = "main_effect_B",
          "Interaction" = "interaction"
        )

        gvA <- estimate$properties$grouping_variable_A_name

        gvB <- estimate$properties$grouping_variable_B_name

        which_title <- switch(
          image$state,
          "Main Effect of A" = paste("Main Effect of", gvA),
          "Main Effect of B" = paste("Main Effect of", gvB),
          "Interaction" = paste("Interaction of", gvA, "and", gvB)
        )

        image$setTitle(
          which_title
        )

        myplot <- jamovi_plot_mdiff(
          self,
          estimate[[which_plot]],
          image,
          ggtheme,
          theme
        )

        xlab <- jamovi_sanitize(
          my_value = self$options$xlab,
          return_value = NULL,
          na_ok = FALSE,
          my_value_name = "X axis: Title"
        )

        if (self$options$xlab %in% c("auto", "Auto", "AUTO")) {
          myplot <- myplot + ggplot2::xlab(NULL)
        }

        # mylabs <- paste(
        #   estimate$overview$grouping_variable_B_level,
        #   "\n",
        #   estimate$overview$grouping_variable_A_level,
        #   sep = ""
        # )

        mylabs <- paste(
          estimate$overview$grouping_variable_B_level,
          " - ",
          estimate$overview$grouping_variable_A_level,
          sep = ""
        )


        if (which_plot != "interaction") {
          mylabs <- c(
            mylabs,
            paste(" \n", myplot$scales$scales[[2]]$labels[5:7],  sep = "")
          )
        } else {
          mylabs <- c(
            mylabs,
            "Difference of\ndifferences"
            #myplot$scales$scales[[2]]$labels[5:5]
          )

          if (!is.null(myplot$layers["simple_effect_points"])) {
            try(myplot$layers[["simple_effect_points"]]$aes_params$fill <- "white")
            try(myplot$layers[["simple_effect_points"]]$aes_params$shape <- 23)
            try(myplot$layers[["simple_effect_points"]]$aes_params$size <- as.numeric(self$options$size_summary_reference) +1 )
          }
          if (!is.null(myplot$layers$simple_effect_lines)) {
            try(myplot$layers$simple_effect_lines$aes_params$size <- as.numeric(self$options$size_interval_reference))
          }
        }


        myplot$scales$scales[[2]]$labels <- mylabs
        myplot <- myplot + guides(x = legendry::guide_axis_nested(key = " - "))

        print(myplot)
        TRUE

      },
      .interaction_plot = function(image, ggtheme, theme, ...) {

        if (!self$options$show_interaction_plot) return(TRUE)

        # Redo analysis
        estimate <- jamovi_mdiff_2x2(
          self = self,
          save_raw_data = TRUE
        )

        if (is.null(estimate)) return(TRUE)

        if(!is(estimate, "esci_estimate"))
          return(TRUE)

        myplot <- plot_interaction(
          estimate,
          effect_size = if (self$options$effect_size == "median_difference") "median" else "mean",
          show_CI = self$options$show_CI,
          line_count = 125,
          line_alpha = 0.03
        )

        ymin <- jamovi_sanitize(
          my_value = self$options$ymin,
          return_value = myplot$esci_ymin,
          na_ok = TRUE,
          convert_to_number = TRUE,
          my_value_name = "Y axis: Minimum"
        )

        ymax <- jamovi_sanitize(
          my_value = self$options$ymax,
          return_value = myplot$esci_ymax,
          na_ok = TRUE,
          convert_to_number = TRUE,
          my_value_name = "Y axis: Maximum"
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
          my_value_name = "Y axis: Number of tick marks"
        )

        myplot <- myplot + ggplot2::scale_y_continuous(
          limits = c(ymin, ymax),
          n.breaks = ybreaks
        )


        # Axis font sizes
        axis.text.y <- jamovi_sanitize(
          my_value = self$options$axis.text.y,
          return_value = 14,
          na_ok = FALSE,
          convert_to_number = TRUE,
          lower = 1,
          lower_inclusive = TRUE,
          upper = 97,
          my_value_name = "Interaction Plot Y axis: Tick font size"
        )
        axis.title.y <- jamovi_sanitize(
          my_value = self$options$axis.title.y,
          return_value = 15,
          na_ok = FALSE,
          convert_to_number = TRUE,
          lower = 1,
          lower_inclusive = TRUE,
          upper = 97,
          my_value_name = "Interaction Plot Y axis: Label font size"
        )
        axis.text.x <- jamovi_sanitize(
          my_value = self$options$axis.text.x,
          return_value = 14,
          na_ok = FALSE,
          convert_to_number = TRUE,
          lower = 1,
          lower_inclusive = TRUE,
          upper = 97,
          my_value_name = "Interaction Plot X axis: Tick font size"
        )
        axis.title.x <- jamovi_sanitize(
          my_value = self$options$axis.title.x,
          return_value = 15,
          na_ok = FALSE,
          convert_to_number = TRUE,
          lower = 1,
          lower_inclusive = TRUE,
          upper = 97,
          my_value_name = "Interaction Plot X axis: Label font size"
        )

        myplot <- myplot + ggplot2::theme(
          axis.text.y = ggtext::element_markdown(size = axis.text.y),
          axis.title.y = ggtext::element_markdown(size = axis.title.y),
          axis.text.x = ggtext::element_markdown(size = axis.text.x),
          axis.title.x = ggtext::element_markdown(size = axis.title.x),
          legend.title = ggtext::element_markdown(size = axis.title.x),
          legend.text = ggtext::element_markdown(size = axis.text.x)
        )


        # Aesthetics
        myplot <- myplot + ggplot2::scale_shape_manual(
          values = c(
            self$options$shape_summary_reference,
            self$options$shape_summary_comparison
          )
        )

        myplot <- myplot + ggplot2::scale_color_manual(
          values = c(
            self$options$color_summary_reference,
            self$options$color_summary_comparison
          )
        )

        myplot <- myplot + ggplot2::scale_fill_manual(
          values = c(
            self$options$fill_summary_reference,
            self$options$fill_summary_comparison
          )
        )

        myplot <- myplot + ggplot2::scale_size_manual(
         values = c(
           as.integer(self$options$size_summary_reference),
           as.integer(self$options$size_summary_comparison)
          )
        )

        myplot <- myplot + ggplot2::scale_alpha_manual(
          values = c(
            as.numeric(self$options$alpha_summary_reference),
            as.numeric(self$options$alpha_summary_comparison)
          )
        )

        myplot <- myplot + ggplot2::scale_linetype_manual(
          values = c(
            self$options$linetype_summary_reference,
            self$options$linetype_summary_comparison
          )
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


        myplot$layers[["simple_effect_lines"]]$aes_params$linewidth <- as.numeric(self$options$size_interval_reference)/3

        # myplot <- myplot + ggplot2::scale_linewidth_manual(
        #   values = c(
        #     self$options$size_interval_comparison,
        #     self$options$size_interval_reference
        #   )
        # )


        print(myplot)

        return(TRUE)

      })
)



jamovi_mdiff_2x2 <- function(
    self,
    save_raw_data = FALSE
) {


  # Prelim -----------------------------------------------------
  mixed <- (self$options$design == "mixed")
  from_raw <- (self$options$switch == "from_raw")
  notes <- c(NULL)

  args <- list()

  if (!mixed) {
    # Step 1 - Check if analysis basics are defined ---------------
    #  if not, return NULL
    if(from_raw) {
      if (
        is.null(self$options$grouping_variable_A) |
        is.null(self$options$grouping_variable_B) |
        is.null(self$options$outcome_variable)
      ) return(NULL)
    } else {
      a_label <- jamovi_sanitize(
        self$options$A_label,
        return_value = "Variable A",
        na_ok = FALSE,
        my_value_name = "Variable A name"
      )

      b_label <- jamovi_sanitize(
        self$options$B_label,
        return_value = "Variable B",
        na_ok = FALSE,
        my_value_name = "Variable B name"
      )

      a1_label <- jamovi_sanitize(
        self$options$A1_label,
        return_value = "A1 level",
        na_ok = FALSE,
        my_value_name = "A1 label"
      )
      a2_label <- jamovi_sanitize(
        self$options$A2_label,
        return_value = "A2 level",
        na_ok = FALSE,
        my_value_name = "A2 label"
      )
      b1_label <- jamovi_sanitize(
        self$options$B1_label,
        return_value = "B1 level",
        na_ok = FALSE,
        my_value_name = "B1 label"
      )
      b2_label <- jamovi_sanitize(
        self$options$B2_label,
        return_value = "B2 level",
        na_ok = FALSE,
        my_value_name = "B2 label"
      )


      args$A1B1_mean <- jamovi_required_numeric(
        self$options$A1B1_mean,
        my_value_name = paste(
          "Mean of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A1B2_mean <- jamovi_required_numeric(
        self$options$A1B2_mean,
        my_value_name = paste(
          "Mean of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A2B1_mean <- jamovi_required_numeric(
        self$options$A2B1_mean,
        my_value_name = paste(
          "Mean of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A2B2_mean <- jamovi_required_numeric(
        self$options$A2B2_mean,
        my_value_name = paste(
          "Mean of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )


      args$A1B1_sd <- jamovi_required_numeric(
        self$options$A1B1_sd,
        lower = 0,
        lower_inclusive = FALSE,
        my_value_name = paste(
          "Standard deviation of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A1B2_sd <- jamovi_required_numeric(
        self$options$A1B2_sd,
        lower = 0,
        lower_inclusive = FALSE,
        my_value_name = paste(
          "Standard deviation of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A2B1_sd <- jamovi_required_numeric(
        self$options$A2B1_sd,
        lower = 0,
        lower_inclusive = FALSE,
        my_value_name = paste(
          "Standard deviation of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A2B2_sd <- jamovi_required_numeric(
        self$options$A2B2_sd,
        lower = 0,
        lower_inclusive = FALSE,
        my_value_name = paste(
          "Standard deviation of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )

      args$A1B1_n <- jamovi_required_numeric(
        self$options$A1B1_n,
        lower = 0,
        lower_inclusive = TRUE,
        integer_required = TRUE,
        my_value_name = paste(
          "Sample size of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A1B2_n <- jamovi_required_numeric(
        self$options$A1B2_n,
        lower = 0,
        lower_inclusive = TRUE,
        integer_required = TRUE,
        my_value_name = paste(
          "Sample size of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A2B1_n <- jamovi_required_numeric(
        self$options$A2B1_n,
        lower = 0,
        lower_inclusive = TRUE,
        integer_required = TRUE,
        my_value_name = paste(
          "Sample size of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
          sep = ""
        )
      )
      args$A2B2_n <- jamovi_required_numeric(
        self$options$A2B2_n,
        lower = 0,
        lower_inclusive = TRUE,
        integer_required = TRUE,
        my_value_name = paste(
          "Sample size of cell ",
          a_label,
          ":",
          a1_label,
          "; ",
          b_label,
          ":",
          b1_label,
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
            " more valid cells statistics.",
            sep = ""
          ),
          notes
        )
      }

      if (length(notes) > 0) {
        self$results$help$setState(notes)
        return(NULL)
      }

    }



    # Step 3: Run analysis ------------------------------------------
    # Fill in analysis properties

    # If from summary:
    # get outcome and grouping variable names
    # and set notes if they have been replaced
    if(!from_raw) {

    }

    call <- esci::estimate_mdiff_2x2_between
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
    notes <- c(notes, names(conf_level))
    args$conf_level <- conf_level/100
    args$assume_equal_variance <- self$options$assume_equal_variance

    # Set args for summary and raw data cases
    if (from_raw) {
      # Analysis from raw data
      args$data <- self$data
      args$grouping_variable_A <- self$options$grouping_variable_A
      args$grouping_variable_B <- self$options$grouping_variable_B

      args$outcome_variable <- self$options$outcome_variable


      if (is.null(levels(self$data[[args$outcome_variable]]))) {
        args$data[[args$outcome_variable]] <- as.numeric(args$data[[args$outcome_variable]])
      } else {
        args$data[[args$outcome_variable]] <- as.numeric(levels(self$data[[args$outcome_variable]]))[self$data[[args$outcome_variable]]]
        notes <- c(
          notes,
          paste(
            "Converted nominal variable ", args$outcome_variable, "to numeric; be sure this makes sense."
          )
        )
      }



    } else {
      args$means <- c(
        args$A1B1_mean,
        args$A1B2_mean,
        args$A2B1_mean,
        args$A2B2_mean
      )

      args$sds <- c(
        args$A1B1_sd,
        args$A1B2_sd,
        args$A2B1_sd,
        args$A2B2_sd
      )

      args$ns <- c(
        args$A1B1_n,
        args$A1B2_n,
        args$A2B1_n,
        args$A2B2_n
      )

      args$A1B1_mean <- NULL
      args$A1B2_mean <- NULL
      args$A2B1_mean <- NULL
      args$A2B2_mean <- NULL
      args$A1B1_sd <- NULL
      args$A1B2_sd <- NULL
      args$A2B1_sd <- NULL
      args$A2B2_sd <- NULL
      args$A1B1_n <- NULL
      args$A1B2_n <- NULL
      args$A2B1_n <- NULL
      args$A2B2_n <- NULL

      args$grouping_variable_A_levels <- c(
        a1_label,
        a2_label
      )

      args$grouping_variable_B_levels <- c(
        b1_label,
        b2_label
      )

      args$grouping_variable_A_name <- a_label
      args$grouping_variable_B_name <- b_label


      args$outcome_variable_name <- jamovi_sanitize(
        self$options$outcome_variable_name_bs,
        return_value = "My outcome variable",
        na_ok = FALSE,
        my_value_name = "Outcome variable name"
      )

      notes <- c(
        notes,
        names(args$outcome_variable_name),
        names(a1_label),
        names(a2_label),
        names(b1_label),
        names(b2_label),
        names(a_label),
        names(b_label)
      )

    }


  } else {

    if (
      is.null(self$options$grouping_variable) |
      is.null(self$options$outcome_variable_level1) |
      is.null(self$options$outcome_variable_level2)
    ) return(NULL)

    call <- esci::estimate_mdiff_2x2_mixed
    args <- list()
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
    notes <- c(notes, names(conf_level))
    args$conf_level <- conf_level/100
    args$data <- self$data
    args$grouping_variable <- self$options$grouping_variable
    args$outcome_variable_level1 <- self$options$outcome_variable_level1
    args$outcome_variable_level2 <- self$options$outcome_variable_level2
    args$outcome_variable_name <- self$options$outcome_variable_name
    args$repeated_measures_name <- self$options$repeated_measures_name

    if (is.null(levels(self$data[[args$outcome_variable_level1]]))) {
      args$data[[args$outcome_variable_level1]] <- as.numeric(args$data[[args$outcome_variable_level1]])
    } else {
      args$data[[args$outcome_variable_level1]] <- as.numeric(levels(self$data[[args$outcome_variable_level1]]))[self$data[[args$outcome_variable_level1]]]
      notes <- c(
        notes,
        paste(
          "Converted nominal variable ",args$outcome_variable_level1, "to numeric; be sure this makes sense."
        )
      )
    }

    if (is.null(levels(self$data[[args$outcome_variable_level2]]))) {
      args$data[[args$outcome_variable_level2]] <- as.numeric(args$data[[args$outcome_variable_level2]])
    } else {
      args$data[[args$outcome_variable_level2]] <- as.numeric(levels(self$data[[args$outcome_variable_level2]]))[self$data[[args$outcome_variable_level2]]]
      notes <- c(
        notes,
        paste(
          "Converted nominal variable ",args$outcome_variable_level2, "to numeric; be sure this makes sense."
        )
      )
    }

  }



  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  #self$results$debug$setVisible(TRUE)
  #self$results$debug$setContent(estimate)
  #return()

  # For summary data, store in a list based on outcome_variable_name
  if (!is(estimate, "try-error")) {

    evaluate_h <- self$options$evaluate_hypotheses

    if(evaluate_h) {
      # Test results
      effect_size = self$options$effect_size
      if (effect_size == "mean_difference") effect_size <- "mean"
      if (effect_size == "median_difference") effect_size <- "median"

      rope_upper <- jamovi_sanitize(
        self$options$null_boundary,
        na_ok = FALSE,
        return_value = 0,
        lower = 0,
        lower_inclusive = TRUE,
        convert_to_number = TRUE
      )

      rope_units <- "raw"
      try(rope_units <- self$options$rope_units)

      estimate$point_null <- NULL
      estimate$interval_null <- NULL

      for (myestimate in estimate) {
        if(is(myestimate, "esci_estimate")) {

          test_results <- test_mdiff(
            myestimate,
            effect_size = effect_size,
            rope = c(rope_upper * -1, rope_upper),
            rope_units = rope_units,
            output_html = TRUE
          )

          estimate$point_null <- rbind(
            estimate$point_null,
            test_results$point_null
          )

          estimate$interval_null <- rbind(
            estimate$interval_null,
            test_results$interval_null
          )

        }
      }

      if (!is.null(estimate$point_null)) {
        estimate$point_null$conclusion[[5]] <- gsub(
          pattern = "diff",
          replacement = "diffdiff",
          x = estimate$point_null$conclusion[[5]]
        )
      }
      if (!is.null(estimate$interval_null)) {
        estimate$interval_null$conclusion[[5]] <- gsub(
          pattern = "diff",
          replacement = "diffdiff",
          x = estimate$interval_null$conclusion[[5]]
        )
      }


      estimate$point_null$effect_type <- estimate$es_smd$effect_type
      estimate$point_null$effects_complex <- estimate$es_smd$effects_complex
      estimate$interval_null$effect_type <- estimate$es_smd$effect_type
      estimate$interval_null$effects_complex <- estimate$es_smd$effects_complex



      if (!is.null(names(rope_upper))) {
        self$results$point_null$setVisible(TRUE)
        self$results$interval_null$setVisible(FALSE)
      }


    }


    notes <- c(notes, estimate$warnings)
    self$results$help$setState(notes)

  }



  return(estimate)

}
