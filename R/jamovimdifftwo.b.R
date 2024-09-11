
# This file is a generated template, your changes will not be overwritten

jamovimdifftwoClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdifftwoClass",
    inherit = jamovimdifftwoBase,
    private = list(
        .init = function() {

            jamovi_mdiff_initialize(self, grouping_variable = TRUE)

        },
        .run = function() {

        from_raw <- (self$options$switch == "from_raw")


        estimate <- jamovi_mdiff_two(
          self,
          outcome_variable = NULL,
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

        # Add calculation details
        alpha <- 1 - self$options$conf_level/100
        estimate$es_mean_difference$t_multiplier <- stats::qt(1-alpha/2, estimate$es_mean_difference$df)

        for (x in 1:nrow(estimate$es_smd)) {
          estimate$overview[estimate$overview$outcome_variable_name == estimate$es_smd$outcome_variable_name[[x]], "s_pooled"] <- estimate$es_smd$denominator[[x]]
          estimate$es_mean_difference$s_component[c(x*3-2, x*3-1, x*3-0)] <- estimate$es_smd$denominator[[x]]
        }
        estimate$es_mean_difference$n_component <- estimate$es_mean_difference$SE / estimate$es_mean_difference$s_component

        # Fill tables
        jamovi_estimate_filler(self, estimate, TRUE)


        # Deal with plots ----------------------------------------
        # Set up array of estimation plots
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
          estimate <- jamovi_mdiff_two(
            self = self,
            outcome_variable = c(image$state),
            save_raw_data = TRUE
          )


          if(!is(estimate, "esci_estimate"))
            return(TRUE)

          if (is.null(estimate$properties$contrast)) {
            return(TRUE)
          }

          myplot <- jamovi_plot_mdiff(
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



jamovi_mdiff_two <- function(
  self,
  outcome_variable = NULL,
  save_raw_data = FALSE
) {
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

    args$reference_mean <- jamovi_required_numeric(
      self$options$reference_mean,
      my_value_name = "Reference <i>M</i>"
    )
    args$reference_sd <- jamovi_required_numeric(
      self$options$reference_sd,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Reference <i>s</i>"
    )
    args$reference_n <- jamovi_required_numeric(
      self$options$reference_n,
      integer_required = TRUE,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Reference <i>n</i>"
    )

    args$comparison_mean <- jamovi_required_numeric(
      self$options$comparison_mean,
      my_value_name = "Comparison <i>M</i>"
    )
    args$comparison_sd <- jamovi_required_numeric(
      self$options$comparison_sd,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "Comparison <i>s</i>"
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

    #unfilled <- names(args[which(is.na(args))])

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
  call <- esci::estimate_mdiff_two

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

  args$assume_equal_variance <- self$options$assume_equal_variance


  if(from_raw) {
    args$data <- self$data
    if (is.null(outcome_variable)) {
      args$outcome_variable <- unname(self$options$outcome_variable)
    } else {
      args$outcome_variable <- unname(outcome_variable)
      args$outcome_variable_name <- outcome_variable
    }
    for (x in 1:length(args$outcome_variable)) {
      args$data[[args$outcome_variable[[x]]]] <- as.numeric(args$data[[args$outcome_variable[[x]]]])
    }
    args$grouping_variable <- unname(self$options$grouping_variable)
    args$grouping_variable_name <- unname(self$options$grouping_variable)
    args$switch_comparison_order <- self$options$switch_comparison_order

  } else {
    args$outcome_variable_name <- jamovi_sanitize(
      self$options$outcome_variable_name,
      return_value = "My outcome variable",
      na_ok = FALSE
    )
    args$grouping_variable_name <- jamovi_sanitize(
      self$options$grouping_variable_name,
      return_value = "My grouping variable",
      na_ok = FALSE
    )
    comparison_level_name <- jamovi_sanitize(
      self$options$comparison_level_name,
      return_value = "Comparison level",
      na_ok = FALSE
    )
    reference_level_name <- jamovi_sanitize(
      self$options$reference_level_name,
      return_value = "Reference level",
      na_ok = FALSE
    )
    args$grouping_variable_levels <- c(
      reference_level_name,
      comparison_level_name
    )


    for (element in args) {
      notes <- c(notes, names(element))
    }

  }


  # self$results$debug$setContent(args)
  # self$results$debug$setVisible(TRUE)

  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  if (!is(estimate, "try-error")) {
    estimate <- jamovi_add_htest_mdiff(
      self = self,
      estimate = estimate
    )

    if (length(estimate$warnings) > 0) {
      if (!self$options$show_ratio) {
        estimate$warnings <- estimate$warnings[! names(estimate$warnings) %in% c("neg_values")]
      } else {
        to_show <- is.na(estimate$warnings["neg_values"])
        self$results$es_mean_ratio$setVisible(to_show & self$options$effect_size == "mean_difference")
        self$results$es_median_ratio$setVisible(to_show & self$options$effect_size == "median_difference")
      }

      notes <- c(notes, estimate$warnings)
    }
  }


  self$results$help$setState(notes)




  return(estimate)

}
