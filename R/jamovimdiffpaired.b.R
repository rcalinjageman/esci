
# This file is a generated template, your changes will not be overwritten

jamovimdiffpairedClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jamovimdiffpairedClass",
    inherit = jamovimdiffpairedBase,
    private = list(
        .init = function() {

          from_raw <- (self$options$switch == "from_raw")

          try(tbl_overview <- self$results$overview)
          try(tbl_es_mean_difference <- self$results$es_mean_difference)
          try(tbl_es_mean_ratio <- self$results$es_mean_ratio)
          try(tbl_es_r <- self$results$es_r)
          try(tbl_es_smd <- self$results$es_smd)
          try(tbl_es_median_difference <- self$results$es_median_difference)
          try(tbl_es_median_ratio <- self$results$es_median_ratio)

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
          jamovi_set_confidence(tbl_es_mean_difference, conf_level)
          jamovi_set_confidence(tbl_es_r, conf_level)
          jamovi_set_confidence(tbl_es_smd, conf_level)
          jamovi_set_confidence(tbl_es_mean_ratio, conf_level)
          jamovi_set_confidence(tbl_es_median_difference, conf_level)
          jamovi_set_confidence(tbl_es_median_ratio, conf_level)


          width <- jamovi_sanitize(
            my_value = self$options$es_plot_width,
            return_value = 600,
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

          image <- self$results$estimation_plots
          image$setSize(width , height)

        },
        .run = function() {

          from_raw <- (self$options$switch == "from_raw")

          estimate <- jamovi_mdiff_paired(
            self,
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
         estimate$es_mean_difference$n_component <- 1/sqrt(estimate$es_mean_difference$df+1)
         estimate$es_mean_difference$s_component <- estimate$es_mean_difference$moe / estimate$es_mean_difference$t_multiplier / estimate$es_mean_difference$n_component


         self$results$es_mean_difference$setNote(
           key = "sdiff",
           note = paste(
             "<i>s</i><sub>diff</sub> = ",
             format(estimate$es_mean_difference$s_component[[3]], 2),
             sep = ""
           )
         )

          # Fill tables
         jamovi_estimate_filler(self, estimate, TRUE)

        },
        .estimation_plots = function(image, ggtheme, theme, ...) {

          # Redo analysis
          estimate <- jamovi_mdiff_paired(
            self = self,
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


jamovi_mdiff_paired <- function(self, save_raw_data = FALSE) {

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


    args$n <- jamovi_required_numeric(
      self$options$n,
      integer_required = TRUE,
      lower = 0,
      lower_inclusive = FALSE,
      my_value_name = "<i>N</i>"
    )


    if (self$options$enter_r_or_sdiff == "enter_sdiff") {
        args$sdiff <- jamovi_required_numeric(
          self$options$sdiff,
          integer_required = FALSE,
          lower = 0,
          lower_inclusive = TRUE,
          my_value_name = "Standard deviation of difference scores (<i>s</i><sub>diff</sub>)"
        )

        if (!is.na(args$sdiff)) {
          args$correlation <- jamovi_required_numeric(
            self$options$correlation,
            integer_required = FALSE,
            lower = -1,
            lower_inclusive = TRUE,
            upper = 1,
            upper_inclusive = TRUE,
            my_value_name = "Standard deviation of difference scores (<i>s</i><sub>diff</sub>) produced an erroneous value of <i>r</i>.  With your input, <i>r</i> = "
          )
          args$sdiff <- NULL
        }

    } else {
      args$correlation <- jamovi_required_numeric(
        self$options$correlation,
        integer_required = FALSE,
        lower = -1,
        lower_inclusive = TRUE,
        upper = 1,
        upper_inclusive = TRUE,
        my_value_name = "correlation between measures (<i>r</i>)"
      )
    }


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
  call <- esci::estimate_mdiff_paired

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
    args$comparison_measure <- self$options$comparison_measure
    args$reference_measure <- self$options$reference_measure
    args$data[[args$comparison_measure]] <- as.numeric(args$data[[args$comparison_measure]])
    args$data[[args$reference_measure]] <- as.numeric(args$data[[args$reference_measure]])

    # self$results$debug$setContent(args$data)
    # self$results$debug$setVisible(TRUE)

  } else {
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

  classes <- NULL
  for (e in args) {
    classes <- paste(classes, class(e))
  }


  # Do analysis, then post any notes that have emerged
  estimate <- try(do.call(what = call, args = args))

  estimate <- jamovi_add_htest_mdiff(
    self = self,
    estimate = estimate
  )


  if (!is(estimate, "try-error")) {
    if (length(estimate$warnings) > 0) {
      notes <- c(notes, estimate$warnings)
    }
  }

  self$results$help$setState(notes)

  return(estimate)

}
