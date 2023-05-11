jamovi_meta_initialize <- function(self, has_switch = TRUE) {


  tbl_raw_data <- self$results$raw_data
  tbl_es_meta <- self$results$es_meta
  tbl_es_meta_difference <- self$results$es_meta_difference
  tbl_es_heterogeneity <- self$results$es_heterogeneity

  conf_level <<- jamovi_sanitize(
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

  jamovi_set_confidence(tbl_raw_data, conf_level)
  jamovi_set_confidence(tbl_es_meta, conf_level)
  jamovi_set_confidence(tbl_es_meta_difference, conf_level)
  jamovi_set_confidence(tbl_es_heterogeneity, conf_level)


  moderator <- !is.null(self$options$moderator)
  if (has_switch) {
    if (self$options$switch != "from_raw") {
      moderator <- !is.null(self$options$dmoderator)
    }
  }

  tbl_es_meta_difference$setVisible(moderator)
  tbl_es_meta$getColumn("moderator_variable_name")$setVisible(moderator)
  tbl_es_meta$getColumn("moderator_variable_level")$setVisible(moderator)
  tbl_raw_data$getColumn("moderator")$setVisible(moderator)

  # Tbl note
  weight_title <- if(self$options$random_effects != "fixed_effects")
    "RE weight"
  else
    "FE weight"

  tbl_raw_data$getColumn("weight")$setTitle(weight_title)



  width <- jamovi_sanitize(
    my_value = self$options$es_plot_width,
    return_value = 600,
    convert_to_number = TRUE,
    lower = 10,
    lower_inclusive = TRUE,
    upper = 2000,
    upper_inclusive = TRUE
  )
  height <- jamovi_sanitize(
    my_value = self$options$es_plot_height,
    return_value = 750,
    convert_to_number = TRUE,
    lower = 176,
    lower_inclusive = TRUE,
    upper = 4000,
    upper_inclusive = TRUE
  )
  image <- self$results$estimation_plots
  image$setSize(width, height)

  return(TRUE)

}


jamovi_meta_run <- function(
    estimate,
    self,
    has_reference = FALSE,
    has_switch = TRUE,
    has_aev = FALSE,
    has_effect_size_names = FALSE
) {

  # table handles
  tbl_raw_data <- self$results$raw_data
  tbl_es_meta <- self$results$es_meta
  tbl_es_meta_difference <- self$results$es_meta_difference
  tbl_es_heterogeneity <- self$results$es_heterogeneity

  # Fill tables
  estimate$es_heterogeneity$measure_html <- jamovi_heterogeneity_to_html(
    estimate$es_heterogeneity$measure
  )
  estimate$es_heterogeneity[estimate$es_heterogeneity$moderator_level != "Overall" & estimate$es_heterogeneity$measure == "Diamond Ratio", c("LL", "UL")] <- NA
  jamovi_estimate_filler(self, estimate, TRUE)


  # Update column headings if reference mean was used
  ref_note <- NULL
  if (has_reference) {
    reference_mean <- jamovi_sanitize(
      self$options$reference_mean,
      return_value = 0,
      na_ok = FALSE,
      convert_to_number = TRUE
    )
    reference_used <- is.null(names(reference_mean))

    if (reference_used) {
      new_title <- "<i>M</i> - <i>M</i><sub>Reference</sub>"
      tbl_raw_data$getColumn("effect_size")$setTitle(new_title)
      tbl_es_meta$getColumn("effect_size")$setTitle(new_title)
      tbl_es_meta_difference$getColumn("effect_size")$setTitle(new_title)
      ref_note <- paste(
        "Effect sizes are relative to a reference value of ",
        format(reference_mean, digits = 2),
        ".<br>",
        sep = ""
      )
    }

  }

  aev_note <- NULL
  aev <- TRUE
  if (has_aev) {
    aev <- self$options$assume_equal_variance
    if (aev) {
      aev_note <- "Effect sizes calculated assuming equal variance.<br>"
    } else {
      aev_note <- "Effect sizes calculated without assuming equal variance.<br>"
    }
  }


  # Tbl note
  model_note <- if(self$options$random_effects != "fixed_effects")
    "Estimate is based on a random effects (RE) model."
  else
    "Estimate is based on a fixed effect (FE) model."


  correction_note <- NULL
  smd_reported <- FALSE
  if (has_switch) {
    if (self$options$switch == "from_raw") {
      if (self$options$reported_effect_size == "smd_unbiased") {
        smd_reported <- TRUE
        correction_note <- paste(
          "  The standardized mean difference (",
          estimate$properties$effect_size_name_html,
          ") has been corrected for sampling bias.<br>",
          sep = ""
        )
      }
    } else {
      smd_reported <- TRUE

      correction_warning <- NULL
      if (!aev) {
        correction_warning <- "Without equal variace not assumed, studies with unequal sample sizes will not have perfect recovery of sampling variance; use equal variance assumption or original units input if this matters to you."
      }


      correction_note <- paste(
        "This analysis expected the inputted Cohen's <i>d</i> values to already be corrected for bias (",
        estimate$properties$effect_size_name_html,
        ").  ",
        correction_warning,
        "<br>",
        sep = ""
      )

    }
  }

  if (smd_reported) {
    tbl_raw_data$getColumn("effect_size_smd")$setTitle(estimate$properties$effect_size_name_html)
    tbl_es_meta$getColumn("effect_size_smd")$setTitle(estimate$properties$effect_size_name_html)
    tbl_es_meta_difference$getColumn("effect_size_smd")$setTitle(estimate$properties$effect_size_name_html)
  }

  if (has_effect_size_names) {
    tbl_raw_data$getColumn("effect_size")$setTitle(estimate$properties$effect_size_name_html)
    tbl_es_meta$getColumn("effect_size")$setTitle(estimate$properties$effect_size_name_html)
    tbl_es_meta_difference$getColumn("effect_size")$setTitle(estimate$properties$effect_size_name_html)
  }



  meta_note <- paste(
    ref_note,
    aev_note,
    correction_note,
    model_note
  )

  raw_note <- paste(
    ref_note,
    aev_note
  )


  tbl_es_meta$setNote(
    key = "meta_note",
    note = meta_note
  )
  tbl_es_meta_difference$setNote(
    key = "meta_note",
    note = meta_note
  )

  if (length(raw_note) > 0) {
    tbl_raw_data$setNote(
      key = "ref_note",
      note = raw_note
    )
  }


  return(TRUE)
}

jamovi_meta_forest_plot <- function(
    estimate,
    self,
    ggtheme,
    theme,
    has_switch = TRUE
) {

  meta_diamond_height <- jamovi_sanitize(
    my_value = self$options$meta_diamond_height,
    return_value = .35,
    na_ok = FALSE,
    convert_to_number = TRUE,
    lower = 0,
    lower_inclusive = FALSE,
    upper = 2,
    my_value_name = "Y axis: Diamond height"
  )

  explain_DR <- self$options$random_effects == "compare"
  include_PIs <- self$options$include_PIs & self$options$random_effects == "random_effects"

  myplot <- plot_meta(
    estimate,
    mark_zero = self$options$mark_zero,
    include_PIs = include_PIs,
    report_CIs = self$options$report_CIs,
    meta_diamond_height = meta_diamond_height,
    explain_DR = explain_DR,
    ggtheme = ggtheme
  )

  notes <- NULL

  myplot <- myplot + ggplot2::scale_size_continuous(
    range = c(
      as.numeric(self$options$size_base),
      as.numeric(self$options$size_base) * as.numeric(self$options$size_multiplier)
    )
  )

  if (!is.null(myplot$layers$raw_Reference_point)) {
    myplot$layers$raw_Reference_point$aes_params$shape <- self$options$shape_raw_reference
    myplot$layers$raw_Reference_point$aes_params$colour <- self$options$color_raw_reference
    myplot$layers$raw_Reference_point$aes_params$fill <- self$options$fill_raw_reference
    myplot$layers$raw_Reference_point$aes_params$alpha <- as.numeric(self$options$alpha_raw_reference)

    myplot$layers$raw_Reference_error$aes_params$colour <- self$options$color_interval_reference
    myplot$layers$raw_Reference_error$aes_params$size <- as.numeric(self$options$size_interval_reference)
    myplot$layers$raw_Reference_error$aes_params$alpha <- as.numeric(self$options$alpha_interval_reference)
    myplot$layers$raw_Reference_error$aes_params$linetype <- self$options$linetype_raw_reference
  }
  if (!is.null(myplot$layers$raw_Comparison_point)){
    myplot$layers$raw_Comparison_point$aes_params$shape <- self$options$shape_raw_comparison
    myplot$layers$raw_Comparison_point$aes_params$colour <- self$options$color_raw_comparison
    myplot$layers$raw_Comparison_point$aes_params$fill <- self$options$fill_raw_comparison
    myplot$layers$raw_Comparison_point$aes_params$alpha <- as.numeric(self$options$alpha_raw_comparison)

    myplot$layers$raw_Comparison_error$aes_params$colour <- self$options$color_interval_comparison
    myplot$layers$raw_Comparison_error$aes_params$size <- as.numeric(self$options$size_interval_comparison)
    myplot$layers$raw_Comparison_error$aes_params$alpha <- as.numeric(self$options$alpha_interval_comparison)
    myplot$layers$raw_Comparison_error$aes_params$linetype <- self$options$linetype_raw_comparison
  }
  if (!is.null(myplot$layers$raw_Unused_point)) {
    myplot$layers$raw_Unused_point$aes_params$shape <- self$options$shape_raw_unused
    myplot$layers$raw_Unused_point$aes_params$colour <- self$options$color_raw_unused
    myplot$layers$raw_Unused_point$aes_params$fill <- self$options$fill_raw_unused
    myplot$layers$raw_Unused_point$aes_params$alpha <- as.numeric(self$options$alpha_raw_unused)

    myplot$layers$raw_Unused_error$aes_params$colour <- self$options$color_interval_unused
    myplot$layers$raw_Unused_error$aes_params$size <- as.numeric(self$options$size_interval_unused)
    myplot$layers$raw_Unused_error$aes_params$alpha <- as.numeric(self$options$alpha_interval_unused)
    myplot$layers$raw_Unused_error$aes_params$linetype <- self$options$linetype_raw_unused
  }

  if (!is.null(myplot$layers$group_Overall_diamond)) {
    myplot$layers$group_Overall_diamond$aes_params$colour <- self$options$color_summary_overall
    myplot$layers$group_Overall_diamond$aes_params$fill <- self$options$fill_summary_overall
    myplot$layers$group_Overall_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_overall)
  }

  if (!is.null(myplot$layers$group_Comparison_diamond)) {
    myplot$layers$group_Comparison_diamond$aes_params$colour <- self$options$color_summary_comparison
    myplot$layers$group_Comparison_diamond$aes_params$fill <- self$options$fill_summary_comparison
    myplot$layers$group_Comparison_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_comparison)
  }

  if (!is.null(myplot$layers$group_Reference_diamond)) {
    myplot$layers$group_Reference_diamond$aes_params$colour <- self$options$color_summary_reference
    myplot$layers$group_Reference_diamond$aes_params$fill <- self$options$fill_summary_reference
    myplot$layers$group_Reference_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_reference)
  }

  if (!is.null(myplot$layers$group_Difference_diamond)) {
    myplot$layers$group_Difference_diamond$aes_params$shape <- self$options$shape_summary_difference
    myplot$layers$group_Difference_diamond$aes_params$colour <- self$options$color_summary_difference
    myplot$layers$group_Difference_diamond$aes_params$fill <- self$options$fill_summary_difference
    myplot$layers$group_Difference_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_difference)
  }

  if (!is.null(myplot$layers$group_Difference_line)) {
    myplot$layers$group_Difference_line$aes_params$colour <- self$options$color_interval_difference
    myplot$layers$group_Difference_line$aes_params$size <-  as.numeric(self$options$size_interval_difference)
    myplot$layers$group_Difference_line$aes_params$alpha <- as.numeric(self$options$alpha_interval_difference)
    myplot$layers$group_Difference_line$aes_params$linetype <- self$options$linetype_summary_difference
  }

  if (!is.null(myplot$layers$group_Unused_diamond)) {
    myplot$layers$group_Unused_diamond$aes_params$colour <- self$options$color_summary_unused
    myplot$layers$group_Unused_diamond$aes_params$fill <- self$options$fill_summary_unused
    myplot$layers$group_Unused_diamond$aes_params$alpha <- as.numeric(self$options$alpha_summary_unused)
  }

  if (!is.null(myplot$layers$group_Overall_PI)) {
    myplot$layers$group_Overall_PI$aes_params$colour <- self$options$color_summary_overall
    #myplot$layers$group_Overall_PI$aes_params$alpha <- as.numeric(self$options$alpha_summary_overall)
    myplot$layers$group_Overall_PI$aes_params$size <- as.numeric(self$options$size_interval_comparison) + 1
  }

  if (!is.null(myplot$layers$group_Comparison_PI)) {
    myplot$layers$group_Comparison_PI$aes_params$colour <- self$options$color_summary_comparison
    #myplot$layers$group_Comparison_PI$aes_params$alpha <- as.numeric(self$options$alpha_summary_comparison)
    myplot$layers$group_Comparison_PI$aes_params$size <- as.numeric(self$options$size_interval_comparison) + 1
  }

  if (!is.null(myplot$layers$group_Reference_PI)) {
    myplot$layers$group_Reference_PI$aes_params$colour <- self$options$color_summary_reference
    #myplot$layers$group_Reference_PI$aes_params$alpha <- as.numeric(self$options$alpha_summary_reference)
    myplot$layers$group_Reference_PI$aes_params$size <- as.numeric(self$options$size_interval_reference) + 1
  }

  if (!is.null(myplot$layers$group_Unused_PI)) {
    myplot$layers$group_Unused_P$aes_params$colour <- self$options$color_summary_unused
    #myplot$layers$group_Unused_PI$aes_params$alpha <- as.numeric(self$options$alpha_summary_unused)
    myplot$layers$group_Unused_PI$aes_params$size <- as.numeric(self$options$size_interval_unused) + 1
  }


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
    axis.text.x = element_text(size = axis.text.x),
    axis.title.x = element_text(size = axis.title.x)
  )

  # Axis limits
  xmin <- jamovi_sanitize(
    my_value = self$options$xmin,
    return_value = NA,
    na_ok = TRUE,
    convert_to_number = TRUE,
    my_value_name = "X axis: Minimum"
  )

  xmax <- jamovi_sanitize(
    my_value = self$options$xmax,
    return_value = NA,
    na_ok = TRUE,
    convert_to_number = TRUE,
    my_value_name = "X axis: Maximum"
  )

  xbreaks <- jamovi_sanitize(
    my_value = self$options$xbreaks,
    return_value = 5,
    na_ok = FALSE,
    lower = 2,
    lower_inclusive = TRUE,
    upper = 50,
    upper_inclusive = TRUE,
    convert_to_number = TRUE,
    my_value_name = "X axis: Number of tick marks"
  )

  dmin <- jamovi_sanitize(
    my_value = self$options$dmin,
    return_value = NA,
    na_ok = TRUE,
    convert_to_number = TRUE,
    my_value_name = "Difference axis: Minimum"
  )

  dmax <- jamovi_sanitize(
    my_value = self$options$dmax,
    return_value = NA,
    na_ok = TRUE,
    convert_to_number = TRUE,
    my_value_name = "Difference axis: Maxmimum"
  )

  dbreaks <- jamovi_sanitize(
    my_value = self$options$dbreaks,
    return_value = 4,
    na_ok = FALSE,
    lower = 1,
    lower_inclusive = TRUE,
    upper = 50,
    upper_inclusive = TRUE,
    convert_to_number = TRUE,
    my_value_name = "Difference axis: Number of tick marks"
  )


  # Axis labels
  xlab_replace <- paste(
    estimate$properties$effect_size_name_ggplot,
    ": ",
    estimate$es_meta$effect_label[[1]],
    sep = ""
  )

  xlab <- jamovi_sanitize(
    my_value = self$options$xlab,
    return_value = xlab_replace,
    na_ok = TRUE,
    my_value_name = "X axis: Title"
  )

  dlab <- jamovi_sanitize(
    my_value = self$options$dlab,
    return_value = "Difference axis",
    na_ok = TRUE,
    my_value_name = "Difference axis: Title"
  )

  # Apply axis labels and scales
  myplot <- myplot + ggplot2::scale_x_continuous(
    name = xlab,
    limits = c(xmin, xmax),
    n.breaks = xbreaks,
    position = "top"
  )

  moderator <- !is.null(self$options$moderator)

  if (has_switch) {
    if (self$options$switch != "from_raw") {
      moderator <- !is.null(self$options$dmoderator)
    }
  }

  if (moderator) {
    myplot <- esci_plot_difference_axis_x(
      myplot,
      estimate$es_meta_difference,
      dlim = c(dmin, dmax),
      d_n.breaks = dbreaks,
      d_lab = dlab
    )
  }

  width <- jamovi_sanitize(
    my_value = self$options$es_plot_width,
    return_value = 600,
    convert_to_number = TRUE,
    lower = 10,
    lower_inclusive = TRUE,
    upper = 2000,
    upper_inclusive = TRUE
  )
  height <- jamovi_sanitize(
    my_value = self$options$es_plot_height,
    return_value = 750,
    convert_to_number = TRUE,
    lower = 176,
    lower_inclusive = TRUE,
    upper = 4000,
    upper_inclusive = TRUE
  )

  notes <- c(
    notes,
    names(meta_diamond_height),
    names(axis.text.y),
    names(axis.text.x),
    names(axis.title.x),
    names(xlab),
    names(xmin),
    names(xmax),
    names(xbreaks),
    names(dlab),
    names(dmin),
    names(dmax),
    names(dbreaks),
    names(width),
    names(height)
  )

  self$results$estimation_plot_warnings$setState(notes)
  jamovi_set_notes(self$results$estimation_plot_warnings)

  return(myplot)

}
