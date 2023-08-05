jamovi_plot_mdiff <- function(
  self,
  estimate,
  image,
  ggtheme,
  theme
) {

  effect_size <- "mean"
  from_raw <- (self$options$switch == "from_raw")
  plot_median <- FALSE
  if (from_raw) {
    try(plot_median <- (self$options$effect_size == "median_difference"), silent = TRUE)
  }
  if (from_raw & plot_median) effect_size <- "median"

  divider <- 1
  if (effect_size == "median") divider <- 4

  # Basic plot
  notes <- NULL
  args <- list()


  #Hyothesis evaluation
  interval_null <- FALSE
  htest <- FALSE
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
      my_value_name = "Hypothesis Evaluation: Null range (+/-)"
    )

    args$rope_units <- "raw"
    try(args$rope_units <- self$options$rope_units, silent = TRUE)

    args$rope <- c(
      0 - args$null_boundary,
      0 + args$null_boundary
    )

    if (args$rope[[1]] != args$rope[[2]]) {
      interval_null <- TRUE
    }

    notes <- c(
      notes,
      names(args$null_boundary)
    )
    args$null_boundary <- NULL
  }



  args$estimate <- estimate
  args$effect_size <- effect_size
  args$data_layout <- self$options$data_layout
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
  args$difference_axis_units <- self$options$difference_axis_units
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
  args$difference_axis_space <- 0.5
  args$simple_contrast_labels <- self$options$simple_contrast_labels

  # Axis breaks
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

  args$ggtheme <- ggtheme[[1]]

  # Store notes from basic plot
  notes <- c(
    notes,
    args$warnings
  )
  args$warnings <- NULL


  # Do basic plot
  myplot <- do.call(
    what = plot_mdiff,
    args = args
  )

  if (htest) {
      myplot$layers[["null_line"]]$aes_params$colour <- self$options$null_color
      if (interval_null) {
        try(myplot$layers[["null_interval"]]$aes_params$fill <- self$options$null_color, silent = TRUE)

        try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_interval_difference)/divider+1, silent = TRUE)
        try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_interval_difference), silent = TRUE)
        try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_interval_difference, silent = TRUE)
        try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$self$options$linetype_summary_difference, silent = TRUE)

        if (plot_median) {
          try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_summary_difference, silent = TRUE)
          try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_summarydifference)/divider*1.3, silent = TRUE)
          try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_summary_difference), silent = TRUE)
          try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$self$options$linetype_summary_difference, silent = TRUE)
        }

      }
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

  shape_raw_reference <- "circle"
  color_raw_reference <- "black"
  fill_raw_reference <- "black"
  size_raw_reference <- 1
  alpha_raw_reference <- 1

  shape_raw_difference <- "circle"
  color_raw_difference <- "black"
  fill_raw_difference <- "black"
  size_raw_difference <- 1
  alpha_raw_difference <- 1

  shape_raw_unused <- "circle"
  shape_summary_unused <- "circle"
  color_raw_unused <- "black"
  color_summary_unused <- "black"
  fill_raw_unused <- "black"
  fill_summary_unused <- "black"
  size_raw_unused <- 1
  size_summary_unused <- 1
  alpha_raw_unused <- 1
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

  try(shape_raw_difference <- self$options$shape_raw_difference, silent = TRUE)
  try(color_raw_difference <- self$options$color_raw_difference, silent = TRUE)
  try(fill_raw_difference <- self$options$fill_raw_difference, silent = TRUE)
  try(size_raw_difference <- as.integer(self$options$size_raw_difference), silent = TRUE)
  try(alpha_raw_difference <- as.numeric(self$options$alpha_raw_difference), silent = TRUE)

  try(shape_raw_reference <- self$options$shape_raw_reference, silent = TRUE)
  try(color_raw_reference <- self$options$color_raw_reference, silent = TRUE)
  try(fill_raw_reference <- self$options$fill_raw_reference, silent = TRUE)
  try(size_raw_reference <- as.integer(self$options$size_raw_reference), silent = TRUE)
  try(alpha_raw_reference <- as.numeric(self$options$alpha_raw_reference), silent = TRUE)

  try(shape_raw_unused <- self$options$shape_raw_unused, silent = TRUE)
  try(shape_summary_unused <- self$options$shape_summary_unused, silent = TRUE)
  try(color_raw_unused <- self$options$color_raw_unused, silent = TRUE)
  try(color_summary_unused <- self$options$color_summary_unused, silent = TRUE)
  try(fill_raw_unused <- self$options$fill_raw_unused, silent = TRUE)
  try(fill_summary_unused <- self$options$fill_summary_unused, silent = TRUE)
  try(size_raw_unused <- as.integer(self$options$size_raw_reference), silent = TRUE)
  try(size_summary_unused <- as.integer(self$options$size_summary_unused), silent = TRUE)
  try(alpha_raw_unused <- as.numeric(self$options$alpha_raw_unused), silent = TRUE)
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


  # Aesthetics
  myplot <- myplot + ggplot2::scale_shape_manual(
    values = c(
      "Reference_raw" = shape_raw_reference,
      "Comparison_raw" = self$options$shape_raw_comparison,
      "Difference_raw" = shape_raw_difference,
      "Unused_raw" = shape_raw_unused,
      "Reference_summary" = self$options$shape_summary_reference,
      "Comparison_summary" = self$options$shape_summary_comparison,
      "Difference_summary" = self$options$shape_summary_difference,
      "Unused_summary" = shape_summary_unused
    )
  )

  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_raw" = color_raw_reference,
      "Comparison_raw" = self$options$color_raw_comparison,
      "Difference_raw" = color_raw_difference,
      "Unused_raw" = color_raw_unused,
      "Reference_summary" = self$options$color_summary_reference,
      "Comparison_summary" = self$options$color_summary_comparison,
      "Difference_summary" = self$options$color_summary_difference,
      "Unused_summary" = color_summary_unused
    ),
    aesthetics = c("color", "point_color")
  )

  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c(
      "Reference_raw" = fill_raw_reference,
      "Comparison_raw" = self$options$fill_raw_comparison,
      "Difference_raw" = fill_raw_difference,
      "Unused_raw" = fill_raw_unused,
      "Reference_summary" = self$options$fill_summary_reference,
      "Comparison_summary" = self$options$fill_summary_comparison,
      "Difference_summary" = self$options$fill_summary_difference,
      "Unused_summary" = fill_summary_unused
    ),
    aesthetics = c("fill", "point_fill")
  )

  divider <- 1
  if (effect_size == "median") divider <- 4

  myplot <- myplot + ggplot2::discrete_scale(
    c("size", "point_size"),
    "point_size_d",
    function(n) return(c(
      "Reference_raw" = size_raw_reference,
      "Comparison_raw" = as.integer(self$options$size_raw_comparison),
      "Difference_raw" = size_raw_difference,
      "Unused_raw" = size_raw_unused,
      "Reference_summary" = as.integer(self$options$size_summary_reference)/divider,
      "Comparison_summary" = as.integer(self$options$size_summary_comparison)/divider,
      "Difference_summary" = as.integer(self$options$size_summary_difference)/divider,
      "Unused_summary" = size_summary_unused/divider
    ))
  )

  myplot <- myplot + ggplot2::discrete_scale(
    c("alpha", "point_alpha"),
    "point_alpha_d",
    function(n) return(c(
      "Reference_raw" = alpha_raw_reference,
      "Comparison_raw" = as.numeric(self$options$alpha_raw_comparison),
      "Difference_raw" = alpha_raw_difference,
      "Unused_raw" = alpha_raw_unused,
      "Reference_summary" = as.numeric(self$options$alpha_summary_reference),
      "Comparison_summary" = as.numeric(self$options$alpha_summary_comparison),
      "Difference_summary" = as.numeric(self$options$alpha_summary_difference),
      "Unused_summary" = alpha_summary_unused
    ))
  )

  # Error bars
  myplot <- myplot + ggplot2::scale_linetype_manual(
    values = c(
      "Reference_summary" = linetype_summary_reference,
      "Comparison_summary" = self$options$linetype_summary_comparison,
      "Difference_summary" = self$options$linetype_summary_difference,
      "Unused_summary" = linetype_summary_unused
    )
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_summary" = color_interval_reference,
      "Comparison_summary" = self$options$color_interval_comparison,
      "Difference_summary" = self$options$color_interval_difference,
      "Unused_summary" = color_interval_unused
    ),
    aesthetics = "interval_color"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_alpha",
    "interval_alpha_d",
    function(n) return(c(
      "Reference_summary" = as.numeric(alpha_interval_reference),
      "Comparison_summary" = as.numeric(self$options$alpha_interval_comparison),
      "Difference_summary" = as.numeric(self$options$alpha_interval_difference),
      "Unused_summary" = alpha_interval_unused
    ))
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_size",
    "interval_size_d",
    function(n) return(c(
      "Reference_summary" = as.integer(size_interval_reference),
      "Comparison_summary" = as.integer(self$options$size_interval_comparison),
      "Difference_summary" = as.integer(self$options$size_interval_difference),
      "Unused_summary" = size_interval_unused
    ))
  )

  # Slab
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c(
      "Reference_summary" = fill_error_reference,
      "Comparison_summary" = self$options$fill_error_comparison,
      "Difference_summary" = self$options$fill_error_difference,
      "Unused_summary" = fill_error_unused
    ),
    aesthetics = "slab_fill"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "slab_alpha",
    "slab_alpha_d",
    function(n) return(c(
      "Reference_summary" = as.numeric(alpha_error_reference),
      "Comparison_summary" = as.numeric(self$options$alpha_error_comparison),
      "Difference_summary" = as.numeric(self$options$alpha_error_difference),
      "Unused_summary" = alpha_error_unused
    ))
  )


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

  return(myplot)
}





jamovi_plot_pdiff <- function(
    self,
    estimate,
    image,
    ggtheme,
    theme
) {

  from_raw <- (self$options$switch == "from_raw")
  divider <- 4

  # Basic plot
  notes <- NULL
  args <- list()


  #Hypothesis evaluation
  interval_null <- FALSE
  htest <- FALSE
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
        my_value_name = "Hypothesis Evaluation: <i>H</i><sub>0</sub> boundary (+/-)"
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
      names(args$null_boundary)
    )
    args$warnings <- NULL
    args$null_boundary <- NULL
  }


  args$estimate <- estimate
  # args$error_layout <- self$options$error_layout
  # args <- jamovi_arg_builder(
  #   args,
  #   "error_scale",
  #   self$options$error_scale,
  #   return_value = 0.25,
  #   lower = 0,
  #   lower_inclusive = TRUE,
  #   upper = 5,
  #   upper_inclusive = TRUE,
  #   my_value_name = "Distributions: Width",
  #   convert_to_number = TRUE
  # )
  # args <- jamovi_arg_builder(
  #   args,
  #   "error_nudge",
  #   self$options$error_nudge,
  #   return_value = 0.4,
  #   lower = 0,
  #   lower_inclusive = TRUE,
  #   upper = 5,
  #   upper_inclusive = TRUE,
  #   my_value_name = "Distributions: Offset from data",
  #   convert_to_number = TRUE
  # )
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
  args$difference_axis_space <- 0.5
  args$simple_contrast_labels <- self$options$simple_contrast_labels


  # Axis breaks
  args <- jamovi_arg_builder(
    args,
    "ylim",
    my_value = self$options$ymin,
    lower = -1,
    lower_inclusive = TRUE,
    upper = 2,
    upper_inclusive = TRUE,
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
    lower = if(is.na(args$ylim)) 0 else args$ylim,
    lower_inclusive = FALSE,
    upper = 2,
    upper_inclusive = TRUE,
    convert_to_number = TRUE,
    my_value_name = "Y axis: Axis maximum"
  )

  args$ylim <- c(args$ylim, args$ylim2)
  args$ylim2 <- NULL


  args <- jamovi_arg_builder(
    args,
    "ybreaks",
    self$options$ybreaks,
    return_value = 5,
    lower = 1,
    lower_inclusive = TRUE,
    upper = 50,
    upper_inclusive = TRUE,
    my_value_name = "Y axis: Number of tick marks",
    convert_to_number = TRUE
  )
  args$ggtheme <- ggtheme[[1]]

  # Store notes from basic plot
  notes <- c(
    notes,
    args$warnings
  )
  args$warnings <- NULL


  # Do basic plot
  myplot <- do.call(
    what = plot_pdiff,
    args = args
  )

  if (htest) {
    myplot$layers[["null_line"]]$aes_params$colour <- self$options$null_color
    if (interval_null) {
      try(myplot$layers[["null_interval"]]$aes_params$fill <- self$options$null_color, silent = TRUE)

      try(myplot$layers[["ta_CI"]]$aes_params$size <- as.numeric(self$options$size_summary_difference, silent = TRUE)/divider+1)
      try(myplot$layers[["ta_CI"]]$aes_params$alpha <- as.numeric(self$options$alpha_summary_difference), silent = TRUE)
      try(myplot$layers[["ta_CI"]]$aes_params$colour <- self$options$color_summary_difference, silent = TRUE)
      try(myplot$layers[["ta_CI"]]$aes_params$linetype <- self$options$self$options$linetype_summary_difference, silent = TRUE)

    }
  }

  # # Basic graph options --------------------
  # # Axis font sizes
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



  # # Axis labels
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
  #
  #
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
  # myplot <- myplot + ggplot2::scale_color_manual(
  #   values = c(
  #     "Reference_summary" = color_interval_reference,
  #     "Comparison_summary" = self$options$color_interval_comparison,
  #     "Difference_summary" = self$options$color_interval_difference,
  #     "Unused_summary" = color_interval_unused
  #   ),
  #   aesthetics = "interval_color"
  # )
  # myplot <- myplot + ggplot2::discrete_scale(
  #   "interval_alpha",
  #   "interval_alpha_d",
  #   function(n) return(c(
  #     "Reference_summary" = as.numeric(alpha_interval_reference),
  #     "Comparison_summary" = as.numeric(self$options$alpha_interval_comparison),
  #     "Difference_summary" = as.numeric(self$options$alpha_interval_difference),
  #     "Unused_summary" = alpha_interval_unused
  #   ))
  # )
  # myplot <- myplot + ggplot2::discrete_scale(
  #   "interval_size",
  #   "interval_size_d",
  #   function(n) return(c(
  #     "Reference_summary" = as.integer(size_interval_reference),
  #     "Comparison_summary" = as.integer(self$options$size_interval_comparison),
  #     "Difference_summary" = as.integer(self$options$size_interval_difference),
  #     "Unused_summary" = size_interval_unused
  #   ))
  # )
  #
  # # Slab
  # myplot <- myplot + ggplot2::scale_fill_manual(
  #   values = c(
  #     "Reference_summary" = fill_error_reference,
  #     "Comparison_summary" = self$options$fill_error_comparison,
  #     "Difference_summary" = self$options$fill_error_difference,
  #     "Unused_summary" = fill_error_unused
  #   ),
  #   aesthetics = "slab_fill"
  # )
  # myplot <- myplot + ggplot2::discrete_scale(
  #   "slab_alpha",
  #   "slab_alpha_d",
  #   function(n) return(c(
  #     "Reference_summary" = as.numeric(alpha_error_reference),
  #     "Comparison_summary" = as.numeric(self$options$alpha_error_comparison),
  #     "Difference_summary" = as.numeric(self$options$alpha_error_difference),
  #     "Unused_summary" = alpha_error_unused
  #   ))
  # )


  width <- jamovi_sanitize(
    my_value = self$options$es_plot_width,
    return_value = 400,
    convert_to_number = TRUE,
    lower = 10,
    lower_inclusive = TRUE,
    upper = 3000,
    upper_inclusive = TRUE,
    my_value_name = "Plot width"
  )
  height <- jamovi_sanitize(
    my_value = self$options$es_plot_height,
    return_value = 450,
    convert_to_number = TRUE,
    lower = 10,
    lower_inclusive = TRUE,
    upper = 4000,
    upper_inclusive = TRUE,
    my_value_name = "Plot height"
  )

  # Struggling to get htest warnings to show up... so copying this in
  #  again?

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
      names(rope_upper)
    )
  )
  jamovi_set_notes(self$results$estimation_plot_warnings)

  return(myplot)
}

