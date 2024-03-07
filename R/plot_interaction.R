#' Plot the interaction from a 2x2 design
#'
#' @description
#' `plot_interaction` helps visualize the interaction from a 2x2 design.
#' It plots the 2 simple effects for the first factor and can also
#' help visualize the CIs on those simple effects.  It is the comparison
#' between those simple effects that represents an interaction (the difference
#' in the difference).
#' You can pass esci-estimate objects generated
#' [esci::estimate_mdiff_2x2_between()] or [esci::estimate_mdiff_2x2_mixed()].
#' This function returns a ggplot2 object.
#'
#'
#' @inherit plot_describe details
#'
#'
#' @param estimate A esci_estimate object with raw data an es_mdiff_2x2_
#' function
#'
#' @param effect_size Optional; one of 'mean' or 'median' to determine the
#' measure of central tendency plotted.  Note that median is only available if
#' the estimate was generated from raw data.  Defauls to 'mean'
#' @param show_CI Optional logical; set to TRUE to visualize the confidence
#' intervals on each simple effect; defaults to FALSE
#' @param ggtheme Optional ggplot2 theme object to specify the visual style of the
#' plot.  Defaults to [ggplot2::theme_classic()]
#' @param line_count Optional integer > 0 to specify the number of lines used
#' to visualize the simple-effect confidence intervals; defaults to 100
#' @param line_alpha Optional numeric between 0 and 1 to specify the
#' alpha (transparancy) of the confidence interval lines; defaults to 0.02
#'
#'
#' @inherit plot_describe return
#'
#'
#' @inherit estimate_mdiff_2x2_between examples
#'
#' @export
plot_interaction <- function(
    estimate,
    effect_size = c("mean", "median"),
    show_CI = FALSE,
    ggtheme = NULL,
    line_count = 100,
    line_alpha = 0.02
) {

  # Input checks
  effect_size <- match.arg(effect_size)
  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}
  x <- y <- yend <- xend <- grouping_variable_A_level <- grouping_variable_B_level <- NULL

  if (effect_size == "median") {
    if (is.null(estimate$es_median_difference)) {
      warning("Median effect size specified but not found in this estimate; switching to mean")
      effect_size = "mean"
    }
  }


  # Pick column to plot
  ov <- estimate$overview
  ov$y <- if (effect_size == "median") ov$median else ov$mean


  # Make plot
  myplot <- ggplot2::ggplot() + ggtheme


  # show CI on simple effects
  if (show_CI) {
    mytbl <- "es_mean_difference"
    if (effect_size == "median") {
      mytbl <- "es_median_difference"
    }

    moe <- (estimate$simple_effect_B_at_A1[[mytbl]]$UL[[3]] - estimate$simple_effect_B_at_A1[[mytbl]]$LL[[3]])/4


    mytail <- (1 - estimate$properties$conf_level)/2
    p_values <- seq(
      from = mytail,
      to = 1 - mytail,
      length.out = line_count
    )
    z_values <- qnorm(p_values, lower.tail = TRUE)

    se_lines <- data.frame(
      to_add = z_values * abs((moe/z_values[1]))
    )

    # se_lines <- data.frame(
    #   to_add = seq(from = moe, to = -moe, length.out = line_count)
    # )

    se_lines$y = ov$y[[1]] + se_lines$to_add
    se_lines$yend = ov$y[[2]] - se_lines$to_add

    se_lines$x <- ov$grouping_variable_B_level[[1]]
    se_lines$xend <- ov$grouping_variable_B_level[[2]]
    se_lines$grouping_variable_A_level <- ov$grouping_variable_A_level[[1]]

    moe2 <- (estimate$simple_effect_B_at_A2[[mytbl]]$UL[[3]] - estimate$simple_effect_B_at_A2[[mytbl]]$LL[[3]])/4

    se_lines2 <- data.frame(
      to_add = z_values * abs((moe2/z_values[1]))
    )

    # se_lines2 <- data.frame(
    #   to_add = seq(from = moe2, to = -moe2, length.out = line_count)
    # )

    se_lines2$y = ov$y[[3]] + se_lines$to_add
    se_lines2$yend = ov$y[[4]] - se_lines$to_add

    se_lines2$x <- ov$grouping_variable_B_level[[1]]
    se_lines2$xend <- ov$grouping_variable_B_level[[2]]
    se_lines2$grouping_variable_A_level <- ov$grouping_variable_A_level[[3]]

    se_lines <- rbind(se_lines, se_lines2)

    myplot <- myplot + ggplot2::geom_segment(
      data = se_lines,
      ggplot2::aes(
        y = y,
        x = x,
        yend = yend,
        xend = xend,
        colour = grouping_variable_A_level
      ),
      alpha = line_alpha,
      linewidth = 1
    )

    myplot <- esci_plot_layers(myplot, "simple_effect_cis")

  }

  # Simple effect lines
  lines <- ov[c(1, 3), ]
  lines$yend <- ov[c(2,4), "y"]
  lines$xend <- ov[c(2,4), "grouping_variable_B_level"]


  myplot <- myplot + ggplot2::geom_segment(
    data = lines,
    ggplot2::aes(
      x = grouping_variable_B_level,
      y = y,
      xend = xend,
      yend = yend,
      colour = grouping_variable_A_level,
      linetype = grouping_variable_A_level,
      alpha = grouping_variable_A_level,
      linewidth = grouping_variable_A_level
    )
  )

  myplot <- esci_plot_layers(myplot, "simple_effect_lines")


  # Central tendency markers
  myplot <- myplot + ggplot2::geom_point(
    data = ov,
    ggplot2::aes(
      x = grouping_variable_B_level,
      y =y,
      group = grouping_variable_A_level,
      shape = grouping_variable_A_level,
      size = grouping_variable_A_level,
      fill = grouping_variable_A_level,
      colour = grouping_variable_A_level,
      alpha = grouping_variable_A_level
    )
  )

  myplot <- esci_plot_layers(myplot, "simple_effect_markers")

  # Correct x axis order
  myplot <- myplot + ggplot2::scale_x_discrete(
    limits = c(
      ov$grouping_variable_B_level[[1]],
      ov$grouping_variable_B_level[[2]]
    )
  )

  # X and Y axis labels
  xlab <- estimate$overview$grouping_variable_B_name[[1]]
  ylab <- paste(
    if (effect_size == "median") "Median" else "Mean",
    estimate$overview$outcome_variable_name[[1]],
    sep = " "
  )

  myplot <- myplot + ggplot2::ylab(ylab)
  myplot <- myplot + ggplot2::xlab(xlab)

  # Title of legend
  scale_title <- estimate$overview$grouping_variable_A_name[[1]]
  myplot <- myplot + ggplot2::labs(
    fill = scale_title,
    colour = scale_title,
    size = scale_title,
    alpha = scale_title,
    shape = scale_title,
    linewidth = scale_title,
    group = scale_title,
    linetype = scale_title
  )

  # Set a y axis limit that respects underlying uncertainty and/or data range
  ys <- NULL
  if (effect_size == "median") {
    ys <- c(ys, estimate$overview$median_LL, estimate$overview$median_UL)
  } else {
    ys <- c(ys, estimate$overview$mean_LL, estimate$overview$mean_UL)
  }
  if (!is.null(estimate$interaction$raw_data$outcome_variable)) {
    ys <- c(ys, estimate$interaction$raw_data$outcome_variable)
  }
  if (show_CI) {
    ys <- c(ys, se_lines$y, se_lines$yend)
  }

  ymin <- min(ys, na.rm = TRUE)
  ymax <- max(ys, na.rm = TRUE)

  myplot <- myplot + ggplot2::ylim(c(ymin, ymax))

  # allow markdown
  myplot <- myplot + ggplot2::theme(
    axis.text.y = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown(),
    axis.text.x = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),
    legend.title = ggtext::element_markdown(),
    legend.text = ggtext::element_markdown()
  )


  myplot$esci_ymin <- ymin
  myplot$esci_ymax <- ymax


  return(myplot)

}
