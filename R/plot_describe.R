#' Plot a histogram or dotplot of an estimated magnitude with raw data
#'
#' @description
#' `esci_plot_describe` returns a ggplot2 object
#'
#'
#' @param estimate A esci_estimate object with raw data an es_mean
#' @param type histogram or dotplot
#' @param mark_mean should mean be marked?
#' @param mark_median should median be marked?
#' @param mark_sd should mean be marked?
#' @param mark_quartiles should mean be marked?
#' @param mark_z_lines should z lines be marked?
#' @param mark_percentile a percentile (0 to 1) to be marked
#' @param histogram_bins number of bins if a histogram
#' @param ylim 2-length numeric vector
#' @param ybreaks numeric >= 1
#' @param xlim 2-length numeric vector
#' @param xbreaks numeric >= 1
#' @param fill_regular color for
#' @param fill_highlighted color for
#' @param color outline color
#' @param marker_size Size of markers
#' @param ggtheme theme to apply, if any
#'
#' @export
plot_describe <- function(
  estimate,
  type = c("histogram", "dotplot"),
  mark_mean = FALSE,
  mark_median = FALSE,
  mark_sd = FALSE,
  mark_quartiles = FALSE,
  mark_z_lines = FALSE,
  mark_percentile = NULL,
  histogram_bins = 12,
  ylim = c(0, NA),
  ybreaks = NULL,
  xlim = c(NA, NA),
  xbreaks = NULL,
  fill_regular = "#008DF9",
  fill_highlighted = "#E20134",
  color = "black",
  marker_size = 5,
  ggtheme = NULL
) {

  # Input checks ------------------------------------------------------
  #esci_assert_type(estimate, "is.estimate")

  if(is.null(estimate$es_mean)) {
    stop("This plot function is for a single quantiative variable; this estimate passed is not the right type.")
  }

  if(is.null(estimate$raw_data)) {
    stop("This plot function requires raw data, but the estimate passed does not have raw data attached.")
  }

  type <- match.arg(type)

  if (is.null(mark_percentile)) {
    draw_percentile <- FALSE
    mark_percentile <- 0
  } else {
    if (mark_percentile == 0) {
      draw_percentile <- FALSE
    } else {
      draw_percentile <- TRUE
    }
  }

  if(is.null(ggtheme)) { ggtheme <- ggplot2::theme_classic()}

  if (type == "dotplot") ylim <- c(0, NA)


  # Prep -------------------------------------------------------------
  # Handle
  rd <- estimate$raw_data
  rd_mean <- estimate$overview$mean[1]
  rd_sd <- estimate$overview$sd[1]
  rd_median <- estimate$overview$median[1]

  # Marking percentile
  fills <- c("FALSE" = fill_regular, "TRUE" = fill_highlighted)

  rd <- rd[!is.na(rd$outcome_variable), ]
  rd$outcome_variable <- sort(rd$outcome_variable)
  rd$q <- seq(1:nrow(rd))/nrow(rd)
  rd$qfill <- rd$q <= mark_percentile

  if (!draw_percentile) rd$qfill <- FALSE


  # Plot --------------------------------------------------------------
  # Histogram or dotplot
  if (type == "histogram") {
    myplot <- ggplot2::ggplot() + ggplot2::geom_histogram(
      data = rd,
      ggplot2::aes(
        x = outcome_variable,
        fill = qfill
      ),
      bins = histogram_bins,
      color = color
    )

  } else {
    myplot <- ggplot2::ggplot() + ggdist::geom_dots(
      data = rd,
      ggplot2::aes(
        x = outcome_variable,
        z = q,
        fill = stat(z <= mark_percentile)
      ),
      orientation = "horizontal",
      scale = 1,
      color = color
    )

  }

  # Theme
  myplot <- myplot + ggtheme

  # Fills for marking percentiles
  myplot <- myplot + ggplot2::scale_fill_manual(values = fills)

  # z lines
  if (mark_z_lines) {
    z <- seq(from = -3, to = 3, by = 1)
    zdata <- data.frame(
      z = z,
      x = rd_mean + (rd_sd * z),
      y = 0,
      label = paste("italic('z')==", z, sep = "")
    )

    myplot <- myplot + ggplot2::geom_vline(
      data = zdata,
      color = "black",
      linetype = "dotted",
      ggplot2::aes(xintercept = x)
    )
  }


  # Get top and bottom
  top <- if (type == "histogram")
    max(
      c(
        ylim[2],
        ggplot2::ggplot_build(myplot)$data[[1]]$y
      ),
      na.rm = TRUE
    )
  else
    max(c(1, ylim[2]), na.rm = TRUE)

  spacing <- if (type == "histogram")
    top * .05
  else
    .07

  bottom <- min(
    c(
      0,
      ylim[1]
    ),
    na.rm = TRUE
  )


  # Mark mean
  if (mark_mean) {
    myplot <- myplot + ggplot2::geom_vline(
      xintercept = rd_mean,
      linetype = "solid",
      color = "#009F81",
      size = 1.5
    )
    myplot <- myplot + ggplot2::geom_point(
      data = data.frame(
        x = rd_mean,
        y = bottom - spacing*.5
      ),
      ggplot2::aes(
        x = x,
        y = y
      ),
      shape = 24,
      size = marker_size,
      fill = "#009F81",
      color = "black"
    )
  }

  # Median
  if (mark_median) {
    myplot <- myplot + ggplot2::geom_point(
      data = data.frame(
        x = rd_median,
        y = top + spacing*2
      ),
      color = "black",
      fill = "#FF5AAF",
      shape = 23,
      size = marker_size,
      ggplot2::aes(
        x = x,
        y = y
      )
    )

    myplot <- myplot + ggplot2::geom_vline(
      xintercept = rd_median,
      color = "#FF5AAF",
      linetype = "solid",
      size = 1.5
    )
  }


  if (mark_sd) {
    mults <- seq(from = -1, to = 1, by = 1)
    sd_df <- data.frame(
      x = rd_mean + mults*rd_sd,
      y = top + spacing
    )
    myplot <- myplot + ggplot2::geom_segment(
      color = "#009F81",
      linetype = "solid",
      ggplot2::aes(
        x = min(sd_df$x),
        xend = max(sd_df$x),
        y = top + spacing,
        yend = top + spacing
      )
    )
    myplot <- myplot + ggplot2::geom_point(
      data = sd_df,
      color = "#009F81",
      shape = 8,
      size = marker_size,
      ggplot2::aes(
        x = x,
        y = y
      )
    )
  }


  if (mark_quartiles) {
    q_df <- data.frame(
      x = c(estimate$overview$q1[1], estimate$overview$q3[1]),
      y = top + spacing*2
    )
    myplot <- myplot + ggplot2::geom_segment(
      color = "#FF5AAF",
      linetype = "solid",
      ggplot2::aes(
        x = estimate$overview$q1[1],
        xend = estimate$overview$q3[1],
        y = top + spacing*2,
        yend = top + spacing*2
      )
    )
    myplot <- myplot + ggplot2::geom_vline(
      color = "#FF5AAF",
      linetype = "solid",
      data = q_df,
      ggplot2::aes(
        xintercept = x
      )
    )

    myplot <- myplot + ggplot2::geom_point(
      data = q_df,
      color = "#FF5AAF",
      fill = "white",
      shape = 23,
      size = marker_size,
      ggplot2::aes(
        x = x,
        y = y
      )
    )
  }


  if (mark_z_lines) {
    zdata$y = top + spacing * 3.5
    myplot <- myplot + ggplot2::geom_text(
      data = zdata,
      ggplot2::aes(
        x=x,
        y=y,
        label=label
      ),
      parse = TRUE
    )

  }



  # Finishing touches ------------------------------------
  myx <- estimate$overview$outcome_variable_name[1]

  if (mark_mean) {
     # myx <- paste("\n", myx)
    ylim[1] <- bottom
  }

  myplot <- myplot + ggplot2::xlab(myx)

  myplot <- myplot + ggplot2::ylab("Frequency")
  if (is.null(ybreaks)) {
    myplot <- myplot + ggplot2::scale_y_continuous(
      expand = c(0, NA)
    )
  } else {
    myplot <- myplot + ggplot2::scale_y_continuous(
      n.breaks = ybreaks,
      expand = c(0, NA)
    )
  }

  if (!is.null(xbreaks)) {
    myplot <- myplot + ggplot2::scale_x_continuous(
      n.breaks = xbreaks
    )
  }


  if (type == "dotplot") {

    myplot <- myplot + ggplot2::theme(
      axis.line.y.left = ggplot2::element_line(color="NA"),
      axis.title.y = ggplot2::element_text(colour = "NA"),
      axis.text.y = ggplot2::element_text(colour = "NA"),
      axis.ticks.y = ggplot2::element_line(colour = "NA")

    )

  }

  myplot <- myplot + ggplot2::coord_cartesian(
    xlim = xlim,
    ylim = ylim,
    expand = FALSE,
    clip = "off"
  )
  # myplot <- myplot +  ggplot2::scale_y_continuous(
  #   limits = ylim,
  #   expand = c(0,NA)
  # )
  # myplot <- myplot + ggplot2::scale_x_continuous(
  #   limits = xlim
  # )
  myplot <- myplot + ggplot2::theme(
    legend.position="none",
    plot.margin = ggplot2::margin(40, 30, 30, 45, "pt")
  )



  return(myplot)

}
