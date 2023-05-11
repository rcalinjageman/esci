# This function is for formatting the floating axis on difference plots
esci_scaleFUN <- function(x) sprintf("%.2f", x)


# This function helps with plotting sampling error in esci graphs
#  The user provides a short/user-friendly name for the style of
#  plotting sampling error, and this maps it onto the specific
#  geom from ggdist
# If no friendly name is passed or the friendly name is not
#  recognized, point_interval is returned, which does not plot sampling error
esci_plot_error_layouts <- function(error_layout = "none") {

  # Mapping of friendly names to ggdist geoms
  error_layouts <- list(
    halfeye = "ggdist::stat_dist_halfeye",
    eye = "ggdist::stat_dist_eye",
    gradient = "ggdist::stat_dist_gradientinterval",
    none = "ggdist::stat_dist_pointinterval"
  )

  # Handle if friendly name not on list
  if(!error_layout %in% names(error_layouts)) {error_layout <- "none"}

  # Return appropriate ggdist geom
  return(error_layouts[[error_layout]])
}


# Same as above, but in this case, maps friendly names for styles of
#  plotting raw data to different geoms in ggplot2, ggbeeswarm, and ggdist
esci_plot_data_layouts <- function(data_layout = "none", data_spread){

  # Mapping of friendly names to geoms for plotting raw data
  data_layouts <- list(
    swarm = "ggbeeswarm::position_beeswarm",
    random = "ggbeeswarm::position_quasirandom",
    none = NULL
  )

  extra_options <- list(
    swarm = paste(", cex = ", data_spread * 10, sep = ""),
    random = paste(", varwidth = TRUE, width = ", data_spread, sep = ""),
    none = NULL
  )

  # Handle if friendly name not on list
  if(!data_layout %in% names(data_layouts)) {data_layout <- "none"}

  res <- list()
  res$call <- data_layouts[[data_layout]]
  res$extras <- extra_options[[data_layout]]

  # Return appropriate ggdist geom
  return(res)
}


esci_plot_raw_data <- function(myplot, data_layout = "none", data_spread) {

  raw_glue <-
    "
    myplot <- myplot + ggplot2::geom_point(
      data = rdata,
      ggplot2::aes(
        x = x_value,
        y = y_value,
        color = type,
        shape = type,
        fill = type,
        alpha = type,
        size = type
      ),
      position = {raw_call$call}(
        groupOnX = TRUE,
        {raw_call$extras}
      )
    )
    "

  raw_call <- esci_plot_data_layouts(data_layout, data_spread)
  raw_expression <- parse(text = glue::glue(raw_glue))
  return(raw_expression)
}

esci_plot_group_data <- function(effect_size) {

  error_glue <- NULL

  if (effect_size == "mean" | effect_size == "mean_difference") {

    error_glue <-
      "
      myplot <- myplot + {error_call}(
        data = gdata,
        orientation = 'vertical',
        ggplot2::aes(
          x = x_value + nudge,
          y = y_value,
          color = type,
          shape = type,
          size = type,
          point_color = type,
          point_fill = type,
          point_size = type,
          point_alpha = type,
          linetype = type,
          interval_color = type,
          interval_size = type,
          interval_alpha = type,
          slab_fill = type,
          slab_alpha = type,
          slab_linetype = type,
          dist = distributional::dist_student_t(
            df = df,
            mu = y_value,
            sigma = SE
          )
        ),
      scale = {error_scale},
      .width = c({conf_level}),
      normalize = '{error_normalize}'
      )
    "
  }

  if (effect_size == "r") {
    error_glue <-
      "
     myplot <- myplot + {error_call}(
      data = gdata,
      ggplot2::aes(
        x = x_value + nudge,
        y = y_value,
        color = type,
        shape = type,
        size = type,
        point_color = type,
        point_fill = type,
        point_size = type,
        point_alpha = type,
        linetype = type,
        interval_color = type,
        interval_size = type,
        interval_alpha = type,
        slab_fill = type,
        slab_alpha = type,
        slab_linetype = type,
        dist = distributional::dist_transformed(
          distributional::dist_normal(
            mu = esci_trans_r_to_z(effect_size),
            sigma = esci_trans_rse_to_sez(n)
          ),
          transform = esci_trans_z_to_r,
          inverse = esci_trans_identity
        )
      ),
      scale = {error_scale},
      .width = c({conf_level})
    )
    "
  }


  if (is.null(error_glue)) {
    error_glue <-
      "

    myplot <- myplot + ggplot2::geom_pointrange(
      data = gdata,
      ggplot2::aes(
        x = x_value + nudge,
        y = y_value,
        ymin = LL,
        ymax = UL,
        size = type,
        linetype = type,
        shape = type,
        colour = type,
        fill = type,
        alpha = type
      ),
      fatten = 3
    )

    "
  }


  return(error_glue)
}


esci_plot_layers <- function(myplot, layer_name) {
  if (length(myplot$layers) == 0) return(myplot)

  if (is.null(names(myplot$layers))) {
      names(myplot$layers) <- seq(1:length(myplot$layers))
  }

  names(myplot$layers)[length(myplot$layers)] <- layer_name

  return(myplot)

}
