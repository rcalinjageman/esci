esci_plot_mdiff_aesthetics <- function(
  myplot,
  use_ggdist = TRUE,
  plot_paired = FALSE
) {
  # Customize plot -------------------------------
  # No legend
  myplot <- myplot + ggplot2::theme(legend.position = "none")

  # Points
  if (plot_paired) {
    # Points
    myplot <- myplot + ggplot2::scale_shape_manual(
      values = c(
        "Reference_raw" = "circle filled",
        "Comparison_raw" = "circle filled",
        "Difference_raw" = "triangle filled",
        "Unused_raw" = "diamond filled",
        "Reference_summary" = "circle filled",
        "Comparison_summary" = "circle filled",
        "Difference_summary" = "triangle filled",
        "Unused_summary" = "circle filled"
      )
    )
  } else {
    myplot <- myplot + ggplot2::scale_shape_manual(
      values = c(
        "Reference_raw" = "circle filled",
        "Comparison_raw" = "square filled",
        "Difference_raw" = "triangle filled",
        "Unused_raw" = "diamond filled",
        "Reference_summary" = "square filled",
        "Comparison_summary" = "diamond filled",
        "Difference_summary" = "triangle filled",
        "Unused_summary" = "circle filled"
      )
    )
  }

  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_raw" = "black",
      "Comparison_raw" = "black",
      "Difference_raw" = "black",
      "Unused_raw" = "black",
      "Reference_summary" = "black",
      "Comparison_summary" = "black",
      "Difference_summary" = "black",
      "Unused_summary" = "gray80"
    ),
    aesthetics = c("color", "point_color")
  )

  if (plot_paired) {
    myplot <- myplot + ggplot2::scale_fill_manual(
      values = c(
        "Reference_raw" = "NA",
        "Comparison_raw" = "NA",
        "Difference_raw" = "NA",
        "Unused_raw" = "gray80",
        "Reference_summary" = "#0072B2",
        "Comparison_summary" = "#0072B2",
        "Difference_summary" = "#000000",
        "Unused_summary" = "gray80"
      ),
      aesthetics = c("fill", "point_fill")
    )
  } else {
    myplot <- myplot + ggplot2::scale_fill_manual(
      values = c(
        "Reference_raw" = "#E69F00",
        "Comparison_raw" = "#0072B2",
        "Difference_raw" = "NA",
        "Unused_raw" = "gray80",
        "Reference_summary" = "#E69F00",
        "Comparison_summary" = "#0072B2",
        "Difference_summary" = "#000000",
        "Unused_summary" = "gray80"
      ),
      aesthetics = c("fill", "point_fill")
    )
  }

  if (use_ggdist) {
    myplot <- myplot + ggplot2::discrete_scale(
      c("size", "point_size"),
      "point_size_d",
      function(n) return(c(
        "Reference_raw" = 2,
        "Comparison_raw" = 2,
        "Difference_raw" = 2,
        "Unused_raw" = 1,
        "Reference_summary" = 3,
        "Comparison_summary" = 3,
        "Difference_summary" = 3,
        "Unused_summary" = 3
      ))
    )
  } else {
    myplot <- myplot + ggplot2::discrete_scale(
      c("size", "point_size"),
      "point_size_d",
      function(n) return(c(
        "Reference_raw" = 2,
        "Comparison_raw" = 2,
        "Difference_raw" = 2,
        "Unused_raw" = 1,
        "Reference_summary" = 1,
        "Comparison_summary" = 1,
        "Difference_summary" = 1,
        "Unused_summary" = 1
      ))
    )
  }


  myplot <- myplot + ggplot2::discrete_scale(
    c("alpha", "point_alpha"),
    "point_alpha_d",
    function(n) return(c(
      "Reference_raw" = .8,
      "Comparison_raw" = .8,
      "Difference_raw" = .8,
      "Unused_raw" = .5,
      "Reference_summary" = 1,
      "Comparison_summary" = 1,
      "Difference_summary" = 1,
      "Unused_summary" = 1
    ))
  )

  # Error bars
  myplot <- myplot + ggplot2::scale_linetype_manual(
    values = c(
      "Reference_summary" = "solid",
      "Comparison_summary" = "solid",
      "Difference_summary" = "solid",
      "Unused_summary" = "dotted"
    )
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c(
      "Reference_summary" = "black",
      "Comparison_summary" = "black",
      "Difference_summary" = "black",
      "Unused_summary" = "gray"
    ),
    aesthetics = "interval_color"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_alpha",
    "interval_alpha_d",
    function(n) return(c(
      "Reference_summary" = 1,
      "Comparison_summary" = 1,
      "Difference_summary" = 1,
      "Unused_summary" = 1
    ))
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "interval_size",
    "interval_size_d",
    function(n) return(c(
      "Reference_summary" = 2,
      "Comparison_summary" = 2,
      "Difference_summary" = 2,
      "Unused_summary" = 1
    ))
  )

  # Slab
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c(
      "Reference_summary" = "gray",
      "Comparison_summary" = "gray",
      "Difference_summary" = "gray",
      "Unused_summary" = "gray"
    ),
    aesthetics = "slab_fill"
  )
  myplot <- myplot + ggplot2::discrete_scale(
    "slab_alpha",
    "slab_alpha_d",
    function(n) return(c(
      "Reference_summary" = 1,
      "Comparison_summary" = 1,
      "Difference_summary" = 1,
      "Unused_summary" = 1
    ))
  )

  return(myplot)

}


esci_plot_simple_aesthetics <- function(myplot, use_ggdist = TRUE) {
  # Customize plot -------------------------------
  # No legend

  ggplot_version <- as.numeric(gsub("\\.", "", utils::packageVersion("ggplot2")))



  myplot <- myplot + ggplot2::theme(legend.position = "none")

  # Points
  myplot <- myplot + ggplot2::scale_shape_manual(
    values = c("raw" = "circle filled", "summary" = "circle filled")
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c("raw" = "black", "summary" = "black"),
    aesthetics = c("color", "point_color")
  )
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c("raw" = "NA", "summary" = "gray"),
    aesthetics = c("fill", "point_fill")
  )

  if (use_ggdist) {
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = c("size", "point_size"),
      scale_name = if (ggplot_version >= 350) NULL else "point_size_d",
      palette = function(n) return(c("raw" = 1, "summary" = 3))
    )

  } else {

    if (ggplot_version >= 350) {
      myplot <- myplot + ggplot2::discrete_scale(
        aesthetics = c("size"),
        palette = function(n) return(c("raw" = 1, "summary" = 1))
      )

    } else {
      myplot <- myplot + ggplot2::discrete_scale(
        aesthetics = c("size"),
        scale_name = "point_size_d",
        palette = function(n) return(c("raw" = 1, "summary" = 1))
      )

    }

  }

  if (ggplot_version >= 350) {
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = c("alpha", "point_alpha"),
      palette = function(n) return(c("raw" = 0.8, "summary" = 1))
    )
  } else {
    myplot <- myplot + ggplot2::discrete_scale(
      c("alpha", "point_alpha"),
      "point_alpha_d",
      function(n) return(c("raw" = 0.8, "summary" = 1))
    )
  }


  # Error bars
  myplot <- myplot + ggplot2::scale_linetype_manual(
    values = c("summary" = "solid")
  )
  myplot <- myplot + ggplot2::scale_color_manual(
    values = c("summary" = "black"),
    aesthetics = "interval_color"
  )

  if (ggplot_version >= 350) {
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = "interval_alpha",
      palette = function(n) return(c("summary" = 1))
    )
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = "interval_size",
      palette = function(n) return(c("summary" = 3))
    )

  } else {

    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = "interval_alpha",
      scale_name = "interval_alpha_d",
      palette = function(n) return(c("summary" = 1))
    )
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = "interval_size",
      scale_name = "interval_size_d",
      palette = function(n) return(c("summary" = 3))
    )

  }


  # Slab
  myplot <- myplot + ggplot2::scale_fill_manual(
    values = c("summary" = "gray"),
    aesthetics = "slab_fill"
  )

  if (ggplot_version >= 350) {
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = "slab_alpha",
      palette = function(n) return(c("summary" = 1))
    )

  } else {
    myplot <- myplot + ggplot2::discrete_scale(
      aesthetics = "slab_alpha",
      scale_name = "slab_alpha_d",
      palette = function(n) return(c("summary" = 1))
    )

  }

  return(myplot)
}
