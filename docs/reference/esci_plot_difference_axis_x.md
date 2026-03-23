# Add a difference axis to the x axis of an esci forest plot

`esci_plot_difference_axis_x` can be used to redraw the difference axis
from a forest plot created with
[plot_meta](https://rcalinjageman.github.io/esci/reference/plot_meta.md).
You must pass the plot returned from plot_meta and the effect size table
containing the estimated difference.

## Usage

``` r
esci_plot_difference_axis_x(
  myplot,
  difference_table,
  dlim = c(NA, NA),
  d_n.breaks = NULL,
  d_lab = NULL
)
```

## Arguments

- myplot:

  required ggplot2 plot returned from a plot_meta function

- difference_table:

  required data frame from an esci-estimate that has a difference-based
  effect size

- dlim:

  Optional 2-item vector to provide the lower and upper boundaries of
  the difference axis. Defaults to c(NA, NA) which is to auto-set both
  boundaries.

- d_n.breaks:

  Optional numeric \> 2 to suggest number of breaks for the difference
  axis; defaults to NULL in which case number of breaks is handled
  automatically by ggplot

- d_lab:

  Optional character to serve as the label for the difference axis;
  defaults to NULL
