# Meta-analysis diamond

`geom_meta_diamond_h` creates a horizontal meta-analytic diamond defined
by an `x` value (horizontal center of diamond), `xmin` and `xmax` values
(for the horizontal ends of the diamond), a `y` value (for the vertical
placement of the diamond) and a `height` (for the vertical height of the
diamond). Note the use of `xmin` and `xmax` allows for representation of
asymmetric confidence intervals with this geom.

## Usage

``` r
geom_meta_diamond_h(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed to the geom. These are often aesthetics, used
  to set an aesthetic to a fixed value, like `colour = "red"` or
  `linewidth = 3` (see **Aesthetics**, below).

  \##Aesthetics \## `geom_meta_diamond_h` understands the following
  aesthetics (required are in bold):

  - **`x`** - The horizontal center of the diamond

  - **`y`** - The vertical placement of the diamond

  - **`xmin`** - The left-side start of the diamond

  - **`xmax`** - The right-side start of the diamond

  - **`height`** - The vertical span of the diamond

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Examples

``` r
# example code
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.4.3

my_effects <- data.frame(
  effect_size = c(1, 2, 1, 0),
  UL = c(2, 3, 2, 1),
  LL = c(0, 1, 0, -1),
  y = c(1, 2, 3, 4)
)


myplot <- ggplot2::ggplot()
myplot <- myplot + geom_meta_diamond_h(
  data = my_effects,
  ggplot2::aes(
    x = effect_size,
    xmin = LL,
    xmax = UL,
    y = y
  ),
  height = 0.25,
  color = "black",
  fill = "red",
)

```
