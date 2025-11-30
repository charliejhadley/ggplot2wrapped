# Interactive UpSet chart

`make_geom_interactive_upset_chart()` visualisation function for
creating an interactive UpSet chart showing which visualisations are
used together in code files. Uses the htmlwidgets package upsetjs.

## Usage

``` r
make_geom_interactive_upset_chart(
  data_geom_usage,
  height = NULL,
  min_geom_appearances = 10,
  min_interaction_size = 1
)
```

## Arguments

- data_geom_usage:

  tibble containing geom usage data produced via
  [`add_geom_usage_to_files()`](https://charliejhadley.github.io/ggplot2wrapped/reference/add_geom_usage_to_files.md).

- height:

  height of htmlwidget in pixels

- min_geom_appearances:

  minimum number of times geom needs to

## Value

A tibble.
