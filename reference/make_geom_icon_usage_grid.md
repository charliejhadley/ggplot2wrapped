# Grid of geom groupings and their usage

`make_geom_icon_usage_grid()` creates a ggplot2 with a grid of geom
groupings from the official ggplot2 charts, optionally highlighting
which geoms are used.

## Usage

``` r
make_geom_icon_usage_grid(data_geom_usage, show_which_geoms = "used")
```

## Arguments

- data_geom_usage:

  Tibble containing geom usage data produced via
  [`add_geom_usage_to_files()`](https://charliejhadley.github.io/ggplot2wrapped/reference/add_geom_usage_to_files.md).

- show_which_geoms:

  Which geoms are shown? Default is "used" which highlights only used
  geoms. Providing "all" will show all geoms. RMarkdown.

## Value

A ggplot2 chart
