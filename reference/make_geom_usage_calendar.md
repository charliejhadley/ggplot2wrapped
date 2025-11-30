# geom usage calendar

`make_geom_usage_calendar()` generates a ggplot2 chart showing which
geoms are used when, it is inspired by the GitHub commit chart.

## Usage

``` r
make_geom_usage_calendar(
  data_geom_usage,
  measure = "per_day_individual_geom_usage"
)
```

## Arguments

- data_geom_usage:

  Tibble containing geom usage data produced via
  [`add_geom_usage_to_files()`](https://charliejhadley.github.io/ggplot2wrapped/reference/add_geom_usage_to_files.md).
  files

- measure:

  How should geom use be summarised? Defaults to
  "per_day_individual_geom_usage", can also be;
  "per_day_files_with_geoms", "per_day_unique_geoms",
  "per_day_total_geom_usage"

## Value

A tibble.
