# Interactive pie chart of geom usage

`make_geom_usage_pies_highcharts()` produces an interactive pie chart
showing your top 4 geom usage across all calls or files

## Usage

``` r
make_geom_usage_pies_highcharts(
  data_summary_geom_calls,
  pie_measure_type = c("geom_total_usage", "geom_usage_in_files")
)
```

## Arguments

- pie_measure_type:

  How should pie chart summarise geom used? Default to
  "geom_total_usage", can also be "geom_usage_in_files"

- data_geom_usage:

  Tibble containing geom usage data produced via
  [`add_geom_usage_to_files()`](https://charliejhadley.github.io/ggplot2wrapped/reference/add_geom_usage_to_files.md).

## Value

A ggplot2 chart.
