# Interactive aes usaege chart

`make_aes_type_percentile_highcharts()` interactive bar chart showing
geoms that use both dataviz-level and chart-level aesthetics and their
relative %

## Usage

``` r
make_aes_type_percentile_highcharts(data_geom_usage)
```

## Arguments

- data_geom_usage:

  Tibble containing geom usage data produced via
  [`add_geom_usage_to_files()`](https://charliejhadley.github.io/ggplot2wrapped/reference/add_geom_usage_to_files.md).

## Value

A ggplot2 chart
