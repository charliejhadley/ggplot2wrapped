# Summarise geom usage per day

`summarise_per_day()` utility function for summarising geom usage by
several different metrics

## Usage

``` r
summarise_per_day(data_geom_usage, measure = "per_day_individual_geom_usage")
```

## Arguments

- data_geom_usage:

  Tibble containing geom usage data produced via
  [`add_geom_usage_to_files()`](https://charliejhadley.github.io/ggplot2wrapped/reference/add_geom_usage_to_files.md).

- measure:

  How to summatrise usage by day? Defaaults to
  "per_day_individual_geom_usage", can also be one of;
  "per_day_files_with_geoms", "per_day_unique_geoms",
  "per_day_total_geom_usage"
