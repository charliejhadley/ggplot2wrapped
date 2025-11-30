# Fill geom usage per day

`fill_geom_usage_date_data()` utility function for filling geom usage
per day from the function
[`summarise_per_day()`](https://charliejhadley.github.io/ggplot2wrapped/reference/summarise_per_day.md)

## Usage

``` r
fill_geom_usage_date_data(data_usage_per_day, target_year = 2025)
```

## Arguments

- data_usage_per_day:

  Tibble containing geom usage per day, generated from
  [`summarise_per_day()`](https://charliejhadley.github.io/ggplot2wrapped/reference/summarise_per_day.md)

- target_year:

  Target year. Defaults to 2025 as that's the first year of
  ggplot2wraped!
