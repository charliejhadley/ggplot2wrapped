# Get file info

`make_geom_usage_calendar()` provides a formatted tibble containg file
info about your selected code files. Heavily copied from
https://restateinsight.com/posts/general-posts/2024-12-github-contributions-plot/

## Usage

``` r
make_geom_usage_calendar(
  data_geom_usage,
  measure = c("per_day_individual_geom_usage", "per_day_files_with_geoms",
    "per_day_unique_geoms", "per_day_total_geom_usage")
)
```

## Arguments

- paths:

  A singular path or a vector of file paths to look in for code files

- file_types:

  Which R code files to look at. Defaults to R, Quarto and RMarkdown.

## Value

A tibble.
