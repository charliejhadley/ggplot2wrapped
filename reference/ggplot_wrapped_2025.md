# Get file info

[`get_code_file_info()`](https://charliejhadley.github.io/ggplot2wrapped/reference/get_code_file_info.md)
provides a formatted tibble containg file info about your selected code
files.

## Usage

``` r
ggplot_wrapped_2025(
  data_geom_usage,
  data_geom_details = ggplot2wrapped::data_geoms,
  report_type = c("scrollytelling"),
  report_year = 2025,
  export_path = ".",
  exported_report_name = "your-ggplot2-wrapped.html",
  overwrite = TRUE
)
```

## Arguments

- paths:

  A singular path or a vector of file paths to look in for code files

- file_types:

  Which R code files to look at. Defaults to R, Quarto and RMarkdown.

## Value

A tibble.
