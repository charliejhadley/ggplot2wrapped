# Get file info

[`get_code_file_info()`](https://charliejhadley.github.io/ggplot2wrapped/reference/get_code_file_info.md)
provides a formatted tibble containg file info about your selected code
files.

## Usage

``` r
make_geom_icon_usage_grid(data_geom_usage, show_which_geoms = c("used", "all"))
```

## Arguments

- paths:

  A singular path or a vector of file paths to look in for code files

- file_types:

  Which R code files to look at. Defaults to R, Quarto and RMarkdown.

## Value

A tibble.
