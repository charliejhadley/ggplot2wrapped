# Get file info

`get_code_file_info()` provides a formatted tibble containg file info
about your selected code files.

## Usage

``` r
get_code_file_info(paths, file_type = c(".R", ".qmd", "Rmd"))
```

## Arguments

- paths:

  A singular path or a vector of file paths to look in for code files

- file_types:

  Which R code files to look at. Defaults to R, Quarto and RMarkdown.

## Value

A tibble.
