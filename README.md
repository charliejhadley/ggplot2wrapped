
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggplot2wrapped

<!-- badges: start -->

<!-- badges: end -->

The goal of ggplot2wrapped is to …

## Installation

You’ll need this other GitHub only package

``` r
# remotes::install_github("visibledata/GPCDStools")
```

``` r
library(ggplot2wrapped)
library(tidyverse)
#> Warning: package 'ggplot2' was built under R version 4.5.2
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.2.0     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

Use this to collect file information. This is useful to make sure you
don’t just include EVERYTHING

``` r
code_files <- get_code_file_info(c("~/Github/", "~/coding/r-projects-scrapbook/"), file_type = c(".qmd", ".R", ".Rmd"))
```

The report assumes you are looking at just one year’s worth of data and
it’s 2025 so we filter for files created in 2025. It could also take a
long time if you have lots of code files, so feel free to look at a
different year.

Note that the value returned from this object contains **sensitive
information** in the `function_call` argument.

``` r
target_year <- 2025
data_geom_usage_2025 <- code_files |>
  add_geom_usage_to_files()
```

You can build your report using this function!

``` r
data_geom_usage_2025 %>% 
  ggplot_wrapped_2025()
```

Or if that doesn’t work there are lots of lovely functions for
visualising your data

``` r
make_geom_interactive_upset_chart(data_geom_usage_2025, height = 600)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
