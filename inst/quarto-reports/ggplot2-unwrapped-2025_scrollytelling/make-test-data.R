library("tidyverse")
library("ggplot2wrapped")

code_files <- get_code_file_info(c("~/Github/", "~/coding/r-projects-scrapbook/"), file_type = c(".qmd", ".R", ".Rmd"))

target_year <- 2025
data_test_scrollytelling <- code_files |>
  filter(year(created_time) == target_year) |>
  mutate(geom_usage = get_geoms_from_code_file(file_path, data_geoms))


save(data_test_scrollytelling, file = "inst/quarto-reports/ggplot2-unwrapped-2025_scrollytelling/data_test_scrollytelling.RData")
