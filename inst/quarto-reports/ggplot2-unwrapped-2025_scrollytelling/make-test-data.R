library("tidyverse")
library("ggplot2wrapped")

code_files <- get_code_file_info(c("~/Github/", "~/coding/r-projects-scrapbook/"), file_type = c(".qmd", ".R", ".Rmd"))
target_year <- 2025

data_geom_usage_2022 <- code_files |>
  filter(year(created_time) == target_year) |>
  add_geom_usage_to_files()

data_geom_usage_2022 |>
  mutate(group = paste("file", cur_group_id()), .by = file_path)

data_geom_usage_2022 |>
  mutate(file_path = fct_anon(file_path, prefix = "anonymised file "))





data_geom_usage_example <- data_geom_usage_2022 |>
  mutate(file_path = fct_anon(file_path, prefix = "anonymised file ")) |>
  select(file_path, ends_with("time"), geom_name, has_aes, n_args_in_call, n_times_used, length_of_call, package_name)
#
#
#
# data_test_scrollytelling <- |>
#   mutate(geom_usage = get_geoms_from_code_file(file_path, data_geoms))
#
# data_test_scrollytelling |>
#   unnest(geom_usage) |>
#   filter(!is.na(geom_name)) |>
#   unnest(function_call)

save(data_geom_usage_example, file = system.file("quarto-reports", "ggplot2-unwrapped-2025_scrollytelling","data_geom_usage_example.RData", package = "ggplot2wrapped"))

code_files |>
  filter(year(created_time) == 2024) |>
  add_geom_usage_to_files() |>
  ggplot_wrapped_2025(data_geoms)

