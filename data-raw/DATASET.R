## code to prepare `DATASET` dataset goes here
library("tidyverse")
library("ggtext")
library("ggrepel")

get_geoms_from_package <- function(package_name){
  tibble(
    geom_name = ls(paste0("package:", package_name)),
    package_name = package_name
  ) |>
    filter(str_starts(geom_name, "geom_"))
}

vec_ggplot2_packages <- c("ggplot2", "ggrepel", "ggtext")

data_geoms_basic <- vec_ggplot2_packages |>
  map_dfr(~get_geoms_from_package(.x)) |>
  distinct()


get_docs_details <- function(geom_name, package_name, section){

  rd_object <- utils:::.getHelpFile(
    help(geom_name, package = as.character(package_name))
  )

  section_indexes <- map(rd_object, ~attr(.x, "Rd_tag")) |>
    as.character() |>
    setNames(1:length(.), .)

  selected_section_index <- section_indexes[[section]]

  rd_object[[selected_section_index]] |>
    as.character() |>
    paste(collapse = " ") |>
    str_replace_all("list\\(\\\"([^\\\"]*)\\\"\\)", "\\1") |>
    str_replace_all("\n", "") |>
    str_trim()
}

get_docs_details("geom_col", "ggplot2", "\\title")

vectorised_get_docs_details <- Vectorize(get_docs_details)

data_geoms_detailed <- data_geoms_basic |>
  mutate(docs_title = vectorised_get_docs_details(geom_name, package_name, "\\title"),
         docs_description = vectorised_get_docs_details(geom_name, package_name, "\\description"))

data_geoms <- data_geoms_detailed

data_geoms |>
  filter(package_name == "ggplot2") |>
  arrange(geom_name) |>
  select(geom_name) |>
  write_csv("data-raw/geom_groupings.csv")

usethis::use_data(data_geoms, overwrite = TRUE)


# example data ------------------------------------------------------------

library("ggplot2wrapped")

code_files <- get_code_file_info(c("~/Github/", "~/coding/r-projects-scrapbook/"), file_type = c(".qmd", ".R", ".Rmd"))


data_geom_usage_all_files <- code_files |>
  add_geom_usage_to_files()


data_example_geom_usage <- data_geom_usage_all_files |>
  filter(between(year(modified_time), 2021, 2025)) |>
  mutate(modified_time = ymd_hms(paste("2025", month(modified_time), day(modified_time), " ", hour(modified_time), minute(modified_time), second(modified_time))), .before = 1) |>
  mutate(file_path = fct_anon(file_path, prefix = "anonymised file ")) |>
  select(file_path, ends_with("time"), geom_name, has_aes, n_args_in_call, n_times_used, length_of_call, package_name)

usethis::use_data(data_example_geom_usage, overwrite = TRUE)

save(data_geom_usage_example, file = system.file("quarto-reports", "ggplot2-unwrapped-2025_scrollytelling","data_geom_usage_example.RData", package = "ggplot2wrapped"))


# data_geom_usage_2022 |>
#   mutate(group = paste("file", cur_group_id()), .by = file_path)
#
# data_geom_usage_2022 |>
#   mutate(file_path = fct_anon(file_path, prefix = "anonymised file "))





# data_geom_usage_example <- data_geom_usage_2022 |>
  # mutate(file_path = fct_anon(file_path, prefix = "anonymised file ")) |>
  # select(file_path, ends_with("time"), geom_name, has_aes, n_args_in_call, n_times_used, length_of_call, package_name)
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

