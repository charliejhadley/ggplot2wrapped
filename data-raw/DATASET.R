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
