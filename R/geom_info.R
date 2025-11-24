#' Get geoms from a package
#'
#' `get_geoms_from_package()` returns a data.frame containing all geoms from a
#' package.
#'
#' @section What counts as a geom?
#'
#' For our purposes a geom function starts with "geom_" and that's it
#'
#' @param package_name Package name, as a string
#' @returns A tibble.
#' @export
get_geoms_from_package <- function(package_name){
  tibble(
    geom_name = ls(paste0("package:", package_name)),
    package_name = package_name
  ) %>%
    filter(str_starts(geom_name, "geom_"))
}

#' Get docs details
#'
#' `get_docs_details()` extracts sections of a function documentation page as
#' text
#'
#' @section What counts as a geom?
#'
#' For our purposes a geom function starts with "geom_" and that's it
#'
#' @param get_docs_details Name of geom
#' @param package_name Package name
#' @param section Section heading of docs page
#' @returns A tibble.
#' @export

get_docs_details <- function(geom_name, package_name, section){

  rd_object <- utils:::.getHelpFile(
    help(geom_name, package = as.character(package_name))
  )

  section_indexes <- map(rd_object, ~attr(.x, "Rd_tag")) %>%
    as.character() %>%
    setNames(1:length(.), .)

  selected_section_index <- section_indexes[[section]]

  rd_object[[selected_section_index]] %>%
    as.character() %>%
    paste(collapse = " ") %>%
    str_replace_all("list\\(\\\"([^\\\"]*)\\\"\\)", "\\1") %>%
    str_replace_all("\n", "") %>%
    str_trim()
}
