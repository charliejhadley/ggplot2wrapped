#' Get geom usage from a file
#'
#' `get_geoms_from_code_file()` uses `{astgrepr}` to both extract and count geom
#' usage from a code file.
#'
#' @param file_path Path to a code file.
#' @param geoms_dataset A data.frame containing a column 'geom_name' with the name of a geom of interest.
#' @returns A tibble.
#' @export
get_geoms_from_code_file <- function(file_path, geoms_dataset){

  # Vectorisation utility function
  get_geoms_from_code_file_singular <- function(file_path){

  code_file <- paste0(read_lines(file_path),collapse = "\n")

  root <- code_file |>
    astgrepr::tree_new() |>
    astgrepr::tree_root()

  vec_geoms <- dplyr::pull(geoms_dataset, geom_name)


  geoms_rule_list <- vec_geoms %>%
    purrr::map(~ {
      # The pattern "geom_name($$$A)" matches the geom call regardless of arguments.
      pattern_str <- paste0(.x, "($$$A)")

      astgrepr::ast_rule(
        id = .x,
        pattern = pattern_str
      )
    })

  data_nodes <- do.call(
    astgrepr::node_find_all,
    c(list(root), geoms_rule_list)
  )

  data_nodes %>%
    astgrepr::node_text_all() %>%
    tibble::enframe(name = "geom_name", value = "geom_function_calls") %>%
    dplyr::left_join(geoms_dataset,
              by = c("geom_name")) %>%
    dplyr::mutate(n_times_used = lengths(geom_function_calls))

  }

  # Vectorisation achieved
  purrr::map(file_path, function(x) get_geoms_from_code_file_singular(x))

}
