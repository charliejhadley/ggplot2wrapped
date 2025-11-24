#' get_geoms_from_code_file
#'
#' `get_geoms_from_code_file()` uses `{astgrepr}` to both extract and count geom
#' usage from a code file.
#'
#' @section Providing a good geoms_dataset
#'
#' This package only looks for the geoms specified in the `data.frame` provided
#' to `geoms_dataset`. There are a few reasons we don't just search for "all" geoms
#'
#' * There are data security concerns from including your custom geoms. The names
#' of the geoms could realistically include sensitive information
#'
#' * This is a fun package put together in a few days
#'
#' The package includes `data_geoms` which contains all of the geoms exported by:
#' ggplot2, ggrepel and ggtext. You can easily augment this dataset with your own
#' geoms. In future releases we plan to have greater support for "custom geoms"
#'
#' @section Using astgrepr
#'
#' This function uses the excellent `{astgrepr}` package to parse the abstract
#' syntax tree of code files. You want to do this to ensure that `geom_point()`
#' is recognised as a geom whereas "I like geom_point()" is not. Compare this
#' with `extract_geom_arguments()`
#'
#' @param file_path Path to a code file.
#' @param geoms_dataset A data.frame containing a column 'geom_name' with the
#' name of a geom of interest.
#' @returns A tibble.
#' @export
get_geoms_from_code_file <- function(file_path, geoms_dataset){

  # Vectorisation utility function
  get_geoms_from_code_file_singular <- function(file_path){

  code_file <- paste0(readLines(file_path),collapse = "\n")

  root <- code_file |>
    astgrepr::tree_new() |>
    astgrepr::tree_root()

  vec_geoms <- dplyr::pull(geoms_dataset, geom_name)


  geoms_rule_list <- vec_geoms |>
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


  data_geom_usage <- data_nodes |>
    astgrepr::node_text_all() |>
    tibble::enframe(name = "geom_name", value = "geom_function_calls") |>
    tidyr::unnest(geom_function_calls) |>
    tidyr::unnest(geom_function_calls) |>
    mutate(function_call = extract_geom_arguments(geom_function_calls)) |>
    mutate(n_args_in_call = map_dbl(function_call, nrow)) |>
    mutate(has_aes = map_lgl(function_call, ~any(.x[["is_aes"]]))) |>
    select(-geom_function_calls) |>
    dplyr::mutate(n_times_used = dplyr::n(), .by = geom_name) |>
    dplyr::left_join(geoms_dataset,
              by = c("geom_name"))

  ## Create single row tibble if no geoms found
  if(nrow(data_geom_usage) == 0){
    return(dplyr::add_row(data_geom_usage))
  } else {
    return(data_geom_usage)
  }

  }

  # Vectorisation achieved
  purrr::map(file_path, function(x) get_geoms_from_code_file_singular(x))

}

#' extract_geom_arguments
#'
#' `extract_geom_arguments()` extracts and labels arguments as aes() or not from
#'  a single geom call
#'
#' @section Using parse
#'
#' This function is intended to be called within `get_geoms_from_code_file()`
#' and is guaranteed to be provided a singular call of a geom, eg
#' `geom_col(colour = "red")`. The function can therefore use `base::parse()` to
#' extract the arguments, unlike the parent function which requires full on
#' Abstract Syntax Trees.
#'
#' @param geom_call A single call of a geom, provided as a string.
#'
#' @export
extract_geom_arguments <- function(geom_call){

  # Vectorize
  extract_geom_arguments_singular <- function(geom_call){

  expr <- parse(text = geom_call)

  call_list <- as.list(expr[[1]])
  args_list <- call_list[-1]

  raw_args_df <- args_list |>
    tibble::enframe(name = "argument_name",
            value = "argument_value") |>
    dplyr::mutate(argument_number = row_number()) |>
    dplyr::mutate(argument_value = as.character(argument_value),
           argument_name = as.character(argument_name))

  # remove positional arg names
  raw_args_df <- raw_args_df |>
    dplyr::mutate(argument_name = dplyr::if_else(stringr::str_detect(argument_name, "^[0-9]{1,}"), "", argument_name))


  args_without_aes <- raw_args_df |>
    dplyr::filter(argument_name != "aes") |>
    dplyr::filter(!(argument_name == "" &
           stringr::str_detect(argument_value, "^aes[(]"))) |>
    dplyr::mutate(is_aes = 0)

  args_aes <- raw_args_df |>
    dplyr::filter((argument_name == "" &
               stringr::str_detect(argument_value, "^aes[(]")) | argument_name == "aes") |>
    dplyr::mutate(is_aes = 1)

  args_all <- dplyr::bind_rows(args_without_aes,
            args_aes) |>
    dplyr::mutate(is_aes = as.logical(is_aes)) |>
    dplyr::select(is_aes, everything())

  args_all
  # tibble(arg_details = args_all, aes_provided = ifelse(nrow(args_aes) > 0, TRUE, FALSE))

  }

  purrr::map(geom_call, ~extract_geom_arguments_singular(.x))
}

