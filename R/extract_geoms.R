#' add_geom_usage_to_files
#'
#' `add_geom_usage_to_files()` is the main function you'll use to add geom usage
#' data to your files.
#'
#' @export
add_geom_usage_to_files <- function(
  data_file_info,
  data_geoms = ggplot2wrapped::data_geoms
) {
  data_file_info |>
    dplyr::mutate(
      geom_usage = get_geoms_from_code_file(file_path, data_geoms)
    ) |>
    tidyr::unnest(geom_usage) |>
    dplyr::filter(!is.na(geom_name)) |>
    dplyr::mutate(file_extension = tools::file_ext(file_path), .after = 1)
}


#' Read code from file
#'
#' Helper function to read R code from a file, handling .Rmd and .qmd files
#'
#' @param file_path Path to a code file (.R, .Rmd, or .qmd)
#' @returns A character string containing the R code from the file
#' @keywords internal
read_code_from_file <- function(file_path) {
  file_ext <- tools::file_ext(file_path)

  if (file_ext %in% c("Rmd", "qmd")) {
    tmp_r <- tempfile(fileext = ".R")
    knitr::purl(file_path, output = tmp_r, quiet = TRUE)
    code_file <- paste0(readLines(tmp_r, warn = FALSE), collapse = "\n")
    unlink(tmp_r)
    code_file
  } else {
    paste0(readLines(file_path, warn = FALSE), collapse = "\n")
  }
}

#' Find geom calls
#'
#' Helper function to recursively find geom function calls in parsed R code
#'
#' @param expr A parsed R expression (from base::parse())
#' @param geom_names Character vector of geom names to search for
#' @returns A named list where names are geom names and values are lists of
#'   character strings containing the full text of each geom call
#' @keywords internal
find_geom_calls_in_parsed_code <- function(expr, geom_names) {
  # Initialize results list
  results <- stats::setNames(vector("list", length(geom_names)), geom_names)

  # Recursive helper function
  find_calls_recursive <- function(e) {
    if (is.call(e)) {
      # Check if this is a function call we're looking for
      func_name <- as.character(e[[1]])
      if (func_name %in% geom_names) {
        # Deparse to get the full text of the call
        call_text <- paste(deparse(e), collapse = " ")
        # Add to results
        results[[func_name]] <<- c(results[[func_name]], call_text)
      }
      # Recursively search within this call's arguments
      for (i in seq_along(e)) {
        find_calls_recursive(e[[i]])
      }
    } else if (is.list(e) || is.expression(e)) {
      # Recursively search within list/expression
      for (item in e) {
        find_calls_recursive(item)
      }
    }
  }

  # Start the recursive search
  find_calls_recursive(expr)

  # Convert to list of lists with names
  lapply(results, function(x) {
    if (length(x) == 0) {
      list()
    } else {
      stats::setNames(as.list(x), paste0("node_", seq_along(x)))
    }
  })
}

#' get_geoms_from_code_file
#'
#' `get_geoms_from_code_file()` extracts and counts geom usage from a code file.
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
#' @section Using Abstract Syntax Trees
#'
#' This function parses the abstract syntax tree of code files using
#' `base::parse()`. This ensures that `geom_point()` is recognised as a geom
#' function call whereas "I like geom_point()" is not. Compare this with
#' `extract_geom_arguments()`
#'
#' @param file_path Path to a code file.
#' @param geoms_dataset A data.frame containing a column 'geom_name' with the
#' name of a geom of interest.
#' @returns A tibble.
#' @export
get_geoms_from_code_file <- function(file_path, geoms_dataset) {
  # Vectorisation utility function
  get_geoms_from_code_file_singular <- function(file_path) {
    code_file <- read_code_from_file(file_path)

    parsed_code <- parse(text = code_file)

    # Get list of geom names to search for
    vec_geoms <- dplyr::pull(geoms_dataset, geom_name)

    geom_calls <- find_geom_calls_in_parsed_code(parsed_code, vec_geoms)

    data_geom_usage <- geom_calls |>
      tibble::enframe(name = "geom_name", value = "geom_function_calls") |>
      tidyr::unnest(geom_function_calls) |>
      tidyr::unnest(geom_function_calls) |>
      dplyr::mutate(
        function_call = extract_geom_arguments(geom_function_calls)
      ) |>
      dplyr::mutate(
        length_of_call = stringr::str_length(stringr::str_remove(
          geom_function_calls,
          geom_name
        )) -
          2
      ) |>
      dplyr::mutate(n_args_in_call = purrr::map_dbl(function_call, nrow)) |>
      dplyr::mutate(
        has_aes = purrr::map_lgl(function_call, ~ any(.x[["is_aes"]])),
        .after = geom_name
      ) |>
      dplyr::select(-geom_function_calls) |>
      dplyr::mutate(n_times_used = dplyr::n(), .by = geom_name) |>
      dplyr::left_join(
        dplyr::select(geoms_dataset, geom_name, package_name),
        by = c("geom_name")
      )

    if (nrow(data_geom_usage) == 0) {
      return(dplyr::add_row(data_geom_usage))
    } else {
      return(data_geom_usage)
    }
  }

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
extract_geom_arguments <- function(geom_call) {
  # Vectorize
  extract_geom_arguments_singular <- function(geom_call) {
    expr <- parse(text = geom_call)

    call_list <- as.list(expr[[1]])
    args_list <- call_list[-1]

    raw_args_df <- args_list |>
      tibble::enframe(name = "argument_name", value = "argument_value") |>
      dplyr::mutate(argument_number = dplyr::row_number()) |>
      dplyr::mutate(
        argument_value = as.character(argument_value),
        argument_name = as.character(argument_name)
      )

    # remove positional arg names
    raw_args_df <- raw_args_df |>
      dplyr::mutate(
        argument_name = dplyr::if_else(
          stringr::str_detect(argument_name, "^[0-9]{1,}"),
          "",
          argument_name
        )
      )

    args_without_aes <- raw_args_df |>
      dplyr::filter(argument_name != "aes") |>
      dplyr::filter(
        !(argument_name == "" &
          stringr::str_detect(argument_value, "^aes[(]"))
      ) |>
      dplyr::mutate(is_aes = 0)

    args_aes <- raw_args_df |>
      dplyr::filter(
        (argument_name == "" &
          stringr::str_detect(argument_value, "^aes[(]")) |
          argument_name == "aes"
      ) |>
      dplyr::mutate(is_aes = 1)

    args_all <- dplyr::bind_rows(args_without_aes, args_aes) |>
      dplyr::mutate(is_aes = as.logical(is_aes)) |>
      dplyr::select(is_aes, everything())

    args_all
    # tibble(arg_details = args_all, aes_provided = ifelse(nrow(args_aes) > 0, TRUE, FALSE))
  }

  purrr::map(geom_call, ~ extract_geom_arguments_singular(.x))
}
