#' Get file info
#'
#' `get_code_file_info()` provides a formatted tibble containg file info about
#' your selected code files.
#'
#' @param paths A singular path or a vector of file paths to look in for code
#' files
#' @param file_types Which R code files to look at. Defaults to R, Quarto and
#' RMarkdown.
#' @returns A tibble.
#' @export
get_code_file_info <- function(paths, file_type = c(".R", ".qmd", "Rmd")) {
  if (!all(purrr::map_lgl(paths, file.exists))) {
    cli::cli_abort(c(
      "{.var paths} must all be valid paths, at least one is invalid"
    ))
  }

  pattern_file_types <- paste0(
    purrr::map(file_type, ~ paste0("(\\", .x, "$)")),
    collapse = "|"
  )

  vec_code_files <- paths |>
    purrr::map(
      ~ list.files(
        .x,
        recursive = TRUE,
        full.names = TRUE,
        pattern = pattern_file_types
      )
    ) |>
    unlist()

  dplyr::tibble(
    file_path = vec_code_files
  ) |>
    dplyr::mutate(file_info = file.info(file_path)) |>
    tidyr::unnest(file_info) |>
    dplyr::select(
      file_path,
      modified_time = "mtime",
      created_time = "ctime",
      access_time = "atime"
    )
}
