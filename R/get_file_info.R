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
get_code_file_info <- function(paths, file_type = c(".R", ".qmd", "Rmd")){

  pattern_file_types <- paste0(map(file_type, ~paste0("(\\", .x, "$)")), collapse = "|")

  vec_code_files <- paths %>%
    map(~list.files(.x, recursive = TRUE, full.names = TRUE, pattern = pattern_file_types)) %>%
    unlist()

  tibble(
    file_path = vec_code_files
  ) %>%
    mutate(file_info = file.info(file_path)) %>%
    unnest(file_info) %>%
    select(file_path,
           modified_time = "mtime",
           created_time = "ctime",
           access_time = "atime"
    )
}
