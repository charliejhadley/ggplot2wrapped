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
ggplot_wrapped_2025 <- function(paths, file_type = c(".R", ".qmd", "Rmd"), report_type = c("scrollytelling"),
                                year_wrapped = 2025,
                                file_action = "created",
                                exported_report = "your-ggplot2-wrapped.html"){

  if(all(purrr::map_lgl(c("~/Github", "foo"), file.exists))){

    cli::cli_abort(c(
      "{.var paths} must all be valid paths, at least one is invalid"
    ))

  }

  code_files <- get_code_file_info(paths, file_type = c(".qmd", ".R", ".Rmd"))

  if(nrow(code_files) == 0){

    cli::cli_abort(c(
      "We di"
    ))

  }

  data_target_year_geoms_raw <- code_files |>
    dplyr::filter(year(created_time) == year_wrapped) |>
    dplyr::mutate(geom_usage = get_geoms_from_code_file(file_path, data_geoms))

  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)

  save(data_target_year_geoms_raw, file = file.path(temp_dir,"FIND ME.RData"))

  quarto::quarto_render(input = "ggplot2-unwrapped-2025_scrollytelling .qmd",
                        output_file = "CHARLIE REPORT.html")

  on.exit(unlink(temp_dir), add = TRUE)

}
