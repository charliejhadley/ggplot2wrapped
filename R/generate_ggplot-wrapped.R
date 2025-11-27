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
ggplot_wrapped_2025 <- function(data_geom_usage,
                                data_geom_details = ggplot2wrapped::data_geoms,
                                report_type = c("scrollytelling"),
                                export_path = ".",
                                exported_report_name = "your-ggplot2-wrapped.html",
                                overwrite = TRUE){

  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE)

  data_geom_usage <- data_geom_usage
  data_geom_details <- data_geom_details

  print(data_geom_details)

  data_geom_usage |>
    write_csv(file.path(temp_dir, "data_geom_usage.csv"))

  data_geom_details |>
    write_csv(file.path(temp_dir, "data_geom_details.csv"))

#
#   save(data_geom_usage, file = file.path(temp_dir,"data_geom_usage.RData"))
#   save(data_geom_details, file = file.path(temp_dir,"data_geom_details.RData"))

  print("JUST HERE")

# quarto::quarto_render(input = system.file("quarto-reports", "ggplot2-unwrapped-2025_scrollytelling","ggplot2-unwrapped-2025_scrollytelling.qmd", package = "ggplot2wrapped"),
#                       output_file = exported_report_name,
#                       execute_dir = temp_dir,
#                       execute_params = list(executed_from = "ggplot_wrapped_2025"))

  quarto::quarto_render(input = system.file("quarto-reports", "ggplot2-unwrapped-2025_scrollytelling","chip-away.qmd", package = "ggplot2wrapped"),
                        output_file = exported_report_name,
                        execute_dir = temp_dir,
                        execute_params = list(executed_from = "ggplot_wrapped_2025"))

  print("check")
  print(file.exists(exported_report_name))

  quarto_output_file <- system.file("quarto-reports", "ggplot2-unwrapped-2025_scrollytelling", exported_report_name, package = "ggplot2wrapped")

  # # Now copy it to the desired path and delete the original file
  # print(file.path(getwd(), exported_report_name))
  # print(list.files(temp_dir))
  file.copy(from = quarto_output_file, to = file.path(export_path, exported_report_name), overwrite = overwrite)
  file.remove(quarto_output_file)

  on.exit(unlink(temp_dir), add = TRUE)

}
