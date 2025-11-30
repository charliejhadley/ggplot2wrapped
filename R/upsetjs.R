#' Interactive UpSet chart
#'
#' `make_geom_interactive_upset_chart()` visualisation function for creating an
#' interactive UpSet chart showing which visualisations are used together in
#' code files. Uses the {htmlwidgets} package {upsetjs}.
#'
#' @param data_geom_usage tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#' @param height height of htmlwidget in pixels
#' @param min_geom_appearances minimum number of times geom needs to
#' @returns A tibble.
#'
#' @export
make_geom_interactive_upset_chart <- function(data_geom_usage, height = NULL, min_geom_appearances = 10, min_interaction_size = 1){


  data_wide_upset_js <- data_geom_usage |>
    dplyr::distinct(file_path, geom_name) |>
    dplyr::mutate(n_times_geom_appears = dplyr::n(), .by = geom_name) |>
    dplyr::filter(n_times_geom_appears > min_geom_appearances) |>
    dplyr::select(file_path, geom_name) |>
    tidyr::pivot_wider(values_from = geom_name,
                names_from = geom_name) |>
    dplyr::select(starts_with("geom")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(is.na(.x), 0, 1)))

  # Note: tidyr::unite() has a fun bug which means this lower dependency solution
  # is better
  data_wide_upset_js <- data_wide_upset_js |>
    dplyr::mutate(interaction = paste0(colnames(data_wide_upset_js), collapse = "_"), .before = 1)

  data_wide_upset_js_filtered <- data_wide_upset_js |>
    dplyr::mutate(interaction_size = dplyr::n(), .by = interaction) |>
    dplyr::filter(interaction_size > min_interaction_size ) |>
    dplyr::select(dplyr::starts_with("geom"))


  upsetjs::upsetjs(height = height) |>
    upsetjs::fromDataFrame(data_wide_upset_js_filtered, order.by = "cardinality", colors = list("red")) |>
    upsetjs::interactiveChart() |>
    # generateDistinctIntersections(min = 1)
    upsetjs::chartLabels(combination.name = "Times used together", set.name = "Times geom appears")
}


#' Interactive Venn Diagram
#'
#' `make_geom_interactive_venn_diagram()` visualisation function for creating an
#' interactive Venn Diagram of top 3 most used geoms.
#'
#' @param data_geom_usage tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#'
#' @export
make_geom_interactive_venn_diagram <- function(data_geom_usage){

  data_wide_upset_js <- data_geom_usage |>
    dplyr::distinct(file_path, geom_name) |>
    dplyr::mutate(n_times_geom_appears = dplyr::n(), .by = geom_name) |>
    # dplyr::filter(n_times_geom_appears > 10) |>
    dplyr::select(file_path, geom_name) |>
    tidyr::pivot_wider(values_from = geom_name,
                       names_from = geom_name) |>
    dplyr::select(starts_with("geom")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(is.na(.x), 0, 1)))

  # I found a bug in tidyr::unite() so this is horrible.

  data_wide_upset_js <- data_wide_upset_js |>
    dplyr::mutate(interaction = paste0(colnames(data_wide_upset_js), collapse = "_"), .before = 1)

  top_3_occuring_geoms <- data_wide_upset_js |>
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~sum(.x))) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "geom_name") |>
    dplyr::slice_max(value, n = 3) |>
    dplyr::pull(geom_name)

  data_top_3_wide_upset_js <- data_wide_upset_js |>
    dplyr::select(dplyr::any_of(top_3_occuring_geoms))

  upsetjs::upsetjsVennDiagram() |>
    upsetjs::fromDataFrame(data_top_3_wide_upset_js) |>
    upsetjs::interactiveChart() |>
    upsetjs::chartFontSizes(set.label = "30px", value.label = "20px")


}
