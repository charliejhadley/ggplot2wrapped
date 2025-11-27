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
make_aes_type_percentile_highcharts <- function(data_geom_usage){

  data_aes_by_geom <- data_geom_usage |>
    dplyr::count(geom_name, has_aes, name = "n_geom_aes_combination") |>
    dplyr::mutate(uses_both_aes_types = dplyr::if_else(dplyr::n() == 2, TRUE, FALSE), .by = geom_name) |>
    dplyr::mutate(aes_type = dplyr::if_else(has_aes == TRUE, "Geom-specific aes()", "Dataviz-level aes()"))

  data_both_aes_chart <- data_aes_by_geom |>
    dplyr::filter(uses_both_aes_types == TRUE) |>
    dplyr::mutate(n_geom_usage = sum(n_geom_aes_combination), .by = geom_name)

  data_order_most_specified_geoms <- data_both_aes_chart |>
    dplyr::select(geom_name, aes_type, n_geom_aes_combination) |>
    tidyr::pivot_wider(names_from = aes_type,
                       values_from = n_geom_aes_combination) |>
    dplyr::mutate(perc_dataviz_aes = `Dataviz-level aes()` / sum(`Dataviz-level aes()` + `Geom-specific aes()`), .by = geom_name) |>
    dplyr::arrange(perc_dataviz_aes) |>
    dplyr::select(geom_name, perc_dataviz_aes)

  data_both_aes_chart |>
    dplyr::left_join(data_order_most_specified_geoms) |>
    dplyr::mutate(geom_name = forcats::fct_reorder(geom_name, perc_dataviz_aes),
                  geom_name = forcats::fct_rev(geom_name)) |>
    dplyr::arrange(perc_dataviz_aes) |>
    highcharter::hchart(
      'bar', highcharter::hcaes(x = geom_name, y = n_geom_aes_combination, group = aes_type),
      stacking = "percent"
    ) |>
    highcharter::hc_xAxis(categories = data_order_most_specified_geoms$geom_name,
                          labels = list(style = list(fontSize = 18)),
                          title = list(text = "")) |>
    highcharter::hc_yAxis(labels = list(style = list(fontSize = 18),
                                        format = "{value}%"),
                          title = list(text = "")) |>
    highcharter::hc_legend(reversed = TRUE)


}
