#' Interactive aes usaege chart
#'
#' `make_aes_type_percentile_highcharts()` interactive bar chart showing geoms
#' that use both dataviz-level and chart-level aesthetics and their relative %
#'
#' @param data_geom_usage Tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#'
#' @returns A ggplot2 chart
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

#' Interactive pie chart of geom usage
#'
#' `make_geom_usage_pies_highcharts()` produces an interactive pie chart showing
#' your top 4 geom usage across all calls or files
#'
#' @param data_geom_usage Tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#' @param pie_measure_type How should pie chart summarise geom used? Default to
#' "geom_total_usage", can also be "geom_usage_in_files"
#'
#' @returns A ggplot2 chart.
#' @export
make_geom_usage_pies_highcharts <- function(data_summary_geom_calls, pie_measure_type = c("geom_total_usage", "geom_usage_in_files")){

  if (!(pie_measure_type %in% c("geom_total_usage", "geom_usage_in_files"))) {
    cli::cli_abort(
      c("x" = "Invalid value for {.arg pie_measure_type}.",
        "i" = "You provided: {.val {pie_measure_type}}",
        "v" = "Permitted values are: {.val geom_total_usage} or {.val geom_usage_in_files}"
      )
    )
  }

  vec_colours_pie <- GPCDStools::colours_gpcds |>
  dplyr::filter(type == "tertiary_lighter") |>
  dplyr::slice(1:4) |>
  dplyr::pull(hex_code) |>
  c(GPCDStools::cols_gpcds$grey_mid)

  data_summary_geom_calls |>
    dplyr::arrange(dplyr::desc(total_times_used)) |>
    dplyr::mutate(geom_name = dplyr::if_else(dplyr::row_number() <= 4, geom_name, "All other geoms")) |>
    dplyr::summarise(total_times_used = sum(total_times_used), .by = geom_name)

  hc_pie_chart <- switch(pie_measure_type,
         "geom_total_usage" = {
           highcharter::highchart() |>
             highcharter::hc_add_series(
               data_total_times_used_top_n,
               "pie",
               highcharter::hcaes(
                 x = geom_name,
                 y = total_times_used
               ),
               center = c(50, 50),
               innerSize="50%",
               dataLabels = list(distance = 18,
                                 format = '<b>{point.name}</b>:<br>Used {point.y} times<br>({point.percentage:.0f} %)',
                                 style = list(fontSize = 16))) |>
             highcharter::hc_colors(vec_colours_pie) |>
             highcharter::hc_plotOptions(
               innersize="50%",
               startAngle=90,
               endAngle=90,
               center=list('50%', '75%'),
               size='110%'
               # width = "300px"
             ) |>
             highcharter::hc_title(text = 'Total times<br>geom used',
                                   verticalAlign = 'middle',
                                   align = 'center',
                                   style = list(fontSize = 24),
                                   y = 0,
                                   floating = TRUE
             )
         },
         "geom_usage_in_files" = {
           highcharter::highchart() |>
             highcharter::hc_add_series(
               data_used_across_files_top_n,
               "pie",
               highcharter::hcaes(
                 x = geom_name,
                 y = used_in_n_files
               ),
               center = c(50, 50),
               innerSize="50%",
               dataLabels = list(distance = 18,
                                 format = '<b>{point.name}</b>:<br>Used in {point.y} files<br>({point.percentage:.0f} %)',
                                 style = list(fontSize = 16))) |>
             highcharter::hc_colors(vec_colours_pie) |>
             highcharter::hc_plotOptions(
               innersize="50%",
               startAngle=90,
               endAngle=90,
               center=list('50%', '75%'),
               size='110%'
               # width = "300px"
             ) |>
             highcharter::hc_title(text = '# of files geom<br>occured in',
                                   verticalAlign = 'middle',
                                   align = 'center',
                                   style = list(fontSize = 24),
                                   y = 0,
                                   floating = TRUE
             )
         })


  hc_pie_chart


}


