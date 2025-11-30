#' Summarise geom usage per day
#'
#' `summarise_per_day()` utility function for summarising geom usage by several
#'  different metrics
#'
#' @param data_geom_usage Tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#' @param measure How to summatrise usage by day? Defaaults to "per_day_individual_geom_usage",
#'  can also be one of; "per_day_files_with_geoms", "per_day_unique_geoms",
#'   "per_day_total_geom_usage"
#'
#' @export
summarise_per_day <- function(data_geom_usage, measure = "per_day_individual_geom_usage"){

  if (!(measure %in% c("per_day_individual_geom_usage",
                       "per_day_files_with_geoms",
                       "per_day_unique_geoms",
                       "per_day_total_geom_usage"))) {
    cli::cli_abort(
      c("x" = "Invalid value for {.arg measure}.",
        "i" = "You provided: {.val {measure}}",
        "v" = "Permitted values are: {.val per_day_individual_geom_usage}, {.val per_day_files_with_geoms}, {.val per_day_unique_geoms} or {.val per_day_total_geom_usage}"
      )
    )
  }

  # TODO: Use tidyeval
  data_summarised_per_day <- switch (measure,
                                     "per_day_individual_geom_usage" = data_geom_usage |>
                                       dplyr::mutate(modified_date = lubridate::as_date(modified_time)) |>
                                       dplyr::select(geom_name, modified_date, n_times_used) |>
                                       dplyr::summarise(calendar_measure = sum(n_times_used), .by = c(geom_name, modified_date)),


                                     "per_day_files_with_geoms" = data_geom_usage |>
                                       dplyr::mutate(modified_date = lubridate::as_date(modified_time)) |>
                                       dplyr::select(file_path, modified_date) |>
                                       dplyr::summarise(calendar_measure = dplyr::n(), .by = c(file_path, modified_date)),

                                     "per_day_unique_geoms" = data_geom_usage |>
                                       dplyr::mutate(modified_date = lubridate::as_date(modified_time)) |>
                                       dplyr::select(geom_name, modified_date) |>
                                       dplyr::summarise(calendar_measure = dplyr::n_distinct(geom_name), .by = modified_date),

                                     "per_day_total_geom_usage" = data_geom_usage |>
                                       dplyr::mutate(modified_date = lubridate::as_date(modified_time)) |>
                                       dplyr::select(geom_name, modified_date, n_times_used) |>
                                       dplyr::summarise(calendar_measure = sum(n_times_used), .by = c(modified_date))
  )

  data_summarised_per_day

}

#' Summarise geom usage per file
#'
#' `summarise_per_file()` utility function for summarising geom usage by several
#'  different metrics
#'
#' @param data_geom_usage Tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#'
#' @export
summarise_per_file <- function(data_geom_usage){

  data_geom_usage |>
    dplyr::distinct(file_path, geom_name) |>
    dplyr::summarise(geoms = list(geom_name), .by = file_path)

}

#' Fill geom usage per day
#'
#' `fill_geom_usage_date_data()` utility function for filling geom usage per day
#'  from the function `summarise_per_day()`
#'
#' @param data_usage_per_day Tibble containing geom usage per day, generated from
#'  `summarise_per_day()`
#' @param target_year Target year. Defaults to 2025 as that's the first year of
#'  ggplot2wraped!
#'
#' @export
fill_geom_usage_date_data <- function(data_usage_per_day, target_year = 2025){

  data_usage_per_day |>
    tidyr::complete(modified_date = seq(lubridate::ymd(paste0(target_year, "-01-01")), lubridate::ymd(paste0(target_year, "-12-31")), by = "day"), fill = list(n_times_used = NA)) |>
    dplyr::mutate(
      n_week = lubridate::week(modified_date),
      n_day = lubridate::wday(modified_date, week_start = 7),
      weekday_label = lubridate::wday(modified_date, week_start = 7, label = TRUE, abbr = TRUE),
      weekday_label = forcats::fct_rev(weekday_label),
      month = lubridate::month(modified_date, label = TRUE, abbr = TRUE),
      month_name = lubridate::month(modified_date, label = TRUE, abbr = FALSE),
      is_workday = dplyr::if_else(weekday_label %in% c("Sat", "Sun"), FALSE, TRUE)
    )


}
#' Fill geom usage per day with nesting
#'
#' `fill_geom_usage_date_data_with_nesting()` utility function for filling geom usage per day
#'  from the function `summarise_per_day()` supporting nesting.
#'
#' @param data_usage_per_day Tibble containing geom usage per day, generated from
#'  `summarise_per_day()`
#' @param target_year Target year. Defaults to 2025 as that's the first year of
#'  ggplot2wraped!
#'
#' @export
fill_geom_usage_date_data_with_nesting <- function(data_usage_per_day, nesting_column = NULL, target_year = 2025){


  nesting_column <- dplyr::ensym(nesting_column)

  data_usage_per_day |>
    tidyr::complete(tidyr::nesting(!!dplyr::ensym(nesting_column)), modified_date = seq(lubridate::ymd(paste0(target_year, "-01-01")), lubridate::ymd(paste0(target_year, "-12-31")), by = "day"), fill = list(n_times_used = NA)) |>
    dplyr::mutate(
      n_week = lubridate::week(modified_date),
      n_day = lubridate::wday(modified_date, week_start = 7),
      weekday_label = lubridate::wday(modified_date, week_start = 7, label = TRUE, abbr = TRUE),
      weekday_label = forcats::fct_rev(weekday_label),
      month = lubridate::month(modified_date, label = TRUE, abbr = TRUE),
      is_workday = dplyr::if_else(weekday_label %in% c("Sat", "Sun"), FALSE, TRUE)
    )


}
