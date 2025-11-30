#' Grid of geom groupings and their usage
#'
#' `make_geom_icon_usage_grid()` creates a ggplot2 with a grid of geom groupings
#' from the official {ggplot2} charts, optionally highlighting which geoms are
#' used.
#'
#' @param data_geom_usage Tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#' @param show_which_geoms Which geoms are shown? Default is "used" which
#' highlights only used geoms. Providing "all" will show all geoms.
#' RMarkdown.
#' @returns A ggplot2 chart
#' @export
make_geom_icon_usage_grid <- function(data_geom_usage, show_which_geoms = "used"){

  if (!(show_which_geoms %in% c("used", "all"))) {
    cli::cli_abort(
      c("x" = "Invalid value for {.arg show_which_geoms}.",
        "i" = "You provided: {.val {show_which_geoms}}",
        "v" = "Permitted values are: {.val usage} or {.val all}"
      )
    )
  }

  data_geom_groupings <- read.csv(system.file("geom_groupings.csv", package = "ggplot2wrapped"))

  icon_file_paths <- list.files(system.file("icons-ggplot2", package = "ggplot2wrapped"),
                                full.names = TRUE)

  data_geom_icon_grid <- dplyr::tibble(
    y = rep(1:8, times = 4),
    x = rep(letters[1:4], each = 8)
  ) |>
    dplyr::mutate(icon_image_path = icon_file_paths) |>
    dplyr::mutate(icon_name = stringr::str_remove(basename(icon_image_path), "[.]png")) |>
    dplyr::left_join(data_geom_groupings,
              by = c("icon_name" = "geom_icon"))

  data_times_geom_used <- data_geom_usage |>
    dplyr::summarise(total_geom_usage = sum(n_times_used), .by = geom_name)


  transparent <- function(img) {
    magick::image_fx(img, expression = "0.09*a", channel = "alpha") # Sets alpha to 50%
  }

  if(show_which_geoms == "used"){

    gg_geom_grid <- data_geom_icon_grid |>
      ggplot2::ggplot() +
      ggplot2::aes(x,
                   y) +
      ggimage::geom_image(
        data = dplyr::filter(data_geom_icon_grid, geom_name %in% data_times_geom_used$geom_name),
        ggplot2::aes(image = icon_image_path),
        size = 0.09
      ) +
      ggimage::geom_image(
        data = dplyr::filter(data_geom_icon_grid, !geom_name %in% data_times_geom_used$geom_name),
        ggplot2::aes(image = icon_image_path),
        image_fun = transparent,
        size = 0.09
      ) +
      ggplot2::coord_equal() +
      # ggplot2::theme_void(paper = GPCDStools::cols_gpcds$neutral)
      ggplot2::theme_void(paper = "white")

  }

  if(show_which_geoms == "all"){

    gg_geom_grid <- data_geom_icon_grid |>
      ggplot2::ggplot() +
      ggplot2::aes(x,
                   y) +
      ggimage::geom_image(
        data = data_geom_icon_grid,
        ggplot2::aes(image = icon_image_path),
        size = 0.09
      ) +
      ggplot2::coord_equal() +
      # ggplot2::theme_void(paper = GPCDStools::cols_gpcds$neutral)
      ggplot2::theme_void(paper = "white")

  }

  return(gg_geom_grid)
}

# Utility function

GeomRtile <- ggplot2::ggproto(
  "GeomRtile",
  statebins:::GeomRrect, # 1) only change compared to ggplot2:::GeomTile

  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
              xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },
  default_aes = ggplot2::aes(
    fill = "grey20", colour = NA, size = 0.1, linetype = 1,
    alpha = NA, width = NA, height = NA
  ),
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
  draw_key = ggplot2::draw_key_polygon
)

geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"), # 2) add radius argument
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}


#' geom usage calendar
#'
#' `make_geom_usage_calendar()` generates a ggplot2 chart showing which geoms
#' are used when, it is inspired by the GitHub commit chart.
#'
#' @section Where does this visualisation come from?
#'
#' This is intended to look like the GitHub commit chart and the code is largely
#' based on https://restateinsight.com/posts/general-posts/2024-12-github-contributions-plot/
#'
#' @param data_geom_usage Tibble containing geom usage data produced via
#' `add_geom_usage_to_files()`.
#' files
#' @param measure How should geom use be summarised? Defaults to "per_day_individual_geom_usage",
#' can also be; "per_day_files_with_geoms", "per_day_unique_geoms", "per_day_total_geom_usage"
#' @returns A tibble.
#' @export
make_geom_usage_calendar <- function(data_geom_usage, measure = "per_day_individual_geom_usage"){

  if (!(measure %in% c("per_day_individual_geom_usage",
                       "per_day_files_with_geoms",
                       "per_day_unique_geoms",
                       "per_day_total_geom_usage"))) {
    cli::cli_abort(
      c("x" = "Invalid value for {.arg measure}.",
        "i" = "You provided: {.val {measure}}",
        "v" = "Permitted values are: {.val per_day_individual_geom_usage}, {.val per_day_files_with_geoms}, {.val per_day_unique_geoms}, {.val per_day_total_geom_usage}"
      )
    )
  }


  data_per_day_individual_geom_usage <- data_geom_usage |>
    dplyr::mutate(modified_date = lubridate::as_date(modified_time)) |>
    dplyr::select(geom_name, modified_date, n_times_used) |>
    dplyr::summarise(calendar_measure = sum(n_times_used), .by = c(geom_name, modified_date))



  data_geom_summarised <- summarise_per_day(data_geom_usage, measure)

  title_text <- switch (measure,
                                  "per_day_files_with_geoms" = "Number of files with geoms modified per day",
                                  "per_day_unique_geoms" = "Unique geoms used per day",
                                  "per_day_total_geom_usage" = "Total number of geoms modified per day",
                        "per_day_individual_geom_usage" = "Per day usage for your top 4 geoms"
  )

  if(measure == "per_day_individual_geom_usage"){

    top_4_geoms <- data_summary_geom_calls |>
      dplyr::slice_max(total_times_used,
                       n = 4) |>
      dplyr::arrange(dplyr::desc(total_times_used)) |>
      dplyr::pull(geom_name)

    data_geom_summarised <- data_geom_usage |>
      dplyr::mutate(geom_name = dplyr::if_else(geom_name %in% top_4_geoms, geom_name, "All other geoms")) |>
      summarise_per_day("per_day_individual_geom_usage")

    data_geom_calendar <- data_geom_summarised |>
      fill_geom_usage_date_data_with_nesting(geom_name) |>
      dplyr::mutate(total_usage = sum(calendar_measure, na.rm = TRUE), .by = geom_name)

    tab <- data_geom_calendar |>
      dplyr::summarise(nmin = min(n_week), .by = "month")

    data_geom_calendar |>
      dplyr::mutate(geom_name = forcats::fct_relevel(geom_name, top_4_geoms),
                    geom_name = forcats::fct_relevel(geom_name, "All other geoms", after = Inf)) |>
      ggplot2::ggplot() +
      ggplot2::aes(n_week, weekday_label) +
      geom_rtile(
        ggplot2::aes(fill = calendar_measure),
        color = "blue",
        radius = ggplot2::unit(2, "pt"),
        width = 0.9,
        height = 0.9
      ) +
      ggplot2::scale_x_continuous(
        breaks = tab$nmin,
        labels = as.character(tab$month),
        position = "top",
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_discrete(breaks = c("Mon", "Wed", "Fri")) +
      ggplot2::scale_fill_continuous(
        palette = "Greens",
        # n.breaks = 4,
        na.value = "gray90") +
      ggplot2::facet_grid(geom_name ~ .,
                 switch="y") +
      ggplot2::labs(x = NULL, y = NULL,
                    title = title_text) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_minimal(base_size = 22, paper = GPCDStools::cols_gpcds$neutral) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        text = ggplot2::element_text(color = "gray10"),
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(angle = 0,
                                                  size = 22)
      )

  } else {

    data_geom_calendar <- data_geom_summarised |>
      fill_geom_usage_date_data()

    tab <- data_geom_calendar |>
      dplyr::summarise(nmin = min(n_week), .by = "month")


   data_geom_calendar |>
      ggplot2::ggplot() +
      ggplot2::aes(n_week, weekday_label) +
      geom_rtile(
        ggplot2::aes(fill = calendar_measure),
        color = "blue",
        radius = ggplot2::unit(2, "pt"),
        width = 0.9,
        height = 0.9
      ) +
      ggplot2::scale_x_continuous(
        breaks = tab$nmin,
        labels = as.character(tab$month),
        position = "top",
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_discrete(breaks = c("Mon", "Wed", "Fri")) +
      ggplot2::scale_fill_binned(
        palette = "Greens",
        n.breaks = 4,
        na.value = "gray90") +
      ggplot2::labs(x = NULL, y = NULL,
                    title = title_text) +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_minimal(base_size = 22, paper = GPCDStools::cols_gpcds$neutral) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        text = ggplot2::element_text(color = "gray10"),
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(angle = 0,
                                                  size = 12)
      )

  }



}

# Under developed, sorry
make_geom_upset_chart <- function(data_geom_usage, focus = c("geom", "date")){


  data_geom_usage |>
    dplyr::summarise_per_file() |>
    ggplot2::ggplot(ggplot2::aes(x=geoms)) +
    ggplot2::geom_bar(fill = cols_gpcds$chart_tertiary_lighter) +
    ggplot2::scale_x_upset(reverse = TRUE) +
    ggupset::theme_combmatrix(combmatrix.panel.point.color.fill = cols_gpcds$graph_tertiary_lighter,
                     combmatrix.panel.line.color = cols_gpcds$graph_tertiary_lighter,
                     # combmatrix.panel.line.size = 0,
                     combmatrix.label.make_space = TRUE) +
    ggplot2::theme(panel.background = element_rect(fill = "transparent"))
}

