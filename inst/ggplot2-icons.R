library(grid)
library(maps)
library(ggplot2)
library(GPCDStools)

set.seed(1014)

# https://github.com/tidyverse/ggplot2/blob/a78da8ac8653dc0e7fba332c29d612fa586d0ace/icons/icons.R

write_icon <- function(name, code) {
  path <- paste0("inst/icons-ggplot2/", name, ".png")
  png(path, width = 300, height = 300, pointsize = 5, bg = icon_bg_colour)
  on.exit(dev.off())

  grid.draw(code)
  invisible()
}

icon_bg_colour <- cols_gpcds$neutral
icon_line_colour <- cols_gpcds$story_tertiary_lighter
icon_fill_colour <- cols_gpcds$story_tertiary_lighter
icon_line_width <- 10

# Geoms ------------------------------------------------------------------------

write_icon(
  "geom_abline",
  linesGrob(
    unit(c(0, 1), "npc"),
    unit(c(0.2, 0.8), "npc"),
    gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width)
  )
)

write_icon("geom_bar", {
  rectGrob(
    c(0.3, 0.7),
    c(0.4, 0.8),
    height = c(0.4, 0.8),
    width = 0.3,
    vjust = 1,
    gp = gpar(fill = icon_fill_colour, col = NA, default.units = "npc")
  )
})

write_icon("geom_bin2d", {
  n <- 5
  x <- seq(0, 1, length.out = n + 1)[-(n + 1)]
  out <- expand.grid(x = x, y = x)
  fill <- sqrt((out$x - 0.5) ^ 2 + (out$y - 0.5) ^ 2)

  pal <- scales::pal_seq_gradient(cols_gpcds$story_tertiary_lighter, cols_gpcds$story_tertiary_darker)
  rectGrob(
    out$x + 1/n/2,
    out$y + 1/n/2,
    width = 1/n,
    height = 1/n,
    gp = gpar(col = "grey20", fill = pal(scales::rescale(fill)), default.units = "npc")
  )
})

write_icon("geom_blank", {
  rectGrob(0.5, 0.5,
           height = 1,
           width = 1,
           gp = gpar(fill = icon_bg_colour, col = icon_line_colour, lwd = 15,  default.units = "npc")
  )
})

write_icon("geom_count", {
  textGrob(expression(Sigma), gp = gpar(cex = 70, default.units = "npc", col = icon_line_colour,  default.units = "npc"))
})

write_icon("geom_histogram", {
  y <- c(0.2, 0.3, 0.5, 0.6, 0.2, 0.8, 0.5, 0.3)
  rectGrob(
    seq(0.1, 0.9, by = 0.1),
    y,
    height = y,
    width = 0.1,
    vjust = 1,
    gp = gpar(fill = icon_fill_colour, col = NA,  default.units = "npc")
  )
})

write_icon("geom_boxplot", {
  gTree(children = gList(
    segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95)),
    rectGrob(
      c(0.3, 0.7),
      c(0.6, 0.8),
      width = 0.3,
      height = c(0.4, 0.4),
      vjust = 1
    ),
    segmentsGrob(c(0.15, 0.55), c(0.5, 0.6), c(0.45, 0.85), c(0.5, 0.6))
  ),
  gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_crossbar", {
  gTree(children = gList(
    # crossbar
    rectGrob(
      0.3,
      0.6,
      width = 0.3,
      height = c(0.4, 0.4),
      vjust = 1
    ),
    segmentsGrob(c(0.15), c(0.5), c(0.45), c(0.5)),

    # error bar
    segmentsGrob(0.70, 0.5, 0.70, 0.90),
    segmentsGrob(0.55, 0.5, 0.85, 0.50),
    segmentsGrob(0.55, 0.9, 0.85, 0.90)
  ),
  gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_dotplot", {
  xpos <- c(1, 1, 2, 3, 3, 3, 4, 4, 5, 5, 5, 5, 6, 7, 7, 7, 8, 8, 9) / 10
  ypos <- c(1, 2, 1, 1, 2, 3, 1, 2, 1, 2, 3, 4, 1, 1, 2, 3, 1, 2, 1) / 10
  pointsGrob(
    x = xpos,
    y = ypos,
    pch = 19,
    size = unit(.1, "npc"),
    gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width),
    default.units = "npc"
  )
})

write_icon("geom_freqpoly", {
  y <- c(0.2, 0.3, 0.5, 0.6, 0.2, 0.8, 0.5, 0.3)
  linesGrob(seq(0.1, 0.9, by = 0.1), y,
            gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_path", {
  linesGrob(c(0.2, 0.4, 0.8, 0.6, 0.5), c(0.2, 0.7, 0.4, 0.1, 0.5),
            gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_contour", {
  gTree(children = gList(polygonGrob(
    c(0.45, 0.5, 0.6, 0.5), c(0.5, 0.4, 0.55, 0.6)
  ),
  polygonGrob(
    c(0.25, 0.6, 0.8, 0.5), c(0.5, 0.2, 0.75, 0.9),
    gp = gpar(fill = NA)
  )),
  gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_hex", {
  theta <- seq(0, 2 * pi, length.out = 7)[-1]
  polygonGrob(
    0.5 + 0.4 * sin(theta),
    0.5 + 0.4 * cos(theta),
    gp = gpar(fill = icon_fill_colour, default.units = "npc", col = cols_gpcds$story_primary, lwd = icon_line_width)
  )
})

write_icon("geom_line", {
  pos <- seq(0, 1, length.out = 5)
  linesGrob(pos, c(0.2, 0.7, 0.4, 0.8, 0.3), gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_step", {
  n <- 10
  xs <- rep(0:n, each = 2)[-2 * (n + 1)] / n
  ys <- c(0, rep(1:n, each = 2)) / n
  linesGrob(xs, ys, gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width))
})

write_icon("geom_point", {
  pos <- seq(0.1, 0.9, length.out = 6)
  pointsGrob(
    x = pos,
    y = pos,
    pch = 19,
    gp = gpar(col = icon_fill_colour, cex = 05),
    default.units = "npc"
  )
})

write_icon("geom_jitter", {
  pointsGrob(
    x = c(0.25, 0.22, 0.34, 0.70, 0.77, 0.80),
    y = c(0.15, 0.24, 0.28, 0.65, 0.90, 0.75),
    pch = 19,
    gp = gpar(col = icon_fill_colour, cex = 7),
    default.units = "npc"
  )
})

write_icon("geom_pointrange", {
  gTree(children = gList(
    segmentsGrob(c(0.3, 0.7), c(0.1, 0.2), c(0.3, 0.7), c(0.7, 0.95),
                 gp = gpar(lwd = 4)),
    pointsGrob(
      c(0.3, 0.7),
      c(0.4, 0.6),
      pch = 19,
      default.units = "npc"
    )
  ),
  gp = gpar(col = icon_line_colour, default.units = "npc", cex = 7))
})

write_icon("geom_polygon", {
  polygonGrob(
    c(0.1, 0.4, 0.7, 0.9, 0.6, 0.3),
    c(0.5, 0.8, 0.9, 0.4, 0.2, 0.3),
    gp = gpar(col = icon_line_colour, default.units = "npc", lwd = icon_line_width)
  )
})

write_icon("geom_quantile", {
  gTree(children = gList(linesGrob(
    c(0, 0.3, 0.5, 0.8, 1), c(0.8, 0.65, 0.6, 0.6, 0.8)
  ),
  linesGrob(
    c(0, 0.3, 0.5, 0.8, 1), c(0.55, 0.45, 0.5, 0.45, 0.55)
  ),
  linesGrob(
    c(0, 0.3, 0.5, 0.8, 1), c(0.3, 0.25, 0.4, 0.3, 0.2)
  )),
  gp = gpar(col = icon_line_colour, default.units = "npc", lwd = 7))
})

write_icon("geom_raster", {
  rectGrob(
    c(0.25, 0.25, 0.75, 0.75),
    c(0.25, 0.75, 0.75, 0.25),
    width = 0.5,
    height = c(0.67, 0.5, 0.67, 0.5),
    gp = gpar(col = cols_gpcds$black_alternative_dark, fill = c(cols_gpcds$story_tertiary_lighter, cols_gpcds$story_tertiary_darker), lwd = 7)
  )
})
write_icon("geom_ribbon", {
  polygonGrob(
    c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
    c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7),
    gp = gpar(col = icon_line_colour, default.units = "npc", cex = 7, fill = icon_fill_colour),
  )
})

write_icon("geom_spoke", {
  theta <- seq(0, 2 * pi, length.out = 10)[-1]
  r <- seq(0.1, 0.45, length.out = length(theta))
  segmentsGrob(
    0.5, 0.5,
    0.5 + sin(theta) * r,
    0.5 + cos(theta) * r,
    gp = gpar(col = icon_line_colour, default.units = "npc", cex = 7, lwd = 7)
  )
})

write_icon("geom_area", {
  polygonGrob(c(0, 0, 0.3, 0.5, 0.8, 1, 1),
              c(0, 1, 0.5, 0.6, 0.3, 0.8, 0),
              gp = gpar(col = icon_line_colour, default.units = "npc", cex = 7, fill = icon_fill_colour))
})

write_icon("geom_density", {
  x <- seq(0, 1, length.out = 80)
  y <- dnorm(x, mean = 0.5, sd = 0.15)
  linesGrob(x, 0.05 + y / max(y) * 0.9, default.units = "npc",
            gp = gpar(col = icon_line_colour, default.units = "npc", cex = 7, lwd = 7))
})


write_icon("geom_rug", {
  x <- seq(0.15, 0.95, length.out = 8)
  gList(
    segmentsGrob(x, 0, x, 0 + 0.1, gp = gpar(col = icon_line_colour, default.units = "npc", lwd = 7)),
    segmentsGrob(0, x, 0 + 0.1, x,gp = gpar(col = icon_line_colour, default.units = "npc", lwd = 7),)
  )
})

write_icon("geom_segment", {
  segmentsGrob(c(0.1, 0.3, 0.5, 0.7),
               c(0.3, 0.5, 0.1, 0.9),
               c(0.2, 0.5, 0.7, 0.9),
               c(0.8, 0.7, 0.4, 0.3),
               gp = gpar(col = icon_line_colour, default.units = "npc", cex = 7, lwd = 7))
})

write_icon("geom_smooth", {
  gTree(children = gList(polygonGrob(
    c(0, 0.3, 0.5, 0.8, 1, 1, 0.8, 0.5, 0.3, 0),
    c(0.5, 0.3, 0.4, 0.2, 0.3, 0.7, 0.5, 0.6, 0.5, 0.7)
    # gp = gpar(fill = "grey60", col = NA)
  ),
  linesGrob(
    c(0, 0.3, 0.5, 0.8, 1), c(0.6, 0.4, 0.5, 0.4, 0.6),
    gp = gpar(col = cols_gpcds$black_alternative_dark, lwd = 7)
  )),
  gp = gpar(default.units = "npc", cex = 7, fill = icon_fill_colour))
})

write_icon("geom_text", {
  textGrob("text", rot = 45,
           gp = gpar(col = icon_line_colour, default.units = "npc", cex = 30))
})

write_icon("geom_function", {
  textGrob("fn(x)", rot = 0,
           gp = gpar(col = icon_line_colour, default.units = "npc", cex = 23))
})

write_icon("geom_sf", {
  nz <- data.frame(map("nz", plot = FALSE)[c("x", "y")])
  nz$x <- nz$x - min(nz$x, na.rm = TRUE)
  nz$y <- nz$y - min(nz$y, na.rm = TRUE)
  nz <- nz / max(nz, na.rm = TRUE)
  linesGrob(nz$x, nz$y, default.units = "npc", gp = gpar(col = icon_fill_colour,
                                                         lwd = 6))
})

write_icon("geom_violin", {
  y <- seq(-.3, .3, length.out = 40)
  x1 <- dnorm(y, mean = -.15, sd = 0.05) +
    1.5 * dnorm(y, mean = 0.1, sd = 0.1)
  x2 <-
    dnorm(y, mean = -.1, sd = 0.1) + dnorm(y, mean = 0.1, sd = 0.1)

  y <- c(y, rev(y))
  x1 <- c(x1,-rev(x1)) / max(8 * x1)
  x2 <- c(x2,-rev(x2)) / max(8 * x2)
  # gp <- gpar(fill = "black")
  gTree(children = gList(
    polygonGrob(x1 + .30, y + .35, default.units = "npc"),
    polygonGrob(x2 + .70, y + .55, default.units = "npc")
  ),
  gp = gpar(fill = icon_line_colour, default.units = "npc", cex = 7))
})


# ggimage -----------------------------------------------------------------

library(ggimage)

icon_file_paths <- list.files("inst/icons-ggplot2/",
                              full.names = TRUE)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha") # Sets alpha to 50%
}

data_geom_icon_grid <- tibble(
  y = rep(1:8, times = 4),
  x = rep(letters[1:4], each = 8)
) |>
  dplyr::mutate(icon_image_path = icon_file_paths) |>
  dplyr::mutate(icon_name = stringr::str_remove(basename(icon_image_path), "[.]png"))

data_geom_groupings <- read.csv(system.file("geom_groupings.csv"))

data_geom_icon_grid





gg_icon_grid <- tibble(
  y = rep(1:8, times = 4),
  x = rep(letters[1:4], each = 8)
) |>
  mutate(icon_image_path = icon_file_paths) |>
  ggplot() +
  aes(x,
      y) +
  # geom_point() +
  geom_image(
    # data = tibble(bill_length_mm = 50, bill_depth_mm = 20),
    aes(image = icon_image_path),
    size = 0.09,
    image_fun = transparent
  ) +
  coord_equal() +
  theme_void()
  # theme(aspect.ratio = 2)

gg_icon_grid

ggsave("inst/geom_grid_tall.png",
       gg_icon_grid,
       height = 20,
       width = 10
       )




