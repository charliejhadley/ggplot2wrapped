test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

library("astgrepr")

library("tidyverse")
get_geoms_from_code_file("tests/testthat/dummy_with_geoms.R", filter(data_geoms, geom_name %in% c("geom_point", "geom_line", "geom_scarf", "geom_label_repel")))

# Test file without geoms -------------------------------------------------

get_geoms_from_code_file("tests/testthat/dummy_without_geoms.R", filter(data_geoms, geom_name %in% c("geom_point", "geom_line", "geom_scarf", "geom_label_repel")))

