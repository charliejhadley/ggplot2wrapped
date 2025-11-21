test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


get_code_file_info(c("~/Github/", "~/coding/r-projects-scrapbook/"))

list.files("~/Github/", recursive = TRUE, full.names = TRUE, pattern = "(\\.R$)|(\\.qmd$)")


file.info("tests/testthat/test-get_file_info.R") %>% dput()



library("astgrepr")

code_file <- paste0(read_lines("dummy.R"),collapse = "\n")

root <- code_file |>
  astgrepr::tree_new() |>
  astgrepr::tree_root()

data_nodes <- root %>%
  astgrepr::node_find_all(
    astgrepr::ast_rule(id = "geom_label_repel", pattern = "geom_label_repel($$$A)"),
    astgrepr::ast_rule(id = "geom_point", pattern = "geom_point($$$A)"),
    astgrepr::ast_rule(id = "geom_line", pattern = "geom_line($$$A)")
  )

data_nodes %>%
  node_text_all()

root %>%
  astgrepr::node_find_all(
    astgrepr::ast_rule(id = "geom_label_repel", pattern = "geom_label_repel($$$A)"),
    astgrepr::ast_rule(id = "geom_point", pattern = "geom_point($$$A)"),
    astgrepr::ast_rule(id = "geom_quantile", pattern = "geom_quantile($$$A)")
  )

get_geoms_from_code_file(c("dummy.R", "dummy.R"), slice(data_geoms, 2))

v_fn <- Vectorize(get_geoms_from_code_file)

get_code_file_info(c("~/Github/", "~/coding/r-projects-scrapbook/")) %>%
  slice(1:1000) %>%
  mutate(geom_usage = get_geoms_from_code_file(file_path, data_geoms)) %>%
  unnest(geom_usage) %>%
  filter(n_times_used > 0)
