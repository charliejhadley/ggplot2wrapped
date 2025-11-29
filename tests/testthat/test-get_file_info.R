test_that("get_geoms_from_code_file works with file containing geoms", {
  result <- get_geoms_from_code_file(
    testthat::test_path("dummy_with_geoms.R"),
    data_geoms
  )

  result_df <- result[[1]]

  expect_equal(nrow(result_df), 5)

  expect_equal(
    result_df$geom_name,
    c("geom_line", "geom_point", "geom_point", "geom_point", "geom_label_repel")
  )

  expect_equal(result_df$has_aes, c(TRUE, FALSE, TRUE, FALSE, TRUE))

  expect_equal(result_df$length_of_call, c(64, 33, 9, 0, 25))

  expect_equal(result_df$n_args_in_call, c(3, 3, 1, 0, 1))

  expect_equal(result_df$n_times_used, c(1, 3, 3, 3, 1))

  expect_equal(
    result_df$package_name,
    c("ggplot2", "ggplot2", "ggplot2", "ggplot2", "ggrepel")
  )

  expect_equal(
    result_df$function_call[[1]],
    tibble::tibble(
      is_aes = c(FALSE, FALSE, TRUE),
      argument_name = c("linewidth", "", ""),
      argument_value = c(
        "50",
        "another_not_aes_arg",
        "aes(x = thing, y = thang)"
      ),
      argument_number = c(1L, 3L, 2L)
    )
  )

  expect_equal(
    result_df$function_call[[2]],
    tibble::tibble(
      is_aes = c(FALSE, FALSE, FALSE),
      argument_name = c("size", "colour", "pch"),
      argument_value = c("1", "foo", "2"),
      argument_number = c(1L, 2L, 3L)
    )
  )

  expect_equal(
    result_df$function_call[[3]],
    tibble::tibble(
      is_aes = TRUE,
      argument_name = "",
      argument_value = "aes(1, 5)",
      argument_number = 1L
    )
  )

  expect_equal(
    result_df$function_call[[4]],
    tibble::tibble(
      is_aes = logical(0),
      argument_name = character(0),
      argument_value = character(0),
      argument_number = integer(0)
    )
  )

  expect_equal(
    result_df$function_call[[5]],
    tibble::tibble(
      is_aes = TRUE,
      argument_name = "aes",
      argument_value = "aes(x = ee, y = yy)",
      argument_number = 1L
    )
  )
})

test_that("get_geoms_from_code_file works with file without geoms", {
  result <- get_geoms_from_code_file(
    testthat::test_path("dummy_without_geoms.R"),
    data_geoms
  )

  result_df <- result[[1]]

  expect_equal(nrow(result_df), 1)
  expect_true(is.na(result_df$geom_name[1]))
})

