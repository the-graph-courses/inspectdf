test_that("inspect_cat summarises categorical columns", {
  df <- data.frame(
    group = c("a", "a", "b", NA),
    flag = c(TRUE, FALSE, TRUE, TRUE),
    day = as.Date("2026-01-01") + 0:3,
    number = c(1.2, 2.3, 3.4, 4.5)
  )

  out <- inspect_cat(df)

  expect_s3_class(out, "inspect_cat")
  expect_equal(out$col_name, c("day", "flag", "group"))
  expect_equal(out$cnt[out$col_name == "group"], 3L)
  expect_equal(out$common[out$col_name == "group"], "a")
  group_levels <- out$levels[[which(out$col_name == "group")]]
  expect_named(group_levels, c("value", "prop", "cnt"))
  expect_true(any(is.na(group_levels$value)))
  expect_equal(attr(out, "type"), list(method = "cat", input_type = "single"))
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})

test_that("inspect_cat can include integers", {
  df <- data.frame(id = 1:3, group = c("a", "a", "b"))

  expect_equal(inspect_cat(df)$col_name, "group")
  expect_equal(inspect_cat(df, include_int = TRUE)$col_name, c("group", "id"))
})

test_that("inspect_cat handles dplyr tibbles", {
  skip_if_not_installed("dplyr")

  out <- inspect_cat(dplyr::starwars)

  expect_s3_class(out, "inspect_cat")
  expect_true("gender" %in% out$col_name)
  expect_s3_class(show_plot(out, high_cardinality = 2, text_labels = FALSE), "ggplot")
})

test_that("inspect_cat handles grouped data like upstream", {
  skip_if_not_installed("dplyr")

  df <- dplyr::group_by(
    data.frame(g = c("a", "a", "b"), x = 1:3, y = c("p", "q", "p")),
    g
  )
  out <- inspect_cat(df)

  expect_s3_class(out, "inspect_cat")
  expect_equal(attr(out, "type"), list(method = "cat", input_type = "grouped"))
  expect_named(out, c("g", "col_name", "cnt", "common", "common_pcnt", "levels"))
  expect_equal(out$col_name, c("y", "y"))
  expect_error(show_plot(out), "Grouped comparison plots")
})
