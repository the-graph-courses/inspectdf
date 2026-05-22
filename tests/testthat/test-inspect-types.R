test_that("inspect_types summarises column types", {
  df <- data.frame(
    id = 1:3,
    amount = c(1.2, 2.3, 3.4),
    group = c("a", "b", "c"),
    flag = c(TRUE, FALSE, TRUE)
  )

  out <- inspect_types(df)

  expect_s3_class(out, "inspect_types")
  expect_named(out, c("type", "cnt", "pcnt", "col_name"))
  expect_equal(attr(out, "type"), list(method = "types", input_type = "single"))
  expect_equal(out$cnt[out$type == "character"], 1L)
  expect_equal(out$pcnt[out$type == "character"], 25)
  expect_equal(unname(out$col_name[[which(out$type == "character")]]), "group")
  expect_equal(names(out$col_name[[which(out$type == "character")]]), "3")
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})

test_that("inspect_types compares data frames", {
  df1 <- data.frame(id = 1:3, group = c("a", "b", "c"))
  df2 <- data.frame(id = as.numeric(1:3), flag = c(TRUE, FALSE, TRUE))

  out <- inspect_types(df1, df2)

  expect_s3_class(out, "inspect_types")
  expect_named(out, c("type", "equal", "cnt_1", "cnt_2", "columns", "issues"))
  expect_equal(attr(out, "type"), list(method = "types", input_type = "pair"))
  expect_equal(out$type, c("character", "integer", "logical", "numeric"))
  expect_equal(out$cnt_1[out$type == "character"], 1L)
  expect_equal(out$cnt_2[out$type == "logical"], 1L)
  expect_true(length(out$issues[[which(out$type == "integer")]]) > 0)
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})

test_that("inspect_types can compare column positions", {
  df1 <- data.frame(id = 1:3, group = c("a", "b", "c"))
  df2 <- data.frame(group = c("a", "b", "c"), id = 1:3)

  by_name <- inspect_types(df1, df2)
  by_index <- inspect_types(df1, df2, compare_index = TRUE)

  expect_true(all(by_name$equal == "\u2714"))
  expect_true(all(by_index$equal == "\u2718"))
})

test_that("inspect_types handles grouped data", {
  skip_if_not_installed("dplyr")

  df <- dplyr::group_by(
    data.frame(g = c("a", "a", "b"), x = 1:3, y = c("p", "q", "r")),
    g
  )
  out <- inspect_types(df)

  expect_s3_class(out, "inspect_types")
  expect_equal(attr(out, "type"), list(method = "types", input_type = "grouped"))
  expect_named(out, c("g", "type", "cnt", "pcnt", "col_name"))
  expect_equal(out$type, c("character", "integer", "character", "integer"))
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})
