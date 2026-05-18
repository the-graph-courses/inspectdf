test_that("inspect_num summarises numeric columns", {
  df <- data.frame(
    x = c(1, 2, 3, NA),
    y = c(10.5, 11.5, 12.5, 13.5),
    group = c("a", "a", "b", "b")
  )

  out <- inspect_num(df, breaks = 3)

  expect_s3_class(out, "inspect_num")
  expect_equal(out$col_name, c("x", "y"))
  expect_equal(out$pcnt_na[out$col_name == "x"], 25)
  expect_named(out$hist[[which(out$col_name == "x")]], c("value", "prop"))
  expect_equal(attr(out, "type"), list(method = "num", input_type = "single"))
  expect_named(attr(out, "brks_list"), c("x", "y"))
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})

test_that("inspect_num can exclude integers", {
  df <- data.frame(id = 1:3, amount = c(1.2, 2.3, 3.4))

  expect_equal(inspect_num(df)$col_name, c("id", "amount"))
  expect_equal(inspect_num(df, include_int = FALSE)$col_name, "amount")
})

test_that("inspect_num handles dplyr tibbles", {
  skip_if_not_installed("dplyr")

  out <- inspect_num(dplyr::starwars)

  expect_s3_class(out, "inspect_num")
  expect_true("height" %in% out$col_name)
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})

test_that("inspect_num supports per-column breaks", {
  out <- inspect_num(data.frame(x = 1:10), breaks = list(x = c(0, 5, 10)))

  expect_equal(attr(out, "brks_list")$x, c(0, 5, 10))
  expect_equal(out$hist[[1]]$value, c("[0, 5)", "[5, 10)"))
})

test_that("inspect_num handles grouped data like upstream", {
  skip_if_not_installed("dplyr")

  df <- dplyr::group_by(
    data.frame(g = c("a", "a", "b"), x = 1:3, y = c("p", "q", "p")),
    g
  )
  out <- inspect_num(df)

  expect_s3_class(out, "inspect_num")
  expect_equal(attr(out, "type"), list(method = "num", input_type = "grouped"))
  expect_named(out, c("g", "col_name", "min", "q1", "median", "mean", "q3", "max", "sd", "pcnt_na", "hist"))
  expect_equal(out$col_name, c("x", "x"))
  expect_s3_class(show_plot(out, text_labels = FALSE), "ggplot")
})
