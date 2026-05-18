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
