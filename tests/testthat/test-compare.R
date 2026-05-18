test_that("comparison outputs and plots work", {
  df1 <- data.frame(group = c("a", "a", "b"), x = c(1, 2, 3))
  df2 <- data.frame(group = c("a", "b", "b"), x = c(2, 3, 4))

  cat_out <- inspect_cat(df1, df2)
  num_out <- inspect_num(df1, df2)

  expect_s3_class(cat_out, "inspect_cat")
  expect_s3_class(num_out, "inspect_num")
  expect_named(cat_out, c("col_name", "jsd", "pval", "lvls_1", "lvls_2"))
  expect_named(num_out, c("col_name", "jsd", "pval", "hist_1", "hist_2"))
  expect_s3_class(show_plot(cat_out, text_labels = FALSE), "ggplot")
  expect_s3_class(show_plot(num_out, text_labels = FALSE), "ggplot")
})

test_that("comparison handles no common inspected columns", {
  cat_out <- inspect_cat(data.frame(a = "x"), data.frame(b = "y"))
  num_out <- inspect_num(data.frame(a = 1), data.frame(b = 2))

  expect_equal(nrow(cat_out), 0L)
  expect_equal(nrow(num_out), 0L)
  expect_s3_class(show_plot(cat_out), "ggplot")
  expect_s3_class(show_plot(num_out), "ggplot")
})
