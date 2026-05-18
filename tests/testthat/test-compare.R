test_that("comparison outputs and plots work", {
  df1 <- data.frame(group = c("a", "a", "b"), x = c(1, 2, 3))
  df2 <- data.frame(group = c("a", "b", "b"), x = c(2, 3, 4))

  cat_out <- inspect_cat(df1, df2)
  num_out <- inspect_num(df1, df2)

  expect_s3_class(cat_out, "inspect_cat")
  expect_s3_class(num_out, "inspect_num")
  expect_named(cat_out, c("col_name", "jsd", "pval", "lvls_1", "lvls_2"))
  expect_named(num_out, c("col_name", "hist_1", "hist_2", "jsd", "pval"))
  expect_equal(attr(cat_out, "type"), list(method = "cat", input_type = "pair"))
  expect_equal(attr(num_out, "type"), list(method = "num", input_type = "pair"))
  expect_s3_class(show_plot(cat_out, text_labels = FALSE), "ggplot")
  expect_s3_class(show_plot(num_out, text_labels = FALSE), "ggplot")
})

test_that("comparison keeps columns present in only one input", {
  cat_out <- inspect_cat(data.frame(a = "x"), data.frame(b = "y"))
  num_out <- inspect_num(data.frame(a = 1), data.frame(b = 2))

  expect_equal(cat_out$col_name, c("a", "b"))
  expect_equal(num_out$col_name, c("a", "b"))
  expect_null(cat_out$lvls_2[[1]])
  expect_null(cat_out$lvls_1[[2]])
  expect_null(num_out$hist_2[[1]])
  expect_null(num_out$hist_1[[2]])
  expect_true(all(is.na(cat_out$jsd)))
  expect_true(all(is.na(num_out$jsd)))
  expect_s3_class(show_plot(cat_out), "ggplot")
  expect_s3_class(show_plot(num_out), "ggplot")
})
