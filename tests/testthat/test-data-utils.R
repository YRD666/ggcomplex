test_that("matrix_to_tidy converts correctly", {
  mat <- matrix(1:12, nrow = 3,
                dimnames = list(c("A", "B", "C"), c("X", "Y", "Z", "W")))
  tidy <- ggcomplex:::matrix_to_tidy(mat)

  expect_s3_class(tidy, "tbl_df")
  expect_equal(nrow(tidy), 12)
  expect_named(tidy, c("row_id", "col_id", "value"))
  expect_true(is.factor(tidy$row_id))
  expect_true(is.factor(tidy$col_id))
  expect_equal(levels(tidy$row_id), c("A", "B", "C"))
  expect_equal(levels(tidy$col_id), c("X", "Y", "Z", "W"))
})

test_that("matrix_to_tidy auto-generates names for unnamed matrix", {
  mat <- matrix(1:6, nrow = 2)
  tidy <- ggcomplex:::matrix_to_tidy(mat)

  expect_equal(levels(tidy$row_id), c("row_1", "row_2"))
  expect_equal(levels(tidy$col_id), c("col_1", "col_2", "col_3"))
})

test_that("relevel_tidy updates factor levels", {
  mat <- matrix(1:6, nrow = 2,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
  tidy <- ggcomplex:::matrix_to_tidy(mat)

  updated <- ggcomplex:::relevel_tidy(tidy,
                                      new_row_order = c("B", "A"),
                                      new_col_order = c("Z", "Y", "X"))
  expect_equal(levels(updated$row_id), c("B", "A"))
  expect_equal(levels(updated$col_id), c("Z", "Y", "X"))
})
