test_that("gg_heatmap returns a valid ggcomplex object", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat)

  expect_s3_class(obj, "ggcomplex")
  expect_true(ggcomplex:::is_ggcomplex(obj))
  expect_s3_class(obj$plots$main, "gg")
  expect_equal(dim(obj$data$original), c(5, 6))
})

test_that("gg_heatmap rejects non-matrix input", {
  expect_error(gg_heatmap("not a matrix"))
})
