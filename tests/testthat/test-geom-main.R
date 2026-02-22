test_that("gg_heatmap handles all key parameters", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat,
                    fill_palette = c("blue", "white", "red"),
                    show_values = TRUE,
                    cell_shape = "rect",
                    show_row_names = FALSE,
                    show_col_names = FALSE,
                    title = "Test")
  expect_s3_class(obj, "ggcomplex")
  expect_s3_class(obj$plots$main, "gg")
  expect_equal(obj$params$plot_type, "heatmap")
  expect_equal(obj$params$plot_title, "Test")
})

test_that("gg_heatmap circle cell shape works", {
  mat <- matrix(rnorm(20), nrow = 4,
                dimnames = list(paste0("R", 1:4), paste0("C", 1:5)))
  obj <- gg_heatmap(mat, cell_shape = "circle")
  expect_s3_class(obj, "ggcomplex")
  expect_equal(obj$params$cell_shape, "circle")
})

test_that("gg_heatmap converts data.frame to matrix", {
  df <- data.frame(a = 1:3, b = 4:6, row.names = c("X", "Y", "Z"))
  obj <- gg_heatmap(df)
  expect_s3_class(obj, "ggcomplex")
  expect_true(is.matrix(obj$data$original))
})

test_that("gg_dotplot returns valid object", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_dotplot(mat)
  expect_s3_class(obj, "ggcomplex")
  expect_equal(obj$params$plot_type, "dotplot")
  expect_true("size_value" %in% colnames(obj$data$tidy))
})

test_that("gg_dotplot with size_mat works", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  smat <- matrix(runif(30), nrow = 5, dimnames = dimnames(mat))
  obj <- gg_dotplot(mat, size_mat = smat)
  expect_s3_class(obj, "ggcomplex")
})

test_that("gg_matrix with custom geom works", {
  mat <- matrix(rnorm(20), nrow = 4,
                dimnames = list(paste0("R", 1:4), paste0("C", 1:5)))
  obj <- gg_matrix(mat, geom_fn = ggplot2::geom_tile)
  expect_s3_class(obj, "ggcomplex")
  expect_equal(obj$params$plot_type, "matrix")
})
