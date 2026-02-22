test_that("compute_cluster returns correct structure", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))

  res <- ggcomplex:::compute_cluster(mat, "row")
  expect_type(res, "list")
  expect_s3_class(res$tree, "hclust")
  expect_length(res$order, 5)
  expect_true(all(res$order %in% rownames(mat)))

  res_col <- ggcomplex:::compute_cluster(mat, "col")
  expect_length(res_col$order, 6)
  expect_true(all(res_col$order %in% colnames(mat)))
})

test_that("add_row_tree re-orders factor levels", {
  set.seed(42)
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>% add_row_tree()

  tree_order <- obj$layout$row_order
  tidy_levels <- levels(obj$data$tidy$row_id)
  expect_equal(tree_order, tidy_levels)
})
