test_that("scale_matrix none returns identity", {
  mat <- matrix(1:12, nrow = 3)
  expect_identical(scale_matrix(mat, "none"), mat)
})

test_that("scale_matrix row_zscore produces zero mean", {
  set.seed(1)
  mat <- matrix(rnorm(60), nrow = 6,
                dimnames = list(paste0("R", 1:6), paste0("C", 1:10)))
  zmat <- scale_matrix(mat, "row_zscore")

  expect_equal(dim(zmat), dim(mat))
  row_means <- rowMeans(zmat)
  expect_true(all(abs(row_means) < 1e-10))
})

test_that("scale_matrix col_zscore produces zero column means", {
  set.seed(1)
  mat <- matrix(rnorm(60), nrow = 6,
                dimnames = list(paste0("R", 1:6), paste0("C", 1:10)))
  zmat <- scale_matrix(mat, "col_zscore")

  col_means <- colMeans(zmat)
  expect_true(all(abs(col_means) < 1e-10))
})

test_that("scale_matrix minmax produces [0,1] range per row", {
  mat <- matrix(c(1, 5, 3, 2, 8, 4), nrow = 2,
                dimnames = list(c("A", "B"), c("X", "Y", "Z")))
  mm <- scale_matrix(mat, "minmax")

  expect_true(all(mm >= 0 & mm <= 1))
  expect_equal(apply(mm, 1, min), c(A = 0, B = 0))
  expect_equal(apply(mm, 1, max), c(A = 1, B = 1))
})

test_that("scale_matrix log2 adds pseudocount", {
  mat <- matrix(c(0, 1, 3, 7), nrow = 2,
                dimnames = list(c("A", "B"), c("X", "Y")))
  lmat <- scale_matrix(mat, "log2", pseudocount = 1)
  expect_equal(lmat[1, 1], log2(0 + 1))
  expect_equal(lmat[2, 2], log2(7 + 1))
})

test_that("scale_matrix center subtracts row mean", {
  mat <- matrix(c(2, 4, 6, 8), nrow = 2,
                dimnames = list(c("A", "B"), c("X", "Y")))
  cmat <- scale_matrix(mat, "center")
  expect_true(all(abs(rowMeans(cmat)) < 1e-10))
})
