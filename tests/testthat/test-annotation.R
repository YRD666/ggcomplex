test_that("add_row_tree clusters and reorders", {
  set.seed(1)
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>% add_row_tree()

  expect_s3_class(obj$layout$row_tree, "hclust")
  expect_length(obj$layout$row_order, 5)
  expect_equal(levels(obj$data$tidy$row_id), obj$layout$row_order)
})

test_that("add_col_tree clusters and reorders", {
  set.seed(1)
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>% add_col_tree()

  expect_s3_class(obj$layout$col_tree, "hclust")
  expect_length(obj$layout$col_order, 6)
})

test_that("add_row_bar attaches annotation", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  meta <- data.frame(gene = paste0("G", 1:5), score = runif(5))
  obj <- gg_heatmap(mat) %>%
    add_row_bar(meta, ggplot2::aes(x = gene, y = score), fill = "blue")

  expect_length(obj$plots$right, 1)
  expect_s3_class(obj$plots$right[[1]], "gg")
})

test_that("add_col_tile attaches annotation", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  meta <- data.frame(sample = paste0("S", 1:6),
                     group = rep(c("A", "B"), 3))
  obj <- gg_heatmap(mat) %>%
    add_col_tile(meta, ggplot2::aes(x = sample, y = 1, fill = group))

  expect_length(obj$plots$top, 1)
})

test_that("add_row_dot attaches point annotation", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  meta <- data.frame(gene = paste0("G", 1:5), val = runif(5))
  obj <- gg_heatmap(mat) %>%
    add_row_dot(meta, ggplot2::aes(x = gene, y = val))

  expect_length(obj$plots$right, 1)
})

test_that("geom_add tree shortcut works", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>%
    geom_add("tree", side = "left") %>%
    geom_add("tree", side = "top")

  expect_s3_class(obj$layout$row_tree, "hclust")
  expect_s3_class(obj$layout$col_tree, "hclust")
})

test_that("geom_add annotation shortcut works", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  meta <- data.frame(gene = paste0("G", 1:5), score = runif(5))
  obj <- gg_heatmap(mat) %>%
    geom_add("bar", side = "right", data = meta,
             mapping = ggplot2::aes(x = gene, y = score),
             fill = "steelblue")

  expect_length(obj$plots$right, 1)
})
