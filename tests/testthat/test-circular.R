test_that("gg_circular returns valid object", {
  mat <- matrix(rnorm(48), nrow = 4,
                dimnames = list(paste0("G", 1:4), paste0("S", 1:12)))
  obj <- gg_circular(mat)

  expect_s3_class(obj, "ggcomplex")
  expect_equal(obj$params$plot_type, "circular")
  expect_s3_class(obj$plots$main, "gg")
})

test_that("gg_circular with gap_degree works", {
  mat <- matrix(rnorm(48), nrow = 4,
                dimnames = list(paste0("G", 1:4), paste0("S", 1:12)))
  obj <- gg_circular(mat, gap_degree = 30, inner_radius = 3)

  expect_equal(obj$params$gap_degree, 30)
})

test_that("gg_circular with col_split works", {
  mat <- matrix(rnorm(48), nrow = 4,
                dimnames = list(paste0("G", 1:4), paste0("S", 1:12)))
  groups <- setNames(rep(c("A", "B"), each = 6), colnames(mat))
  obj <- gg_circular(mat, col_split = groups)

  expect_false(is.null(obj$params$.sector_info))
  expect_length(obj$params$.sector_info, 2)
})

test_that("add_circular_tree adds column tree", {
  mat <- matrix(rnorm(48), nrow = 4,
                dimnames = list(paste0("G", 1:4), paste0("S", 1:12)))
  obj <- gg_circular(mat) %>% add_circular_tree(which = "col")

  expect_s3_class(obj$layout$col_tree, "hclust")
  expect_false(is.null(obj$params$circular_tree))
})

test_that("add_outer_ring adds a ring", {
  mat <- matrix(rnorm(48), nrow = 4,
                dimnames = list(paste0("G", 1:4), paste0("S", 1:12)))
  meta <- data.frame(sample = colnames(mat),
                     group = rep(c("X", "Y"), 6))
  obj <- gg_circular(mat) %>%
    add_outer_ring(meta, col_var = "sample", fill_var = "group",
                   geom = "tile",
                   palette = c(X = "red", Y = "blue"))

  expect_length(obj$params$rings, 1)
})

test_that("add_inner_links adds chord links", {
  mat <- matrix(rnorm(48), nrow = 4,
                dimnames = list(paste0("G", 1:4), paste0("S", 1:12)))
  links <- data.frame(from = c("S1", "S3"), to = c("S8", "S10"))
  obj <- gg_circular(mat) %>% add_inner_links(links)

  expect_false(is.null(obj$params$inner_links))
})
