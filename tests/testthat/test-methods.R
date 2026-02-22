test_that("subset_rows keeps correct rows", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>% subset_rows(c("G1", "G3", "G5"))

  expect_equal(nrow(obj$data$original), 3)
  expect_equal(rownames(obj$data$original), c("G1", "G3", "G5"))
  expect_equal(nlevels(obj$data$tidy$row_id), 3)
})

test_that("subset_cols keeps correct columns", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>% subset_cols(c("S2", "S4"))

  expect_equal(ncol(obj$data$original), 2)
  expect_equal(colnames(obj$data$original), c("S2", "S4"))
})

test_that("subset_rows errors on empty intersection", {
  mat <- matrix(rnorm(12), nrow = 3,
                dimnames = list(c("A", "B", "C"), c("X", "Y", "Z", "W")))
  obj <- gg_heatmap(mat)
  expect_error(subset_rows(obj, c("Z1", "Z2")), "No matching rows")
})

test_that("set_title updates params", {
  mat <- matrix(rnorm(12), nrow = 3,
                dimnames = list(c("A", "B", "C"), c("X", "Y", "Z", "W")))
  obj <- gg_heatmap(mat) %>%
    set_title(title = "Main", subtitle = "Sub", caption = "Cap")

  expect_equal(obj$params$plot_title, "Main")
  expect_equal(obj$params$plot_subtitle, "Sub")
  expect_equal(obj$params$plot_caption, "Cap")
})

test_that("summary prints without error", {
  mat <- matrix(rnorm(20), nrow = 4,
                dimnames = list(paste0("R", 1:4), paste0("C", 1:5)))
  obj <- gg_heatmap(mat) %>% add_row_tree() %>% add_col_tree()

  expect_output(summary(obj), "ggcomplex")
  expect_output(summary(obj), "Row tree.*yes")
})

test_that("assemble_plot returns aplot object", {
  mat <- matrix(rnorm(30), nrow = 5,
                dimnames = list(paste0("G", 1:5), paste0("S", 1:6)))
  obj <- gg_heatmap(mat) %>% add_row_tree()
  ap <- assemble_plot(obj)

  expect_false(is.null(ap))
})

test_that("print.ggcomplex returns invisibly", {
  mat <- matrix(rnorm(20), nrow = 4,
                dimnames = list(paste0("R", 1:4), paste0("C", 1:5)))
  obj <- gg_heatmap(mat)
  expect_invisible(print(obj))
})

test_that("theme_clean_side returns a theme", {
  th <- theme_clean_side()
  expect_s3_class(th, "theme")
})
