test_that("gc_palette returns correct length vectors", {
  div <- gc_palette("RdBu")
  expect_length(div, 3)

  seq_pal <- gc_palette("viridis")
  expect_length(seq_pal, 2)
})

test_that("gc_palette rejects unknown names", {
  expect_error(gc_palette("nonexistent"), "Unknown palette")
})

test_that("gc_cat_palette returns colours", {
  cols <- gc_cat_palette("npg")
  expect_length(cols, 10)
  expect_true(all(grepl("^#", cols)))
})

test_that("gc_cat_palette respects n argument", {
  cols5 <- gc_cat_palette("jco", n = 5)
  expect_length(cols5, 5)
})

test_that("gc_cat_palette interpolates beyond palette size", {
  cols20 <- gc_cat_palette("npg", n = 20)
  expect_length(cols20, 20)
})

test_that("gc_cat_palette rejects unknown names", {
  expect_error(gc_cat_palette("nonexistent"), "Unknown palette")
})

test_that("gc_palette_names runs without error", {
  expect_invisible(gc_palette_names())
})
