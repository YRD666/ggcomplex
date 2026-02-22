#' Print a ggcomplex object
#'
#' Assembles all registered sub-plots (trees, annotations) around the main
#' heatmap using [aplot] and prints the result.
#'
#' @param x A `ggcomplex` object.
#' @param ... Ignored.
#'
#' @return `x` (invisibly).
#' @export
print.ggcomplex <- function(x, ...) {
  if ((x$params$plot_type %||% "") == "circular") {
    print(x$plots$main)
  } else {
    assembled <- assemble_plot(x)
    print(assembled)
  }
  invisible(x)
}


#' Assemble the composite plot
#'
#' Reads the `plots` and `params` slots of a `ggcomplex` object and uses
#' [aplot::insert_left()], [aplot::insert_right()], [aplot::insert_top()], and
#' [aplot::insert_bottom()] to compose the final figure.
#'
#' @param obj A `ggcomplex` object.
#' @return An `aplot` object ready for printing.
#' @export
assemble_plot <- function(obj) {
  validate_ggcomplex(obj)

  final <- obj$plots$main

  left_widths  <- obj$params$panel_widths$left    %||% list()
  right_widths <- obj$params$panel_widths$right   %||% list()
  top_heights  <- obj$params$panel_heights$top    %||% list()
  bot_heights  <- obj$params$panel_heights$bottom %||% list()

  default_lw <- obj$params$side_widths$left    %||% 0.15
  default_rw <- obj$params$side_widths$right   %||% 0.25
  default_th <- obj$params$side_heights$top    %||% 0.08
  default_bh <- obj$params$side_heights$bottom %||% 0.08

  for (i in seq_along(obj$plots$left)) {
    w <- if (i <= length(left_widths)) left_widths[[i]] else default_lw
    final <- aplot::insert_left(final, obj$plots$left[[i]], width = w)
  }

  for (i in seq_along(obj$plots$right)) {
    w <- if (i <= length(right_widths)) right_widths[[i]] else default_rw
    final <- aplot::insert_right(final, obj$plots$right[[i]], width = w)
  }

  for (i in seq_along(obj$plots$top)) {
    h <- if (i <= length(top_heights)) top_heights[[i]] else default_th
    final <- aplot::insert_top(final, obj$plots$top[[i]], height = h)
  }

  for (i in seq_along(obj$plots$bottom)) {
    h <- if (i <= length(bot_heights)) bot_heights[[i]] else default_bh
    final <- aplot::insert_bottom(final, obj$plots$bottom[[i]], height = h)
  }

  final
}


#' Set global titles for a ggcomplex plot
#'
#' Adds a title, subtitle, and/or caption that will be rendered above/below
#' the entire assembled composite figure.
#'
#' @param obj A `ggcomplex` object.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption (bottom-right).
#'
#' @return Updated `ggcomplex` object.
#' @export
set_title <- function(obj, title = NULL, subtitle = NULL, caption = NULL) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  obj$params$plot_title    <- title
  obj$params$plot_subtitle <- subtitle
  obj$params$plot_caption  <- caption
  obj
}


#' Save a ggcomplex object to a file
#'
#' Convenience wrapper that assembles the composite plot and saves it via
#' [ggplot2::ggsave()].
#'
#' @param obj A `ggcomplex` object.
#' @param filename Output file path (e.g. `"plot.pdf"`, `"fig.png"`).
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution for raster output (png, tiff). Defaults to 300.
#' @param ... Additional arguments passed to [ggplot2::ggsave()].
#'
#' @return The file path (invisibly).
#' @export
gc_save <- function(obj, filename, width = 10, height = 8, dpi = 300, ...) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  if ((obj$params$plot_type %||% "") == "circular") {
    plot_out <- obj$plots$main
  } else {
    plot_out <- assemble_plot(obj)
  }
  ggplot2::ggsave(filename, plot = plot_out, width = width, height = height,
                  dpi = dpi, ...)
  invisible(filename)
}


#' Summary method for ggcomplex
#'
#' Prints a human-readable overview of the composite plot structure.
#'
#' @param object A `ggcomplex` object.
#' @param ... Ignored.
#' @export
summary.ggcomplex <- function(object, ...) {
  mat <- object$data$original
  cat("<ggcomplex> composite plot\n")
  cat(sprintf("  Plot type: %s\n", object$params$plot_type %||% "heatmap"))
  cat(sprintf("  Matrix   : %d rows x %d cols\n", nrow(mat), ncol(mat)))
  cat(sprintf("  Row tree : %s\n",
              if (!is.null(object$layout$row_tree)) "yes" else "no"))
  cat(sprintf("  Col tree : %s\n",
              if (!is.null(object$layout$col_tree)) "yes" else "no"))
  if ((object$params$plot_type %||% "") == "circular") {
    cat(sprintf("  Outer rings  : %d\n", length(object$params$rings)))
    cat(sprintf("  Inner tree   : %s\n",
                if (!is.null(object$params$circular_tree)) "yes" else "no"))
  } else {
    cat(sprintf("  Left panels  : %d\n", length(object$plots$left)))
    cat(sprintf("  Right panels : %d\n", length(object$plots$right)))
    cat(sprintf("  Top panels   : %d\n", length(object$plots$top)))
    cat(sprintf("  Bottom panels: %d\n", length(object$plots$bottom)))
    if (!is.null(object$params$row_split)) cat("  Row split: yes\n")
    if (!is.null(object$params$col_split)) cat("  Col split: yes\n")
  }
  invisible(object)
}


#' Subset rows of a ggcomplex object
#'
#' Re-creates the main plot with only the specified rows.
#'
#' @param obj A `ggcomplex` object.
#' @param rows Character vector of row names to keep.
#'
#' @return Updated `ggcomplex` object.
#' @export
subset_rows <- function(obj, rows) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  keep <- intersect(rows, rownames(obj$data$original))
  if (length(keep) == 0) rlang::abort("No matching rows found.")

  obj$data$original <- obj$data$original[keep, , drop = FALSE]
  obj$data$tidy <- obj$data$tidy[obj$data$tidy$row_id %in% keep, ]
  obj$data$tidy$row_id <- factor(obj$data$tidy$row_id, levels = keep)
  obj$layout$row_order <- keep
  obj$layout$row_tree <- NULL
  obj <- rebuild_main_plot(obj)
  obj
}


#' Subset columns of a ggcomplex object
#'
#' Re-creates the main plot with only the specified columns.
#'
#' @param obj A `ggcomplex` object.
#' @param cols Character vector of column names to keep.
#'
#' @return Updated `ggcomplex` object.
#' @export
subset_cols <- function(obj, cols) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  keep <- intersect(cols, colnames(obj$data$original))
  if (length(keep) == 0) rlang::abort("No matching columns found.")

  obj$data$original <- obj$data$original[, keep, drop = FALSE]
  obj$data$tidy <- obj$data$tidy[obj$data$tidy$col_id %in% keep, ]
  obj$data$tidy$col_id <- factor(obj$data$tidy$col_id, levels = keep)
  obj$layout$col_order <- keep
  obj$layout$col_tree <- NULL
  obj <- rebuild_main_plot(obj)
  obj
}
