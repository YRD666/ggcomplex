#' Add a row-side annotation track
#'
#' Attaches an arbitrary ggplot annotation to the right or left side of the
#' heatmap. The annotation data must contain a column matching the row names
#' of the original matrix so that axes can be aligned.
#'
#' @param obj A `ggcomplex` object.
#' @param anno_data A data frame whose first column (or `id_col`) matches
#'   the row names of the original matrix.
#' @param mapping An aesthetic mapping created by [ggplot2::aes()].
#' @param geom Geometry type: `"bar"`, `"point"`, `"boxplot"`, `"tile"`,
#'   `"violin"`, `"line"`, `"text"`, or `"histogram"`.
#' @param id_col Column name in `anno_data` identifying rows.
#' @param width Relative width for this panel.
#' @param side `"right"` (default) or `"left"`.
#' @param palette Named or unnamed character vector of colours for discrete
#'   fill/colour scales. Passed to [ggplot2::scale_fill_manual()].
#' @param continuous_palette Colour vector (length 2 or 3) for continuous
#'   fill/colour scales. When set, `palette` is ignored and a gradient scale
#'   is used instead.
#' @param fill Single fill colour for the geom (overrides mapping when set).
#' @param color Single outline/stroke colour.
#' @param alpha Transparency, 0--1.
#' @param border_color Border colour for tile/bar geoms.
#' @param border_width Border line width.
#' @param show_legend Logical; show this annotation's legend?
#' @param anno_title Optional title string for this annotation track. Displayed
#'   as a small axis title alongside the panel.
#' @param ylim Numeric length-2 vector limiting the value axis (for bar,
#'   point, line, histogram). `NULL` = auto.
#' @param bar_baseline Baseline value for bar geoms. Defaults to 0.
#' @param text_angle Rotation angle for text geom. Defaults to 0.
#' @param text_size Font size for text geom.
#' @param text_fontface Font face for text geom (`"plain"`, `"bold"`,
#'   `"italic"`).
#' @param line_show_points Logical; for `geom = "line"`, show data points?
#' @param line_point_size Size of points on a line annotation.
#' @param panel_border_color Colour for a border rectangle around the
#'   annotation panel. `NA` (default) = no border.
#' @param panel_border_width Line width of the panel border.
#' @param extra_layers A list of additional ggplot layers (scales, themes).
#' @param ... Additional arguments passed to the geom function.
#'
#' @return Updated `ggcomplex` object.
#' @export
add_row_annotation <- function(obj,
                               anno_data,
                               mapping,
                               geom              = c("bar", "point", "boxplot",
                                                     "tile", "violin", "line",
                                                     "text", "histogram"),
                               id_col            = NULL,
                               width             = 0.25,
                               side              = "right",
                               palette           = NULL,
                               continuous_palette = NULL,
                               fill              = NULL,
                               color             = NULL,
                               alpha             = NULL,
                               border_color      = NA,
                               border_width      = 0.2,
                               show_legend       = TRUE,
                               anno_title        = NULL,
                               ylim              = NULL,
                               bar_baseline      = 0,
                               text_angle        = 0,
                               text_size         = 3,
                               text_fontface     = "plain",
                               line_show_points  = TRUE,
                               line_point_size   = 1,
                               panel_border_color = NA,
                               panel_border_width = 0.5,
                               gap               = NULL,
                               extra_layers      = list(),
                               ...) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  geom <- match.arg(geom)

  id_col <- id_col %||% colnames(anno_data)[1]
  anno_data[[id_col]] <- factor(anno_data[[id_col]],
                                levels = obj$layout$row_order)

  p <- ggplot2::ggplot(anno_data, mapping)

  geom_extra <- build_geom_args(fill, color, alpha, border_color,
                                border_width, mapping, list(...))

  p <- switch(geom,
    bar       = p + do.call(ggplot2::geom_col, geom_extra),
    point     = p + do.call(ggplot2::geom_point, geom_extra),
    boxplot   = p + do.call(ggplot2::geom_boxplot, geom_extra),
    violin    = p + do.call(ggplot2::geom_violin, geom_extra),
    line      = {
      line_args <- c(list(group = 1), geom_extra)
      pp <- p + do.call(ggplot2::geom_line, line_args)
      if (line_show_points) {
        pp <- pp + ggplot2::geom_point(size = line_point_size)
      }
      pp
    },
    histogram = p + do.call(ggplot2::geom_bar,
                            c(list(stat = "identity"), geom_extra)),
    text      = {
      txt_args <- c(geom_extra,
                    list(angle = text_angle, size = text_size,
                         fontface = text_fontface))
      p + do.call(ggplot2::geom_text, txt_args)
    },
    tile      = p + do.call(ggplot2::geom_tile, geom_extra)
  )

  if (!is.null(ylim)) {
    p <- p + ggplot2::coord_flip(ylim = ylim)
  } else {
    p <- p + ggplot2::coord_flip()
  }

  p <- apply_anno_styling(p, palette, continuous_palette, show_legend,
                          extra_layers, discrete_axis = "y",
                          anno_title = anno_title,
                          panel_border_color = panel_border_color,
                          panel_border_width = panel_border_width)

  sp <- gap %||% obj$params$panel_spacing %||% 2
  if (sp > 0) {
    if (side == "left") {
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, sp, 0, 0, "pt"))
    } else {
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, sp, "pt"))
    }
  }

  slot <- if (side == "left") "left" else "right"
  obj$plots[[slot]] <- c(obj$plots[[slot]], list(p))
  if (is.null(obj$params$panel_widths[[slot]])) {
    obj$params$panel_widths[[slot]] <- list()
  }
  obj$params$panel_widths[[slot]] <- c(obj$params$panel_widths[[slot]], width)
  obj$params$side_widths[[slot]] <- width
  obj
}


#' Add a column-side annotation track
#'
#' Attaches an arbitrary ggplot annotation to the top or bottom of the
#' heatmap.
#'
#' @inheritParams add_row_annotation
#' @param height Relative height for this panel.
#' @param side `"top"` (default) or `"bottom"`.
#'
#' @return Updated `ggcomplex` object.
#' @export
add_col_annotation <- function(obj,
                               anno_data,
                               mapping,
                               geom              = c("bar", "point", "boxplot",
                                                     "tile", "violin", "line",
                                                     "text", "histogram"),
                               id_col            = NULL,
                               height            = 0.15,
                               side              = "top",
                               palette           = NULL,
                               continuous_palette = NULL,
                               fill              = NULL,
                               color             = NULL,
                               alpha             = NULL,
                               border_color      = NA,
                               border_width      = 0.2,
                               show_legend       = TRUE,
                               anno_title        = NULL,
                               ylim              = NULL,
                               bar_baseline      = 0,
                               text_angle        = 0,
                               text_size         = 3,
                               text_fontface     = "plain",
                               line_show_points  = TRUE,
                               line_point_size   = 1,
                               panel_border_color = NA,
                               panel_border_width = 0.5,
                               gap               = NULL,
                               extra_layers      = list(),
                               ...) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  geom <- match.arg(geom)

  id_col <- id_col %||% colnames(anno_data)[1]
  anno_data[[id_col]] <- factor(anno_data[[id_col]],
                                levels = obj$layout$col_order)

  p <- ggplot2::ggplot(anno_data, mapping)

  geom_extra <- build_geom_args(fill, color, alpha, border_color,
                                border_width, mapping, list(...))

  p <- switch(geom,
    bar       = p + do.call(ggplot2::geom_col, geom_extra),
    point     = p + do.call(ggplot2::geom_point, geom_extra),
    boxplot   = p + do.call(ggplot2::geom_boxplot, geom_extra),
    violin    = p + do.call(ggplot2::geom_violin, geom_extra),
    line      = {
      line_args <- c(list(group = 1), geom_extra)
      pp <- p + do.call(ggplot2::geom_line, line_args)
      if (line_show_points) {
        pp <- pp + ggplot2::geom_point(size = line_point_size)
      }
      pp
    },
    histogram = p + do.call(ggplot2::geom_bar,
                            c(list(stat = "identity"), geom_extra)),
    text      = {
      txt_args <- c(geom_extra,
                    list(angle = text_angle, size = text_size,
                         fontface = text_fontface))
      p + do.call(ggplot2::geom_text, txt_args)
    },
    tile      = p + do.call(ggplot2::geom_tile, geom_extra)
  )

  if (!is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }

  p <- apply_anno_styling(p, palette, continuous_palette, show_legend,
                          extra_layers, discrete_axis = "x",
                          anno_title = anno_title,
                          panel_border_color = panel_border_color,
                          panel_border_width = panel_border_width)

  sp <- gap %||% obj$params$panel_spacing %||% 2
  if (sp > 0) {
    if (side == "top") {
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(0, 0, sp, 0, "pt"))
    } else {
      p <- p + ggplot2::theme(plot.margin = ggplot2::margin(sp, 0, 0, 0, "pt"))
    }
  }

  slot <- if (side == "top") "top" else "bottom"
  obj$plots[[slot]] <- c(obj$plots[[slot]], list(p))
  if (is.null(obj$params$panel_heights[[slot]])) {
    obj$params$panel_heights[[slot]] <- list()
  }
  obj$params$panel_heights[[slot]] <- c(obj$params$panel_heights[[slot]], height)
  obj$params$side_heights[[slot]] <- height
  obj
}


# ===========================================================================
#  Internal helpers for annotation
# ===========================================================================

#' Build a named list of geom arguments from the convenience parameters
#' @keywords internal
build_geom_args <- function(fill, color, alpha, border_color, border_width,
                            mapping = NULL, dot_args = list()) {
  args <- dot_args
  mapped_aes <- if (!is.null(mapping)) names(mapping) else character(0)

  if (!is.null(fill))  args$fill  <- fill
  if (!is.null(color)) args$color <- color
  if (!is.null(alpha)) args$alpha <- alpha
  if (!is.na(border_color)) {
    if (!"colour" %in% mapped_aes && is.null(args$color)) {
      args$color <- border_color
    }
    args$linewidth <- border_width
  }
  args
}


#' Apply palette, legend, extra layers, and clean theme
#' @keywords internal
apply_anno_styling <- function(p, palette, continuous_palette, show_legend,
                               extra_layers, discrete_axis = "x",
                               anno_title = NULL,
                               panel_border_color = NA,
                               panel_border_width = 0.5) {
  mapped_aes <- names(p$mapping)

  if (!is.null(continuous_palette)) {
    if ("fill" %in% mapped_aes) {
      if (length(continuous_palette) == 2) {
        p <- p + ggplot2::scale_fill_gradient(
          low = continuous_palette[1], high = continuous_palette[2]
        )
      } else {
        p <- p + ggplot2::scale_fill_gradient2(
          low = continuous_palette[1], mid = continuous_palette[2],
          high = continuous_palette[3]
        )
      }
    }
    if ("colour" %in% mapped_aes) {
      if (length(continuous_palette) == 2) {
        p <- p + ggplot2::scale_color_gradient(
          low = continuous_palette[1], high = continuous_palette[2]
        )
      } else {
        p <- p + ggplot2::scale_color_gradient2(
          low = continuous_palette[1], mid = continuous_palette[2],
          high = continuous_palette[3]
        )
      }
    }
  } else if (!is.null(palette)) {
    if ("fill"   %in% mapped_aes) p <- p + ggplot2::scale_fill_manual(values = palette)
    if ("colour" %in% mapped_aes) p <- p + ggplot2::scale_color_manual(values = palette)
  }

  if (discrete_axis == "x") {
    p <- p + ggplot2::scale_x_discrete(expand = c(0, 0))
  } else {
    p <- p + ggplot2::scale_y_discrete(expand = c(0, 0))
  }

  for (layer in extra_layers) p <- p + layer

  p <- p + theme_clean_side(keep_legend = show_legend)

  if (!is.null(anno_title)) {
    if (discrete_axis == "x") {
      p <- p + ggplot2::ylab(anno_title) +
        ggplot2::theme(
          axis.title.y = ggplot2::element_text(size = 8, angle = 90)
        )
    } else {
      p <- p + ggplot2::xlab(anno_title) +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 8)
        )
    }
  }

  if (!is.na(panel_border_color)) {
    p <- p + ggplot2::theme(
      panel.border = ggplot2::element_rect(
        color = panel_border_color, fill = NA, linewidth = panel_border_width
      )
    )
  }

  p
}


# ===========================================================================
#  Convenience wrappers — bar / box / tile
# ===========================================================================

#' @describeIn add_row_annotation Bar chart for row annotations.
#' @export
add_row_bar <- function(obj, anno_data, mapping, width = 0.25, side = "right",
                        palette = NULL, continuous_palette = NULL,
                        fill = NULL, color = NULL, alpha = NULL,
                        show_legend = TRUE, anno_title = NULL, ylim = NULL,
                        ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "bar",
                     width = width, side = side, palette = palette,
                     continuous_palette = continuous_palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     ylim = ylim, ...)
}

#' @describeIn add_row_annotation Boxplot for row annotations.
#' @export
add_row_box <- function(obj, anno_data, mapping, width = 0.25, side = "right",
                        palette = NULL, fill = NULL, color = NULL, alpha = NULL,
                        show_legend = TRUE, anno_title = NULL, ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "boxplot",
                     width = width, side = side, palette = palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title, ...)
}

#' @describeIn add_row_annotation Coloured tile strip for row annotations.
#' @export
add_row_tile <- function(obj, anno_data, mapping, width = 0.08, side = "right",
                         palette = NULL, border_color = "white",
                         border_width = 0.5, show_legend = TRUE,
                         anno_title = NULL, ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "tile",
                     width = width, side = side, palette = palette,
                     border_color = border_color, border_width = border_width,
                     show_legend = show_legend, anno_title = anno_title, ...)
}

#' @describeIn add_col_annotation Bar chart for column annotations.
#' @export
add_col_bar <- function(obj, anno_data, mapping, height = 0.15, side = "top",
                        palette = NULL, continuous_palette = NULL,
                        fill = NULL, color = NULL, alpha = NULL,
                        show_legend = TRUE, anno_title = NULL, ylim = NULL,
                        ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "bar",
                     height = height, side = side, palette = palette,
                     continuous_palette = continuous_palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     ylim = ylim, ...)
}

#' @describeIn add_col_annotation Boxplot for column annotations.
#' @export
add_col_box <- function(obj, anno_data, mapping, height = 0.15, side = "top",
                        palette = NULL, fill = NULL, color = NULL, alpha = NULL,
                        show_legend = TRUE, anno_title = NULL, ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "boxplot",
                     height = height, side = side, palette = palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title, ...)
}

#' @describeIn add_col_annotation Coloured tile strip for column annotations.
#' @export
add_col_tile <- function(obj, anno_data, mapping, height = 0.05, side = "top",
                         palette = NULL, border_color = "white",
                         border_width = 0.5, show_legend = TRUE,
                         anno_title = NULL, ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "tile",
                     height = height, side = side, palette = palette,
                     border_color = border_color, border_width = border_width,
                     show_legend = show_legend, anno_title = anno_title, ...)
}


#' @describeIn add_row_annotation Dot/point plot for row annotations.
#' @export
add_row_dot <- function(obj, anno_data, mapping, width = 0.25, side = "right",
                        palette = NULL, continuous_palette = NULL,
                        fill = NULL, color = NULL, alpha = NULL,
                        show_legend = TRUE, anno_title = NULL, ylim = NULL,
                        ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "point",
                     width = width, side = side, palette = palette,
                     continuous_palette = continuous_palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     ylim = ylim, ...)
}

#' @describeIn add_col_annotation Dot/point plot for column annotations.
#' @export
add_col_dot <- function(obj, anno_data, mapping, height = 0.15, side = "top",
                        palette = NULL, continuous_palette = NULL,
                        fill = NULL, color = NULL, alpha = NULL,
                        show_legend = TRUE, anno_title = NULL, ylim = NULL,
                        ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "point",
                     height = height, side = side, palette = palette,
                     continuous_palette = continuous_palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     ylim = ylim, ...)
}


# ===========================================================================
#  Convenience wrappers — violin / line / text / histogram
# ===========================================================================

#' @describeIn add_row_annotation Violin plot for row annotations.
#' @export
add_row_violin <- function(obj, anno_data, mapping, width = 0.25,
                           side = "right", palette = NULL, fill = NULL,
                           color = NULL, alpha = NULL, show_legend = TRUE,
                           anno_title = NULL, ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "violin",
                     width = width, side = side, palette = palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title, ...)
}

#' @describeIn add_col_annotation Violin plot for column annotations.
#' @export
add_col_violin <- function(obj, anno_data, mapping, height = 0.15,
                           side = "top", palette = NULL, fill = NULL,
                           color = NULL, alpha = NULL, show_legend = TRUE,
                           anno_title = NULL, ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "violin",
                     height = height, side = side, palette = palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title, ...)
}

#' @describeIn add_row_annotation Line plot for row annotations.
#' @export
add_row_line <- function(obj, anno_data, mapping, width = 0.25,
                         side = "right", color = NULL, alpha = NULL,
                         show_legend = TRUE, anno_title = NULL,
                         line_show_points = TRUE, line_point_size = 1, ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "line",
                     width = width, side = side,
                     color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     line_show_points = line_show_points,
                     line_point_size = line_point_size, ...)
}

#' @describeIn add_col_annotation Line plot for column annotations.
#' @export
add_col_line <- function(obj, anno_data, mapping, height = 0.15,
                         side = "top", color = NULL, alpha = NULL,
                         show_legend = TRUE, anno_title = NULL,
                         line_show_points = TRUE, line_point_size = 1, ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "line",
                     height = height, side = side,
                     color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     line_show_points = line_show_points,
                     line_point_size = line_point_size, ...)
}

#' @describeIn add_row_annotation Text labels for row annotations.
#' @export
add_row_text <- function(obj, anno_data, mapping, width = 0.12,
                         side = "right", color = NULL, show_legend = FALSE,
                         anno_title = NULL, text_angle = 0, text_size = 3,
                         text_fontface = "plain", ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "text",
                     width = width, side = side,
                     color = color, show_legend = show_legend,
                     anno_title = anno_title,
                     text_angle = text_angle, text_size = text_size,
                     text_fontface = text_fontface, ...)
}

#' @describeIn add_col_annotation Text labels for column annotations.
#' @export
add_col_text <- function(obj, anno_data, mapping, height = 0.08,
                         side = "top", color = NULL, show_legend = FALSE,
                         anno_title = NULL, text_angle = 0, text_size = 3,
                         text_fontface = "plain", ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "text",
                     height = height, side = side,
                     color = color, show_legend = show_legend,
                     anno_title = anno_title,
                     text_angle = text_angle, text_size = text_size,
                     text_fontface = text_fontface, ...)
}

#' @describeIn add_row_annotation Histogram for row annotations.
#' @export
add_row_histogram <- function(obj, anno_data, mapping, width = 0.2,
                              side = "right", palette = NULL, fill = NULL,
                              color = NULL, alpha = NULL,
                              show_legend = TRUE, anno_title = NULL,
                              ylim = NULL, ...) {
  add_row_annotation(obj, anno_data, mapping, geom = "histogram",
                     width = width, side = side, palette = palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     ylim = ylim, ...)
}

#' @describeIn add_col_annotation Histogram for column annotations.
#' @export
add_col_histogram <- function(obj, anno_data, mapping, height = 0.15,
                              side = "top", palette = NULL, fill = NULL,
                              color = NULL, alpha = NULL,
                              show_legend = TRUE, anno_title = NULL,
                              ylim = NULL, ...) {
  add_col_annotation(obj, anno_data, mapping, geom = "histogram",
                     height = height, side = side, palette = palette,
                     fill = fill, color = color, alpha = alpha,
                     show_legend = show_legend, anno_title = anno_title,
                     ylim = ylim, ...)
}


# ===========================================================================
#  Unified interface: geom_add()
# ===========================================================================

#' Add any subplot to the main plot (unified interface)
#'
#' A single entry-point that wraps every annotation and dendrogram helper.
#' Call it repeatedly in a pipe to attach subplots of any type to any side.
#'
#' @param obj A `ggcomplex` object.
#' @param geom Subplot type: `"bar"`, `"tile"`, `"dot"`, `"boxplot"`,
#'   `"violin"`, `"line"`, `"text"`, `"histogram"`, or `"tree"`.
#' @param side Position: `"right"` (default), `"left"`, `"top"`, or
#'   `"bottom"`. For trees, `"left"` / `"right"` maps to `add_row_tree()`,
#'   `"top"` / `"bottom"` maps to `add_col_tree()`.
#' @param data A data frame for annotation types. Ignored for `geom = "tree"`.
#' @param mapping An aesthetic mapping created by [ggplot2::aes()].
#'   Ignored for `geom = "tree"`.
#' @param size Relative size of the subplot panel. Maps to `width` for
#'   left/right sides, `height` for top/bottom. `NULL` = sensible default.
#' @param palette Discrete colour vector.
#' @param continuous_palette 2- or 3-colour gradient vector.
#' @param fill Fixed fill colour.
#' @param color Fixed colour.
#' @param alpha Transparency.
#' @param border_color Border colour for tile/bar geoms.
#' @param border_width Border line width.
#' @param show_legend Show this panel's legend?
#' @param title Small axis title for the annotation panel.
#' @param ylim Numeric length-2 vector limiting the value axis.
#' @param panel_border_color Panel border colour (`NA` = none).
#' @param panel_border_width Panel border line width.
#' @param gap Override spacing (pt) between this panel and the main plot.
#' @param ... Further arguments forwarded to the underlying function
#'   (e.g. `text_size`, `line_show_points`, `k`, `dist_method`, etc.).
#'
#' @details
#' The function determines row-side vs column-side from `side`:
#'
#' \itemize{
#'   \item `"left"` / `"right"` → row-side annotations / row tree
#'   \item `"top"` / `"bottom"` → column-side annotations / column tree
#' }
#'
#' For `geom = "tree"`, `data` and `mapping` are not required; all tree
#' parameters (`tree`, `order`, `dist_method`, `hclust_method`, `k`, etc.)
#' can be passed via `...`.
#'
#' @return Updated `ggcomplex` object.
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(rnorm(100), 10, 10,
#'               dimnames = list(paste0("G", 1:10), paste0("S", 1:10)))
#' meta_r <- data.frame(gene = rownames(mat), score = runif(10))
#' meta_c <- data.frame(sample = colnames(mat), quality = runif(10, 70, 100))
#'
#' gg_heatmap(mat) %>%
#'   geom_add("tree", side = "left", k = 3) %>%
#'   geom_add("tree", side = "top") %>%
#'   geom_add("bar", side = "right", data = meta_r,
#'            mapping = aes(x = gene, y = score), fill = "steelblue") %>%
#'   geom_add("dot", side = "top", data = meta_c,
#'            mapping = aes(x = sample, y = quality), color = "red")
#' }
geom_add <- function(obj,
                     geom  = c("bar", "tile", "dot", "boxplot", "violin",
                               "line", "text", "histogram", "tree"),
                     side  = "right",
                     data  = NULL,
                     mapping = NULL,
                     size  = NULL,
                     palette           = NULL,
                     continuous_palette = NULL,
                     fill              = NULL,
                     color             = NULL,
                     alpha             = NULL,
                     border_color      = NA,
                     border_width      = 0.2,
                     show_legend       = TRUE,
                     title             = NULL,
                     ylim              = NULL,
                     panel_border_color = NA,
                     panel_border_width = 0.5,
                     gap               = NULL,
                     ...) {

  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")
  geom <- match.arg(geom)
  side <- match.arg(side, c("left", "right", "top", "bottom"))

  is_row <- side %in% c("left", "right")

  # ---- tree shortcut -------------------------------------------------------
  if (geom == "tree") {
    tree_args <- list(obj = obj, gap = gap, ...)
    if (!is.null(color)) tree_args$color <- color
    if (!is.null(size)) {
      if (is_row) tree_args$width <- size else tree_args$height <- size
    }
    fn <- if (is_row) add_row_tree else add_col_tree
    return(do.call(fn, tree_args))
  }

  # ---- annotation geoms ----------------------------------------------------
  if (is.null(data))    rlang::abort("`data` is required for annotation geoms.")
  if (is.null(mapping)) rlang::abort("`mapping` is required for annotation geoms.")

  anno_geom <- switch(geom, dot = "point", geom)

  common <- list(
    obj                = obj,
    anno_data          = data,
    mapping            = mapping,
    geom               = anno_geom,
    side               = side,
    palette            = palette,
    continuous_palette = continuous_palette,
    fill               = fill,
    color              = color,
    alpha              = alpha,
    border_color       = border_color,
    border_width       = border_width,
    show_legend        = show_legend,
    anno_title         = title,
    ylim               = ylim,
    panel_border_color = panel_border_color,
    panel_border_width = panel_border_width,
    gap                = gap
  )

  extra <- list(...)
  args  <- c(common, extra)

  if (is_row) {
    if (!is.null(size)) args$width <- size
    do.call(add_row_annotation, args)
  } else {
    if (!is.null(size)) args$height <- size
    do.call(add_col_annotation, args)
  }
}
