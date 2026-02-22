#' Create a composite heatmap
#'
#' Primary entry point for building a composite plot. Accepts a numeric matrix,
#' converts it to tidy format, renders a heatmap via [ggplot2::geom_tile()],
#' and returns a `ggcomplex` object ready for pipeline operations.
#'
#' @param mat A numeric matrix. Row names = features, column names = samples.
#' @param fill_palette Character vector of length 2 or 3. Length 3 gives a
#'   diverging gradient (low, mid, high); length 2 gives a sequential gradient
#'   (low, high). Defaults to `c("#2166AC", "white", "#B2182B")`.
#' @param fill_label Legend title for the fill scale.
#' @param fill_limits Numeric vector of length 2 giving the value range to
#'   display. Values outside this range are squished. `NULL` = auto.
#' @param fill_midpoint Numeric midpoint for diverging palettes. Defaults to 0.
#' @param na_color Colour used for `NA` cells. Defaults to `"grey50"`.
#' @param border_color Tile border colour. Use `NA` for no borders.
#' @param border_width Tile border line width. Defaults to 0.2.
#' @param cell_shape Cell shape: `"rect"` (default tile) or `"circle"`.
#'   Circle cells are useful for correlation matrices.
#' @param show_values Logical; overlay the numeric values as text on each cell?
#' @param value_format [sprintf()] format string for cell values (e.g.
#'   `"%.1f"`). Only used when `show_values = TRUE`.
#' @param value_size Font size of overlaid cell values.
#' @param value_color Colour of overlaid cell values.
#' @param value_auto_color Logical; if `TRUE`, text colour is chosen
#'   automatically (black on light backgrounds, white on dark) and
#'   `value_color` is ignored.
#' @param show_row_names Logical; show row-axis text?
#' @param show_col_names Logical; show column-axis text?
#' @param row_names_size Font size for row names.
#' @param col_names_size Font size for column names.
#' @param col_names_angle Rotation angle for column names.
#' @param row_names_side `"left"` or `"right"`. Where to put row-axis labels.
#' @param col_names_side `"bottom"` or `"top"`. Where to put column-axis labels.
#' @param row_names_face Font face for row names (`"plain"`, `"bold"`,
#'   `"italic"`, `"bold.italic"`).
#' @param col_names_face Font face for column names.
#' @param legend_position Legend position: `"right"`, `"left"`, `"top"`,
#'   `"bottom"`, or `"none"`.
#' @param title Optional title string displayed above the main plot.
#' @param base_size Base font size for the theme.
#' @param row_split Factor or integer. Splits rows into groups separated by
#'   white-space gaps. If an integer, uses [stats::cutree()] on the row
#'   dendrogram (requires [add_row_tree()] first). If a named factor/vector
#'   whose names match row names, rows are grouped accordingly.
#' @param col_split Like `row_split` but for columns.
#' @param split_gap Numeric gap width between split groups (in data units).
#'   Defaults to 0.8.
#' @param panel_border_color Colour for a border rectangle around the main
#'   plot panel. `NA` (default) = no border.
#' @param panel_border_width Line width of the panel border.
#' @param panel_spacing Global spacing (pt) between the main plot and all
#'   surrounding sub-plots. Individual panels can override via their `gap`
#'   argument. Defaults to 2.
#' @param ... Currently unused; reserved for future extensions.
#'
#' @return A `ggcomplex` S3 object.
#'
#' @examples
#' mat <- matrix(rnorm(200), nrow = 20,
#'               dimnames = list(paste0("Gene", 1:20), paste0("S", 1:10)))
#' gg_heatmap(mat)
#' gg_heatmap(mat, show_values = TRUE, value_format = "%.1f",
#'            fill_palette = c("white", "firebrick"))
#'
#' @export
gg_heatmap <- function(mat,
                       fill_palette    = c("#2166AC", "white", "#B2182B"),
                       fill_label      = "Value",
                       fill_limits     = NULL,
                       fill_midpoint   = 0,
                       na_color        = "grey50",
                       border_color    = "grey90",
                       border_width    = 0.2,
                       cell_shape      = c("rect", "circle"),
                       show_values     = FALSE,
                       value_format    = "%.2f",
                       value_size      = 2.5,
                       value_color     = "black",
                       value_auto_color = FALSE,
                       show_row_names  = TRUE,
                       show_col_names  = TRUE,
                       row_names_size  = 8,
                       col_names_size  = 8,
                       col_names_angle = 45,
                       row_names_side  = "left",
                       col_names_side  = "bottom",
                       row_names_face  = "plain",
                       col_names_face  = "plain",
                       legend_position = "right",
                       title           = NULL,
                       base_size       = 11,
                       panel_border_color = NA,
                       panel_border_width = 0.5,
                      row_split       = NULL,
                      col_split       = NULL,
                      split_gap       = 0.8,
                      panel_spacing   = 2,
                      ...) {
 cell_shape <- match.arg(cell_shape)
 mat <- validate_matrix_input(mat)

  tidy <- matrix_to_tidy(mat)
  obj  <- new_ggcomplex(mat, tidy)

  obj$params$plot_type        <- "heatmap"
  obj$params$fill_palette     <- fill_palette
  obj$params$fill_label       <- fill_label
  obj$params$fill_limits      <- fill_limits
  obj$params$fill_midpoint    <- fill_midpoint
  obj$params$na_color         <- na_color
  obj$params$border_color     <- border_color
  obj$params$border_width     <- border_width
  obj$params$cell_shape       <- cell_shape
  obj$params$show_values      <- show_values
  obj$params$value_format     <- value_format
  obj$params$value_size       <- value_size
  obj$params$value_color      <- value_color
  obj$params$value_auto_color <- value_auto_color
  obj$params$show_row_names   <- show_row_names
  obj$params$show_col_names   <- show_col_names
  obj$params$row_names_size   <- row_names_size
  obj$params$col_names_size   <- col_names_size
  obj$params$col_names_angle  <- col_names_angle
  obj$params$row_names_side   <- row_names_side
  obj$params$col_names_side   <- col_names_side
  obj$params$row_names_face   <- row_names_face
  obj$params$col_names_face   <- col_names_face
  obj$params$legend_position  <- legend_position
  obj$params$plot_title          <- title
  obj$params$base_size           <- base_size
  obj$params$panel_border_color  <- panel_border_color
  obj$params$panel_border_width  <- panel_border_width
  obj$params$row_split           <- row_split
  obj$params$col_split           <- col_split
  obj$params$split_gap           <- split_gap
  obj$params$panel_spacing       <- panel_spacing

  obj <- rebuild_main_plot(obj)
  obj
}


#' Create a composite dot plot (bubble chart)
#'
#' Maps values to both colour and point size, producing a bubble / dot-matrix
#' chart. Useful when encoding two numeric dimensions simultaneously (e.g.
#' expression level as colour, significance as size).
#'
#' @inheritParams gg_heatmap
#' @param mat A numeric matrix used for the **colour** mapping.
#' @param size_mat Optional numeric matrix (same dimensions) for the **size**
#'   mapping. If `NULL`, `abs(mat)` is used.
#' @param color_palette Character vector of length 2 or 3.
#' @param color_label Legend title for colour.
#' @param size_label Legend title for size.
#' @param size_range Numeric length-2 vector: min and max point sizes.
#' @param size_threshold Numeric; points with absolute size value below this
#'   threshold are hidden (set to `NA`). Useful for filtering noise in
#'   enrichment dot plots.
#' @param size_reverse Logical; if `TRUE`, smaller values get larger points.
#'   Useful when size encodes p-values.
#' @param point_shape Point shape (integer 0--25). Shapes 21--25 have both
#'   `fill` and `color` aesthetics.
#' @param point_stroke Stroke width for shapes that have a border (21--25).
#' @param bg_color Background tile colour behind dots. `NA` = no background.
#' @param bg_border Background tile border colour.
#'
#' @return A `ggcomplex` S3 object.
#' @export
gg_dotplot <- function(mat,
                       size_mat         = NULL,
                       color_palette    = c("#2166AC", "white", "#B2182B"),
                       color_label      = "Value",
                       size_label       = "Size",
                       size_range       = c(0.5, 5),
                       size_threshold   = NULL,
                       size_reverse     = FALSE,
                       point_shape      = 16,
                       point_stroke     = 0.5,
                       bg_color         = NA,
                       bg_border        = "grey90",
                       na_color         = "grey50",
                       fill_limits      = NULL,
                       fill_midpoint    = 0,
                       show_values      = FALSE,
                       value_format     = "%.2f",
                       value_size       = 2,
                       value_color      = "black",
                       show_row_names   = TRUE,
                       show_col_names   = TRUE,
                       row_names_size   = 8,
                       col_names_size   = 8,
                       col_names_angle  = 45,
                       row_names_side   = "left",
                       col_names_side   = "bottom",
                       row_names_face   = "plain",
                       col_names_face   = "plain",
                       legend_position  = "right",
                       title            = NULL,
                       base_size        = 11,
                       panel_border_color = NA,
                       panel_border_width = 0.5,
                       row_split        = NULL,
                       col_split        = NULL,
                       split_gap        = 0.8,
                       panel_spacing    = 2,
                       ...) {
  mat <- validate_matrix_input(mat)

  tidy <- matrix_to_tidy(mat)

  if (!is.null(size_mat)) {
    size_tidy <- matrix_to_tidy(size_mat)
    tidy$size_value <- size_tidy$value
  } else {
    tidy$size_value <- abs(tidy$value)
  }

  if (!is.null(size_threshold)) {
    tidy$size_value[abs(tidy$size_value) < size_threshold] <- NA
  }

  obj <- new_ggcomplex(mat, tidy)

  obj$params$plot_type        <- "dotplot"
  obj$params$color_palette    <- color_palette
  obj$params$color_label      <- color_label
  obj$params$size_label       <- size_label
  obj$params$size_range       <- size_range
  obj$params$size_threshold   <- size_threshold
  obj$params$size_reverse     <- size_reverse
  obj$params$point_shape      <- point_shape
  obj$params$point_stroke     <- point_stroke
  obj$params$bg_color         <- bg_color
  obj$params$bg_border        <- bg_border
  obj$params$na_color         <- na_color
  obj$params$fill_limits      <- fill_limits
  obj$params$fill_midpoint    <- fill_midpoint
  obj$params$show_values      <- show_values
  obj$params$value_format     <- value_format
  obj$params$value_size       <- value_size
  obj$params$value_color      <- value_color
  obj$params$show_row_names   <- show_row_names
  obj$params$show_col_names   <- show_col_names
  obj$params$row_names_size   <- row_names_size
  obj$params$col_names_size   <- col_names_size
  obj$params$col_names_angle  <- col_names_angle
  obj$params$row_names_side   <- row_names_side
  obj$params$col_names_side   <- col_names_side
  obj$params$row_names_face   <- row_names_face
  obj$params$col_names_face   <- col_names_face
  obj$params$legend_position  <- legend_position
  obj$params$plot_title          <- title
  obj$params$base_size           <- base_size
  obj$params$panel_border_color  <- panel_border_color
  obj$params$panel_border_width  <- panel_border_width
  obj$params$row_split           <- row_split
  obj$params$col_split           <- col_split
  obj$params$split_gap           <- split_gap
  obj$params$panel_spacing       <- panel_spacing

  obj <- rebuild_main_plot(obj)
  obj
}


#' Create a generic matrix plot with a custom geom
#'
#' The most flexible main-plot constructor. Supply a matrix and a geom
#' function, and the function builds a ggcomplex object with that layer.
#'
#' @inheritParams gg_heatmap
#' @param geom_fn A ggplot2 geom function such as `ggplot2::geom_point`.
#' @param mapping An aesthetic mapping created by [ggplot2::aes()]. The
#'   variables `col_id`, `row_id`, and `value` are available.
#' @param fill_palette Low/mid/high colours. Set to `NULL` to skip adding a
#'   colour scale.
#' @param scale_type Which colour scale to add: `"fill"`, `"color"`, or
#'   `"auto"` (inspect the mapping to decide). Set to `"none"` to skip.
#' @param extra_layers A list of additional ggplot layers (scales, themes).
#' @param ... Additional arguments passed to `geom_fn()`.
#'
#' @return A `ggcomplex` S3 object.
#' @export
gg_matrix <- function(mat,
                      geom_fn         = ggplot2::geom_tile,
                      mapping         = ggplot2::aes(fill = .data$value),
                      fill_palette    = c("#2166AC", "white", "#B2182B"),
                      fill_label      = "Value",
                      fill_limits     = NULL,
                      fill_midpoint   = 0,
                      na_color        = "grey50",
                      scale_type      = c("auto", "fill", "color", "none"),
                      show_row_names  = TRUE,
                      show_col_names  = TRUE,
                      row_names_size  = 8,
                      col_names_size  = 8,
                      col_names_angle = 45,
                      row_names_side  = "left",
                      col_names_side  = "bottom",
                      row_names_face  = "plain",
                      col_names_face  = "plain",
                      legend_position = "right",
                      title           = NULL,
                      base_size       = 11,
                      panel_border_color = NA,
                      panel_border_width = 0.5,
                      panel_spacing   = 2,
                      extra_layers    = list(),
                      ...) {
  scale_type <- match.arg(scale_type)
  mat <- validate_matrix_input(mat)

  tidy <- matrix_to_tidy(mat)
  obj  <- new_ggcomplex(mat, tidy)

  obj$params$plot_type        <- "matrix"
  obj$params$geom_fn          <- geom_fn
  obj$params$custom_mapping   <- mapping
  obj$params$fill_palette     <- fill_palette
  obj$params$fill_label       <- fill_label
  obj$params$fill_limits      <- fill_limits
  obj$params$fill_midpoint    <- fill_midpoint
  obj$params$na_color         <- na_color
  obj$params$scale_type       <- scale_type
  obj$params$show_row_names   <- show_row_names
  obj$params$show_col_names   <- show_col_names
  obj$params$row_names_size   <- row_names_size
  obj$params$col_names_size   <- col_names_size
  obj$params$col_names_angle  <- col_names_angle
  obj$params$row_names_side   <- row_names_side
  obj$params$col_names_side   <- col_names_side
  obj$params$row_names_face   <- row_names_face
  obj$params$col_names_face   <- col_names_face
  obj$params$legend_position  <- legend_position
  obj$params$plot_title          <- title
  obj$params$base_size           <- base_size
  obj$params$panel_border_color  <- panel_border_color
  obj$params$panel_border_width  <- panel_border_width
  obj$params$panel_spacing       <- panel_spacing
  obj$params$extra_layers        <- extra_layers
  obj$params$geom_args           <- list(...)

  obj <- rebuild_main_plot(obj)
  obj
}


# ===========================================================================
#  Internal: validate input
# ===========================================================================

#' @keywords internal
validate_matrix_input <- function(mat) {
  if (!is.matrix(mat) && !is.data.frame(mat)) {
    rlang::abort("`mat` must be a matrix or data.frame.")
  }
  if (is.data.frame(mat)) mat <- as.matrix(mat)
  mat
}


# ===========================================================================
#  Internal: rebuild dispatcher
# ===========================================================================

#' Rebuild the main ggplot from current tidy data
#'
#' Called internally whenever factor levels change (e.g. after clustering) so
#' that the rendered plot stays in sync with the tree ordering.
#'
#' @param obj A `ggcomplex` object.
#' @return Updated `ggcomplex` object with a fresh `plots$main`.
#' @keywords internal
rebuild_main_plot <- function(obj) {
  plot_type <- obj$params$plot_type %||% "heatmap"

  tidy <- apply_split_gaps(obj)

  p <- switch(plot_type,
    dotplot = build_dotplot_layer(obj, tidy),
    matrix  = build_matrix_layer(obj, tidy),
    build_heatmap_layer(obj, tidy)
  )

  show_row_names  <- obj$params$show_row_names  %||% TRUE
  show_col_names  <- obj$params$show_col_names  %||% TRUE
  row_names_size  <- obj$params$row_names_size  %||% 8
  col_names_size  <- obj$params$col_names_size  %||% 8
  col_names_angle <- obj$params$col_names_angle %||% 45
  legend_position <- obj$params$legend_position %||% "right"
  row_names_side  <- obj$params$row_names_side  %||% "left"
  col_names_side  <- obj$params$col_names_side  %||% "bottom"
  row_names_face  <- obj$params$row_names_face  %||% "plain"
  col_names_face  <- obj$params$col_names_face  %||% "plain"
  base_size       <- obj$params$base_size       %||% 11
  plot_title      <- obj$params$plot_title

  x_pos <- if (col_names_side == "top") "top" else "bottom"
  y_pos <- if (row_names_side == "right") "right" else "left"

  p <- p +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = x_pos) +
    ggplot2::scale_y_discrete(expand = c(0, 0), position = y_pos) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title  = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank(),
      axis.text.x = if (show_col_names)
        ggplot2::element_text(
          angle = col_names_angle,
          hjust = if (col_names_angle > 0) 1 else 0.5,
          size  = col_names_size,
          face  = col_names_face
        )
      else ggplot2::element_blank(),
      axis.text.y = if (show_row_names)
        ggplot2::element_text(size = row_names_size, face = row_names_face)
      else ggplot2::element_blank(),
      legend.position = legend_position
    )

  border_color <- obj$params$panel_border_color %||% NA
  border_width <- obj$params$panel_border_width %||% 0.5
  if (!is.na(border_color)) {
    p <- p + ggplot2::theme(
      panel.border = ggplot2::element_rect(
        color = border_color, fill = NA, linewidth = border_width
      )
    )
  }

  if (!is.null(plot_title)) {
    p <- p + ggplot2::ggtitle(plot_title)
  }

  obj$plots$main <- p
  obj
}


# ===========================================================================
#  Internal: split gap logic
# ===========================================================================

#' Insert visual gaps between row/col split groups
#' @keywords internal
apply_split_gaps <- function(obj) {
  tidy      <- obj$data$tidy
  row_split <- obj$params$row_split
  col_split <- obj$params$col_split

  if (is.null(row_split) && is.null(col_split)) return(tidy)

  if (!is.null(row_split)) {
    tidy <- inject_split(tidy, "row_id", row_split, obj)
  }
  if (!is.null(col_split)) {
    tidy <- inject_split(tidy, "col_id", col_split, obj)
  }
  tidy
}


#' @keywords internal
inject_split <- function(tidy, axis, split_var, obj) {
  current_levels <- levels(tidy[[axis]])

  if (is.numeric(split_var) && length(split_var) == 1) {
    tree_slot <- if (axis == "row_id") "row_tree" else "col_tree"
    hc <- obj$layout[[tree_slot]]
    if (is.null(hc)) {
      rlang::warn(paste0(
        "Integer `", if (axis == "row_id") "row_split" else "col_split",
        "` requires a dendrogram. Ignoring."
      ))
      return(tidy)
    }
    grp <- stats::cutree(hc, k = split_var)
    split_var <- grp
  }

  if (!is.null(names(split_var))) {
    groups <- split_var[current_levels]
  } else {
    groups <- split_var[seq_along(current_levels)]
  }

  grp_rle <- rle(as.character(groups))
  new_levels <- character(0)
  idx <- 1
  for (i in seq_along(grp_rle$lengths)) {
    block <- current_levels[idx:(idx + grp_rle$lengths[i] - 1)]
    if (i > 1) {
      gap_label <- paste0(".gap_", i)
      new_levels <- c(new_levels, gap_label)
    }
    new_levels <- c(new_levels, block)
    idx <- idx + grp_rle$lengths[i]
  }

  tidy[[axis]] <- factor(tidy[[axis]], levels = new_levels)
  tidy
}


# ===========================================================================
#  Internal: layer builders
# ===========================================================================

#' @keywords internal
build_heatmap_layer <- function(obj, tidy = NULL) {
  tidy         <- tidy %||% obj$data$tidy
  fill_palette <- obj$params$fill_palette
  fill_label   <- obj$params$fill_label
  fill_limits  <- obj$params$fill_limits
  fill_midpoint <- obj$params$fill_midpoint %||% 0
  na_color     <- obj$params$na_color     %||% "grey50"
  border_color <- obj$params$border_color %||% "grey90"
  border_width <- obj$params$border_width %||% 0.2
  cell_shape   <- obj$params$cell_shape   %||% "rect"
  show_values  <- obj$params$show_values  %||% FALSE
  value_format <- obj$params$value_format %||% "%.2f"
  value_size   <- obj$params$value_size   %||% 2.5
  value_color  <- obj$params$value_color  %||% "black"
  value_auto_color <- obj$params$value_auto_color %||% FALSE

  p <- ggplot2::ggplot(tidy, ggplot2::aes(
    x = .data$col_id, y = .data$row_id, fill = .data$value
  ))

  if (cell_shape == "circle") {
    p <- p +
      ggplot2::geom_tile(fill = "white", color = NA) +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$value),
        shape = 19, size = 4, show.legend = FALSE
      )
  } else {
    p <- p + ggplot2::geom_tile(
      color     = border_color,
      linewidth = border_width
    )
  }

  if (length(fill_palette) == 2) {
    p <- p + ggplot2::scale_fill_gradient(
      low = fill_palette[1], high = fill_palette[2],
      name = fill_label, limits = fill_limits,
      oob = scales::squish, na.value = na_color
    )
    if (cell_shape == "circle") {
      p <- p + ggplot2::scale_color_gradient(
        low = fill_palette[1], high = fill_palette[2],
        limits = fill_limits, oob = scales::squish,
        na.value = na_color, guide = "none"
      )
    }
  } else {
    p <- p + ggplot2::scale_fill_gradient2(
      low = fill_palette[1], mid = fill_palette[2], high = fill_palette[3],
      midpoint = fill_midpoint,
      name = fill_label, limits = fill_limits,
      oob = scales::squish, na.value = na_color
    )
    if (cell_shape == "circle") {
      p <- p + ggplot2::scale_color_gradient2(
        low = fill_palette[1], mid = fill_palette[2], high = fill_palette[3],
        midpoint = fill_midpoint,
        limits = fill_limits, oob = scales::squish,
        na.value = na_color, guide = "none"
      )
    }
  }

  if (show_values) {
    if (value_auto_color) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          label = sprintf(value_format, .data$value),
          color = ggplot2::after_stat(
            ifelse(.data$value > mean(range(.data$value, na.rm = TRUE)),
                   "white", "black")
          )
        ),
        size = value_size, na.rm = TRUE, show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = sprintf(value_format, .data$value)),
        color = value_color, size = value_size, na.rm = TRUE
      )
    }
  }

  p
}


#' @keywords internal
build_dotplot_layer <- function(obj, tidy = NULL) {
  tidy          <- tidy %||% obj$data$tidy
  color_palette <- obj$params$color_palette
  color_label   <- obj$params$color_label
  size_label    <- obj$params$size_label
  size_range    <- obj$params$size_range
  size_reverse  <- obj$params$size_reverse  %||% FALSE
  point_shape   <- obj$params$point_shape   %||% 16
  point_stroke  <- obj$params$point_stroke  %||% 0.5
  bg_color      <- obj$params$bg_color
  bg_border     <- obj$params$bg_border     %||% "grey90"
  na_color      <- obj$params$na_color      %||% "grey50"
  fill_limits   <- obj$params$fill_limits
  fill_midpoint <- obj$params$fill_midpoint %||% 0
  show_values   <- obj$params$show_values   %||% FALSE
  value_format  <- obj$params$value_format  %||% "%.2f"
  value_size    <- obj$params$value_size    %||% 2
  value_color   <- obj$params$value_color   %||% "black"

  use_fill_shape <- point_shape %in% 21:25

  p <- ggplot2::ggplot(tidy, ggplot2::aes(
    x = .data$col_id, y = .data$row_id
  ))

  if (!is.na(bg_color)) {
    p <- p + ggplot2::geom_tile(fill = bg_color, color = bg_border,
                                linewidth = 0.2)
  }

  if (use_fill_shape) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(fill = .data$value, size = .data$size_value),
      shape  = point_shape,
      stroke = point_stroke,
      color  = "grey30"
    )
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = .data$value, size = .data$size_value),
      shape  = point_shape,
      stroke = point_stroke
    )
  }

  scale_fn <- if (use_fill_shape) "fill" else "color"

  if (length(color_palette) == 2) {
    grad <- ggplot2::scale_fill_gradient
    if (scale_fn == "color") grad <- ggplot2::scale_color_gradient
    p <- p + grad(
      low = color_palette[1], high = color_palette[2],
      name = color_label, limits = fill_limits,
      oob = scales::squish, na.value = na_color
    )
  } else {
    grad2 <- ggplot2::scale_fill_gradient2
    if (scale_fn == "color") grad2 <- ggplot2::scale_color_gradient2
    p <- p + grad2(
      low = color_palette[1], mid = color_palette[2], high = color_palette[3],
      midpoint = fill_midpoint,
      name = color_label, limits = fill_limits,
      oob = scales::squish, na.value = na_color
    )
  }

  size_trans <- if (size_reverse) "reverse" else "identity"
  p <- p + ggplot2::scale_size_continuous(
    range = size_range, name = size_label, trans = size_trans
  )

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf(value_format, .data$value)),
      color = value_color, size = value_size, na.rm = TRUE
    )
  }

  p
}


#' @keywords internal
build_matrix_layer <- function(obj, tidy = NULL) {
  tidy       <- tidy %||% obj$data$tidy
  geom_fn    <- obj$params$geom_fn
  mapping    <- obj$params$custom_mapping
  palette    <- obj$params$fill_palette
  label      <- obj$params$fill_label
  limits     <- obj$params$fill_limits
  midpoint   <- obj$params$fill_midpoint %||% 0
  na_color   <- obj$params$na_color      %||% "grey50"
  scale_type <- obj$params$scale_type    %||% "auto"
  extras     <- obj$params$extra_layers  %||% list()
  args       <- obj$params$geom_args     %||% list()

  base_mapping <- ggplot2::aes(x = .data$col_id, y = .data$row_id)
  full_mapping <- modifyList(base_mapping, mapping)

  p <- ggplot2::ggplot(tidy, full_mapping) +
    do.call(geom_fn, args)

  if (!is.null(palette) && scale_type != "none") {
    use_scale <- scale_type
    if (use_scale == "auto") {
      mapped <- names(mapping)
      if ("fill" %in% mapped)        use_scale <- "fill"
      else if ("colour" %in% mapped) use_scale <- "color"
      else                            use_scale <- "fill"
    }

    if (length(palette) == 2) {
      grad <- if (use_scale == "color") ggplot2::scale_color_gradient
              else ggplot2::scale_fill_gradient
      p <- p + grad(
        low = palette[1], high = palette[2], name = label,
        limits = limits, oob = scales::squish, na.value = na_color
      )
    } else {
      grad2 <- if (use_scale == "color") ggplot2::scale_color_gradient2
               else ggplot2::scale_fill_gradient2
      p <- p + grad2(
        low = palette[1], mid = palette[2], high = palette[3],
        midpoint = midpoint,
        name = label, limits = limits, oob = scales::squish,
        na.value = na_color
      )
    }
  }

  for (layer in extras) p <- p + layer
  p
}
