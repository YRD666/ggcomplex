#' Create a circular heatmap
#'
#' Builds a circular (polar) heatmap where **columns** are arranged around
#' the circle (angular axis) and **rows** form concentric rings (radial
#' axis).
#'
#' @param mat A numeric matrix.
#' @param fill_palette Colour vector of length 2 (sequential) or 3 (diverging).
#' @param fill_label Legend title for the fill scale.
#' @param fill_limits Numeric limits; values outside are squished.
#' @param fill_midpoint Midpoint for diverging palettes.
#' @param na_color Colour for `NA` cells.
#' @param border_color Cell border colour.
#' @param border_width Cell border line width.
#' @param inner_radius Radius of the inner hole (data-units).
#' @param track_height Radial height of each row ring.
#' @param ring_gap Default radial gap between rings.
#' @param gap_degree Size of the opening in degrees (0 = full circle).
#' @param col_split A named character/factor vector (one per column) that
#'   splits columns into sectors separated by angular gaps. Names must match
#'   column names.
#' @param split_gap Angular gap between sectors (x-units). Default 1.
#' @param heatmap_border_color Colour for thick borders around the heatmap
#'   ring and sector boundaries. `NA` (default) = no border.
#' @param heatmap_border_width Line width of the heatmap border.
#' @param show_row_labels Show row labels inside the gap opening?
#' @param show_col_labels Show column labels around the outer edge?
#' @param row_label_size Font size for row labels.
#' @param col_label_size Font size for column labels.
#' @param start_angle Starting angle in radians (`NULL` = auto).
#' @param direction 1 = clockwise, -1 = counter-clockwise.
#' @param legend_position Legend position.
#' @param title Plot title.
#' @param ... Currently unused.
#'
#' @return A `ggcomplex` S3 object with `plot_type = "circular"`.
#' @export
gg_circular <- function(mat,
                        fill_palette         = c("#2166AC", "white", "#B2182B"),
                        fill_label           = "Value",
                        fill_limits          = NULL,
                        fill_midpoint        = 0,
                        na_color             = "grey50",
                        border_color         = "white",
                        border_width         = 0.3,
                        inner_radius         = 3,
                        track_height         = 1,
                        ring_gap             = 0.5,
                        gap_degree           = 0,
                        col_split            = NULL,
                        split_gap            = 1,
                        heatmap_border_color = NA,
                        heatmap_border_width = 1,
                        show_row_labels      = FALSE,
                        show_col_labels      = TRUE,
                        row_label_size       = 2.5,
                        col_label_size       = 2.5,
                        start_angle          = NULL,
                        direction            = 1,
                        legend_position      = "right",
                        title                = NULL,
                        ...) {
  mat <- validate_matrix_input(mat)
  tidy <- matrix_to_tidy(mat)
  obj  <- new_ggcomplex(mat, tidy)

  obj$params$plot_type             <- "circular"
  obj$params$fill_palette          <- fill_palette
  obj$params$fill_label            <- fill_label
  obj$params$fill_limits           <- fill_limits
  obj$params$fill_midpoint         <- fill_midpoint
  obj$params$na_color              <- na_color
  obj$params$border_color          <- border_color
  obj$params$border_width          <- border_width
  obj$params$inner_radius          <- inner_radius
  obj$params$track_height          <- track_height
  obj$params$ring_gap              <- ring_gap
  obj$params$gap_degree            <- gap_degree
  obj$params$col_split             <- col_split
  obj$params$split_gap             <- split_gap
  obj$params$heatmap_border_color  <- heatmap_border_color
  obj$params$heatmap_border_width  <- heatmap_border_width
  obj$params$show_row_labels       <- show_row_labels
  obj$params$show_col_labels       <- show_col_labels
  obj$params$row_label_size        <- row_label_size
  obj$params$col_label_size        <- col_label_size
  obj$params$start_angle           <- start_angle
  obj$params$direction             <- direction
  obj$params$legend_position       <- legend_position
  obj$params$plot_title            <- title
  obj$params$rings                 <- list()
  obj$params$circular_tree         <- NULL
  obj$params$inner_links           <- NULL

  obj <- rebuild_circular_plot(obj)
  obj
}


# =========================================================================
#  Outer ring annotations
# =========================================================================

#' Add an outer annotation ring to a circular plot
#'
#' @inheritParams gg_circular
#' @param obj A `ggcomplex` object with `plot_type = "circular"`.
#' @param data Data frame containing annotation values.
#' @param col_var Column in `data` matching the matrix column names.
#' @param fill_var Column in `data` to map to tile fill colour.
#' @param geom Ring geometry: `"tile"`, `"bar"`, `"point"`, `"text"`,
#'   `"line"`, or `"histogram"`.
#' @param palette Named or unnamed colour vector for discrete tile fills.
#' @param continuous_palette 2- or 3-colour vector for continuous tile fills.
#' @param fill Fixed fill colour (for bar/point when no grouping).
#' @param label_var Column for text labels (when `geom = "text"`).
#' @param value_var Column with numeric values (for bar height / point size).
#' @param height Radial height of this ring.
#' @param gap Radial gap before this ring.
#' @param border_color Ring cell border colour.
#' @param border_width Ring cell border width.
#' @param ring_label Text label shown at the gap position.
#' @param ... Currently unused.
#'
#' @return Updated `ggcomplex` object.
#' @export
add_outer_ring <- function(obj, data,
                           col_var = NULL, fill_var = NULL,
                           geom = c("tile", "bar", "point", "text",
                                    "line", "histogram"),
                           palette = NULL, continuous_palette = NULL,
                           fill = NULL, label_var = NULL, value_var = NULL,
                           height = 1, gap = NULL,
                           border_color = "white", border_width = 0.3,
                           ring_label = NULL, ...) {
  if (!is_ggcomplex(obj))
    rlang::abort("`obj` must be a <ggcomplex> object.")
  if ((obj$params$plot_type %||% "") != "circular")
    rlang::abort("`add_outer_ring()` requires a circular plot.")
  geom <- match.arg(geom)

  col_var  <- col_var %||% colnames(data)[1]
  ring_gap <- gap %||% obj$params$ring_gap

  nr <- nrow(obj$data$original)
  inner_r <- obj$params$inner_radius
  th <- obj$params$track_height

  n_existing <- length(obj$params$rings)
  if (n_existing == 0) {
    outer_edge <- inner_r + nr * th
  } else {
    last <- obj$params$rings[[n_existing]]
    outer_edge <- last$y_center + last$height / 2
  }

  y_center <- outer_edge + ring_gap + height / 2

  obj$params$rings <- c(obj$params$rings, list(list(
    type = geom, data = data, col_var = col_var,
    fill_var = fill_var, palette = palette,
    continuous_palette = continuous_palette, fill = fill,
    label_var = label_var, value_var = value_var,
    height = height, y_center = y_center,
    border_color = border_color, border_width = border_width,
    ring_label = ring_label
  )))
  obj <- rebuild_circular_plot(obj)
  obj
}


# =========================================================================
#  Circular dendrogram
# =========================================================================

#' Add a circular dendrogram to a circular plot
#'
#' @param obj A `ggcomplex` object with `plot_type = "circular"`.
#' @param which `"col"` or `"row"`.
#' @param tree Optional pre-computed `hclust` / `dendrogram` object.
#' @param dist_method Distance method.
#' @param hclust_method Agglomeration method.
#' @param dist_fn Optional custom distance function.
#' @param color Branch colour.
#' @param line_size Branch line width.
#' @return Updated `ggcomplex` object.
#' @export
add_circular_tree <- function(obj,
                              which = c("col", "row"),
                              tree = NULL, dist_method = "euclidean",
                              hclust_method = "complete",
                              dist_fn = NULL,
                              color = "grey30", line_size = 0.3) {
  if (!is_ggcomplex(obj))
    rlang::abort("`obj` must be a <ggcomplex> object.")
  if ((obj$params$plot_type %||% "") != "circular")
    rlang::abort("`add_circular_tree()` requires a circular plot.")
  which <- match.arg(which)

  if (!is.null(tree)) {
    hc <- coerce_to_hclust(tree)
  } else {
    res <- compute_cluster(obj$data$original, which, dist_method,
                           hclust_method, dist_fn)
    hc <- res$tree
  }

  if (which == "col") {
    nms <- colnames(obj$data$original)
    new_order <- nms[hc$order]
    obj$layout$col_tree  <- hc
    obj$layout$col_order <- new_order
    obj$data$tidy <- relevel_tidy(obj$data$tidy, new_col_order = new_order)

    obj$params$circular_tree <- list(
      hc = hc, color = color, line_size = line_size
    )
  } else {
    nms <- rownames(obj$data$original)
    new_order <- nms[hc$order]
    obj$layout$row_tree  <- hc
    obj$layout$row_order <- new_order
    obj$data$tidy <- relevel_tidy(obj$data$tidy, new_row_order = new_order)
  }

  obj <- rebuild_circular_plot(obj)
  obj
}


# =========================================================================
#  Inner links (chord / network curves)
# =========================================================================

#' Add inner chord links to a circular plot
#'
#' Draws curved Bézier connections inside the inner circle, producing a
#' chord-diagram / network-graph effect. Each link connects two column
#' positions, curving through the centre of the circle.
#'
#' @param obj A `ggcomplex` object with `plot_type = "circular"`.
#' @param links Data frame with at least *from* and *to* columns whose
#'   values match matrix column names.
#' @param from_var Column in `links` for the source position.
#' @param to_var Column in `links` for the target position.
#' @param color_var Optional column to map to link colour.
#' @param color Fixed colour when `color_var` is `NULL`.
#' @param palette Named colour vector for discrete `color_var`.
#' @param continuous_palette 2- or 3-colour vector for continuous `color_var`.
#' @param linewidth_var Optional column to map to line width.
#' @param linewidth Fixed line width when `linewidth_var` is `NULL`.
#' @param linewidth_range Numeric range for mapped line widths.
#' @param alpha Transparency (0–1).
#' @param curvature How deeply curves dip toward the centre (0 = flat at
#'   inner edge, 1 = passes through the very centre).
#' @param n_points Number of interpolation points per curve.
#' @param ... Currently unused.
#'
#' @return Updated `ggcomplex` object.
#' @export
add_inner_links <- function(obj, links,
                            from_var  = "from",
                            to_var    = "to",
                            color_var = NULL,
                            color     = "grey50",
                            palette   = NULL,
                            continuous_palette = NULL,
                            linewidth_var   = NULL,
                            linewidth       = 0.5,
                            linewidth_range = c(0.3, 2),
                            alpha     = 0.5,
                            curvature = 0.7,
                            n_points  = 50,
                            ...) {
  if (!is_ggcomplex(obj))
    rlang::abort("`obj` must be a <ggcomplex> object.")
  if ((obj$params$plot_type %||% "") != "circular")
    rlang::abort("`add_inner_links()` requires a circular plot.")

  obj$params$inner_links <- list(
    links       = links,
    from_var    = from_var,
    to_var      = to_var,
    color_var   = color_var,
    color       = color,
    palette     = palette,
    continuous_palette = continuous_palette,
    linewidth_var   = linewidth_var,
    linewidth       = linewidth,
    linewidth_range = linewidth_range,
    alpha       = alpha,
    curvature   = curvature,
    n_points    = n_points
  )

  obj <- rebuild_circular_plot(obj)
  obj
}


# =========================================================================
#  Internal: rebuild the entire circular ggplot
# =========================================================================

#' @keywords internal
rebuild_circular_plot <- function(obj) {
  tidy   <- obj$data$tidy
  params <- obj$params

  nr      <- nrow(obj$data$original)
  nc      <- ncol(obj$data$original)
  inner_r <- params$inner_radius
  th      <- params$track_height
  dir     <- params$direction %||% 1
  gap_deg <- params$gap_degree %||% 0

  # -----------------------------------------------------------------
  #  1. Column split → x positions, sector_info, x_map
  # -----------------------------------------------------------------
  col_lvls <- levels(tidy$col_id)

  if (!is.null(params$col_split)) {
    cs <- params$col_split
    if (is.null(names(cs))) names(cs) <- colnames(obj$data$original)
    col_groups <- split(col_lvls, cs[col_lvls])
    new_order  <- unlist(col_groups, use.names = FALSE)
    tidy$col_id <- factor(tidy$col_id, levels = new_order)
    col_lvls <- new_order

    sgap <- params$split_gap %||% 1
    x_map <- setNames(numeric(length(new_order)), new_order)
    sector_info <- list()
    cx <- 1
    gnames <- names(col_groups)
    for (gi in seq_along(col_groups)) {
      cols <- col_groups[[gi]]
      for (cc in cols) { x_map[cc] <- cx; cx <- cx + 1 }
      sector_info[[gi]] <- list(
        name = gnames[gi],
        x_start = x_map[cols[1]],
        x_end   = x_map[cols[length(cols)]]
      )
      if (gi < length(col_groups)) cx <- cx + sgap
    }
    total_data_x <- cx - 1
  } else {
    x_map <- setNames(seq_len(nc), col_lvls)
    total_data_x <- nc
    sector_info <- list(list(name = NULL, x_start = 1, x_end = nc))
  }

  obj$params$.x_map <- x_map
  obj$params$.sector_info <- sector_info

  # -----------------------------------------------------------------
  #  2. Gap geometry
  # -----------------------------------------------------------------
  if (gap_deg > 0 && gap_deg < 360) {
    gap_frac    <- gap_deg / 360
    total_x     <- total_data_x / (1 - gap_frac)
    x_lim       <- c(0.5, total_x + 0.5)
    polar_start <- params$start_angle %||%
                     (pi + pi * gap_deg / 360)
    gap_center_x <- total_data_x + 0.5 + (total_x - total_data_x) / 2
  } else {
    total_x      <- total_data_x
    x_lim        <- c(0.5, total_data_x + 0.5)
    polar_start  <- params$start_angle %||% 0
    gap_center_x <- 0.8
  }

  obj$params$.x_lim       <- x_lim
  obj$params$.total_x     <- total_x
  obj$params$.polar_start <- polar_start

  # -----------------------------------------------------------------
  #  3. Tidy numeric coordinates
  # -----------------------------------------------------------------
  tidy$col_num <- x_map[as.character(tidy$col_id)]
  tidy$row_num <- inner_r + (as.numeric(tidy$row_id) - 1) * th + th / 2

  # -----------------------------------------------------------------
  #  4. Main heatmap tiles
  # -----------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = tidy,
      ggplot2::aes(x = .data$col_num, y = .data$row_num, fill = .data$value),
      color     = params$border_color %||% "white",
      linewidth = params$border_width %||% 0.3,
      width = 1, height = th
    )

  fp <- params$fill_palette
  if (length(fp) == 2) {
    p <- p + ggplot2::scale_fill_gradient(
      low = fp[1], high = fp[2], name = params$fill_label,
      limits = params$fill_limits, oob = scales::squish,
      na.value = params$na_color %||% "grey50"
    )
  } else {
    p <- p + ggplot2::scale_fill_gradient2(
      low = fp[1], mid = fp[2], high = fp[3],
      midpoint = params$fill_midpoint %||% 0,
      name = params$fill_label, limits = params$fill_limits,
      oob = scales::squish, na.value = params$na_color %||% "grey50"
    )
  }

  # -----------------------------------------------------------------
  #  5. Ring annotation layers
  # -----------------------------------------------------------------
  for (ring in params$rings) {
    p <- build_ring_layer(p, ring, obj)
  }

  # -----------------------------------------------------------------
  #  6. Inner links (chord curves through the centre)
  # -----------------------------------------------------------------
  if (!is.null(params$inner_links)) {
    il <- params$inner_links
    lk <- il$links
    fx <- x_map[as.character(lk[[il$from_var]])]
    tx <- x_map[as.character(lk[[il$to_var]])]
    ok <- !is.na(fx) & !is.na(tx)
    lk <- lk[ok, , drop = FALSE]; fx <- fx[ok]; tx <- tx[ok]
    nl <- nrow(lk)

    if (nl > 0) {
      link_r <- if (!is.null(params$col_split) && length(sector_info) > 1) {
        inner_r - 1.3
      } else {
        inner_r
      }
      curv   <- il$curvature %||% 0.7
      y_ctrl <- link_r * (1 - curv)
      npt    <- il$n_points %||% 50
      lcols  <- resolve_link_colors(lk, il)
      llws   <- resolve_link_linewidths(lk, il)

      path_list <- vector("list", nl)
      for (i in seq_len(nl)) {
        tt <- seq(0, 1, length.out = npt)
        x1 <- fx[i]; x2 <- tx[i]; xm <- (x1 + x2) / 2
        bx <- (1 - tt)^2 * x1 + 2 * (1 - tt) * tt * xm + tt^2 * x2
        by <- (1 - tt)^2 * link_r + 2 * (1 - tt) * tt * y_ctrl + tt^2 * link_r
        path_list[[i]] <- data.frame(x = bx, y = by,
                                     grp = paste0("lk", i),
                                     lk_col = lcols[i],
                                     lk_lw  = llws[i])
      }
      lk_df <- do.call(rbind, path_list)
      p <- p +
        ggplot2::geom_path(
          data = lk_df,
          ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp,
                       color = .data$lk_col, linewidth = .data$lk_lw),
          alpha = il$alpha %||% 0.5,
          inherit.aes = FALSE, show.legend = FALSE
        ) +
        ggplot2::scale_color_identity() +
        ggplot2::scale_linewidth_identity()
    }
  }

  # -----------------------------------------------------------------
  #  7. Circular dendrogram
  # -----------------------------------------------------------------
  if (!is.null(params$circular_tree)) {
    ct <- params$circular_tree
    leaf_pos <- x_map[col_lvls[ct$hc$order]]
    paths <- hclust_to_polar_paths(ct$hc, inner_r, leaf_positions = leaf_pos)
    p <- p + ggplot2::geom_path(
      data = paths,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp),
      color = ct$color %||% "grey30",
      linewidth = ct$line_size %||% 0.3,
      inherit.aes = FALSE
    )
  }

  # -----------------------------------------------------------------
  #  8. Heatmap ring borders + sector borders
  # -----------------------------------------------------------------
  hm_outer  <- inner_r + nr * th
  border_col <- params$heatmap_border_color
  border_lw  <- params$heatmap_border_width %||% 1

  if (!is.na(border_col %||% NA)) {
    bpaths <- list(); bid <- 0
    for (s in sector_info) {
      x_lo <- s$x_start - 0.5; x_hi <- s$x_end + 0.5
      n_pts <- max(round((x_hi - x_lo) * 4), 30)
      ax <- seq(x_lo, x_hi, length.out = n_pts)
      bid <- bid + 1
      bpaths[[bid]] <- data.frame(x = ax, y = hm_outer, grp = paste0("bo", bid))
      bid <- bid + 1
      bpaths[[bid]] <- data.frame(x = ax, y = inner_r, grp = paste0("bi", bid))
      bid <- bid + 1
      bpaths[[bid]] <- data.frame(x = c(x_lo, x_lo), y = c(inner_r, hm_outer),
                                  grp = paste0("bl", bid))
      bid <- bid + 1
      bpaths[[bid]] <- data.frame(x = c(x_hi, x_hi), y = c(inner_r, hm_outer),
                                  grp = paste0("br", bid))
    }
    bdf <- do.call(rbind, bpaths)
    p <- p + ggplot2::geom_path(
      data = bdf,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp),
      color = border_col, linewidth = border_lw,
      inherit.aes = FALSE
    )
  }

  # -----------------------------------------------------------------
  #  9. Split group labels (inner ring)
  # -----------------------------------------------------------------
  if (!is.null(params$col_split) && length(sector_info) > 1) {
    label_r  <- inner_r - 0.3
    label_ri <- inner_r - 1.2
    for (s in sector_info) {
      if (is.null(s$name)) next
      lx <- (s$x_start + s$x_end) / 2
      ta <- polar_text_angles(lx, x_lim, polar_start, dir)
      p <- p + ggplot2::annotate(
        "text", x = lx, y = (label_r + label_ri) / 2,
        label = s$name, size = 4, fontface = "bold",
        angle = ta$angle, hjust = 0.5
      )
      if (!is.na(border_col %||% NA)) {
        x_lo <- s$x_start - 0.5; x_hi <- s$x_end + 0.5
        n_pts <- max(round((x_hi - x_lo) * 4), 30)
        ax <- seq(x_lo, x_hi, length.out = n_pts)
        lbl_b <- rbind(
          data.frame(x = ax, y = label_r, grp = paste0("lo_", s$name)),
          data.frame(x = ax, y = label_ri, grp = paste0("li_", s$name)),
          data.frame(x = c(x_lo, x_lo), y = c(label_ri, label_r),
                     grp = paste0("ll_", s$name)),
          data.frame(x = c(x_hi, x_hi), y = c(label_ri, label_r),
                     grp = paste0("lr_", s$name))
        )
        p <- p + ggplot2::geom_path(
          data = lbl_b,
          ggplot2::aes(x = .data$x, y = .data$y, group = .data$grp),
          color = border_col, linewidth = border_lw * 0.7,
          inherit.aes = FALSE
        )
      }
    }
  }

  # -----------------------------------------------------------------
  # 10. Outer edge for labels
  # -----------------------------------------------------------------
  outer_r <- hm_outer
  if (length(params$rings) > 0) {
    last <- params$rings[[length(params$rings)]]
    outer_r <- last$y_center + last$height / 2
  }

  # -----------------------------------------------------------------
  # 11. Column labels
  # -----------------------------------------------------------------
  if (params$show_col_labels %||% TRUE) {
    col_x <- x_map[col_lvls]
    col_df <- data.frame(
      col_num = unname(col_x), label = col_lvls, y = outer_r + 0.8
    )
    ta <- polar_text_angles(col_df$col_num, x_lim, polar_start, dir)
    col_df$angle <- ta$angle; col_df$hjust <- ta$hjust
    p <- p + ggplot2::geom_text(
      data = col_df,
      ggplot2::aes(x = .data$col_num, y = .data$y, label = .data$label,
                   angle = .data$angle, hjust = .data$hjust),
      size = params$col_label_size %||% 2.5, inherit.aes = FALSE
    )
  }

  # -----------------------------------------------------------------
  # 12. Row labels (gap area) — tangential, text extends into the gap
  # -----------------------------------------------------------------
  if (params$show_row_labels %||% FALSE) {
    label_x <- if (gap_deg > 0) total_data_x + 0.5 + 0.1 else 0.8
    row_df <- data.frame(
      row_num = inner_r + (seq_len(nr) - 1) * th + th / 2,
      label   = levels(tidy$row_id)[seq_len(nr)],
      x       = label_x
    )
    x_range     <- x_lim[2] - x_lim[1]
    start_deg   <- polar_start * 180 / pi
    vis_deg     <- start_deg + dir * 360 * (label_x - x_lim[1]) / x_range
    tangent_raw <- (-vis_deg) %% 360
    flip        <- tangent_raw > 90 & tangent_raw < 270
    row_df$angle <- if (flip) tangent_raw - 180 else tangent_raw
    row_df$hjust <- if (flip) 1 else 0
    p <- p + ggplot2::geom_text(
      data = row_df,
      ggplot2::aes(x = .data$x, y = .data$row_num, label = .data$label,
                   angle = .data$angle, hjust = .data$hjust),
      size = params$row_label_size %||% 2.5, inherit.aes = FALSE
    )
  }

  # -----------------------------------------------------------------
  # 13. Ring labels
  # -----------------------------------------------------------------
  for (ring in params$rings) {
    if (!is.null(ring$ring_label)) {
      ta_rl <- polar_text_angles(gap_center_x, x_lim, polar_start, dir)
      p <- p + ggplot2::annotate(
        "text", x = gap_center_x, y = ring$y_center,
        label = ring$ring_label, size = 2.5,
        angle = ta_rl$angle, hjust = 0.5, fontface = "bold"
      )
    }
  }

  # -----------------------------------------------------------------
  # 14. Polar transform + theme
  # -----------------------------------------------------------------
  label_margin <- if (params$show_col_labels %||% TRUE) 2.0 else 0.5
  y_upper <- outer_r + 0.8 + label_margin
  p <- p +
    ggplot2::coord_polar(theta = "x", start = polar_start, direction = dir) +
    ggplot2::scale_x_continuous(limits = x_lim, breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, y_upper), breaks = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = params$legend_position %||% "right")

  if (!is.null(params$plot_title))
    p <- p + ggplot2::ggtitle(params$plot_title)

  obj$plots$main <- p
  obj
}


# =========================================================================
#  Internal helpers
# =========================================================================

#' @keywords internal
polar_text_angles <- function(x_pos, x_lim, polar_start, direction = 1) {
  x_range   <- x_lim[2] - x_lim[1]
  start_deg <- polar_start * 180 / pi
  vis_deg   <- start_deg + direction * 360 * (x_pos - x_lim[1]) / x_range
  raw       <- (90 - vis_deg) %% 360
  flip      <- raw >= 90 & raw <= 270
  list(angle = ifelse(flip, raw - 180, raw),
       hjust = ifelse(flip, 1, 0))
}


#' @keywords internal
build_ring_layer <- function(p, ring, obj) {
  col_levels <- levels(obj$data$tidy$col_id)
  x_map <- obj$params$.x_map

  rdata <- ring$data
  rdata[[ring$col_var]] <- factor(rdata[[ring$col_var]], levels = col_levels)
  if (!is.null(x_map)) {
    rdata$col_num <- x_map[as.character(rdata[[ring$col_var]])]
  } else {
    rdata$col_num <- as.numeric(rdata[[ring$col_var]])
  }
  rdata <- rdata[!is.na(rdata$col_num), , drop = FALSE]

  yc <- ring$y_center; hh <- ring$height

  if (ring$type == "tile") {
    colors <- resolve_ring_colors(rdata, ring)
    p <- p + ggplot2::annotate(
      "rect",
      xmin = rdata$col_num - 0.5, xmax = rdata$col_num + 0.5,
      ymin = yc - hh / 2, ymax = yc + hh / 2,
      fill = colors,
      color = ring$border_color %||% "white",
      linewidth = ring$border_width %||% 0.3
    )
  } else if (ring$type == "bar") {
    vvar <- ring$value_var %||% ring$fill_var
    bar_fill <- ring$fill %||% "steelblue"
    vals <- rdata[[vvar]]
    mx <- max(abs(vals), na.rm = TRUE); if (mx == 0) mx <- 1
    bh <- (vals / mx) * hh; bb <- yc - hh / 2
    p <- p + ggplot2::annotate(
      "rect",
      xmin = rdata$col_num - 0.4, xmax = rdata$col_num + 0.4,
      ymin = bb, ymax = bb + bh, fill = bar_fill, color = NA
    )
  } else if (ring$type == "point") {
    pt_fill <- ring$fill %||% "black"; vvar <- ring$value_var
    if (!is.null(vvar)) {
      pt_df <- data.frame(x = rdata$col_num, y = yc, sz = rdata[[vvar]])
      p <- p + ggplot2::geom_point(
        data = pt_df,
        ggplot2::aes(x = .data$x, y = .data$y, size = .data$sz),
        color = pt_fill, inherit.aes = FALSE, show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::annotate("point", x = rdata$col_num, y = yc,
                                 color = pt_fill, size = 2)
    }
  } else if (ring$type == "text") {
    lvar <- ring$label_var %||% ring$fill_var
    x_lim <- obj$params$.x_lim; pstart <- obj$params$.polar_start %||% 0
    pdir <- obj$params$direction %||% 1
    ta <- polar_text_angles(rdata$col_num, x_lim, pstart, pdir)
    txt_df <- data.frame(x = rdata$col_num, y = yc, label = rdata[[lvar]],
                         angle = ta$angle, hjust = ta$hjust)
    p <- p + ggplot2::geom_text(
      data = txt_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   angle = .data$angle, hjust = .data$hjust),
      size = 2.5, inherit.aes = FALSE
    )
  } else if (ring$type == "line") {
    vvar <- ring$value_var %||% ring$fill_var
    vals <- rdata[[vvar]]
    mx <- max(abs(vals), na.rm = TRUE); if (mx == 0) mx <- 1
    mn <- min(vals, na.rm = TRUE)
    scaled_y <- yc - hh / 2 + (vals - mn) / (max(vals, na.rm = TRUE) - mn + 1e-10) * hh
    line_col <- ring$fill %||% "black"
    line_df <- data.frame(x = rdata$col_num, y = scaled_y)
    line_df <- line_df[order(line_df$x), ]
    p <- p + ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = line_col, linewidth = 0.6, inherit.aes = FALSE
    ) + ggplot2::geom_point(
      data = line_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = line_col, size = 1, inherit.aes = FALSE
    )
  } else if (ring$type == "histogram") {
    vvar <- ring$value_var %||% ring$fill_var
    vals <- rdata[[vvar]]
    mx <- max(abs(vals), na.rm = TRUE); if (mx == 0) mx <- 1
    bh <- (vals / mx) * hh; bb <- yc - hh / 2
    colors <- resolve_ring_colors(rdata, ring)
    p <- p + ggplot2::annotate(
      "rect",
      xmin = rdata$col_num - 0.45, xmax = rdata$col_num + 0.45,
      ymin = bb, ymax = bb + bh, fill = colors,
      color = ring$border_color %||% "white",
      linewidth = ring$border_width %||% 0.2
    )
  }
  p
}


#' @keywords internal
resolve_ring_colors <- function(rdata, ring) {
  fvar <- ring$fill_var
  if (!is.null(ring$continuous_palette) && !is.null(fvar)) {
    vals <- rdata[[fvar]]; rng <- range(vals, na.rm = TRUE)
    if (diff(rng) == 0) rng <- rng + c(-1, 1)
    scaled <- (vals - rng[1]) / diff(rng)
    ramp <- grDevices::colorRamp(ring$continuous_palette)
    grDevices::rgb(ramp(scaled), maxColorValue = 255)
  } else if (!is.null(ring$palette) && !is.null(fvar)) {
    vals <- as.character(rdata[[fvar]]); pal <- ring$palette
    if (is.null(names(pal))) {
      lvls <- unique(vals); names(pal) <- lvls[seq_along(pal)]
    }
    unname(pal[vals])
  } else if (!is.null(ring$fill)) {
    rep(ring$fill, nrow(rdata))
  } else {
    rep("grey80", nrow(rdata))
  }
}


#' @keywords internal
resolve_link_colors <- function(lk, il) {
  cvar <- il$color_var
  if (is.null(cvar)) return(rep(il$color %||% "grey50", nrow(lk)))
  vals <- lk[[cvar]]
  if (!is.null(il$continuous_palette) && is.numeric(vals)) {
    rng <- range(vals, na.rm = TRUE)
    if (diff(rng) == 0) rng <- rng + c(-1, 1)
    scaled <- (vals - rng[1]) / diff(rng)
    ramp <- grDevices::colorRamp(il$continuous_palette)
    grDevices::rgb(ramp(scaled), maxColorValue = 255)
  } else if (!is.null(il$palette)) {
    vals <- as.character(vals); pal <- il$palette
    if (is.null(names(pal))) {
      lvls <- unique(vals); names(pal) <- lvls[seq_along(pal)]
    }
    unname(pal[vals])
  } else {
    rep(il$color %||% "grey50", nrow(lk))
  }
}


#' @keywords internal
resolve_link_linewidths <- function(lk, il) {
  lwvar <- il$linewidth_var
  if (is.null(lwvar)) return(rep(il$linewidth %||% 0.5, nrow(lk)))
  vals <- lk[[lwvar]]
  rng <- range(vals, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(mean(il$linewidth_range %||% c(0.3, 2)),
                                 nrow(lk)))
  scaled <- (vals - rng[1]) / diff(rng)
  lwr <- il$linewidth_range %||% c(0.3, 2)
  lwr[1] + scaled * (lwr[2] - lwr[1])
}


#' @keywords internal
hclust_to_polar_paths <- function(hc, inner_radius,
                                  leaf_positions = NULL, n_arc = 60) {
  n <- length(hc$order)
  if (!is.null(leaf_positions)) {
    leaf_x <- leaf_positions
  } else {
    leaf_x <- rep(NA_real_, n)
    leaf_x[hc$order] <- seq_len(n)
  }

  max_h <- max(hc$height); if (max_h == 0) max_h <- 1
  node_x <- rep(NA_real_, n - 1); node_y <- rep(NA_real_, n - 1)
  paths <- vector("list", 3 * (n - 1)); idx <- 0

  get_pos <- function(k) {
    if (k < 0) list(x = leaf_x[-k], y = inner_radius)
    else        list(x = node_x[k],  y = node_y[k])
  }

  for (i in seq_len(n - 1)) {
    left <- hc$merge[i, 1]; right <- hc$merge[i, 2]
    y_pos <- inner_radius * (1 - hc$height[i] / max_h)
    lp <- get_pos(left); rp <- get_pos(right)
    node_x[i] <- (lp$x + rp$x) / 2; node_y[i] <- y_pos
    gid <- paste0("m", i)
    idx <- idx + 1
    paths[[idx]] <- data.frame(x = rep(lp$x, 2), y = c(lp$y, y_pos),
                               grp = paste0(gid, "L"))
    idx <- idx + 1
    paths[[idx]] <- data.frame(x = rep(rp$x, 2), y = c(rp$y, y_pos),
                               grp = paste0(gid, "R"))
    idx <- idx + 1
    arc_x <- seq(min(lp$x, rp$x), max(lp$x, rp$x), length.out = n_arc)
    paths[[idx]] <- data.frame(x = arc_x, y = rep(y_pos, n_arc),
                               grp = paste0(gid, "A"))
  }
  do.call(rbind, paths[seq_len(idx)])
}
