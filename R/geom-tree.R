#' Add a row dendrogram to the composite plot
#'
#' Performs hierarchical clustering on the rows of the data matrix, renders a
#' dendrogram with [ggtree::ggtree()], and inserts it into the left panel.
#' **The main heatmap is automatically re-ordered** to match the tree leaves.
#'
#' @param obj A `ggcomplex` object created by [gg_heatmap()].
#' @param tree Optional pre-computed tree object (`hclust`, `dendrogram`, or
#'   `phylo`). When supplied the internal clustering step is skipped.
#' @param order Optional character vector giving a custom row order. Useful
#'   when the tree has already been computed externally. If `tree` is given
#'   without `order`, the order is derived from `tree`.
#' @param dist_method Distance method for [stats::dist()]. Also accepts
#'   `"correlation"`, `"spearman"`, `"kendall"`.
#' @param hclust_method Agglomeration method for [stats::hclust()].
#' @param dist_fn Optional custom distance function (overrides `dist_method`).
#' @param width Relative width of the tree panel.
#' @param line_size Line width of dendrogram branches.
#' @param color Branch colour. Can be a single colour string, or a named
#'   vector to colour branches by sub-tree (requires `k` to be set).
#' @param linetype Line type (`"solid"`, `"dashed"`, `"dotted"`, etc.).
#' @param k Number of clusters for branch colouring.
#' @param k_colors Character vector of `k` colours.
#' @param cut_height Optional height at which to draw a horizontal cut line.
#' @param cut_linetype Linetype for the cut line.
#' @param show_tip_labels Logical; show leaf/tip labels on the dendrogram?
#' @param tip_label_size Font size for tip labels.
#' @param gap Override spacing (pt) between this panel and the main plot.
#'   `NULL` uses the global `panel_spacing` value.
#'
#' @return Updated `ggcomplex` object.
#' @export
add_row_tree <- function(obj,
                         tree            = NULL,
                         order           = NULL,
                         dist_method     = "euclidean",
                         hclust_method   = "complete",
                         dist_fn         = NULL,
                         width           = NULL,
                         line_size       = 0.5,
                         color           = "grey30",
                         linetype        = "solid",
                         k               = NULL,
                         k_colors        = NULL,
                         cut_height      = NULL,
                         cut_linetype    = "dashed",
                         show_tip_labels = FALSE,
                         tip_label_size  = 2.5,
                         gap             = NULL) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")

  if (!is.null(tree)) {
    hc <- coerce_to_hclust(tree)
    nms <- rownames(obj$data$original)
    row_order <- order %||% nms[hc$order]
  } else {
    res <- compute_cluster(obj$data$original, "row", dist_method,
                           hclust_method, dist_fn)
    hc <- res$tree
    row_order <- order %||% res$order
  }

  obj$layout$row_tree  <- hc
  obj$layout$row_order <- row_order

  obj$data$tidy <- relevel_tidy(obj$data$tidy, new_row_order = row_order)
  obj <- rebuild_main_plot(obj)

  p_tree <- build_tree_plot(
    hc,
    direction       = "row",
    line_size       = line_size,
    color           = color,
    linetype        = linetype,
    k               = k,
    k_colors        = k_colors,
    cut_height      = cut_height,
    cut_linetype    = cut_linetype,
    show_tip_labels = show_tip_labels,
    tip_label_size  = tip_label_size
  )

  sp <- gap %||% obj$params$panel_spacing %||% 2
  if (sp > 0) {
    p_tree <- p_tree +
      ggplot2::theme(plot.margin = ggplot2::margin(0, sp, 0, 0, "pt"))
  }

  if (!is.null(width)) obj$params$side_widths$left <- width
  obj$plots$left <- c(list(tree = p_tree), obj$plots$left)
  obj
}


#' Add a column dendrogram to the composite plot
#'
#' Performs hierarchical clustering on the columns of the data matrix, renders
#' a dendrogram, and inserts it into the top panel.
#'
#' @inheritParams add_row_tree
#' @param height Relative height of the tree panel.
#'
#' @return Updated `ggcomplex` object.
#' @export
add_col_tree <- function(obj,
                         tree            = NULL,
                         order           = NULL,
                         dist_method     = "euclidean",
                         hclust_method   = "complete",
                         dist_fn         = NULL,
                         height          = NULL,
                         line_size       = 0.5,
                         color           = "grey30",
                         linetype        = "solid",
                         k               = NULL,
                         k_colors        = NULL,
                         cut_height      = NULL,
                         cut_linetype    = "dashed",
                         show_tip_labels = FALSE,
                         tip_label_size  = 2.5,
                         gap             = NULL) {
  if (!is_ggcomplex(obj)) rlang::abort("`obj` must be a <ggcomplex> object.")

  if (!is.null(tree)) {
    hc <- coerce_to_hclust(tree)
    nms <- colnames(obj$data$original)
    col_order <- order %||% nms[hc$order]
  } else {
    res <- compute_cluster(obj$data$original, "col", dist_method,
                           hclust_method, dist_fn)
    hc <- res$tree
    col_order <- order %||% res$order
  }

  obj$layout$col_tree  <- hc
  obj$layout$col_order <- col_order

  obj$data$tidy <- relevel_tidy(obj$data$tidy, new_col_order = col_order)
  obj <- rebuild_main_plot(obj)

  p_tree <- build_tree_plot(
    hc,
    direction       = "col",
    line_size       = line_size,
    color           = color,
    linetype        = linetype,
    k               = k,
    k_colors        = k_colors,
    cut_height      = cut_height,
    cut_linetype    = cut_linetype,
    show_tip_labels = show_tip_labels,
    tip_label_size  = tip_label_size
  )

  sp <- gap %||% obj$params$panel_spacing %||% 2
  if (sp > 0) {
    p_tree <- p_tree +
      ggplot2::theme(plot.margin = ggplot2::margin(sp, 0, 0, 0, "pt"))
  }

  if (!is.null(height)) obj$params$side_heights$top <- height
  obj$plots$top <- c(list(tree = p_tree), obj$plots$top)
  obj
}


# ===========================================================================
#  Internal helpers
# ===========================================================================

#' Coerce tree-like objects to hclust
#' @keywords internal
coerce_to_hclust <- function(tree) {
  if (inherits(tree, "hclust")) return(tree)
  if (inherits(tree, "dendrogram")) return(stats::as.hclust(tree))
  if (inherits(tree, "phylo")) return(stats::as.hclust(tree))
  rlang::abort("`tree` must be an hclust, dendrogram, or phylo object.")
}


#' @keywords internal
build_tree_plot <- function(hc,
                            direction       = c("row", "col"),
                            line_size       = 0.5,
                            color           = "grey30",
                            linetype        = "solid",
                            k               = NULL,
                            k_colors        = NULL,
                            cut_height      = NULL,
                            cut_linetype    = "dashed",
                            show_tip_labels = FALSE,
                            tip_label_size  = 2.5) {
  direction <- match.arg(direction)

  dend <- stats::as.dendrogram(hc)

  p_tree <- suppressMessages(
    ggtree::ggtree(dend, ladderize = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin     = ggplot2::margin(0, 0, 0, 0),
        legend.position = "none"
      )
  )

  if (!is.null(k) && k >= 2) {
    if (is.null(k_colors)) {
      k_colors <- default_cat_palette(k)
    }
    grp <- stats::cutree(hc, k = k)
    tree_data <- p_tree$data
    if ("label" %in% colnames(tree_data)) {
      label_grp <- grp[tree_data$label]
      tree_data$cluster <- as.factor(label_grp)
      p_tree$data <- tree_data
      p_tree <- p_tree +
        ggtree::aes(color = .data$cluster) +
        ggplot2::scale_color_manual(values = k_colors, na.value = color)
    }
  } else {
    p_tree <- p_tree +
      ggtree::aes(color = I(color))
  }

  p_tree <- p_tree +
    ggtree::aes(linewidth = I(line_size), linetype = I(linetype))

  if (show_tip_labels) {
    p_tree <- p_tree +
      ggtree::geom_tiplab(size = tip_label_size, hjust = -0.1)
  }

  if (!is.null(cut_height)) {
    p_tree <- p_tree +
      ggplot2::geom_hline(yintercept = cut_height, linetype = cut_linetype,
                          color = "red", linewidth = 0.4)
  }

  if (direction == "col") {
    p_tree <- suppressMessages(
      p_tree +
        ggplot2::scale_x_reverse() +
        ggplot2::coord_flip()
    )
  }

  p_tree
}
